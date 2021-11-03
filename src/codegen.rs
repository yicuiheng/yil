use std::convert::TryInto;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    types::BasicTypeEnum,
    values::{BasicMetadataValueEnum, BasicValueEnum, CallableValue, FunctionValue, PointerValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};

use crate::{
    ast::{builtin::BuiltinData, *},
    env::{Env, NameEnv},
};

struct CodegenHelper<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
}

impl<'a> CodegenHelper<'a> {
    fn new(context: &'a Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
        }
    }

    fn add_function(&mut self, func: &Func, name_env: &NameEnv) -> FunctionValue<'a> {
        let func_type = make_llvm_type_from_simple_type(self.context, &SimpleType::from(&func.typ))
            .into_pointer_type()
            .get_element_type()
            .into_function_type();
        let func_name = name_env.lookup(func.typ.ident());
        self.module
            .add_function(func_name.as_str(), func_type, None)
    }

    fn build_function_body(
        &mut self,
        func: Func,
        name_env: &NameEnv,
        func_env: &Env<FunctionValue<'a>>,
        value_env: &mut Env<PointerValue<'a>>,
    ) {
        let function = func_env.lookup(func.typ.ident());
        let entry_basic_block = self.context.append_basic_block(*function, "entry");
        self.builder.position_at_end(entry_basic_block);

        let mut func_type = func.typ;
        let mut value_env = value_env.clone();
        for param_value in function.get_param_iter() {
            let (param_type, func_type_) = match func_type {
                Type::FuncType(_, type1, type2, _) => (*type1, *type2),
                Type::IntType(_, _, _) => unreachable!(),
            };
            func_type = func_type_;
            let param_ident = param_type.ident();
            let param_name = name_env.lookup(param_type.ident());
            let param_type =
                make_llvm_type_from_simple_type(self.context, &SimpleType::from(param_type));
            let param_value = match param_type {
                BasicMetadataTypeEnum::IntType(i) => {
                    let param = self.builder.build_alloca(i, &param_name);
                    self.builder.build_store(param, param_value);
                    param
                }
                BasicMetadataTypeEnum::PointerType(p) => {
                    let param = self.builder.build_alloca(p, &param_name);
                    param
                }
                _ => unreachable!(),
            };
            value_env.insert(param_ident, param_value);
        }

        let ret_expr = self.build_expr(func.body, name_env, &value_env, function);
        self.builder.build_return(Some(&ret_expr));
    }

    fn build_expr(
        &self,
        expr: Expr,
        name_env: &NameEnv,
        value_env: &Env<PointerValue<'a>>,
        current_function: &FunctionValue,
    ) -> BasicValueEnum<'a> {
        match expr {
            Expr::Num(n, _) => {
                BasicValueEnum::IntValue(self.context.i32_type().const_int(n as u64, false))
            }
            Expr::Var(ident, _) => {
                let var = value_env.lookup(ident).clone();
                let name = format!("{}.load", name_env.lookup(ident));
                if var.get_type().get_element_type().is_function_type() {
                    var.into()
                } else {
                    self.builder.build_load(var, name.as_str())
                }
            }
            Expr::Ifz(cond, then_expr, else_expr, _) => {
                let cond_expr = self.build_expr(*cond, name_env, value_env, current_function);
                let cond_value = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    cond_expr.into_int_value(),
                    self.context.i32_type().const_int(0, false),
                    "cond",
                );
                let then_block = self
                    .context
                    .append_basic_block(*current_function, "then_block");
                let else_block = self
                    .context
                    .append_basic_block(*current_function, "else_block");
                let merge_block = self
                    .context
                    .append_basic_block(*current_function, "merge_block");
                self.builder
                    .build_conditional_branch(cond_value, then_block, else_block);

                // then block
                self.builder.position_at_end(then_block);
                let then_ret = self.build_expr(*then_expr, name_env, value_env, current_function);
                self.builder.build_unconditional_branch(merge_block);
                let then_block = self.builder.get_insert_block().unwrap();

                // else block
                self.builder.position_at_end(else_block);
                let else_ret = self.build_expr(*else_expr, name_env, value_env, current_function);
                self.builder.build_unconditional_branch(merge_block);
                let else_block = self.builder.get_insert_block().unwrap();

                // merge block
                self.builder.position_at_end(merge_block);
                let phi = self.builder.build_phi(then_ret.get_type(), "phi");
                phi.add_incoming(&[(&then_ret, then_block), (&else_ret, else_block)]);
                phi.as_basic_value()
            }
            Expr::Let(ident, e1, e2, _) => {
                let e1_value = self.build_expr(*e1, name_env, value_env, current_function);
                let var_value = match e1_value.get_type() {
                    BasicTypeEnum::IntType(i) => {
                        self.builder.build_alloca(i, name_env.lookup(ident))
                    }
                    BasicTypeEnum::PointerType(p) => {
                        self.builder.build_alloca(p, name_env.lookup(ident))
                    }
                    _ => unreachable!(),
                };
                self.builder.build_store(var_value, e1_value);
                let mut value_env = value_env.clone();
                value_env.insert(ident, var_value);
                self.build_expr(*e2, name_env, &value_env, current_function)
            }
            Expr::App(mut e1, e2, _) => {
                // TODO: 部分適用の場合についても実装する
                let mut args: Vec<BasicMetadataValueEnum> = vec![self
                    .build_expr(*e2, name_env, value_env, current_function)
                    .into()];
                while let Expr::App(e1_, e2, _) = *e1 {
                    args.push(
                        self.build_expr(*e2, name_env, value_env, current_function)
                            .into(),
                    );
                    e1 = e1_
                }
                let args = args.into_iter().rev().collect();

                match *e1 {
                    Expr::Var(ident, _) if ident.is_builtin => self.build_builtin_call(ident, args),
                    _ => {
                        let func_value: CallableValue = self
                            .build_expr(*e1, name_env, value_env, current_function)
                            .into_pointer_value()
                            .try_into()
                            .unwrap();
                        self.builder
                            .build_call(func_value, args.as_slice(), "ret")
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                    }
                }
            }
        }
    }

    fn build_builtin_call(
        &self,
        ident: Ident,
        args: Vec<BasicMetadataValueEnum<'a>>,
    ) -> BasicValueEnum<'a> {
        let builtin = BuiltinData::instance();
        if builtin.or_ident == ident {
            self.builder
                .build_or(args[0].into_int_value(), args[1].into_int_value(), "or")
                .into()
        } else if builtin.and_ident == ident {
            self.builder
                .build_and(args[0].into_int_value(), args[1].into_int_value(), "and")
                .into()
        } else if builtin.eq_ident == ident {
            self.builder
                .build_int_compare(
                    IntPredicate::EQ,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    "eq",
                )
                .into()
        } else if builtin.neq_ident == ident {
            self.builder
                .build_int_compare(
                    IntPredicate::NE,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    "ne",
                )
                .into()
        } else if builtin.lt_ident == ident {
            self.builder
                .build_int_compare(
                    IntPredicate::SLT,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    "lt",
                )
                .into()
        } else if builtin.leq_ident == ident {
            self.builder
                .build_int_compare(
                    IntPredicate::SLE,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    "le",
                )
                .into()
        } else if builtin.gt_ident == ident {
            self.builder
                .build_int_compare(
                    IntPredicate::SGT,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    "gt",
                )
                .into()
        } else if builtin.geq_ident == ident {
            self.builder
                .build_int_compare(
                    IntPredicate::SGE,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    "ge",
                )
                .into()
        } else if builtin.add_ident == ident {
            self.builder
                .build_int_add(args[0].into_int_value(), args[1].into_int_value(), "add")
                .into()
        } else if builtin.sub_ident == ident {
            self.builder
                .build_int_sub(args[0].into_int_value(), args[1].into_int_value(), "sub")
                .into()
        } else if builtin.mult_ident == ident {
            self.builder
                .build_int_mul(args[0].into_int_value(), args[1].into_int_value(), "mul")
                .into()
        } else if builtin.div_ident == ident {
            self.builder
                .build_int_signed_div(args[0].into_int_value(), args[1].into_int_value(), "div")
                .into()
        } else if builtin.rem_ident == ident {
            self.builder
                .build_int_signed_rem(args[0].into_int_value(), args[1].into_int_value(), "rem")
                .into()
        } else {
            unimplemented!()
        }
    }

    fn run(&self) {
        let execution_engine = self
            .module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();
        let result = unsafe {
            execution_engine
                .get_function::<unsafe extern "C" fn(i32) -> i32>("main")
                .unwrap()
                .call(42)
        };
        eprintln!("result: {}", result);
    }
}

fn make_llvm_type_from_simple_type<'a>(
    context: &'a Context,
    mut simple_type: &SimpleType,
) -> BasicMetadataTypeEnum<'a> {
    let mut param_types: Vec<BasicMetadataTypeEnum<'a>> = vec![];
    while let SimpleType::FuncType(from_type, to_type) = simple_type {
        param_types.push(make_llvm_type_from_simple_type(context, from_type));
        simple_type = to_type;
    }

    if param_types.is_empty() {
        context.i32_type().into()
    } else {
        context
            .i32_type()
            .fn_type(param_types.as_slice(), false)
            .ptr_type(AddressSpace::Generic)
            .into()
    }
}

pub fn program(program: Program, name_env: &NameEnv) {
    let context = Context::create();
    let mut helper = CodegenHelper::new(&context);

    let mut func_env = Env::empty();
    let mut value_env = Env::empty();
    for func in &program.funcs {
        let func_value = helper.add_function(func, name_env);
        func_env.insert(func.typ.ident(), func_value);
        value_env.insert(
            func.typ.ident(),
            func_value.as_global_value().as_pointer_value(),
        );
    }

    for func in program.funcs {
        helper.build_function_body(func, name_env, &func_env, &mut value_env);
    }

    helper.run();
}
