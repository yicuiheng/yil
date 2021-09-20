use std::collections::HashMap;
use crate::ast::*;

pub enum TypeError {
    Hoge(String),
}

pub fn check_program(program: Program) -> Result<(), TypeError> 
{
    let func_types = program.funcs.iter().map(|func| {
        let func_name = func.name.clone();
        let func_type = (func.params.clone(), func.ret.clone());
        (func_name, func_type)
    }).collect();

    for func in program.funcs {
        check_func(func, &func_types)?;
    }

    Ok(())
}

fn check_func(mut func: Func, func_types: &HashMap<Ident, (Vec<Type>, Type)>) -> Result<Type, TypeError> {
    check_expr(func.body, func_types, &mut func.params)
}

fn lookup_var_type<'a>(name: &Ident, var_types: &'a Vec<Type>) -> Option<&'a Type> {
    var_types.iter().find(|var_type| match var_type {
        Type::NonFuncType(non_func_type) => &non_func_type.param_name == name,
        _ => todo!()
    })
}

fn check_expr(expr: Expr, func_types: &HashMap<Ident, (Vec<Type>, Type)>, var_types: &mut Vec<Type>) -> Result<Type, TypeError> {
    match expr {
        Expr::Constant(Constant {
            val,
            pos
        }) => {
            let param = Ident::fresh();
            Ok(Type::NonFuncType(NonFuncType {
                param_name: param.clone(),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::BinApp(logic::BinPred::Eq(PosInfo::dummy()), logic::Expr::Var(param), logic::Expr::Constant(Constant {
                    val,
                    pos
                }), PosInfo::dummy()),
                pos: PosInfo::dummy()
            }))
        },
        Expr::Var(ident) => lookup_var_type(&ident, &var_types).cloned().ok_or_else(|| TypeError::Hoge(format!("unbound variable: {:?}", ident))),
        Expr::BinApp(binop, e1, e2, _) => todo!(),
        Expr::Ifz(cond, e1, e2, _) => {
            let cond_refine_type = check_expr(*cond, func_types, var_types)?;
            let cond_name = if let Type::NonFuncType(non_func_type) = cond_refine_type {
                non_func_type.param_name
            } else {
                todo!()
            };
            let fresh_name = Ident::fresh();
            var_types.push(Type::NonFuncType(NonFuncType {
                param_name: fresh_name.clone(),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::BinApp(logic::BinPred::Eq(PosInfo::dummy()), logic::Expr::Var(cond_name.clone()), logic::Expr::Constant(Constant {
                    val: 0,
                    pos: PosInfo::dummy()
                }), PosInfo::dummy()),
                pos: PosInfo::dummy()
            }));
            let e1_type = check_expr(*e1, func_types, var_types)?;
            var_types.pop();

            var_types.push(Type::NonFuncType(NonFuncType {
                param_name: fresh_name.clone(),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::Not(
                    Box::new(logic::Formula::BinApp(
                        logic::BinPred::Eq(PosInfo::dummy()),
                        logic::Expr::Var(cond_name.clone()),
                        logic::Expr::Constant(Constant {
                            val: 0,
                            pos: PosInfo::dummy()
                        }),
                        PosInfo::dummy())),
                    PosInfo::dummy()),
                pos: PosInfo::dummy()
            }));
            let e2_type = check_expr(*e2, func_types, var_types)?;
            var_types.pop();
            Ok(e1_type)
        },
        Expr::Let(ident, e1, e2, _) => {
            todo!()
        },
        Expr::FuncApp(ident, args, _) => {
            let (params, ret) = func_types.get(&ident).ok_or_else(|| TypeError::Hoge(format!("unbound function variable: {:?}", ident)))?.clone();
            let params_len = params.len();
            let arg_types: Result<Vec<_>, _> = args.into_iter().map(|arg| check_expr(arg, func_types, var_types)).collect();
            let arg_types = arg_types?;
            let param_formulas_and_names: Vec<(logic::Formula, Ident)> = params.into_iter().map(|param| match param {
                Type::NonFuncType(non_func_type) => (non_func_type.formula, non_func_type.param_name),
                _ => todo!()
            }).collect();
            assert_eq!(arg_types.len(), params_len);
            let (arg_formulas, arg_names) = arg_types.into_iter().fold((vec![], vec![]), |(mut arg_formulas, mut arg_names), arg_type| {
                if let Type::NonFuncType(non_func_type) = arg_type {
                    arg_formulas.push(non_func_type.formula);
                    arg_names.push(non_func_type.param_name);
                } else {
                    todo!()
                }
                (arg_formulas, arg_names)
            });

            for i in 0..params_len {
                let constraint = logic::Formula::Or(
                    Box::new(logic::Formula::Not(Box::new(arg_formulas[i].clone()), PosInfo::dummy())),
                    Box::new(param_formulas_and_names[i].0.clone()),
                    PosInfo::dummy()
                );
                let error_str = format!("type error: {:?}", constraint);
                /*
                if check_validity::check_validity(constraint) == check_validity::Status::Valid {
                    return Err(TypeError::Hoge(error_str));
                } */
            }
            Ok(arg_names.into_iter().enumerate().fold(ret.clone(), |acc, (i, arg_name)| {
                if let Type::NonFuncType(non_func_type) = acc {
                    Type::NonFuncType(NonFuncType {
                        formula : non_func_type.formula.subst(&param_formulas_and_names[i].1, logic::Expr::Var(arg_name)),
                        ..non_func_type
                    })
                } else {
                    todo!()
                }
            }))
        },
    }
}
