mod constraints;
mod error;

use crate::{ast::*, typecheck::constraints::add_subtype_constraint};
use error::TypeError;
use std::collections::HashMap;

pub fn program(program: &Program) -> Result<(), TypeError> {
    let common_type_env: HashMap<Ident, Type> = program
        .funcs
        .iter()
        .map(|func| {
            (
                func.name.clone(),
                Type::FuncType(FuncType {
                    params: func.params.clone(),
                    ret: Box::new(func.ret.clone()),
                    pos: PosInfo::dummy(),
                }),
            )
        })
        .collect();

    for func in &program.funcs {
        typecheck_func(func, common_type_env.clone())?;
    }
    Ok(())
}

fn typecheck_func(func: &Func, common_type_env: HashMap<Ident, Type>) -> Result<(), TypeError> {
    let mut type_env = common_type_env.clone();
    for param in &func.params {
        if let Type::NonFuncType(NonFuncType {
            param_name,
            base_type,
            formula,
            pos,
        }) = param.clone()
        {
            type_env.insert(
                param_name.clone(),
                Type::NonFuncType(NonFuncType {
                    param_name,
                    base_type,
                    formula,
                    pos,
                }),
            );
        }
    }

    let mut constraints = vec![];
    let ret_type = typecheck_expr(&func.body, &type_env, &mut constraints)?;
    constraints::add_subtype_constraint(&ret_type, &func.ret, &mut constraints);

    constraints::solve_constraints(constraints)?;

    Ok(())
}

fn typecheck_expr(
    e: &Expr,
    type_env: &HashMap<Ident, Type>,
    constraints: &mut Vec<logic::Formula>,
) -> Result<Type, TypeError> {
    match e {
        Expr::Constant(c) => {
            let fresh_ident = Ident::fresh();
            Ok(Type::NonFuncType(NonFuncType {
                param_name: fresh_ident.clone(),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::BinApp(
                    logic::BinPred::Eq(PosInfo::dummy()),
                    logic::Expr::Var(fresh_ident),
                    logic::Expr::Constant(c.clone()),
                    PosInfo::dummy(),
                ),
                pos: PosInfo::dummy(),
            }))
        }
        Expr::Var(ident) => type_env
            .get(ident)
            .cloned()
            .ok_or_else(|| TypeError::UnboundVariable(ident.clone())),
        Expr::BinApp(_op, _e1, _e2, _) => todo!(),
        Expr::FuncApp(func_name, args, pos) => {
            let func_type = typecheck_expr(&Expr::Var(func_name.clone()), type_env, constraints)?;
            if let Type::FuncType(FuncType {
                params,
                ret,
                pos: func_type_pos,
            }) = func_type
            {
                let mut ret = *ret;
                // NOTE: 現状，部分適用は起きないので型が付くなら args.len() と params.len() は等しい
                //       これは関数適用の式の関数側は識別子しか許していないためである
                // TODO: 関数式を識別子以外も許した時に部分適用の型付けに対応する
                if args.len() == params.len() {
                    for i in 0..args.len() {
                        let arg_type = typecheck_expr(&args[i], type_env, constraints)?;
                        add_subtype_constraint(&arg_type, &params[i], constraints);
                        if let (
                            Type::NonFuncType(NonFuncType { param_name, .. }),
                            Type::NonFuncType(NonFuncType {
                                param_name: arg_name,
                                ..
                            }),
                        ) = (&params[i], arg_type)
                        {
                            ret = ret.subst_logical_expr(param_name, logic::Expr::Var(arg_name));
                        } else {
                            // 現状部分適用はないので arg_type が関数型になることはない
                            unimplemented!()
                        }
                    }
                    Ok(ret)
                } else {
                    Err(TypeError::FuncAppError(
                        FuncType {
                            params,
                            ret: Box::new(ret),
                            pos: func_type_pos,
                        },
                        args.clone(),
                    ))
                }
            } else {
                Err(TypeError::FuncExpected(func_type.clone(), pos.clone()))
            }
        }
        _ => todo!(),
    }
}
