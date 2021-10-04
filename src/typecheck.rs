mod constraints;
mod error;
mod test;
mod typecheck_expr_detail;
mod util;

use std::collections::HashMap;

use crate::ast::*;
use error::TypeError;
use typecheck_expr_detail::*;

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
    let (ret_type, type_env) = typecheck_expr(
        &util::preprocess(func.body.clone()),
        &type_env,
        &mut constraints,
    )?;

    constraints::add_subtype_constraint(&ret_type, &func.ret, &type_env, &mut constraints);
    constraints::solve_constraints(constraints)?;

    Ok(())
}

fn typecheck_expr(
    e: &Expr,
    type_env: &HashMap<Ident, Type>,
    constraints: &mut Vec<logic::Formula>,
) -> Result<(Type, HashMap<Ident, Type>), TypeError> {
    match e {
        Expr::Constant(c) => Ok((typecheck_constant(c), type_env.clone())),
        Expr::Var(ident) => typecheck_var_expr(ident, type_env),
        Expr::BinApp(op, e1, e2, pos) => {
            typecheck_binapp_expr(op, &*e1, &*e2, pos, type_env, constraints)
        }
        Expr::Ifz(cond, e1, e2, _) => typecheck_ifz_expr(&*cond, &*e1, &*e2, type_env, constraints),
        Expr::Let(ident, e1, e2, _) => typecheck_let_expr(ident, e1, e2, type_env, constraints),
        Expr::FuncApp(func_name, args, pos) => {
            typecheck_funcapp_expr(func_name, args, pos, type_env, constraints)
        }
    }
}
