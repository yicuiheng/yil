mod constraints;
pub mod error;
mod test;
mod typecheck_expr_detail;

use crate::{ast::*, env::TypeEnv};
use error::TypeError;
use typecheck_expr_detail::*;

pub fn program(program: &Program) -> Result<(), TypeError> {
    let mut common_type_env = builtin::BuiltinData::type_env();
    for func in &program.funcs {
        common_type_env.insert(func.typ.ident(), func.typ.clone())
    }

    for func in &program.funcs {
        typecheck_func(func, common_type_env.clone())?;
    }
    Ok(())
}

fn typecheck_func(func: &Func, common_type_env: TypeEnv) -> Result<(), TypeError> {
    let mut type_env = common_type_env;

    let mut ret_type: &Type = &func.typ;
    while let Type::FuncType(_, from_type, to_type, _) = ret_type {
        type_env.insert(from_type.ident(), *from_type.clone());
        ret_type = to_type;
    }

    let mut constraints = vec![];
    let (body_ret_type, type_env) = typecheck_expr(&func.body, &type_env, &mut constraints)?;

    constraints::add_subtype_constraint(&body_ret_type, &ret_type, &type_env, &mut constraints);
    constraints::solve_constraints(constraints)?;

    Ok(())
}

fn typecheck_expr(
    e: &Expr,
    type_env: &TypeEnv,
    constraints: &mut Vec<logic::Term>,
) -> Result<(Type, TypeEnv), TypeError> {
    match e {
        Expr::Num(n, _) => typecheck_number(*n, type_env),
        Expr::Var(ident, _) => typecheck_var_expr(ident, type_env),
        Expr::Ifz(cond, e1, e2, _) => typecheck_ifz_expr(&*cond, &*e1, &*e2, type_env, constraints),
        Expr::Let(ident, e1, e2, _) => typecheck_let_expr(ident, e1, e2, type_env, constraints),
        Expr::App(func, arg, info) => typecheck_app_expr(func, arg, info, type_env, constraints),
    }
}
