mod calc_constraints;
mod constraint;
pub mod error;
mod simple_typecheck;
mod test;

use crate::{ast::*, env::TypeEnv};
use error::TypeError;

pub fn typecheck_program(program: &Program) -> Result<(), TypeError> {
    simple_typecheck::check_program(program)?;
    refinement_typecheck(program)?;
    Ok(())
}

fn refinement_typecheck(program: &Program) -> Result<(), TypeError> {
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
    let mut count = func.params_len;
    while let Type::FuncType(_, from_type, to_type, _) = ret_type {
        if count == 0 {
            break;
        }
        type_env.insert(from_type.ident(), *from_type.clone());
        ret_type = to_type;
        count -= 1;
    }

    let mut constraints = vec![];
    let (body_ret_type, type_env) = calc_constraints::expr(&func.body, &type_env, &mut constraints);

    let body_range = func.body.info().as_range();
    let ret_type_range = ret_type.info().as_range();
    constraint::add_subtype_constraint(
        &body_ret_type,
        &ret_type,
        &type_env,
        &mut constraints,
        ret_type_range,
        body_range,
    );
    constraint::solve_constraints(constraints)?;

    Ok(())
}

#[cfg(test)]
pub fn typecheck_expr(
    expr: &Expr,
    type_env: &TypeEnv,
    constraints: &mut Vec<constraint::Constraint>,
) -> Result<(Type, TypeEnv), TypeError> {
    use crate::env::SimpleTypeEnv;
    let simple_type_env = SimpleTypeEnv::from(type_env.clone());
    simple_typecheck::check_expr(expr, &simple_type_env)?;
    Ok(calc_constraints::expr(expr, type_env, constraints))
}
