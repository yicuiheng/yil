use crate::ast::*;
use crate::env::SimpleTypeEnv;
use crate::typecheck::error::TypeError;

pub fn check_program(program: &Program) -> Result<(), TypeError> {
    let mut common_type_env = builtin::BuiltinData::simple_type_env();
    for func in &program.funcs {
        common_type_env.insert(func.typ.ident(), SimpleType::from(&func.typ));
    }
    for func in &program.funcs {
        check_func(func, &common_type_env)?;
    }
    Ok(())
}

fn check_func(func: &Func, type_env: &SimpleTypeEnv) -> Result<(), TypeError> {
    let mut type_env = type_env.clone();

    let mut ret_type = &func.typ;
    let mut count = func.params_len;
    while let Type::FuncType(_, from_type, to_type, _) = ret_type {
        if count == 0 {
            break;
        }
        type_env.insert(from_type.ident(), SimpleType::from(&**from_type));
        ret_type = to_type;
        count -= 1;
    }

    let (actual_ret_type, range) = check_expr(&func.body, &type_env)?;
    let expected_ret_type = SimpleType::from(ret_type);
    if actual_ret_type == expected_ret_type {
        Ok(())
    } else {
        Err(TypeError::UnexpectedSimpleType {
            actual: actual_ret_type,
            expected: expected_ret_type,
            range,
            msg: "the type of function body differs from the expected type",
        })
    }
}

pub fn check_expr(expr: &Expr, type_env: &SimpleTypeEnv) -> Result<(SimpleType, Range), TypeError> {
    let range = expr.info().range.unwrap();
    match expr {
        Expr::Num(_, _) => Ok((SimpleType::BaseType(BaseTypeKind::Int), range)),
        Expr::Boolean(_, _) => Ok((SimpleType::BaseType(BaseTypeKind::Bool), range)),
        Expr::Var(ident, _) => Ok((type_env.lookup(*ident).clone(), range)),
        Expr::If(cond, expr1, expr2, _) => {
            let (cond_type, range) = check_expr(cond, type_env)?;
            if !cond_type.is_bool() {
                return Err(TypeError::UnexpectedSimpleType {
                    actual: cond_type,
                    expected: SimpleType::BaseType(BaseTypeKind::Bool),
                    range,
                    msg: "if-condition expression must be boolean",
                });
            }
            let (type1, range1) = check_expr(expr1, type_env)?;
            let (type2, range2) = check_expr(expr2, type_env)?;
            if type1 != type2 {
                return Err(TypeError::UnexpectedSimpleType {
                    actual: type2,
                    expected: type1,
                    range: range2,
                    msg: "`if` and `else` have incompatible types",
                });
            }
            Ok((type1, range1))
        }
        Expr::Let(ident, expr1, expr2, _) => {
            let (type1, _) = check_expr(expr1, type_env)?;
            let mut type_env = type_env.clone();
            type_env.insert(*ident, type1);
            check_expr(expr2, &type_env)
        }
        Expr::App(expr1, expr2, _) => {
            let (type1, range1) = check_expr(expr1, type_env)?;
            let (type2, range2) = check_expr(expr2, type_env)?;
            if let SimpleType::FuncType(from_type, to_type) = type1 {
                if *from_type == type2 {
                    Ok((*to_type, Range::merge(range1, range2)))
                } else {
                    Err(TypeError::UnexpectedSimpleType {
                        actual: type2,
                        expected: *from_type,
                        range: range2,
                        msg: "mismatched types",
                    })
                }
            } else {
                Err(TypeError::FunctionExpected {
                    actual: type1,
                    range: range1,
                })
            }
        }
    }
}
