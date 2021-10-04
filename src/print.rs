use crate::ast::*;
use std::collections::HashMap;

pub fn print_type(typ: &Type) {
    println!("{}", string_of_type(typ))
}

pub fn print_type_env(env: &HashMap<Ident, Type>) {
    for (ident, typ) in env {
        println!("{}: {}", ident.name, string_of_type(typ));
    }
}

pub fn string_of_type(typ: &Type) -> String {
    match typ {
        Type::NonFuncType(NonFuncType {
            param_name,
            base_type,
            formula,
            ..
        }) => format!(
            "{{{}: {} | {}}}",
            param_name.name,
            string_of_base_type(base_type),
            string_of_formula(formula)
        ),
        _ => "<unimplemented>".to_string(), // TODO
    }
}

pub fn string_of_base_type(base_type: &BaseType) -> String {
    match base_type {
        BaseType::Int(_) => "int".to_string(),
    }
}

pub fn string_of_formula(formula: &logic::Formula) -> String {
    match formula {
        logic::Formula::True(_) => "true".to_string(),
        logic::Formula::False(_) => "false".to_string(),
        logic::Formula::Not(f, _) => format!("not ({})", string_of_formula(f)),
        logic::Formula::And(f1, f2, _) => format!(
            "({}) and ({})",
            string_of_formula(f1),
            string_of_formula(f2)
        ),
        logic::Formula::Or(f1, f2, _) => {
            format!("({}) or ({})", string_of_formula(f1), string_of_formula(f2))
        }
        logic::Formula::Imply(f1, f2, _) => {
            format!("({}) => ({})", string_of_formula(f1), string_of_formula(f2))
        }
        logic::Formula::BinApp(pred, e1, e2, _) => {
            let pred = match pred {
                logic::BinPred::Eq(_) => "=",
                logic::BinPred::Neq(_) => "!=",
                logic::BinPred::Lt(_) => "<",
                logic::BinPred::Leq(_) => "<=",
                logic::BinPred::Gt(_) => ">",
                logic::BinPred::Geq(_) => ">=",
            };
            format!(
                "({}) {} ({})",
                string_of_logical_expr(e1),
                pred,
                string_of_logical_expr(e2)
            )
        }
    }
}

pub fn string_of_logical_expr(e: &logic::Expr) -> String {
    match e {
        logic::Expr::Var(ident) => format!("{}", ident.name),
        logic::Expr::Constant(c) => format!("{}", c.val),
        logic::Expr::BinApp(op, e1, e2, _) => {
            let op = match op {
                logic::BinOp::Add(_) => "+",
                logic::BinOp::Sub(_) => "-",
                logic::BinOp::Mult(_) => "*",
                logic::BinOp::Div(_) => "/",
                logic::BinOp::Surplus(_) => "%",
            };
            format!(
                "({}) {} ({})",
                string_of_logical_expr(e1),
                op,
                string_of_logical_expr(e2)
            )
        }
    }
}
