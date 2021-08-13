use crate::ast::{logic::*, Constant};

pub enum Status {
    Sat,
    Unsat,
    Unknown,
}

pub fn check_validity(formula: Formula) -> Status {
    use lexpr::Value;
    let sexpr = formula_to_sexpr(formula);
    println!("{}", sexpr);
    let sexpr = vec![
        Value::list(vec![Value::symbol("set-logic"), Value::symbol("QF_LIA")]),
        Value::list(vec![Value::symbol("assert"), sexpr]),
        Value::list(vec![Value::symbol("check-sat")]),
    ];
    run_z3(&sexpr)
}

fn formula_to_sexpr(formula: Formula) -> lexpr::Value {
    use lexpr::Value;
    match formula {
        Formula::True(_) => Value::symbol("true"),
        Formula::False(_) => Value::symbol("false"),
        Formula::Not(inner, _) => Value::list(vec![Value::symbol("not"), formula_to_sexpr(*inner)]),
        Formula::And(lhs, rhs, _) => Value::list(vec![
            Value::symbol("and"),
            formula_to_sexpr(*lhs),
            formula_to_sexpr(*rhs),
        ]),
        Formula::Or(lhs, rhs, _) => Value::list(vec![
            Value::symbol("or"),
            formula_to_sexpr(*lhs),
            formula_to_sexpr(*rhs),
        ]),
        Formula::BinApp(BinPred::Eq(_), lhs, rhs, _) => Value::list(vec![
            Value::symbol("="),
            expr_to_sexpr(lhs),
            expr_to_sexpr(rhs),
        ]),
        Formula::BinApp(BinPred::Neq(pos1), lhs, rhs, pos2) => formula_to_sexpr(Formula::Not(
            Box::new(Formula::BinApp(BinPred::Eq(pos1.clone()), lhs, rhs, pos1)),
            pos2,
        )),
        Formula::BinApp(BinPred::Lt(_), lhs, rhs, _) => Value::list(vec![
            Value::symbol("<"),
            expr_to_sexpr(lhs),
            expr_to_sexpr(rhs),
        ]),
        Formula::BinApp(BinPred::Leq(_), lhs, rhs, _) => Value::list(vec![
            Value::symbol("<="),
            expr_to_sexpr(lhs),
            expr_to_sexpr(rhs),
        ]),
        Formula::BinApp(BinPred::Gt(_), lhs, rhs, _) => Value::list(vec![
            Value::symbol(">"),
            expr_to_sexpr(lhs),
            expr_to_sexpr(rhs),
        ]),
        Formula::BinApp(BinPred::Geq(_), lhs, rhs, _) => Value::list(vec![
            Value::symbol(">="),
            expr_to_sexpr(lhs),
            expr_to_sexpr(rhs),
        ]),
    }
}

fn expr_to_sexpr(expr: Expr) -> lexpr::Value {
    use lexpr::Value;
    match expr {
        Expr::Var(ident) => Value::symbol(ident.name),
        Expr::Constant(constant) => constant_to_sexpr(constant),
        Expr::BinApp(binop, lhs, rhs, _) => Value::list(vec![
            binop_to_sexpr(binop),
            expr_to_sexpr(*lhs),
            expr_to_sexpr(*rhs),
        ]),
    }
}

fn binop_to_sexpr(binop: BinOp) -> lexpr::Value {
    use lexpr::Value;
    match binop {
        BinOp::Add(_) => Value::symbol("+"),
        BinOp::Mult(_) => Value::symbol("*"),
        BinOp::Sub(_) => Value::symbol("-"),
        BinOp::Div(_) => Value::symbol("/"),
    }
}

fn constant_to_sexpr(constant: Constant) -> lexpr::Value {
    use lexpr::Value;
    Value::from(lexpr::Number::from(constant.val))
}

fn run_z3(sexpr: &Vec<lexpr::Value>) -> Status {
    use std::io::{self, Read, Write};
    use std::process::Command;
    use tempfile::NamedTempFile;

    let mut temp = tempfile::Builder::new()
        .suffix(".smtlib2")
        .tempfile()
        .expect("cannot create temp file for z3 input");
    for inner in sexpr {
        writeln!(temp, "{}", inner);
    }
    let z3_input_filename = temp.path().to_str().unwrap().to_string();

    let output = Command::new("z3")
        .arg(z3_input_filename)
        .output()
        .expect("failed to run z3");

    println!("status: {}", output.status);
    println!("[stdout]");
    println!("{}", std::str::from_utf8(output.stdout.as_slice()).unwrap());
    println!("[stderr]");
    println!("{}", std::str::from_utf8(output.stderr.as_slice()).unwrap());
    Status::Sat
}
