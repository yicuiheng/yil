use crate::ast::{logic::*, Constant};

#[derive(Debug, PartialEq, Eq)]
pub enum Status {
    Valid,
    Invalid,
    Unknown,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Z3Status {
    Sat,
    Unsat,
    Unknown,
}

pub fn check_validity(formula: Formula) -> Status {
    use lexpr::Value;

    let positive_sexpr = formula_to_sexpr(formula);
    let negative_sexpr = Value::list(vec![Value::symbol("not"), positive_sexpr.clone()]);

    if run_z3(negative_sexpr) == Z3Status::Unsat {
        return Status::Valid;
    }

    if run_z3(positive_sexpr) == Z3Status::Sat {
        return Status::Invalid;
    }

    Status::Unknown
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

fn run_z3(sexpr: lexpr::Value) -> Z3Status {
    use lexpr::Value;
    use std::io::Write;
    use std::process::Command;

    let sexpr = vec![
        Value::list(vec![Value::symbol("set-logic"), Value::symbol("QF_LIA")]),
        Value::list(vec![Value::symbol("assert"), sexpr]),
        Value::list(vec![Value::symbol("check-sat")]),
    ];

    let query = sexpr
        .iter()
        .map(lexpr::Value::to_string)
        .collect::<Vec<_>>()
        .join("\n");

    let mut temp = tempfile::Builder::new()
        .suffix(".smtlib2")
        .tempfile()
        .expect("cannot create temp file for z3 input");

    writeln!(temp, "{}", query);

    let z3_input_filename = temp.path().to_str().unwrap().to_string();

    let output = Command::new("z3")
        .arg(z3_input_filename)
        .output()
        .expect("failed to run z3");

    println!("{}", output.status.code().unwrap());

    if output.status.code().unwrap() != 0 {
        eprintln!(
            "[UNEXPECTED ERROR] z3 cannot recognized the query generated from yil type system.."
        );
        eprintln!("please contact me, yicuiheng <yicuiheng@gmail.com>");
        eprintln!("query passed to z3: ");
        eprintln!("```");
        eprintln!("{}", query);
        eprintln!("```");
        eprintln!("error: ");
        eprintln!("```");
        eprintln!("{}", std::str::from_utf8(output.stderr.as_slice()).unwrap());
        eprintln!("```");
        return Z3Status::Unknown;
    }

    if !output.stderr.is_empty() {
        eprintln!("[UNEXPECTED WARNING] warning occured in z3..");
        eprintln!("please contact me, yicuiheng <yicuiheng@gmail.com>");
        eprintln!("query passed to z3: ");
        eprintln!("```");
        eprintln!("{}", query);
        eprintln!("``");
        eprintln!("warning: ");
        eprintln!("```");
        eprintln!("{}", std::str::from_utf8(output.stderr.as_slice()).unwrap());
        eprintln!("```");
        return Z3Status::Unknown;
    }

    match std::str::from_utf8(output.stdout.as_slice()).unwrap() {
        "sat\n" => Z3Status::Sat,
        "unsat\n" => Z3Status::Unsat,
        _ => {
            eprintln!("[UNEXPECTED ERROR] z3 cannot recognized the query generated from yil type system..");
            eprintln!("please contact me, yicuiheng <yicuiheng@gmail.com>");
            eprintln!("query passed to z3: ");
            eprintln!("```");
            eprintln!("{}", query);
            eprintln!("``");

            eprintln!("output: ");
            eprintln!("```");
            eprintln!("{}", std::str::from_utf8(output.stdout.as_slice()).unwrap());
            eprintln!("```");

            eprintln!("warning: ");
            eprintln!("```");
            eprintln!("{}", std::str::from_utf8(output.stderr.as_slice()).unwrap());
            eprintln!("```");

            return Z3Status::Unknown;
        }
    }
}
