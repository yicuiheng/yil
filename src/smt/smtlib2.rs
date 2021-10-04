use crate::ast::{logic::*, Constant, Ident};

pub fn make_not(formula_sexp: lexpr::Value) -> lexpr::Value {
    lexpr::Value::list(vec![lexpr::Value::symbol("not"), formula_sexp])
}

pub fn formula_to_sexpr(formula: &Formula) -> lexpr::Value {
    use lexpr::Value;
    match formula {
        Formula::True(_) => Value::symbol("true"),
        Formula::False(_) => Value::symbol("false"),
        Formula::Not(inner, _) => Value::list(vec![Value::symbol("not"), formula_to_sexpr(inner)]),
        Formula::And(lhs, rhs, _) => Value::list(vec![
            Value::symbol("and"),
            formula_to_sexpr(lhs),
            formula_to_sexpr(rhs),
        ]),
        Formula::Or(lhs, rhs, _) => Value::list(vec![
            Value::symbol("or"),
            formula_to_sexpr(lhs),
            formula_to_sexpr(rhs),
        ]),
        Formula::BinApp(BinPred::Eq(_), lhs, rhs, _) => Value::list(vec![
            Value::symbol("="),
            expr_to_sexpr(lhs),
            expr_to_sexpr(rhs),
        ]),
        Formula::BinApp(BinPred::Neq(pos1), lhs, rhs, pos2) => formula_to_sexpr(&Formula::Not(
            Box::new(Formula::BinApp(
                BinPred::Eq(pos1.clone()),
                lhs.clone(),
                rhs.clone(),
                pos1.clone(),
            )),
            pos2.clone(),
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
        Formula::Imply(f1, f2, pos) => formula_to_sexpr(&Formula::Or(
            Box::new(Formula::Not(f1.clone(), pos.clone())),
            f2.clone(),
            pos.clone(),
        )),
    }
}

fn expr_to_sexpr(expr: &Expr) -> lexpr::Value {
    use lexpr::Value;
    match expr {
        Expr::Var(ident) => Value::symbol(ident.name.clone()),
        Expr::Constant(constant) => constant_to_sexpr(constant),
        Expr::BinApp(binop, lhs, rhs, _) => Value::list(vec![
            binop_to_sexpr(binop),
            expr_to_sexpr(lhs),
            expr_to_sexpr(rhs),
        ]),
    }
}

fn binop_to_sexpr(binop: &BinOp) -> lexpr::Value {
    use lexpr::Value;
    match binop {
        BinOp::Add(_) => Value::symbol("+"),
        BinOp::Mult(_) => Value::symbol("*"),
        BinOp::Sub(_) => Value::symbol("-"),
        BinOp::Div(_) => Value::symbol("/"),
        BinOp::Surplus(_) => Value::symbol("mod"),
    }
}

fn constant_to_sexpr(constant: &Constant) -> lexpr::Value {
    use lexpr::Value;
    Value::from(lexpr::Number::from(constant.val))
}

pub fn make_smtlib2(formula_sexpr: lexpr::Value, free_vars: Vec<Ident>) -> String {
    use lexpr::Value;
    let mut sexpr = vec![];
    for free_var in free_vars {
        sexpr.push(Value::list(vec![
            Value::symbol("declare-fun"),
            Value::symbol(free_var.name),
            Value::list(vec![] as Vec<Value>),
            Value::symbol("Int"),
        ]));
    }
    sexpr.push(Value::list(vec![Value::symbol("assert"), formula_sexpr]));
    sexpr.push(Value::list(vec![Value::symbol("check-sat")]));

    sexpr
        .iter()
        .map(lexpr::Value::to_string)
        .collect::<Vec<_>>()
        .join("\n")
}
