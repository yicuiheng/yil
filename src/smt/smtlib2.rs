use crate::ast::{logic::*, Ident};

pub fn term_to_sexpr(term: &Term) -> lexpr::Value {
    use lexpr::Value;
    match term {
        Term::Var(ident, _) => Value::symbol(ident.logical_symbol()),
        Term::True(_) => Value::symbol("true"),
        Term::False(_) => Value::symbol("false"),
        Term::Num(n, _) => Value::from(lexpr::Number::from(*n)),
        Term::Not(t, _) => Value::list(vec![Value::symbol("not"), term_to_sexpr(t)]),
        Term::Bin(op, t1, t2, _) => {
            let op = binop_to_sexpr(op);
            Value::list(vec![op, term_to_sexpr(t1), term_to_sexpr(t2)])
        }
    }
}

fn binop_to_sexpr(op: &BinOp) -> lexpr::Value {
    lexpr::Value::symbol(match op {
        BinOp::And => "and",
        BinOp::Or => "or",
        BinOp::Imply => "=>",
        BinOp::Eq => "=",
        BinOp::Neq => "distinct",
        BinOp::Lt => "<",
        BinOp::Leq => "<=",
        BinOp::Gt => ">",
        BinOp::Geq => ">=",
        BinOp::Add => "+",
        BinOp::Mult => "*",
        BinOp::Sub => "-",
        BinOp::Div => "/",
        BinOp::Rem => "mod",
    })
}

pub fn make_smtlib2(term_sexpr: lexpr::Value, free_idents: Vec<Ident>) -> String {
    use lexpr::Value;
    let mut sexpr = vec![];
    for ident in free_idents {
        sexpr.push(Value::list(vec![
            Value::symbol("declare-fun"),
            Value::symbol(ident.logical_symbol()),
            Value::list(vec![] as Vec<Value>),
            Value::symbol("Int"),
        ]));
    }
    sexpr.push(Value::list(vec![Value::symbol("assert"), term_sexpr]));
    sexpr.push(Value::list(vec![Value::symbol("check-sat")]));

    sexpr
        .iter()
        .map(lexpr::Value::to_string)
        .collect::<Vec<_>>()
        .join("\n")
}
