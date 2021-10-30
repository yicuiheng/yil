mod test;

use std::collections::HashMap;
use std::convert::TryInto;

use z3::{
    ast::{Ast, Bool, Int},
    *,
};

use crate::ast::{
    logic::{BinOp, Term},
    Ident, Info,
};

#[derive(Debug, PartialEq, Eq)]
pub enum SmtResult {
    CounterExamplesFound(HashMap<Ident, Term>),
    Unknown,
    Valid,
}

pub fn check_validity(term: Term) -> SmtResult {
    let config = Config::new();
    let context = Context::new(&config);
    let solver = Solver::new(&context);

    let (ast, consts) = bool_from_term(Term::Not(Box::new(term), Info::Dummy), &context);

    solver.assert(&ast);

    match solver.check() {
        // SatResult::Sat if consts.is_empty() => SmtResult::Valid,
        SatResult::Sat => {
            let model = solver.get_model().unwrap();
            let mut counter_examples = HashMap::new();
            for free_var in consts {
                let value = model.eval(&free_var, true).unwrap().as_i64().unwrap();
                let id = free_var.to_string()[1..].parse().unwrap();
                counter_examples.insert(
                    Ident {
                        id,
                        is_builtin: false,
                    },
                    Term::Num(value.try_into().unwrap(), Info::Dummy),
                );
            }
            SmtResult::CounterExamplesFound(counter_examples)
        }
        SatResult::Unsat => SmtResult::Valid,
        SatResult::Unknown => SmtResult::Unknown,
    }
}

fn bool_from_term<'a>(term: Term, context: &'a Context) -> (Bool, Vec<Int<'a>>) {
    match term {
        Term::True(_) => (Bool::from_bool(context, true), vec![]),
        Term::False(_) => (Bool::from_bool(context, false), vec![]),
        Term::Not(t, _) => {
            let (b, consts) = bool_from_term(*t, context);
            (b.not(), consts)
        }
        Term::Bin(BinOp::And, t1, t2, _) => {
            let mut consts = vec![];
            let (b1, consts1) = bool_from_term(*t1, context);
            consts.extend(consts1);
            let (b2, consts2) = bool_from_term(*t2, context);
            consts.extend(consts2);
            (Bool::and(context, &[&b1, &b2]), consts)
        }
        Term::Bin(BinOp::Or, t1, t2, _) => {
            let mut consts = vec![];
            let (b1, consts1) = bool_from_term(*t1, context);
            consts.extend(consts1);
            let (b2, consts2) = bool_from_term(*t2, context);
            consts.extend(consts2);
            (Bool::or(context, &[&b1, &b2]), consts)
        }
        Term::Bin(BinOp::Imply, t1, t2, _) => {
            let mut consts = vec![];
            let (b1, consts1) = bool_from_term(*t1, context);
            consts.extend(consts1);
            let (b2, consts2) = bool_from_term(*t2, context);
            consts.extend(consts2);
            (b1.implies(&b2), consts)
        }
        Term::Bin(BinOp::Eq, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (n1._eq(&n2), consts)
        }
        Term::Bin(BinOp::Neq, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (Ast::distinct(context, &[&n1, &n2]), consts)
        }
        Term::Bin(BinOp::Lt, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (n1.lt(&n2), consts)
        }
        Term::Bin(BinOp::Leq, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (n1.le(&n2), consts)
        }
        Term::Bin(BinOp::Gt, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (n1.gt(&n2), consts)
        }
        Term::Bin(BinOp::Geq, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (n1.ge(&n2), consts)
        }
        _ => unreachable!(),
    }
}

fn int_from_term<'a>(term: Term, context: &'a Context) -> (Int, Vec<Int<'a>>) {
    match term {
        Term::Num(n, _) => (Int::from_i64(context, n.try_into().unwrap()), vec![]),
        Term::Var(v, _) => {
            let new_var = Int::new_const(context, v.logical_symbol());
            (new_var.clone(), vec![new_var])
        }
        Term::Bin(BinOp::Add, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (Int::add(context, &[&n1, &n2]), consts)
        }
        Term::Bin(BinOp::Mult, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (Int::mul(context, &[&n1, &n2]), consts)
        }
        Term::Bin(BinOp::Sub, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (Int::sub(context, &[&n1, &n2]), consts)
        }
        Term::Bin(BinOp::Div, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (n1 / n2, consts)
        }
        Term::Bin(BinOp::Rem, t1, t2, _) => {
            let mut consts = vec![];
            let (n1, consts1) = int_from_term(*t1, context);
            consts.extend(consts1);
            let (n2, consts2) = int_from_term(*t2, context);
            consts.extend(consts2);
            (n1 % n2, consts)
        }
        _ => unreachable!(),
    }
}
