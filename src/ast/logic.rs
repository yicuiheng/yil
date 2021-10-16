use std::fmt::{Display, Formatter, Result};

use crate::{ast::*, env::NameEnv};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    Var(Ident, Info),
    True(Info),
    False(Info),
    Num(i32, Info),
    Not(Box<Term>, Info),
    Bin(BinOp, Box<Term>, Box<Term>, Info),
}

impl Node for Term {
    fn to_readable_string(&self, name_env: &NameEnv) -> String {
        match self {
            Term::Var(ident, _) => {
                if let Some(name) = name_env.try_lookup(*ident) {
                    name.clone()
                } else {
                    ident.logical_symbol()
                }
            }
            Term::True(_) => "true".to_string(),
            Term::False(_) => "false".to_string(),
            Term::Num(n, _) => format!("{}", n),
            Term::Not(t, _) => format!("not {}", t.to_readable_string(name_env)),
            Term::Bin(op, t1, t2, _) => format!(
                "({} {} {})",
                t1.to_readable_string(name_env),
                op,
                t2.to_readable_string(name_env)
            ),
        }
    }

    fn info(&self) -> &Info {
        match self {
            Term::Var(_, info)
            | Term::True(info)
            | Term::False(info)
            | Term::Num(_, info)
            | Term::Not(_, info)
            | Term::Bin(_, _, _, info) => info,
        }
    }
}

impl Term {
    pub fn free_idents(&self) -> Vec<Ident> {
        match self {
            Term::Var(ident, _) => vec![*ident],
            Term::Not(t, _) => t.free_idents(),
            Term::Bin(_, t1, t2, _) => {
                let mut free_idents = vec![];
                free_idents.append(&mut t1.free_idents());
                free_idents.append(&mut t2.free_idents());
                free_idents
            }
            _ => vec![],
        }
    }

    pub fn subst(self, ident: Ident, term: &Self) -> Self {
        match self {
            Term::Var(id, _) if id == ident => term.clone(),
            Term::Not(t, info) => Term::Not(Box::new(t.subst(ident, term)), info),
            Term::Bin(op, t1, t2, info) => Term::Bin(
                op,
                Box::new(t1.subst(ident, term)),
                Box::new(t2.subst(ident, term)),
                info,
            ),
            _ => self,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOp {
    And,
    Or,
    Imply,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Add,
    Mult,
    Sub,
    Div,
    Rem,
}

impl Display for BinOp {
    fn fmt(&self, fmt: &mut Formatter) -> Result {
        write!(
            fmt,
            "{}",
            match self {
                BinOp::And => "and",
                BinOp::Or => "or",
                BinOp::Imply => "=>",
                BinOp::Eq => "=",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Leq => "<=",
                BinOp::Gt => ">",
                BinOp::Geq => ">=",
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mult => "*",
                BinOp::Div => "/",
                BinOp::Rem => "%",
            }
        )
    }
}
