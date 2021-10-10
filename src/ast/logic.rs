use super::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    Var(Ident, Info),
    True(Info),
    False(Info),
    Num(i32, Info),
    Not(Box<Term>, Info),
    Bin(BinOp, Box<Term>, Box<Term>, Info),
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
