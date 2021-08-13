use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PosInfo {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub pos: PosInfo,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BaseType {
    Int(PosInfo),
}

pub mod logic {
    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    pub enum BinPred {
        Eq(PosInfo),
        Neq(PosInfo),
        Lt(PosInfo),
        Leq(PosInfo),
        Gt(PosInfo),
        Geq(PosInfo),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Formula {
        True(PosInfo),
        False(PosInfo),
        Not(Box<Formula>, PosInfo),
        And(Box<Formula>, Box<Formula>, PosInfo),
        Or(Box<Formula>, Box<Formula>, PosInfo),
        BinApp(BinPred, Expr, Expr, PosInfo),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum BinOp {
        Add(PosInfo),
        Mult(PosInfo),
        Sub(PosInfo),
        Div(PosInfo),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Expr {
        Var(Ident),
        Constant(Constant),
        BinApp(BinOp, Box<Expr>, Box<Expr>, PosInfo),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub funcs: Vec<Func>,
    pub pos: PosInfo,
}

#[derive(Debug, PartialEq, Eq)]
pub struct RefineType {
    pub param_name: Ident,
    pub base_type: BaseType,
    pub formula: logic::Formula,
    pub pos: PosInfo,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Func {
    pub name: Ident,
    pub params: Vec<RefineType>,
    pub ret: RefineType,
    pub is_rec: bool,
    pub body: Expr,
    pub pos: PosInfo,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Constant {
    pub val: i64,
    pub pos: PosInfo,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinOp {
    Or(PosInfo),
    And(PosInfo),
    Eq(PosInfo),
    Neq(PosInfo),
    Lt(PosInfo),
    Leq(PosInfo),
    Gt(PosInfo),
    Geq(PosInfo),
    Add(PosInfo),
    Sub(PosInfo),
    Mul(PosInfo),
    Div(PosInfo),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Constant(Constant),
    Var(Ident),
    BinApp(BinOp, Box<Expr>, Box<Expr>, PosInfo),
    Ifz(Box<Expr>, Box<Expr>, Box<Expr>, PosInfo),
    Let(Ident, Box<Expr>, Box<Expr>, PosInfo),
    FuncApp(Box<Expr>, Box<Expr>, PosInfo),
}
