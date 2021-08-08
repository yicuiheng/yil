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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LogicalBinPred {
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
    And(Box<Formula>, Box<Formula>, PosInfo),
    Or(Box<Formula>, Box<Formula>, PosInfo),
    BinApp(LogicalBinPred, LogicalExpr, LogicalExpr, PosInfo),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalBinOp {
    Add(PosInfo),
    Mult(PosInfo),
    Sub(PosInfo),
    Div(PosInfo),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalExpr {
    Var(Ident),
    Constant(Constant),
    BinApp(LogicalBinOp, Box<LogicalExpr>, Box<LogicalExpr>, PosInfo),
}

#[derive(Debug, PartialEq, Eq)]
pub struct RefineType {
    pub param_name: Ident,
    pub base_type: BaseType,
    pub formula: Formula,
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
