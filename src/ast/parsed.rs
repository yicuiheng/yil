use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum Formula {
    True(PosInfo),
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
}

impl Expr {
    pub fn info(&self) -> &PosInfo {
        match self {
            Expr::Constant(c) => &c.pos,
            Expr::Var(ident) => &ident.pos,
            Expr::BinApp(_, _, _, info) => info,
            Expr::Ifz(_, _, _, info) => info,
            Expr::Let(_, _, _, info) => info,
        }
    }
}
