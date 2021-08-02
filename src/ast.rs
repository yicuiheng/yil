use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub struct PosInfo {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident<InfoT: Debug> {
    pub name: String,
    pub info: InfoT,
}

impl<InfoT: Debug> Ident<InfoT> {
    pub fn info(&self) -> &InfoT {
        &self.info
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Constant<InfoT: Debug> {
    pub val: i64,
    pub info: InfoT,
}

impl<InfoT: Debug> Constant<InfoT> {
    pub fn info(&self) -> &InfoT {
        &self.info
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinOp<InfoT: Debug> {
    Add(InfoT),
    Sub(InfoT),
    Mul(InfoT),
    Div(InfoT),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<InfoT: Debug> {
    Constant(Constant<InfoT>),
    Var(Ident<InfoT>),
    BinApp(BinOp<InfoT>, Box<Expr<InfoT>>, Box<Expr<InfoT>>, InfoT),
    Ifz(Box<Expr<InfoT>>, Box<Expr<InfoT>>, Box<Expr<InfoT>>, InfoT),
    Let(Ident<InfoT>, Box<Expr<InfoT>>, Box<Expr<InfoT>>, InfoT),
}

impl<InfoT: Debug> Expr<InfoT> {
    pub fn info(&self) -> &InfoT {
        match self {
            Expr::Constant(c) => c.info(),
            Expr::Var(ident) => ident.info(),
            Expr::BinApp(_, _, _, info) => info,
            Expr::Ifz(_, _, _, info) => info,
            Expr::Let(_, _, _, info) => info,
        }
    }
}
