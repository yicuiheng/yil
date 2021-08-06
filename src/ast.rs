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
pub enum BaseType<InfoT: Debug> {
    Int(InfoT),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Formula<InfoT: Debug> {
    True(InfoT),
}

#[derive(Debug, PartialEq, Eq)]
pub struct RefineType<InfoT: Debug> {
    pub param_name: Ident<InfoT>,
    pub base_type: BaseType<InfoT>,
    pub formula: Formula<InfoT>,
    pub info: InfoT,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Func<InfoT: Debug> {
    pub name: Ident<InfoT>,
    pub params: Vec<RefineType<InfoT>>,
    pub ret: RefineType<InfoT>,
    pub is_rec: bool,
    pub body: Expr<InfoT>,
    pub info: InfoT,
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
