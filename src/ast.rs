use std::fmt::Debug;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};

#[cfg(not(test))]
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct PosInfo {
    pub start: usize,
    pub end: usize,
}

#[cfg(test)]
#[derive(Debug, Clone, Hash)]
pub struct PosInfo {
    pub start: usize,
    pub end: usize,
}

#[cfg(test)]
impl PartialEq for PosInfo {
    fn eq(&self, other: &Self) -> bool {
        (self.start == 0 && self.end == 0)
            || (other.start == 0 && other.end == 0)
            || (self.start == other.start && self.end == other.end)
    }
}

#[cfg(test)]
impl Eq for PosInfo {}

impl PosInfo {
    pub fn dummy() -> Self {
        //[0, 0] を特別扱いする
        Self { start: 0, end: 0 }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: String,
    pub pos: PosInfo,
}

static FRESH_IDENT_COUNT: AtomicUsize = AtomicUsize::new(0);

impl Ident {
    pub fn fresh() -> Self {
        Self {
            name: format!("<fresh-{}>", FRESH_IDENT_COUNT.fetch_add(1, SeqCst)),
            pos: PosInfo::dummy(),
        }
    }

    #[cfg(test)]
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            pos: PosInfo::dummy(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BaseType {
    Int(PosInfo),
}

pub mod logic {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum BinPred {
        Eq(PosInfo),
        Neq(PosInfo),
        Lt(PosInfo),
        Leq(PosInfo),
        Gt(PosInfo),
        Geq(PosInfo),
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Formula {
        True(PosInfo),
        False(PosInfo),
        Not(Box<Formula>, PosInfo),
        And(Box<Formula>, Box<Formula>, PosInfo),
        Or(Box<Formula>, Box<Formula>, PosInfo),
        Imply(Box<Formula>, Box<Formula>, PosInfo),
        BinApp(BinPred, Expr, Expr, PosInfo),
    }

    impl Formula {
        pub fn subst(self, _var: &Ident, _e: Expr) -> Formula {
            todo!()
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum BinOp {
        Add(PosInfo),
        Mult(PosInfo),
        Sub(PosInfo),
        Div(PosInfo),
        Surplus(PosInfo),
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Expr {
        Var(Ident),
        Constant(Constant),
        BinApp(BinOp, Box<Expr>, Box<Expr>, PosInfo),
    }

    impl Expr {
        pub fn subst(self, _var: Ident, _e: Expr) -> Self {
            todo!()
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub funcs: Vec<Func>,
    pub pos: PosInfo,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
    pub pos: PosInfo,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NonFuncType {
    pub param_name: Ident,
    pub base_type: BaseType,
    pub formula: logic::Formula,
    pub pos: PosInfo,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    FuncType(FuncType),
    NonFuncType(NonFuncType),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Func {
    pub name: Ident,
    pub params: Vec<Type>,
    pub ret: Type,
    pub is_rec: bool,
    pub body: Expr,
    pub pos: PosInfo,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Constant {
    pub val: i64,
    pub pos: PosInfo,
}

impl Constant {
    #[cfg(test)]
    pub fn new(val: i64) -> Self {
        Self {
            val,
            pos: PosInfo::dummy(),
        }
    }
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
    Surplus(PosInfo),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Constant(Constant),
    Var(Ident),
    BinApp(BinOp, Box<Expr>, Box<Expr>, PosInfo),
    Ifz(Box<Expr>, Box<Expr>, Box<Expr>, PosInfo),
    Let(Ident, Box<Expr>, Box<Expr>, PosInfo),
    FuncApp(Ident, Vec<Expr>, PosInfo),
}
