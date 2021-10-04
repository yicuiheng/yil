use std::cmp::Ordering;
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

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub pos: PosInfo,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Ident) -> bool {
        self.name == other.name
    }
}

impl Eq for Ident {}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl std::hash::Hash for Ident {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.name.hash(state)
    }
}

static FRESH_IDENT_COUNT: AtomicUsize = AtomicUsize::new(0);

impl Ident {
    pub fn fresh() -> Self {
        Self {
            name: format!("_fresh.{}", FRESH_IDENT_COUNT.fetch_add(1, SeqCst)),
            pos: PosInfo::dummy(),
        }
    }

    #[cfg(test)]
    pub fn reset_fresh_count() {
        FRESH_IDENT_COUNT.store(0, SeqCst)
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

    // TODO: subst や free_vars のパフォーマンスが明らかに悪いのでデータの持ち方を改善する
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
        pub fn subst(&self, var: &Ident, e: Expr) -> Formula {
            use Formula::*;
            match self {
                True(_) | False(_) => self.clone(),
                Not(f, pos) => Not(Box::new(f.subst(var, e)), pos.clone()),
                And(f1, f2, pos) => And(
                    Box::new(f1.subst(var, e.clone())),
                    Box::new(f2.subst(var, e.clone())),
                    pos.clone(),
                ),
                Or(f1, f2, pos) => Or(
                    Box::new(f1.subst(var, e.clone())),
                    Box::new(f2.subst(var, e.clone())),
                    pos.clone(),
                ),
                Imply(f1, f2, pos) => Imply(
                    Box::new(f1.subst(var, e.clone())),
                    Box::new(f2.subst(var, e.clone())),
                    pos.clone(),
                ),
                BinApp(op, e1, e2, pos) => BinApp(
                    op.clone(),
                    e1.subst(var, e.clone()),
                    e2.subst(var, e),
                    pos.clone(),
                ),
            }
        }

        pub fn free_vars(&self) -> Vec<Ident> {
            use Formula::*;
            match self {
                True(_) | False(_) => vec![],
                Not(f, _) => f.free_vars(),
                And(f1, f2, _) | Or(f1, f2, _) | Imply(f1, f2, _) => {
                    let mut result = vec![];
                    result.append(&mut f1.free_vars());
                    result.append(&mut f2.free_vars());
                    result
                }
                BinApp(_, e1, e2, _) => {
                    let mut result = vec![];
                    result.append(&mut e1.free_vars());
                    result.append(&mut e2.free_vars());
                    result
                }
            }
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
        pub fn subst(&self, var: &Ident, e: Expr) -> Expr {
            use self::Expr::*;
            match self {
                Var(ident) if var == ident => e,
                Var(ident) => Var(ident.clone()),
                Constant(c) => Constant(c.clone()),
                BinApp(op, e1, e2, pos) => BinApp(
                    op.clone(),
                    Box::new(e1.subst(var, e.clone())),
                    Box::new(e2.subst(var, e)),
                    pos.clone(),
                ),
            }
        }

        pub fn free_vars(&self) -> Vec<Ident> {
            use self::Expr::*;
            match self {
                Var(ident) => vec![ident.clone()],
                Constant(_) => vec![],
                BinApp(_, e1, e2, _) => {
                    let mut result = vec![];
                    result.append(&mut e1.free_vars());
                    result.append(&mut e2.free_vars());
                    result
                }
            }
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

impl Type {
    pub fn subst_logical_expr(self, ident: &Ident, e: logic::Expr) -> Type {
        match self {
            Type::FuncType(FuncType { .. }) => {
                // 現状，これを使う例がないので必要になったときに実装する
                todo!()
            }
            Type::NonFuncType(NonFuncType {
                param_name,
                base_type,
                formula,
                pos,
            }) => Type::NonFuncType(NonFuncType {
                param_name,
                base_type,
                formula: formula.subst(ident, e),
                pos,
            }),
        }
    }
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
    pub fn new(val: i64) -> Self {
        Self {
            val,
            pos: PosInfo::dummy(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Constant(Constant),
    Var(Ident),
    BinApp(BinOp, Box<Expr>, Box<Expr>, PosInfo),
    Ifz(Box<Expr>, Box<Expr>, Box<Expr>, PosInfo),
    Let(Ident, Box<Expr>, Box<Expr>, PosInfo),
    FuncApp(Ident, Vec<Expr>, PosInfo),
}
