pub mod builtin;
pub mod logic;

use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Info {
    Range(Pos, Pos),
    Dummy,
}

impl PartialEq for Info {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Info::Range(p11, p12), Info::Range(p21, p22)) => p11 == p21 && p12 == p22,
            _ => true,
        }
    }
}

impl Eq for Info {}

impl Info {
    pub fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Range(start_pos, _), Self::Range(_, end_pos)) => Info::Range(start_pos, end_pos),
            _ => Self::Dummy,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Ident {
    pub id: usize,
}

static FRESH_IDENT_COUNT: AtomicUsize = AtomicUsize::new(0);

impl Ident {
    pub fn fresh() -> Self {
        let id = FRESH_IDENT_COUNT.fetch_add(1, SeqCst);
        Self { id }
    }

    #[cfg(test)]
    pub fn current_count() -> usize {
        FRESH_IDENT_COUNT.load(SeqCst)
    }

    #[cfg(test)]
    pub fn set_fresh_count(n: usize) {
        FRESH_IDENT_COUNT.store(n, SeqCst)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub funcs: Vec<Func>,
    pub info: Info,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    FuncType(Ident, Box<Type>, Box<Type>, Info),
    IntType(Ident, logic::Term, Info),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Func {
    pub ident: Ident,
    pub typ: Type,
    pub is_rec: bool,
    pub body: Expr,
    pub info: Info,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i32, Info),
    Var(Ident, Info),
    Ifz(Box<Expr>, Box<Expr>, Box<Expr>, Info),
    Let(Ident, Box<Expr>, Box<Expr>, Info),
    App(Box<Expr>, Box<Expr>, Info),
}
