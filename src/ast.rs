pub mod builtin;
pub mod logic;

use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};

use crate::env::NameEnv;

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

pub type Range = (Pos, Pos);

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
    #[cfg(not(test))]
    pub fn as_range(self) -> (Pos, Pos) {
        match self {
            Info::Range(start, end) => (start, end),
            _ => unreachable!(),
        }
    }

    #[cfg(test)]
    pub fn as_range(self) -> (Pos, Pos) {
        (Pos { line: 0, col: 0 }, Pos { line: 0, col: 0 })
    }

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
    pub is_builtin: bool,
}

static FRESH_IDENT_COUNT: AtomicUsize = AtomicUsize::new(0);

impl Ident {
    pub fn fresh() -> Self {
        let id = FRESH_IDENT_COUNT.fetch_add(1, SeqCst);
        Self {
            id,
            is_builtin: false,
        }
    }

    pub fn builtin_fresh() -> Self {
        let id = FRESH_IDENT_COUNT.fetch_add(1, SeqCst);
        Self {
            id,
            is_builtin: true,
        }
    }

    pub fn with_id(id: usize) -> Self {
        Self {
            id,
            is_builtin: false,
        }
    }

    pub fn logical_symbol(&self) -> String {
        format!("t{}", self.id)
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

pub trait Node {
    fn to_readable_string(&self, name_env: &NameEnv) -> String;
    fn info(&self) -> &Info;
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub funcs: Vec<Func>,
    pub info: Info,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SimpleType {
    FuncType(Box<SimpleType>, Box<SimpleType>),
    IntType,
}

impl std::fmt::Display for SimpleType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SimpleType::IntType => write!(fmt, "int"),
            SimpleType::FuncType(from, to) => write!(fmt, "({} -> {})", from, to),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    FuncType(Ident, Box<Type>, Box<Type>, Info),
    IntType(Ident, logic::Term, Info),
}

impl Node for Type {
    fn to_readable_string(&self, name_env: &NameEnv) -> String {
        match self {
            Type::FuncType(ident, type1, type2, _) => {
                if let Some(name) = name_env.try_lookup(*ident) {
                    format!(
                        "({}: {} -> {})",
                        name,
                        type1.to_readable_string(name_env),
                        type2.to_readable_string(name_env)
                    )
                } else {
                    format!(
                        "{} -> {}",
                        type1.to_readable_string(name_env),
                        type2.to_readable_string(name_env)
                    )
                }
            }
            Type::IntType(ident, term, _) => {
                if let Some(name) = name_env.try_lookup(*ident) {
                    format!("({}:int | {})", name, term.to_readable_string(name_env))
                } else {
                    format!(
                        "({}:int | {})",
                        ident.logical_symbol(),
                        term.to_readable_string(name_env)
                    )
                }
            }
        }
    }

    fn info(&self) -> &Info {
        match self {
            Type::FuncType(_, _, _, info) => info,
            Type::IntType(_, _, info) => info,
        }
    }
}

impl Type {
    pub fn ident(&self) -> Ident {
        match self {
            Type::FuncType(ident, _, _, _) => *ident,
            Type::IntType(ident, _, _) => *ident,
        }
    }

    pub fn subst(self, ident: Ident, t: &logic::Term) -> Self {
        match self {
            Type::FuncType(param_ident, param_type, ret_type, info) if param_ident == ident => {
                Type::FuncType(param_ident, param_type, ret_type, info)
            }
            Type::FuncType(param_ident, param_type, ret_type, info) => Type::FuncType(
                param_ident,
                Box::new(param_type.subst(ident, t)),
                Box::new(ret_type.subst(ident, t)),
                info,
            ),
            Type::IntType(ident_, term_, info_) if ident_ == ident => {
                Type::IntType(ident_, term_, info_)
            }
            Type::IntType(ident_, term_, info) => {
                Type::IntType(ident_, term_.subst(ident, t), info)
            }
        }
    }
}

impl From<&Type> for SimpleType {
    fn from(typ: &Type) -> SimpleType {
        match typ {
            Type::FuncType(_, type1, type2, _) => SimpleType::FuncType(
                Box::new(SimpleType::from(&**type1)),
                Box::new(SimpleType::from(&**type2)),
            ),
            Type::IntType(_, _, _) => SimpleType::IntType,
        }
    }
}

impl From<Type> for SimpleType {
    fn from(typ: Type) -> SimpleType {
        match typ {
            Type::FuncType(_, type1, type2, _) => SimpleType::FuncType(
                Box::new(SimpleType::from(*type1)),
                Box::new(SimpleType::from(*type2)),
            ),
            Type::IntType(_, _, _) => SimpleType::IntType,
        }
    }
}

impl SimpleType {
    pub fn is_func(&self) -> bool {
        match self {
            SimpleType::FuncType(_, _) => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            SimpleType::IntType => true,
            _ => false,
        }
    }

    pub fn as_func(&self) -> (&SimpleType, &SimpleType) {
        match self {
            SimpleType::FuncType(type1, type2) => (type1, type2),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Func {
    pub typ: Type,
    pub params_len: usize, // どこからが関数の返り値の型かを知るために必要
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

impl Node for Expr {
    fn to_readable_string(&self, name_env: &NameEnv) -> String {
        match self {
            Expr::Num(n, _) => n.to_string(),
            Expr::Var(ident, _) => name_env.lookup(*ident).clone(),
            Expr::Ifz(cond, expr1, expr2, _) => format!(
                "ifz {} {{ {} }} else {{ {} }}",
                cond.to_readable_string(name_env),
                expr1.to_readable_string(name_env),
                expr2.to_readable_string(name_env)
            ),
            Expr::Let(ident, expr1, expr2, _) => format!(
                "let {} = {} in {}",
                name_env.lookup(*ident),
                expr1.to_readable_string(name_env),
                expr2.to_readable_string(name_env)
            ),
            Expr::App(expr1, expr2, _) => format!(
                "({} {})",
                expr1.to_readable_string(name_env),
                expr2.to_readable_string(name_env)
            ),
        }
    }

    fn info(&self) -> &Info {
        match self {
            Expr::Num(_, info)
            | Expr::Var(_, info)
            | Expr::Ifz(_, _, _, info)
            | Expr::Let(_, _, _, info)
            | Expr::App(_, _, info) => info,
        }
    }
}
