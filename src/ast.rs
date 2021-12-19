pub mod builtin;
pub mod logic;

use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};

use crate::env::NameEnv;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Range {
    pub start: Pos,
    pub end: Pos,
}

impl Range {
    pub fn merge(self, other: Self) -> Self {
        Self {
            start: Pos {
                line: self.start.line.min(other.start.line),
                col: self.start.col.min(other.start.col),
            },
            end: Pos {
                line: self.end.line.max(other.end.line),
                col: self.end.col.max(other.end.col),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, Eq)]
pub struct Info {
    pub range: Option<Range>,
    pub is_builtin: bool,
}

impl PartialEq for Info {
    #[cfg(not(test))]
    fn eq(&self, other: &Self) -> bool {
        match (self.range, other.range) {
            (Some(range1), Some(range2)) => range1 == range2,
            _ => true,
        }
    }

    #[cfg(test)]
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Info {
    pub fn builtin() -> Self {
        Self {
            range: None,
            is_builtin: true,
        }
    }

    pub fn builtin_info_from_range(range: Range) -> Self {
        Self {
            range: Some(range),
            is_builtin: true,
        }
    }

    pub fn from_range(range: Range) -> Self {
        Self {
            range: Some(range),
            is_builtin: false,
        }
    }

    #[cfg(test)]
    pub fn dummy() -> Self {
        Self {
            range: Some(Range {
                start: Pos { line: 0, col: 0 },
                end: Pos { line: 0, col: 0 },
            }),
            is_builtin: false,
        }
    }

    #[cfg(not(test))]
    pub fn dummy() -> Self {
        Self {
            range: None,
            is_builtin: false,
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

    pub fn with_id(id: usize) -> Self {
        Self { id }
    }

    pub fn logical_symbol(&self) -> String {
        format!("t{}", self.id)
    }

    #[cfg(test)]
    pub fn builtin_ident_with_id(id: usize) -> Self {
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

pub trait Node {
    fn to_readable_string(&self, name_env: &NameEnv) -> String;
    fn info(&self) -> &Info;
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub funcs: Vec<Func>,
    pub info: Info,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BaseTypeKind {
    Int,
    Bool,
}

impl std::fmt::Display for BaseTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseTypeKind::Int => write!(f, "int"),
            BaseTypeKind::Bool => write!(f, "bool"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SimpleType {
    FuncType(Box<SimpleType>, Box<SimpleType>),
    BaseType(BaseTypeKind),
}

impl std::fmt::Display for SimpleType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SimpleType::FuncType(from, to) => write!(fmt, "({} -> {})", from, to),
            SimpleType::BaseType(base_type) => write!(fmt, "{}", base_type),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    FuncType(Ident, Box<Type>, Box<Type>, Info),
    BaseType(Ident, BaseTypeKind, logic::Term, Info),
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
            Type::BaseType(ident, base_type, term, _) => {
                let logical_symbol = ident.logical_symbol();
                format!(
                    "({}:{} | {})",
                    if let Some(name) = name_env.try_lookup(*ident) {
                        name
                    } else {
                        &logical_symbol
                    },
                    base_type,
                    term.to_readable_string(name_env)
                )
            }
        }
    }

    fn info(&self) -> &Info {
        match self {
            Type::FuncType(_, _, _, info) => info,
            Type::BaseType(_, _, _, info) => info,
        }
    }
}

impl Type {
    pub fn ident(&self) -> Ident {
        match self {
            Type::FuncType(ident, _, _, _) => *ident,
            Type::BaseType(ident, _, _, _) => *ident,
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
            Type::BaseType(ident_, base_type, term, info) if ident_ == ident => {
                Type::BaseType(ident_, base_type, term, info)
            }
            Type::BaseType(ident_, base_type, term, info) => {
                Type::BaseType(ident_, base_type, term.subst(ident, t), info)
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
            Type::BaseType(_, base_type, _, _) => SimpleType::BaseType(*base_type),
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
            Type::BaseType(_, base_type, _, _) => SimpleType::BaseType(base_type),
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

    pub fn is_base(&self) -> bool {
        match self {
            SimpleType::BaseType(_) => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            SimpleType::BaseType(BaseTypeKind::Int) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            SimpleType::BaseType(BaseTypeKind::Bool) => true,
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
    Boolean(bool, Info),
    Var(Ident, Info),
    If(Box<Expr>, Box<Expr>, Box<Expr>, Info),
    Let(Ident, Box<Expr>, Box<Expr>, Info),
    App(Box<Expr>, Box<Expr>, Info),
}

impl Node for Expr {
    fn to_readable_string(&self, name_env: &NameEnv) -> String {
        match self {
            Expr::Num(n, _) => n.to_string(),
            Expr::Boolean(b, _) => b.to_string(),
            Expr::Var(ident, _) => name_env.lookup(*ident).clone(),
            Expr::If(cond, expr1, expr2, _) => format!(
                "if {} {{ {} }} else {{ {} }}",
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
            | Expr::Boolean(_, info)
            | Expr::Var(_, info)
            | Expr::If(_, _, _, info)
            | Expr::Let(_, _, _, info)
            | Expr::App(_, _, info) => info,
        }
    }
}
