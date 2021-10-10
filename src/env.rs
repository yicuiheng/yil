use std::collections::HashMap;

use crate::ast::*;

#[derive(Debug, Clone)]
pub struct Env<T>(HashMap<Ident, T>);

pub type NameEnv = Env<String>;
pub type TypeEnv = Env<Type>;
pub type ExprEnv = Env<Expr>;
pub type LogicalTermEnv = Env<logic::Term>;

impl<T> PartialEq for Env<T> {
    fn eq(&self, other: &Self) -> bool {
        return self.0.len() == other.0.len()
            && self.0.keys().all(|ident| other.0.contains_key(ident));
    }
}

impl<T> std::iter::FromIterator<(Ident, T)> for Env<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (Ident, T)>,
    {
        Self(HashMap::from_iter(iter))
    }
}

impl<T> Env<T> {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, id: Ident, v: T) {
        self.0.insert(id, v);
    }

    pub fn append(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn lookup(&self, id: Ident) -> &T {
        self.0.get(&id).unwrap()
    }

    pub fn try_lookup(&self, id: Ident) -> Option<&T> {
        self.0.get(&id)
    }
}
