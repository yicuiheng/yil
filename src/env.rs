use std::collections::{hash_map, HashMap};
use std::iter::{FromIterator, IntoIterator};

use crate::ast::*;

#[derive(Debug, Clone)]
pub struct Env<T>(HashMap<Ident, T>);

pub type NameEnv = Env<String>;
pub type SimpleTypeEnv = Env<SimpleType>;
pub type TypeEnv = Env<Type>;

impl<T> PartialEq for Env<T> {
    fn eq(&self, other: &Self) -> bool {
        return self.0.len() == other.0.len()
            && self.0.keys().all(|ident| other.0.contains_key(ident));
    }
}

impl<T> FromIterator<(Ident, T)> for Env<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (Ident, T)>,
    {
        Self(HashMap::from_iter(iter))
    }
}

impl<T> IntoIterator for Env<T> {
    type Item = (Ident, T);
    type IntoIter = hash_map::IntoIter<Ident, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Env<T> {
    type Item = (&'a Ident, &'a T);
    type IntoIter = hash_map::Iter<'a, Ident, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T: std::fmt::Debug> Env<T> {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, ident: Ident, v: T) {
        self.0.insert(ident, v);
    }

    pub fn append(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn drop(&mut self, ident: Ident) {
        self.0.remove(&ident);
    }

    pub fn lookup(&self, ident: Ident) -> &T {
        self.0.get(&ident).unwrap()
    }

    pub fn try_lookup(&self, ident: Ident) -> Option<&T> {
        self.0.get(&ident)
    }

    pub fn idents<'a>(&'a self) -> hash_map::Keys<'a, Ident, T> {
        self.0.keys()
    }
}

impl From<TypeEnv> for SimpleTypeEnv {
    fn from(type_env: TypeEnv) -> SimpleTypeEnv {
        let mut simple_type_env = SimpleTypeEnv::empty();
        for (ident, typ) in type_env.0 {
            simple_type_env.insert(ident, SimpleType::from(typ))
        }
        simple_type_env
    }
}

impl NameEnv {
    pub fn print(&self) {
        eprintln!("NameEnv {{");
        for (ident, name) in &self.0 {
            eprintln!("  {}: {}", ident.id, name);
        }
        eprintln!("}}");
    }
}

impl<T: Node> Env<T> {
    pub fn print(&self, name_env: &NameEnv) {
        eprintln!("Env {{");
        for (ident, v) in &self.0 {
            eprintln!(
                "  {}: {}",
                if let Some(name) = name_env.try_lookup(*ident) {
                    name.clone()
                } else {
                    ident.logical_symbol()
                },
                v.to_readable_string(name_env)
            );
        }
        eprintln!("}}");
    }
}
