use crate::{ast::*, smt::error::SmtError};

#[derive(Debug, PartialEq, Eq)]
pub enum TypeError {
    IfzBranchSimpleTypeError {
        type1: Type,
        type2: Type,
        info: Info,
    },
    FuncExpected(Type, Info),
    IfzCondMustBeInt(Expr),
    NotValidConstraint(logic::Term),
    SmtError(SmtError),
}

impl From<SmtError> for TypeError {
    fn from(item: SmtError) -> Self {
        Self::SmtError(item)
    }
}

pub fn print_error(e: TypeError, _: &str) {
    eprintln!("{:?}", e);
    todo!()
}
