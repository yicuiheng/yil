use crate::ast::*;
use crate::smt::error::SmtError;

#[derive(Debug)]
pub enum TypeError {
    UnboundVariable(Ident),
    SimpleTypeError {
        type1: Type,
        type2: Type,
        pos: PosInfo,
    },
    FuncAppError(FuncType, Vec<Expr>),
    FuncExpected(Type, PosInfo),
    NotValidConstraint(logic::Formula),
    SmtError(SmtError),
}

impl From<SmtError> for TypeError {
    fn from(item: SmtError) -> Self {
        Self::SmtError(item)
    }
}
