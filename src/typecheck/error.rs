use crate::ast::*;
use crate::smt::error::SmtError;

#[derive(Debug, PartialEq, Eq)]
pub enum TypeError {
    UnboundVariable(Ident),
    BinAppSimpleTypeError {
        op: BinOp,
        type1: Type,
        type2: Type,
        pos: PosInfo,
    },
    FuncAppError(FuncType, Vec<Expr>),
    FuncExpected(Type, PosInfo),
    IfzCondMustBeInt(Expr),
    NotValidConstraint(logic::Formula),
    SmtError(SmtError),
}

impl From<SmtError> for TypeError {
    fn from(item: SmtError) -> Self {
        Self::SmtError(item)
    }
}
