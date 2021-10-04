pub mod error;
mod smtlib2;
mod z3;

use crate::ast::logic::Formula;

use self::error::SmtError;

pub fn check_validity(formula: &Formula) -> Result<bool, SmtError> {
    let neg_sexpr = smtlib2::make_not(smtlib2::formula_to_sexpr(formula));
    let mut free_vars = formula.free_vars();
    free_vars.sort();
    free_vars.dedup();
    z3::check_unsat(neg_sexpr, free_vars)
}
