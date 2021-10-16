pub mod error;
mod smtlib2;
mod z3;

use crate::ast::{logic::Term, Info};

use self::error::SmtError;

pub fn check_validity(term: Term) -> Result<bool, SmtError> {
    let mut free_idents = term.free_idents();
    free_idents.sort();
    free_idents.dedup();
    let neg_term = Term::Not(Box::new(term), Info::Dummy);
    let sexpr = smtlib2::term_to_sexpr(&neg_term);
    let smtlib2_query = smtlib2::make_smtlib2(sexpr, free_idents);
    z3::check_unsat(smtlib2_query)
}
