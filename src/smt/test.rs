#[cfg(test)]
use crate::{
    ast::{
        logic::{BinOp, Term},
        Ident, Info,
    },
    smt::{check_validity, SmtResult},
};

#[test]
fn check_valid_term() {
    assert_eq!(check_validity(Term::True(Info::dummy())), SmtResult::Valid);
    assert_eq!(
        check_validity(Term::Not(
            Box::new(Term::False(Info::dummy())),
            Info::dummy()
        )),
        SmtResult::Valid
    );

    // 0 = 0
    assert_eq!(
        check_validity(Term::Bin(
            BinOp::Eq,
            Box::new(Term::Num(0, Info::dummy())),
            Box::new(Term::Num(0, Info::dummy())),
            Info::dummy()
        )),
        SmtResult::Valid
    );

    // (a = 1) => (a >= 0)
    let ident = Ident::fresh();
    assert_eq!(
        check_validity(Term::Bin(
            BinOp::Imply,
            Box::new(Term::Bin(
                BinOp::Eq,
                Box::new(Term::Var(ident, Info::dummy())),
                Box::new(Term::Num(1, Info::dummy())),
                Info::dummy()
            )),
            Box::new(Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ident, Info::dummy())),
                Box::new(Term::Num(0, Info::dummy())),
                Info::dummy()
            )),
            Info::dummy()
        )),
        SmtResult::Valid
    );
}

#[test]
fn check_not_valid_term() {
    assert_eq!(
        check_validity(Term::False(Info::dummy())),
        SmtResult::CounterExamplesFound(vec![].into_iter().collect())
    );
    assert_eq!(
        check_validity(Term::Not(
            Box::new(Term::True(Info::dummy())),
            Info::dummy()
        )),
        SmtResult::CounterExamplesFound(vec![].into_iter().collect())
    );

    let ident = Ident::fresh();

    // a = 0
    assert!(
        check_validity(Term::Bin(
            BinOp::Eq,
            Box::new(Term::Var(ident, Info::dummy())),
            Box::new(Term::Num(0, Info::dummy())),
            Info::dummy()
        )) != SmtResult::Valid
    );

    // (a = 0) => (a >= 1)
    assert!(
        check_validity(Term::Bin(
            BinOp::Imply,
            Box::new(Term::Bin(
                BinOp::Eq,
                Box::new(Term::Var(ident, Info::dummy())),
                Box::new(Term::Num(0, Info::dummy())),
                Info::dummy()
            )),
            Box::new(Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ident, Info::dummy())),
                Box::new(Term::Num(1, Info::dummy())),
                Info::dummy()
            )),
            Info::dummy()
        )) != SmtResult::Valid
    );
}
