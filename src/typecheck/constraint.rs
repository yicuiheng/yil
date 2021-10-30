use crate::{ast::*, env::TypeEnv, smt, typecheck::TypeError};
use std::collections::HashMap;

pub struct Constraint {
    pub term: logic::Term,
    pub spec_range: Range,
    pub impl_range: Range,
}
pub fn add_subtype_constraint(
    type1: &Type,
    type2: &Type,
    type_env: &TypeEnv,
    constraints: &mut Vec<Constraint>,
    spec_range: Range,
    impl_range: Range,
) {
    let env_term =
        type_env
            .clone()
            .into_iter()
            .fold(logic::Term::True(Info::Dummy), |acc, (_, typ)| {
                if let Type::IntType(_, term, _) = typ {
                    logic::Term::Bin(
                        logic::BinOp::And,
                        Box::new(acc),
                        Box::new(term),
                        Info::Dummy,
                    )
                } else {
                    acc
                }
            });

    match (type1, type2) {
        (Type::IntType(ident1, term1, _), Type::IntType(ident2, term2, _)) => {
            let term1 = term1.clone();
            let term2 = term2
                .clone()
                .subst(*ident2, &logic::Term::Var(*ident1, Info::Dummy));
            constraints.push(Constraint {
                term: logic::Term::Bin(
                    logic::BinOp::Imply,
                    Box::new(logic::Term::Bin(
                        logic::BinOp::And,
                        Box::new(env_term),
                        Box::new(term1),
                        Info::Dummy,
                    )),
                    Box::new(term2),
                    Info::Dummy,
                ),
                spec_range,
                impl_range,
            });
        }
        (Type::FuncType(_, type11, type12, _), Type::FuncType(_, type21, type22, _)) => {
            add_subtype_constraint(
                type12,
                type22,
                type_env,
                constraints,
                spec_range,
                impl_range,
            );
            add_subtype_constraint(
                type21,
                type11,
                type_env,
                constraints,
                spec_range,
                impl_range,
            );
        }
        _ => unreachable!(),
    }
}

pub fn solve_constraints(constraints: Vec<Constraint>) -> Result<(), TypeError> {
    for constraint in constraints {
        match smt::check_validity(constraint.term.clone()) {
            smt::SmtResult::CounterExamplesFound(counter_examples) => {
                return Err(TypeError::NotValidConstraint {
                    counter_examples,
                    spec_range: constraint.spec_range,
                    impl_range: constraint.impl_range,
                });
            }
            smt::SmtResult::Unknown => {
                return Err(TypeError::NotValidConstraint {
                    counter_examples: HashMap::new(),
                    spec_range: constraint.spec_range,
                    impl_range: constraint.impl_range,
                });
            }
            smt::SmtResult::Valid => (),
        }
    }
    Ok(())
}


