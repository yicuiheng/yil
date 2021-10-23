use crate::{ast::*, env::TypeEnv, smt, typecheck::TypeError};

pub fn add_subtype_constraint(
    type1: &Type,
    type2: &Type,
    type_env: &TypeEnv,
    constraints: &mut Vec<(logic::Term, (Pos, Pos))>,
    range: (Pos, Pos),
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
            constraints.push((
                logic::Term::Bin(
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
                range,
            ));
        }
        (Type::FuncType(_, type11, type12, _), Type::FuncType(_, type21, type22, _)) => {
            add_subtype_constraint(type12, type22, type_env, constraints, range);
            add_subtype_constraint(type21, type11, type_env, constraints, range);
        }
        _ => unreachable!(),
    }
}

pub fn solve_constraints(constraints: Vec<(logic::Term, (Pos, Pos))>) -> Result<(), TypeError> {
    for (constraint, (start, end)) in constraints {
        if !smt::check_validity(constraint.clone())? {
            return Err(TypeError::NotValidConstraint(constraint, (start, end)));
        }
    }
    Ok(())
}
