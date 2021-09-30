use crate::ast::*;
use crate::smt;
use crate::typecheck::TypeError;

pub fn add_subtype_constraint(type1: &Type, type2: &Type, constraints: &mut Vec<logic::Formula>) {
    match (type1, type2) {
        (
            Type::NonFuncType(NonFuncType {
                param_name: param_name1,
                base_type: BaseType::Int(_),
                formula: formula1,
                ..
            }),
            Type::NonFuncType(NonFuncType {
                param_name: param_name2,
                base_type: BaseType::Int(_),
                formula: formula2,
                ..
            }),
        ) => {
            let fresh_name = Ident::fresh();
            let formula1 = formula1.subst(param_name1, logic::Expr::Var(fresh_name.clone()));
            let formula2 = formula2.subst(param_name2, logic::Expr::Var(fresh_name));
            constraints.push(logic::Formula::Imply(
                Box::new(formula1.clone()),
                Box::new(formula2.clone()),
                PosInfo::dummy(),
            ));
        }
        _ => todo!(),
    }
}

pub fn solve_constraints(constraints: Vec<logic::Formula>) -> Result<(), TypeError> {
    for constraint in constraints {
        if !smt::check_validity(&constraint)? {
            return Err(TypeError::NotValidConstraint(constraint));
        }
    }
    Ok(())
}
