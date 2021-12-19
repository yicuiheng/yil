#[cfg(test)]
use crate::{
    ast::{builtin::BuiltinData, logic::*, *},
    env::TypeEnv,
    typecheck::*,
};

#[cfg(test)]
fn check_well_typed_expr(e: Expr, typ: Type, type_env: &TypeEnv) {
    let mut constraints = vec![];
    let (typ_, type_env) = typecheck_expr(&e, &type_env, &mut constraints).unwrap();
    let dummy_pos = Pos { line: 0, col: 0 };
    let dummy_range = Range {
        start: dummy_pos,
        end: dummy_pos,
    };
    constraint::add_subtype_constraint(
        &typ_,
        &typ,
        &type_env,
        &mut constraints,
        dummy_range,
        dummy_range,
    );

    assert_eq!(constraint::solve_constraints(constraints), Ok(()));
}

#[cfg(test)]
fn check_ill_typed_expr(e: Expr, typ: Type, type_env: &TypeEnv) {
    let mut constraints = vec![];
    let (typ_, type_env) = typecheck_expr(&e, &type_env, &mut constraints).unwrap();
    let dummy_pos = Pos { line: 0, col: 0 };
    let dummy_range = Range {
        start: dummy_pos,
        end: dummy_pos,
    };

    constraint::add_subtype_constraint(
        &typ_,
        &typ,
        &type_env,
        &mut constraints,
        dummy_range,
        dummy_range,
    );
    if let Ok(_) = constraint::solve_constraints(constraints) {
        panic!()
    }
}

#[test]
fn typecheck_expr_test() {
    let builtin = BuiltinData::instance();
    let mut type_env = BuiltinData::type_env();
    let a_ident = Ident::fresh();
    let init_id = Ident::current_count();

    // a: {a: int | a >= 0}
    type_env.insert(
        a_ident,
        Type::BaseType(
            a_ident,
            BaseTypeKind::Int,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(a_ident, Info::dummy())),
                Box::new(Term::Num(0, Info::dummy())),
                Info::dummy(),
            ),
            Info::dummy(),
        ),
    );

    // number literal
    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    check_well_typed_expr(
        Expr::Num(42, Info::dummy()),
        Type::BaseType(
            ret_ident,
            BaseTypeKind::Int,
            Term::Bin(
                BinOp::Eq,
                Box::new(Term::Var(ret_ident, Info::dummy())),
                Box::new(Term::Num(42, Info::dummy())),
                Info::dummy(),
            ),
            Info::dummy(),
        ),
        &type_env,
    );

    // boolean literal
    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    check_well_typed_expr(
        Expr::Boolean(false, Info::dummy()),
        Type::BaseType(
            ret_ident,
            BaseTypeKind::Bool,
            Term::Not(Box::new(Term::Var(ret_ident, Info::dummy())), Info::dummy()),
            Info::dummy(),
        ),
        &type_env,
    );

    // variable
    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    check_well_typed_expr(
        Expr::Var(a_ident, Info::dummy()),
        Type::BaseType(
            ret_ident,
            BaseTypeKind::Int,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ret_ident, Info::dummy())),
                Box::new(Term::Num(-1, Info::dummy())),
                Info::dummy(),
            ),
            Info::dummy(),
        ),
        &type_env,
    );

    // a + 1 : {ret: int | ret >= 1 }
    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    check_well_typed_expr(
        Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                Box::new(Expr::Var(a_ident, Info::dummy())),
                Info::dummy(),
            )),
            Box::new(Expr::Num(1, Info::dummy())),
            Info::dummy(),
        ),
        Type::BaseType(
            ret_ident,
            BaseTypeKind::Int,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ret_ident, Info::dummy())),
                Box::new(Term::Num(1, Info::dummy())),
                Info::dummy(),
            ),
            Info::dummy(),
        ),
        &type_env,
    );

    // let b = 41 in b + 1 : {ret: int | ret = 43}
    Ident::set_fresh_count(init_id);
    let b_ident = Ident::fresh();
    let ret_ident = Ident::fresh();
    check_well_typed_expr(
        Expr::Let(
            b_ident,
            Box::new(Expr::Num(41, Info::dummy())),
            Box::new(Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                    Box::new(Expr::Var(b_ident, Info::dummy())),
                    Info::dummy(),
                )),
                Box::new(Expr::Num(1, Info::dummy())),
                Info::dummy(),
            )),
            Info::dummy(),
        ),
        Type::BaseType(
            ret_ident,
            BaseTypeKind::Int,
            Term::True(Info::dummy()),
            Info::dummy(),
        ),
        &type_env,
    );

    // let b = a in b + 1 : {ret: int | ret >= 1}
    Ident::set_fresh_count(init_id);
    let b_ident = Ident::fresh();
    let ret_ident = Ident::fresh();
    check_well_typed_expr(
        Expr::Let(
            b_ident,
            Box::new(Expr::Var(a_ident, Info::dummy())),
            Box::new(Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                    Box::new(Expr::Var(b_ident, Info::dummy())),
                    Info::dummy(),
                )),
                Box::new(Expr::Num(1, Info::dummy())),
                Info::dummy(),
            )),
            Info::dummy(),
        ),
        Type::BaseType(
            ret_ident,
            BaseTypeKind::Int,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ret_ident, Info::dummy())),
                Box::new(Term::Num(1, Info::dummy())),
                Info::dummy(),
            ),
            Info::dummy(),
        ),
        &type_env,
    );

    // a + 2 : {ret: int | ret >= 3}
    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    check_ill_typed_expr(
        Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                Box::new(Expr::Var(a_ident, Info::dummy())),
                Info::dummy(),
            )),
            Box::new(Expr::Num(2, Info::dummy())),
            Info::dummy(),
        ),
        Type::BaseType(
            ret_ident,
            BaseTypeKind::Int,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ret_ident, Info::dummy())),
                Box::new(Term::Num(3, Info::dummy())),
                Info::dummy(),
            ),
            Info::dummy(),
        ),
        &type_env,
    );

    // a + 2 : {ret: int | ret >= a + 3 }
    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    check_ill_typed_expr(
        Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                Box::new(Expr::Var(a_ident, Info::dummy())),
                Info::dummy(),
            )),
            Box::new(Expr::Num(2, Info::dummy())),
            Info::dummy(),
        ),
        Type::BaseType(
            ret_ident,
            BaseTypeKind::Int,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ret_ident, Info::dummy())),
                Box::new(Term::Bin(
                    BinOp::Add,
                    Box::new(Term::Var(a_ident, Info::dummy())),
                    Box::new(Term::Num(3, Info::dummy())),
                    Info::dummy(),
                )),
                Info::dummy(),
            ),
            Info::dummy(),
        ),
        &type_env,
    );

    // (1 + 2) >= 3 : {ret: bool | ret}
    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    check_well_typed_expr(
        Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var(builtin.geq_ident, Info::dummy())),
                Box::new(Expr::App(
                    Box::new(Expr::App(
                        Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                        Box::new(Expr::Num(2, Info::dummy())),
                        Info::dummy(),
                    )),
                    Box::new(Expr::Num(2, Info::dummy())),
                    Info::dummy(),
                )),
                Info::dummy(),
            )),
            Box::new(Expr::Num(3, Info::dummy())),
            Info::dummy(),
        ),
        Type::BaseType(
            ret_ident,
            BaseTypeKind::Bool,
            Term::Var(ret_ident, Info::dummy()),
            Info::dummy(),
        ),
        &type_env,
    );
}
