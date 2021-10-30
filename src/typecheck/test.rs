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
    let dummy_range = (dummy_pos, dummy_pos);
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
    let dummy_range = (dummy_pos, dummy_pos);
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
        Type::IntType(
            a_ident,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(a_ident, Info::Dummy)),
                Box::new(Term::Num(0, Info::Dummy)),
                Info::Dummy,
            ),
            Info::Dummy,
        ),
    );

    // variable
    check_well_typed_expr(
        Expr::Var(a_ident, Info::Dummy),
        Type::IntType(
            a_ident,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(a_ident, Info::Dummy)),
                Box::new(Term::Num(-1, Info::Dummy)),
                Info::Dummy,
            ),
            Info::Dummy,
        ),
        &type_env,
    );

    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    // a + 1 : {ret: int | ret >= 1 }
    check_well_typed_expr(
        Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                Box::new(Expr::Var(a_ident, Info::Dummy)),
                Info::Dummy,
            )),
            Box::new(Expr::Num(1, Info::Dummy)),
            Info::Dummy,
        ),
        Type::IntType(
            ret_ident,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ret_ident, Info::Dummy)),
                Box::new(Term::Num(1, Info::Dummy)),
                Info::Dummy,
            ),
            Info::Dummy,
        ),
        &type_env,
    );

    Ident::set_fresh_count(init_id);
    let b_ident = Ident::fresh();
    let ret_ident = Ident::fresh();
    // let b = 41 in b + 1 : {ret: int | ret = 43}
    check_well_typed_expr(
        Expr::Let(
            b_ident,
            Box::new(Expr::Num(41, Info::Dummy)),
            Box::new(Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                    Box::new(Expr::Var(b_ident, Info::Dummy)),
                    Info::Dummy,
                )),
                Box::new(Expr::Num(1, Info::Dummy)),
                Info::Dummy,
            )),
            Info::Dummy,
        ),
        Type::IntType(ret_ident, Term::True(Info::Dummy), Info::Dummy),
        &type_env,
    );

    Ident::set_fresh_count(init_id);
    // let b = a in b + 1 : {ret: int | ret >= 1}
    let b_ident = Ident::fresh();
    let ret_ident = Ident::fresh();
    check_well_typed_expr(
        Expr::Let(
            b_ident,
            Box::new(Expr::Var(a_ident, Info::Dummy)),
            Box::new(Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                    Box::new(Expr::Var(b_ident, Info::Dummy)),
                    Info::Dummy,
                )),
                Box::new(Expr::Num(1, Info::Dummy)),
                Info::Dummy,
            )),
            Info::Dummy,
        ),
        Type::IntType(
            ret_ident,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ret_ident, Info::Dummy)),
                Box::new(Term::Num(1, Info::Dummy)),
                Info::Dummy,
            ),
            Info::Dummy,
        ),
        &type_env,
    );

    Ident::set_fresh_count(init_id);

    let ret_ident = Ident::fresh();
    // a + 2 : {ret: int | ret >= 3}
    check_ill_typed_expr(
        Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                Box::new(Expr::Var(a_ident, Info::Dummy)),
                Info::Dummy,
            )),
            Box::new(Expr::Num(2, Info::Dummy)),
            Info::Dummy,
        ),
        Type::IntType(
            ret_ident,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ret_ident, Info::Dummy)),
                Box::new(Term::Num(3, Info::Dummy)),
                Info::Dummy,
            ),
            Info::Dummy,
        ),
        &type_env,
    );

    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    // a + 2 : {ret: int | ret >= a + 3 }
    check_ill_typed_expr(
        Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                Box::new(Expr::Var(a_ident, Info::Dummy)),
                Info::Dummy,
            )),
            Box::new(Expr::Num(2, Info::Dummy)),
            Info::Dummy,
        ),
        Type::IntType(
            ret_ident,
            Term::Bin(
                BinOp::Geq,
                Box::new(Term::Var(ret_ident, Info::Dummy)),
                Box::new(Term::Bin(
                    BinOp::Add,
                    Box::new(Term::Var(a_ident, Info::Dummy)),
                    Box::new(Term::Num(3, Info::Dummy)),
                    Info::Dummy,
                )),
                Info::Dummy,
            ),
            Info::Dummy,
        ),
        &type_env,
    );

    Ident::set_fresh_count(init_id);
    let ret_ident = Ident::fresh();
    // (1 + 2) >= 3 : {ret: int | ret = 0}
    check_well_typed_expr(
        Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var(builtin.geq_ident, Info::Dummy)),
                Box::new(Expr::App(
                    Box::new(Expr::App(
                        Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                        Box::new(Expr::Num(2, Info::Dummy)),
                        Info::Dummy,
                    )),
                    Box::new(Expr::Num(2, Info::Dummy)),
                    Info::Dummy,
                )),
                Info::Dummy,
            )),
            Box::new(Expr::Num(3, Info::Dummy)),
            Info::Dummy,
        ),
        Type::IntType(
            ret_ident,
            Term::Bin(
                BinOp::Eq,
                Box::new(Term::Var(ret_ident, Info::Dummy)),
                Box::new(Term::Num(0, Info::Dummy)),
                Info::Dummy,
            ),
            Info::Dummy,
        ),
        &type_env,
    );
}
