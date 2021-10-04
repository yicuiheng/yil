#[cfg(test)]
use std::collections::HashMap;

#[cfg(test)]
use crate::ast::*;

#[cfg(test)]
use super::*;

#[test]
fn preprocess_test() {
    // preprocess(let x = y in y) = let x = y in y
    let e1 = Expr::Let(
        Ident::new("x"),
        Box::new(Expr::Var(Ident::new("y"))),
        Box::new(Expr::Var(Ident::new("y"))),
        PosInfo::dummy(),
    );
    Ident::reset_fresh_count();
    assert_eq!(e1, util::preprocess(e1.clone()));

    // preprocess(f (1 + 2)) =
    //   let fresh.2 =
    //     let fresh.0 = 1 in
    //     let fresh.1 = 2 in
    //     fresh.0 + fresh.1
    //   in
    //   f fresh.2
    let e2 = Expr::FuncApp(
        Ident::new("f"),
        vec![Expr::BinApp(
            BinOp::Add(PosInfo::dummy()),
            Box::new(Expr::Constant(Constant::new(1))),
            Box::new(Expr::Constant(Constant::new(2))),
            PosInfo::dummy(),
        )],
        PosInfo::dummy(),
    );
    let e2_expected = Expr::Let(
        Ident::new("_fresh.2"),
        Box::new(Expr::Let(
            Ident::new("_fresh.0"),
            Box::new(Expr::Constant(Constant::new(1))),
            Box::new(Expr::Let(
                Ident::new("_fresh.1"),
                Box::new(Expr::Constant(Constant::new(2))),
                Box::new(Expr::BinApp(
                    BinOp::Add(PosInfo::dummy()),
                    Box::new(Expr::Var(Ident::new("_fresh.0"))),
                    Box::new(Expr::Var(Ident::new("_fresh.1"))),
                    PosInfo::dummy(),
                )),
                PosInfo::dummy(),
            )),
            PosInfo::dummy(),
        )),
        Box::new(Expr::FuncApp(
            Ident::new("f"),
            vec![Expr::Var(Ident::new("_fresh.2"))],
            PosInfo::dummy(),
        )),
        PosInfo::dummy(),
    );
    Ident::reset_fresh_count();
    assert_eq!(util::preprocess(e2), e2_expected);
}

#[test]
fn typecheck_program_success_test() {
    Ident::reset_fresh_count();
    assert_eq!(
        program(&Program {
            funcs: vec![Func {
                name: Ident::new("f"),
                params: vec![Type::NonFuncType(NonFuncType {
                    param_name: Ident::new("x"),
                    base_type: BaseType::Int(PosInfo::dummy()),
                    formula: logic::Formula::True(PosInfo::dummy()),
                    pos: PosInfo::dummy()
                })],
                ret: Type::NonFuncType(NonFuncType {
                    param_name: Ident::new("y"),
                    base_type: BaseType::Int(PosInfo::dummy()),
                    formula: logic::Formula::True(PosInfo::dummy()),
                    pos: PosInfo::dummy()
                }),
                is_rec: true,
                body: Expr::Var(Ident::new("x")),
                pos: PosInfo::dummy()
            }],
            pos: PosInfo::dummy(),
        }),
        Ok(())
    );
}

#[cfg(test)]
fn check_expr_type(e: Expr, typ: Type, type_env: &HashMap<Ident, Type>) {
    Ident::reset_fresh_count();
    let mut constraints = vec![];
    let (typ_, type_env) = typecheck_expr(&e, &type_env, &mut constraints).unwrap();
    constraints::add_subtype_constraint(&typ_, &typ, &type_env, &mut constraints);

    assert_eq!(constraints::solve_constraints(constraints), Ok(()));
}

#[test]
fn typecheck_expr_test() {
    let mut type_env = HashMap::new();

    // a: {a: int | a >= 0}
    type_env.insert(
        Ident::new("a"),
        Type::NonFuncType(NonFuncType {
            param_name: Ident::new("a"),
            base_type: BaseType::Int(PosInfo::dummy()),
            formula: logic::Formula::BinApp(
                logic::BinPred::Geq(PosInfo::dummy()),
                logic::Expr::Var(Ident::new("a")),
                logic::Expr::Constant(Constant::new(0)),
                PosInfo::dummy(),
            ),
            pos: PosInfo::dummy(),
        }),
    );

    Ident::reset_fresh_count();
    // let b = a in b + 1 : {b: int | b >= 1}
    check_expr_type(
        util::preprocess(Expr::Let(
            Ident::new("b"),
            Box::new(Expr::Var(Ident::new("a"))),
            Box::new(Expr::BinApp(
                BinOp::Add(PosInfo::dummy()),
                Box::new(Expr::Var(Ident::new("b"))),
                Box::new(Expr::Constant(Constant::new(1))),
                PosInfo::dummy(),
            )),
            PosInfo::dummy(),
        )),
        Type::NonFuncType(NonFuncType {
            param_name: Ident::new("b"),
            base_type: BaseType::Int(PosInfo::dummy()),
            formula: logic::Formula::BinApp(
                logic::BinPred::Geq(PosInfo::dummy()),
                logic::Expr::Var(Ident::new("b")),
                logic::Expr::Constant(Constant::new(1)),
                PosInfo::dummy(),
            ),
            pos: PosInfo::dummy(),
        }),
        &type_env,
    );
}
