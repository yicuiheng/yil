#[cfg(test)]
use std::collections::HashMap;

#[cfg(test)]
use crate::{ast::*, env::Env};

#[cfg(test)]
use super::{expr, expr_with_name_map, func, ident, program, typ};

#[test]
fn parse_program_test() {
    let _builtin = builtin::BuiltinData::instance();
    let init_id = Ident::current_count();
    let actual = program("func hoge (x: int | true): (y: int | true) ( 1 )  func fuga (a: int | false): (b: int | false) ( 2 )").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Program {
                funcs: vec![
                    Func {
                        ident: Ident { id: init_id },
                        typ: Type::FuncType(
                            Ident { id: init_id + 2 },
                            Box::new(Type::IntType(
                                Ident { id: init_id + 2 },
                                logic::Term::True(Info::Dummy),
                                Info::Dummy
                            )),
                            Box::new(Type::IntType(
                                Ident { id: init_id + 3 },
                                logic::Term::True(Info::Dummy),
                                Info::Dummy
                            )),
                            Info::Dummy,
                        ),
                        is_rec: false,
                        body: Expr::Num(1, Info::Dummy),
                        info: Info::Dummy,
                    },
                    Func {
                        ident: Ident { id: init_id + 1 },
                        typ: Type::FuncType(
                            Ident { id: init_id + 4 },
                            Box::new(Type::IntType(
                                Ident { id: init_id + 4 },
                                logic::Term::False(Info::Dummy),
                                Info::Dummy
                            )),
                            Box::new(Type::IntType(
                                Ident { id: init_id + 5 },
                                logic::Term::False(Info::Dummy),
                                Info::Dummy
                            )),
                            Info::Dummy,
                        ),
                        is_rec: false,
                        body: Expr::Num(2, Info::Dummy),
                        info: Info::Dummy,
                    }
                ],
                info: Info::Dummy,
            },
            vec![
                (Ident { id: init_id }, "hoge".to_string()),
                (Ident { id: init_id + 1 }, "fuga".to_string()),
                (Ident { id: init_id + 2 }, "x".to_string()),
                (Ident { id: init_id + 3 }, "y".to_string()),
                (Ident { id: init_id + 4 }, "a".to_string()),
                (Ident { id: init_id + 5 }, "b".to_string()),
            ]
            .into_iter()
            .collect()
        )
    );
}

#[test]
fn parse_func_test() {
    let _builtin = builtin::BuiltinData::instance();
    let init_id = Ident::current_count();
    let actual = func("rec func hoge (x: int | true): (y: int | true) ( 1 )").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Func {
                ident: Ident { id: init_id },
                typ: Type::FuncType(
                    Ident { id: init_id + 1 },
                    Box::new(Type::IntType(
                        Ident { id: init_id + 1 },
                        logic::Term::True(Info::Dummy),
                        Info::Dummy
                    )),
                    Box::new(Type::IntType(
                        Ident { id: init_id + 2 },
                        logic::Term::True(Info::Dummy),
                        Info::Dummy
                    )),
                    Info::Dummy
                ),
                is_rec: true,
                body: Expr::Num(1, Info::Dummy),
                info: Info::Dummy
            },
            vec![
                (Ident { id: init_id }, "hoge".to_string()),
                (Ident { id: init_id + 1 }, "x".to_string()),
                (Ident { id: init_id + 2 }, "y".to_string())
            ]
            .into_iter()
            .collect()
        )
    );

    let actual = func("func hoge (x: int | true) (y: int | true): (z: int | true) ( x )").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Func {
                ident: Ident { id: init_id },
                typ: Type::FuncType(
                    Ident { id: init_id + 1 },
                    Box::new(Type::IntType(
                        Ident { id: init_id + 1 },
                        logic::Term::True(Info::Dummy),
                        Info::Dummy
                    )),
                    Box::new(Type::FuncType(
                        Ident { id: init_id + 2 },
                        Box::new(Type::IntType(
                            Ident { id: init_id + 2 },
                            logic::Term::True(Info::Dummy),
                            Info::Dummy
                        )),
                        Box::new(Type::IntType(
                            Ident { id: init_id + 3 },
                            logic::Term::True(Info::Dummy),
                            Info::Dummy
                        )),
                        Info::Dummy
                    )),
                    Info::Dummy
                ),
                is_rec: false,
                body: Expr::Var(Ident { id: init_id + 1 }, Info::Dummy),
                info: Info::Dummy
            },
            vec![
                (Ident { id: init_id }, "hoge".to_string()),
                (Ident { id: init_id + 1 }, "x".to_string()),
                (Ident { id: init_id + 2 }, "y".to_string()),
                (Ident { id: init_id + 3 }, "z".to_string())
            ]
            .into_iter()
            .collect()
        )
    );
}

#[test]
fn parse_type_test() {
    let _builtin = builtin::BuiltinData::instance();
    let init_id = Ident::current_count();

    let actual = typ("(n: int | n >= 0)").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Type::IntType(
                Ident { id: init_id },
                logic::Term::Bin(
                    logic::BinOp::Geq,
                    Box::new(logic::Term::Var(Ident { id: init_id }, Info::Dummy)),
                    Box::new(logic::Term::Num(0, Info::Dummy)),
                    Info::Dummy
                ),
                Info::Dummy
            ),
            vec![(Ident { id: init_id }, "n".to_string())]
                .into_iter()
                .collect()
        )
    );

    let actual = typ("(x: int | true) -> (y: int | true)").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Type::FuncType(
                Ident { id: init_id },
                Box::new(Type::IntType(
                    Ident { id: init_id },
                    logic::Term::True(Info::Dummy),
                    Info::Dummy
                )),
                Box::new(Type::IntType(
                    Ident { id: init_id + 1 },
                    logic::Term::True(Info::Dummy),
                    Info::Dummy
                )),
                Info::Dummy
            ),
            vec![
                (Ident { id: init_id }, "x".to_string()),
                (Ident { id: init_id + 1 }, "y".to_string()),
            ]
            .into_iter()
            .collect()
        )
    );

    let actual = typ("(x: int | true) -> (y: int | true) -> (z: int | true)").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Type::FuncType(
                Ident { id: init_id },
                Box::new(Type::IntType(
                    Ident { id: init_id },
                    logic::Term::True(Info::Dummy),
                    Info::Dummy
                )),
                Box::new(Type::FuncType(
                    Ident { id: init_id + 1 },
                    Box::new(Type::IntType(
                        Ident { id: init_id + 1 },
                        logic::Term::True(Info::Dummy),
                        Info::Dummy
                    )),
                    Box::new(Type::IntType(
                        Ident { id: init_id + 2 },
                        logic::Term::True(Info::Dummy),
                        Info::Dummy
                    )),
                    Info::Dummy
                )),
                Info::Dummy
            ),
            vec![
                (Ident { id: init_id }, "x".to_string()),
                (Ident { id: init_id + 1 }, "y".to_string()),
                (Ident { id: init_id + 2 }, "z".to_string()),
            ]
            .into_iter()
            .collect()
        )
    );
}

#[test]
fn parse_expr_test() {
    let builtin = builtin::BuiltinData::instance();
    let init_id = Ident::current_count();

    assert_eq!(
        expr("0").unwrap(),
        (Expr::Num(0, Info::Dummy), Env::empty())
    );
    assert_eq!(
        expr("-1").unwrap(),
        (Expr::Num(-1, Info::Dummy), Env::empty())
    );

    assert_eq!(
        expr("1 + 2 + 3").unwrap(), // (1 + 2) + 3
        (
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                    Box::new(Expr::App(
                        Box::new(Expr::App(
                            Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                            Box::new(Expr::Num(1, Info::Dummy)),
                            Info::Dummy
                        )),
                        Box::new(Expr::Num(2, Info::Dummy)),
                        Info::Dummy
                    )),
                    Info::Dummy
                )),
                Box::new(Expr::Num(3, Info::Dummy)),
                Info::Dummy
            ),
            Env::empty()
        )
    );

    assert_eq!(
        expr("1 + 2 * 3").unwrap(), // 1 + (2 * 3)
        (
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                    Box::new(Expr::Num(1, Info::Dummy)),
                    Info::Dummy
                )),
                Box::new(Expr::App(
                    Box::new(Expr::App(
                        Box::new(Expr::Var(builtin.mult_ident, Info::Dummy)),
                        Box::new(Expr::Num(2, Info::Dummy)),
                        Info::Dummy
                    )),
                    Box::new(Expr::Num(3, Info::Dummy)),
                    Info::Dummy
                )),
                Info::Dummy
            ),
            Env::empty()
        )
    );

    let mut name_to_ident = HashMap::new();
    let a_ident = Ident::fresh();
    name_to_ident.insert("a".to_string(), a_ident);

    assert_eq!(
        expr_with_name_map("1 + ifz a { 2 } else { 3 }", &name_to_ident).unwrap(),
        (
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                    Box::new(Expr::Num(1, Info::Dummy)),
                    Info::Dummy
                )),
                Box::new(Expr::Ifz(
                    Box::new(Expr::Var(a_ident, Info::Dummy)),
                    Box::new(Expr::Num(2, Info::Dummy)),
                    Box::new(Expr::Num(3, Info::Dummy)),
                    Info::Dummy
                )),
                Info::Dummy
            ),
            Env::empty()
        )
    );
    Ident::set_fresh_count(init_id);

    assert_eq!(
        expr("let a = 0 in let b = 1 in a + b").unwrap(),
        (
            Expr::Let(
                Ident { id: init_id },
                Box::new(Expr::Num(0, Info::Dummy)),
                Box::new(Expr::Let(
                    Ident { id: init_id + 1 },
                    Box::new(Expr::Num(1, Info::Dummy)),
                    Box::new(Expr::App(
                        Box::new(Expr::App(
                            Box::new(Expr::Var(builtin.add_ident, Info::Dummy)),
                            Box::new(Expr::Var(Ident { id: init_id }, Info::Dummy)),
                            Info::Dummy
                        )),
                        Box::new(Expr::Var(Ident { id: init_id + 1 }, Info::Dummy)),
                        Info::Dummy
                    )),
                    Info::Dummy
                )),
                Info::Dummy
            ),
            vec![
                (Ident { id: init_id }, "a".to_string()),
                (Ident { id: init_id + 1 }, "b".to_string())
            ]
            .into_iter()
            .collect()
        )
    );
    Ident::set_fresh_count(init_id);

    assert_eq!(
        expr("let a = let b = 1 in b in a").unwrap(),
        (
            Expr::Let(
                Ident { id: init_id },
                Box::new(Expr::Let(
                    Ident { id: init_id + 1 },
                    Box::new(Expr::Num(1, Info::Dummy)),
                    Box::new(Expr::Var(Ident { id: init_id + 1 }, Info::Dummy)),
                    Info::Dummy
                )),
                Box::new(Expr::Var(Ident { id: init_id }, Info::Dummy)),
                Info::Dummy
            ),
            vec![
                (Ident { id: init_id }, "a".to_string()),
                (Ident { id: init_id + 1 }, "b".to_string()),
            ]
            .into_iter()
            .collect()
        ),
    );
    Ident::set_fresh_count(init_id);

    let mut name_to_ident = HashMap::new();
    let f_ident = Ident::fresh();
    name_to_ident.insert("f".to_string(), f_ident);

    assert_eq!(
        expr_with_name_map("f(1)", &name_to_ident).unwrap(),
        (
            Expr::App(
                Box::new(Expr::Var(f_ident, Info::Dummy)),
                Box::new(Expr::Num(1, Info::Dummy)),
                Info::Dummy
            ),
            Env::empty()
        )
    );
    Ident::set_fresh_count(init_id);

    let mut name_to_ident = HashMap::new();
    let f_ident = Ident::fresh();
    name_to_ident.insert("f".to_string(), f_ident);

    assert_eq!(
        expr_with_name_map("f(1, 2)", &name_to_ident).unwrap(),
        (
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(f_ident, Info::Dummy)),
                    Box::new(Expr::Num(1, Info::Dummy)),
                    Info::Dummy
                )),
                Box::new(Expr::Num(2, Info::Dummy)),
                Info::Dummy
            ),
            Env::empty()
        )
    );
    Ident::set_fresh_count(init_id);
}

#[test]
fn parse_ident_test() {
    assert_eq!(ident("hoge_42_").unwrap(), "hoge_42_");
    assert!(ident("42hoge").is_err()); // ident does not start with digits
    assert!(ident("_hoge").is_err()); // ident does not start with underscore
    assert!(ident("let").is_err()); // keyword is not ident
    assert!(ident("42").is_err());
}
