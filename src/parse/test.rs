#[cfg(test)]
use std::collections::HashMap;

#[cfg(test)]
use crate::{
    ast::*,
    env::Env,
    parse::{expr, expr_with_name_map, func, ident, program, typ},
};

#[test]
fn parse_program_test() {
    let _builtin = builtin::BuiltinData::instance();
    let init_id = Ident::current_count();
    let actual = program("func hoge (x: int | true): (y: int | true) = 1 func fuga (a: int | false): (b: int | false) = 2").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Program {
                funcs: vec![
                    Func {
                        typ: Type::FuncType(
                            Ident::with_id(init_id),
                            Box::new(Type::BaseType(
                                Ident::with_id(init_id + 2),
                                BaseTypeKind::Int,
                                logic::Term::True(Info::dummy()),
                                Info::dummy()
                            )),
                            Box::new(Type::BaseType(
                                Ident::with_id(init_id + 3),
                                BaseTypeKind::Int,
                                logic::Term::True(Info::dummy()),
                                Info::dummy()
                            )),
                            Info::dummy(),
                        ),
                        params_len: 1,
                        is_rec: false,
                        body: Expr::Num(1, Info::dummy()),
                        info: Info::dummy(),
                    },
                    Func {
                        typ: Type::FuncType(
                            Ident::with_id(init_id + 1),
                            Box::new(Type::BaseType(
                                Ident::with_id(init_id + 4),
                                BaseTypeKind::Int,
                                logic::Term::False(Info::dummy()),
                                Info::dummy()
                            )),
                            Box::new(Type::BaseType(
                                Ident::with_id(init_id + 5),
                                BaseTypeKind::Int,
                                logic::Term::False(Info::dummy()),
                                Info::dummy()
                            )),
                            Info::dummy(),
                        ),
                        params_len: 1,
                        is_rec: false,
                        body: Expr::Num(2, Info::dummy()),
                        info: Info::dummy(),
                    }
                ],
                info: Info::dummy(),
            },
            vec![
                (Ident::builtin_ident_with_id(13), "print_bool".to_string()),
                (Ident::with_id(init_id), "hoge".to_string()),
                (Ident::with_id(init_id + 1), "fuga".to_string()),
                (Ident::with_id(init_id + 2), "x".to_string()),
                (Ident::with_id(init_id + 3), "y".to_string()),
                (Ident::with_id(init_id + 4), "a".to_string()),
                (Ident::with_id(init_id + 5), "b".to_string()),
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
    let actual = func("rec func hoge (x: int | true): (y: int | true) = 1").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Func {
                typ: Type::FuncType(
                    Ident::with_id(init_id),
                    Box::new(Type::BaseType(
                        Ident::with_id(init_id + 1),
                        BaseTypeKind::Int,
                        logic::Term::True(Info::dummy()),
                        Info::dummy()
                    )),
                    Box::new(Type::BaseType(
                        Ident::with_id(init_id + 2),
                        BaseTypeKind::Int,
                        logic::Term::True(Info::dummy()),
                        Info::dummy()
                    )),
                    Info::dummy()
                ),
                params_len: 1,
                is_rec: true,
                body: Expr::Num(1, Info::dummy()),
                info: Info::dummy()
            },
            vec![
                (Ident::with_id(init_id), "hoge".to_string()),
                (Ident::with_id(init_id + 1), "x".to_string()),
                (Ident::with_id(init_id + 2), "y".to_string())
            ]
            .into_iter()
            .collect()
        )
    );

    let actual = func("func hoge (x: int | true) (y: int | true): (z: int | true) = x").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Func {
                typ: Type::FuncType(
                    Ident::with_id(init_id),
                    Box::new(Type::BaseType(
                        Ident::with_id(init_id + 1),
                        BaseTypeKind::Int,
                        logic::Term::True(Info::dummy()),
                        Info::dummy()
                    )),
                    Box::new(Type::FuncType(
                        Ident::with_id(init_id + 4),
                        Box::new(Type::BaseType(
                            Ident::with_id(init_id + 2),
                            BaseTypeKind::Int,
                            logic::Term::True(Info::dummy()),
                            Info::dummy()
                        )),
                        Box::new(Type::BaseType(
                            Ident::with_id(init_id + 3),
                            BaseTypeKind::Int,
                            logic::Term::True(Info::dummy()),
                            Info::dummy()
                        )),
                        Info::dummy()
                    )),
                    Info::dummy()
                ),
                params_len: 2,
                is_rec: false,
                body: Expr::Var(Ident::with_id(init_id + 1), Info::dummy()),
                info: Info::dummy()
            },
            vec![
                (Ident::with_id(init_id), "hoge".to_string()),
                (Ident::with_id(init_id + 1), "x".to_string()),
                (Ident::with_id(init_id + 2), "y".to_string()),
                (Ident::with_id(init_id + 3), "z".to_string())
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
            Type::BaseType(
                Ident::with_id(init_id),
                BaseTypeKind::Int,
                logic::Term::Bin(
                    logic::BinOp::Geq,
                    Box::new(logic::Term::Var(Ident::with_id(init_id), Info::dummy())),
                    Box::new(logic::Term::Num(0, Info::dummy())),
                    Info::dummy()
                ),
                Info::dummy()
            ),
            vec![(Ident::with_id(init_id), "n".to_string())]
                .into_iter()
                .collect()
        )
    );

    let actual = typ("(f: (x: int | true) -> (y: int | true))").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Type::FuncType(
                Ident::with_id(init_id),
                Box::new(Type::BaseType(
                    Ident::with_id(init_id + 1),
                    BaseTypeKind::Int,
                    logic::Term::True(Info::dummy()),
                    Info::dummy()
                )),
                Box::new(Type::BaseType(
                    Ident::with_id(init_id + 2),
                    BaseTypeKind::Int,
                    logic::Term::True(Info::dummy()),
                    Info::dummy()
                )),
                Info::dummy()
            ),
            vec![
                (Ident::with_id(init_id), "f".to_string()),
                (Ident::with_id(init_id + 1), "x".to_string()),
                (Ident::with_id(init_id + 2), "y".to_string()),
            ]
            .into_iter()
            .collect()
        )
    );

    let actual = typ("(f: (x: int | true) -> (y: int | true) -> (z: int | true))").unwrap();
    Ident::set_fresh_count(init_id);

    assert_eq!(
        actual,
        (
            Type::FuncType(
                Ident::with_id(init_id),
                Box::new(Type::BaseType(
                    Ident::with_id(init_id + 1),
                    BaseTypeKind::Int,
                    logic::Term::True(Info::dummy()),
                    Info::dummy()
                )),
                Box::new(Type::FuncType(
                    Ident::with_id(init_id + 4),
                    Box::new(Type::BaseType(
                        Ident::with_id(init_id + 2),
                        BaseTypeKind::Int,
                        logic::Term::True(Info::dummy()),
                        Info::dummy()
                    )),
                    Box::new(Type::BaseType(
                        Ident::with_id(init_id + 3),
                        BaseTypeKind::Int,
                        logic::Term::True(Info::dummy()),
                        Info::dummy()
                    )),
                    Info::dummy()
                )),
                Info::dummy()
            ),
            vec![
                (Ident::with_id(init_id), "f".to_string()),
                (Ident::with_id(init_id + 1), "x".to_string()),
                (Ident::with_id(init_id + 2), "y".to_string()),
                (Ident::with_id(init_id + 3), "z".to_string()),
            ]
            .into_iter()
            .collect()
        )
    );

    let actual = typ("(x: int)").unwrap();
    Ident::set_fresh_count(init_id);
    assert_eq!(
        actual,
        (
            Type::BaseType(
                Ident::with_id(init_id),
                BaseTypeKind::Int,
                logic::Term::True(Info::dummy()),
                Info::dummy()
            ),
            vec![(Ident::with_id(init_id), "x".to_string())]
                .into_iter()
                .collect()
        )
    );

    let actual = typ("(x: bool)").unwrap();
    Ident::set_fresh_count(init_id);
    assert_eq!(
        actual,
        (
            Type::BaseType(
                Ident::with_id(init_id),
                BaseTypeKind::Bool,
                logic::Term::True(Info::dummy()),
                Info::dummy()
            ),
            vec![(Ident::with_id(init_id), "x".to_string())]
                .into_iter()
                .collect()
        )
    );

    let actual = typ("(bool | true)").unwrap();
    Ident::set_fresh_count(init_id);
    assert_eq!(
        actual,
        (
            Type::BaseType(
                Ident::with_id(init_id),
                BaseTypeKind::Bool,
                logic::Term::True(Info::dummy()),
                Info::dummy()
            ),
            vec![].into_iter().collect()
        )
    );

    let actual = typ("bool").unwrap();
    Ident::set_fresh_count(init_id);
    assert_eq!(
        actual,
        (
            Type::BaseType(
                Ident::with_id(init_id),
                BaseTypeKind::Bool,
                logic::Term::True(Info::dummy()),
                Info::dummy()
            ),
            vec![].into_iter().collect()
        )
    );

    let actual = typ("(int | 42)").unwrap();
    Ident::set_fresh_count(init_id);
    assert_eq!(
        actual,
        (
            Type::BaseType(
                Ident::with_id(init_id),
                BaseTypeKind::Int,
                logic::Term::Bin(
                    logic::BinOp::Eq,
                    Box::new(logic::Term::Var(Ident::with_id(init_id), Info::dummy())),
                    Box::new(logic::Term::Num(42, Info::dummy())),
                    Info::dummy()
                ),
                Info::dummy()
            ),
            vec![].into_iter().collect()
        )
    );

    let actual = typ("int").unwrap();
    Ident::set_fresh_count(init_id);
    assert_eq!(
        actual,
        (
            Type::BaseType(
                Ident::with_id(init_id),
                BaseTypeKind::Int,
                logic::Term::True(Info::dummy()),
                Info::dummy()
            ),
            vec![].into_iter().collect()
        )
    );
}

#[test]
fn parse_expr_test() {
    let builtin = builtin::BuiltinData::instance();
    let init_id = Ident::current_count();

    assert_eq!(
        expr("0").unwrap(),
        (Expr::Num(0, Info::dummy()), Env::empty())
    );
    assert_eq!(
        expr("-1").unwrap(),
        (Expr::Num(-1, Info::dummy()), Env::empty())
    );
    assert_eq!(
        expr("true").unwrap(),
        (Expr::Boolean(true, Info::dummy()), Env::empty())
    );
    assert_eq!(
        expr("false").unwrap(),
        (Expr::Boolean(false, Info::dummy()), Env::empty())
    );

    assert_eq!(
        expr("1 + 2 + 3").unwrap(), // (1 + 2) + 3
        (
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                    Box::new(Expr::App(
                        Box::new(Expr::App(
                            Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                            Box::new(Expr::Num(1, Info::dummy())),
                            Info::dummy()
                        )),
                        Box::new(Expr::Num(2, Info::dummy())),
                        Info::dummy()
                    )),
                    Info::dummy()
                )),
                Box::new(Expr::Num(3, Info::dummy())),
                Info::dummy()
            ),
            Env::empty()
        )
    );

    assert_eq!(
        expr("1 + 2 * 3").unwrap(), // 1 + (2 * 3)
        (
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                    Box::new(Expr::Num(1, Info::dummy())),
                    Info::dummy()
                )),
                Box::new(Expr::App(
                    Box::new(Expr::App(
                        Box::new(Expr::Var(builtin.mult_ident, Info::dummy())),
                        Box::new(Expr::Num(2, Info::dummy())),
                        Info::dummy()
                    )),
                    Box::new(Expr::Num(3, Info::dummy())),
                    Info::dummy()
                )),
                Info::dummy()
            ),
            Env::empty()
        )
    );

    let mut name_to_ident = HashMap::new();
    let a_ident = Ident::fresh();
    name_to_ident.insert("a".to_string(), a_ident);

    assert_eq!(
        expr_with_name_map("1 + if a = 0 { 2 } else { 3 }", &name_to_ident).unwrap(),
        (
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                    Box::new(Expr::Num(1, Info::dummy())),
                    Info::dummy()
                )),
                Box::new(Expr::If(
                    Box::new(Expr::App(
                        Box::new(Expr::App(
                            Box::new(Expr::Var(builtin.eq_ident, Info::dummy())),
                            Box::new(Expr::Var(a_ident, Info::dummy())),
                            Info::dummy()
                        )),
                        Box::new(Expr::Num(0, Info::dummy())),
                        Info::dummy()
                    )),
                    Box::new(Expr::Num(2, Info::dummy())),
                    Box::new(Expr::Num(3, Info::dummy())),
                    Info::dummy()
                )),
                Info::dummy()
            ),
            Env::empty()
        )
    );
    Ident::set_fresh_count(init_id);

    assert_eq!(
        expr("let a = 0 in let b = 1 in a + b").unwrap(),
        (
            Expr::Let(
                Ident::with_id(init_id),
                Box::new(Expr::Num(0, Info::dummy())),
                Box::new(Expr::Let(
                    Ident::with_id(init_id + 1),
                    Box::new(Expr::Num(1, Info::dummy())),
                    Box::new(Expr::App(
                        Box::new(Expr::App(
                            Box::new(Expr::Var(builtin.add_ident, Info::dummy())),
                            Box::new(Expr::Var(Ident::with_id(init_id), Info::dummy())),
                            Info::dummy()
                        )),
                        Box::new(Expr::Var(Ident::with_id(init_id + 1), Info::dummy())),
                        Info::dummy()
                    )),
                    Info::dummy()
                )),
                Info::dummy()
            ),
            vec![
                (Ident::with_id(init_id), "a".to_string()),
                (Ident::with_id(init_id + 1), "b".to_string())
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
                Ident::with_id(init_id),
                Box::new(Expr::Let(
                    Ident::with_id(init_id + 1),
                    Box::new(Expr::Num(1, Info::dummy())),
                    Box::new(Expr::Var(Ident::with_id(init_id + 1), Info::dummy())),
                    Info::dummy()
                )),
                Box::new(Expr::Var(Ident::with_id(init_id), Info::dummy())),
                Info::dummy()
            ),
            vec![
                (Ident::with_id(init_id), "a".to_string()),
                (Ident::with_id(init_id + 1), "b".to_string()),
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
        expr_with_name_map("f 1", &name_to_ident).unwrap(),
        (
            Expr::App(
                Box::new(Expr::Var(f_ident, Info::dummy())),
                Box::new(Expr::Num(1, Info::dummy())),
                Info::dummy()
            ),
            Env::empty()
        )
    );
    Ident::set_fresh_count(init_id);

    let mut name_to_ident = HashMap::new();
    let f_ident = Ident::fresh();
    name_to_ident.insert("f".to_string(), f_ident);

    assert_eq!(
        expr_with_name_map("f 1 2", &name_to_ident).unwrap(),
        (
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(f_ident, Info::dummy())),
                    Box::new(Expr::Num(1, Info::dummy())),
                    Info::dummy()
                )),
                Box::new(Expr::Num(2, Info::dummy())),
                Info::dummy()
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
