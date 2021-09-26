use crate::ast::logic::BinPred;
#[cfg(test)]
use crate::ast::*;

#[cfg(test)]
use super::{expr, func, ident, program, typ};

#[test]
fn parse_program_test() {
    assert_eq!(
        program("func hoge: (x: int | true) ( 1 )").unwrap(),
        Program {
            funcs: vec![Func {
                name: Ident::new("hoge"),
                params: vec![],
                ret: Type::NonFuncType(NonFuncType {
                    param_name: Ident::new("x"),
                    base_type: BaseType::Int(PosInfo::dummy()),
                    formula: logic::Formula::True(PosInfo::dummy()),
                    pos: PosInfo::dummy()
                }),
                is_rec: false,
                body: Expr::Constant(Constant::new(1)),
                pos: PosInfo::dummy()
            }],
            pos: PosInfo::dummy(),
        }
    );
}

#[test]
fn parse_func_test() {
    assert_eq!(
        func("rec func hoge: (x: int | true) ( 1 ) ").unwrap(),
        Func {
            name: Ident::new("hoge"),
            params: vec![],
            ret: Type::NonFuncType(NonFuncType {
                param_name: Ident::new("x"),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::True(PosInfo::dummy()),
                pos: PosInfo::dummy()
            }),
            is_rec: true,
            body: Expr::Constant(Constant::new(1)),
            pos: PosInfo::dummy()
        }
    );

    assert_eq!(
        func("func hoge (x: int | true): (y: int | true) ( x ) ").unwrap(),
        Func {
            name: Ident::new("hoge"),
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
            is_rec: false,
            body: Expr::Var(Ident::new("x")),
            pos: PosInfo::dummy()
        }
    );

    assert_eq!(
        func("func hoge (x: int | true) (y: int | true): (z: int | true) ( x ) ").unwrap(),
        Func {
            name: Ident::new("hoge"),
            params: vec![
                Type::NonFuncType(NonFuncType {
                    param_name: Ident::new("x"),
                    base_type: BaseType::Int(PosInfo::dummy()),
                    formula: logic::Formula::True(PosInfo::dummy()),
                    pos: PosInfo::dummy()
                }),
                Type::NonFuncType(NonFuncType {
                    param_name: Ident::new("y"),
                    base_type: BaseType::Int(PosInfo::dummy()),
                    formula: logic::Formula::True(PosInfo::dummy()),
                    pos: PosInfo::dummy()
                })
            ],
            ret: Type::NonFuncType(NonFuncType {
                param_name: Ident::new("z"),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::True(PosInfo::dummy()),
                pos: PosInfo::dummy()
            }),
            is_rec: false,
            body: Expr::Var(Ident::new("x")),
            pos: PosInfo::dummy()
        }
    );
}

#[test]
fn parse_type_test() {
    assert_eq!(
        typ("(n: int | true)").unwrap(),
        Type::NonFuncType(NonFuncType {
            param_name: Ident::new("n"),
            base_type: BaseType::Int(PosInfo::dummy()),
            formula: logic::Formula::True(PosInfo::dummy()),
            pos: PosInfo::dummy()
        })
    );

    assert_eq!(
        typ("(n: int | n >= 0)").unwrap(),
        Type::NonFuncType(NonFuncType {
            param_name: Ident::new("n"),
            base_type: BaseType::Int(PosInfo::dummy()),
            formula: logic::Formula::BinApp(
                logic::BinPred::Geq(PosInfo::dummy()),
                logic::Expr::Var(Ident::new("n")),
                logic::Expr::Constant(Constant::new(0)),
                PosInfo::dummy()
            ),
            pos: PosInfo::dummy()
        })
    );

    assert_eq!(
        typ("(x: int | true) -> (y: int | true)").unwrap(),
        Type::FuncType(FuncType {
            params: vec![Type::NonFuncType(NonFuncType {
                param_name: Ident::new("x"),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::True(PosInfo::dummy()),
                pos: PosInfo::dummy()
            })],
            ret: Box::new(Type::NonFuncType(NonFuncType {
                param_name: Ident::new("y"),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::True(PosInfo::dummy()),
                pos: PosInfo::dummy()
            })),
            pos: PosInfo::dummy()
        })
    );
}

#[test]
fn parse_expr_test() {
    assert_eq!(expr("0").unwrap(), Expr::Constant(Constant::new(0)));
    assert_eq!(expr("-1").unwrap(), Expr::Constant(Constant::new(-1)));

    assert_eq!(
        expr("1 + 2 + 3").unwrap(), // (1 + 2) + 3
        Expr::BinApp(
            BinOp::Add(PosInfo::dummy()),
            Box::new(Expr::BinApp(
                BinOp::Add(PosInfo::dummy()),
                Box::new(Expr::Constant(Constant::new(1))),
                Box::new(Expr::Constant(Constant::new(2))),
                PosInfo::dummy()
            )),
            Box::new(Expr::Constant(Constant::new(3))),
            PosInfo::dummy()
        )
    );
    assert_eq!(
        expr("1 + 2 * 3").unwrap(), // 1 + (2 * 3)
        Expr::BinApp(
            BinOp::Add(PosInfo::dummy()),
            Box::new(Expr::Constant(Constant::new(1))),
            Box::new(Expr::BinApp(
                BinOp::Mul(PosInfo::dummy()),
                Box::new(Expr::Constant(Constant::new(2))),
                Box::new(Expr::Constant(Constant::new(3))),
                PosInfo::dummy()
            )),
            PosInfo::dummy()
        ),
    );

    assert_eq!(
        expr("1 + ifz a { 2 } else { 3 }").unwrap(),
        Expr::BinApp(
            BinOp::Add(PosInfo::dummy()),
            Box::new(Expr::Constant(Constant::new(1))),
            Box::new(Expr::Ifz(
                Box::new(Expr::Var(Ident::new("a"))),
                Box::new(Expr::Constant(Constant::new(2))),
                Box::new(Expr::Constant(Constant::new(3))),
                PosInfo::dummy()
            )),
            PosInfo::dummy()
        )
    );

    assert_eq!(
        expr("let a = 0 in let b = 1 in a + b").unwrap(),
        Expr::Let(
            Ident::new("a"),
            Box::new(Expr::Constant(Constant::new(0))),
            Box::new(Expr::Let(
                Ident::new("b"),
                Box::new(Expr::Constant(Constant::new(1))),
                Box::new(Expr::BinApp(
                    BinOp::Add(PosInfo::dummy()),
                    Box::new(Expr::Var(Ident::new("a"))),
                    Box::new(Expr::Var(Ident::new("b"))),
                    PosInfo::dummy()
                )),
                PosInfo::dummy()
            )),
            PosInfo::dummy()
        )
    );

    assert_eq!(
        expr("let a = let b = 1 in b in a").unwrap(),
        Expr::Let(
            Ident::new("a"),
            Box::new(Expr::Let(
                Ident::new("b"),
                Box::new(Expr::Constant(Constant::new(1))),
                Box::new(Expr::Var(Ident::new("b"))),
                PosInfo::dummy()
            )),
            Box::new(Expr::Var(Ident::new("a"))),
            PosInfo::dummy()
        ),
    );

    assert_eq!(
        expr("f ()").unwrap(),
        Expr::FuncApp(Ident::new("f"), vec![], PosInfo::dummy())
    );

    assert_eq!(
        expr("f (1)").unwrap(),
        Expr::FuncApp(
            Ident::new("f"),
            vec![Expr::Constant(Constant::new(1))],
            PosInfo::dummy()
        )
    );

    assert_eq!(
        expr("f (1, 2)").unwrap(),
        Expr::FuncApp(
            Ident::new("f"),
            vec![
                Expr::Constant(Constant::new(1)),
                Expr::Constant(Constant::new(2))
            ],
            PosInfo::dummy()
        )
    );
}

#[test]
fn parse_ident_test() {
    assert_eq!(ident("hoge_42_").unwrap().name, "hoge_42_");
    assert!(ident("42hoge").is_err()); // ident does not start with digits
    assert!(ident("_hoge").is_err()); // ident does not start with underscore
    assert!(ident("let").is_err()); // keyword is not ident
    assert!(ident("42").is_err());
}
