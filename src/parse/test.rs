#[cfg(test)]
use crate::ast::*;

#[cfg(test)]
use super::{expr, func, ident};

#[test]
fn parse_func_test() {
    assert_eq!(
        func("rec func hoge (x: int | true): (y: int | true) ( x ) ").unwrap(),
        Func {
            name: Ident {
                name: "hoge".to_string(),
                pos: PosInfo { start: 9, end: 13 }
            },
            params: vec![Type::NonFuncType(NonFuncType {
                param_name: Ident {
                    name: "x".to_string(),
                    pos: PosInfo { start: 15, end: 16 }
                },
                base_type: BaseType::Int(PosInfo { start: 18, end: 21 }),
                formula: logic::Formula::True(PosInfo { start: 24, end: 28 }),
                pos: PosInfo { start: 14, end: 29 }
            })],
            ret: Type::NonFuncType(NonFuncType {
                param_name: Ident {
                    name: "y".to_string(),
                    pos: PosInfo { start: 32, end: 33 }
                },
                base_type: BaseType::Int(PosInfo { start: 35, end: 38 }),
                formula: logic::Formula::True(PosInfo { start: 41, end: 45 }),
                pos: PosInfo { start: 31, end: 46 }
            }),
            is_rec: true,
            body: Expr::Var(Ident {
                name: "x".to_string(),
                pos: PosInfo { start: 49, end: 50 }
            }),
            pos: PosInfo { start: 0, end: 52 }
        }
    );
}

#[test]
fn parse_expr_test() {
    assert_eq!(
        expr("1 + 2 + a").unwrap(), // (1 + 2) + 3
        Expr::BinApp(
            BinOp::Add(PosInfo { start: 6, end: 7 }),
            Box::new(Expr::BinApp(
                BinOp::Add(PosInfo { start: 2, end: 3 }),
                Box::new(Expr::Constant(Constant {
                    val: 1,
                    pos: PosInfo { start: 0, end: 1 }
                },)),
                Box::new(Expr::Constant(Constant {
                    val: 2,
                    pos: PosInfo { start: 4, end: 5 }
                })),
                PosInfo { start: 0, end: 6 }
            )),
            Box::new(Expr::Var(Ident {
                name: "a".to_string(),
                pos: PosInfo { start: 8, end: 9 }
            })),
            PosInfo { start: 0, end: 9 }
        )
    );
    assert_eq!(
        expr("(1 * 2 * 3)").unwrap(), // (1 * 2) * 3
        Expr::BinApp(
            BinOp::Mul(PosInfo { start: 7, end: 8 }),
            Box::new(Expr::BinApp(
                BinOp::Mul(PosInfo { start: 3, end: 4 }),
                Box::new(Expr::Constant(Constant {
                    val: 1,
                    pos: PosInfo { start: 1, end: 2 }
                },)),
                Box::new(Expr::Constant(Constant {
                    val: 2,
                    pos: PosInfo { start: 5, end: 6 }
                })),
                PosInfo { start: 1, end: 6 }
            )),
            Box::new(Expr::Constant(Constant {
                val: 3,
                pos: PosInfo { start: 9, end: 10 }
            })),
            PosInfo { start: 1, end: 10 }
        )
    );

    assert_eq!(
        expr("1 + ifz a { 41 } else { 90 + 9 }").unwrap(),
        Expr::BinApp(
            BinOp::Add(PosInfo { start: 2, end: 3 }),
            Box::new(Expr::Constant(Constant {
                val: 1,
                pos: PosInfo { start: 0, end: 1 }
            })),
            Box::new(Expr::Ifz(
                Box::new(Expr::Var(Ident {
                    name: "a".to_string(),
                    pos: PosInfo { start: 8, end: 9 }
                })),
                Box::new(Expr::Constant(Constant {
                    val: 41,
                    pos: PosInfo { start: 12, end: 14 }
                })),
                Box::new(Expr::BinApp(
                    BinOp::Add(PosInfo { start: 27, end: 28 }),
                    Box::new(Expr::Constant(Constant {
                        val: 90,
                        pos: PosInfo { start: 24, end: 26 }
                    })),
                    Box::new(Expr::Constant(Constant {
                        val: 9,
                        pos: PosInfo { start: 29, end: 30 }
                    })),
                    PosInfo { start: 24, end: 31 }
                )),
                PosInfo { start: 4, end: 32 }
            )),
            PosInfo { start: 0, end: 32 }
        )
    );

    assert_eq!(
        expr("let a = 0 in let b = 2 in a + b").unwrap(),
        Expr::Let(
            Ident {
                name: "a".to_string(),
                pos: PosInfo { start: 4, end: 5 }
            },
            Box::new(Expr::Constant(Constant {
                val: 0,
                pos: PosInfo { start: 8, end: 9 }
            })),
            Box::new(Expr::Let(
                Ident {
                    name: "b".to_string(),
                    pos: PosInfo { start: 17, end: 18 }
                },
                Box::new(Expr::Constant(Constant {
                    val: 2,
                    pos: PosInfo { start: 21, end: 22 }
                })),
                Box::new(Expr::BinApp(
                    BinOp::Add(PosInfo { start: 28, end: 29 }),
                    Box::new(Expr::Var(Ident {
                        name: "a".to_string(),
                        pos: PosInfo { start: 26, end: 27 }
                    })),
                    Box::new(Expr::Var(Ident {
                        name: "b".to_string(),
                        pos: PosInfo { start: 30, end: 31 }
                    })),
                    PosInfo { start: 26, end: 31 }
                )),
                PosInfo { start: 13, end: 31 }
            )),
            PosInfo { start: 0, end: 31 }
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
