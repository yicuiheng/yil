use crate::ast::{BinOp, Constant, Expr, Ident, PosInfo};
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct YilParser;

#[derive(Debug)]
pub enum ParseError {
    Pest(pest::error::Error<Rule>),
}

fn span_to_pos_info(span: &pest::Span) -> PosInfo {
    PosInfo {
        start: span.start(),
        end: span.end(),
    }
}

pub fn ident(str: &str) -> Result<Ident<PosInfo>, ParseError> {
    let mut pairs = YilParser::parse(Rule::ident, str).map_err(|e| ParseError::Pest(e))?;
    let ident_pair = pairs.nth(0).unwrap();
    Ok(Ident {
        name: ident_pair.as_str().to_string(),
        info: span_to_pos_info(&ident_pair.as_span()),
    })
}

pub fn expr(str: &str) -> Result<Expr<PosInfo>, ParseError> {
    let mut pairs = YilParser::parse(Rule::expr, str).map_err(|e| ParseError::Pest(e))?;
    let expr_pair = pairs.nth(0).unwrap();
    assert_eq!(expr_pair.as_rule(), Rule::expr);

    expr_from_pair(expr_pair)
}

fn expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr<PosInfo>, ParseError> {
    let mut pairs = pair.into_inner();
    let additive_expr_pair = pairs.next().unwrap();
    assert_eq!(additive_expr_pair.as_rule(), Rule::additive_expr);
    let head_additive_expr = additive_expr_from_pair(additive_expr_pair)?;
    Ok(head_additive_expr)
}

fn additive_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr<PosInfo>, ParseError> {
    let mut pairs_iter = pair.into_inner();
    let head_pair = pairs_iter.next().unwrap();
    assert_eq!(head_pair.as_rule(), Rule::multive_expr);

    let mut acc = multive_expr_from_pair(head_pair)?;
    while let Some(pair) = pairs_iter.next() {
        let op = match pair.as_rule() {
            Rule::plus => BinOp::Add,
            Rule::minus => BinOp::Sub,
            _ => unreachable!(),
        };
        let rhs_pair = pairs_iter.next().unwrap();
        assert_eq!(rhs_pair.as_rule(), Rule::multive_expr);
        let rhs = multive_expr_from_pair(rhs_pair)?;
        let pos = PosInfo {
            start: acc.info().start,
            end: rhs.info().end,
        };
        acc = Expr::BinApp(
            op(span_to_pos_info(&pair.as_span())),
            Box::new(acc),
            Box::new(rhs),
            pos,
        )
    }
    Ok(acc)
}

fn multive_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr<PosInfo>, ParseError> {
    let mut pairs_iter = pair.into_inner();
    let head_pair = pairs_iter.next().unwrap();
    assert_eq!(head_pair.as_rule(), Rule::primary_expr);

    let mut acc = primary_expr_from_pair(head_pair)?;
    while let Some(pair) = pairs_iter.next() {
        let op = match pair.as_rule() {
            Rule::ast => BinOp::Mul,
            Rule::slash => BinOp::Div,
            _ => unreachable!(),
        };
        let rhs_pair = pairs_iter.next().unwrap();
        assert_eq!(rhs_pair.as_rule(), Rule::primary_expr);
        let rhs = primary_expr_from_pair(rhs_pair)?;
        let pos = PosInfo {
            start: acc.info().start,
            end: rhs.info().end,
        };
        acc = Expr::BinApp(
            op(span_to_pos_info(&pair.as_span())),
            Box::new(acc),
            Box::new(rhs),
            pos,
        )
    }
    Ok(acc)
}

fn primary_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr<PosInfo>, ParseError> {
    let mut pairs_iter = pair.into_inner();
    let pair = pairs_iter.next().unwrap();

    match pair.as_rule() {
        Rule::ifz_expr => ifz_expr_from_pair(pair),
        Rule::let_expr => let_expr_from_pair(pair),
        Rule::constant => constant_from_pair(pair).map(|c| Expr::Constant(c)),
        Rule::variable => ident_from_pair(pair).map(|ident| Expr::Var(ident)),
        Rule::left_paren => {
            let pair = pairs_iter.next().unwrap();
            assert_eq!(pair.as_rule(), Rule::expr);
            let e = expr_from_pair(pair)?;
            let pair = pairs_iter.next().unwrap();
            assert_eq!(pair.as_rule(), Rule::right_paren);
            Ok(e)
        }
        _ => unreachable!(),
    }
}

fn ifz_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr<PosInfo>, ParseError> {
    let pos_info = span_to_pos_info(&pair.as_span());
    let mut pairs_iter = pair.into_inner();

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_ifz);

    let cond_pair = pairs_iter.next().unwrap();
    assert_eq!(cond_pair.as_rule(), Rule::expr);
    let cond = expr_from_pair(cond_pair)?;

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_then);

    let expr1_pair = pairs_iter.next().unwrap();
    assert_eq!(expr1_pair.as_rule(), Rule::expr);
    let expr1 = expr_from_pair(expr1_pair)?;

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_else);

    let expr2_pair = pairs_iter.next().unwrap();
    assert_eq!(expr2_pair.as_rule(), Rule::expr);
    let expr2 = expr_from_pair(expr2_pair)?;

    Ok(Expr::Ifz(
        Box::new(cond),
        Box::new(expr1),
        Box::new(expr2),
        pos_info,
    ))
}

fn let_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr<PosInfo>, ParseError> {
    let pos_info = span_to_pos_info(&pair.as_span());
    let mut pairs_iter = pair.into_inner();

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_let);

    let ident_pair = pairs_iter.next().unwrap();
    assert_eq!(ident_pair.as_rule(), Rule::ident);
    let ident = ident_from_pair(ident_pair)?;

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::equal);

    let expr1_pair = pairs_iter.next().unwrap();
    assert_eq!(expr1_pair.as_rule(), Rule::expr);
    let expr1 = expr_from_pair(expr1_pair)?;

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_in);

    let expr2_pair = pairs_iter.next().unwrap();
    assert_eq!(expr2_pair.as_rule(), Rule::expr);
    let expr2 = expr_from_pair(expr2_pair)?;

    Ok(Expr::Let(ident, Box::new(expr1), Box::new(expr2), pos_info))
}

fn constant_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Constant<PosInfo>, ParseError> {
    Ok(Constant {
        val: pair.as_str().parse().unwrap(),
        info: PosInfo {
            start: pair.as_span().start(),
            end: pair.as_span().end(),
        },
    })
}

fn ident_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Ident<PosInfo>, ParseError> {
    Ok(Ident {
        name: pair.as_str().to_string(),
        info: span_to_pos_info(&pair.as_span()),
    })
}

#[test]
fn test() {
    assert_eq!(ident("hoge42").unwrap().name, "hoge42");
    assert!(ident("42hoge").is_err());
    assert!(ident("42").is_err());

    assert_eq!(
        expr("1 + 2 + a").unwrap(), // (1 + 2) + 3
        Expr::BinApp(
            BinOp::Add(PosInfo { start: 6, end: 7 }),
            Box::new(Expr::BinApp(
                BinOp::Add(PosInfo { start: 2, end: 3 }),
                Box::new(Expr::Constant(Constant {
                    val: 1,
                    info: PosInfo { start: 0, end: 1 }
                },)),
                Box::new(Expr::Constant(Constant {
                    val: 2,
                    info: PosInfo { start: 4, end: 5 }
                })),
                PosInfo { start: 0, end: 5 }
            )),
            Box::new(Expr::Var(Ident {
                name: "a".to_string(),
                info: PosInfo { start: 8, end: 9 }
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
                    info: PosInfo { start: 1, end: 2 }
                },)),
                Box::new(Expr::Constant(Constant {
                    val: 2,
                    info: PosInfo { start: 5, end: 6 }
                })),
                PosInfo { start: 1, end: 6 }
            )),
            Box::new(Expr::Constant(Constant {
                val: 3,
                info: PosInfo { start: 9, end: 10 }
            })),
            PosInfo { start: 1, end: 10 }
        )
    );

    assert_eq!(
        expr("1 + ifz a then 41 else 90 + 9").unwrap(),
        Expr::BinApp(
            BinOp::Add(PosInfo { start: 2, end: 3 }),
            Box::new(Expr::Constant(Constant {
                val: 1,
                info: PosInfo { start: 0, end: 1 }
            })),
            Box::new(Expr::Ifz(
                Box::new(Expr::Var(Ident {
                    name: "a".to_string(),
                    info: PosInfo { start: 8, end: 9 }
                })),
                Box::new(Expr::Constant(Constant {
                    val: 41,
                    info: PosInfo { start: 15, end: 17 }
                })),
                Box::new(Expr::BinApp(
                    BinOp::Add(PosInfo { start: 26, end: 27 }),
                    Box::new(Expr::Constant(Constant {
                        val: 90,
                        info: PosInfo { start: 23, end: 25 }
                    })),
                    Box::new(Expr::Constant(Constant {
                        val: 9,
                        info: PosInfo { start: 28, end: 29 }
                    })),
                    PosInfo { start: 23, end: 29 }
                )),
                PosInfo { start: 4, end: 29 }
            )),
            PosInfo { start: 0, end: 29 }
        )
    );

    assert_eq!(
        expr("let a = 1 in let b = 2 in a + b").unwrap(),
        Expr::Let(
            Ident {
                name: "a".to_string(),
                info: PosInfo { start: 4, end: 5 }
            },
            Box::new(Expr::Constant(Constant {
                val: 1,
                info: PosInfo { start: 8, end: 9 }
            })),
            Box::new(Expr::Let(
                Ident {
                    name: "b".to_string(),
                    info: PosInfo { start: 17, end: 18 }
                },
                Box::new(Expr::Constant(Constant {
                    val: 2,
                    info: PosInfo { start: 21, end: 22 }
                })),
                Box::new(Expr::BinApp(
                    BinOp::Add(PosInfo { start: 28, end: 29 }),
                    Box::new(Expr::Var(Ident {
                        name: "a".to_string(),
                        info: PosInfo { start: 26, end: 27 }
                    })),
                    Box::new(Expr::Var(Ident {
                        name: "b".to_string(),
                        info: PosInfo { start: 30, end: 31 }
                    })),
                    PosInfo { start: 26, end: 31 }
                )),
                PosInfo { start: 13, end: 31 }
            )),
            PosInfo { start: 0, end: 31 }
        )
    );
}
