use crate::ast::parsed::{
    BaseType, BinOp, Constant, Expr, Formula, Func, Ident, PosInfo, RefineType,
};
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct YilParser;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: Vec<&'static str>,
        unexpected: Vec<&'static str>,
        pos: usize,
    },
}

fn span_to_pos_info(span: &pest::Span) -> PosInfo {
    PosInfo {
        start: span.start(),
        end: span.end(),
    }
}

fn rule_to_str(rule: Rule) -> &'static str {
    match rule {
        Rule::func => "function",
        Rule::refine_type => "refinement type",
        Rule::formula => "logical formula",
        Rule::base_type => "base type",
        Rule::expr => "expression",
        Rule::ifz_expr => "if-expresion",
        Rule::let_expr => "let-expression",
        Rule::constant => "constant",
        Rule::variable => "variable",
        Rule::paren_expr => "parened-expression",
        Rule::left_paren => "'('",
        Rule::right_paren => "')'",
        Rule::left_brace => "'{'",
        Rule::right_brace => "'}'",
        Rule::plus => "'+'",
        Rule::minus => "'-'",
        Rule::ast => "'*'",
        Rule::slash => "'/'",
        Rule::equal => "'='",
        Rule::colon => "':'",
        Rule::bar => "'|'",
        Rule::kw_ifz => "'ifz'",
        Rule::kw_then => "'then'",
        Rule::kw_else => "'else'",
        Rule::kw_let => "'let'",
        Rule::kw_in => "'in'",
        Rule::kw_rec => "'rec'",
        Rule::kw_func => "'func'",
        Rule::kw_int => "'int'",
        Rule::kw_true => "'true'",
        Rule::ident => "<ident>",
        _ => unreachable!(),
    }
}

fn pest_err_to_parse_err(e: pest::error::Error<Rule>) -> ParseError {
    use pest::error::*;
    let pos = match e.location {
        InputLocation::Pos(pos) => pos,
        InputLocation::Span((pos, _)) => pos,
    };
    match e.variant {
        pest::error::ErrorVariant::ParsingError {
            positives,
            negatives,
        } => {
            let expected = positives.into_iter().map(rule_to_str).collect();
            let unexpected = negatives.into_iter().map(rule_to_str).collect();
            ParseError::UnexpectedToken {
                expected,
                unexpected,
                pos,
            }
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
pub fn ident(str: &str) -> Result<Ident, ParseError> {
    let mut pairs = YilParser::parse(Rule::ident, str).map_err(pest_err_to_parse_err)?;
    let ident_pair = pairs.nth(0).unwrap();
    Ok(Ident {
        name: ident_pair.as_str().to_string(),
        pos: span_to_pos_info(&ident_pair.as_span()),
    })
}

pub fn func(str: &str) -> Result<Func, ParseError> {
    let mut pairs = YilParser::parse(Rule::func, str).map_err(pest_err_to_parse_err)?;
    let func_pair = pairs.nth(0).unwrap();
    assert_eq!(func_pair.as_rule(), Rule::func);

    func_from_pair(func_pair)
}

fn func_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Func, ParseError> {
    let pos = span_to_pos_info(&pair.as_span());
    let mut pairs = pair.into_inner();
    let is_rec = match pairs.next().unwrap().as_rule() {
        Rule::kw_rec => {
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::kw_func);
            true
        }
        Rule::kw_func => false,
        _ => unreachable!(),
    };

    let name = ident_from_pair(pairs.next().unwrap())?;

    let mut params = vec![];
    let mut pair = pairs.next().unwrap();
    while pair.as_rule() == Rule::refine_type {
        params.push(refine_type_from_pair(pair)?);
        pair = pairs.next().unwrap();
    }

    assert_eq!(pair.as_rule(), Rule::colon);

    let ret = refine_type_from_pair(pairs.next().unwrap())?;
    let body = paren_expr_from_pair(pairs.next().unwrap())?;

    Ok(Func {
        name,
        params,
        ret,
        is_rec,
        body,
        pos,
    })
}

fn base_type_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<BaseType, ParseError> {
    let info = span_to_pos_info(&pair.as_span());
    let mut pairs = pair.into_inner();
    match pairs.next().unwrap().as_rule() {
        Rule::kw_int => Ok(BaseType::Int(info)),
        _ => unreachable!(),
    }
}

fn formula_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Formula, ParseError> {
    let info = span_to_pos_info(&pair.as_span());
    let mut pairs = pair.into_inner();
    match pairs.next().unwrap().as_rule() {
        Rule::kw_true => Ok(Formula::True(info)),
        _ => unreachable!(),
    }
}

fn refine_type_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<RefineType, ParseError> {
    let pos = span_to_pos_info(&pair.as_span());
    let mut pairs = pair.into_inner();

    assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
    let param_name = ident_from_pair(pairs.next().unwrap())?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::colon);
    let base_type = base_type_from_pair(pairs.next().unwrap())?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::bar);
    let formula = formula_from_pair(pairs.next().unwrap())?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
    Ok(RefineType {
        param_name,
        base_type,
        formula,
        pos,
    })
}

#[cfg(test)]
pub fn expr(str: &str) -> Result<Expr, ParseError> {
    let mut pairs = YilParser::parse(Rule::expr, str).map_err(pest_err_to_parse_err)?;
    let expr_pair = pairs.nth(0).unwrap();
    assert_eq!(expr_pair.as_rule(), Rule::expr);

    expr_from_pair(expr_pair)
}

fn expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    let additive_expr_pair = pairs.next().unwrap();
    assert_eq!(additive_expr_pair.as_rule(), Rule::additive_expr);
    let head_additive_expr = additive_expr_from_pair(additive_expr_pair)?;
    Ok(head_additive_expr)
}

fn additive_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr, ParseError> {
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

fn multive_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr, ParseError> {
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

fn primary_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr, ParseError> {
    let mut pairs_iter = pair.into_inner();
    let pair = pairs_iter.next().unwrap();

    match pair.as_rule() {
        Rule::ifz_expr => ifz_expr_from_pair(pair),
        Rule::let_expr => let_expr_from_pair(pair),
        Rule::constant => constant_from_pair(pair).map(|c| Expr::Constant(c)),
        Rule::variable => ident_from_pair(pair).map(|ident| Expr::Var(ident)),
        Rule::paren_expr => paren_expr_from_pair(pair),
        _ => unreachable!(),
    }
}

fn ifz_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr, ParseError> {
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

fn let_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr, ParseError> {
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

fn constant_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Constant, ParseError> {
    Ok(Constant {
        val: pair.as_str().parse().unwrap(),
        pos: PosInfo {
            start: pair.as_span().start(),
            end: pair.as_span().end(),
        },
    })
}

fn paren_expr_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
    let e = expr_from_pair(pairs.next().unwrap())?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
    Ok(e)
}

fn ident_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<Ident, ParseError> {
    Ok(Ident {
        name: pair.as_str().to_string(),
        pos: span_to_pos_info(&pair.as_span()),
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
                    pos: PosInfo { start: 0, end: 1 }
                },)),
                Box::new(Expr::Constant(Constant {
                    val: 2,
                    pos: PosInfo { start: 4, end: 5 }
                })),
                PosInfo { start: 0, end: 5 }
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
        expr("1 + ifz a then 41 else 90 + 9").unwrap(),
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
                    pos: PosInfo { start: 15, end: 17 }
                })),
                Box::new(Expr::BinApp(
                    BinOp::Add(PosInfo { start: 26, end: 27 }),
                    Box::new(Expr::Constant(Constant {
                        val: 90,
                        pos: PosInfo { start: 23, end: 25 }
                    })),
                    Box::new(Expr::Constant(Constant {
                        val: 9,
                        pos: PosInfo { start: 28, end: 29 }
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
                pos: PosInfo { start: 4, end: 5 }
            },
            Box::new(Expr::Constant(Constant {
                val: 1,
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

    assert_eq!(
        func("rec func hoge (x: int | true): (y: int | true) ( x ) ").unwrap(),
        Func {
            name: Ident {
                name: "hoge".to_string(),
                pos: PosInfo { start: 9, end: 13 }
            },
            params: vec![RefineType {
                param_name: Ident {
                    name: "x".to_string(),
                    pos: PosInfo { start: 15, end: 16 }
                },
                base_type: BaseType::Int(PosInfo { start: 18, end: 21 }),
                formula: Formula::True(PosInfo { start: 24, end: 28 }),
                pos: PosInfo { start: 14, end: 29 }
            }],
            ret: RefineType {
                param_name: Ident {
                    name: "y".to_string(),
                    pos: PosInfo { start: 32, end: 33 }
                },
                base_type: BaseType::Int(PosInfo { start: 35, end: 38 }),
                formula: Formula::True(PosInfo { start: 41, end: 45 }),
                pos: PosInfo { start: 31, end: 46 }
            },
            is_rec: true,
            body: Expr::Var(Ident {
                name: "x".to_string(),
                pos: PosInfo { start: 49, end: 50 }
            }),
            pos: PosInfo { start: 0, end: 52 }
        }
    );
}
