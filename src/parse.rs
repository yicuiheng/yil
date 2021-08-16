use crate::ast::*;

use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use std::collections::HashMap;

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
        Rule::primary_logical_expr => "logical primary expr",
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
        Rule::and => "&",
        Rule::or => "|",
        Rule::eq => "=",
        Rule::neq => "<>",
        Rule::lt => "<",
        Rule::leq => "<=",
        Rule::gt => ">",
        Rule::geq => ">=",
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
        Rule::kw_false => "'false'",
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

pub fn program(str: &str) -> Result<Program, ParseError> {
    let mut pairs = YilParser::parse(Rule::program, str).map_err(pest_err_to_parse_err)?;
    let program_pair = pairs.nth(0).unwrap();
    assert_eq!(program_pair.as_rule(), Rule::program);

    program_from_pair(program_pair)
}

fn program_from_pair(pair: Pair<Rule>) -> Result<Program, ParseError> {
    assert_eq!(pair.as_rule(), Rule::program);
    let pos = span_to_pos_info(&pair.as_span());
    let mut pairs = pair.into_inner();
    let mut funcs = vec![];

    while let Some(func_pair) = pairs.next() {
        assert_eq!(func_pair.as_rule(), Rule::func);
        funcs.push(func_from_pair(func_pair)?);
    }
    Ok(Program {
        funcs: funcs,
        pos: pos,
    })
}

#[cfg(test)]
pub fn func(str: &str) -> Result<Func, ParseError> {
    let mut pairs = YilParser::parse(Rule::func, str).map_err(pest_err_to_parse_err)?;
    let func_pair = pairs.nth(0).unwrap();
    assert_eq!(func_pair.as_rule(), Rule::func);

    func_from_pair(func_pair)
}

fn func_from_pair(pair: Pair<Rule>) -> Result<Func, ParseError> {
    assert_eq!(pair.as_rule(), Rule::func);

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

fn base_type_from_pair(pair: Pair<Rule>) -> Result<BaseType, ParseError> {
    assert_eq!(pair.as_rule(), Rule::base_type);
    let info = span_to_pos_info(&pair.as_span());
    let mut pairs = pair.into_inner();
    match pairs.next().unwrap().as_rule() {
        Rule::kw_int => Ok(BaseType::Int(info)),
        _ => unreachable!(),
    }
}

fn binop<T, OpT, OpF: Fn(PosInfo) -> OpT>(
    mut pairs: Pairs<Rule>,
    child_rule: Rule,
    child_from_pair: fn(Pair<Rule>) -> Result<T, ParseError>,
    op_map: HashMap<Rule, OpF>,
    ctor: fn(OpT, T, T, PosInfo) -> T,
) -> Result<T, ParseError> {
    let pair = pairs.next().unwrap();
    let start_pos = pair.as_span().start();
    assert_eq!(pair.as_rule(), child_rule);
    let mut acc = child_from_pair(pair)?;
    while let Some(pair) = pairs.next() {
        let op = op_map.get(&pair.as_rule()).unwrap();
        let op_pos = span_to_pos_info(&pair.as_span());
        let pair = pairs.next().unwrap();
        assert_eq!(pair.as_rule(), child_rule);
        let end_pos = pair.as_span().end();
        let rhs = child_from_pair(pair)?;
        acc = ctor(
            op(op_pos),
            acc,
            rhs,
            PosInfo {
                start: start_pos,
                end: end_pos,
            },
        );
    }
    Ok(acc)
}

fn formula_from_pair(pair: Pair<Rule>) -> Result<logic::Formula, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;

    assert_eq!(pair.as_rule(), Rule::formula);
    binop(
        pair.into_inner(),
        Rule::and_formula,
        and_formula_from_pair,
        HashMap::from_iter(IntoIter::new([(
            Rule::or,
            Box::new(|_| logic::Formula::Or),
        )])),
        |op, lhs, rhs, pos| op(Box::new(lhs), Box::new(rhs), pos),
    )
}

fn and_formula_from_pair(pair: Pair<Rule>) -> Result<logic::Formula, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;

    assert_eq!(pair.as_rule(), Rule::and_formula);
    binop(
        pair.into_inner(),
        Rule::binop_formula,
        binop_formula_from_pair,
        HashMap::from_iter(IntoIter::new([(
            Rule::and,
            Box::new(|_| logic::Formula::And),
        )])),
        |op, lhs, rhs, pos| op(Box::new(lhs), Box::new(rhs), pos),
    )
}

fn binop_formula_from_pair(pair: Pair<Rule>) -> Result<logic::Formula, ParseError> {
    assert_eq!(pair.as_rule(), Rule::binop_formula);
    let pos = span_to_pos_info(&pair.as_span());

    let mut pairs = pair.into_inner();
    Ok(match pairs.peek().unwrap().as_rule() {
        Rule::kw_true => logic::Formula::True(pos),
        Rule::kw_false => logic::Formula::False(pos),
        Rule::additive_logical_expr => {
            let expr1 = additive_logical_expr_from_pair(pairs.next().unwrap())?;
            let op_pair = pairs.next().unwrap();
            let op_pos = span_to_pos_info(&op_pair.as_span());
            let op = match op_pair.as_rule() {
                Rule::eq => logic::BinPred::Eq(op_pos),
                Rule::neq => logic::BinPred::Neq(op_pos),
                Rule::lt => logic::BinPred::Lt(op_pos),
                Rule::leq => logic::BinPred::Leq(op_pos),
                Rule::gt => logic::BinPred::Gt(op_pos),
                Rule::geq => logic::BinPred::Geq(op_pos),
                _ => unreachable!(),
            };
            let expr2 = additive_logical_expr_from_pair(pairs.next().unwrap())?;
            logic::Formula::BinApp(op, expr1, expr2, pos)
        }
        _ => unreachable!(),
    })
}

fn additive_logical_expr_from_pair(pair: Pair<Rule>) -> Result<logic::Expr, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::additive_logical_expr);

    binop(
        pair.into_inner(),
        Rule::multive_logical_expr,
        multive_logical_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::plus, logic::BinOp::Add as fn(PosInfo) -> logic::BinOp),
            (Rule::minus, logic::BinOp::Sub),
        ])),
        |op, lhs, rhs, pos| logic::Expr::BinApp(op, Box::new(lhs), Box::new(rhs), pos),
    )
}

fn multive_logical_expr_from_pair(pair: Pair<Rule>) -> Result<logic::Expr, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::multive_logical_expr);

    binop(
        pair.into_inner(),
        Rule::primary_logical_expr,
        primary_logical_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::ast, logic::BinOp::Mult as fn(PosInfo) -> logic::BinOp),
            (Rule::slash, logic::BinOp::Div),
        ])),
        |op, lhs, rhs, pos| logic::Expr::BinApp(op, Box::new(lhs), Box::new(rhs), pos),
    )
}

fn primary_logical_expr_from_pair(pair: Pair<Rule>) -> Result<logic::Expr, ParseError> {
    assert_eq!(pair.as_rule(), Rule::primary_logical_expr);
    let mut pairs = pair.into_inner();

    let pair = pairs.next().unwrap();
    match pair.as_rule() {
        Rule::ident => Ok(logic::Expr::Var(ident_from_pair(pair)?)),
        Rule::constant => Ok(logic::Expr::Constant(constant_from_pair(pair)?)),
        Rule::left_paren => {
            let e = additive_logical_expr_from_pair(pairs.next().unwrap())?;
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
            Ok(e)
        }
        _ => unreachable!(),
    }
}

fn refine_type_from_pair(pair: Pair<Rule>) -> Result<RefineType, ParseError> {
    assert_eq!(pair.as_rule(), Rule::refine_type);
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

fn expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    assert_eq!(pair.as_rule(), Rule::expr);
    let mut pairs = pair.into_inner();
    let or_expr_pair = pairs.next().unwrap();
    assert_eq!(or_expr_pair.as_rule(), Rule::or_expr);
    let head_or_expr = or_expr_from_pair(or_expr_pair)?;
    Ok(head_or_expr)
}

fn or_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::or_expr);

    binop(
        pair.into_inner(),
        Rule::and_expr,
        and_expr_from_pair,
        HashMap::from_iter(IntoIter::new([(Rule::or, BinOp::Or)])),
        |op, lhs, rhs, pos| Expr::BinApp(op, Box::new(lhs), Box::new(rhs), pos),
    )
}

fn and_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::and_expr);

    binop(
        pair.into_inner(),
        Rule::comp_expr,
        comp_expr_from_pair,
        HashMap::from_iter(IntoIter::new([(Rule::and, BinOp::And)])),
        |op, lhs, rhs, pos| Expr::BinApp(op, Box::new(lhs), Box::new(rhs), pos),
    )
}

fn comp_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::comp_expr);

    binop(
        pair.into_inner(),
        Rule::additive_expr,
        additive_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::eq, BinOp::Eq as fn(PosInfo) -> BinOp),
            (Rule::neq, BinOp::Neq),
            (Rule::lt, BinOp::Lt),
            (Rule::leq, BinOp::Leq),
            (Rule::gt, BinOp::Gt),
            (Rule::geq, BinOp::Geq),
        ])),
        |op, lhs, rhs, pos| Expr::BinApp(op, Box::new(lhs), Box::new(rhs), pos),
    )
}

fn additive_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::additive_expr);

    binop(
        pair.into_inner(),
        Rule::multive_expr,
        multive_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::plus, BinOp::Add as fn(PosInfo) -> BinOp),
            (Rule::minus, BinOp::Sub),
        ])),
        |op, lhs, rhs, pos| Expr::BinApp(op, Box::new(lhs), Box::new(rhs), pos),
    )
}

fn multive_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::multive_expr);

    binop(
        pair.into_inner(),
        Rule::apply_expr,
        apply_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::ast, BinOp::Mul as fn(PosInfo) -> BinOp),
            (Rule::slash, BinOp::Div),
        ])),
        |op, lhs, rhs, pos| Expr::BinApp(op, Box::new(lhs), Box::new(rhs), pos),
    )
}

fn apply_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    assert_eq!(pair.as_rule(), Rule::apply_expr);
    let pos = span_to_pos_info(&pair.as_span());
    let mut pairs = pair.into_inner();
    let head_pair = pairs.next().unwrap();

    if head_pair.as_rule() == Rule::primary_expr {
        primary_expr_from_pair(head_pair)
    } else if head_pair.as_rule() == Rule::ident {
        let func_name = ident_from_pair(head_pair)?;
        assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
        let mut args = vec![];

        let pair = pairs.next().unwrap();
        if pair.as_rule() != Rule::right_paren {
            args.push(expr_from_pair(pair)?);

            loop {
                let pair = pairs.next().unwrap();
                match pair.as_rule() {
                    Rule::right_paren => break,
                    Rule::comma =>
                        args.push(expr_from_pair(pairs.next().unwrap())?),
                    _ => unreachable!(),
                }
            }
        }

        Ok(Expr::FuncApp(func_name, args, pos))
    } else {
        unreachable!()
    }
}

fn primary_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    assert_eq!(pair.as_rule(), Rule::primary_expr);
    let mut pairs_iter = pair.into_inner();
    let pair = pairs_iter.next().unwrap();

    match pair.as_rule() {
        Rule::ifz_expr => ifz_expr_from_pair(pair),
        Rule::let_expr => let_expr_from_pair(pair),
        Rule::constant => constant_from_pair(pair).map(|c| Expr::Constant(c)),
        Rule::variable => variable_from_pair(pair),
        Rule::paren_expr => paren_expr_from_pair(pair),
        _ => unreachable!(),
    }
}

fn ifz_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    assert_eq!(pair.as_rule(), Rule::ifz_expr);
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

fn let_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    assert_eq!(pair.as_rule(), Rule::let_expr);
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

fn constant_from_pair(pair: Pair<Rule>) -> Result<Constant, ParseError> {
    assert_eq!(pair.as_rule(), Rule::constant);
    Ok(Constant {
        val: pair.as_str().parse().unwrap(),
        pos: PosInfo {
            start: pair.as_span().start(),
            end: pair.as_span().end(),
        },
    })
}

fn variable_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    assert_eq!(pair.as_rule(), Rule::variable);
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    assert_eq!(pair.as_rule(), Rule::ident);
    Ok(Expr::Var(ident_from_pair(pair)?))
}

fn paren_expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    assert_eq!(pair.as_rule(), Rule::paren_expr);
    let mut pairs = pair.into_inner();
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
    let e = expr_from_pair(pairs.next().unwrap())?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
    Ok(e)
}

fn ident_from_pair(pair: Pair<Rule>) -> Result<Ident, ParseError> {
    assert_eq!(pair.as_rule(), Rule::ident);
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
                PosInfo { start: 1, end: 7 }
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
                formula: logic::Formula::True(PosInfo { start: 24, end: 28 }),
                pos: PosInfo { start: 14, end: 29 }
            }],
            ret: RefineType {
                param_name: Ident {
                    name: "y".to_string(),
                    pos: PosInfo { start: 32, end: 33 }
                },
                base_type: BaseType::Int(PosInfo { start: 35, end: 38 }),
                formula: logic::Formula::True(PosInfo { start: 41, end: 45 }),
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
