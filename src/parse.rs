use crate::ast::*;

mod error;
mod test;
mod util;

use itertools::Position;
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use std::collections::HashMap;

pub use error::{print_error, ParseError};
use util::*;

#[derive(Parser)]
#[grammar = "parse/grammar.pest"]
struct YilParser;

pub fn program(str: &str) -> Result<Program, ParseError> {
    let mut pairs = YilParser::parse(Rule::program, str)?;
    let program_pair = pairs.nth(0).unwrap();
    assert_eq!(program_pair.as_rule(), Rule::program);

    program_from_pair(program_pair)
}

#[cfg(test)]
pub fn func(str: &str) -> Result<Func, ParseError> {
    let mut pairs = YilParser::parse(Rule::func, str)?;
    let func_pair = pairs.nth(0).unwrap();
    assert_eq!(func_pair.as_rule(), Rule::func);

    func_from_pair(func_pair)
}

#[cfg(test)]
pub fn typ(str: &str) -> Result<Type, ParseError> {
    let mut pairs = YilParser::parse(Rule::refine_type, str)?;
    let type_pair = pairs.next().unwrap();
    assert_eq!(type_pair.as_rule(), Rule::refine_type);
    assert!(pairs.next().is_none());

    refine_type_from_pair(type_pair)
}

#[cfg(test)]
pub fn expr(str: &str) -> Result<Expr, ParseError> {
    let mut pairs = YilParser::parse(Rule::expr, str)?;
    let expr_pair = pairs.nth(0).unwrap();
    assert_eq!(expr_pair.as_rule(), Rule::expr);

    expr_from_pair(expr_pair)
}

#[cfg(test)]
pub fn ident(str: &str) -> Result<Ident, ParseError> {
    let mut pairs = YilParser::parse(Rule::ident, str)?;
    let ident_pair = pairs.nth(0).unwrap();
    Ok(Ident {
        name: ident_pair.as_str().to_string(),
        pos: span_to_pos_info(&ident_pair.as_span()),
    })
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
        Rule::imply_formula,
        imply_formula_from_pair,
        HashMap::from_iter(IntoIter::new([(Rule::and, |_| logic::Formula::And)])),
        |op, lhs, rhs, pos| op(Box::new(lhs), Box::new(rhs), pos),
    )
}

fn imply_formula_from_pair(pair: Pair<Rule>) -> Result<logic::Formula, ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;

    assert_eq!(pair.as_rule(), Rule::imply_formula);
    binop(
        pair.into_inner(),
        Rule::binop_formula,
        binop_formula_from_pair,
        HashMap::from_iter(IntoIter::new([(Rule::fat_arrow, |_| {
            logic::Formula::Imply
        })])),
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
        Rule::left_paren => {
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
            let formula = formula_from_pair(pairs.next().unwrap())?;
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
            formula
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
            (Rule::percent, logic::BinOp::Surplus),
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

fn refine_type_from_pair(pair: Pair<Rule>) -> Result<Type, ParseError> {
    assert_eq!(pair.as_rule(), Rule::refine_type);
    let mut pairs = pair.into_inner();

    let pair = pairs.next().unwrap();
    let start_pos = pair.as_span().start();
    assert_eq!(pair.as_rule(), Rule::primary_refine_type);
    let mut param_types = vec![];
    let mut ret_type = primary_refine_type_from_pair(pair)?;
    let mut end_pos = 0;

    while let Some(pair) = pairs.next() {
        assert_eq!(pair.as_rule(), Rule::arrow);
        let pair = pairs.next().unwrap();
        end_pos = pair.as_span().end();
        assert_eq!(pair.as_rule(), Rule::primary_refine_type);
        param_types.push(ret_type);
        ret_type = primary_refine_type_from_pair(pair)?;
    }
    if param_types.is_empty() {
        Ok(ret_type)
    } else {
        Ok(Type::FuncType(FuncType {
            params: param_types,
            ret: Box::new(ret_type),
            pos: PosInfo {
                start: start_pos,
                end: end_pos,
            },
        }))
    }
}

fn primary_refine_type_from_pair(pair: Pair<Rule>) -> Result<Type, ParseError> {
    assert_eq!(pair.as_rule(), Rule::primary_refine_type);
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();

    match pair.as_rule() {
        Rule::refine_non_func_type => refine_non_func_type_from_pair(pair),
        Rule::left_paren => {
            let pair = pairs.next().unwrap();
            assert_eq!(pair.as_rule(), Rule::refine_type);
            let typ = refine_type_from_pair(pair);
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
            typ
        }
        _ => unreachable!(),
    }
}

fn refine_non_func_type_from_pair(pair: Pair<Rule>) -> Result<Type, ParseError> {
    assert_eq!(pair.as_rule(), Rule::refine_non_func_type);
    let pos = span_to_pos_info(&pair.as_span());
    let mut pairs = pair.into_inner();

    assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
    let param_name = ident_from_pair(pairs.next().unwrap())?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::colon);
    let base_type = base_type_from_pair(pairs.next().unwrap())?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::bar);
    let formula = formula_from_pair(pairs.next().unwrap())?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
    Ok(Type::NonFuncType(NonFuncType {
        param_name,
        base_type,
        formula,
        pos,
    }))
}

fn expr_from_pair(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    assert_eq!(pair.as_rule(), Rule::expr);
    let mut pairs_iter = pair.into_inner();
    let pair = pairs_iter.next().unwrap();

    match pair.as_rule() {
        Rule::let_expr => let_expr_from_pair(pair),
        Rule::or_expr => or_expr_from_pair(pair),
        _ => unreachable!(),
    }
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
            (Rule::percent, BinOp::Surplus),
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
                    Rule::comma => args.push(expr_from_pair(pairs.next().unwrap())?),
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

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::left_brace);

    let expr1_pair = pairs_iter.next().unwrap();
    assert_eq!(expr1_pair.as_rule(), Rule::expr);
    let expr1 = expr_from_pair(expr1_pair)?;

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::right_brace);
    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_else);
    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::left_brace);

    let expr2_pair = pairs_iter.next().unwrap();
    assert_eq!(expr2_pair.as_rule(), Rule::expr);
    let expr2 = expr_from_pair(expr2_pair)?;

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::right_brace);

    Ok(Expr::Ifz(
        Box::new(cond),
        Box::new(expr1),
        Box::new(expr2),
        pos_info,
    ))
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
