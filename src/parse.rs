use crate::ast::*;
use crate::env::NameEnv;

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

pub fn program(str: &str) -> Result<(Program, NameEnv), ParseError> {
    let mut pairs = YilParser::parse(Rule::program, str)?;
    let program_pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    program_from_pair(program_pair)
}

#[cfg(test)]
pub fn func(str: &str) -> Result<(Func, NameEnv), ParseError> {
    let mut pairs = YilParser::parse(Rule::func, str)?;
    let func_pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    assert_eq!(func_pair.as_rule(), Rule::func);

    let mut name_env = NameEnv::empty();
    let mut name_to_ident = HashMap::new();

    let (func_name, is_rec, info, pairs) = func_former_part_from_pair(func_pair);
    let func_ident = Ident::fresh();
    name_env.insert(func_ident, func_name.clone());
    name_to_ident.insert(func_name, func_ident);

    let (func, name_env_) = func_from_later_pairs(func_ident, is_rec, info, &name_to_ident, pairs)?;
    name_env.append(name_env_);
    Ok((func, name_env))
}

#[cfg(test)]
pub fn typ(str: &str) -> Result<(Type, NameEnv), ParseError> {
    let mut pairs = YilParser::parse(Rule::refine_type, str)?;
    let type_pair = pairs.next().unwrap();
    assert!(pairs.next().is_none());
    assert_eq!(type_pair.as_rule(), Rule::refine_type);

    refine_type_from_pair(type_pair, &HashMap::new())
}

#[cfg(test)]
pub fn expr_with_name_map(
    str: &str,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    let mut pairs = YilParser::parse(Rule::expr, str)?;
    let expr_pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    assert_eq!(expr_pair.as_rule(), Rule::expr);

    expr_from_pair(expr_pair, name_to_ident)
}

#[cfg(test)]
pub fn expr(str: &str) -> Result<(Expr, NameEnv), ParseError> {
    expr_with_name_map(str, &HashMap::new())
}

#[cfg(test)]
pub fn ident(str: &str) -> Result<String, ParseError> {
    let mut pairs = YilParser::parse(Rule::ident, str)?;
    let ident_pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    Ok(ident_pair.as_str().to_string())
}

fn program_from_pair(pair: Pair<Rule>) -> Result<(Program, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::program);
    let info = pair_to_info(&pair);
    let mut pairs = pair.into_inner();

    let mut name_env = NameEnv::empty();
    let mut name_to_ident = HashMap::new();

    let mut func_later_pairss = vec![];

    while let Some(func_pair) = pairs.next() {
        if func_pair.as_rule() == Rule::EOI {
            break;
        }
        assert_eq!(func_pair.as_rule(), Rule::func);
        // TODO: is_rec を反映させるか仕様から ommit する
        let (func_name, is_rec, info, pairs) = func_former_part_from_pair(func_pair);
        let func_ident = Ident::fresh();
        name_env.insert(func_ident, func_name.clone());
        name_to_ident.insert(func_name, func_ident);
        func_later_pairss.push((func_ident, is_rec, info, pairs));
    }

    let mut funcs = vec![];
    for (func_ident, is_rec, info, func_later_pairs) in func_later_pairss {
        let (func, name_env_) =
            func_from_later_pairs(func_ident, is_rec, info, &name_to_ident, func_later_pairs)?;
        name_env.append(name_env_);
        funcs.push(func);
    }

    Ok((Program { funcs, info }, name_env))
}

fn func_former_part_from_pair(pair: Pair<Rule>) -> (String, bool, Info, Pairs<Rule>) {
    assert_eq!(pair.as_rule(), Rule::func);
    let info = pair_to_info(&pair);

    let mut pairs = pair.into_inner();
    let is_rec = match pairs.peek().unwrap().as_rule() {
        Rule::kw_rec => {
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::kw_rec);
            true
        }
        Rule::kw_func => false,
        _ => unreachable!(),
    };
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::kw_func);

    let (func_name, _) = name_from_pair(pairs.next().unwrap());
    (func_name, is_rec, info, pairs)
}

fn func_from_later_pairs(
    func_ident: Ident,
    is_rec: bool,
    info: Info,
    name_to_ident: &HashMap<String, Ident>,
    mut pairs: Pairs<Rule>,
) -> Result<(Func, NameEnv), ParseError> {
    let mut name_env = NameEnv::empty();
    let mut name_to_ident = name_to_ident.clone();

    let mut param_types = vec![];
    let mut pair = pairs.next().unwrap();
    while pair.as_rule() == Rule::refine_type {
        let param_type_info = pair_to_info(&pair);
        let (param_type, name_env_) = refine_type_from_pair(pair, &name_to_ident)?;
        name_env.append(name_env_);
        let param_ident = if let Type::IntType(ident, _, _) = &param_type {
            *ident
        } else {
            unreachable!()
        };
        name_to_ident.insert(name_env.lookup(param_ident).clone(), param_ident);
        param_types.push((param_type, param_type_info));
        pair = pairs.next().unwrap();
    }

    assert_eq!(pair.as_rule(), Rule::colon);

    let ret_pair = pairs.next().unwrap();
    let ret_info = pair_to_info(&ret_pair);
    let (ret, name_env_) = refine_type_from_pair(ret_pair, &name_to_ident)?;
    name_env.append(name_env_);

    let typ = param_types
        .into_iter()
        .rev()
        .fold(ret, |acc, (param_type, param_type_info)| {
            let ident = if let Type::IntType(param_ident, _, _) = &param_type {
                *param_ident
            } else {
                Ident::fresh()
            };
            Type::FuncType(
                ident,
                Box::new(param_type),
                Box::new(acc),
                Info::merge(param_type_info, ret_info),
            )
        });

    let (body, name_env_) = paren_expr_from_pair(pairs.next().unwrap(), &name_to_ident)?;
    name_env.append(name_env_);

    Ok((
        Func {
            ident: func_ident,
            typ,
            is_rec,
            body,
            info,
        },
        name_env,
    ))
}

fn base_type_from_pair(pair: Pair<Rule>) -> Result<(), ParseError> {
    assert_eq!(pair.as_rule(), Rule::base_type);
    Ok(())
}

fn binop<T, BinOpT: Copy>(
    mut pairs: Pairs<Rule>,
    name_to_ident: &HashMap<String, Ident>,
    child_rule: Rule,
    child_from_pair: fn(
        Pair<Rule>,
        name_to_ident: &HashMap<String, Ident>,
    ) -> Result<(T, NameEnv), ParseError>,
    op_map: HashMap<Rule, BinOpT>,
    ctor: fn(BinOpT, Box<T>, Box<T>, Info) -> T,
) -> Result<(T, NameEnv), ParseError> {
    let pair = pairs.next().unwrap();
    let start_info = pair_to_info(&pair);
    assert_eq!(pair.as_rule(), child_rule);
    let (mut acc, mut name_env) = child_from_pair(pair, name_to_ident)?;
    while let Some(pair) = pairs.next() {
        let op = *op_map.get(&pair.as_rule()).unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pair.as_rule(), child_rule);
        let end_info = pair_to_info(&pair);
        let (rhs, name_env_) = child_from_pair(pair, name_to_ident)?;
        name_env.append(name_env_);
        acc = ctor(
            op,
            Box::new(acc),
            Box::new(rhs),
            Info::merge(start_info, end_info),
        );
    }
    Ok((acc, name_env))
}

fn formula_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(logic::Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;

    assert_eq!(pair.as_rule(), Rule::formula);
    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::and_formula,
        and_formula_from_pair,
        HashMap::from_iter(IntoIter::new([(Rule::or, logic::BinOp::Or)])),
        logic::Term::Bin,
    )
}

fn and_formula_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(logic::Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;

    assert_eq!(pair.as_rule(), Rule::and_formula);
    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::imply_formula,
        imply_formula_from_pair,
        HashMap::from_iter(IntoIter::new([(Rule::and, logic::BinOp::And)])),
        logic::Term::Bin,
    )
}

fn imply_formula_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(logic::Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;

    assert_eq!(pair.as_rule(), Rule::imply_formula);
    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::binop_formula,
        binop_formula_from_pair,
        HashMap::from_iter(IntoIter::new([(Rule::fat_arrow, logic::BinOp::Imply)])),
        logic::Term::Bin,
    )
}

fn binop_formula_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(logic::Term, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::binop_formula);
    let info = pair_to_info(&pair);

    let mut pairs = pair.into_inner();
    Ok(match pairs.peek().unwrap().as_rule() {
        Rule::kw_true => (logic::Term::True(info), NameEnv::empty()),
        Rule::kw_false => (logic::Term::False(info), NameEnv::empty()),
        Rule::additive_logical_expr => {
            let mut name_env = NameEnv::empty();
            let (expr1, name_env_) =
                additive_logical_expr_from_pair(pairs.next().unwrap(), name_to_ident)?;
            name_env.append(name_env_);
            let op_pair = pairs.next().unwrap();
            let op_info = pair_to_info(&op_pair);
            let op = match op_pair.as_rule() {
                Rule::eq => logic::BinOp::Eq,
                Rule::neq => logic::BinOp::Neq,
                Rule::lt => logic::BinOp::Lt,
                Rule::leq => logic::BinOp::Leq,
                Rule::gt => logic::BinOp::Gt,
                Rule::geq => logic::BinOp::Geq,
                _ => unreachable!(),
            };
            let (expr2, name_env_) =
                additive_logical_expr_from_pair(pairs.next().unwrap(), name_to_ident)?;
            name_env.append(name_env_);
            (
                logic::Term::Bin(op, Box::new(expr1), Box::new(expr2), op_info),
                name_env,
            )
        }
        Rule::left_paren => {
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
            let (formula, name_env) = formula_from_pair(pairs.next().unwrap(), name_to_ident)?;
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
            (formula, name_env)
        }
        _ => unreachable!(),
    })
}

fn additive_logical_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(logic::Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::additive_logical_expr);

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::multive_logical_expr,
        multive_logical_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::plus, logic::BinOp::Add),
            (Rule::minus, logic::BinOp::Sub),
        ])),
        logic::Term::Bin,
    )
}

fn multive_logical_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(logic::Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::multive_logical_expr);

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::primary_logical_expr,
        primary_logical_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::ast, logic::BinOp::Mult),
            (Rule::slash, logic::BinOp::Div),
            (Rule::percent, logic::BinOp::Rem),
        ])),
        logic::Term::Bin,
    )
}

fn primary_logical_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(logic::Term, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::primary_logical_expr);
    let info = pair_to_info(&pair);
    let mut pairs = pair.into_inner();

    let pair = pairs.next().unwrap();
    match pair.as_rule() {
        Rule::ident => {
            let (name, info) = name_from_pair(pair);
            let id = *name_to_ident.get(&name).unwrap();
            Ok((logic::Term::Var(id, info), NameEnv::empty()))
        }
        Rule::constant => {
            let n = integer_constant_from_pair(pair);
            Ok((logic::Term::Num(n, info), NameEnv::empty()))
        }
        Rule::left_paren => {
            let e = additive_logical_expr_from_pair(pairs.next().unwrap(), name_to_ident)?;
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
            Ok(e)
        }
        _ => unreachable!(),
    }
}

fn refine_type_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Type, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::refine_type);
    let mut pairs = pair.into_inner();

    let mut name_env = NameEnv::empty();

    let (mut ret_type, name_env_) =
        primary_refine_type_from_pair(pairs.next().unwrap(), name_to_ident)?;
    name_env.append(name_env_);
    let mut param_types = vec![];

    while let Some(pair) = pairs.next() {
        assert_eq!(pair.as_rule(), Rule::arrow);
        let pair = pairs.next().unwrap();
        assert_eq!(pair.as_rule(), Rule::primary_refine_type);

        let (typ, name_env_) = primary_refine_type_from_pair(pair, name_to_ident)?;
        name_env.append(name_env_);
        param_types.push(ret_type);
        ret_type = typ;
    }

    Ok((
        param_types
            .into_iter()
            .rev()
            .fold(ret_type, |acc, param_type| {
                let ident = if let Type::IntType(ident, _, _) = &param_type {
                    *ident
                } else {
                    Ident::fresh()
                };
                Type::FuncType(ident, Box::new(param_type), Box::new(acc), Info::Dummy)
            }),
        name_env,
    ))
}

fn primary_refine_type_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Type, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::primary_refine_type);
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();

    match pair.as_rule() {
        Rule::refine_non_func_type => refine_non_func_type_from_pair(pair, name_to_ident),
        Rule::left_paren => {
            let pair = pairs.next().unwrap();
            assert_eq!(pair.as_rule(), Rule::refine_type);
            let typ = refine_type_from_pair(pair, name_to_ident);
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
            typ
        }
        _ => unreachable!(),
    }
}

fn refine_non_func_type_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Type, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::refine_non_func_type);
    let info = pair_to_info(&pair);
    let mut pairs = pair.into_inner();

    let mut name_env = NameEnv::empty();
    let mut name_to_ident = name_to_ident.clone();

    assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
    let (param_name, _) = name_from_pair(pairs.next().unwrap());
    let param_ident = Ident::fresh();
    name_env.insert(param_ident, param_name.clone());
    name_to_ident.insert(param_name, param_ident);
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::colon);
    let () = base_type_from_pair(pairs.next().unwrap())?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::bar);
    let (formula, name_env_) = formula_from_pair(pairs.next().unwrap(), &name_to_ident)?;
    name_env.append(name_env_);
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
    Ok((Type::IntType(param_ident, formula, info), name_env))
}

fn expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::expr);
    let mut pairs_iter = pair.into_inner();
    let pair = pairs_iter.next().unwrap();
    assert_eq!(pairs_iter.next(), None);

    match pair.as_rule() {
        Rule::let_expr => let_expr_from_pair(pair, name_to_ident),
        Rule::or_expr => or_expr_from_pair(pair, name_to_ident),
        _ => unreachable!(),
    }
}

fn let_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::let_expr);
    let info = pair_to_info(&pair);
    let mut pairs_iter = pair.into_inner();

    let mut name_env = NameEnv::empty();
    let mut name_to_ident = name_to_ident.clone();

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_let);

    let ident_pair = pairs_iter.next().unwrap();
    assert_eq!(ident_pair.as_rule(), Rule::ident);
    let (name, _) = name_from_pair(ident_pair);
    let ident = Ident::fresh();
    name_env.insert(ident, name.clone());
    name_to_ident.insert(name, ident);

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::equal);

    let expr1_pair = pairs_iter.next().unwrap();
    assert_eq!(expr1_pair.as_rule(), Rule::expr);
    let (expr1, name_env_) = expr_from_pair(expr1_pair, &name_to_ident)?;
    name_env.append(name_env_);

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_in);

    let expr2_pair = pairs_iter.next().unwrap();
    assert_eq!(expr2_pair.as_rule(), Rule::expr);
    let (expr2, name_env_) = expr_from_pair(expr2_pair, &name_to_ident)?;
    name_env.append(name_env_);

    Ok((
        Expr::Let(ident, Box::new(expr1), Box::new(expr2), info),
        name_env,
    ))
}

fn or_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::or_expr);

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::and_expr,
        and_expr_from_pair,
        HashMap::from_iter(IntoIter::new([(
            Rule::or,
            builtin::BuiltinData::instance().or_ident,
        )])),
        |op_ident, lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(op_ident, Info::Dummy)),
                    lhs,
                    info,
                )),
                rhs,
                info,
            )
        },
    )
}

fn and_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::and_expr);

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::comp_expr,
        comp_expr_from_pair,
        HashMap::from_iter(IntoIter::new([(
            Rule::and,
            builtin::BuiltinData::instance().and_ident,
        )])),
        |op_ident, lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(op_ident, Info::Dummy)),
                    lhs,
                    info,
                )),
                rhs,
                info,
            )
        },
    )
}

fn comp_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::comp_expr);

    let builtin = builtin::BuiltinData::instance();

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::additive_expr,
        additive_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::eq, builtin.eq_ident),
            (Rule::neq, builtin.neq_ident),
            (Rule::lt, builtin.lt_ident),
            (Rule::leq, builtin.leq_ident),
            (Rule::gt, builtin.gt_ident),
            (Rule::geq, builtin.geq_ident),
        ])),
        |op_ident, lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(op_ident, Info::Dummy)),
                    lhs,
                    info,
                )),
                rhs,
                info,
            )
        },
    )
}

fn additive_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::additive_expr);

    let builtin = builtin::BuiltinData::instance();

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::multive_expr,
        multive_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::plus, builtin.add_ident),
            (Rule::minus, builtin.sub_ident),
        ])),
        |op_ident, lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(op_ident, Info::Dummy)),
                    lhs,
                    info,
                )),
                rhs,
                info,
            )
        },
    )
}

fn multive_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::multive_expr);

    let builtin = builtin::BuiltinData::instance();

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::apply_expr,
        apply_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (Rule::ast, builtin.mult_ident),
            (Rule::slash, builtin.div_ident),
            (Rule::percent, builtin.rem_ident),
        ])),
        |op_ident, lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Var(op_ident, Info::Dummy)),
                    lhs,
                    info,
                )),
                rhs,
                info,
            )
        },
    )
}

fn apply_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::apply_expr);
    let info = pair_to_info(&pair);
    let mut pairs = pair.into_inner();

    let head_pair = pairs.next().unwrap();

    if head_pair.as_rule() == Rule::primary_expr {
        primary_expr_from_pair(head_pair, name_to_ident)
    } else if head_pair.as_rule() == Rule::ident {
        let mut name_env = NameEnv::empty();

        let (func_name, func_info) = name_from_pair(head_pair);
        let func_ident = *name_to_ident.get(&func_name).unwrap();
        let func_var = Expr::Var(func_ident, func_info);

        assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
        let mut args = vec![];

        let mut next_pair = pairs.next().unwrap();
        while next_pair.as_rule() != Rule::right_paren {
            let (arg, name_env_) = expr_from_pair(next_pair, name_to_ident)?;
            name_env.append(name_env_);
            args.push(arg);

            if pairs.peek().unwrap().as_rule() == Rule::comma {
                assert_eq!(pairs.next().unwrap().as_rule(), Rule::comma);
            }
            next_pair = pairs.next().unwrap();
        }

        let expr = args.into_iter().fold(func_var, |acc, arg| {
            Expr::App(Box::new(acc), Box::new(arg), info)
        });

        Ok((expr, name_env))
    } else {
        unreachable!()
    }
}

fn primary_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::primary_expr);
    let mut pairs_iter = pair.into_inner();
    let pair = pairs_iter.next().unwrap();
    let info = pair_to_info(&pair);

    match pair.as_rule() {
        Rule::ifz_expr => ifz_expr_from_pair(pair, name_to_ident),
        Rule::constant => Ok((
            Expr::Num(integer_constant_from_pair(pair), info),
            NameEnv::empty(),
        )),
        Rule::variable => variable_from_pair(pair, name_to_ident),
        Rule::paren_expr => paren_expr_from_pair(pair, name_to_ident),
        _ => unreachable!(),
    }
}

fn ifz_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::ifz_expr);
    let info = pair_to_info(&pair);
    let mut pairs_iter = pair.into_inner();

    let mut name_env = NameEnv::empty();

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_ifz);

    let cond_pair = pairs_iter.next().unwrap();
    assert_eq!(cond_pair.as_rule(), Rule::expr);
    let (cond, name_env_) = expr_from_pair(cond_pair, name_to_ident)?;
    name_env.append(name_env_);

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::left_brace);

    let expr1_pair = pairs_iter.next().unwrap();
    assert_eq!(expr1_pair.as_rule(), Rule::expr);
    let (expr1, name_env_) = expr_from_pair(expr1_pair, name_to_ident)?;
    name_env.append(name_env_);

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::right_brace);
    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_else);
    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::left_brace);

    let expr2_pair = pairs_iter.next().unwrap();
    assert_eq!(expr2_pair.as_rule(), Rule::expr);
    let (expr2, name_env_) = expr_from_pair(expr2_pair, name_to_ident)?;
    name_env.append(name_env_);

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::right_brace);

    Ok((
        Expr::Ifz(Box::new(cond), Box::new(expr1), Box::new(expr2), info),
        name_env,
    ))
}

fn integer_constant_from_pair(pair: Pair<Rule>) -> i32 {
    pair.as_str().parse().unwrap()
}

fn variable_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::variable);
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    assert_eq!(pair.as_rule(), Rule::ident);
    let (name, info) = name_from_pair(pair);
    let ident = *name_to_ident.get(&name).unwrap();
    Ok((Expr::Var(ident, info), NameEnv::empty()))
}

fn paren_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::paren_expr);
    let mut pairs = pair.into_inner();
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
    let e = expr_from_pair(pairs.next().unwrap(), name_to_ident)?;
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
    Ok(e)
}

fn name_from_pair(pair: Pair<Rule>) -> (String, Info) {
    assert_eq!(pair.as_rule(), Rule::ident);
    (pair.as_str().to_string(), pair_to_info(&pair))
}
