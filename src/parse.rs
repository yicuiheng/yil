pub mod error;
mod test;
pub mod util;

use itertools::Position;
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use std::collections::HashMap;

use crate::{
    ast::{logic::Term, *},
    env::NameEnv,
};
use error::ParseError;
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

    let (func_name, is_rec, info, pairs) = func_from_pair_step1(func_pair);
    let func_ident = Ident::fresh();
    name_env.insert(func_ident, func_name.clone());
    name_to_ident.insert(func_name, func_ident);

    let (func, name_env_) = func_from_pair_step2(func_ident, is_rec, info, &name_to_ident, pairs)?;
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
    let mut pairs = YilParser::parse(Rule::expr_for_debug, str)?;
    let expr_for_debug_pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    assert_eq!(expr_for_debug_pair.as_rule(), Rule::expr_for_debug);
    let mut pairs = expr_for_debug_pair.into_inner();
    let expr_pair = pairs.next().unwrap();
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::EOI);
    assert_eq!(expr_pair.as_rule(), Rule::expr);

    expr_from_pair(expr_pair, name_to_ident)
}

#[cfg(test)]
pub fn expr(str: &str) -> Result<(Expr, NameEnv), ParseError> {
    expr_with_name_map(str, &HashMap::new())
}

#[cfg(test)]
pub fn ident(str: &str) -> Result<String, ParseError> {
    let mut pairs = YilParser::parse(Rule::name, str)?;
    let ident_pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    Ok(ident_pair.as_str().to_string())
}

fn program_from_pair(pair: Pair<Rule>) -> Result<(Program, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::program);
    let range = pair_to_range(&pair);
    let mut pairs = pair.into_inner();

    let mut name_env = NameEnv::empty();
    let mut name_to_ident = HashMap::new();

    // builtin functions
    let builtin_data = builtin::BuiltinData::instance();
    name_env.insert(builtin_data.print_bool_ident, "print_bool".to_string());
    name_to_ident.insert("print_bool".to_string(), builtin_data.print_bool_ident);

    let mut func_later_pairss = vec![];

    while let Some(func_pair) = pairs.next() {
        if func_pair.as_rule() == Rule::EOI {
            break;
        }
        assert_eq!(func_pair.as_rule(), Rule::func);
        // TODO: is_rec を反映させるか仕様から ommit する
        let (func_name, is_rec, info, pairs) = func_from_pair_step1(func_pair);
        let func_ident = Ident::fresh();
        name_env.insert(func_ident, func_name.clone());
        name_to_ident.insert(func_name, func_ident);
        func_later_pairss.push((func_ident, is_rec, info, pairs));
    }

    let mut funcs = vec![];
    for (func_ident, is_rec, range, func_later_pairs) in func_later_pairss {
        let (func, name_env_) =
            func_from_pair_step2(func_ident, is_rec, range, &name_to_ident, func_later_pairs)?;
        name_env.append(name_env_);
        funcs.push(func);
    }

    Ok((
        Program {
            funcs,
            info: Info::from_range(range),
        },
        name_env,
    ))
}

fn func_from_pair_step1(pair: Pair<Rule>) -> (String, bool, Range, Pairs<Rule>) {
    assert_eq!(pair.as_rule(), Rule::func);
    let range = pair_to_range(&pair);

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
    (func_name, is_rec, range, pairs)
}

fn func_from_pair_step2(
    func_ident: Ident,
    is_rec: bool,
    range: Range,
    name_to_ident: &HashMap<String, Ident>,
    mut pairs: Pairs<Rule>,
) -> Result<(Func, NameEnv), ParseError> {
    let mut name_env = NameEnv::empty();
    let mut name_to_ident = name_to_ident.clone();

    let mut param_types = vec![];
    let mut pair = pairs.next().unwrap();
    while pair.as_rule() == Rule::refine_type {
        let param_type_range = pair_to_range(&pair);
        let (param_type, name_env_) = refine_type_from_pair(pair, &name_to_ident)?;
        name_env.append(name_env_);
        let param_ident = param_type.ident();
        name_to_ident.insert(name_env.lookup(param_ident).clone(), param_ident);
        param_types.push((param_type, param_type_range));
        pair = pairs.next().unwrap();
    }
    assert!(param_types.len() >= 1);
    let params_len = param_types.len();

    assert_eq!(pair.as_rule(), Rule::colon);

    let ret_pair = pairs.next().unwrap();
    let ret_range = pair_to_range(&ret_pair);
    let (ret, name_env_) = refine_type_from_pair(ret_pair, &name_to_ident)?;
    name_env.append(name_env_);

    let first_param_ident = param_types[0].0.ident();
    let typ = param_types
        .into_iter()
        .rev()
        .fold(ret, |acc, (param_type, param_type_range)| {
            let ident = if param_type.ident() == first_param_ident {
                func_ident
            } else {
                Ident::fresh()
            };
            Type::FuncType(
                ident,
                Box::new(param_type),
                Box::new(acc),
                Info::from_range(Range::merge(param_type_range, ret_range)),
            )
        });

    assert_eq!(pairs.next().unwrap().as_rule(), Rule::equal);

    let (body, name_env_) = expr_from_pair(pairs.next().unwrap(), &name_to_ident)?;
    name_env.append(name_env_);

    Ok((
        Func {
            typ,
            params_len,
            is_rec,
            body,
            info: Info::from_range(range),
        },
        name_env,
    ))
}

fn binop<T, BinOpT: Copy>(
    mut pairs: Pairs<Rule>,
    name_to_ident: &HashMap<String, Ident>,
    child_rule: Rule,
    child_from_pair: fn(
        Pair<Rule>,
        name_to_ident: &HashMap<String, Ident>,
    ) -> Result<(T, NameEnv), ParseError>,
    op_map: HashMap<Rule, fn(Info) -> BinOpT>,
    ctor: fn(BinOpT, Box<T>, Box<T>, Info) -> T,
) -> Result<(T, NameEnv), ParseError> {
    let pair = pairs.next().unwrap();
    let start_range = pair_to_range(&pair);
    assert_eq!(pair.as_rule(), child_rule);
    let (mut acc, mut name_env) = child_from_pair(pair, name_to_ident)?;
    while let Some(pair) = pairs.next() {
        let op_range = pair_to_range(&pair);
        let op = *op_map.get(&pair.as_rule()).unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pair.as_rule(), child_rule);
        let end_range = pair_to_range(&pair);
        let (rhs, name_env_) = child_from_pair(pair, name_to_ident)?;
        name_env.append(name_env_);
        acc = ctor(
            op(Info::builtin_info_from_range(op_range)),
            Box::new(acc),
            Box::new(rhs),
            Info::from_range(Range::merge(start_range, end_range)),
        );
    }
    Ok((acc, name_env))
}

fn term_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;

    assert_eq!(pair.as_rule(), Rule::term);
    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::and_term,
        and_term_from_pair,
        HashMap::from_iter(IntoIter::new([(
            Rule::or,
            (|_| logic::BinOp::Or) as fn(Info) -> logic::BinOp,
        )])),
        Term::Bin,
    )
}

fn and_term_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;

    assert_eq!(pair.as_rule(), Rule::and_term);
    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::imply_term,
        imply_term_from_pair,
        HashMap::from_iter(IntoIter::new([(
            Rule::and,
            (|_| logic::BinOp::And) as fn(Info) -> logic::BinOp,
        )])),
        Term::Bin,
    )
}

fn imply_term_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;

    assert_eq!(pair.as_rule(), Rule::imply_term);
    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::not_term,
        not_term_from_pair,
        HashMap::from_iter(IntoIter::new([(
            Rule::fat_arrow,
            (|_| logic::BinOp::Imply) as fn(Info) -> logic::BinOp,
        )])),
        Term::Bin,
    )
}

fn not_term_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Term, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::not_term);
    let range = pair_to_range(&pair);

    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    match pair.as_rule() {
        Rule::exclamation => {
            let (inner, name_env) = binpred_term_from_pair(pairs.next().unwrap(), name_to_ident)?;
            Ok((
                Term::Not(Box::new(inner), Info::from_range(range)),
                name_env,
            ))
        }
        Rule::binpred_term => binpred_term_from_pair(pair, name_to_ident),
        _ => unreachable!(),
    }
}

fn binpred_term_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Term, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::binpred_term);
    let range = pair_to_range(&pair);

    let mut pairs = pair.into_inner();
    Ok(match pairs.peek().unwrap().as_rule() {
        Rule::additive_term => {
            let mut name_env = NameEnv::empty();
            let (term1, name_env_) = additive_term_from_pair(pairs.next().unwrap(), name_to_ident)?;
            name_env.append(name_env_);
            let op_pair = pairs.next().unwrap();
            let op_range = pair_to_range(&op_pair);
            let op = match op_pair.as_rule() {
                Rule::eq => logic::BinOp::Eq,
                Rule::neq => logic::BinOp::Neq,
                Rule::lt => logic::BinOp::Lt,
                Rule::leq => logic::BinOp::Leq,
                Rule::gt => logic::BinOp::Gt,
                Rule::geq => logic::BinOp::Geq,
                _ => unreachable!(),
            };
            let (term2, name_env_) = additive_term_from_pair(pairs.next().unwrap(), name_to_ident)?;
            name_env.append(name_env_);
            (
                Term::Bin(
                    op,
                    Box::new(term1),
                    Box::new(term2),
                    Info::from_range(op_range),
                ),
                name_env,
            )
        }
        Rule::left_paren => {
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
            let (term, name_env) = term_from_pair(pairs.next().unwrap(), name_to_ident)?;
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
            (term, name_env)
        }
        Rule::kw_true => (
            Term::True(Info::builtin_info_from_range(range)),
            NameEnv::empty(),
        ),
        Rule::kw_false => (
            Term::False(Info::builtin_info_from_range(range)),
            NameEnv::empty(),
        ),
        Rule::name => {
            let (name, info) = name_from_pair(pairs.next().unwrap());
            let id = *name_to_ident.get(&name).unwrap();
            (Term::Var(id, info), NameEnv::empty())
        }
        _ => unreachable!(),
    })
}

fn additive_term_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::additive_term);

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::multive_term,
        multive_term_from_pair,
        HashMap::from_iter(IntoIter::new([
            (
                Rule::plus,
                (|_| logic::BinOp::Add) as fn(Info) -> logic::BinOp,
            ),
            (Rule::minus, |_| logic::BinOp::Sub),
        ])),
        Term::Bin,
    )
}

fn multive_term_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Term, NameEnv), ParseError> {
    use std::array::IntoIter;
    use std::iter::FromIterator;
    assert_eq!(pair.as_rule(), Rule::multive_term);

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::primary_term,
        primary_term_from_pair,
        HashMap::from_iter(IntoIter::new([
            (
                Rule::ast,
                (|_| logic::BinOp::Mult) as fn(Info) -> logic::BinOp,
            ),
            (Rule::slash, |_| logic::BinOp::Div),
            (Rule::percent, |_| logic::BinOp::Rem),
        ])),
        Term::Bin,
    )
}

fn primary_term_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Term, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::primary_term);
    let range = pair_to_range(&pair);
    let mut pairs = pair.into_inner();

    let pair = pairs.next().unwrap();
    match pair.as_rule() {
        Rule::name => {
            let (name, info) = name_from_pair(pair);
            let id = *name_to_ident.get(&name).unwrap();
            Ok((Term::Var(id, info), NameEnv::empty()))
        }
        Rule::number => {
            let n = number_from_pair(pair);
            Ok((
                Term::Num(n, Info::builtin_info_from_range(range)),
                NameEnv::empty(),
            ))
        }
        Rule::left_paren => {
            let e = additive_term_from_pair(pairs.next().unwrap(), name_to_ident)?;
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

    match pairs.peek().unwrap().as_rule() {
        Rule::base_refine_type => base_refine_type_from_pair(pairs.next().unwrap(), name_to_ident),
        Rule::func_refine_type => func_refine_type_from_pair(pairs.next().unwrap(), name_to_ident),
        Rule::left_paren => {
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);
            let (typ, name_env) = refine_type_from_pair(pairs.next().unwrap(), name_to_ident)?;
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
            Ok((typ, name_env))
        }
        _ => unreachable!(),
    }
}

fn base_refine_type_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Type, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::base_refine_type);
    let range = pair_to_range(&pair);
    let mut pairs = pair.into_inner();

    let pair = pairs.next().unwrap();
    if pair.as_rule() == Rule::kw_bool {
        let ident = Ident::fresh();
        return Ok((
            Type::BaseType(
                ident,
                BaseTypeKind::Bool,
                Term::True(Info::dummy()),
                Info::from_range(range),
            ),
            NameEnv::empty(),
        ));
    } else if pair.as_rule() == Rule::kw_int {
        let ident = Ident::fresh();
        return Ok((
            Type::BaseType(
                ident,
                BaseTypeKind::Int,
                Term::True(Info::dummy()),
                Info::from_range(range),
            ),
            NameEnv::empty(),
        ));
    }

    assert_eq!(pair.as_rule(), Rule::left_paren);

    let pair = pairs.next().unwrap();
    let (typ, name_env) = match pair.as_rule() {
        Rule::name => {
            let mut name_to_ident = name_to_ident.clone();
            let mut name_env = NameEnv::empty();
            let (name, _) = name_from_pair(pair);
            let ident = Ident::fresh();
            name_to_ident.insert(name.clone(), ident);
            name_env.insert(ident, name);

            assert_eq!(pairs.next().unwrap().as_rule(), Rule::colon);

            let base_type_kind = match pairs.next().unwrap().as_rule() {
                Rule::kw_int => BaseTypeKind::Int,
                Rule::kw_bool => BaseTypeKind::Bool,
                _ => unreachable!(),
            };

            let term = if pairs.peek().unwrap().as_rule() == Rule::bar {
                assert_eq!(pairs.next().unwrap().as_rule(), Rule::bar);
                let (term, name_env_) = term_from_pair(pairs.next().unwrap(), &name_to_ident)?;
                name_env.append(name_env_);
                term
            } else {
                Term::True(Info::dummy())
            };

            (
                Type::BaseType(ident, base_type_kind, term, Info::from_range(range)),
                name_env,
            )
        }
        Rule::kw_bool => {
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::bar);
            let (term, name_env) = term_from_pair(pairs.next().unwrap(), &name_to_ident)?;
            let ident = Ident::fresh();
            let constraint_term = Term::Bin(
                logic::BinOp::And,
                Box::new(Term::Bin(
                    logic::BinOp::Imply,
                    Box::new(term.clone()),
                    Box::new(Term::Var(ident, Info::dummy())),
                    Info::dummy(),
                )),
                Box::new(Term::Bin(
                    logic::BinOp::Imply,
                    Box::new(Term::Not(Box::new(term), Info::dummy())),
                    Box::new(Term::Not(
                        Box::new(Term::Var(ident, Info::dummy())),
                        Info::dummy(),
                    )),
                    Info::dummy(),
                )),
                Info::dummy(),
            );

            (
                Type::BaseType(
                    ident,
                    BaseTypeKind::Bool,
                    constraint_term,
                    Info::from_range(range),
                ),
                name_env,
            )
        }
        Rule::kw_int => {
            assert_eq!(pairs.next().unwrap().as_rule(), Rule::bar);
            let (term, name_env) = additive_term_from_pair(pairs.next().unwrap(), &name_to_ident)?;
            let ident = Ident::fresh();
            let constraint_term = Term::Bin(
                logic::BinOp::Eq,
                Box::new(Term::Var(ident, Info::dummy())),
                Box::new(term),
                Info::dummy(),
            );
            (
                Type::BaseType(
                    ident,
                    BaseTypeKind::Int,
                    constraint_term,
                    Info::from_range(range),
                ),
                name_env,
            )
        }
        _ => unreachable!(),
    };

    assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);
    Ok((typ, name_env))
}

fn func_refine_type_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Type, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::func_refine_type);
    let mut pairs = pair.into_inner();

    assert_eq!(pairs.next().unwrap().as_rule(), Rule::left_paren);

    let mut name_to_ident = name_to_ident.clone();
    let mut name_env = NameEnv::empty();
    let (func_name, _) = name_from_pair(pairs.next().unwrap());
    let func_ident = Ident::fresh();
    name_to_ident.insert(func_name.clone(), func_ident);
    name_env.insert(func_ident, func_name);

    let mut types = vec![];
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::colon);
    let type_pair = pairs.next().unwrap();
    let type_range = pair_to_range(&type_pair);
    let (typ, name_env_) = refine_type_from_pair(type_pair, &name_to_ident)?;
    name_env.append(name_env_);
    types.push((typ, type_range));

    while pairs.peek().unwrap().as_rule() == Rule::arrow {
        assert_eq!(pairs.next().unwrap().as_rule(), Rule::arrow);
        let type_pair = pairs.next().unwrap();
        let range = pair_to_range(&type_pair);
        let (typ, name_env_) = refine_type_from_pair(type_pair, &name_to_ident)?;
        name_env.append(name_env_);
        types.push((typ, range));
    }
    assert_eq!(pairs.next().unwrap().as_rule(), Rule::right_paren);

    let first_param_ident = types[0].0.ident();

    let (typ, _) = types
        .into_iter()
        .rev()
        .reduce(|(acc, acc_range), (typ, type_range)| {
            let range = Range::merge(acc_range, type_range);
            if typ.ident() == first_param_ident {
                (
                    Type::FuncType(
                        func_ident,
                        Box::new(typ),
                        Box::new(acc),
                        Info::from_range(range),
                    ),
                    range,
                )
            } else {
                (
                    Type::FuncType(
                        Ident::fresh(),
                        Box::new(typ),
                        Box::new(acc),
                        Info::from_range(range),
                    ),
                    range,
                )
            }
        })
        .unwrap();

    Ok((typ, name_env))
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
    let range = pair_to_range(&pair);
    let mut pairs_iter = pair.into_inner();

    let mut name_env = NameEnv::empty();
    let mut name_to_ident = name_to_ident.clone();

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_let);

    let ident_pair = pairs_iter.next().unwrap();
    assert_eq!(ident_pair.as_rule(), Rule::name);
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
        Expr::Let(
            ident,
            Box::new(expr1),
            Box::new(expr2),
            Info::from_range(range),
        ),
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
            (|info| (builtin::BuiltinData::instance().or_ident, info)) as fn(Info) -> (Ident, Info),
        )])),
        |(op_ident, op_info), lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(Box::new(Expr::Var(op_ident, op_info)), lhs, info)),
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
            (|info| (builtin::BuiltinData::instance().and_ident, info))
                as fn(Info) -> (Ident, Info),
        )])),
        |(op_ident, op_info), lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(Box::new(Expr::Var(op_ident, op_info)), lhs, info)),
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

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::additive_expr,
        additive_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (
                Rule::eq,
                (|info| (builtin::BuiltinData::instance().eq_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
            (
                Rule::neq,
                (|info| (builtin::BuiltinData::instance().neq_ident, info)),
            ),
            (
                Rule::lt,
                (|info| (builtin::BuiltinData::instance().lt_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
            (
                Rule::leq,
                (|info| (builtin::BuiltinData::instance().leq_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
            (
                Rule::gt,
                (|info| (builtin::BuiltinData::instance().gt_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
            (
                Rule::geq,
                (|info| (builtin::BuiltinData::instance().geq_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
        ])),
        |(op_ident, op_info), lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(Box::new(Expr::Var(op_ident, op_info)), lhs, info)),
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

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::multive_expr,
        multive_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (
                Rule::plus,
                (|info| (builtin::BuiltinData::instance().add_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
            (
                Rule::minus,
                (|info| (builtin::BuiltinData::instance().sub_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
        ])),
        |(op_ident, op_info), lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(Box::new(Expr::Var(op_ident, op_info)), lhs, info)),
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

    binop(
        pair.into_inner(),
        name_to_ident,
        Rule::apply_expr,
        apply_expr_from_pair,
        HashMap::from_iter(IntoIter::new([
            (
                Rule::ast,
                (|info| (builtin::BuiltinData::instance().mult_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
            (
                Rule::slash,
                (|info| (builtin::BuiltinData::instance().div_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
            (
                Rule::percent,
                (|info| (builtin::BuiltinData::instance().rem_ident, info))
                    as fn(Info) -> (Ident, Info),
            ),
        ])),
        |(op_ident, op_info), lhs, rhs, info| {
            Expr::App(
                Box::new(Expr::App(Box::new(Expr::Var(op_ident, op_info)), lhs, info)),
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
    let start_range = pair_to_range(&pair);
    let mut pairs = pair.into_inner();

    let (mut acc, mut name_env) = primary_expr_from_pair(pairs.next().unwrap(), name_to_ident)?;

    for pair in pairs {
        let end_range = pair_to_range(&pair);
        let (e, name_env_) = primary_expr_from_pair(pair, name_to_ident)?;
        name_env.append(name_env_);
        acc = Expr::App(
            Box::new(acc),
            Box::new(e),
            Info::from_range(Range::merge(start_range, end_range)),
        );
    }
    Ok((acc, name_env))
}

fn primary_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::primary_expr);
    let mut pairs_iter = pair.into_inner();
    let pair = pairs_iter.next().unwrap();
    let info = Info::builtin_info_from_range(pair_to_range(&pair));

    match pair.as_rule() {
        Rule::if_expr => if_expr_from_pair(pair, name_to_ident),
        Rule::number => Ok((Expr::Num(number_from_pair(pair), info), NameEnv::empty())),
        Rule::kw_true => Ok((Expr::Boolean(true, info), NameEnv::empty())),
        Rule::kw_false => Ok((Expr::Boolean(false, info), NameEnv::empty())),
        Rule::variable => variable_from_pair(pair, name_to_ident),
        Rule::paren_expr => paren_expr_from_pair(pair, name_to_ident),
        _ => unreachable!(),
    }
}

fn if_expr_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    assert_eq!(pair.as_rule(), Rule::if_expr);
    let range = pair_to_range(&pair);
    let mut pairs_iter = pair.into_inner();

    let mut name_env = NameEnv::empty();

    assert_eq!(pairs_iter.next().unwrap().as_rule(), Rule::kw_if);

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
        Expr::If(
            Box::new(cond),
            Box::new(expr1),
            Box::new(expr2),
            Info::from_range(range),
        ),
        name_env,
    ))
}

fn number_from_pair(pair: Pair<Rule>) -> i32 {
    pair.as_str().parse().unwrap()
}

fn variable_from_pair(
    pair: Pair<Rule>,
    name_to_ident: &HashMap<String, Ident>,
) -> Result<(Expr, NameEnv), ParseError> {
    use crate::error::Location;
    assert_eq!(pair.as_rule(), Rule::variable);
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    assert_eq!(pair.as_rule(), Rule::name);
    let (name, info) = name_from_pair(pair);
    let ident = *name_to_ident
        .get(&name)
        .ok_or_else(|| ParseError::UnboundVariable {
            location: Location::from_info(info).expect("unreachable"),
        })?;
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
    assert_eq!(pair.as_rule(), Rule::name);
    (
        pair.as_str().to_string(),
        Info::from_range(pair_to_range(&pair)),
    )
}
