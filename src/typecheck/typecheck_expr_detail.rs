use std::collections::HashMap;

use super::{constraints::add_subtype_constraint, typecheck_expr, TypeError};
use crate::ast::*;

pub fn typecheck_constant(c: &Constant) -> Type {
    let fresh_ident = Ident::fresh();
    Type::NonFuncType(NonFuncType {
        param_name: fresh_ident.clone(),
        base_type: BaseType::Int(PosInfo::dummy()),
        formula: logic::Formula::BinApp(
            logic::BinPred::Eq(PosInfo::dummy()),
            logic::Expr::Var(fresh_ident),
            logic::Expr::Constant(c.clone()),
            PosInfo::dummy(),
        ),
        pos: PosInfo::dummy(),
    })
}

pub fn typecheck_var_expr(
    ident: &Ident,
    type_env: &HashMap<Ident, Type>,
) -> Result<(Type, HashMap<Ident, Type>), TypeError> {
    type_env
        .get(ident)
        .cloned()
        .ok_or_else(|| TypeError::UnboundVariable(ident.clone()))
        .map(|typ| (typ, type_env.clone()))
}

pub fn typecheck_binapp_expr(
    op: &BinOp,
    e1: &Expr,
    e2: &Expr,
    pos: &PosInfo,
    type_env: &HashMap<Ident, Type>,
    constraints: &mut Vec<logic::Formula>,
) -> Result<(Type, HashMap<Ident, Type>), TypeError> {
    let (e1_type, type_env) = typecheck_expr(e1, type_env, constraints)?;
    let (e2_type, type_env) = typecheck_expr(e2, &type_env, constraints)?;
    if let (
        Type::NonFuncType(NonFuncType {
            param_name: param_name1,
            base_type: BaseType::Int(int_pos),
            ..
        }),
        Type::NonFuncType(NonFuncType {
            param_name: param_name2,
            base_type: BaseType::Int(_),
            ..
        }),
    ) = (e1_type.clone(), e2_type.clone())
    {
        let ident = Ident::fresh();
        let op = match op {
            BinOp::Add(_) => logic::BinOp::Add,
            BinOp::Sub(_) => logic::BinOp::Sub,
            BinOp::Mul(_) => logic::BinOp::Mult,
            BinOp::Div(_) => logic::BinOp::Div,
            _ => todo!(),
        };
        let formula = logic::Formula::BinApp(
            logic::BinPred::Eq(PosInfo::dummy()),
            logic::Expr::Var(ident.clone()),
            logic::Expr::BinApp(
                op(PosInfo::dummy()),
                Box::new(logic::Expr::Var(param_name1)),
                Box::new(logic::Expr::Var(param_name2)),
                PosInfo::dummy(),
            ),
            PosInfo::dummy(),
        );
        Ok((
            Type::NonFuncType(NonFuncType {
                param_name: ident,
                base_type: BaseType::Int(int_pos),
                formula,
                pos: PosInfo::dummy(),
            }),
            type_env,
        ))
    } else {
        Err(TypeError::BinAppSimpleTypeError {
            op: op.clone(),
            type1: e1_type,
            type2: e2_type,
            pos: pos.clone(),
        })
    }
}

pub fn typecheck_ifz_expr(
    cond: &Expr,
    e1: &Expr,
    e2: &Expr,
    type_env: &HashMap<Ident, Type>,
    constraints: &mut Vec<logic::Formula>,
) -> Result<(Type, HashMap<Ident, Type>), TypeError> {
    if let (
        Type::NonFuncType(NonFuncType {
            param_name: cond_name,
            base_type: BaseType::Int(_),
            formula,
            ..
        }),
        type_env,
    ) = typecheck_expr(cond, type_env, constraints)?
    {
        let dummy_ident = Ident::fresh();
        let mut true_branch_type_env = type_env;
        true_branch_type_env.insert(
            dummy_ident.clone(),
            Type::NonFuncType(NonFuncType {
                param_name: dummy_ident.clone(),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::And(
                    Box::new(formula.clone()),
                    Box::new(logic::Formula::BinApp(
                        logic::BinPred::Eq(PosInfo::dummy()),
                        logic::Expr::Var(cond_name.clone()),
                        logic::Expr::Constant(Constant::new(0)),
                        PosInfo::dummy(),
                    )),
                    PosInfo::dummy(),
                ),
                pos: PosInfo::dummy(),
            }),
        );
        let (true_branch_type, type_env) = typecheck_expr(e1, &true_branch_type_env, constraints)?;
        let mut false_branch_type_env = type_env;
        false_branch_type_env.insert(
            dummy_ident.clone(),
            Type::NonFuncType(NonFuncType {
                param_name: dummy_ident.clone(),
                base_type: BaseType::Int(PosInfo::dummy()),
                formula: logic::Formula::And(
                    Box::new(formula.clone()),
                    Box::new(logic::Formula::BinApp(
                        logic::BinPred::Neq(PosInfo::dummy()),
                        logic::Expr::Var(cond_name.clone()),
                        logic::Expr::Constant(Constant::new(0)),
                        PosInfo::dummy(),
                    )),
                    PosInfo::dummy(),
                ),
                pos: PosInfo::dummy(),
            }),
        );
        let (false_branch_type, type_env) =
            typecheck_expr(e2, &false_branch_type_env, constraints)?;

        if let (
            Type::NonFuncType(NonFuncType {
                param_name: true_branch_param_name,
                base_type: BaseType::Int(_),
                formula: true_formula,
                ..
            }),
            Type::NonFuncType(NonFuncType {
                param_name: false_branch_param_name,
                base_type: BaseType::Int(_),
                formula: false_formula,
                ..
            }),
        ) = (true_branch_type, false_branch_type)
        {
            // (cond = 0 => true_formula) and (cond != 0 => false_formula)
            let ident = Ident::fresh();
            let true_formula =
                true_formula.subst(&true_branch_param_name, logic::Expr::Var(ident.clone()));
            let false_formula =
                false_formula.subst(&false_branch_param_name, logic::Expr::Var(ident.clone()));
            Ok((
                Type::NonFuncType(NonFuncType {
                    param_name: ident,
                    base_type: BaseType::Int(PosInfo::dummy()),
                    formula: logic::Formula::And(
                        Box::new(logic::Formula::Imply(
                            Box::new(logic::Formula::BinApp(
                                logic::BinPred::Eq(PosInfo::dummy()),
                                logic::Expr::Var(cond_name.clone()),
                                logic::Expr::Constant(Constant::new(0)),
                                PosInfo::dummy(),
                            )),
                            Box::new(true_formula),
                            PosInfo::dummy(),
                        )),
                        Box::new(logic::Formula::Imply(
                            Box::new(logic::Formula::BinApp(
                                logic::BinPred::Neq(PosInfo::dummy()),
                                logic::Expr::Var(cond_name.clone()),
                                logic::Expr::Constant(Constant::new(0)),
                                PosInfo::dummy(),
                            )),
                            Box::new(false_formula),
                            PosInfo::dummy(),
                        )),
                        PosInfo::dummy(),
                    ),
                    pos: PosInfo::dummy(),
                }),
                type_env,
            ))
        } else {
            unimplemented!()
        }
    } else {
        Err(TypeError::IfzCondMustBeInt(cond.clone()))
    }
}

pub fn typecheck_let_expr(
    ident: &Ident,
    e1: &Expr,
    e2: &Expr,
    type_env: &HashMap<Ident, Type>,
    constraints: &mut Vec<logic::Formula>,
) -> Result<(Type, HashMap<Ident, Type>), TypeError> {
    let (e1_type, mut type_env) = typecheck_expr(e1, type_env, constraints)?;
    type_env.insert(ident.clone(), e1_type.clone());
    typecheck_expr(e2, &type_env, constraints)
}

pub fn typecheck_funcapp_expr(
    func_name: &Ident,
    args: &Vec<Expr>,
    pos: &PosInfo,
    type_env: &HashMap<Ident, Type>,
    constraints: &mut Vec<logic::Formula>,
) -> Result<(Type, HashMap<Ident, Type>), TypeError> {
    let (func_type, type_env) =
        typecheck_expr(&Expr::Var(func_name.clone()), type_env, constraints)?;
    if let Type::FuncType(FuncType {
        params,
        ret,
        pos: func_type_pos,
    }) = func_type
    {
        let mut ret = *ret;
        // NOTE: 現状，部分適用は起きないので型が付くなら args.len() と params.len() は等しい
        //       これは関数適用の式の関数側は識別子しか許していないためである
        // TODO: 関数式を識別子以外も許した時に部分適用の型付けに対応する
        if args.len() == params.len() {
            let mut type_env = type_env;
            for i in 0..args.len() {
                let (arg_type, type_env_) = typecheck_expr(&args[i], &type_env, constraints)?;
                type_env = type_env_;
                add_subtype_constraint(&arg_type, &params[i], &type_env, constraints);
                if let (
                    Type::NonFuncType(NonFuncType { param_name, .. }),
                    Type::NonFuncType(NonFuncType {
                        param_name: arg_name,
                        ..
                    }),
                ) = (&params[i], arg_type)
                {
                    ret = ret.subst_logical_expr(param_name, logic::Expr::Var(arg_name));
                } else {
                    // 現状部分適用はないので arg_type が関数型になることはない
                    unimplemented!()
                }
            }
            Ok((ret, type_env))
        } else {
            Err(TypeError::FuncAppError(
                FuncType {
                    params,
                    ret: Box::new(ret),
                    pos: func_type_pos,
                },
                args.clone(),
            ))
        }
    } else {
        Err(TypeError::FuncExpected(func_type.clone(), pos.clone()))
    }
}
