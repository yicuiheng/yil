use crate::{
    ast::{logic::*, Expr, Ident, Info, Pos, Type},
    env::TypeEnv,
    typecheck::{constraints::add_subtype_constraint, typecheck_expr, TypeError},
};

pub fn typecheck_number(n: i32, type_env: &TypeEnv) -> Result<(Type, TypeEnv), TypeError> {
    let ident = Ident::fresh();
    let typ = Type::IntType(
        ident,
        Term::Bin(
            BinOp::Eq,
            Box::new(Term::Var(ident, Info::Dummy)),
            Box::new(Term::Num(n, Info::Dummy)),
            Info::Dummy,
        ),
        Info::Dummy,
    );
    let mut type_env = type_env.clone();
    type_env.insert(ident, typ.clone());
    Ok((typ, type_env))
}

pub fn typecheck_var_expr(ident: &Ident, type_env: &TypeEnv) -> Result<(Type, TypeEnv), TypeError> {
    Ok((type_env.lookup(*ident).clone(), type_env.clone()))
}

pub fn typecheck_ifz_expr(
    cond: &Expr,
    e1: &Expr,
    e2: &Expr,
    type_env: &TypeEnv,
    constraints: &mut Vec<(Term, (Pos, Pos))>,
) -> Result<(Type, TypeEnv), TypeError> {
    if let (Type::IntType(cond_ident, cond_term, _), type_env) =
        typecheck_expr(cond, type_env, constraints)?
    {
        let dummy_ident = Ident::fresh();
        let mut true_branch_type_env = type_env;
        true_branch_type_env.insert(
            dummy_ident,
            Type::IntType(
                dummy_ident,
                Term::Bin(
                    BinOp::And,
                    Box::new(cond_term.clone()),
                    Box::new(Term::Bin(
                        BinOp::Eq,
                        Box::new(Term::Var(cond_ident, Info::Dummy)),
                        Box::new(Term::Num(0, Info::Dummy)),
                        Info::Dummy,
                    )),
                    Info::Dummy,
                ),
                Info::Dummy,
            ),
        );
        let (true_branch_type, type_env) = typecheck_expr(e1, &true_branch_type_env, constraints)?;

        let mut false_branch_type_env = type_env;
        false_branch_type_env.insert(
            dummy_ident,
            Type::IntType(
                dummy_ident,
                Term::Bin(
                    BinOp::And,
                    Box::new(cond_term),
                    Box::new(Term::Bin(
                        BinOp::Neq,
                        Box::new(Term::Var(cond_ident, Info::Dummy)),
                        Box::new(Term::Num(0, Info::Dummy)),
                        Info::Dummy,
                    )),
                    Info::Dummy,
                ),
                Info::Dummy,
            ),
        );
        let (false_branch_type, type_env) =
            typecheck_expr(e2, &false_branch_type_env, constraints)?;

        match (true_branch_type, false_branch_type) {
            (Type::IntType(ident1, term1, _), Type::IntType(ident2, term2, _)) => {
                // (cond = 0 => true_formula) and (cond != 0 => false_formula)
                let ident = Ident::fresh();
                let term1 = term1.subst(ident1, &Term::Var(ident, Info::Dummy));
                let term2 = term2.subst(ident2, &Term::Var(ident, Info::Dummy));
                Ok((
                    Type::IntType(
                        ident,
                        Term::Bin(
                            BinOp::And,
                            Box::new(Term::Bin(
                                BinOp::Imply,
                                Box::new(Term::Bin(
                                    BinOp::Eq,
                                    Box::new(Term::Var(cond_ident, Info::Dummy)),
                                    Box::new(Term::Num(0, Info::Dummy)),
                                    Info::Dummy,
                                )),
                                Box::new(term1),
                                Info::Dummy,
                            )),
                            Box::new(Term::Bin(
                                BinOp::Imply,
                                Box::new(Term::Bin(
                                    BinOp::Neq,
                                    Box::new(Term::Var(cond_ident, Info::Dummy)),
                                    Box::new(Term::Num(0, Info::Dummy)),
                                    Info::Dummy,
                                )),
                                Box::new(term2),
                                Info::Dummy,
                            )),
                            Info::Dummy,
                        ),
                        Info::Dummy,
                    ),
                    type_env,
                ))
            }
            (
                Type::FuncType(_param_ident1, _type11, _type12, _),
                Type::FuncType(_param_ident2, _type21, _type22, _),
            ) => {
                todo!()
            }
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

pub fn typecheck_let_expr(
    ident: &Ident,
    e1: &Expr,
    e2: &Expr,
    type_env: &TypeEnv,
    constraints: &mut Vec<(Term, (Pos, Pos))>,
) -> Result<(Type, TypeEnv), TypeError> {
    let (e1_type, mut type_env) = typecheck_expr(e1, type_env, constraints)?;
    type_env.insert(*ident, e1_type.clone());
    let (e2_type, type_env) = typecheck_expr(e2, &type_env, constraints)?;
    Ok((
        e2_type.subst(*ident, &Term::Var(e1_type.ident(), Info::Dummy)),
        type_env,
    ))
}

pub fn typecheck_app_expr(
    func: &Expr,
    arg: &Expr,
    _info: &Info,
    type_env: &TypeEnv,
    constraints: &mut Vec<(Term, (Pos, Pos))>,
) -> Result<(Type, TypeEnv), TypeError> {
    use crate::ast::Node;
    let (start, _) = func.info().as_range();
    let (_, end) = arg.info().as_range();

    let (func_type, type_env) = typecheck_expr(func, type_env, constraints)?;
    let (arg_type, mut type_env) = typecheck_expr(arg, &type_env, constraints)?;
    match func_type {
        Type::FuncType(_, from_type, to_type, _) => {
            add_subtype_constraint(&arg_type, &from_type, &type_env, constraints, (start, end));
            let arg_ident = arg_type.ident();
            type_env.insert(arg_ident, arg_type);
            let ret_type = to_type.subst(from_type.ident(), &Term::Var(arg_ident, Info::Dummy));
            Ok((ret_type, type_env))
        }
        _ => unreachable!(),
    }
}
