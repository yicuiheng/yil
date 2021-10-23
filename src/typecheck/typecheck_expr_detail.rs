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

        make_ifz_type(cond_ident, true_branch_type, false_branch_type, type_env)
    } else {
        unreachable!()
    }
}

fn make_ifz_type(
    cond_ident: Ident,
    mut type1: Type,
    mut type2: Type,
    type_env: TypeEnv,
) -> Result<(Type, TypeEnv), TypeError> {
    let mut param_types = vec![];

    loop {
        match (type1, type2) {
            (
                Type::FuncType(ident1, type11, type12, _),
                Type::FuncType(ident2, type21, type22, _),
            ) => {
                let ident = Ident::fresh();
                let ident_var = Term::Var(ident, Info::Dummy);
                let type11 = type11.subst(ident1, &ident_var);
                let mut type12 = type12.subst(ident1, &ident_var);
                let type21 = type21.subst(ident2, &ident_var);
                let mut type22 = type22.subst(ident2, &ident_var);

                let (param_type, subst) = make_and_type(type11, type21);
                param_types.push((ident, param_type));
                for (from_ident, to_ident) in subst {
                    let to_ident_var = Term::Var(to_ident, Info::Dummy);
                    type12 = type12.subst(from_ident, &to_ident_var);
                    type22 = type22.subst(from_ident, &to_ident_var);
                }
                type1 = type12;
                type2 = type22;
            }
            (Type::IntType(ident1, term1, _), Type::IntType(ident2, term2, _)) => {
                // (cond = 0 and term1) or (cond != 0 and term2)
                let ident = Ident::fresh();
                let ident_var = Term::Var(ident, Info::Dummy);
                let term1 = term1.subst(ident1, &ident_var);
                let term2 = term2.subst(ident2, &ident_var);
                let typ = Type::IntType(
                    ident,
                    Term::Bin(
                        BinOp::Or,
                        Box::new(Term::Bin(
                            BinOp::And,
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
                            BinOp::And,
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
                );
                let typ = param_types
                    .into_iter()
                    .rev()
                    .fold(typ, |acc, (ident, typ)| {
                        Type::FuncType(ident, Box::new(typ), Box::new(acc), Info::Dummy)
                    });
                return Ok((typ, type_env));
            }
            _ => unreachable!(),
        }
    }
}

fn make_and_type(type1: Type, type2: Type) -> (Type, Vec<(Ident, Ident)>) {
    match (type1, type2) {
        (Type::IntType(ident1, term1, _), Type::IntType(ident2, term2, _)) => {
            let mut subst = vec![];
            let ident = Ident::fresh();
            subst.push((ident1, ident));
            subst.push((ident2, ident));
            let ident_var = Term::Var(ident, Info::Dummy);
            let term1 = term1.subst(ident1, &ident_var);
            let term2 = term2.subst(ident2, &ident_var);
            (
                Type::IntType(
                    ident,
                    Term::Bin(BinOp::And, Box::new(term1), Box::new(term2), Info::Dummy),
                    Info::Dummy,
                ),
                subst,
            )
        }
        (Type::FuncType(ident1, type11, type12, _), Type::FuncType(ident2, type21, type22, _)) => {
            let mut subst = vec![];
            let ident = Ident::fresh();
            subst.push((ident1, ident));
            subst.push((ident2, ident));
            let ident_var = Term::Var(ident, Info::Dummy);
            let type11 = type11.subst(ident1, &ident_var);
            let type12 = type12.subst(ident1, &ident_var);
            let type21 = type21.subst(ident2, &ident_var);
            let type22 = type22.subst(ident2, &ident_var);
            let (type1, mut subst_) = make_or_type(type11, type21);
            subst.append(&mut subst_);
            let (type2, mut subst_) = make_and_type(type12, type22);
            subst.append(&mut subst_);
            (
                Type::FuncType(ident, Box::new(type1), Box::new(type2), Info::Dummy),
                subst,
            )
        }
        _ => unreachable!(),
    }
}

fn make_or_type(type1: Type, type2: Type) -> (Type, Vec<(Ident, Ident)>) {
    match (type1, type2) {
        (Type::IntType(ident1, term1, _), Type::IntType(ident2, term2, _)) => {
            let mut subst = vec![];
            let ident = Ident::fresh();
            subst.push((ident1, ident));
            subst.push((ident2, ident));
            let ident_var = Term::Var(ident, Info::Dummy);
            let term1 = term1.subst(ident1, &ident_var);
            let term2 = term2.subst(ident2, &ident_var);
            (
                Type::IntType(
                    ident,
                    Term::Bin(BinOp::Or, Box::new(term1), Box::new(term2), Info::Dummy),
                    Info::Dummy,
                ),
                subst,
            )
        }
        (Type::FuncType(ident1, type11, type12, _), Type::FuncType(ident2, type21, type22, _)) => {
            let mut subst = vec![];
            let ident = Ident::fresh();
            subst.push((ident1, ident));
            subst.push((ident2, ident));
            let ident_var = Term::Var(ident, Info::Dummy);
            let type11 = type11.subst(ident1, &ident_var);
            let type12 = type12.subst(ident1, &ident_var);
            let type21 = type21.subst(ident2, &ident_var);
            let type22 = type22.subst(ident2, &ident_var);
            let (type1, mut subst_) = make_and_type(type11, type21);
            subst.append(&mut subst_);
            let (type2, mut subst_) = make_or_type(type12, type22);
            subst.append(&mut subst_);
            (
                Type::FuncType(ident, Box::new(type1), Box::new(type2), Info::Dummy),
                subst,
            )
        }
        _ => unreachable!(),
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
