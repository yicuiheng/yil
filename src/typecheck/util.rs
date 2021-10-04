use crate::ast::*;

pub fn preprocess(e: Expr) -> Expr {
    // 型システムの制約上，内側にある式を let で値にする
    match e {
        Expr::BinApp(op, e1, e2, pos) => {
            let e1 = Box::new(preprocess(*e1));
            let e2 = Box::new(preprocess(*e2));
            let e1_ident = Ident::fresh();
            let e2_ident = Ident::fresh();
            Expr::Let(
                e1_ident.clone(),
                e1,
                Box::new(Expr::Let(
                    e2_ident.clone(),
                    e2,
                    Box::new(Expr::BinApp(
                        op,
                        Box::new(Expr::Var(e1_ident)),
                        Box::new(Expr::Var(e2_ident)),
                        pos,
                    )),
                    PosInfo::dummy(),
                )),
                PosInfo::dummy(),
            )
        }
        Expr::Ifz(cond, e1, e2, pos) => {
            let cond = Box::new(preprocess(*cond));
            let e1 = Box::new(preprocess(*e1));
            let e2 = Box::new(preprocess(*e2));
            let cond_ident = Ident::fresh();
            Expr::Let(
                cond_ident.clone(),
                cond,
                Box::new(Expr::Ifz(Box::new(Expr::Var(cond_ident)), e1, e2, pos)),
                PosInfo::dummy(),
            )
        }
        Expr::Let(ident, e1, e2, pos) => {
            let e1 = Box::new(preprocess(*e1));
            let e2 = Box::new(preprocess(*e2));
            Expr::Let(ident, e1, e2, pos)
        }
        Expr::FuncApp(func_name, args, pos) => {
            let args: Vec<_> = args.into_iter().map(|arg| preprocess(arg)).collect();
            let arg_idents: Vec<_> = args.iter().map(|_| Ident::fresh()).collect();
            let args: Vec<_> = args.into_iter().zip(arg_idents.clone()).collect();

            let func_app = Expr::FuncApp(
                func_name,
                arg_idents.into_iter().map(Expr::Var).collect(),
                pos,
            );
            args.into_iter().fold(func_app, |acc, (arg, arg_ident)| {
                Expr::Let(arg_ident, Box::new(arg), Box::new(acc), PosInfo::dummy())
            })
        }
        e => e,
    }
}
