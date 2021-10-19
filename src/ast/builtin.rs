use once_cell::sync::OnceCell;

use crate::{
    ast::{logic::*, *},
    env::{SimpleTypeEnv, TypeEnv},
};

static INSTANCE: OnceCell<BuiltinData> = OnceCell::new();

pub struct BuiltinData {
    pub or_ident: Ident,
    pub and_ident: Ident,
    pub eq_ident: Ident,
    pub neq_ident: Ident,
    pub lt_ident: Ident,
    pub leq_ident: Ident,
    pub gt_ident: Ident,
    pub geq_ident: Ident,
    pub add_ident: Ident,
    pub sub_ident: Ident,
    pub mult_ident: Ident,
    pub div_ident: Ident,
    pub rem_ident: Ident,
}

impl std::iter::IntoIterator for &BuiltinData {
    type Item = Ident;
    type IntoIter = std::vec::IntoIter<Ident>;
    fn into_iter(self) -> Self::IntoIter {
        vec![
            self.or_ident,
            self.and_ident,
            self.eq_ident,
            self.neq_ident,
            self.lt_ident,
            self.leq_ident,
            self.gt_ident,
            self.geq_ident,
            self.add_ident,
            self.sub_ident,
            self.mult_ident,
            self.div_ident,
            self.rem_ident,
        ]
        .into_iter()
    }
}

impl BuiltinData {
    pub fn instance() -> &'static Self {
        INSTANCE.get_or_init(|| BuiltinData {
            or_ident: Ident::fresh(),
            and_ident: Ident::fresh(),
            eq_ident: Ident::fresh(),
            neq_ident: Ident::fresh(),
            lt_ident: Ident::fresh(),
            leq_ident: Ident::fresh(),
            gt_ident: Ident::fresh(),
            geq_ident: Ident::fresh(),
            add_ident: Ident::fresh(),
            sub_ident: Ident::fresh(),
            mult_ident: Ident::fresh(),
            div_ident: Ident::fresh(),
            rem_ident: Ident::fresh(),
        })
    }

    pub fn str_of(ident: Ident) -> Option<&'static str> {
        let inst = Self::instance();
        if inst.or_ident == ident {
            Some("||")
        } else if inst.and_ident == ident {
            Some("&&")
        } else if inst.eq_ident == ident {
            Some("==")
        } else if inst.neq_ident == ident {
            Some("!=")
        } else if inst.lt_ident == ident {
            Some("<")
        } else if inst.leq_ident == ident {
            Some("<=")
        } else if inst.gt_ident == ident {
            Some(">")
        } else if inst.geq_ident == ident {
            Some(">=")
        } else if inst.add_ident == ident {
            Some("+")
        } else if inst.sub_ident == ident {
            Some("-")
        } else if inst.mult_ident == ident {
            Some("*")
        } else if inst.div_ident == ident {
            Some("/")
        } else if inst.rem_ident == ident {
            Some("%")
        } else {
            None
        }
    }

    pub fn simple_type_env() -> SimpleTypeEnv {
        let builtin = BuiltinData::instance();
        let mut simple_type_env = SimpleTypeEnv::empty();
        for ident in builtin.into_iter() {
            simple_type_env.insert(ident, simple_type_of(ident).unwrap());
        }
        simple_type_env
    }

    pub fn type_env() -> TypeEnv {
        let builtin = BuiltinData::instance();
        let mut type_env = TypeEnv::empty();
        for ident in builtin.into_iter() {
            type_env.insert(ident, type_of(ident).unwrap());
        }
        type_env
    }
}

fn simple_type_of(ident: Ident) -> Option<SimpleType> {
    let inst = BuiltinData::instance();
    let binop_simple_type = SimpleType::FuncType(
        Box::new(SimpleType::IntType),
        Box::new(SimpleType::FuncType(
            Box::new(SimpleType::IntType),
            Box::new(SimpleType::IntType),
        )),
    );
    Some(if inst.or_ident == ident {
        binop_simple_type
    } else if inst.and_ident == ident {
        binop_simple_type
    } else if inst.eq_ident == ident {
        binop_simple_type
    } else if inst.neq_ident == ident {
        binop_simple_type
    } else if inst.lt_ident == ident {
        binop_simple_type
    } else if inst.leq_ident == ident {
        binop_simple_type
    } else if inst.gt_ident == ident {
        binop_simple_type
    } else if inst.geq_ident == ident {
        binop_simple_type
    } else if inst.add_ident == ident {
        binop_simple_type
    } else if inst.sub_ident == ident {
        binop_simple_type
    } else if inst.mult_ident == ident {
        binop_simple_type
    } else if inst.div_ident == ident {
        binop_simple_type
    } else if inst.rem_ident == ident {
        binop_simple_type
    } else {
        return None;
    })
}

fn type_of(ident: Ident) -> Option<Type> {
    let inst = BuiltinData::instance();
    let arg1_ident = Ident::fresh();
    let arg2_ident = Ident::fresh();
    let ret_ident = Ident::fresh();

    let make_logical = |op: BinOp| {
        Term::Bin(
            BinOp::And,
            Box::new(Term::Bin(
                BinOp::Imply,
                Box::new(Term::Bin(
                    op,
                    Box::new(Term::Bin(
                        BinOp::Eq,
                        Box::new(Term::Var(arg1_ident, Info::Dummy)),
                        Box::new(Term::Num(0, Info::Dummy)),
                        Info::Dummy,
                    )),
                    Box::new(Term::Bin(
                        BinOp::Eq,
                        Box::new(Term::Var(arg2_ident, Info::Dummy)),
                        Box::new(Term::Num(0, Info::Dummy)),
                        Info::Dummy,
                    )),
                    Info::Dummy,
                )),
                Box::new(Term::Bin(
                    BinOp::Eq,
                    Box::new(Term::Var(ret_ident, Info::Dummy)),
                    Box::new(Term::Num(0, Info::Dummy)),
                    Info::Dummy,
                )),
                Info::Dummy,
            )),
            Box::new(Term::Bin(
                BinOp::Imply,
                Box::new(Term::Not(
                    Box::new(Term::Bin(
                        op,
                        Box::new(Term::Bin(
                            BinOp::Eq,
                            Box::new(Term::Var(arg1_ident, Info::Dummy)),
                            Box::new(Term::Num(0, Info::Dummy)),
                            Info::Dummy,
                        )),
                        Box::new(Term::Bin(
                            BinOp::Eq,
                            Box::new(Term::Var(arg2_ident, Info::Dummy)),
                            Box::new(Term::Num(0, Info::Dummy)),
                            Info::Dummy,
                        )),
                        Info::Dummy,
                    )),
                    Info::Dummy,
                )),
                Box::new(Term::Bin(
                    BinOp::Eq,
                    Box::new(Term::Var(ret_ident, Info::Dummy)),
                    Box::new(Term::Num(1, Info::Dummy)),
                    Info::Dummy,
                )),
                Info::Dummy,
            )),
            Info::Dummy,
        )
    };
    let make_pred = |op: BinOp| {
        Term::Bin(
            BinOp::And,
            Box::new(Term::Bin(
                BinOp::Imply,
                Box::new(Term::Bin(
                    op,
                    Box::new(Term::Var(arg1_ident, Info::Dummy)),
                    Box::new(Term::Var(arg2_ident, Info::Dummy)),
                    Info::Dummy,
                )),
                Box::new(Term::Bin(
                    BinOp::Eq,
                    Box::new(Term::Var(ret_ident, Info::Dummy)),
                    Box::new(Term::Num(0, Info::Dummy)),
                    Info::Dummy,
                )),
                Info::Dummy,
            )),
            Box::new(Term::Bin(
                BinOp::Imply,
                Box::new(Term::Not(
                    Box::new(Term::Bin(
                        op,
                        Box::new(Term::Var(arg1_ident, Info::Dummy)),
                        Box::new(Term::Var(arg2_ident, Info::Dummy)),
                        Info::Dummy,
                    )),
                    Info::Dummy,
                )),
                Box::new(Term::Bin(
                    BinOp::Eq,
                    Box::new(Term::Var(ret_ident, Info::Dummy)),
                    Box::new(Term::Num(1, Info::Dummy)),
                    Info::Dummy,
                )),
                Info::Dummy,
            )),
            Info::Dummy,
        )
    };
    let make_arithmeric = |op: BinOp| -> Term {
        Term::Bin(
            BinOp::Eq,
            Box::new(Term::Var(ret_ident, Info::Dummy)),
            Box::new(Term::Bin(
                op,
                Box::new(Term::Var(arg1_ident, Info::Dummy)),
                Box::new(Term::Var(arg2_ident, Info::Dummy)),
                Info::Dummy,
            )),
            Info::Dummy,
        )
    };

    let ret_term: Term = if inst.or_ident == ident {
        make_logical(BinOp::Or)
    } else if inst.and_ident == ident {
        make_logical(BinOp::And)
    } else if inst.eq_ident == ident {
        make_pred(BinOp::Eq)
    } else if inst.neq_ident == ident {
        make_pred(BinOp::Neq)
    } else if inst.lt_ident == ident {
        make_pred(BinOp::Lt)
    } else if inst.leq_ident == ident {
        make_pred(BinOp::Leq)
    } else if inst.gt_ident == ident {
        make_pred(BinOp::Gt)
    } else if inst.geq_ident == ident {
        make_pred(BinOp::Geq)
    } else if inst.add_ident == ident {
        make_arithmeric(BinOp::Add)
    } else if inst.sub_ident == ident {
        make_arithmeric(BinOp::Sub)
    } else if inst.mult_ident == ident {
        make_arithmeric(BinOp::Mult)
    } else if inst.div_ident == ident {
        make_arithmeric(BinOp::Div)
    } else if inst.rem_ident == ident {
        make_arithmeric(BinOp::Rem)
    } else {
        return None;
    };

    Some(Type::FuncType(
        ident,
        Box::new(Type::IntType(
            arg1_ident,
            Term::True(Info::Dummy),
            Info::Dummy,
        )),
        Box::new(Type::FuncType(
            arg2_ident,
            Box::new(Type::IntType(
                arg2_ident,
                Term::True(Info::Dummy),
                Info::Dummy,
            )),
            Box::new(Type::IntType(ret_ident, ret_term, Info::Dummy)),
            Info::Dummy,
        )),
        Info::Dummy,
    ))
}
