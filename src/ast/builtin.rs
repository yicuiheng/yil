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
    pub print_bool_ident: Ident,
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
            self.print_bool_ident,
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
            print_bool_ident: Ident::fresh(),
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
        } else if inst.print_bool_ident == ident {
            Some("print_bool")
        } else {
            None
        }
    }

    pub fn simple_type_env() -> SimpleTypeEnv {
        let builtin = BuiltinData::instance();
        let mut simple_type_env = SimpleTypeEnv::empty();
        for ident in builtin.into_iter() {
            simple_type_env.insert(ident, SimpleType::from(type_of(ident).unwrap()));
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

fn type_of(ident: Ident) -> Option<Type> {
    let inst = BuiltinData::instance();

    if inst.print_bool_ident == ident {
        let func_ident = Ident::fresh();
        let arg_ident = Ident::fresh();
        let ret_ident = Ident::fresh();
        return Some(Type::FuncType(
            func_ident,
            Box::new(Type::BaseType(
                arg_ident,
                BaseTypeKind::Bool,
                Term::True(Info::builtin()),
                Info::builtin(),
            )),
            Box::new(Type::BaseType(
                ret_ident,
                BaseTypeKind::Int,
                Term::True(Info::builtin()),
                Info::builtin(),
            )),
            Info::builtin(),
        ));
    }

    let partial_app_func_ident = Ident::fresh();
    let arg1_ident = Ident::fresh();
    let arg2_ident = Ident::fresh();
    let ret_ident = Ident::fresh();

    let make_logical = |op: BinOp| {
        (
            Term::Bin(
                BinOp::And,
                Box::new(Term::Bin(
                    BinOp::Imply,
                    Box::new(Term::Bin(
                        op,
                        Box::new(Term::Var(arg1_ident, Info::builtin())),
                        Box::new(Term::Var(arg2_ident, Info::builtin())),
                        Info::builtin(),
                    )),
                    Box::new(Term::Var(ret_ident, Info::builtin())),
                    Info::builtin(),
                )),
                Box::new(Term::Bin(
                    BinOp::Imply,
                    Box::new(Term::Not(
                        Box::new(Term::Bin(
                            op,
                            Box::new(Term::Var(arg1_ident, Info::builtin())),
                            Box::new(Term::Var(arg2_ident, Info::builtin())),
                            Info::builtin(),
                        )),
                        Info::builtin(),
                    )),
                    Box::new(Term::Not(
                        Box::new(Term::Var(ret_ident, Info::builtin())),
                        Info::builtin(),
                    )),
                    Info::builtin(),
                )),
                Info::builtin(),
            ),
            BaseTypeKind::Bool,
            BaseTypeKind::Bool,
        )
    };
    let make_pred = |op: BinOp| {
        (
            Term::Bin(
                BinOp::And,
                Box::new(Term::Bin(
                    BinOp::Imply,
                    Box::new(Term::Bin(
                        op,
                        Box::new(Term::Var(arg1_ident, Info::builtin())),
                        Box::new(Term::Var(arg2_ident, Info::builtin())),
                        Info::builtin(),
                    )),
                    Box::new(Term::Var(ret_ident, Info::builtin())),
                    Info::builtin(),
                )),
                Box::new(Term::Bin(
                    BinOp::Imply,
                    Box::new(Term::Not(
                        Box::new(Term::Bin(
                            op,
                            Box::new(Term::Var(arg1_ident, Info::builtin())),
                            Box::new(Term::Var(arg2_ident, Info::builtin())),
                            Info::builtin(),
                        )),
                        Info::builtin(),
                    )),
                    Box::new(Term::Not(
                        Box::new(Term::Var(ret_ident, Info::builtin())),
                        Info::builtin(),
                    )),
                    Info::builtin(),
                )),
                Info::builtin(),
            ),
            BaseTypeKind::Int,
            BaseTypeKind::Bool,
        )
    };
    let make_arithmeric = |op: BinOp| {
        (
            Term::Bin(
                BinOp::Eq,
                Box::new(Term::Var(ret_ident, Info::builtin())),
                Box::new(Term::Bin(
                    op,
                    Box::new(Term::Var(arg1_ident, Info::builtin())),
                    Box::new(Term::Var(arg2_ident, Info::builtin())),
                    Info::builtin(),
                )),
                Info::builtin(),
            ),
            BaseTypeKind::Int,
            BaseTypeKind::Int,
        )
    };

    let (ret_term, arg_base_type_kind, ret_base_type_kind) = if inst.or_ident == ident {
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
        Box::new(Type::BaseType(
            arg1_ident,
            arg_base_type_kind,
            Term::True(Info::builtin()),
            Info::builtin(),
        )),
        Box::new(Type::FuncType(
            partial_app_func_ident,
            Box::new(Type::BaseType(
                arg2_ident,
                arg_base_type_kind,
                Term::True(Info::builtin()),
                Info::builtin(),
            )),
            Box::new(Type::BaseType(
                ret_ident,
                ret_base_type_kind,
                ret_term,
                Info::builtin(),
            )),
            Info::builtin(),
        )),
        Info::builtin(),
    ))
}
