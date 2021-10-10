use once_cell::sync::OnceCell;

use super::*;

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

    pub fn type_of(&self, ident: Ident) -> &Type {
        todo!()
    }
}
