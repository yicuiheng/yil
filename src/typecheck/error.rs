use crate::{ast::*, env::NameEnv};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
pub enum TypeError {
    UnexpectedSimpleType {
        actual: SimpleType,
        expected: SimpleType,
        range: Range,
        msg: &'static str,
    },
    FunctionExpected {
        actual: SimpleType,
        range: Range,
    },
    NotValidConstraint {
        counter_example: HashMap<Ident, logic::Term>,
        spec_range: Range,
        impl_range: Range,
    },
}

use crate::error::{Diagnostic, Error, Kind, Location};
impl Error for TypeError {
    fn to_diagnostic(self, name_env: &NameEnv) -> Diagnostic {
        match self {
            TypeError::UnexpectedSimpleType {
                actual,
                expected,
                msg: inner_msg,
                range,
            } => {
                let msg = format!(
                    "{}. expected `{}`, found `{}`.",
                    inner_msg, expected, actual
                );
                Diagnostic {
                    loc: Location::Range(range),
                    kind: Kind::TypeError,
                    msg,
                }
            }
            TypeError::FunctionExpected { actual, range } => {
                let msg = format!("function expected, but found `{}`", actual);
                Diagnostic {
                    loc: Location::Range(range),
                    kind: Kind::TypeError,
                    msg,
                }
            }
            TypeError::NotValidConstraint {
                counter_example,
                spec_range,
                impl_range,
            } => {
                let mut msg = format!(
                    "this implementation does not meet specification at ({}:{}-{}:{}).\n",
                    spec_range.start.line,
                    spec_range.start.col,
                    spec_range.end.line,
                    spec_range.end.col
                );
                msg += "the specification is not satisfied when\n";
                msg += "[\n";
                for (ident, term) in counter_example {
                    if let Some(name) = name_env.try_lookup(ident) {
                        msg += format!("  {} = {}\n", name, term.to_readable_string(name_env))
                            .as_str();
                    }
                }
                msg += "]";
                Diagnostic {
                    loc: Location::Range(impl_range),
                    kind: Kind::TypeError,
                    msg,
                }
            }
        }
    }

    fn location(&self) -> Location {
        match self {
            TypeError::UnexpectedSimpleType { range, .. } => Location::Range(range.clone()),
            TypeError::FunctionExpected { range, .. } => Location::Range(range.clone()),
            TypeError::NotValidConstraint { impl_range, .. } => Location::Range(impl_range.clone()),
        }
    }
}
