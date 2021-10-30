use crate::{ast::*, env::NameEnv, error_report_util::write_lines_in_range};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
pub enum TypeError {
    UnexpectedSimpleType {
        actual: SimpleType,
        expected: SimpleType,
        range: (Pos, Pos),
        msg: &'static str,
    },
    UnmatchSimpleType {
        type1: SimpleType,
        range1: (Pos, Pos),
        type2: SimpleType,
        range2: (Pos, Pos),
        msg: &'static str,
    },
    FunctionExpected {
        actual: SimpleType,
        range: (Pos, Pos),
    },
    NotValidConstraint {
        counter_examples: HashMap<Ident, logic::Term>,
        spec_range: (Pos, Pos),
        impl_range: (Pos, Pos),
    },
}

use std::io::{stderr, BufWriter, Write};
impl TypeError {
    pub fn print(&self, name_env: &NameEnv, src: &Vec<&str>) -> Result<(), std::io::Error> {
        let stderr = stderr();
        let mut out = BufWriter::new(stderr.lock());
        let out = &mut out;
        write!(out, "[type error] ")?;
        match self {
            TypeError::UnexpectedSimpleType {
                actual,
                expected,
                range,
                msg,
            } => {
                writeln!(out, "{}", msg)?;
                write_lines_in_range(out, src, range)?;
                writeln!(out, " expected `{}`, found `{}`", expected, actual)
            }
            TypeError::UnmatchSimpleType {
                type1,
                range1,
                type2,
                range2,
                msg,
            } => {
                writeln!(out, "{}", msg)?;
                write_lines_in_range(out, src, range1)?;
                writeln!(out, " {}", type1)?;
                write_lines_in_range(out, src, range2)?;
                writeln!(out, " {}", type2)
            }
            TypeError::FunctionExpected { actual, range } => {
                writeln!(out, "function expected, but found `{}`", actual)?;
                write_lines_in_range(out, src, range)?;
                writeln!(out, "here")
            }
            TypeError::NotValidConstraint {
                counter_examples,
                spec_range,
                impl_range,
            } => {
                writeln!(out, "given implementation does not satisfy specification")?;
                write_lines_in_range(out, src, impl_range)?;
                writeln!(out, " implementation")?;
                write_lines_in_range(out, src, spec_range)?;
                writeln!(out, " specification")?;
                writeln!(out, "\ncounter example [")?;
                for (ident, term) in counter_examples {
                    if let Some(name) = name_env.try_lookup(*ident) {
                        writeln!(out, "  {} = {},", name, term.to_readable_string(name_env))?;
                    }
                }
                writeln!(out, "]")
            }
        }
    }
}
