use crate::{ast::*, env::NameEnv, error_report_util::write_lines_in_range, smt::error::SmtError};

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
    NotValidConstraint(logic::Term, (Pos, Pos)),
    SmtError(SmtError),
}

impl From<SmtError> for TypeError {
    fn from(item: SmtError) -> Self {
        Self::SmtError(item)
    }
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
            TypeError::NotValidConstraint(term, range) => {
                writeln!(out, "given constraint is not valid")?;
                writeln!(out, "constraint: {}", term.to_readable_string(name_env))?;
                write_lines_in_range(out, src, range)?;
                writeln!(out, "generated from here")
            }
            TypeError::SmtError(_err) => {
                todo!()
            }
        }
    }
}
