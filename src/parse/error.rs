use itertools::Itertools;
use pest::error;

use super::*;
use crate::error_report_util::{write_line_at_pos, write_lines_in_range};

#[derive(Debug)]
pub enum ParseError {
    Pest(error::Error<Rule>),
    UnboundVariable(String, Info),
}

impl From<error::Error<Rule>> for ParseError {
    fn from(item: error::Error<Rule>) -> Self {
        Self::Pest(item)
    }
}

use std::io::{stderr, BufWriter, Write};
impl ParseError {
    pub fn print(&self, src: &Vec<&str>) -> Result<(), std::io::Error> {
        let stderr = stderr();
        let mut out = BufWriter::new(stderr.lock());
        let out = &mut out;
        write!(out, "[parse error] ")?;
        match self {
            ParseError::Pest(error::Error {
                variant: error::ErrorVariant::ParsingError { positives, .. },
                line_col,
                ..
            }) => {
                let (line, col) = match line_col {
                    error::LineColLocation::Pos((line, col)) => (line, col),
                    error::LineColLocation::Span((line, col), _) => (line, col),
                };

                writeln!(out, "unexpected token at ({}:{})", line, col)?;
                write!(out, " expected ")?;
                for e in positives.iter().with_position() {
                    match e {
                        Position::First(rule) | Position::Middle(rule) => {
                            write!(out, "{}, ", rule_to_str(rule))?;
                        }
                        Position::Last(rule) => {
                            write!(out, "{}\n", rule_to_str(rule))?;
                        }
                        Position::Only(rule) => {
                            write!(out, "{}\n", rule_to_str(rule))?;
                        }
                    }
                }
                write_line_at_pos(
                    out,
                    src,
                    &Pos {
                        line: *line,
                        col: *col,
                    },
                )
            }
            ParseError::UnboundVariable(name, info) => {
                let range = info.as_range();
                writeln!(
                    out,
                    "unbound variable `{}` at ({}:{})",
                    name, range.0.line, range.0.col
                )?;
                write_lines_in_range(out, src, &range)?;
                writeln!(out, "")
            }
            _ => unreachable!(),
        }
    }
}
