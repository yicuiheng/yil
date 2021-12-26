use itertools::Itertools;

use super::*;
use crate::ast;
use crate::error::{Diagnostic, Error, Kind, Location};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        location: Location,
        expected: Vec<Rule>,
    },
    UnboundVariable {
        location: Location,
    },
}

impl Error for ParseError {
    fn to_diagnostic(self, _: &NameEnv) -> Diagnostic {
        match self {
            ParseError::UnexpectedToken { location, expected } => {
                let mut msg = format!("unexpected token, expected ");
                for e in expected.iter().with_position() {
                    match e {
                        Position::First(rule) | Position::Middle(rule) => {
                            msg += format!("{}, ", rule_to_str(rule)).as_str();
                        }
                        Position::Last(rule) | Position::Only(rule) => {
                            msg += rule_to_str(rule);
                        }
                    }
                }
                Diagnostic {
                    loc: location,
                    kind: Kind::ParseError,
                    msg,
                }
            }
            ParseError::UnboundVariable { location } => Diagnostic {
                loc: location,
                kind: Kind::ParseError,
                msg: "unbound variable".to_string(),
            },
        }
    }

    fn location(&self) -> Location {
        match self {
            ParseError::UnexpectedToken { location, .. } => location.clone(),
            ParseError::UnboundVariable { location } => location.clone(),
        }
    }
}

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(item: pest::error::Error<Rule>) -> Self {
        use pest::error::*;
        match item {
            Error {
                variant: ErrorVariant::ParsingError { positives, .. },
                line_col,
                ..
            } => {
                let location = match line_col {
                    LineColLocation::Pos((line, col)) => Location::Position(ast::Pos { line, col }),
                    LineColLocation::Span((start_line, start_col), (end_line, end_col)) => {
                        Location::Range(ast::Range {
                            start: ast::Pos {
                                line: start_line,
                                col: start_col,
                            },
                            end: ast::Pos {
                                line: end_line,
                                col: end_col,
                            },
                        })
                    }
                };
                ParseError::UnexpectedToken {
                    location,
                    expected: positives,
                }
            }
            _ => unreachable!(),
        }
    }
}
