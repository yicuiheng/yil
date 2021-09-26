use super::*;
use itertools::Itertools;
use pest::error;

#[derive(Debug)]
pub enum ParseError {
    Pest(error::Error<Rule>),
}

impl From<error::Error<Rule>> for ParseError {
    fn from(item: error::Error<Rule>) -> Self {
        Self::Pest(item)
    }
}

pub fn print_error(e: ParseError, src: &str) {
    match e {
        ParseError::Pest(error::Error {
            variant: error::ErrorVariant::ParsingError { positives, .. },
            line_col,
            ..
        }) => {
            let (line, col) = match line_col {
                error::LineColLocation::Pos((line, col)) => (line, col),
                error::LineColLocation::Span((line, col), _) => (line, col),
            };
            let mut msg = format!("{}\n", line_of(src, line - 1));
            msg.push_str(&format!(
                "{}^^^\n",
                std::iter::repeat(' ').take(col).collect::<String>()
            ));
            msg.push_str(&format!("unexpected token at line {}\n expeted : ", line));
            for e in positives.iter().with_position() {
                match e {
                    Position::First(p) | Position::Middle(p) => {
                        msg.push_str(rule_to_str(p));
                    }
                    Position::Last(p) => {
                        msg.push_str(rule_to_str(p));
                    }
                    Position::Only(p) => {
                        msg.push_str(rule_to_str(p));
                    }
                }
            }
            eprintln!("{}", msg);
            std::process::exit(-1);
        }
        _ => todo!(),
    }
}
