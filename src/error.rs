pub mod util;
use crate::ast::{Info, Pos, Range};
use crate::env::NameEnv;

pub trait Error {
    fn to_diagnostic(self, name_env: &NameEnv) -> Diagnostic;
    fn location(&self) -> Location;
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub loc: Location,
    pub kind: Kind,
    pub msg: String,
}

#[derive(Debug, Clone)]
pub enum Location {
    Range(Range),
    Position(Pos),
}

#[derive(Debug, Clone)]
pub enum Kind {
    ParseError,
    TypeError,
}

use std::fmt;
impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Location::Range(range) => write!(
                f,
                "{}:{}-{}:{}",
                range.start.line, range.start.col, range.end.line, range.end.col
            ),
            Location::Position(pos) => write!(f, "{}:{}", pos.line, pos.col),
        }
    }
}

impl Location {
    pub fn from_info(info: Info) -> Option<Self> {
        info.range.map(|range| Location::Range(range))
    }

    pub fn print(&self, src: &Vec<&str>) -> Result<(), std::io::Error> {
        use crate::error::util::*;
        use std::io::{stderr, BufWriter};
        let stderr = stderr();
        let mut stderr = BufWriter::new(stderr.lock());
        match self {
            Location::Position(pos) => write_line_at_pos(&mut stderr, src, pos),
            Location::Range(range) => write_lines_in_range(&mut stderr, src, range),
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Kind::ParseError => write!(f, "parse error"),
            Kind::TypeError => write!(f, "type error"),
        }
    }
}
