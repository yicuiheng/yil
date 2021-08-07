#[macro_use]
extern crate pest_derive;

mod ast;
mod parse;

use clap::{AppSettings, Clap};

#[derive(Clap)]
#[clap(version = "1.0", author = "yicuiheng <yicuiheng@gmail.com>")]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    filename: String,
}

fn main() {
    let opts = Opts::parse();

    let src = std::fs::read_to_string(opts.filename).expect("failed to read file..");

    let parsed_func = parse::func(src.as_str()).unwrap_or_else(|e| {
        use parse::ParseError::*;
        match e {
            UnexpectedToken {
                expected,
                unexpected,
                pos,
            } => {
                eprintln!("positive: {:?}", expected);
                eprintln!("negative: {:?}", unexpected);
                eprintln!("pos: {:?}", pos);
                std::process::exit(-1);
            }
            _ => unreachable!(),
        }
    });

    println!("parsed func: {:?}", parsed_func);
}
