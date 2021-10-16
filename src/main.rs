extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod env;
mod parse;
mod smt;
mod typecheck;

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

    let (program, _) = parse::program(src.as_str()).unwrap_or_else(|e| {
        parse::error::print_error(e, src.as_str());
        std::process::exit(-1);
    });
    typecheck::program(&program).unwrap_or_else(|e| {
        typecheck::error::print_error(e, src.as_str());
        std::process::exit(-1);
    });
}
