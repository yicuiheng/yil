extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod env;
mod error_report_util;
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
    let src_lines: Vec<&str> = src.lines().collect();

    let (program, name_env) = parse::program(src.as_str()).unwrap_or_else(|err| {
        err.print(&src_lines).unwrap();
        std::process::exit(-1);
    });
    typecheck::typecheck_program(&program).unwrap_or_else(|err| {
        err.print(&name_env, &src_lines).unwrap();
        std::process::exit(-1);
    });
    println!("well typed!");
}
