extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod env;
mod error_report_util;
mod parse;
mod smt;
mod typecheck;

use clap::{App, Arg};

fn main() {
    let matches = App::new("yil")
        .version("1.0")
        .author("yicuiheng <yicuiheng@gmail.com>")
        .about("a language with refinement types")
        .arg(Arg::with_name("filename").required(true).index(1))
        .get_matches();

    let filename = matches.value_of("filename").unwrap_or_else(|| {
        eprintln!("{}", matches.usage());
        std::process::exit(1);
    });

    let src = std::fs::read_to_string(filename).expect("failed to read file..");
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
