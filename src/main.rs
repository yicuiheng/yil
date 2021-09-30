extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod parse;
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

    let program = parse::program(src.as_str()).unwrap_or_else(|e| {
        parse::print_error(e, src.as_str());
        std::process::exit(-1);
    });

    if let Err(e) = typecheck::program(&program) {
        eprintln!("{:?}", e);
        std::process::exit(-1);
    }
}
