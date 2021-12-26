extern crate pest;
#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate log;

mod ast;
mod codegen;
mod env;
mod error;
mod lsp;
mod parse;
mod smt;
mod typecheck;

use clap::{App, Arg, SubCommand};

fn main() {
    let matches = App::new("yil")
        .version("1.0")
        .author("yicuiheng <yicuiheng@gmail.com>")
        .about("a language with refinement types")
        .subcommand(
            SubCommand::with_name("exec")
                .about("compile and execute a given yil file")
                .arg(Arg::with_name("filename").required(true).index(1)),
        )
        .subcommand(
            SubCommand::with_name("lsp")
                .about("run as language server")
                .arg(Arg::with_name("project-dir")),
        )
        .get_matches();

    if let Some(_) = matches.subcommand_matches("lsp") {
        use simplelog::*;
        CombinedLogger::init(vec![TermLogger::new(
            LevelFilter::Info,
            Config::default(),
            TerminalMode::Stderr,
            ColorChoice::Never,
        )])
        .unwrap();
        lsp::run();
    } else if let Some(matches) = matches.subcommand_matches("exec") {
        let filename = matches.value_of("filename").unwrap_or_else(|| {
            eprintln!("{}", matches.usage());
            std::process::exit(1);
        });

        let src = std::fs::read_to_string(filename).expect("failed to read file..");
        let src_lines: Vec<&str> = src.lines().collect();

        use crate::env::NameEnv;
        use crate::error::Error;
        fn report_err<E: Error>(err: E, name_env: &NameEnv, src_lines: &Vec<&str>) -> ! {
            let diag = err.to_diagnostic(name_env);
            eprintln!("[{} at {}] {}", diag.kind, diag.loc, diag.msg);
            diag.loc
                .print(&src_lines)
                .expect("failed to write error message to stderr..");
            eprintln!("");
            std::process::exit(-1);
        }

        let (program, name_env) = parse::program(src.as_str())
            .unwrap_or_else(|err| report_err(err, &NameEnv::empty(), &src_lines));
        typecheck::typecheck_program(&program)
            .unwrap_or_else(|err| report_err(err, &name_env, &src_lines));
        std::process::exit(codegen::program(program, &name_env));
    }
}
