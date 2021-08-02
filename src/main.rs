#[macro_use]
extern crate pest_derive;

mod ast;
mod parse;

fn main() {
    println!("hoge42: {:?}", parse::ident("hoge42"));
    println!("42hoge: {:?}", parse::ident("42hoge"));
    println!("42: {:?}", parse::ident("42"));

    println!("1 + 2 + 3: {:?}", parse::expr("1 + 2 + 3"));
}
