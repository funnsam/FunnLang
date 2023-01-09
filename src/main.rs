mod buffer;

mod token;
mod scanner;
mod lexer;

mod parser;
mod ast;

use lexer::*;
use scanner::*;
use ast::*;

fn main() {
    let src = std::fs::read_to_string("source.funn").expect("F");
    let tok = lex(&mut Scanner::new(src.chars().collect::<Vec<char>>()));
    println!("{:#?}", tok);
    let ast = generate_ast(tok, src);
    println!("{:#?}", ast.ast);
}
