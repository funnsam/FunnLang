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
    let ast = generate_ast(tok, src);
    println!("{:#?}", ast.ast);
}

pub fn fuck_mut<T>(a: &T) -> &mut T {
    unsafe {
        &mut *(a as *const T as *mut T)
    }
}