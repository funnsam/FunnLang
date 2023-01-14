mod buffer;

mod token;
mod scanner;

mod errors;

mod lexer;

mod preprocess;

mod parser;
mod ast;

use scanner::*;
use lexer::*;
use preprocess::*;
use ast::*;

fn main() {
    let env = std::env::args().collect::<Vec<String>>();
    let mut path = "source.funn";
    if env.len() >= 2 {
        path = &env[1];
    }

    let src = std::fs::read_to_string(path).expect("F");
    let mut lex = lex(&mut Scanner::new(src.chars().collect::<Vec<char>>()));
    let tok = preprocess(&mut lex);
    let ast = generate_ast(&tok, src.clone());
    if ast.err.errors.len() == 0 {
        println!("{:#?}", ast.ast);
    } else {
        println!("{}", ast.err.as_string(src))
    }
}

pub fn to_mut_ptr<T>(a: &T) -> &mut T {
    unsafe {
        &mut *(a as *const T as *mut T)
    }
}