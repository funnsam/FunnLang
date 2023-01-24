mod buffer;

mod token;
mod scanner;

mod errors;

mod lexer;

mod preprocess;

mod parser;
mod ast;

mod compiler;

use std::{process::exit, path::Path};

use crate::compiler::ast_compiler::CodeGen;
use scanner::*;
use lexer::*;
use preprocess::*;
use ast::*;

use clap::Parser;

#[derive(Parser)]
struct Args {
    #[arg(value_name="Input file")]
    input_file: String,

    #[arg(short, long, default_value="auto")]
    target: String,

    #[arg(short, long, default_value="out.o", value_name="Output file")]
    output: String
}

pub enum CompilerTarget {
    RV64,
    AA64,
    X64,
}

impl CompilerTarget {
    pub fn from_string(str: &str) -> Option<Self> {
        use CompilerTarget::*;
        match str.to_ascii_lowercase().as_str() {
            "rv64" | "riscv" | "riscv64"
                => Some(RV64),
            "aa64" | "arm" | "aarch64"
                => Some(AA64),
            "x86" | "x86_64" | "x64"
                => Some(X64),
            "auto"
                => CompilerTarget::from_string(std::env::consts::ARCH),
            _   => None
        }
    }
    pub fn to_str(&self) -> &str {
        use CompilerTarget::*;
        match self {
            RV64 => "rv64",
            AA64 => "aa64",
            X64  => "x64"
        }
    }
}

fn main() {
    let args = Args::parse();
    let target = match CompilerTarget::from_string(&args.target) {
        Some(v) => v,
        None => {
            println!("\x1b[1;31merror:\x1b[0m unsupported target '{}' had been specified\x1b[0m", args.target);
            exit(1)
        }
    };
    
    let src = std::fs::read_to_string(args.input_file.clone()).expect("F");
    
    let mut lex = lex(&mut Scanner::new(src.chars().collect::<Vec<char>>()), 0);
    let mut srcs = vec![src];
    let mut files = vec![args.input_file];
    let tok = preprocess(&mut lex, &mut srcs, &mut files, &mut 1, &target);
    let ast = generate_ast(&tok);
    if ast.err.errors.len() != 0 {
        println!("{}", ast.err.as_string(srcs, files));
        return
    }

    CodeGen::compile(&ast.ast, Path::new(&args.output));
}
pub fn to_mut_ptr<T>(a: &T) -> &mut T {
    unsafe {
        &mut *(a as *const T as *mut T)
    }
}
