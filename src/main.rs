mod buffer;

mod token;
mod scanner;

mod errors;

mod lexer;

mod preprocess;

mod parser;
mod ast;

mod compiler;

use codegem::regalloc::RegAlloc;
use codegem::arch::{urcl::UrclSelector, rv64::RvSelector, x64::X64Selector};
use scanner::*;
use lexer::*;
use preprocess::*;
use ast::*;

use clap::Parser;

#[derive(Parser)]
struct Args {
    #[arg(value_name = "Input file")]
    input_file: String,

    #[arg(short, long, default_value="urcl")]
    target: String
}

fn main() {
    let args = Args::parse();


    let src = std::fs::read_to_string(args.input_file).expect("F");
    let mut lex = lex(&mut Scanner::new(src.chars().collect::<Vec<char>>()));
    let tok = preprocess(&mut lex);
    let ast = generate_ast(&tok, src);
    println!("{:#?}", ast.ast);
    let mut file = std::fs::File::create("out.s").unwrap();

    let ir = compiler::ast_compiler::compiler(ast.ast);
    println!("{}", ir);
    match args.target.to_ascii_lowercase().as_str() {
        "urcl" => {
            let mut vcode = ir.lower_to_vcode::<_, UrclSelector>();
            vcode.allocate_regs::<RegAlloc>();
            vcode.emit_assembly(&mut file);
        }
        "rv64" | "riscv" => {
            let mut vcode = ir.lower_to_vcode::<_, RvSelector>();
            vcode.allocate_regs::<RegAlloc>();
            vcode.emit_assembly(&mut file);
        }
        "x86" | "x86_64" | "x64" => {
            let mut vcode = ir.lower_to_vcode::<_, X64Selector>();
            vcode.allocate_regs::<RegAlloc>();
            vcode.emit_assembly(&mut file);
        }
        _ => panic!("Unsupported arch.")
    }
}

pub fn to_mut_ptr<T>(a: &T) -> &mut T {
    unsafe {
        &mut *(a as *const T as *mut T)
    }
}