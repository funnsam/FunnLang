use std::path::PathBuf;

use crate::buffer::Buffer;
use crate::lexer::lex;
use crate::token::*;

pub fn preprocess(s: &mut Buffer<Token>) -> Buffer<Token> {
    let mut final_buf = s.clone();
    'scan_for_macro: while let Some(a) = s.next() {
        match a.kind {
            TokenKind::Macro => {
                match a.str.to_lowercase().as_str() {
                    "@import" => {
                        let src = std::fs::read_to_string(match s.next().unwrap().kind {
                            TokenKind::Str(v) => {
                                match &v[0..0] {
                                    "*" => {
                                        let mut a = exec_path();
                                        a.push("std");
                                        a.push(v);
                                        format!("{}", a.display())
                                    }
                                    _ => v
                                }
                            },
                            _ => panic!()
                        }).expect("F");
                        let mut tmp = lex(&mut crate::scanner::Scanner::new(src.chars().collect::<Vec<char>>()));
                        let tok = preprocess(&mut tmp);
                        final_buf.data.extend(tok.data);
                    }
                    _ => panic!()
                }
            },
            _ => {
                while let Some(b) = s.next() {
                    if b.kind == TokenKind::LF {
                        continue 'scan_for_macro;
                    }
                }
            }
        }
    }
    final_buf
}

fn exec_path() -> PathBuf {
    let mut dir = std::env::current_exe().unwrap();
    dir.pop();
    dir
}