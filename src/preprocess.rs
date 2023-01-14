use std::path::PathBuf;

use crate::buffer::Buffer;
use crate::lexer::lex;
use crate::token::*;

pub fn preprocess(s: &mut Buffer<Token>) -> Buffer<Token> {
    let mut extra: Buffer<Token> = Buffer::new(Vec::new());
    'scan_for_macro: while let Some(a) = s.next() {
        match a.kind {
            TokenKind::Macro => {
                match a.str.to_lowercase().as_str() {
                    "@import" => {
                        let src = std::fs::read_to_string(match s.next().unwrap().kind {
                            TokenKind::Str(v) => {
                                match &v[0..1] {
                                    "*" => {
                                        let mut a = exec_path();
                                        a.push("std");
                                        a.push(v[1..v.len()].to_owned() + ".funn");
                                        format!("{}", a.display())
                                    }
                                    _ => v
                                }
                            },
                            _ => panic!()
                        }).expect("F");
                        let mut tmp = lex(&mut crate::scanner::Scanner::new(src.chars().collect::<Vec<char>>()));
                        let mut tok = preprocess(&mut tmp);
                        extra.data.append(&mut tok.data);
                        s.data.remove(s.index);
                        s.data.remove(s.index-1);
                        s.index -= 2;
                    },
                    _ => panic!()
                };
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
    extra.data.append(&mut s.data);
    extra
}

fn exec_path() -> PathBuf {
    let mut dir = std::env::current_exe().unwrap();
    dir.pop();
    dir
}