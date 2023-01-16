use std::path::PathBuf;

use crate::buffer::Buffer;
use crate::lexer::lex;
use crate::{token::*, CompilerTarget};

pub fn preprocess(s: &mut Buffer<Token>, srcs: &mut Vec<String>, files: &mut Vec<String>, filec: &mut usize, targ: &CompilerTarget) -> Buffer<Token> {
    let mut extra: Buffer<Token> = Buffer::new(Vec::new());
    'scan_for_macro: while let Some(a) = s.next() {
        match a.kind {
            TokenKind::Macro => {
                match a.str.to_lowercase().as_str() {
                    "@import" => {
                        let fname = match s.next().unwrap().kind {
                            TokenKind::Str(v) => {
                                match &v[0..1] {
                                    "*" => {
                                        let mut a = exec_path();
                                        match &v[1..2] {
                                            "*" => {
                                                a.push("std");
                                                a.push("any");
                                                a.push(v[2..v.len()].to_owned() + ".funn");
                                            }
                                            _ => {
                                                a.push("std");
                                                a.push(targ.to_str());
                                                a.push(v[1..v.len()].to_owned() + ".funn");
                                            }
                                        }
                                        
                                        format!("{}", a.display())
                                    }
                                    _ => v
                                }
                            },
                            _ => panic!()
                        };
                        let src = std::fs::read_to_string(fname.clone()).expect("F");
                        let mut tmp = lex(&mut crate::scanner::Scanner::new(src.chars().collect::<Vec<char>>()), *filec);
                        let mut tok = preprocess(&mut tmp, srcs, files, filec, &targ);
                        extra.data.push(Token { kind: TokenKind::LF(0, *filec), str: "\n".to_string() });
                        extra.data.append(&mut tok.data);
                        s.data.remove(s.index);
                        s.data.remove(s.index-1);
                        s.index -= 2;
                        *filec += 1;
                        srcs.push(src);
                        files.push(fname);
                    },
                    _ => panic!()
                };
            },
            _ => {
                while let Some(b) = s.next() {
                    match b.kind {
                        TokenKind::LF(_, _) => continue 'scan_for_macro,
                        _ => ()
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