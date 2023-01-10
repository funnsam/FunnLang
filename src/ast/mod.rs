pub mod nodes;

use std::os::raw;

use crate::parser::*;
use crate::buffer::*;
use crate::token::{*, TokenKind::*};

use self::nodes::*;

pub fn generate_ast(tok: Buffer<Token>, src: String) -> Parser {
    let mut p = Parser::new(tok);

    p.buf.buf.index -= 1;
    
    while let Some(t) = p.buf.next() {
        match t.kind {
            Keyword => {
                match t.str.to_lowercase().as_str() {
                    "func" => {
                        let name = p.buf.next().unwrap();    // TODO error checking
                        if name.kind != Name {todo!()}
                        if p.buf.next().unwrap().kind != LParenthesis {todo!()}
                        
                        let mut raw_args: Vec<Vec<String>> = Vec::new();
                        let mut tmp = Vec::with_capacity(2);
                        while let Some(t) = p.buf.next() {
                            if t.kind == RParenthesis {
                                if !tmp.is_empty() {raw_args.push(tmp.clone())}
                                break;
                            } else if t.kind == Comma {
                                raw_args.push(tmp.clone());
                                tmp.clear()
                            } else {
                                tmp.push(t.str)
                            }
                        }
                        drop(tmp);

                        let mut func_args = Vec::new();
                        for el in raw_args.into_iter() {
                            func_args.push(FuncDefArg{name: el[0].clone(), typ: Type::Name(el[1].clone())})
                        }

                        let func_body = Program { body: Vec::new(), escaped: false };
                        let func_type = Type::Name(p.buf.next().unwrap().str.clone());
                        
                        p.add_node(Node::FuncDefine {
                            func_name: name.str,
                            func_args,
                            func_type,
                            func_body,
                        });
                        p.buf.advance();
                    },
                    "var" => {
                        let name = p.buf.next().unwrap();
                        let typ  = p.buf.next().unwrap();
                        if name.kind != Name && typ.kind != Name {todo!()}
                        p.buf.advance();
                        let expr = parse_expr_from_parser(&mut p, &vec![SemiColon]);

                        p.add_node(
                            Node::VarDefine {
                                var_type: Type::Name(typ.str),
                                var_name: name.str,
                                val_expr: expr
                            }
                        )
                    },
                    "if" => {
                        p.buf.advance();
                        while let Some(t) = p.buf.next() {
                            if t.kind == RParenthesis {break}
                        }
                        p.buf.advance();
                        p.add_node(
                            Node::Branch {
                                cond: vec![Expr::Number(1)],
                                body: vec![Program { body: Vec::new(), escaped: false }]
                            }
                        )
                    },
                    "else" => {
                        match p.buf.buf.peek().unwrap().str.as_str() {
                            "if" => {
                                p.buf.advance();
                                p.buf.advance();
                                while let Some(t) = p.buf.next() {
                                    if t.kind == RParenthesis {break}
                                }
                                p.buf.advance();
                                match p.find_branch_block().unwrap() {
                                    Node::Branch { cond, body } => {
                                        cond.push(Expr::Number(1));
                                        body.push(Program { body: Vec::new(), escaped: false });
                                        p.buf.advance()
                                    },
                                    _ => todo!(),
                                }
                            },
                            _ => {
                                match p.find_branch_block().unwrap() {
                                    Node::Branch { cond: _, body } => {
                                        body.push(Program { body: Vec::new(), escaped: false });
                                        p.buf.advance()
                                    },
                                    _ => todo!(),
                                }
                            }
                        }
                    }
                    _ => ()
                }
            },
            Name => {
                let name = p.buf.current().unwrap().str;
                match p.buf.next().unwrap().kind {
                    EqualSign => {
                        let expr = parse_expr_from_parser(&mut p, &vec![SemiColon]);
                        p.add_node(
                            Node::VarAssign { var_name: name, val_expr: expr }
                        );
                    },
                    LParenthesis => {
                        let mut raw_args: Vec<Vec<Token>> = vec![Vec::new()];
                        
                        while let Some(a) = p.buf.next() {
                            match a.kind {
                                RParenthesis => break,
                                Comma => raw_args.push(Vec::new()),
                                _ => raw_args.last_mut().unwrap().push(a),
                            }
                        }
                        
                        let mut args: Vec<Expr> = Vec::new();
                        for el in raw_args.into_iter() {
                            args.push(parse_expr(&mut Buffer::new(el)))
                        }

                        p.add_node(Node::FuncCall { func_name: name, func_args: args });
                    },
                    _ => (),
                }
            }
            RCurlyBracket => {
                p.find_scope().escaped = true
            },
            SemiColon => (),
            _ => {
                println!("Debug: Unexpected {:?}.", t)
            },
        }
    }
    p
}

fn parse_expr_from_parser(p: &mut Parser, stop_tokens: &Vec<TokenKind>) -> Expr {
    let mut tmp = Vec::new();
    while let Some(a) = p.buf.next() {
        if stop_tokens.contains(&a.kind) {break;}
        tmp.push(a);
    };
    parse_expr(&mut Buffer::new(tmp))
}

fn parse_expr(toks: &mut Buffer<Token>) -> Expr {
    let expr: Expr = Expr::Number(toks.peek().unwrap().str.parse().unwrap_or(0));
    while let Some(_) = toks.next() {};
    expr
}
