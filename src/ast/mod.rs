pub mod nodes;

use core::panic;
use std::vec;

use crate::parser::*;
use crate::buffer::*;
use crate::token::{*, TokenKind::*};

use self::nodes::*;

pub fn generate_ast(tok: &Buffer<Token>, _src: String) -> Parser {
    let mut p = Parser::new(tok);
    
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
                        let func_type = parse_type_from_parser(&mut p, &vec![RCurlyBracket]);
                        
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
                        if name.kind != Name {todo!()}

                        let typ  = parse_type_from_parser(&mut p, &vec![EqualSign, SemiColon]);

                        let expr = parse_expr_from_parser(&mut p, &vec![SemiColon]);

                        p.add_node(
                            Node::VarDefine {
                                var_type: typ,
                                var_name: name.str,
                                val_expr: expr
                            }
                        )
                    },
                    "while" => {
                        p.buf.advance();
                        while let Some(t) = p.buf.next() {
                            if t.kind == RParenthesis {break}
                        }
                        p.buf.advance();
                        p.add_node(
                            Node::While {
                                cond: Expr::Number(1),
                                body: Program { body: Vec::new(), escaped: false }
                            }
                        )
                    },
                    "for" => {
                        p.buf.advance();
                        let lv = p.buf.next().unwrap().str;
                        p.buf.advance();
                        let from = parse_expr_from_parser(&mut p, &vec![To]);
                        let to = parse_expr_from_parser(&mut p, &vec![RParenthesis]);
                        p.buf.advance();
                        p.add_node(
                            Node::For {
                                loopv: lv,
                                from,
                                to,
                                body : Program { body: Vec::new(), escaped: false }
                            }
                        )
                    },
                    "if" => {
                        p.buf.advance();
                        let expr = parse_expr_from_parser(&mut p, &vec![RParenthesis]);
                        p.add_node(
                            Node::Branch {
                                cond: vec![expr],
                                body: vec![Program { body: Vec::new(), escaped: false }]
                            }
                        )
                    },
                    "else" => {
                        p.buf.advance();
                        match p.buf.current().unwrap().str.as_str() {
                            "if" => {
                                p.buf.advance();
                                let expr = parse_expr_from_parser(&mut p, &vec![RParenthesis]);
                                match p.find_branch_block() {
                                    Node::Branch { cond, body } => {
                                        cond.push(expr);
                                        body.push(Program { body: Vec::new(), escaped: false });
                                    },
                                    _ => todo!(),
                                }
                            },
                            _ => {
                                match p.find_branch_block() {
                                    Node::Branch { cond: _, body } => {
                                        body.push(Program { body: Vec::new(), escaped: false });
                                    },
                                    _ => todo!(),
                                }
                            }
                        }
                    },
                    "asm" => {
                        let asm_blk = match p.buf.next().unwrap().kind {
                            Str(s) => s,
                            _ => panic!()
                        };
                        p.buf.advance();
                        p.add_node(Node::AsmBlock(
                            asm_blk
                        ))
                    },
                    "return" => {
                        let expr = parse_expr_from_parser(&mut p, &vec![SemiColon]);
                        p.add_node(
                            Node::Return(expr)
                        )
                    },
                    _ => todo!()
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
                            if !el.is_empty() {
                                args.push(parse_expr(&mut Buffer::new(el)))
                            }
                        }

                        p.buf.advance();
                        p.add_node(Node::FuncCall { func_name: name, func_args: args });
                    },
                    _ => (),
                }
            },
            Macro => (),
            Str(_) => (),
            LCurlyBracket => {
                p.add_node(Node::CodeBlock(Program { body: Vec::new(), escaped: false }))
            }
            RCurlyBracket => {
                p.find_scope().escaped = true
            },
            _ => {
                println!("Debug: Unexpected {:?}.", t)
            },
        }
    }
    p
}

fn parse_type_from_parser(p: &mut Parser, stop_tokens: &Vec<TokenKind>) -> Type {
    let mut tmp = Vec::new();
    while let Some(a) = p.buf.next() {
        if stop_tokens.contains(&a.kind) { break }
        tmp.push(a);
    };
    parse_type(&mut Buffer::new(tmp))
}

fn parse_type(toks: &mut Buffer<Token>) -> Type {
    toks.data.reverse();

    let mut tmp: Type = Type::Name(match toks.data[0].kind {
        Name => toks.data[0].str.clone(),
        _ => panic!()
    });
    while let Some(tok) = toks.next() {
        match tok.kind {
            Ampersand     => tmp = Type::Pointer(Box::new(tmp)),
            RBracket    => {
                let size = match toks.next().unwrap().kind {
                    Number(v) => v,
                    _ => panic!()
                };
                toks.next();
                tmp = Type::Array(Box::new(tmp), size as usize)
            },
            Name => {
                if toks.index != 0 {
                    panic!()
                }
            },
            _ => panic!("Bruh {:?}", tok)
        }
    }
    tmp
}

fn parse_expr_from_parser(p: &mut Parser, stop_tokens: &Vec<TokenKind>) -> Expr {
    let mut tmp = Vec::new();
    let mut lvl = 0;
    while let Some(a) = p.buf.next() {
        if stop_tokens.contains(&a.kind) {
            match a.kind {
                RParenthesis => if lvl == 0 {break},
                _ => break
            };
        }
        
        match &a.kind {
            LParenthesis => lvl += 1, RParenthesis => lvl -= 1,
            _ => ()
        }

        tmp.push(a);
    };
    parse_expr(&mut Buffer::new(tmp))
}

fn parse_expr(toks: &mut Buffer<Token>) -> Expr {
    #[derive(Debug, Clone, PartialEq)]
    enum ExprTmp {
        Number(i64),
        Ident(String),
        BoolOp(BoolOp),
        UnaryOp(UnaryOp),
        CompOp(CompOp),
        Expr(Expr),
        LParenthesis,
    }
    fn get_precedence(a: &ExprTmp) -> u8 {
        match a {
            ExprTmp::UnaryOp(_) => 5,
            ExprTmp::BoolOp(op) => {
                use BoolOp::*;
                match op {
                    Add | Sub => 1,
                    Mul | Div | Mod => 2,
                    And | Or  | XOr => 3,
                }
            },
            ExprTmp::CompOp(_)  => 4,
            _ => 0,
        }
    }
    fn as_expr_tmp(el: Token, is_unary: bool, iter: &mut Buffer<Token>) -> ExprTmp {
        match el.kind {
            Number(v) => ExprTmp::Number(v),
            Name => {
                match iter.peek().unwrap_or(Token { kind: LF, str: "\n".to_string() }).kind {
                    LParenthesis => {
                        let name = iter.current().unwrap();
                        iter.next();

                        let mut raw_args: Vec<Vec<Token>> = vec![Vec::new()];
                        while let Some(a) = iter.next() {
                            match a.kind {
                                RParenthesis => break,
                                Comma => raw_args.push(Vec::new()),
                                _ => raw_args.last_mut().unwrap().push(a),
                            }
                        }
                        let mut args: Vec<Expr> = Vec::new();
                        for el in raw_args.into_iter() {
                            if !el.is_empty() {
                                args.push(parse_expr(&mut Buffer::new(el)))
                            }
                        }

                        ExprTmp::Expr(Expr::Function { name: name.str, args })
                    },
                    _ => ExprTmp::Ident(el.str)
                }
            },
            MathSymbol | Star | Ampersand => {
                if !is_unary {
                    ExprTmp::BoolOp(
                        match el.str.as_str() {
                            "+" => BoolOp::Add, "-" => BoolOp::Sub, "*" => BoolOp::Mul, "/" => BoolOp::Div,
                            "%" => BoolOp::Mod,
                            "&" => BoolOp::And, "|" => BoolOp::Or , "^" => BoolOp::XOr,
                            _ => panic!("Bruh {:?}", el)
                        }
                    )
                } else {
                    ExprTmp::UnaryOp(
                        match el.str.chars().next().unwrap() {
                            '+' => UnaryOp::Abs, '-' => UnaryOp::Neg, '!' => UnaryOp::Not,
                            '&' => UnaryOp::Ptr, '*' => UnaryOp::Drf,
                            _ => panic!("Bruh {:?}", el)
                        }
                    )
                }
            },
            Logic => {
                use CompOp::*;
                ExprTmp::CompOp(match el.str.as_str() {
                    "==" => EQ, "!=" => NEQ,
                    ">"  => LT, ">=" => LTE,
                    "<"  => GT, "<=" => GTE,
                    _ => panic!()
                })
            },
            _ => panic!()
        }
    }
    fn is_math(a: TokenKind) -> bool {
        match a {
            MathSymbol | Star | Ampersand | LParenthesis | Logic => true,
            _ => false
        }
    }

    let mut output: Vec<ExprTmp> = Vec::new();
    let mut op_stk: Vec<ExprTmp> = Vec::new();
    let mut prev = None;
    
    while let Some(a) = toks.next() {
        let _a = a.kind.clone();
        match a.kind {
            Number(v) => output.push(ExprTmp::Number(v)),
            Name => output.push(as_expr_tmp(a, false, toks)),
            MathSymbol | Star | Ampersand | Logic => {
                let is_unary = is_math(prev.unwrap_or(MathSymbol));
                while let Some(b) = op_stk.pop() {
                    if b != ExprTmp::LParenthesis && get_precedence(&b) >= get_precedence(&as_expr_tmp(a.clone(), is_unary, toks)) {
                        output.push(b);
                    } else {
                        op_stk.push(b);
                        break
                    }
                };
                op_stk.push(as_expr_tmp(a, is_unary, toks));
            },
            LParenthesis => op_stk.push(ExprTmp::LParenthesis),
            RParenthesis => {
                while let Some(v) = op_stk.pop() {
                    if v == ExprTmp::LParenthesis { break }
                    output.push(v)
                }
            },
            _ => panic!("Bruh {:?}", a),
        }
        prev = Some(_a);
    }

    output.append(&mut op_stk);


    let mut tmp1 = Vec::new();
    let iter = output.into_iter();
    for el in iter {
        match el {
            ExprTmp::Number(_) | ExprTmp::Ident(_) | ExprTmp::Expr(_) => tmp1.push(el),
            ExprTmp::BoolOp(op) => {
                let last_2 = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => panic!()
                };
                let last_1 = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => panic!()
                };
                tmp1.push(ExprTmp::Expr(Expr::BoolOp { left: Box::new(last_1), oper: op, right: Box::new(last_2) }))
            },
            ExprTmp::UnaryOp(op) => {
                let last = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => panic!()
                };
                tmp1.push(ExprTmp::Expr(Expr::UnaryOp { oper: op, val: Box::new(last) }))
            },
            ExprTmp::CompOp(op) => {
                let last_2 = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => panic!()
                };
                let last_1 = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => panic!()
                };
                tmp1.push(ExprTmp::Expr(Expr::CompOp { left: Box::new(last_1), oper: op, right: Box::new(last_2) }))
            }
            ExprTmp::LParenthesis   => panic!("Bruh {:?}", el)
        }
    }

    if tmp1.len() != 1 {panic!("Bruh {:?}", tmp1)}

    match &tmp1[0] {
        ExprTmp::Number(v) => Expr::Number(*v),
        ExprTmp::Ident(v)  => Expr::Ident(v.clone()),
        ExprTmp::Expr(v)   => v.clone(),
        _ => panic!()
    }
}
