pub mod nodes;

use core::panic;
use std::vec;

use crate::errors::error::*;
use crate::parser::*;
use crate::buffer::*;
use crate::token::{*, TokenKind::*};

use self::nodes::*;

pub fn generate_ast(tok: &Buffer<Token>) -> Parser {
    let mut p = Parser::new(tok);
    
    while let Some(t) = p.buf.next() {
        match t.kind {
            Keyword => {
                match t.str.to_lowercase().as_str() {
                    "func" => {
                        let mut linkage = InternLinkage::Private;
                        let a = p.buf.buf.peek().unwrap();
                        match a.kind {
                            Keyword => {
                                let a = p.buf.next().unwrap();
                                if a.str == "extern" {
                                    linkage = InternLinkage::Extern
                                } else if a.str == "pub" {
                                    linkage = InternLinkage::Public
                                } else {
                                    p.error(ErrorKind::UnexpectedToken { found: a.kind })
                                }
                            },
                            Name => (),
                            _ => p.error(ErrorKind::UnexpectedToken { found: a.kind })
                        }

                        let name = p.buf.next().unwrap();
                        if name.kind != Name { p.error(ErrorKind::UnexpectedToken { found: name.kind }) }

                        p.buf.advance();

                        let mut raw_args: Vec<Vec<Token>> = Vec::new();
                        let mut tmp: Vec<Token> = Vec::with_capacity(2);
                        while let Some(t) = p.buf.next() {
                            if t.kind == RParenthesis {
                                if !tmp.is_empty() {raw_args.push(tmp.clone())}
                                break;
                            } else if t.kind == Comma {
                                raw_args.push(tmp.clone());
                                tmp.clear()
                            } else {
                                tmp.push(t)
                            }
                        }
                        drop(tmp);

                        let mut func_args = Vec::new();
                        for el in raw_args.iter_mut() {
                            let name = match linkage {
                                InternLinkage::Extern => "".to_string(),
                                _ => {
                                    let a = el[0].str.clone();
                                    el.remove(0);
                                    a
                                }
                            }; 
                            let typ  = parse_type(&mut Buffer::new(el.to_vec()), &mut p);
                            func_args.push(FuncDefArg{name, typ})
                        }

                        let func_body = Program { body: Vec::new(), escaped: linkage == InternLinkage::Extern };

                        let stop_tok = match linkage {
                            InternLinkage::Extern => vec![SemiColon],
                            _ => vec![LCurlyBracket]
                        };
                        let func_type = parse_type_from_parser(&mut p, &stop_tok);

                        let node = Node::FuncDefine {
                            func_name: name.str,
                            func_args,
                            func_type,
                            func_body,
                            linkage
                        };

                        if !p.is_at_root() {
                            p.error(ErrorKind::UnexpectedNodeType { found: node });
                        } else {
                            p.add_node(node);
                        }
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
                        let expr = parse_expr_from_parser(&mut p, &vec![RParenthesis]);
                        p.buf.advance();
                        p.add_node(
                            Node::While {
                                cond: expr,
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
                    "break" => {
                        p.expect_in_loop(&Node::Break);
                        p.expect_semicolon();
                        p.add_node(Node::Break)
                    },
                    "continue" => {
                        p.expect_in_loop(&Node::Continue);
                        p.expect_semicolon();
                        p.add_node(Node::Continue)
                    },
                    "if" => {
                        p.buf.advance();
                        let expr = parse_expr_from_parser(&mut p, &vec![RParenthesis]);
                        p.buf.advance();
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
                                p.buf.advance();
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
                        p.expect_semicolon();
                        p.add_node(Node::AsmBlock(
                            asm_blk
                        ))
                    },
                    "return" => {
                        let expr = match p.buf.buf.peek().unwrap().kind {
                            SemiColon => {
                                p.buf.advance();
                                None
                            },
                            _ => Some(parse_expr_from_parser(&mut p, &vec![SemiColon]))
                        };
                        p.add_node(
                            Node::Return(expr)
                        )
                    },
                    _ => {
                        p.error(ErrorKind::UnexpectedToken { found: t.kind })
                    }
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
                                args.push(parse_expr(&mut Buffer::new(el), &mut p))
                            }
                        }

                        p.expect_semicolon();
                        p.add_node(Node::FuncCall { func_name: name, func_args: args });
                    },
                    _ => (),
                }
            },
            LCurlyBracket => {
                p.add_node(Node::CodeBlock(Program { body: Vec::new(), escaped: false }))
            }
            RCurlyBracket => {
                if p.is_at_root() {
                    p.error(ErrorKind::UnexpectedToken { found: t.kind })
                } else {
                    p.expect_fn_with_ret();
                    p.find_scope().escaped = true
                }
            },
            _ => {
                p.error(ErrorKind::UnexpectedToken { found: t.kind })
            },
        }
    }
    if !p.is_at_root() {
        p.error(ErrorKind::UnclosedBracket)
    }
    p
}

fn parse_type_from_parser(p: &mut Parser, stop_tokens: &Vec<TokenKind>) -> Type {
    let mut tmp = Vec::new();
    while let Some(a) = p.buf.next() {
        if stop_tokens.contains(&a.kind) { break }
        tmp.push(a);
    };
    parse_type(&mut Buffer::new(tmp), p)
}

fn parse_type(toks: &mut Buffer<Token>, p: &mut Parser) -> Type{
    toks.data.reverse();

    if toks.data.len() != 1 {
        todo!()
    }

    let mut tmp: Type = Type::Name(match toks.data[0].kind {
        Name => toks.data[0].str.clone(),
        _ => {
            p.error(ErrorKind::UnexpectedToken { found: toks.data[0].clone().kind });
            "u8".to_string()
        }
    });

    while let Some(tok) = toks.next() {
        match tok.kind {
            Ampersand     => tmp = Type::Pointer(Box::new(tmp)),
            RBracket      => {
                let size = match toks.next().unwrap().kind {
                    Number(v) => v,
                    _ => {
                        p.error(ErrorKind::ExpectsButFound { expect: Number(0), found: toks.current().unwrap().kind } );
                        0
                    }
                };
                toks.next();
                tmp = Type::Array(Box::new(tmp), size as usize)
            },
            Name => (),
            _ => p.error(ErrorKind::UnexpectedToken { found: tok.kind })
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
    parse_expr(&mut Buffer::new(tmp), p)
}

fn parse_expr(toks: &mut Buffer<Token>, p: &mut Parser) -> Expr {
    #[derive(Debug, Clone, PartialEq)]
    enum ExprTmp {
        Number(i64),
        Ident(String),
        BoolOp(BiOp),
        UnaryOp(UnaryOp),
        CompOp(CompOp),
        Expr(Expr),
        Cast(Type),
        LParenthesis,
    }
    fn get_precedence(a: &ExprTmp) -> u8 {
        match a {
            ExprTmp::UnaryOp(_) => 5,
            ExprTmp::BoolOp(op) => {
                use BiOp::*;
                match op {
                    Add | Sub => 1,
                    Mul | Div | Mod => 2,
                    And | Or  | XOr | LSh | RSh => 3,
                }
            },
            ExprTmp::CompOp(_)  => 4,
            ExprTmp::Cast(_)    => 5,
            _ => 0,
        }
    }
    fn as_expr_tmp(el: Token, is_unary: bool, iter: &mut Buffer<Token>, p: &mut Parser) -> ExprTmp {
        match el.kind {
            Number(v) => ExprTmp::Number(v), Char(v) => ExprTmp::Number(v as i64),
            Name => {
                match iter.peek().unwrap_or(Token { kind: LF(0, 0), str: "\n".to_string() }).kind {
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
                                args.push(parse_expr(&mut Buffer::new(el), p))
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
                            "+" => BiOp::Add, "-" => BiOp::Sub, "*" => BiOp::Mul, "/" => BiOp::Div,
                            "%" => BiOp::Mod,
                            "&" => BiOp::And, "|" => BiOp::Or , "^" => BiOp::XOr, "<<" => BiOp::LSh, ">>" => BiOp::RSh,
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
            RightArrow => {
                ExprTmp::Cast(
                    {
                        let mut tmp = Vec::new();
                        let mut lvl = 0;
                        while let Some(v) = iter.next() {
                            match v.kind {
                                RParenthesis => {
                                    lvl -= 1;
                                    if lvl == 0 {
                                        tmp.push(v);
                                        break;
                                    }
                                }
                                LParenthesis => lvl += 1,
                                Name => {
                                    tmp.push(v);
                                    break;
                                },
                                _ => ()
                            }
                            tmp.push(v)
                        }
                        parse_type(&mut Buffer::new(tmp), p)
                    }
                )
            },
            Logic => {
                use CompOp::*;
                ExprTmp::CompOp(match el.str.as_str() {
                    "==" => EQ, "!=" => NEQ,
                    ">"  => LT, ">=" => LTE,
                    "<"  => GT, "<=" => GTE,
                    _ => unreachable!()
                })
            },
            _ => {
                p.error(ErrorKind::MathError);
                ExprTmp::Number(0)
            }
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
            Number(v) => output.push(ExprTmp::Number(v)), Char(v) => output.push(ExprTmp::Number(v as i64)),
            Name => output.push(as_expr_tmp(a, false, toks, p)),
            MathSymbol | Star | Ampersand | Logic | RightArrow => {
                let is_unary = is_math(prev.unwrap_or(MathSymbol));
                while let Some(b) = op_stk.pop() {
                    if b != ExprTmp::LParenthesis && get_precedence(&b) >= get_precedence(&as_expr_tmp(a.clone(), is_unary, toks, p)) {
                        output.push(b);
                    } else {
                        op_stk.push(b);
                        break
                    }
                };
                op_stk.push(as_expr_tmp(a, is_unary, toks, p));
            },
            LParenthesis => op_stk.push(ExprTmp::LParenthesis),
            RParenthesis => {
                while let Some(v) = op_stk.pop() {
                    if v == ExprTmp::LParenthesis { break }
                    output.push(v)
                }
            },
            _ => p.error(ErrorKind::UnexpectedToken { found: a.kind }),
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
                    _ => {
                        p.error(ErrorKind::MathError);
                        Expr::Number(0)
                    }
                };
                let last_1 = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => {
                        p.error(ErrorKind::MathError);
                        Expr::Number(0)
                    }
                };
                tmp1.push(ExprTmp::Expr(Expr::BiOp { left: Box::new(last_1), oper: op, right: Box::new(last_2) }))
            },
            ExprTmp::UnaryOp(op) => {
                let last = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => {
                        p.error(ErrorKind::MathError);
                        Expr::Number(0)
                    }
                };
                tmp1.push(ExprTmp::Expr(Expr::UnaryOp { oper: op, val: Box::new(last) }))
            },
            ExprTmp::CompOp(op) => {
                let last_2 = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => {
                        p.error(ErrorKind::MathError);
                        Expr::Number(0)
                    }
                };
                let last_1 = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => {
                        p.error(ErrorKind::MathError);
                        Expr::Number(0)
                    }
                };
                tmp1.push(ExprTmp::Expr(Expr::CompOp { left: Box::new(last_1), oper: op, right: Box::new(last_2) }))
            },
            ExprTmp::Cast(typ) => {
                let val = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => {
                        p.error(ErrorKind::MathError);
                        Expr::Number(0)
                    }
                };
                tmp1.push(ExprTmp::Expr(Expr::Cast { typ, val: Box::new(val) }))
            },
            ExprTmp::LParenthesis   => p.error(ErrorKind::UnclosedBracket)
        }
    }

    if tmp1.len() != 1 {
        p.error(ErrorKind::MathError);
        return Expr::Number(0)
    }

    match &tmp1[0] {
        ExprTmp::Number(v) => Expr::Number(*v),
        ExprTmp::Ident(v)  => Expr::Ident(v.clone()),
        ExprTmp::Expr(v)   => v.clone(),
        _ => {
            p.error(ErrorKind::MathError);
            Expr::Number(0)
        }
    }
}
