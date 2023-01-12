pub mod nodes;

use crate::parser::*;
use crate::buffer::*;
use crate::token::{*, TokenKind::*};

use self::nodes::*;

pub fn generate_ast(tok: Buffer<Token>, src: String) -> Parser {
    let mut p = Parser::new(tok);
    
    while let Some(t) = p.buf.next() {
        println!("{:?}", t);
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
                        p.buf.advance();
                        match p.buf.current().unwrap().str.as_str() {
                            "if" => {
                                p.buf.advance();
                                p.buf.advance();
                                while let Some(t) = p.buf.next() {
                                    if t.kind == RParenthesis {break}
                                }
                                p.buf.advance();
                                match p.find_branch_block() {
                                    Node::Branch { cond, body } => {
                                        cond.push(Expr::Number(1));
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
                            if !el.is_empty() {
                                args.push(parse_expr(&mut Buffer::new(el)))
                            }
                        }

                        p.buf.advance();
                        p.add_node(Node::FuncCall { func_name: name, func_args: args });
                    },
                    _ => (),
                }
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

fn parse_expr_from_parser(p: &mut Parser, stop_tokens: &Vec<TokenKind>) -> Expr {
    let mut tmp = Vec::new();
    while let Some(a) = p.buf.next() {
        if stop_tokens.contains(&a.kind) {break;}
        tmp.push(a);
    };
    parse_expr(&mut Buffer::new(tmp))
}

fn parse_expr(toks: &mut Buffer<Token>) -> Expr {
    fn get_precedence(a: &Token) -> u8 {
        match a.str.chars().next().unwrap() {
            '+' | '-' => 1,
            '*' | '/' => 2,
            '&' | '|' | '^' => 3,
            _ => 0
        }
    }

    let mut output = Vec::new();
    let mut op_stk: Vec<Token> = Vec::new();

    while let Some(a) = toks.next() {
        match a.kind {
            Number(_) | Name => output.push(a),
            MathSymbol => {
                while let Some(b) = op_stk.pop() {
                    if b.kind != LParenthesis && get_precedence(&b) >= get_precedence(&a) {
                        output.push(b);
                    } else {
                        op_stk.push(b);
                        break
                    }
                };
                op_stk.push(a)
            }
            LParenthesis => op_stk.push(a),
            RParenthesis => {
                while let Some(v) = op_stk.pop() {
                    if v.kind == LParenthesis {
                        break;
                    }
                    output.push(v)
                }
            }
            _ => panic!("Bruh {:?}", a),
        }
    }
    output.append(&mut op_stk);

    #[derive(Debug)]
    enum ExprTmp {
        Number(i64),
        Ident(String),
        Math(MathOp),
        Expr(Expr)
    }

    let mut exprs_1 = Vec::new();

    for el in output.into_iter() {
        match el.kind {
            Number(v) => exprs_1.push(ExprTmp::Number(v)),
            Name => exprs_1.push(ExprTmp::Ident(el.str)),
            MathSymbol => exprs_1.push(ExprTmp::Math(match el.str.chars().next().unwrap() {
                '+' => MathOp::Add, '-' => MathOp::Sub, '*' => MathOp::Mul, '/' => MathOp::Div,
                '%' => MathOp::Mod, '&' => MathOp::And, '|' => MathOp::Or , '^' => MathOp::XOr,
                _ => panic!()
            })),
            _ => panic!()
        }
    }

    let mut tmp1 = Vec::new();

    for el in exprs_1.into_iter() {
        match el {
            ExprTmp::Number(_) | ExprTmp::Ident(_) => tmp1.push(el),
            ExprTmp::Math(m) => {
                let last_2 = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => panic!()
                };
                let last_1 = match tmp1.pop().unwrap() {
                    ExprTmp::Number(v) => Expr::Number(v), ExprTmp::Ident(v) => Expr::Ident(v), ExprTmp::Expr(v) => v,
                    _ => panic!()
                };

                tmp1.push(ExprTmp::Expr(Expr::Math { left: Box::new(last_1), oper: m, right: Box::new(last_2) }))
            },

            ExprTmp::Expr(_) => ()
        }
    }

    match &tmp1[0] {
        ExprTmp::Number(v) => Expr::Number(*v),
        ExprTmp::Ident(v)  => Expr::Ident(v.clone()),
        ExprTmp::Expr(v)   => v.clone(),
        _ => panic!()
    }
}
