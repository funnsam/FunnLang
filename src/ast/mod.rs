pub mod nodes;

use crate::parser::*;
use crate::buffer::*;
use crate::token::{*, TokenKind::*};

use self::nodes::*;

pub fn generate_ast(tok: Buffer<Token>, src: String) -> Parser<'static> {
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
                        let mut tmp = Vec::new();
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

                        let mut args = Vec::new();
                        for el in raw_args.into_iter() {
                            args.push(FuncDefArg{name: el[0].clone(), typ: Type::Name(el[1].clone())})
                        }

                        let mut body = Program { body: Vec::new() };
                        
                        p.ast.body.push(Node::FuncDefine {
                            func_name: name.str,
                            func_args: args,
                            func_type: Type::Name(p.buf.next().unwrap().str),
                            func_body: body
                        });
                    },
                    "var" => {
                        let name = p.buf.next().unwrap();
                        let typ  = p.buf.next().unwrap();
                        if name.kind != Name && typ.kind != Name {todo!()}
                        p.buf.advance();

                        let mut expr: Expr = Expr::Number(p.buf.buf.peek().unwrap().str.parse().unwrap());
                        while let Some(a) = p.buf.next() {
                            if a.kind == SemiColon {break;}
                        }

                        p.ast.body.push(
                            Node::VarDefine {
                                var_type: Type::Name(typ.str),
                                var_name: name.str,
                                val_expr: expr
                            }
                        )
                    }
                    _ => ()
                }
            },
            _ => (),
        }
    }
    p
}