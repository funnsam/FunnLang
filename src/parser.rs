use crate::ast::nodes::Node;
use crate::ast::nodes::Program;
use crate::buffer::*;
use crate::errors::*;
use crate::parser::error::{ErrorKind::*, ErrorKind};
use crate::to_mut_ptr;
use crate::token::*;

pub struct Parser {
    pub buf: ParserBuffer,
    pub ast: Program,
    pub err: ErrorHandler
}

impl Parser {
    pub fn new(src: &Buffer<Token>) -> Self {
        let ast = Program{body: Vec::new(), escaped: false};
        Self {
            buf: ParserBuffer::new(src),
            ast,
            err: ErrorHandler::new()
        }
    }

    pub fn add_node(&mut self, node: Node) {
        self.find_scope().body.push(node)
    }

    pub fn error(&mut self, errkind: ErrorKind) {
        self.err.add_error(
            Error::new(
                errkind, error::ErrorLevel::Error, self.buf.line, self.buf.file)
        )
    }

    pub fn expect_semicolon(&mut self) {
        match self.buf.next().unwrap().kind {
            TokenKind::SemiColon => (),
            _ => {
                self.err.add_error(
                    Error::new(
                        ExpectsButFound {
                            expect: TokenKind::SemiColon,
                            found: self.buf.current().unwrap().kind
                        },
                        error::ErrorLevel::Error,
                        self.buf.line,
                        self.buf.file
                    )
                )
            }
        }
    }

    pub fn expect_in_loop(&mut self, cur: &Node) {
        if !self.is_in_loop() {
            self.error(ErrorKind::UnexpectedNodeType { found: cur.to_owned() })
        }
    }

    pub fn expect_fn_with_ret(&mut self) {
        if self.depth() != 1 {
            return
        }

        if matches!(self.ast.body.last().unwrap(), Node::FuncDefine { func_name: _, func_args: _, func_type: _, func_body: _, linkage: _ }) {
            if !matches!(self.find_scope().body.last().unwrap_or(&Node::Break), Node::Return(_)) {
                self.error(ErrorKind::NoReturn)
            }
        }
    }

    pub fn find_scope(&mut self) -> &mut Program {
        let mut scope: &Program = &self.ast;
        while let Some(a) = find_scope_from_program(scope) {scope = a}
        to_mut_ptr(scope)
    }

    pub fn depth(&self) -> usize {
        let mut depth = 0;
        let mut scope: &Program = &self.ast;
        while let Some(a) = find_scope_from_program(scope) {depth += 1; scope=a}
        depth
    }

    pub fn is_at_root(&self) -> bool {
        let mut scope: &Program = &self.ast;
        while let Some(a) = find_scope_from_program(scope) {scope = a}
        scope == &self.ast
    }

    pub fn is_in_loop(&self) -> bool {
        let mut scope: &Program = &self.ast;
        while let Some(a) = find_scope_from_program(scope) {
            if let Some(_) = find_loop(a) {
                return true
            }
            scope = a;
        }
        false
    }

    pub fn find_branch_block(&mut self) -> &mut Node {
        let scope = self.find_scope();
        let last = find_branch(scope).unwrap();
        to_mut_ptr(last)
    }
}

fn find_branch(p: &Program) -> Option<&Node> {
    let mut last: Option<&Node> = None;
    for el in p.body.iter().rev() {
        match el {
            Node::Branch { .. } => {
                last = Some(&el);
                break;
            }
            _ => (),
        }
    };
    last
}

fn find_loop(p: &Program) -> Option<&Node> {
    let mut last: Option<&Node> = None;
    for el in p.body.iter().rev() {
        match el {
            Node::While { .. } => {
                last = Some(&el);
                break;
            },
            Node::For { .. } => {
                last = Some(&el);
                break;
            }
            _ => (),
        }
    };
    last
}

fn find_scope_from_program(p: &Program) -> Option<&Program> {
    let mut scope: Option<&Program> = None;
    'find_scope_loop: for el in p.body.iter().rev() {
        match el {
            Node::FuncDefine { func_body, .. } => {
                if func_body.escaped {continue;}
                scope = Some(&func_body);
                break;
            },
            Node::For { body, .. } => {
                if body.escaped {continue;}
                scope = Some(&body);
                break;
            },
            Node::While { body, .. } => {
                if body.escaped {continue;}
                scope = Some(&body);
                break;
            },
            Node::Branch { body, .. } => {
                for el in body.iter().rev() {
                    if el.escaped {continue;}
                    scope = Some(el);
                    break 'find_scope_loop;
                };
            },
            Node::CodeBlock(c) => {
                if c.escaped {continue;}
                scope = Some(c);
                break;
            },
            _ => (),
        }
    };
    scope
}

#[derive(Debug)]
pub struct ParserBuffer {
    pub buf: Buffer<Token>,
    pub line: usize,
    pub file: usize
}

impl ParserBuffer {
    pub fn new(src: &Buffer<Token>) -> Self {
        Self {
            buf : src.clone(),
            line: 0,
            file: 0,
        }
    }
    #[inline]
    pub fn _while<F: Fn(Token) -> bool>(&mut self, f: F) {
        self.buf._while(f)
    }
    #[inline]
    pub fn _if<F: Fn(Token) -> bool>(&mut self, f: F) -> bool {
        self.buf._if(f)
    }
    #[inline]
    pub fn advance(&mut self) {
        self.buf.index += 1;
        while match self.buf.current().unwrap_or(Token { kind: TokenKind::Comma, str: "".to_string() }).kind {
            TokenKind::LF(l, f) => {
                self.line = l;
                self.file = f;
                true
            },
            _ => false
        } {
            self.buf.index += 1;
        }
    }
    #[inline]
    pub fn next(&mut self) -> Option<Token> {
        self.advance();
        self.buf.current()
    }
    #[inline]
    pub fn current(&self) -> Option<Token> {
        self.buf.current()
    }
}

impl Iterator for ParserBuffer {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.buf.next()
    }
}
