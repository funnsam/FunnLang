use crate::ast::nodes::Node;
use crate::ast::nodes::Program;
use crate::buffer::*;
use crate::token::*;

#[derive(Debug)]
pub struct Parser<'a> {
    pub buf: ParserBuffer,
    pub ast: crate::ast::nodes::Program,
    pub last: Option<&'a mut Program>
}

impl Parser<'_> {
    pub fn new(src: Buffer<Token>) -> Self {
        Self {
            buf : ParserBuffer::new(src),
            ast : crate::ast::nodes::Program{body: Vec::new()},
            last: None,
        }
    }
    pub fn add_node(&mut self, node: Node) {
        match &self.last {Some(a) => *a, None => &mut self.ast}.body.push(node)
    }
}

#[derive(Debug)]
pub struct ParserBuffer {
    pub buf: Buffer<Token>,
}

impl ParserBuffer {
    pub fn new(src: Buffer<Token>) -> Self {
        Self {
            buf : src,
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
        while self.buf.current().unwrap_or(Token { kind: TokenKind::Comma, str: "".to_string() }).kind == TokenKind::Space {
            self.buf.index += 1;
        }
    }
    #[inline]
    pub fn next(&mut self) -> Option<Token> {
        let a = self.buf.current();
        self.advance();
        a
    }
}

impl Iterator for ParserBuffer {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.buf.next()
    }
}