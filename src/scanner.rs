use crate::token::Token;
use crate::buffer::*;
use crate::token::*;

#[derive(Debug)]
pub struct Scanner {
    pub buf: Buffer<char>,
    pub toks: Vec<Token>,
    pub start: usize
}

impl Scanner {
    pub fn new(src: Vec<char>) -> Self {
        Self {
            buf     : Buffer::new(src),
            toks    : Vec::new(),
            start   : 0
        }
    }
    #[inline]
    pub fn peek(&mut self) -> Option<char> {
        self.buf.peek()
    }
    #[inline]
    pub fn _while<F: Fn(char) -> bool>(&mut self, f: F) {
        self.buf._while(f)
    }
    #[inline]
    pub fn _if<F: Fn(char) -> bool>(&mut self, f: F) -> bool {
        self.buf._if(f)
    }
    #[inline]
    pub fn create(&mut self, kind: TokenKind) {
        let start   = self.start;
        let end     = self.buf.index;
        self.start = end;

        self.toks.push(Token{kind, str: self.buf.data[start..end].iter().collect()});
    }
    #[inline]
    pub fn str(&mut self) -> String {
        self.buf.data[self.start..self.buf.index].iter().collect()
    }
}

impl Iterator for Scanner {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.buf.next()
    }
}
