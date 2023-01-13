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
            start   : usize::MAX
        }
    }
    #[inline]
    #[allow(dead_code)]
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
        self.toks.push(Token{kind, str: self.str()});
        self.start = self.buf.index;
    }
    #[inline]
    pub fn str(&self) -> String {
        self.buf.data[self.start+1..self.buf.index+1].iter().collect()
    }
    #[inline]
    pub fn skip(&mut self) {
        self.start = self.buf.index;
    }
}

impl Iterator for Scanner {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.buf.next()
    }
}
