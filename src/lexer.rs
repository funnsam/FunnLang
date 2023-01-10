use crate::buffer::Buffer;
use crate::scanner::*;
use crate::token::{*, TokenKind::*};

pub fn lex(s: &mut Scanner) -> Buffer<Token> {
    while let Some(c) = s.next() {
        match c {
            'a'..='z' | 'A'..='Z' => {
                s._while(|c| c.is_alphanumeric());
                match s.str().to_lowercase().as_str() {
                    "if"    | "else"    |
                    "var"   |
                    "for"   | "while"   | "break"   | "continue"    |
                    "func"  | "return"  |
                    "asm" => s.create(Keyword),
                    
                    _   => s.create(Name)
                }
            },
            '0'..='9' => {let a = parse_number(s, 0).unwrap_or(0); s.create(Number(a));}
            '+' | '-' | '*' | '/' | '%' | '^' | '!' => s.create(MathSymbol),
            '&' => {
                if s._if(|c| c == '&') {
                    s.create(MathSymbol)
                } else {
                    s.create(Ampersand)
                }
            },
            '|' => {s._if(|c| c == '|'); s.create(MathSymbol)}
            ' ' | '\t' | '\r' | '\n' => { s._while(|c| c.is_whitespace()); s.create(Space)},
            '.' => {if s._if(|c| c == '.') {s.create(To)} else {todo!()}}
            ',' => s.create(Comma),
            '{' => s.create(LCurlyBracket),
            '}' => s.create(RCurlyBracket),
            '[' => s.create(LBracket),
            ']' => s.create(RBracket),
            '(' => s.create(LParenthesis),
            ')' => s.create(RParenthesis),
            ';' => s.create(SemiColon),
            '=' => {if s._if(|c| c == '=') { s.create(Logic) } else { s.create(EqualSign) }},
            '<' => {
                if s._if(|c| c == '=') {
                    s.create(Logic)
                } else if s._if(|c| c == '<') {
                    s.create(MathSymbol)
                }
            },
            '>' => {
                if s._if(|c| c == '=') {
                    s.create(Logic)
                } else if s._if(|c| c == '>') {
                    s.create(MathSymbol)
                }
            },
            '\'' => {
                let a = s.next().unwrap();      // TODO error checking
                if a == '\\' {
                    let b = match s.next().unwrap() {
                        'n' => '\n', 't' => '\t', 'r' => '\r', '\'' => '\'',
                        _ => todo!()
                    };
                    if s.next().unwrap() != '\'' {todo!()}
                    s.create(Char(b))
                } else {
                    if s.next().unwrap() != '\'' {todo!()}
                    s.create(Char(a));
                }
            },
            '"' => {
                while let Some(c) = s.next() {
                    match c {
                        '\n' | '\r' => todo!(),
                        '"' => {s.create(Str); break;},
                        _ => continue,
                    }
                }
            },
            _ => { s._while(|c| c.is_alphabetic()); s.create(TokenKind::Unknown);},
        }
    };
    Buffer::new(s.toks.clone())
}

fn parse_number(s: &mut Scanner, skip: usize) -> Option<i64> {
    let total_pref_len = skip + 2;

    match s.buf.current().unwrap() {
        '-' | '+' | '1'..='9' => {
            s._while(|c|c.is_ascii_digit());
            return Some(s.str().parse().unwrap_or(0))
        },
        _ => (),
    }

    match s.peek().unwrap_or(' ') {
        '0'..='9' => {
            s.next();
            s._while(|c|c.is_ascii_digit());
            Some(s.str().parse().unwrap_or(0))
        },
        'b' => {
            s.next();
            s._while(|c|c == '0' || c == '1');
            if s.str().len() <= total_pref_len { return None; }
            Some(i64::from_str_radix(&s.str()[total_pref_len..s.str().len()], 2).unwrap_or(0))
        },
        'o' => {
            s.next();
            s._while(|c|c.is_ascii_digit() && c != '8' && c != '9');
            if s.str().len() <= total_pref_len { return None; }
            Some(i64::from_str_radix(&s.str()[total_pref_len..s.str().len()], 8).unwrap_or(0))
        },
        'x' => {
            s.next();
            s._while(|c|c.is_ascii_hexdigit());
            if s.str().len() <= total_pref_len { return None; }
            Some(i64::from_str_radix(&s.str()[total_pref_len..s.str().len()], 16).unwrap_or(0))
        },
        _ => None
    }
}