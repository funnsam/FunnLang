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
            '0'..='9' => {let a = parse_number(s, 0).unwrap(); s.create(Number(a));}
            '+' | '-' | '/' | '%' | '^' | '|' => s.create(MathSymbol),
            '*' => s.create(Star),
            '&' => s.create(Ampersand),
            '!' => {
                if s._if(|c| c == '=') {
                    s.create(Logic)
                } else {
                    s.create(MathSymbol)
                }
            }
            ' ' | '\t' | '\r' => { s._while(|c| c == ' ' || c == '\t' || c == '\r'); s.skip()},
            '\n'=> s.create(LF),
            '.' => {if s._if(|c| c == '.') {s.create(To)} else {s.create(Unknown)}}
            ',' => s.create(Comma),
            '{' => s.create(LCurlyBracket),
            '}' => s.create(RCurlyBracket),
            '[' => s.create(LBracket),
            ']' => s.create(RBracket),
            '(' => s.create(LParenthesis),
            ')' => s.create(RParenthesis),
            ':' => s.create(Colon),
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
                    let b = parse_escape(s);
                    if s.next().unwrap() != '\'' {todo!()}
                    s.create(Char(b))
                } else {
                    if s.next().unwrap() != '\'' {todo!()}
                    s.create(Char(a));
                }
            },
            '"' => {
                let mut str = String::new();
                while let Some(c) = s.next() {
                    match c {
                        '\n' | '\r' => todo!(),
                        '"'  => {s.create(Str(str)); break;},
                        '\\' => {
                            str.push(parse_escape(s))
                        }
                        _ => {
                            str.push(c);
                        },
                    }
                }
            },
            _ => { s._while(|c| c.is_alphabetic()); s.create(TokenKind::Unknown);},
        }
    };
    Buffer::new(s.toks.clone())
}

fn parse_escape(s: &mut Scanner) -> char {
    match s.next().unwrap() {
        'n' => '\n', 't' => '\t', 'r' => '\r', '\'' => '\'', '"' => '"',
        _ => panic!()
    }
}

fn parse_number(s: &mut Scanner, skip: usize) -> Option<i64> {
    let total_pref_len = skip + 2;

    match s.buf.current().unwrap() {
        '1'..='9' => {
            s._while(|c|c.is_ascii_digit());
            s.str().parse().ok()
        },
        '0' => match s.next().unwrap_or(' ') {
            '0'..='9' => {
                s._while(|c|c.is_ascii_digit());
                s.str().parse().ok()
            },
            'b' => {
                s._while(|c|c == '0' || c == '1');
                if s.str().len() <= total_pref_len { return None; }
                i64::from_str_radix(&s.str()[total_pref_len..s.str().len()], 2).ok()
            },
            'o' => {
                s._while(|c|c.is_ascii_digit() && c != '8' && c != '9');
                if s.str().len() <= total_pref_len { return None; }
                i64::from_str_radix(&s.str()[total_pref_len..s.str().len()], 8).ok()
            },
            'x' => {
                s._while(|c|c.is_ascii_hexdigit());
                if s.str().len() <= total_pref_len { return None; }
                i64::from_str_radix(&s.str()[total_pref_len..s.str().len()], 16).ok()
            },
            _ => {
                s.buf.index -= 1;
                Some(0)
            }
        },
        _ => None
    }
}
