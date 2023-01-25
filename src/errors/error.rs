use crate::{token::TokenKind, ast::nodes::Node};


pub enum ErrorKind {
    ExpectsButFound {
        expect  : TokenKind,
        found   : TokenKind
    },
    UnclosedBracket,
    UnexpectedNodeType {
        found: Node
    },
    UnexpectedToken {
        found: TokenKind
    },
    MathError,
    NoReturn
}

#[allow(dead_code)]
pub enum ErrorLevel {
    Error, Warning, Info
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::ErrorKind::*;
        match self {
            ExpectsButFound { expect, found }
                => write!(f, "expecting {}, but found {}", expect, found),
            UnclosedBracket
                => write!(f, "unclosed bracket"),
            UnexpectedNodeType { found }
                => write!(f, "unexpected {}", found),
            UnexpectedToken { found }
                => write!(f, "unexpected token {}", found),
            MathError
                => write!(f, "math error"),
            NoReturn
                => write!(f, "no return found")
        }
    }
}

impl std::fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::ErrorLevel::*;
        match self {
            Error => write!(f, "\x1b[1;31merror:\x1b[0;0m"),
            Warning => write!(f, "\x1b[1;33mwarning:\x1b[0;0m"),
            Info => write!(f, "\x1b[1;36minfo:\x1b[0;0m")
        }
    }
}