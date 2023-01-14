use crate::{token::TokenKind, ast::nodes::Node};

#[derive(Debug)]
pub enum ErrorKind {
    MissingSemiColon {
        found: TokenKind
    },
    UnclosedBracket,
    UnexpectedNodeType {
        found: Node
    },
    MainNotFound,
    UnexpectedToken {
        found: TokenKind
    },
    TreatAs {
        into: Node
    }
}

#[derive(Debug)]
pub enum ErrorLevel {
    Error, Warning, Info
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::ErrorKind::*;
        match self {
            MissingSemiColon { found }
                => write!(f, "Missing semicolon, found {:?}", found),
            UnclosedBracket
                => write!(f, "Unclosed Bracket"),
            UnexpectedNodeType { found }
                => write!(f, "Unexpected {}", found),
            MainNotFound
                => write!(f, "Cannot find main function"),
            UnexpectedToken { found }
                => write!(f, "Unexpected token {}", found),
            TreatAs { into }
                => write!(f, "Treat it as a {}", into)
        }
    }
}

impl std::fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::ErrorLevel::*;
        match self {
            Error => write!(f, "\x1b[1;31mError\x1b[0;0m"),
            Warning => write!(f, "\x1b[1;33mWarning\x1b[0;0m"),
            Info => write!(f, "\x1b[1;36mInfo\x1b[0;0m")
        }
    }
}