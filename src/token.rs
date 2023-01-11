#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub str : String
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword, Name, Number(i64),
    Space,
    Comma, SemiColon, EqualSign, MathSymbol, Logic, To, Ampersand,
    LCurlyBracket, RCurlyBracket, LBracket, RBracket, LParenthesis, RParenthesis,
    Char(char), Str(String),
    Unknown,
}