#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub str : String
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword, Name, Number(i64),
    LF(usize, usize),
    Comma, Colon, SemiColon, EqualSign, MathSymbol, Logic, To, Ampersand, Star, Macro, RightArrow,
    LCurlyBracket, RCurlyBracket, LBracket, RBracket, LParenthesis, RParenthesis,
    Char(char), Str(String),
    Unknown,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::TokenKind::*;
        write!(f, "{}", match self {
            Keyword => "keyword", Name => "name", Number(_) => "number",
            LF(_, _) => "line feed",
            Comma => "comma", Colon => "colon", SemiColon => "semicolon", EqualSign => "equal sign", MathSymbol => "math symbol",
            Logic => "comparason", To => "to", Ampersand => "ampersand", Star => "star", Macro => "macro", RightArrow => "right arrow",
            LCurlyBracket => "left curly bracket", RCurlyBracket => "right curly bracket",
            LBracket => "left bracket", RBracket => "right bracket",
            LParenthesis => "left parenthesis", RParenthesis => "right parenthesis",
            Char(_) => "character", Str(_) => "string",
            Unknown => "unknown"
        })
    }
}