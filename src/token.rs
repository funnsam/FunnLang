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
            Keyword => "Keyword", Name => "Name", Number(_) => "Number",
            LF(_, _) => "Line feed",
            Comma => "Comma", Colon => "Colon", SemiColon => "Semicolon", EqualSign => "Equal sign", MathSymbol => "Math symbol",
            Logic => "Comparason", To => "To", Ampersand => "Ampersand", Star => "Star", Macro => "Macro", RightArrow => "Right arrow",
            LCurlyBracket => "Left curly bracket", RCurlyBracket => "Right curly bracket",
            LBracket => "Left bracket", RBracket => "Right bracket",
            LParenthesis => "Left parenthesis", RParenthesis => "Right parenthesis",
            Char(_) => "Character", Str(_) => "String",
            Unknown => "Unknown"
        })
    }
}