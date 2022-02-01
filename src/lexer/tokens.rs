use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub row: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, row: usize, column: usize) -> Self {
        Self { kind, row, column }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    AlphaNumeric(String),
    AsmBLock(String),
    Bracket(BracketKind, BracketStatus),
    Comment(String),
    EndOfLine,
    KeyWord(KeywordKind),
    Number(String),
    Punctuation(PunctuationKind),
    StringLiteral(String),
    WhiteSpaces(String),
}

#[derive(Debug, PartialEq)]
pub enum PunctuationKind {
    Dot,
    Colon,
    Comma,
    SemiColon,
}

#[derive(Debug, PartialEq)]
pub enum BracketKind {
    Angle,
    Brace,
    Round,
    Square,
}

#[derive(Debug, PartialEq)]
pub enum BracketStatus {
    Close,
    Open,
}

#[derive(Debug, PartialEq, EnumIter)]
pub enum KeywordKind {
    Asm,
    Fn,
}

impl KeywordKind {
    pub fn name(&self) -> String {
        match self {
            KeywordKind::Fn => "fn".into(),
            KeywordKind::Asm => "asm".into()
        }
    }

    pub fn from_name(name: &str) -> Option<TokenKind> {
        KeywordKind::iter().find(|it| it.name() == name).map(|it| TokenKind::KeyWord(it))
    }
}
