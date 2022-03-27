use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum PunctuationKind {
    And,
    Dot,
    Colon,
    Comma,
    RightArrow,
    SemiColon,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BracketKind {
    Angle,
    Brace,
    Round,
    Square,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BracketStatus {
    Close,
    Open,
}

#[derive(Debug, PartialEq, EnumIter, Clone)]
pub enum KeywordKind {
    Asm,
    Fn,
    Include,
    Inline
}

impl KeywordKind {
    pub fn name(&self) -> String {
        match self {
            KeywordKind::Fn => "fn".into(),
            KeywordKind::Asm => "asm".into(),
            KeywordKind::Include => "include".into(),
            KeywordKind::Inline => "inline".into()
        }
    }

    pub fn from_name(name: &str) -> Option<TokenKind> {
        KeywordKind::iter().find(|it| it.name() == name).map(TokenKind::KeyWord)
    }
}
