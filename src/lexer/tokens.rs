use std::fmt::{Display, Formatter};
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

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::AlphaNumeric(s) => {
                write!(f, "'{}'", s)
            }
            TokenKind::AsmBLock(_) => {
                write!(f, "asm")
            }
            TokenKind::Bracket(kind, status) => {
                match kind {
                    BracketKind::Angle => {
                        if status == &BracketStatus::Open {
                            write!(f, "<")
                        } else {
                            write!(f, ">")
                        }
                    }
                    BracketKind::Brace => {
                        if status == &BracketStatus::Open {
                            write!(f, "{{")
                        } else {
                            write!(f, "}}")
                        }
                    }
                    BracketKind::Round => {
                        if status == &BracketStatus::Open {
                            write!(f, "(")
                        } else {
                            write!(f, ")")
                        }
                    }
                    BracketKind::Square => {
                        if status == &BracketStatus::Open {
                            write!(f, "[")
                        } else {
                            write!(f, "]")
                        }
                    }
                }
            }
            TokenKind::Comment(_) => {
                write!(f, "comment")
            }
            TokenKind::EndOfLine => {
                write!(f, "EOL")
            }
            TokenKind::KeyWord(k) => {
                write!(f, "{:?}", k)
            }
            TokenKind::Number(n) => {
                write!(f, "{}", n)
            }
            TokenKind::Punctuation(p) => {
                match p {
                    PunctuationKind::And => {
                        write!(f, "&")
                    }
                    PunctuationKind::Dot => {
                        write!(f, ".")
                    }
                    PunctuationKind::Colon => {
                        write!(f, ":")
                    }
                    PunctuationKind::Comma => {
                        write!(f, ",")
                    }
                    PunctuationKind::RightArrow => {
                        write!(f, "->")
                    }
                    PunctuationKind::SemiColon => {
                        write!(f, ";")
                    }
                }
            }
            TokenKind::StringLiteral(s) => {
                write!(f, "\"{}\"", s)
            }
            TokenKind::WhiteSpaces(_) => {
                write!(f, "WS")
            }
        }
    }
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
    Inline,
    Enum
}

impl KeywordKind {
    pub fn name(&self) -> String {
        match self {
            KeywordKind::Fn => "fn".into(),
            KeywordKind::Asm => "asm".into(),
            KeywordKind::Include => "include".into(),
            KeywordKind::Inline => "inline".into(),
            KeywordKind::Enum => "enum".into()
        }
    }

    pub fn from_name(name: &str) -> Option<TokenKind> {
        KeywordKind::iter().find(|it| it.name() == name).map(TokenKind::KeyWord)
    }
}
