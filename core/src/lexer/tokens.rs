use std::fmt::{Display, Formatter};

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::parser::ast::ASTPosition;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: ASTPosition,
}

impl Token {
    pub fn new(kind: TokenKind, row: usize, column: usize) -> Self {
        Self {
            kind,
            position: ASTPosition::new(row, column),
        }
    }

    pub fn alpha(&self) -> Option<String> {
        if let TokenKind::AlphaNumeric(name) = &self.kind {
            Some(name.to_owned())
        } else {
            None
        }
    }

    pub fn keyword(&self) -> Option<String> {
        if let TokenKind::KeyWord(kind) = &self.kind {
            Some(kind.name())
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum TokenKind {
    AlphaNumeric(String),
    NativeBlock(String),
    Bracket(BracketKind, BracketStatus),
    Comment(String),
    MultiLineComment(String),
    EndOfLine,
    KeyWord(KeywordKind),
    Number(String),
    Punctuation(PunctuationKind),
    StringLiteral(String),
    CharLiteral(String),
    WhiteSpaces(String),
    Reserved(ReservedKind),
}

#[derive(Debug, PartialEq, Clone, Eq, EnumIter)]
pub enum ReservedKind {
    I32,
    F32,
    STR,
    BOOL,
    CHAR,
}

impl ReservedKind {
    pub fn from_name(name: &str) -> Option<TokenKind> {
        ReservedKind::iter()
            .find(|it| it.name() == name)
            .map(TokenKind::Reserved)
    }

    fn name(&self) -> &str {
        match self {
            ReservedKind::I32 => "i32",
            ReservedKind::F32 => "f32",
            ReservedKind::STR => "str",
            ReservedKind::BOOL => "bool",
            ReservedKind::CHAR => "char",
        }
    }
}

impl Display for ReservedKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::AlphaNumeric(s) => {
                write!(f, "'{}'", s)
            }
            TokenKind::NativeBlock(_) => {
                write!(f, "native")
            }
            TokenKind::Bracket(kind, status) => match kind {
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
            },
            TokenKind::Comment(_) => {
                write!(f, "comment")
            }
            TokenKind::MultiLineComment(_) => {
                write!(f, "multi line comment")
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
            TokenKind::Punctuation(p) => match p {
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
                PunctuationKind::Equal => {
                    write!(f, "=")
                }
            },
            TokenKind::StringLiteral(s) => {
                write!(f, "\"{}\"", s)
            }
            TokenKind::WhiteSpaces(_) => {
                write!(f, "WS")
            }
            TokenKind::CharLiteral(c) => write!(f, "{c}"),
            TokenKind::Reserved(reserved_kind) => write!(f, "{reserved_kind}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PunctuationKind {
    Dot,
    Colon,
    Comma,
    Equal,
    RightArrow,
    SemiColon,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BracketKind {
    Angle,
    Brace,
    Round,
    Square,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BracketStatus {
    Close,
    Open,
}

#[derive(Debug, PartialEq, Eq, EnumIter, Clone)]
pub enum KeywordKind {
    Const,
    Enum,
    False,
    Fn,
    Inline,
    Let,
    Native,
    Pub,
    Struct,
    True,
    Type,
}

impl KeywordKind {
    pub fn name(&self) -> String {
        match self {
            KeywordKind::Const => "const".into(),
            KeywordKind::Enum => "enum".into(),
            KeywordKind::False => "false".into(),
            KeywordKind::Fn => "fn".into(),
            KeywordKind::Inline => "inline".into(),
            KeywordKind::Let => "let".into(),
            KeywordKind::Native => "native".into(),
            KeywordKind::Pub => "pub".into(),
            KeywordKind::Struct => "struct".into(),
            KeywordKind::True => "true".into(),
            KeywordKind::Type => "type".into(),
        }
    }

    pub fn from_name(name: &str) -> Option<TokenKind> {
        KeywordKind::iter()
            .find(|it| it.name() == name)
            .map(TokenKind::KeyWord)
    }
}
