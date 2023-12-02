use std::fmt::{Display, Formatter};
use std::path::PathBuf;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::parser::ast::ASTIndex;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub file_name: Option<PathBuf>,
    pub row: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, file_name: Option<PathBuf>, row: usize, column: usize) -> Self {
        Self {
            kind,
            file_name,
            row,
            column,
        }
    }

    pub fn index(&self) -> ASTIndex {
        ASTIndex::new(self.file_name.clone(), self.row, self.column)
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
    NativeBLock(String),
    Bracket(BracketKind, BracketStatus),
    Comment(String),
    MultiLineComment(String),
    EndOfLine,
    KeyWord(KeywordKind),
    Number(String),
    Punctuation(PunctuationKind),
    StringLiteral(String),
    CharLiteral(char),
    WhiteSpaces(String),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::AlphaNumeric(s) => {
                write!(f, "'{}'", s)
            }
            TokenKind::NativeBLock(_) => {
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
    Extern,
    False,
    Fn,
    Include,
    Inline,
    Let,
    Native,
    Pub,
    Requires,
    Struct,
    True,
    Type,
}

impl KeywordKind {
    pub fn name(&self) -> String {
        match self {
            KeywordKind::Const => "const".into(),
            KeywordKind::Enum => "enum".into(),
            KeywordKind::Extern => "extern".into(),
            KeywordKind::False => "false".into(),
            KeywordKind::Fn => "fn".into(),
            KeywordKind::Include => "include".into(),
            KeywordKind::Inline => "inline".into(),
            KeywordKind::Let => "let".into(),
            KeywordKind::Native => "native".into(),
            KeywordKind::Pub => "pub".into(),
            KeywordKind::Requires => "requires".into(),
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
