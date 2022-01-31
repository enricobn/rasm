use std::fs::File;
use std::io::Read;
use std::path::Path;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

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

#[derive(Debug, PartialEq)]
enum LexStatus {
    None,
    AlphaNumeric,
    AsmBlock,
    Comment,
    Numeric,
    String,
    WhiteSpace,
}

#[derive(Debug, PartialEq, EnumIter)]
pub enum KeywordKind {
    Asm,
    Fn,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    kind: TokenKind,
    row: usize,
    column: usize
}

impl KeywordKind {
    fn name(&self) -> String {
        match self {
            KeywordKind::Fn => "fn".into(),
            KeywordKind::Asm => "asm".into()
        }
    }

    fn from_name(name: &str) -> Option<TokenKind> {
        KeywordKind::iter().find(|it| it.name() == name).map(|it| TokenKind::KeyWord(it))
    }
}

pub struct Lexer {
    source: String,
    index: usize,
    row: usize,
    column: usize
}

impl Lexer {

    pub fn from_file(path: &Path) -> Result<Self, String> {
        let mut s = String::new();
        if let Ok(mut file) = File::open(path) {
            if let Result::Ok(size) = file.read_to_string(&mut s) {
                println!("Reading file {:?}, size {}", path, size);
                Ok(Lexer::new(s))
            } else {
                Err(format!("Cannot read {:?}", path.to_str()))
            }
        } else {
            Err(format!("Cannot find {:?}", path.to_str()))
        }
    }

    pub fn new(source: String) -> Self {
        Self { source, index: 0, row: 1, column: 1 }
    }

    fn some_token(&self, kind: TokenKind) -> Option<Token> {
        Some(Token{ kind, row: self.row, column: self.column})
    }

    fn get_bracket(c: char) -> Option<TokenKind> {
        match c {
            '(' => Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)),
            ')' => Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close)),
            '[' => Some(TokenKind::Bracket(BracketKind::Square, BracketStatus::Open)),
            ']' => Some(TokenKind::Bracket(BracketKind::Square, BracketStatus::Close)),
            '{' => Some(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)),
            '}' => Some(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close)),
            '<' => Some(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open)),
            '>' => Some(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close)),
            _ => None
        }
    }

    fn get_punctuation(c: char) -> Option<TokenKind> {
        match c {
            '.' => Some(TokenKind::Punctuation(PunctuationKind::Dot)),
            ',' => Some(TokenKind::Punctuation(PunctuationKind::Comma)),
            ':' => Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            ';' => Some(TokenKind::Punctuation(PunctuationKind::SemiColon)),
            _ => None
        }
    }

}

const END_OF_FILE: char = '\u{0}';

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut actual = String::new();
        let mut chars = self.source.chars();
        if self.index > 0 {
            chars.nth(self.index - 1);
        }
        let mut status = LexStatus::None;
        let mut exit = false;
        loop {
            //println!("status {:?}, actual <{}>, /* {}", status, actual, actual == "/*");
            let c =
                if let Some(a_char) = chars.next() {
                    a_char
                } else {
                    if exit {
                        break;
                    } else {
                        exit = true;
                        END_OF_FILE
                    }
                };

            match status {
                LexStatus::None => {
                    if actual == "//" || actual == "/*" {
                        actual.push(c);
                        status = LexStatus::Comment;
                        println!("*** start comment at {},{}", self.row, self.column);
                    } else if c == '\n' {
                        let token = self.some_token(TokenKind::EndOfLine);
                        self.index += 1;
                        self.column = 1;
                        self.row += 1;
                        return token;
                    } else if c == '/' || c == '*' {
                        actual.push(c);
                    } else if actual == "/" && c == '{' {
                        actual.clear();
                        status = LexStatus::AsmBlock;
                    } else if let Some(punctuation) = Lexer::get_punctuation(c) {
                        if !actual.is_empty() {
                            panic!("Punctuation, but actual = {}", actual);
                        }
                        let token = self.some_token(punctuation);
                        self.column += 1;
                        self.index += 1;
                        return token;
                    } else if let Some(bracket) = Lexer::get_bracket(c) {
                        if !actual.is_empty() {
                            panic!("Bracket, but actual = {}", actual);
                        }
                        let token = self.some_token(bracket);
                        self.column += 1;
                        self.index += 1;
                        return token;
                    } else if c.is_whitespace() {
                        status = LexStatus::WhiteSpace;
                        actual.push(c);
                    } else if c == '"' {
                        status = LexStatus::String;
                    } else if c.is_digit(10) {
                        status = LexStatus::Numeric;
                        actual.push(c);
                    } else if c.is_alphanumeric() {
                        status = LexStatus::AlphaNumeric;
                        actual.push(c);
                    } else {
                        println!("*** Unknown char '{}' at {},{}", c, self.row, self.column);
                    }
                }
                LexStatus::WhiteSpace => {
                    if c.is_whitespace() {
                        actual.push(c);
                    } else {
                        return self.some_token(TokenKind::WhiteSpaces(actual));
                    }
                }
                LexStatus::String => {
                    if c == '"' {
                        let token = self.some_token(TokenKind::StringLiteral(actual));
                        self.index += 1;
                        self.column += 1;
                        return token;
                    } else {
                        actual.push(c);
                    }
                }
                LexStatus::AlphaNumeric => {
                    if c.is_alphanumeric() {
                        actual.push(c);
                    } else {
                        if let Some(keyword) = KeywordKind::from_name(&actual) {
                            return self.some_token(keyword);
                        } else {
                            return self.some_token(TokenKind::AlphaNumeric(actual));
                        }
                    }
                }
                LexStatus::Numeric => {
                    if c.is_numeric() || c == '.' {
                        actual.push(c);
                    } else {
                        return self.some_token(TokenKind::Number(actual));
                    }
                }
                LexStatus::Comment => {
                    if actual.starts_with("//") {
                        if c == '\n' || c == END_OF_FILE {
                            let token = self.some_token(TokenKind::Comment(actual));
                            self.index += 1; // I remove the end of line, is it right?
                            self.column = 1;
                            self.row += 1;
                            return token;
                        } else {
                            actual.push(c);
                        }
                    } else if actual.starts_with("/*") {
                        if actual.ends_with("*/") {
                            let token = self.some_token(TokenKind::Comment(actual));
                            return token;
                        } else if c == '\n' {
                            self.row += 1;
                            self.column = 0;
                            actual.push(c);
                        } else {
                            actual.push(c);
                        }
                    }
                }
                LexStatus::AsmBlock => {
                    if actual.ends_with("}/") {
                        let token = self.some_token(TokenKind::AsmBLock(actual.split_at(actual.len() - 2).0.into()));
                        self.index += 1;
                        self.column += 1;
                        return token;
                    } else if c == '\n' {
                            self.row += 1;
                            self.column = 0;
                            actual.push(c);
                    } else {
                        actual.push(c);
                    }
                }
            }

            self.index += 1;
            self.column += 1;
        }

        if actual.len() > 0 {
            println!("Do you have missed something? {}", actual);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::BracketKind::*;
    use crate::lexer::BracketStatus::*;
    use crate::lexer::PunctuationKind::{Colon, Comma};
    use crate::lexer::TokenKind::*;

    use super::*;

    #[test]
    fn test1() {
        let lexer = Lexer::from_file(Path::new("resources/test/test1.rasm")).unwrap();
        let lst: Vec<TokenKind> = lexer.map(|it| it.kind).collect();
        assert_eq!(vec![Comment("// test1.rasm file".into())], lst);
    }

    #[test]
    fn test2() {
        let lexer = Lexer::from_file(Path::new("resources/test/test2.rasm")).unwrap();
        let lst: Vec<TokenKind> = lexer.map(|it| it.kind).collect();
        assert_eq!(vec![Comment("// test2.rasm file".into()),
                        AlphaNumeric("println".into()),
                        Bracket(Round, Open),
                        StringLiteral("hello world".into()),
                        Bracket(Round, Close)], lst);
    }

    #[test]
    fn test3() {
        let lexer = Lexer::from_file(Path::new("resources/test/test3.rasm")).unwrap();
        let lst: Vec<TokenKind> = lexer.map(|it| it.kind).collect();
        assert_eq!(vec![Comment("// test3.rasm file".into()),
                        AlphaNumeric("println".into()),
                        Bracket(Round, Open),
                        Number("100".into()),
                        Bracket(Round, Close), EndOfLine,
                        AlphaNumeric("println".into()),
                        Bracket(Round, Open),
                        Number("100.123".into()),
                        Bracket(Round, Close)], lst);
    }

    #[test]
    fn test4() {
        let lexer = Lexer::from_file(Path::new("resources/test/test4.rasm")).unwrap();
        let lst: Vec<TokenKind> = lexer.map(|it| it.kind).collect();
        assert_eq!(vec![Comment("// test4.rasm file".into()),
                        KeyWord(KeywordKind::Fn),
                        WhiteSpaces(" ".into()),
                        AlphaNumeric("add".into()),
                        Bracket(Round, Open),
                        AlphaNumeric("a".into()),
                        Punctuation(Colon),
                        WhiteSpaces(" ".into()),
                        AlphaNumeric("i32".into()),
                        Punctuation(Comma),
                        WhiteSpaces(" ".into()),
                        AlphaNumeric("b".into()),
                        Punctuation(Colon),
                        WhiteSpaces(" ".into()),
                        AlphaNumeric("i32".into()),
                        Bracket(Round, Close),
                        WhiteSpaces(" ".into()),
                        Bracket(Angle, Close),
                        WhiteSpaces(" ".into()),
                        AlphaNumeric("i32".into()),
                        WhiteSpaces(" ".into()),
                        Bracket(Brace, Open),
                        EndOfLine,
                        WhiteSpaces("    ".into()),
                        AlphaNumeric("I32".into()),
                        Punctuation(Colon),
                        Punctuation(Colon),
                        AlphaNumeric("add".into()),
                        Bracket(Round, Open),
                        AlphaNumeric("a".into()),
                        Punctuation(Comma),
                        WhiteSpaces(" ".into()),
                        AlphaNumeric("b".into()),
                        Bracket(Round, Close),
                        EndOfLine,
                        Bracket(Brace, Close)], lst);
    }

    #[test]
    fn test5() {
        let lexer = Lexer::from_file(Path::new("resources/test/test5.rasm")).unwrap();
        let lst: Vec<TokenKind> = lexer.map(|it| it.kind).filter(|it| {
            if let TokenKind::AsmBLock(_) = it {
                true
            } else {
                false
            }
        }).collect();
        assert_eq!(vec![AsmBLock("\n    add assembler code\n".into()), AsmBLock("\n    sub assembler code\n".into())], lst);
    }

    #[test]
    fn test6() {
        let lexer = Lexer::from_file(Path::new("resources/test/test6.rasm")).unwrap();
        let lst: Vec<TokenKind> = lexer.map(|it| it.kind).collect();
        assert_eq!(vec![Comment("// test6.rasm file".into()), Comment("/*\n   A multi\n   line comment\n */".into())], lst);
    }
}