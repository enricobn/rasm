use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug, PartialEq)]
pub enum Token {
    WhiteSpaces(String),
    EndOfLine,
    Number(String),
    StringLiteral(String),
    Punctuation(PunctuationKind),
    Bracket(BracketKind, BracketStatus),
    AlphaNumeric(String),
    Comment(String),
    KeyWord(String)
}

#[derive(Debug, PartialEq)]
pub enum PunctuationKind {
    Dot,
    Comma,
    Colon,
    SemiColon,
}

#[derive(Debug, PartialEq)]
pub enum BracketKind {
    Round,
    Square,
    Brace,
    Angle,
}

#[derive(Debug, PartialEq)]
pub enum BracketStatus {
    Open,
    Close,
}

#[derive(PartialEq)]
enum LexStatus {
    None,
    WhiteSpace,
    String,
    AlphaNumeric,
    Numeric,
    Comment,
}

pub struct Lexer {
    source: String,
    index: usize,
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
        Self { source, index: 0 }
    }
}

const END_OF_FILE: char = '\u{0}';
const KEYWORDS: [&str; 1] = ["fn"];

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
                    if c == '\n' {
                        self.index += 1;
                        return Some(Token::EndOfLine);
                    } else if let Some(punctuation) = Lexer::get_punctuation(c) {
                        self.index += 1;
                        return Some(punctuation);
                    } else if let Some(bracket) = Lexer::get_bracket(c) {
                        self.index += 1;
                        return Some(bracket);
                    } else if c == '/' {
                        actual.push(c);
                        if actual == "//" {
                            status = LexStatus::Comment;
                        }
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
                    }
                }
                LexStatus::WhiteSpace => {
                    if c.is_whitespace() {
                        actual.push(c);
                    } else {
                        return Some(Token::WhiteSpaces(actual));
                    }
                }
                LexStatus::String => {
                    if c == '"' {
                        self.index += 1;
                        return Some(Token::StringLiteral(actual));
                    } else {
                        actual.push(c);
                    }
                }
                LexStatus::AlphaNumeric => {
                    if c.is_alphanumeric() {
                        actual.push(c);
                    } else {
                        if KEYWORDS.iter().any(|it| it == &actual) {
                            return Some(Token::KeyWord(actual));
                        } else {
                            return Some(Token::AlphaNumeric(actual));
                        }
                    }
                }
                LexStatus::Numeric => {
                    if c.is_numeric() || c == '.' {
                        actual.push(c);
                    } else {
                        return Some(Token::Number(actual));
                    }
                }
                LexStatus::Comment => {
                    if c == '\n' || c == END_OF_FILE {
                        self.index += 1; // I remove the end of line, is it right?
                        return Some(Token::Comment(actual));
                    } else {
                        actual.push(c);
                    }
                }
            }

            self.index += 1;
        }

        if actual.len() > 0 {
            println!("Do you have missed something? {}", actual);
        }

        None
    }
}

impl Lexer {
    fn get_bracket(c: char) -> Option<Token> {
        match c {
            '(' => { Some(Token::Bracket(BracketKind::Round, BracketStatus::Open)) }
            ')' => { Some(Token::Bracket(BracketKind::Round, BracketStatus::Close)) }
            '[' => { Some(Token::Bracket(BracketKind::Square, BracketStatus::Open)) }
            ']' => { Some(Token::Bracket(BracketKind::Square, BracketStatus::Close)) }
            '{' => { Some(Token::Bracket(BracketKind::Brace, BracketStatus::Open)) }
            '}' => { Some(Token::Bracket(BracketKind::Brace, BracketStatus::Close)) }
            '<' => { Some(Token::Bracket(BracketKind::Angle, BracketStatus::Open)) }
            '>' => { Some(Token::Bracket(BracketKind::Angle, BracketStatus::Close)) }
            _ => None
        }
    }

    fn get_punctuation(c: char) -> Option<Token> {
        match c {
            '.' => { Some(Token::Punctuation(PunctuationKind::Dot)) }
            ',' => { Some(Token::Punctuation(PunctuationKind::Comma)) }
            ':' => { Some(Token::Punctuation(PunctuationKind::Colon)) }
            ';' => { Some(Token::Punctuation(PunctuationKind::SemiColon)) }
            _ => None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::BracketKind::*;
    use crate::lexer::BracketStatus::*;
    use crate::lexer::PunctuationKind::{Colon, Comma};
    use crate::lexer::Token::*;

    use super::*;

    #[test]
    fn test1() {
        let lexer = Lexer::from_file(Path::new("resources/test/test1.rasm")).unwrap();
        let lst: Vec<Token> = lexer.collect();
        assert_eq!(vec![Comment("// test1.rasm file".into())], lst);
    }

    #[test]
    fn test2() {
        let lexer = Lexer::from_file(Path::new("resources/test/test2.rasm")).unwrap();
        let lst: Vec<Token> = lexer.collect();
        assert_eq!(vec![Comment("// test2.rasm file".into()),
                        AlphaNumeric("println".into()),
                        Bracket(Round, Open),
                        StringLiteral("hello world".into()),
                        Bracket(Round, Close)], lst);
    }

    #[test]
    fn test3() {
        let lexer = Lexer::from_file(Path::new("resources/test/test3.rasm")).unwrap();
        let lst: Vec<Token> = lexer.collect();
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
        let lst: Vec<Token> = lexer.collect();
        assert_eq!(vec![Comment("// test4.rasm file".into()),
                        KeyWord("fn".into()),
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
}

