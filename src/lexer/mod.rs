pub(crate) mod tokens;

use std::fs::File;
use std::io::Read;
use std::path::Path;
use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind};

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
            if let Ok(size) = file.read_to_string(&mut s) {
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
        Some(Token::new(kind, self.row, self.column))
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

    fn get_punctuation(actual: &str, c: char) -> Option<TokenKind> {
        let mut string: String = actual.into();
        string.push(c);

        let s = string.as_str();

        match s {
            "." => Some(TokenKind::Punctuation(PunctuationKind::Dot)),
            "," => Some(TokenKind::Punctuation(PunctuationKind::Comma)),
            ":" => Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            ";" => Some(TokenKind::Punctuation(PunctuationKind::SemiColon)),
            "&" => Some(TokenKind::Punctuation(PunctuationKind::And)),
            "->" => Some(TokenKind::Punctuation(PunctuationKind::RightArrow)),
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
            // TODO optimize, I don't like that
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

            //println!("status {:?}, actual <{}>, c <{}>", status, actual, c);

            match status {
                LexStatus::None => {
                    if actual == "//" || actual == "/*" {
                        actual.push(c);
                        status = LexStatus::Comment;
                    } else if c == '\n' {
                        let token = self.some_token(TokenKind::EndOfLine);
                        self.index += 1;
                        self.column = 1;
                        self.row += 1;
                        return token;
                    } else if c == '/' || c == '*' || c == '-' {
                        actual.push(c);
                    } else if actual == "/" && c == '{' {
                        actual.clear();
                        status = LexStatus::AsmBlock;
                    } else if let Some(punctuation) = Lexer::get_punctuation(&actual, c) {
                        let token = self.some_token(punctuation);
                        self.column += 1;
                        self.index += 1;
                        return token;
                    } else if let Some(bracket) = Lexer::get_bracket(c) {
                        if !actual.is_empty() {
                            // TODO handle error?
                            panic!("Bracket, but actual={}", actual);
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
                    } else if c != END_OF_FILE {
                        println!("WARNING: unknown char '{}' ({}) at {},{} ***", c, c.escape_debug(), self.row, self.column);
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
                    } else if let Some(keyword) = KeywordKind::from_name(&actual) {
                        return self.some_token(keyword);
                    } else {
                        return self.some_token(TokenKind::AlphaNumeric(actual));
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

        if !actual.is_empty() {
            println!("Do you have missed something? {}", actual);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::tokens::BracketKind::*;
    use crate::lexer::tokens::BracketStatus::*;
    use crate::lexer::tokens::PunctuationKind::*;
    use crate::lexer::tokens::TokenKind::*;

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
                        Bracket(Round, Close),
                        Punctuation(SemiColon)], lst);
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
                        Punctuation(RightArrow),
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
            matches!(it, TokenKind::AsmBLock(_))
        }).collect();
        assert_eq!(vec![AsmBLock("\n    add assembler code\n".into()), AsmBLock("\n    sub assembler code\n".into())], lst);
    }

    #[test]
    fn test6() {
        let lexer = Lexer::from_file(Path::new("resources/test/test6.rasm")).unwrap();
        let lst: Vec<TokenKind> = lexer.map(|it| it.kind).collect();
        assert_eq!(vec![Comment("// test6.rasm file".into()), Comment("/*\n   A multi\n   line comment\n */".into())], lst);
    }

    #[test]
    fn test7() {
        let lexer = Lexer::from_file(Path::new("resources/test/test7.rasm")).unwrap();
        let lst: Vec<TokenKind> = lexer.map(|it| it.kind).collect();
        assert_eq!(vec![Comment("// test7.rasm file".into()), KeyWord(KeywordKind::Fn),
                        WhiteSpaces(" ".into()), AlphaNumeric("add".into()), Bracket(Round, Open),
                        AlphaNumeric("s".into()), Punctuation(Colon), WhiteSpaces(" ".into()),
                        Punctuation(And),
                        AlphaNumeric("str".into()), Bracket(Round, Close),
                        WhiteSpaces(" ".into()), Bracket(Brace, Open), Bracket(Brace, Close)], lst);
    }

    /*
    TODO
    #[test]
    fn test_ro_col() {
        let path = Path::new("resources/test/helloworld.rasm");
        let lexer = Lexer::from_file(path).unwrap();

        let mut s = String::new();
        File::open(path).unwrap().read_to_string(&mut s).unwrap();

        let lst: Vec<TokenKind> = lexer
            .filter(|it| it.)
        assert_eq!(vec![Comment("// test6.rasm file".into()), Comment("/*\n   A multi\n   line comment\n */".into())], lst);
    }
     */

}