use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use log::debug;

use crate::errors::{CompilationError, CompilationErrorKind};
use crate::lexer::tokens::{
    BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind,
};
use crate::parser::ast::ASTIndex;

pub mod tokens;

#[derive(Debug, PartialEq)]
enum LexStatus {
    None,
    AlphaNumeric,
    AsmBlock,
    Comment,
    Numeric,
    String,
    StringEscape,
    WhiteSpace,
    Char,
}

//#[derive(Clone)]
pub struct Lexer {
    file_name: Option<PathBuf>,
    index: usize,
    row: usize,
    column: usize,
    chars: Vec<char>,
    errors: Vec<CompilationError>,
}

impl Lexer {
    pub fn from_file(path: &Path) -> Result<Self, String> {
        let mut s = String::new();
        if let Ok(mut file) = File::open(path) {
            if let Ok(size) = file.read_to_string(&mut s) {
                debug!("Reading file {:?}, size {}", path, size);
                Ok(Lexer::new(s, Some(path.to_path_buf())))
            } else {
                Err(format!("Cannot read file {:?}", path.to_str()))
            }
        } else {
            Err(format!("Cannot find file {:?}", path.to_str()))
        }
    }

    pub fn new(source: String, file_name: Option<PathBuf>) -> Self {
        Self {
            file_name,
            index: 0,
            row: 1,
            column: 1,
            chars: source.chars().collect::<Vec<_>>(),
            errors: Vec::new(),
        }
    }

    fn some_token(&self, kind: TokenKind) -> Option<Token> {
        Some(Token::new(
            kind,
            self.file_name.clone(),
            self.row,
            self.column,
        ))
    }

    fn get_bracket(c: char) -> Option<TokenKind> {
        match c {
            '(' => Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)),
            ')' => Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close)),
            '[' => Some(TokenKind::Bracket(BracketKind::Square, BracketStatus::Open)),
            ']' => Some(TokenKind::Bracket(
                BracketKind::Square,
                BracketStatus::Close,
            )),
            '{' => Some(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)),
            '}' => Some(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close)),
            '<' => Some(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open)),
            '>' => Some(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close)),
            _ => None,
        }
    }

    fn get_punctuation(&self, actual: &str, c: char) -> Option<TokenKind> {
        let mut string: String = actual.into();
        string.push(c);

        let s = string.as_str();

        match s {
            "." => Some(TokenKind::Punctuation(PunctuationKind::Dot)),
            "," => Some(TokenKind::Punctuation(PunctuationKind::Comma)),
            ":" => Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            ";" => Some(TokenKind::Punctuation(PunctuationKind::SemiColon)),
            "->" => Some(TokenKind::Punctuation(PunctuationKind::RightArrow)),
            "=" => Some(TokenKind::Punctuation(PunctuationKind::Equal)),
            _ => None,
        }
    }

    fn get_index(&self) -> ASTIndex {
        ASTIndex::new(self.file_name.clone(), self.row, self.column)
    }

    pub fn process(mut self) -> (Vec<Token>, Vec<CompilationError>) {
        let mut tokens = Vec::new();
        loop {
            match self.next() {
                None => break,
                Some(token) => {
                    tokens.push(token);
                }
            }
        }

        (tokens, self.errors)
    }

    fn add_error(&mut self, error_message: String) {
        self.errors.push(CompilationError {
            index: self.get_index(),
            error_kind: CompilationErrorKind::Lexer(error_message),
        });
    }
}

const END_OF_FILE: char = '\u{0}';

impl Lexer {
    fn next(&mut self) -> Option<Token> {
        let mut actual = String::new();
        let mut status = LexStatus::None;
        let mut exit = false;
        loop {
            let c = if let Some(a_char) = self.chars.get(self.index) {
                *a_char
            } else if exit {
                break;
            } else {
                exit = true;
                END_OF_FILE
            };

            //debug!("status {:?}, actual <{}>, c <{}>", status, actual, c);

            match status {
                LexStatus::None => {
                    if actual == "//" || actual == "/*" {
                        // TODO empty one line comment
                        if c == '\n' {
                            self.row += 1;
                            self.column = 0;
                        } else {
                            actual.push(c);
                        }
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
                    } else if let Some(punctuation) = self.get_punctuation(&actual, c) {
                        let token = self.some_token(punctuation);
                        self.column += 1;
                        self.index += 1;
                        return token;
                    } else if let Some(bracket) = Lexer::get_bracket(c) {
                        if !actual.is_empty() {
                            self.add_error(format!("Bracket, but actual={}", actual));
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
                    } else if c.is_ascii_digit() {
                        status = LexStatus::Numeric;
                        actual.push(c);
                    } else if c.is_alphanumeric() {
                        status = LexStatus::AlphaNumeric;
                        actual.push(c);
                    } else if c == '\'' {
                        status = LexStatus::Char
                    } else if c != END_OF_FILE {
                        self.add_error(format!(
                            "unknown char '{}' ({}) : {} ***",
                            c,
                            c.escape_debug(),
                            self.get_index()
                        ));
                    }
                }
                LexStatus::WhiteSpace => {
                    if c != '\n' && c.is_whitespace() {
                        actual.push(c);
                    } else {
                        //self.chars.next_back();
                        return self.some_token(TokenKind::WhiteSpaces(actual));
                    }
                }
                LexStatus::String => {
                    if c == '\\' {
                        status = LexStatus::StringEscape;
                    } else if c == '"' {
                        let token = self.some_token(TokenKind::StringLiteral(actual));
                        self.index += 1;
                        self.column += 1;
                        return token;
                    } else {
                        actual.push(c);
                    }
                }
                LexStatus::StringEscape => {
                    if c == '"' {
                        actual.push(c);
                    } else {
                        actual.push('\\');
                        actual.push(c);
                    }
                    status = LexStatus::String;
                }
                LexStatus::Char => {
                    if c == '\'' {
                        //println!("Char literal '{actual}'");
                        let mut actual_char = '?';

                        if actual.chars().count() != 1 {
                            if actual.len() == 2 && actual.starts_with('\\') {
                                if actual == "\\n" {
                                    actual_char = '\n';
                                } else if actual == "\\t" {
                                    actual_char = '\t';
                                } else if actual == "\\\\" {
                                    actual_char = '\\';
                                } else {
                                    self.add_error(format!("Invalid char literal '{actual}'"));
                                };
                            } else {
                                self.add_error(format!("Invalid char literal '{actual}'"));
                            }
                        } else {
                            actual_char = actual.chars().next().unwrap();
                        }
                        let token = self.some_token(TokenKind::CharLiteral(actual_char));
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
                        //self.chars.next_back();
                        return self.some_token(keyword);
                    } else {
                        //self.chars.next_back();
                        return self.some_token(TokenKind::AlphaNumeric(actual));
                    }
                }
                LexStatus::Numeric => {
                    if c.is_numeric() {
                        actual.push(c);
                    } else {
                        //self.chars.next_back();
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
                            if c == '\n' {
                                self.row += 1;
                                self.column = 0;
                            }
                            self.index += 1;
                            //self.chars.next_back();
                            return self.some_token(TokenKind::MultiLineComment(actual));
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
                        let token = self.some_token(TokenKind::AsmBLock(
                            actual.split_at(actual.len() - 2).0.into(),
                        ));
                        //self.chars.next_back();
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
            debug!("Do you missed something? {}", actual);
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
    use crate::utils::SliceDisplay;

    use super::*;

    #[test]
    fn test1() {
        let lexer = Lexer::from_file(Path::new("resources/test/test1.rasm")).unwrap();

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());

        let lst: Vec<TokenKind> = tokens.into_iter().map(|it| it.kind).collect();

        assert_eq!(vec![Comment("// test1.rasm file".into())], lst);
    }

    #[test]
    fn test2() {
        let lexer = Lexer::from_file(Path::new("resources/test/test2.rasm")).unwrap();

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());

        let lst: Vec<TokenKind> = tokens.into_iter().map(|it| it.kind).collect();

        assert_eq!(
            vec![
                Comment("// test2.rasm file".into()),
                AlphaNumeric("println".into()),
                Bracket(Round, Open),
                StringLiteral("hello world".into()),
                Bracket(Round, Close),
                Punctuation(SemiColon),
            ],
            lst
        );
    }

    #[test]
    fn test3() {
        let lexer = Lexer::from_file(Path::new("resources/test/test3.rasm")).unwrap();

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());

        let lst: Vec<TokenKind> = tokens.into_iter().map(|it| it.kind).collect();

        assert_eq!(
            vec![
                Comment("// test3.rasm file".into()),
                AlphaNumeric("println".into()),
                Bracket(Round, Open),
                Number("100".into()),
                Bracket(Round, Close),
                EndOfLine,
                AlphaNumeric("println".into()),
                Bracket(Round, Open),
                Number("100".into()),
                Punctuation(PunctuationKind::Dot),
                Number("123".into()),
                Bracket(Round, Close),
            ],
            lst
        );
    }

    #[test]
    fn test4() {
        let lexer = Lexer::from_file(Path::new("resources/test/test4.rasm")).unwrap();

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());

        let lst: Vec<TokenKind> = tokens.into_iter().map(|it| it.kind).collect();

        assert_eq!(
            vec![
                Comment("// test4.rasm file".into()),
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
                Bracket(Brace, Close),
            ],
            lst
        );
    }

    #[test]
    fn test5() {
        let lexer = Lexer::from_file(Path::new("resources/test/test5.rasm")).unwrap();

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());

        let lst: Vec<TokenKind> = tokens
            .into_iter()
            .map(|it| it.kind)
            .filter(|it| matches!(it, TokenKind::AsmBLock(_)))
            .collect();
        assert_eq!(
            vec![
                AsmBLock("\n    add assembler code\n".into()),
                AsmBLock("\n    sub assembler code\n".into()),
            ],
            lst
        );
    }

    #[test]
    fn test6() {
        let lexer = Lexer::from_file(Path::new("resources/test/test6.rasm")).unwrap();

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());

        let lst: Vec<TokenKind> = tokens.into_iter().map(|it| it.kind).collect();

        assert_eq!(
            vec![
                Comment("// test6.rasm file".into()),
                MultiLineComment("/*   A multi\n   line comment\n */".into()),
            ],
            lst
        );
    }

    #[test]
    fn test7() {
        let lexer = Lexer::from_file(Path::new("resources/test/test7.rasm")).unwrap();

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());

        let lst: Vec<TokenKind> = tokens.into_iter().map(|it| it.kind).collect();

        assert_eq!(
            vec![
                Comment("// test7.rasm file".into()),
                KeyWord(KeywordKind::Fn),
                WhiteSpaces(" ".into()),
                AlphaNumeric("add".into()),
                Bracket(Round, Open),
                AlphaNumeric("s".into()),
                Punctuation(Colon),
                WhiteSpaces(" ".into()),
                AlphaNumeric("str".into()),
                Bracket(Round, Close),
                WhiteSpaces(" ".into()),
                Bracket(Brace, Open),
                Bracket(Brace, Close),
            ],
            lst
        );
    }

    #[test]
    fn test13() {
        let lexer = Lexer::from_file(Path::new("resources/test/test13.rasm")).unwrap();

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());

        //let lst: Vec<TokenKind> = tokens.into_iter().map(|it| it.kind).collect();

        assert_eq!(tokens.get(3).unwrap().kind, KeyWord(KeywordKind::Fn));
        assert_eq!(tokens.get(3).unwrap().row, 2);
    }

    #[test]
    fn test15() {
        let lexer = Lexer::from_file(Path::new("resources/test/test15.rasm")).unwrap();

        let (_tokens, errors) = lexer.process();

        println!("errors {}", SliceDisplay(&errors));

        assert!(errors.is_empty());
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
        assert_eq!(vec![Comment("// test6.rasm file".into()), Comment("/*\n   A multi\n   line comment\n */
    ".into())], lst);
    }
     */
}
