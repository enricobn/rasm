use std::fs::File;
use std::io::Read;
use std::path::Path;

use log::debug;
use snailquote::unescape;

use tokens::{
    BracketKind, BracketStatus, KeywordKind, PunctuationKind, ReservedKind, Token, TokenKind,
};

pub mod tokens;

#[derive(Debug, PartialEq)]
enum LexStatus {
    None,
    AlphaNumeric,
    Comment(usize, usize),
    NativeBlock(usize, usize),
    Numeric,
    String,
    StringEscape,
    CharEscape,
    WhiteSpace,
    Char,
}

//#[derive(Clone)]
pub struct Lexer {
    index: usize,
    row: usize,
    column: usize,
    chars: Vec<char>,
    errors: Vec<LexerError>,
}

#[derive(Clone)]
pub struct LexerError {
    pub message: String,
    pub row: usize,
    pub column: usize,
}

impl Iterator for Lexer {
    type Item = (Option<Token>, Vec<LexerError>);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner_next()
    }
}

impl Lexer {
    //pub fn from_read(read: impl Read) -> Result<Iterator<Token>, String> {}

    pub fn from_file(path: &Path) -> Result<Self, String> {
        let mut s = String::new();
        if let Ok(mut file) = File::open(path) {
            if let Ok(size) = file.read_to_string(&mut s) {
                debug!("Reading file {:?}, size {}", path, size);
                Ok(Lexer::new(s))
            } else {
                Err(format!("Cannot read file {:?}", path.to_str()))
            }
        } else {
            Err(format!("Cannot find file {:?}", path.to_str()))
        }
    }

    pub fn new(source: String) -> Self {
        Self {
            index: 0,
            row: 1,
            column: 1,
            chars: source.chars().collect::<Vec<_>>(),
            errors: Vec::new(),
        }
    }

    fn some_token(&mut self, kind: TokenKind) -> Option<(Option<Token>, Vec<LexerError>)> {
        let length = kind.len();

        let result = Some((
            Some(Token::new(kind, self.row, self.column - length)),
            self.errors.clone(),
        ));

        self.errors.clear();

        result
    }

    fn some_token_at(
        &mut self,
        kind: TokenKind,
        row: usize,
        column: usize,
    ) -> Option<(Option<Token>, Vec<LexerError>)> {
        let result = Some((Some(Token::new(kind, row, column)), self.errors.clone()));

        self.errors.clear();

        result
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
            "!" => Some(TokenKind::Punctuation(PunctuationKind::Esclamation)),
            _ => None,
        }
    }

    pub fn process(mut self) -> (Vec<Token>, Vec<LexerError>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        loop {
            match self.next() {
                None => break,
                Some((token_o, es)) => {
                    if let Some(token) = token_o {
                        tokens.push(token);
                    }
                    errors.extend(es);
                }
            }
        }

        (tokens, errors)
    }

    fn add_error(&mut self, error_message: String) {
        self.errors.push(LexerError {
            message: error_message,
            row: self.row,
            column: self.column,
        });
    }
}

const END_OF_FILE: char = '\u{0}';

impl Lexer {
    fn inner_next(&mut self) -> Option<(Option<Token>, Vec<LexerError>)> {
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
                        let row = self.row;
                        let column = self.column - 2;
                        // TODO empty one line comment
                        if c == '\n' {
                            self.row += 1;
                            self.column = 0;
                        }
                        actual.push(c);

                        status = LexStatus::Comment(row, column);
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
                        status = LexStatus::NativeBlock(self.row, self.column - 1);
                    } else if let Some(punctuation) = self.get_punctuation(&actual, c) {
                        let token = self.some_token(punctuation);
                        self.column += 1;
                        self.index += 1;
                        return token;
                    } else if let Some(bracket) = Lexer::get_bracket(c) {
                        if !actual.is_empty() {
                            self.add_error(format!("Bracket, but actual={}", actual));
                        }
                        let token = self.some_token_at(bracket, self.row, self.column);
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
                        self.add_error(format!("unknown char '{}' ({})", c, c.escape_debug(),));
                    }
                }
                LexStatus::WhiteSpace => {
                    if c != '\n' && c.is_whitespace() {
                        actual.push(c);
                    } else {
                        //self.chars.next_back();
                        if actual.chars().any(|it| !it.is_whitespace()) {
                            self.add_error(format!("invalid chars in {actual}"));
                        }
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
                    if c == '\\' {
                        status = LexStatus::CharEscape;
                    } else if c == '\'' {
                        let unescaped =
                            unescape(&format!("\"{}\"", actual.replace("\"", "\\\""))).unwrap();
                        if unescaped.chars().count() != 1 {
                            self.add_error("bad char literal".to_string())
                        }
                        let token = self.some_token(TokenKind::CharLiteral(actual));
                        self.index += 1;
                        self.column += 1;
                        return token;
                    } else {
                        actual.push(c);
                    }
                }
                LexStatus::CharEscape => {
                    if c == '\'' {
                        actual.push(c);
                    } else {
                        actual.push('\\');
                        actual.push(c);
                    }
                    status = LexStatus::Char;
                }
                LexStatus::AlphaNumeric => {
                    if c.is_alphanumeric() {
                        actual.push(c);
                    } else if let Some(keyword) = KeywordKind::from_name(&actual) {
                        //self.chars.next_back();
                        return self.some_token(keyword);
                    } else if let Some(keyword) = ReservedKind::from_name(&actual) {
                        //self.chars.next_back();
                        return self.some_token(keyword);
                    } else {
                        //self.chars.next_back();
                        if actual.chars().any(|it| !it.is_alphanumeric()) {
                            self.add_error(format!("invalid chars in {actual}"));
                        }
                        return self.some_token(TokenKind::AlphaNumeric(actual));
                    }
                }
                LexStatus::Numeric => {
                    if c.is_numeric() {
                        actual.push(c);
                    } else {
                        //self.chars.next_back();
                        if actual.chars().any(|it| !it.is_numeric() && it != '-') {
                            self.add_error(format!("invalid chars in {actual}"));
                        }
                        return self.some_token(TokenKind::Number(actual));
                    }
                }
                LexStatus::Comment(row, column) => {
                    if actual.starts_with("//") {
                        if c == '\n' || c == END_OF_FILE {
                            let token = self.some_token_at(TokenKind::Comment(actual), row, column);
                            self.index += 1; // I remove the end of line, is it right?
                            self.column = 1;
                            self.row += 1;
                            return token;
                        } else {
                            actual.push(c);
                        }
                    } else if actual.starts_with("/*") {
                        if actual.ends_with("*/") {
                            //self.chars.next_back();
                            return self.some_token_at(
                                TokenKind::MultiLineComment(actual),
                                row,
                                column,
                            );
                        } else if c == '\n' {
                            self.row += 1;
                            self.column = 0;
                            actual.push(c);
                        } else {
                            actual.push(c);
                        }
                    }
                }
                LexStatus::NativeBlock(row, column) => {
                    if actual.ends_with("}/") {
                        let token = self.some_token_at(
                            TokenKind::NativeBlock(actual.split_at(actual.len() - 2).0.into()),
                            row,
                            column,
                        );
                        //self.chars.next_back();
                        return token;
                    } else if c == '\n' {
                        self.row += 1;
                        self.column = 0;
                        if !actual.is_empty() {
                            actual.push(c);
                        }
                    } else {
                        actual.push(c);
                    }
                }
            }

            self.index += 1;
            self.column += 1;
        }

        if !actual.is_empty() {
            self.add_error(format!("Unexpected chunk: {}", actual));
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
                Reserved(ReservedKind::I32),
                Punctuation(Comma),
                WhiteSpaces(" ".into()),
                AlphaNumeric("b".into()),
                Punctuation(Colon),
                WhiteSpaces(" ".into()),
                Reserved(ReservedKind::I32),
                Bracket(Round, Close),
                WhiteSpaces(" ".into()),
                Punctuation(RightArrow),
                WhiteSpaces(" ".into()),
                Reserved(ReservedKind::I32),
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
            .filter(|it| matches!(it, TokenKind::NativeBlock(_)))
            .collect();
        assert_eq!(
            vec![
                NativeBlock("    add assembler code\n".into()),
                NativeBlock("    sub assembler code\n".into()),
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
                MultiLineComment("/*\n   A multi\n   line comment\n */".into()),
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
                Reserved(ReservedKind::STR),
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
        assert_eq!(tokens.get(3).unwrap().position.row, 2);
    }

    #[test]
    fn test15() {
        let lexer = Lexer::from_file(Path::new("resources/test/test15.rasm")).unwrap();

        let (_tokens, errors) = lexer.process();

        assert!(errors.is_empty());
    }

    #[test]
    fn test18() {
        let lexer = Lexer::from_file(Path::new("resources/test/test18.rasm")).unwrap();

        let (_tokens, errors) = lexer.process();

        assert!(errors.is_empty());
    }

    #[test]
    fn test_invalid_chars() {
        let lexer = Lexer::new("let a = f.len - 1;".to_string());

        let (_tokens, errors) = lexer.process();

        assert!(!errors.is_empty());
    }

    #[test]
    fn test_invalid_chars_1() {
        let lexer = Lexer::new("let a = f.len /1;".to_string());

        let (_tokens, errors) = lexer.process();

        assert!(!errors.is_empty());
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
