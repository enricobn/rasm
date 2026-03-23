use std::fs::File;
use std::io::Read;
use std::path::Path;

use log::debug;

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
    config: LexerConfig,
    index: usize,
    row: usize,
    column: usize,
    chars: Vec<char>,
    errors: Vec<LexerError>,
}

pub struct LexerConfig {
    pub collect_white_spaces: bool,
}

impl Default for LexerConfig {
    fn default() -> Self {
        Self {
            collect_white_spaces: true,
        }
    }
}

#[derive(Clone)]
pub struct LexerError {
    pub message: String,
    pub row: usize,
    pub column: usize,
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner_next()
    }
}

impl Lexer {
    pub fn from_file(path: &Path) -> Result<Self, String> {
        let mut s = String::new();
        let mut file = File::open(path).map_err(|e| e.to_string())?;
        let size = file.read_to_string(&mut s).map_err(|e| e.to_string())?;

        debug!("Reading file {:?}, size {}", path, size);
        Ok(Lexer::new(s))
    }

    pub fn new(source: String) -> Self {
        Self {
            config: LexerConfig::default(),
            index: 0,
            row: 1,
            column: 1,
            chars: source.chars().collect::<Vec<_>>(),
            errors: Vec::new(),
        }
    }

    pub fn process(mut self) -> (Vec<Token>, Vec<LexerError>) {
        let mut tokens = Vec::new();
        loop {
            match self.inner_next() {
                None => break,
                Some(token) => {
                    tokens.push(token);
                }
            }
        }

        (tokens, self.errors)
    }

    pub fn errors(self) -> Vec<LexerError> {
        self.errors
    }

    fn create_token_at_current_position(&mut self, kind: TokenKind) -> Option<Token> {
        let length = kind.len();

        if self.column < length {
            println!("Lexer internal error");
            self.add_error("Lexer internal error".to_owned());
            return None;
        }

        Some(Token::new(kind, self.row, self.column - length))
    }

    #[inline]
    fn create_token_at(&mut self, kind: TokenKind, row: usize, column: usize) -> Option<Token> {
        Some(Token::new(kind, row, column))
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
        match (actual, c) {
            ("", '@') => Some(TokenKind::Punctuation(PunctuationKind::At)),
            ("", '.') => Some(TokenKind::Punctuation(PunctuationKind::Dot)),
            ("", ',') => Some(TokenKind::Punctuation(PunctuationKind::Comma)),
            ("", ':') => Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            ("", ';') => Some(TokenKind::Punctuation(PunctuationKind::SemiColon)),
            ("-", '>') => Some(TokenKind::Punctuation(PunctuationKind::RightArrow)),
            ("", '=') => Some(TokenKind::Punctuation(PunctuationKind::Equal)),
            ("", '!') => Some(TokenKind::Punctuation(PunctuationKind::Esclamation)),
            _ => None,
        }
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
    fn inner_next(&mut self) -> Option<Token> {
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
                    if actual == "/" && (c == '/' || c == '*') {
                        let row = self.row;
                        let column = self.column - 1;
                        actual.push(c);

                        status = LexStatus::Comment(row, column);
                    } else if c == '\n' {
                        let token = self.create_token_at_current_position(TokenKind::EndOfLine);
                        self.index += 1;
                        self.column = 1;
                        self.row += 1;
                        return token;
                    } else if c == '/' || c == '-' {
                        actual.push(c);
                    } else if actual == "/" && c == '{' {
                        actual.clear();
                        status = LexStatus::NativeBlock(self.row, self.column - 1);
                    } else if let Some(punctuation) = self.get_punctuation(&actual, c) {
                        let token = self.create_token_at_current_position(punctuation);
                        self.column += 1;
                        self.index += 1;
                        return token;
                    } else if let Some(bracket) = Lexer::get_bracket(c) {
                        if !actual.is_empty() {
                            self.add_error(format!("Bracket, but actual={}", actual));
                        }
                        let token = self.create_token_at(bracket, self.row, self.column);
                        self.column += 1;
                        self.index += 1;
                        return token;
                    } else if c.is_whitespace() {
                        if !actual.is_empty() {
                            self.add_error(format!("Whitespace, but actual={}", actual));
                        }
                        status = LexStatus::WhiteSpace;
                        if self.config.collect_white_spaces {
                            actual.push(c);
                        }
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
                    if self.config.collect_white_spaces {
                        if c != '\n' && c.is_whitespace() {
                            actual.push(c);
                        } else {
                            if actual.chars().any(|it| !it.is_whitespace()) {
                                self.add_error(format!("invalid chars in {actual}"));
                            }
                            return self
                                .create_token_at_current_position(TokenKind::WhiteSpaces(actual));
                        }
                    } else {
                        if c == '\n' || !c.is_whitespace() {
                            status = LexStatus::None;
                            continue;
                        }
                    }
                }
                LexStatus::String => {
                    if c == '\\' {
                        status = LexStatus::StringEscape;
                    } else if c == '"' {
                        let token =
                            self.create_token_at_current_position(TokenKind::StringLiteral(actual));
                        self.index += 1;
                        self.column += 1;
                        return token;
                    } else {
                        actual.push(c);
                        if c == '\n' {
                            self.add_error(format!("String literals must be on one line."));
                        }
                    }
                }
                LexStatus::StringEscape => {
                    self.unescape(&mut actual, c);
                    status = LexStatus::String;
                }
                LexStatus::Char => {
                    if c == '\\' {
                        status = LexStatus::CharEscape;
                    } else if c == '\'' {
                        let token =
                            self.create_token_at_current_position(TokenKind::CharLiteral(actual));
                        self.index += 1;
                        self.column += 1;
                        return token;
                    } else {
                        actual.push(c);
                    }
                }
                LexStatus::CharEscape => {
                    self.unescape(&mut actual, c);
                    status = LexStatus::Char;
                }
                LexStatus::AlphaNumeric => {
                    if c.is_alphanumeric() {
                        actual.push(c);
                    } else if let Some(keyword) = KeywordKind::from_name(&actual) {
                        //self.chars.next_back();
                        return self.create_token_at_current_position(keyword);
                    } else if let Some(keyword) = ReservedKind::from_name(&actual) {
                        //self.chars.next_back();
                        return self.create_token_at_current_position(keyword);
                    } else {
                        //self.chars.next_back();
                        if actual.chars().any(|it| !it.is_alphanumeric()) {
                            self.add_error(format!("invalid chars in {actual}"));
                        }
                        return self
                            .create_token_at_current_position(TokenKind::AlphaNumeric(actual));
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
                        return self.create_token_at_current_position(TokenKind::Number(actual));
                    }
                }
                LexStatus::Comment(row, column) => {
                    if actual.starts_with("//") {
                        if c == '\n' || c == END_OF_FILE {
                            let token =
                                self.create_token_at(TokenKind::Comment(actual), row, column);
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
                            return self.create_token_at(
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
                        let token = self.create_token_at(
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

    fn unescape(&mut self, actual: &mut String, c: char) {
        if c == 'n' {
            actual.push('\n');
        } else if c == 'r' {
            actual.push('\r');
        } else if c == 't' {
            actual.push('\t');
        } else if c.is_ascii_digit() {
            let mut escape_sequence = c.to_string();

            while self
                .chars
                .get(self.index + 1)
                .map(|it| it.is_ascii_digit())
                .unwrap_or(false)
            {
                escape_sequence.push(self.chars[self.index + 1]);
                self.index += 1;
                self.column += 1;
            }

            let value = u32::from_str_radix(&escape_sequence, 8)
                .ok()
                .and_then(|it| char::from_u32(it))
                .unwrap_or('?');
            actual.push(value);
        } else {
            actual.push(c);
        }
    }
}

#[cfg(test)]
mod tests {
    use rasm_utils::SliceDisplay;

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
                AlphaNumeric("add".into()),
                Bracket(Round, Open),
                AlphaNumeric("a".into()),
                Punctuation(Colon),
                Reserved(ReservedKind::INT),
                Punctuation(Comma),
                AlphaNumeric("b".into()),
                Punctuation(Colon),
                Reserved(ReservedKind::INT),
                Bracket(Round, Close),
                Punctuation(RightArrow),
                Reserved(ReservedKind::INT),
                Bracket(Brace, Open),
                EndOfLine,
                AlphaNumeric("AType".into()),
                Punctuation(Colon),
                Punctuation(Colon),
                AlphaNumeric("add".into()),
                Bracket(Round, Open),
                AlphaNumeric("a".into()),
                Punctuation(Comma),
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
                AlphaNumeric("add".into()),
                Bracket(Round, Open),
                AlphaNumeric("s".into()),
                Punctuation(Colon),
                Reserved(ReservedKind::STR),
                Bracket(Round, Close),
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

        assert_eq!(tokens.get(2).unwrap().kind, KeyWord(KeywordKind::Fn));
        assert_eq!(tokens.get(2).unwrap().position.row, 2);
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

        let (tokens, errors) = lexer.process();
        for token in tokens {
            println!("{:?}", token);
        }

        assert!(!errors.is_empty());
    }

    #[test]
    fn test_invalid_chars_1() {
        let lexer = Lexer::new("let a = f.len /1;".to_string());

        let (_tokens, errors) = lexer.process();

        assert!(!errors.is_empty());
    }

    #[test]
    fn test_invalid_chars_2() {
        let lexer = Lexer::new("let a = f.len - > 1;".to_string());

        let (tokens, errors) = lexer.process();

        for token in tokens {
            println!("{:?}", token);
        }

        assert!(!errors.is_empty());
    }

    #[test]
    fn test_empty_single_line_comment() {
        let lexer = Lexer::new("//\n//\n".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(2, tokens.len());

        assert_eq!(Some("//"), token_comment(&tokens[0]));
        assert_eq!(Some("//"), token_comment(&tokens[1]));
    }

    #[test]
    fn test_single_line_comments() {
        let lexer = Lexer::new("//\nlet a = 1;\n".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(7, tokens.len());

        assert_eq!(Some("//"), token_comment(&tokens[0]));
        assert_eq!(
            "comment, Let, 'a', =, 1, ;, EOL",
            format!("{}", SliceDisplay(&tokens))
        );
    }

    #[test]
    fn test_single_line_comments_2() {
        let lexer = Lexer::new("//\n\nlet a = 1;\n".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(8, tokens.len());

        assert_eq!(Some("//"), token_comment(&tokens[0]));
        assert_eq!(
            "comment, EOL, Let, 'a', =, 1, ;, EOL",
            format!("{}", SliceDisplay(&tokens))
        );
    }

    #[test]
    fn test_empty_multi_line_comment() {
        let lexer = Lexer::new("/*\n*/\n".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(2, tokens.len(), "{}", SliceDisplay(&tokens));

        assert_eq!(Some("/*\n*/"), token_multiline_comment(&tokens[0]));
    }

    #[test]
    fn string_escape() {
        let lexer = Lexer::new("\"\\\"\"".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(1, tokens.len(), "{}", SliceDisplay(&tokens));

        if let TokenKind::StringLiteral(s) = &tokens[0].kind {
            assert_eq!("\"", s);
        } else {
            panic!("Expected StringLiteral");
        }
    }

    #[test]
    fn string_escape_1() {
        let lexer = Lexer::new("\"\\\\\"".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(1, tokens.len(), "{}", SliceDisplay(&tokens));

        if let TokenKind::StringLiteral(s) = &tokens[0].kind {
            assert_eq!("\\", s);
        } else {
            panic!("Expected StringLiteral");
        }
    }

    #[test]
    fn string_escape_2() {
        let lexer = Lexer::new("\"\\n\"".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(1, tokens.len(), "{}", SliceDisplay(&tokens));

        if let TokenKind::StringLiteral(s) = &tokens[0].kind {
            assert_eq!("\n", s);
        } else {
            panic!("Expected StringLiteral");
        }
    }

    #[test]
    fn char_escape() {
        let lexer = Lexer::new("'\\\"'".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(1, tokens.len(), "{}", SliceDisplay(&tokens));

        if let TokenKind::CharLiteral(s) = &tokens[0].kind {
            assert_eq!("\"", s);
        } else {
            panic!("Expected StringLiteral");
        }
    }

    #[test]
    fn char_escape1() {
        let lexer = Lexer::new("'\\\\'".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(1, tokens.len(), "{}", SliceDisplay(&tokens));

        if let TokenKind::CharLiteral(s) = &tokens[0].kind {
            assert_eq!("\\", s);
        } else {
            panic!("Expected StringLiteral");
        }
    }

    #[test]
    fn char_escape2() {
        let lexer = Lexer::new("'\\\\'".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(1, tokens.len(), "{}", SliceDisplay(&tokens));

        if let TokenKind::CharLiteral(s) = &tokens[0].kind {
            assert_eq!("\\", s);
        } else {
            panic!("Expected StringLiteral");
        }
    }

    #[test]
    fn char_escape3() {
        let lexer = Lexer::new("'\\''".to_string());

        let (tokens, errors) = lexer.process();

        assert!(errors.is_empty());
        assert_eq!(1, tokens.len(), "{}", SliceDisplay(&tokens));

        if let TokenKind::CharLiteral(s) = &tokens[0].kind {
            assert_eq!("'", s);
        } else {
            panic!("Expected StringLiteral");
        }
    }

    fn token_comment(token: &Token) -> Option<&str> {
        if let TokenKind::Comment(comment) = &token.kind {
            Some(comment)
        } else {
            None
        }
    }

    fn token_multiline_comment(token: &Token) -> Option<&str> {
        if let TokenKind::MultiLineComment(comment) = &token.kind {
            Some(comment)
        } else {
            None
        }
    }

    #[test]
    fn negative_number() {
        let lexer = Lexer::new("let a = -1;".to_string());

        let (tokens, errors) = lexer.process();

        assert_eq!(
            vec![
                TokenKind::KeyWord(KeywordKind::Let),
                TokenKind::AlphaNumeric("a".into()),
                TokenKind::Punctuation(Equal),
                TokenKind::Number("-1".into()),
                TokenKind::Punctuation(SemiColon),
            ],
            tokens.iter().map(|it| it.kind.clone()).collect::<Vec<_>>()
        );

        assert!(errors.is_empty());
    }

    #[test]
    fn right_arrow() {
        let lexer = Lexer::new("fn a() -> str".to_string());

        let (tokens, errors) = lexer.process();

        assert_eq!(
            vec![
                TokenKind::KeyWord(KeywordKind::Fn),
                TokenKind::AlphaNumeric("a".into()),
                TokenKind::Bracket(BracketKind::Round, BracketStatus::Open),
                TokenKind::Bracket(BracketKind::Round, BracketStatus::Close),
                TokenKind::Punctuation(RightArrow),
                TokenKind::Reserved(ReservedKind::STR),
            ],
            tokens.iter().map(|it| it.kind.clone()).collect::<Vec<_>>()
        );

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
