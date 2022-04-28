use crate::lexer::tokens::{Token, TokenKind};
use crate::lexer::tokens::TokenKind::AlphaNumeric;
use crate::parser::{Parser, ParserTrait};

pub struct TokensMatcher {
    matchers: Vec<Box<dyn TokenMatcher>>,
}

impl TokensMatcher {

    pub fn new() -> Self {
        Self { matchers: Vec::new() }
    }

    pub fn add(&mut self, matcher: Box<dyn TokenMatcher>) {
        self.matchers.push(matcher);
    }

    pub fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<(Vec<TokenKind>, usize)> {
        let mut result = Vec::new();

        let mut i = n;

        for matcher in self.matchers.iter() {
            if let Some(token) = parser.get_token_n(i) {
                if let Some(kind) = matcher.match_token(token) {
                    result.push(kind);
                } else {
                    return None;
                }
            } else {
                return None;
            }
            i += 1;
        }
        Some((result, i))
    }

    pub fn match_and_get_values(&self, parser: &dyn ParserTrait, n: usize) -> Option<(Vec<String>, usize)> {
        let mut result = Vec::new();

        let mut i = n;

        for matcher in self.matchers.iter() {
            if let Some(token) = parser.get_token_n(i) {
                if matcher.match_token(token).is_some() {
                    if let Some(value) = matcher.get_value(token) {
                        result.push(value);
                    }
                } else {
                    return None;
                }
            } else {
                return None;
            }
            i += 1;
        }
        Some((result, i))
    }
}

pub enum Quantifier {
    One,
    AtLeastOne,
    ZeroOrMore,
    AtMostOne
}

pub struct KindTokenMatcher {
    kind: TokenKind,
}

impl KindTokenMatcher {
    pub fn new(kind: TokenKind) -> Self {
        Self { kind }
    }
}

pub struct AlphanumericTokenMatcher {}

impl AlphanumericTokenMatcher {
    pub fn new() -> Self {
        Self {}
    }
}

impl TokenMatcher for AlphanumericTokenMatcher {
    fn match_token(&self, token: &Token) -> Option<TokenKind> {
        if let AlphaNumeric(name) = &token.kind {
            Some(AlphaNumeric(name.clone()))
        } else {
            None
        }
    }

    fn get_value(&self, token: &Token) -> Option<String> {
        if let AlphaNumeric(name) = &token.kind {
            Some(name.clone())
        } else {
            None
        }
    }
}

pub trait TokenMatcher {
    fn match_token(&self, token: &Token) -> Option<TokenKind>;

    fn get_value(&self, token: &Token) -> Option<String>;
}

impl TokenMatcher for KindTokenMatcher {
    fn match_token(&self, token: &Token) -> Option<TokenKind> {
        if token.kind == self.kind {
            Some(token.kind.clone())
        } else {
            None
        }
    }

    fn get_value(&self, _token: &Token) -> Option<String> {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind};
    use crate::parser::test_utils::get_parser;
    use super::*;

    #[test]
    fn test() {
        let parser = get_parser("enum Option<T> {");

        let mut matcher = TokensMatcher::new();
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::KeyWord(KeywordKind::Enum))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close))));
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open))));

        let match_result = matcher.match_tokens(&parser, 0);

        assert_eq!(Some((vec![
            TokenKind::KeyWord(KeywordKind::Enum),
            AlphaNumeric("Option".into()),
            TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open),
            AlphaNumeric("T".into()),
            TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close),
            TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)], 6)),
                   match_result);
    }

    #[test]
    fn test_get_value() {
        let parser = get_parser("enum Option<T> {");

        let mut matcher = TokensMatcher::new();
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::KeyWord(KeywordKind::Enum))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close))));
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open))));

        let match_result = matcher.match_and_get_values(&parser, 0);

        assert_eq!(Some((vec!["Option".into(), "T".into()], 6)),match_result);
    }

    #[test]
    fn not_matching_test() {
        let parser = get_parser("enum Option<T> {");

        let mut matcher = TokensMatcher::new();
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::KeyWord(KeywordKind::Asm))));

        let match_result = matcher.match_tokens(&parser, 0);

        assert!(match_result.is_none());
    }

    #[test]
    fn few_tokens() {
        let parser = get_parser("enum");

        let mut matcher = TokensMatcher::new();
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::KeyWord(KeywordKind::Enum))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));

        let match_result = matcher.match_tokens(&parser, 0);

        assert!(match_result.is_none());
    }

    /*
    #[test]
    fn test_groups() {
        let parser = get_parser("enum Option {");

        let mut matcher = TokensMatcher::new();
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::KeyWord(KeywordKind::Enum))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        //matcher.start_group("paramTypes");
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        //matcher.start_group();
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Punctuation(PunctuationKind::Colon))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        //matcher.end_group(Quantifier::ZeroOrMore);
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close))));
        //matcher.end_group(Quantifier::AtMostOne);
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open))));

        let match_result = matcher.match_tokens(&parser, 0);

        assert_eq!(Some((vec![
            TokenKind::KeyWord(KeywordKind::Enum),
            AlphaNumeric("Option".into()),
            TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open),
            AlphaNumeric("T".into()),
            TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close),
            TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)], 6)),
                   match_result);
    }

     */
}
