use std::fmt::Debug;
use std::vec;
use crate::lexer::tokens::{Token, TokenKind};
use crate::lexer::tokens::TokenKind::AlphaNumeric;
use crate::parser::ParserTrait;

pub trait TokensMatcherTrait {

    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult>;

}

struct TokensGroup {
    name: String,
    matchers: Vec<Box<dyn TokenMatcher>>,
    groups: Vec<TokensGroup>,
    quantifier: Quantifier,
    groups_stack: Vec<TokensGroup>,
}

impl TokensGroup {
    pub fn new(name: String, quantifier: Quantifier) -> Self {
        Self { name, matchers: Vec::new(), groups: Vec::new(), groups_stack: Vec::new(), quantifier }
    }

    pub fn add(&mut self, matcher: Box<dyn TokenMatcher>) {
        if let Some(first) = self.groups_stack.first_mut() {
            first.add(matcher);
        } else {
            let name = format!("_{}_", self.groups.len());
            let mut group = TokensGroup::new(name, Quantifier::One);
            group.matchers.push(matcher);
            self.groups.push(group);
        }
    }

    pub fn start_group(&mut self, name: &str, quantifier: Quantifier) {
        self.groups_stack.push(TokensGroup::new(name.to_string(), quantifier));
    }

    pub fn end_group(&mut self) {
        self.groups.push(self.groups_stack.pop().unwrap());
    }


    // TODO flush
}

impl TokensMatcherTrait for TokensGroup {

    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult> {
        println!("n {}", n);
        let mut i = n;

        if self.groups.is_empty() {
            println!("groups is empty");

            let mut kinds = Vec::new();
            let mut values = Vec::new();

            let mut num_of_matches = 0;

            let mut matches = true;

            for matcher in self.matchers.iter() {
                if let Some(token) = parser.get_token_n(i) {
                    if let Some(kind) = matcher.match_token(token) {
                        kinds.push(kind);
                        if let Some(value) = matcher.get_value(token) {
                            values.push(value);
                        }
                        println!("matched {:?} matcher {:?}, {}", token, matcher, i);
                    } else {
                        println!("cannot match {:?} matcher {:?}, {}", token, matcher, i);
                        matches = false;
                        break;
                    }
                } else {
                    matches = false;
                    break;
                }
                i += 1;
            }

            if matches {
                num_of_matches += 1;
            }

            if match self.quantifier {
                Quantifier::One => num_of_matches == 1,
                Quantifier::AtLeastOne => num_of_matches >= 1,
                Quantifier::ZeroOrMore => true,
                Quantifier::AtMostOne => num_of_matches <= 1
            } {
                Some(TokensMatcherResult::new(kinds, values, i))
            } else {
                None
            }
        } else {
            println!("groups is full");

            let mut kinds: Vec<TokenKind> = Vec::new();
            let mut values: Vec<String> = Vec::new();

            let mut num_of_matches = 0;

            loop {
                let mut matches = true;

                for group in self.groups.iter() {
                    if let Some(mut result) = group.match_tokens(parser, i) {
                        let group_kinds = result.kinds_mut();
                        kinds.append(group_kinds);
                        let group_values = result.values_mut();
                        values.append(group_values);
                        i = result.next_n;
                    } else {
                        matches = false;
                        break;
                    }
                }

                if matches {
                    num_of_matches += 1;
                } else {
                    break;
                }
            }

            if match self.quantifier {
                Quantifier::One => num_of_matches == 1,
                Quantifier::AtLeastOne => num_of_matches >= 1,
                Quantifier::ZeroOrMore => true,
                Quantifier::AtMostOne => num_of_matches <= 1
            } {
                Some(TokensMatcherResult::new(kinds, values, i))
            } else {
                None
            }
        }
    }

}

pub struct TokensMatcher {
    group: TokensGroup,
}

impl TokensMatcher {
    pub fn new() -> Self {
        Self { group: TokensGroup::new("_".to_string(), Quantifier::One) }
    }

    pub fn add(&mut self, matcher: Box<dyn TokenMatcher>) {
        self.group.add(matcher);
    }

    pub fn start_group(&mut self, name: &str, quantifier: Quantifier) {
        self.group.start_group(name, quantifier);
    }

    pub fn end_group(&mut self) {
        self.group.end_group();
    }

}

impl TokensMatcherTrait for TokensMatcher {

    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult> {
        self.group.match_tokens(parser, n)
    }

}

pub struct TokensMatcherResult {
    kinds: Vec<TokenKind>,
    values: Vec<String>,
    next_n: usize,
}

impl TokensMatcherResult {
    pub fn new(kinds: Vec<TokenKind>, values: Vec<String>, next_n: usize) -> Self {
        Self { kinds, values, next_n }
    }

    pub fn kinds(&self) -> &Vec<TokenKind> {
        &self.kinds
    }

    pub fn kinds_mut(&mut self) -> &mut Vec<TokenKind> {
        &mut self.kinds
    }

    pub fn next_n(&self) -> usize {
        self.next_n
    }

    pub fn values(&self) -> &Vec<String> {
        &self.values
    }

    pub fn values_mut(&mut self) -> &mut Vec<String> {
        &mut self.values
    }
}

pub enum Quantifier {
    One,
    AtLeastOne,
    ZeroOrMore,
    AtMostOne,
}

#[derive(Debug)]
pub struct KindTokenMatcher {
    kind: TokenKind,
}

impl KindTokenMatcher {
    pub fn new(kind: TokenKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug)]
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

pub trait TokenMatcher: Debug {

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

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            assert_eq!(&vec![
                TokenKind::KeyWord(KeywordKind::Enum),
                AlphaNumeric("Option".into()),
                TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open),
                AlphaNumeric("T".into()),
                TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close),
                TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)],
                       match_result.kinds());
            assert_eq!(&vec!["Option".to_string(), "T".to_string()], match_result.values());
        } else {
            panic!();
        }
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

    #[test]
    fn test_groups() {
        let mut matcher = TokensMatcher::new();
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::KeyWord(KeywordKind::Enum))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        matcher.start_group("paramTypes", Quantifier::AtMostOne);
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        matcher.start_group("type", Quantifier::ZeroOrMore);
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Punctuation(PunctuationKind::Comma))));
        matcher.add(Box::new(AlphanumericTokenMatcher::new()));
        matcher.end_group();
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close))));
        matcher.end_group();
        matcher.add(Box::new(KindTokenMatcher::new(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open))));

        /*
        let parser = get_parser("enum Option {");

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            assert_eq!(&vec![
                TokenKind::KeyWord(KeywordKind::Enum),
                AlphaNumeric("Option".into()),
                TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)],
                       match_result.kinds());
            assert_eq!(&vec!["Option".to_string()], match_result.values());
        } else {
            panic!()
        }

         */

        let parser = get_parser("enum Option<T,Y> {");

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            assert_eq!(&vec![
                TokenKind::KeyWord(KeywordKind::Enum),
                AlphaNumeric("Option".into()),
                TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open),
                AlphaNumeric("T".into()),
                TokenKind::Punctuation(PunctuationKind::Comma),
                AlphaNumeric("Y".into()),
                TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close),
                TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open),
            ],
                       match_result.kinds());
            assert_eq!(&vec!["Option".to_string(), "T".to_string(), "Y".to_string()], match_result.values());
        } else {
            panic!()
        }
    }
}
