use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use crate::lexer::tokens::{Token, TokenKind};
use crate::lexer::tokens::TokenKind::AlphaNumeric;
use crate::parser::ParserTrait;
use crate::parser::tokens_group::TokensGroup;

pub trait TokensMatcherTrait: Debug {
    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult>;

    fn name(&self) -> Vec<String>;
}

pub struct TokensMatcher {
    group: TokensGroup,
}

impl Default for TokensMatcher {
    fn default() -> Self {
        TokensMatcher::new("main", Quantifier::One)
    }
}

impl TokensMatcher {
    pub fn new(name: &str, quantifier: Quantifier) -> Self {
        Self { group: TokensGroup::new(vec![name.to_string()], quantifier) }
    }

    pub fn add_matcher<T: 'static>(&mut self, matcher: T)
        where T: TokensMatcherTrait {
        self.group.add_matcher(matcher);
    }

    pub fn add_kind(&mut self, kind: TokenKind) {
        self.group.add_matcher(KindTokenMatcher::new(kind));
    }

    pub fn add_alphanumeric(&mut self) {
        self.group.add_matcher(AlphanumericTokenMatcher::new());
    }

    pub fn start_group(&mut self, name: &str, quantifier: Quantifier) {
        self.group.start_group(name, quantifier);
    }

    pub fn end_group(&mut self) {
        self.group.end_group();
    }
}

impl Debug for TokensMatcher {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("TokensMatcher")
    }
}

impl TokensMatcherTrait for TokensMatcher {
    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult> {
        self.group.match_tokens(parser, n)
    }

    fn name(&self) -> Vec<String> {
        self.group.name().clone()
    }
}

#[derive(Debug)]
pub struct TokensMatcherResult {
    kinds: Vec<TokenKind>,
    values: Vec<String>,
    next_n: usize,
    groups_values: HashMap<String, Vec<String>>,
    // TODO
    empty_vec: Vec<String>,
}

impl TokensMatcherResult {
    pub fn new(kinds: Vec<TokenKind>, values: Vec<String>, group_values: HashMap<String, Vec<String>>, next_n: usize) -> Self {
        Self { kinds, values, next_n, groups_values: group_values, empty_vec: Vec::new() }
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

    pub fn group_values(&self, name: &str) -> &Vec<String> {
        if let Some(values) = self.groups_values.get(name) {
            values
        } else {
            &self.empty_vec
        }
    }

    pub fn values_mut(&mut self) -> &mut Vec<String> {
        &mut self.values
    }

    pub fn groups_values_mut(&mut self) -> &mut HashMap<String, Vec<String>> {
        &mut self.groups_values
    }
}

#[derive(Debug, Clone)]
pub enum Quantifier {
    One,
    AtLeastOne,
    ZeroOrMore,
    AtMostOne,
}

#[derive(Debug, Clone)]
struct KindTokenMatcher {
    kind: TokenKind,
}

impl KindTokenMatcher {
    fn new(kind: TokenKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug, Clone)]
struct AlphanumericTokenMatcher {}

impl AlphanumericTokenMatcher {
    fn new() -> Self {
        Self {}
    }
}

impl<T> TokensMatcherTrait for T where T: TokenMatcher {
    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult> {
        if let Some(token) = parser.get_token_n(n) {
            if let Some(kind) = self.match_token(token) {
                let values = if let Some(value) = self.get_value(token) {
                    vec![value]
                } else {
                    vec![]
                };
                Some(TokensMatcherResult::new(vec![kind], values, HashMap::new(), n + 1))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn name(&self) -> Vec<String> {
        vec![]
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

pub trait TokenMatcher: Debug + TokensMatcherTrait {
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
    fn test_simple() {
        let parser = get_parser("n1 n2");

        println!("hhhh 1");

        let mut matcher = TokensMatcher::default();
        matcher.add_alphanumeric();
        matcher.add_alphanumeric();

        println!("hhhh 2");

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            assert_eq!(&vec!["n1", "n2"], match_result.values());
        } else {
            panic!()
        }
    }

    #[test]
    fn test_group_simple() {
        let parser = get_parser("n1 n2");

        let mut matcher = TokensMatcher::default();
        matcher.add_alphanumeric();
        matcher.start_group("g1", Quantifier::One);
        matcher.add_alphanumeric();
        matcher.end_group();

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            assert_eq!(&vec!["n1"], match_result.values());
            assert_eq!(&vec!["n2"], match_result.group_values("g1"));
        } else {
            panic!()
        }
    }

    #[test]
    fn test_group_simple_with_matcher() {
        let parser = get_parser("n1 n2 n3 {");

        let mut g1 = TokensMatcher::new("g1", Quantifier::AtLeastOne);
        g1.add_alphanumeric();

        let mut matcher = TokensMatcher::default();
        matcher.add_alphanumeric();
        matcher.add_matcher(g1);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            assert_eq!(&vec!["n1"], match_result.values());
            assert_eq!(&vec!["n2", "n3"], match_result.group_values("g1"));
        } else {
            panic!()
        }
    }

    #[test]
    fn test() {
        let parser = get_parser("enum Option<T> {");

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();
        matcher.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open));
        matcher.add_alphanumeric();
        matcher.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close));
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            println!("{:?}", match_result);

            assert_eq!(&vec![
                TokenKind::KeyWord(KeywordKind::Enum),
                AlphaNumeric("Option".into()),
                TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open),
                AlphaNumeric("T".into()),
                TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close),
                TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)],
                       match_result.kinds());
            assert_eq!(&vec!["Option", "T"], match_result.values());
        } else {
            panic!();
        }
    }

    #[test]
    fn not_matching_test() {
        let parser = get_parser("enum Option<T> {");

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Asm));

        let match_result = matcher.match_tokens(&parser, 0);

        assert!(match_result.is_none());
    }

    #[test]
    fn few_tokens() {
        let parser = get_parser("enum");

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();

        let match_result = matcher.match_tokens(&parser, 0);

        assert!(match_result.is_none());
    }

    #[test]
    fn test_groups() {
        let mut param_types = TokensMatcher::new("paramTypes", Quantifier::AtMostOne);
        param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open));
        param_types.add_alphanumeric();
        param_types.start_group("type", Quantifier::ZeroOrMore);
        param_types.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        param_types.add_alphanumeric();
        param_types.end_group();
        param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close));

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();
        matcher.add_matcher(param_types);

        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        let parser = get_parser("enum Option {");

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            println!("{:?}", match_result);

            assert_eq!(&vec![
                TokenKind::KeyWord(KeywordKind::Enum),
                AlphaNumeric("Option".into()),
                TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)],
                       match_result.kinds());
            assert_eq!(&vec!["Option".to_string()], match_result.values());
            assert!(match_result.group_values("paramTypes").is_empty());
            assert!(match_result.group_values("type").is_empty());
            assert!(match_result.group_values("unknown").is_empty());
            assert_eq!(3, match_result.next_n());
        } else {
            panic!()
        }

        let parser = get_parser("enum Option<T,Y> {");

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            println!("{:?}", match_result);

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
            assert_eq!(&vec!["Option"], match_result.values());
            assert_eq!(&vec!["T", "Y"], match_result.group_values("paramTypes"));
            assert_eq!(&vec!["Y"], match_result.group_values("type"));
            assert_eq!(8, match_result.next_n());
        } else {
            panic!()
        }
    }

    #[test]
    fn test_groups1() {
        let mut param_types = TokensMatcher::new("paramTypes", Quantifier::AtMostOne);
        param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open));
        param_types.add_alphanumeric();
        param_types.start_group("type", Quantifier::ZeroOrMore);
        param_types.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        param_types.add_alphanumeric();
        param_types.end_group();
        param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close));

        let parser = get_parser("");

        if let Some(match_result) = param_types.match_tokens(&parser, 0) {
            println!("{:?}", match_result);
        } else {
            panic!()
        }
    }
}
