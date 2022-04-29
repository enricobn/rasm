use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use crate::lexer::tokens::{Token, TokenKind};
use crate::lexer::tokens::TokenKind::AlphaNumeric;
use crate::parser::ParserTrait;

pub trait TokensMatcherTrait {
    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult>;
}

pub struct TokensGroup {
    name: Vec<String>,
    matchers: HashMap<usize, Box<dyn TokenMatcher>>,
    groups: HashMap<usize, TokensGroup>,
    quantifier: Quantifier,
    current_group: Vec<TokensGroup>,
}

impl Debug for TokensGroup {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let groups_names: Vec<String> = self.groups.values().map(|it| it.name.join(", ")).collect();
        let groups_stack_names: Vec<String> = self.current_group.iter().map(|it| it.name.join(", ")).collect();
        write!(f, "{} \n  groups: {}, \n  stack: {}", self.name.join(", "),
               groups_names.join(" | "),
               groups_stack_names.join(" | ")
        )
    }
}

impl TokensGroup {
    pub fn new(name: Vec<String>, quantifier: Quantifier) -> Self {
        Self { name, matchers: HashMap::new(), groups: HashMap::new(), current_group: Vec::new(), quantifier }
    }

    fn add_matcher(&mut self, matcher: Box<dyn TokenMatcher>) {
        if let Some(first) = self.current_group.first_mut() {
            first.add_matcher(matcher);
        } else {
            self.matchers.insert(self.len(), matcher);
        }
    }

    fn len(&self) -> usize {
        self.matchers.len() + self.groups.len()
    }

    fn start_group(&mut self, name: &str, quantifier: Quantifier) {
        if self.current_group.is_empty() {
            let mut n = self.name.clone();
            n.push(name.to_string());

            let group = TokensGroup::new(n, quantifier);
            self.current_group.push(group);
        } else {
            self.current_group.first_mut().unwrap().start_group(name, quantifier);
        }
    }

    fn end_group(&mut self) -> bool {
        if self.current_group.is_empty() {
            return false;
        } else {
            if !self.current_group.first_mut().unwrap().end_group() {
                self.groups.insert(self.len(), self.current_group.pop().unwrap());
                return true;
            }
            return false;
        }
    }

    fn push_value(map: &mut HashMap<String, Vec<String>>, key: String, values: &mut Vec<String>) {
        if !map.contains_key(&key) {
            map.insert(key.clone(), Vec::new());
        }
        map.get_mut(&key).unwrap().append(values);
    }

}

impl TokensMatcherTrait for TokensGroup {
    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult> {
        if !self.current_group.is_empty() {
            panic!("not ended group");
        }
        //println!("match_tokens {:?}, {}", self, n);

        let mut i = n;

        let mut kinds = Vec::new();
        let mut values = Vec::new();
        let mut groups_values: HashMap<String, Vec<String>> = HashMap::new();

        let mut num_of_matches = 0;

        loop {
            let mut matches = true;

            for j in 0..self.len() {
                if let Some(matcher) = self.matchers.get(&j) {
                    if let Some(token) = parser.get_token_n(i) {
                        if let Some(kind) = matcher.match_token(token) {
                            kinds.push(kind);
                            if let Some(value) = matcher.get_value(token) {
                                values.push(value);
                            }
                            i += 1;
                            println!("matched {:?} matcher {:?}, {} -> {:?}", token, matcher, i, self.name);
                        } else {
                            println!("cannot match {:?} matcher {:?}, {} -> {:?}", token, matcher, i, self.name);
                            matches = false;
                            break;
                        }
                    } else {
                        matches = false;
                        break;
                    }
                } else if let Some(group) = self.groups.get(&j) {
                    if let Some(mut result) = group.match_tokens(parser, i) {
                        let group_kinds = result.kinds_mut();
                        kinds.append(group_kinds);

                        println!("result {:?} -> {:?}", result, group.name);

                        for k in group.name.iter() {
                            let mut vec = result.values.clone();
                            TokensGroup::push_value(&mut groups_values, k.to_string(), &mut vec);
                        }

                        for (k, v) in result.groups_values_mut().iter_mut() {
                            TokensGroup::push_value(&mut groups_values, k.to_string(), v);
                        }

                        println!("groups_values {:?}\n", groups_values);

                        i = result.next_n;
                    } else {
                        matches = false;
                        break;
                    }
                } else {
                    panic!();
                }
            }

            if matches {
                num_of_matches += 1;
            } else {
                break;
            }

            if match self.quantifier {
                Quantifier::One => num_of_matches != 1,
                Quantifier::AtMostOne => num_of_matches > 1,
                _ => true,
            } {
                break;
            }
        }

        if match self.quantifier {
            Quantifier::One => num_of_matches == 1,
            Quantifier::AtLeastOne => num_of_matches >= 1,
            Quantifier::ZeroOrMore => true,
            Quantifier::AtMostOne => num_of_matches <= 1
        } {
            println!("values: {:?}, group values: {:?} -> {:?}", values, groups_values, self.name);
            Some(TokensMatcherResult::new(kinds, values, groups_values, i))
        } else {
            None
        }
    }
}

pub struct TokensMatcher {
    group: TokensGroup,
}

impl TokensMatcher {
    pub fn new() -> Self {
        Self { group: TokensGroup::new(Vec::new(), Quantifier::One) }
    }

    pub fn add(&mut self, matcher: Box<dyn TokenMatcher>) {
        self.group.add_matcher(matcher);
    }

    pub fn add_kind(&mut self, kind: TokenKind) {
        self.group.add_matcher(Box::new(KindTokenMatcher::new(kind)));
    }

    pub fn add_alphanumeric(&mut self) {
        self.group.add_matcher(Box::new(AlphanumericTokenMatcher::new()));
    }

    pub fn start_group(&mut self, name: &str, quantifier: Quantifier) {
        if name.starts_with("_") && name.ends_with("_") {
            panic!("name cannot start and end with _")
        }
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

    pub fn add_group_value(&mut self, name: &str, value: String) {
        if !self.groups_values.contains_key(name) {
            self.groups_values.insert(name.to_string(), Vec::new());
        }
        self.groups_values.get_mut(name).unwrap().push(value);
    }

    pub fn add_value(&mut self, value: String) {
        self.values.push(value);
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

    fn values_mut(&mut self) -> &mut Vec<String> {
        &mut self.values
    }

    pub fn groups_values(&self) -> &HashMap<String, Vec<String>> {
        &self.groups_values
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

#[derive(Debug)]
struct KindTokenMatcher {
    kind: TokenKind,
}

impl KindTokenMatcher {
    fn new(kind: TokenKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug)]
struct AlphanumericTokenMatcher {}

impl AlphanumericTokenMatcher {
    fn new() -> Self {
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
    fn test_simple() {
        let parser = get_parser("n1 n2");

        let mut matcher = TokensMatcher::new();
        matcher.add_alphanumeric();
        matcher.add_alphanumeric();

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            assert_eq!(&vec!["n1".to_string(), "n2".to_string()], match_result.values());
        } else {
            panic!()
        }
    }

    #[test]
    fn test_group_simple() {
        let parser = get_parser("n1 n2");

        let mut matcher = TokensMatcher::new();
        matcher.add_alphanumeric();
        matcher.start_group("g1", Quantifier::One);
        matcher.add_alphanumeric();
        matcher.end_group();

        if let Some(match_result) = matcher.match_tokens(&parser, 0) {
            assert_eq!(&vec!["n1".to_string()], match_result.values());
            assert_eq!(&vec!["n2".to_string()], match_result.group_values("g1"));
        } else {
            panic!()
        }
    }

    #[test]
    fn test() {
        let parser = get_parser("enum Option<T> {");

        let mut matcher = TokensMatcher::new();
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
            assert_eq!(&vec!["Option".to_string(), "T".to_string()], match_result.values());
        } else {
            panic!();
        }
    }

    #[test]
    fn not_matching_test() {
        let parser = get_parser("enum Option<T> {");

        let mut matcher = TokensMatcher::new();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Asm));

        let match_result = matcher.match_tokens(&parser, 0);

        assert!(match_result.is_none());
    }

    #[test]
    fn few_tokens() {
        let parser = get_parser("enum");

        let mut matcher = TokensMatcher::new();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();

        let match_result = matcher.match_tokens(&parser, 0);

        assert!(match_result.is_none());
    }

    #[test]
    fn test_groups() {
        let mut matcher = TokensMatcher::new();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();
        matcher.start_group("paramTypes", Quantifier::AtMostOne);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open));
        matcher.add_alphanumeric();
        matcher.start_group("type", Quantifier::ZeroOrMore);
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        matcher.add_alphanumeric();
        matcher.end_group();
        matcher.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close));
        matcher.end_group();
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
            assert_eq!(&vec!["Option".to_string()], match_result.values());
            assert_eq!(&vec!["T".to_string(), "Y".to_string()], match_result.group_values("paramTypes"));
            assert_eq!(&vec!["Y".to_string()], match_result.group_values("type"));
            assert_eq!(8, match_result.next_n());
        } else {
            panic!()
        }
    }
}
