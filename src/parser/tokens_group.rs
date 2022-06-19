use std::fmt::{Debug, Display, Formatter};
use std::collections::HashMap;
use crate::parser::ParserTrait;
use crate::parser::tokens_matcher::{Quantifier, TokensMatcherResult, TokensMatcherTrait};

#[derive(Debug)]
pub struct TokensGroup {
    name: Vec<String>,
    tokens_matchers: Vec<Box<dyn TokensMatcherTrait>>,
    quantifier: Quantifier,
    current_group: Vec<TokensGroup>,
}


impl Display for TokensGroup {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{:?}(", self.name.last().unwrap(), self.quantifier).unwrap();
        let mut first = true;
        for matcher in self.tokens_matchers.iter() {
            if !first {
                write!(f, " ").unwrap();
            }
            write!(f, "{}", matcher).unwrap();

            first = false;
        }
        write!(f, ")")
    }
}

impl TokensGroup {
    pub fn new(name: Vec<String>, quantifier: Quantifier) -> Self {
        //println!("created group {:?}", name);
        Self { name, current_group: Vec::new(), tokens_matchers: Vec::new(), quantifier }
    }

    pub fn add_matcher<T: 'static>(&mut self, matcher: T)
        where T: TokensMatcherTrait {
        //println!("adding matcher {:?}", &matcher);
        if let Some(first) = self.current_group.first_mut() {
            first.add_matcher(matcher);
        } else {
            //self.matchers.insert(self.len(), matcher);
            self.tokens_matchers.push(Box::new(matcher));
        }
        //println!("added to {:?}", self);
    }

    pub fn start_group(&mut self, name: &str, quantifier: Quantifier) {
        if self.current_group.is_empty() {
            let mut n = self.name.clone();
            n.push(name.to_string());

            let group = TokensGroup::new(n, quantifier);
            self.current_group.push(group);
        } else {
            self.current_group.first_mut().unwrap().start_group(name, quantifier);
        }
    }

    /// returns true if the group should be closed
    pub fn end_group(&mut self) -> bool {
        if self.current_group.is_empty() {
            true
        } else {
            if self.current_group.first_mut().unwrap().end_group() {
                //self.groups.insert(self.len(), self.current_group.pop().unwrap());
                self.tokens_matchers.push(Box::new(self.current_group.pop().unwrap()));
            }
            false
        }
    }

    pub fn push_result(map: &mut HashMap<String, Vec<TokensMatcherResult>>, key: String, result: TokensMatcherResult) {
        if !map.contains_key(&key) {
            map.insert(key.clone(), Vec::new());
        }
        map.get_mut(&key).unwrap().push(result);
    }
}

impl TokensMatcherTrait for TokensGroup {
    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult> {
        if !self.current_group.is_empty() {
            panic!("not ended group {:?}", self.current_group);
        }
        //println!("\nmatch_tokens for {:?}, n {}", self, n);

        let mut i = n;

        let mut tokens = Vec::new();
        let mut values = Vec::new();
        let mut groups_result: HashMap<String, Vec<TokensMatcherResult>> = HashMap::new();

        let mut num_of_matches = 0;

        loop {
            let mut matches = true;

            for matcher in self.tokens_matchers.iter() {
                if let Some(result) = matcher.match_tokens(parser, i) {
                    tokens.append(&mut result.tokens().clone());

                    //println!("matched matcher {:?} for {:?}, result {:?}", matcher, self, result);

                    i = result.next_n();

                    if !result.values().is_empty() {
                        // it's not a group I add the values directly to the result
                        if matcher.name().is_empty() {
                            let mut matcher_values = result.values().clone();
                            values.append(&mut matcher_values);
                        }
                    }

                    if !matcher.name().is_empty() {
                        let name = matcher.name().last().unwrap().clone();
                        TokensGroup::push_result(&mut groups_result, name, result);
                    }
                    //println!("groups_values {:?}", groups_result);
                } else {
                    //println!("not matched matcher {:?}", matcher);
                    matches = false;
                    break;
                }
            }

            if matches {
                num_of_matches += 1;
            } else {
                break;
            }

            if matches!(self.quantifier, Quantifier::One | Quantifier::AtMostOne) {
                //println!("exited loop for {:?}", self);
                break;
            }
        }

        if match self.quantifier {
            Quantifier::One => num_of_matches == 1,
            Quantifier::AtLeastOne => num_of_matches >= 1,
            Quantifier::ZeroOrMore => true,
            Quantifier::AtMostOne => num_of_matches <= 1
        } {
            //println!("matched all for {:?}, values {:?}, group values: {:?}\n", self, values, groups_result);
            Some(TokensMatcherResult::new(tokens, values, groups_result, i, num_of_matches))
        } else {
            //println!("not matched all for {:?}, found {} matches\n", self, num_of_matches);
            None
        }
    }

    fn name(&self) -> Vec<String> {
        self.name.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::tokens_matcher::{AlphanumericTokenMatcher};
    use super::*;

    #[test]
    fn test() {
        let mut sut = TokensGroup::new(vec!["main".into()], Quantifier::One);
        sut.add_matcher(AlphanumericTokenMatcher::new());
        sut.start_group("g1", Quantifier::One);
        sut.add_matcher(AlphanumericTokenMatcher::new());
        sut.start_group("g2", Quantifier::One);
        sut.add_matcher(AlphanumericTokenMatcher::new());
        sut.end_group();
        sut.start_group("g2", Quantifier::One);
        sut.add_matcher(AlphanumericTokenMatcher::new());
        sut.end_group();
        sut.end_group();
        sut.add_matcher(AlphanumericTokenMatcher::new());

        let s = format!("sut {:?}", sut);
        assert_eq!(2, s.matches("\"main\", \"g1\", \"g2\"").count());
    }

}