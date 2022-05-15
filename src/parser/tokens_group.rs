use std::fmt::{Debug, Formatter};
use std::collections::HashMap;
use crate::parser::ParserTrait;
use crate::parser::tokens_matcher::{Quantifier, TokensMatcherResult, TokensMatcherTrait};

pub struct TokensGroup {
    name: Vec<String>,
    tokens_matchers: Vec<Box<dyn TokensMatcherTrait>>,
    quantifier: Quantifier,
    current_group: Vec<TokensGroup>,
}

impl Debug for TokensGroup {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "group {:?} {:?}", self.name, self.quantifier)
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

    pub fn end_group(&mut self) -> bool {
        if self.current_group.is_empty() {
            return false;
        } else {
            if !self.current_group.first_mut().unwrap().end_group() {
                //self.groups.insert(self.len(), self.current_group.pop().unwrap());
                self.tokens_matchers.push(Box::new(self.current_group.pop().unwrap()));
                return true;
            }
            return false;
        }
    }

    pub fn push_values(map: &mut HashMap<String, Vec<String>>, key: String, values: &mut Vec<String>) {
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
        println!("\nmatch_tokens for {:?}, n {}", self, n);

        let mut i = n;

        let mut kinds = Vec::new();
        let mut values = Vec::new();
        let mut groups_values: HashMap<String, Vec<String>> = HashMap::new();

        let mut num_of_matches = 0;

        loop {
            let mut matches = true;

            for matcher in self.tokens_matchers.iter() {
                if let Some(mut result) = matcher.match_tokens(parser, i) {
                    let group_kinds = result.kinds_mut();
                    kinds.append(group_kinds);

                    println!("matched matcher {:?} for {:?}, result {:?}", matcher, self, result);

                    if !result.values().is_empty() {
                        // it's not a group I add the values directly to the result
                        if matcher.name().is_empty() {
                            let mut matcher_values = result.values().clone();
                            values.append(&mut matcher_values);
                        } else {
                            for k in matcher.name().iter() {
                                let mut vec = result.values().clone();
                                println!("adding values to {} : {:?}", k, vec);
                                TokensGroup::push_values(&mut groups_values, k.to_string(), &mut vec);
                            }
                        }
                    }

                    for (k, v) in result.groups_values_mut().iter_mut() {
                        TokensGroup::push_values(&mut groups_values, k.to_string(), v);
                    }

                    println!("groups_values {:?}", groups_values);

                    i = result.next_n();
                } else {
                    println!("not matched matcher {:?}", matcher);
                    matches = false;
                    break;
                }
            }

            if matches {
                num_of_matches += 1;
            } else {
                break;
            }

            if match self.quantifier {
                Quantifier::One => num_of_matches != 1,
                Quantifier::AtMostOne => true,
                _ => false,
            } {
                println!("exited loop for {:?}", self);
                break;
            }
        }

        if match self.quantifier {
            Quantifier::One => num_of_matches == 1,
            Quantifier::AtLeastOne => num_of_matches >= 1,
            Quantifier::ZeroOrMore => true,
            Quantifier::AtMostOne => num_of_matches <= 1
        } {
            println!("matched all for {:?}, values {:?}, group values: {:?}\n", self, values, groups_values);
            Some(TokensMatcherResult::new(kinds, values, groups_values, i, num_of_matches))
        } else {
            println!("not matched all for {:?}, found {} matches\n", self, num_of_matches);
            None
        }
    }

    fn name(&self) -> Vec<String> {
        self.name.clone()
    }
}
