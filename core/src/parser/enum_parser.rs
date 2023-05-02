use std::fmt::{Display, Formatter};

use linked_hash_map::LinkedHashMap;

use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, TokenKind};
use crate::parser::ast::{ASTEnumDef, ASTEnumVariantDef, ASTParameterDef};
use crate::parser::matchers::param_types_matcher;
use crate::parser::tokens_matcher::{
    Quantifier, TokensMatcher, TokensMatcherResult, TokensMatcherTrait,
};
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserTrait;

pub struct EnumParser<'a> {
    parser: &'a dyn ParserTrait,
}

impl<'a> EnumParser<'a> {
    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse(&self) -> Option<(String, Vec<String>, usize)> {
        let param_types = param_types_matcher();

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();
        matcher.add_matcher(param_types);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        matcher.match_tokens(self.parser, 0).map(|result| {
            let x = result.group_values("type");
            (
                result.values().first().unwrap().clone(),
                x,
                self.parser.get_i() + result.next_n(),
            )
        })
    }

    pub fn try_parse_enum(&self) -> Option<(ASTEnumDef, usize)> {
        if let Some((name, type_parameters, next_i)) = self.try_parse() {
            if let Some((variants, next_i)) =
                self.parse_variants(&type_parameters, next_i - self.parser.get_i())
            {
                return Some((
                    ASTEnumDef {
                        name,
                        type_parameters,
                        variants,
                        index: self.parser.get_index(0).unwrap().clone(),
                    },
                    next_i,
                ));
            }
        }
        None
    }

    pub fn parse_variants(
        &self,
        type_parameters: &[String],
        n: usize,
    ) -> Option<(Vec<ASTEnumVariantDef>, usize)> {
        //println!("parse_variants n {}", n);
        let mut matcher = TokensMatcher::default();
        matcher.add_matcher(EnumParser::variant_matcher(
            "variant",
            Quantifier::One,
            type_parameters,
        ));
        matcher.start_group("variant_", Quantifier::ZeroOrMore);
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        matcher.add_matcher(EnumParser::variant_matcher(
            "variant",
            Quantifier::One,
            type_parameters,
        ));
        matcher.end_group();
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close));

        if let Some(variants_result) = matcher.match_tokens(self.parser, n) {
            let variant_results: Vec<&TokensMatcherResult> =
                variants_result.group_results("variant");

            let variants = variant_results
                .iter()
                .map(|result| {
                    let name = result.values().first().unwrap();

                    let parameters_s = result.group_values("parameter");

                    let type_result = result.group_results("parameter_type");

                    let parameters = if parameters_s.is_empty() {
                        Vec::new()
                    } else {
                        let mut parameters = Vec::new();
                        for i in 0..parameters_s.len() {
                            //println!("parsing parameter {}, n: {}", parameters_s.get(i).unwrap(), n);
                            let parser = *type_result.get(i).unwrap();
                            let type_parser = TypeParser::new(parser);
                            if let Some((ast_type, _)) =
                                type_parser.try_parse_ast_type(0, type_parameters)
                            {
                                parameters.push(ASTParameterDef {
                                    name: parameters_s.get(i).unwrap().clone(),
                                    ast_type,
                                    ast_index: parser.get_index(0).unwrap(),
                                });
                            } else {
                                self.parser.panic(&format!(
                                    "Cannot parse type for enum variant {}:",
                                    name
                                ));
                                panic!();
                            }
                        }
                        parameters
                    };

                    ASTEnumVariantDef {
                        name: name.clone(),
                        parameters,
                        index: self.parser.get_index(n).unwrap(),
                    }
                })
                .collect();

            //println!("parse_variants next_n: {}", variants_result.next_n());
            Some((variants, self.parser.get_i() + variants_result.next_n()))
        } else {
            None
        }
    }

    // TODO it seems to be more generic: it is a list of parameters surrounded by (), it can be used to match even function declarations?
    fn variant_matcher(
        name: &str,
        quantifier: Quantifier,
        type_parameters: &[String],
    ) -> TokensMatcher {
        let mut matcher = TokensMatcher::new(name, quantifier);
        matcher.add_alphanumeric();
        matcher.start_group("parameters", Quantifier::AtMostOne);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open));
        matcher.start_group("parameter_list", Quantifier::One);
        matcher.add_matcher(EnumParser::parameter_matcher(type_parameters));
        matcher.end_group();
        matcher.start_group("parameter_list", Quantifier::ZeroOrMore);
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        matcher.add_matcher(EnumParser::parameter_matcher(type_parameters));
        matcher.end_group();
        matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close));
        matcher.end_group();
        matcher
    }

    pub fn parameter_matcher(context_generic_types: &[String]) -> ParameterMatcher {
        ParameterMatcher {
            context_generic_types: context_generic_types.into(),
        }
    }
}

#[derive(Debug)]
pub struct ParameterMatcher {
    context_generic_types: Vec<String>,
}

impl Display for ParameterMatcher {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("ParameterMatcher")
    }
}

impl TokensMatcherTrait for ParameterMatcher {
    fn match_tokens(&self, parser: &dyn ParserTrait, n: usize) -> Option<TokensMatcherResult> {
        if let Some(TokenKind::AlphaNumeric(name)) = parser.get_token_kind_n(n) {
            if let Some(TokenKind::Punctuation(PunctuationKind::Colon)) =
                parser.get_token_kind_n(n + 1)
            {
                let type_parser = TypeParser::new(parser);
                // I don't care of type since who evaluates this resul gets only the tokens
                if let Some((_ast_type, next_i)) =
                    type_parser.try_parse_ast_type(n + 2, &self.context_generic_types)
                {
                    let next_n = next_i - parser.get_i();

                    let mut tokens = Vec::new();
                    let mut type_tokens = Vec::new();

                    for i in n..next_n {
                        tokens.push(parser.get_token_n(i).unwrap().clone());
                        if i > n + 1 {
                            type_tokens.push(parser.get_token_n(i).unwrap().clone());
                        }
                    }

                    //println!("match_tokens n: {}\ni: {}\nnext_i: {}\ntokens {:?}\nast_type: {:?}\ntype_tokens: {:?}", n, parser.get_i(), next_i, tokens, _ast_type, type_tokens);

                    let mut groups_results = LinkedHashMap::new();
                    // values is an empty array, it does not matter for now since who evaluates this resul gets only the tokens
                    let types_result = TokensMatcherResult::new(
                        type_tokens,
                        Vec::new(),
                        LinkedHashMap::new(),
                        next_i - parser.get_i(),
                        1,
                    );
                    groups_results.insert("parameter_type".into(), vec![types_result]);
                    return Some(TokensMatcherResult::new(
                        tokens,
                        vec![name.clone()],
                        groups_results,
                        next_i - parser.get_i(),
                        1,
                    ));
                }
            }
        }
        None
    }

    fn name(&self) -> Vec<String> {
        vec!["parameter".into()]
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::ASTType::Generic;
    use crate::parser::ast::{ASTEnumVariantDef, ASTIndex, ASTParameterDef, ASTType};
    use crate::parser::test_utils::get_parser;
    use crate::parser::tokens_matcher::TokensMatcherResult;

    use super::*;

    #[test]
    fn empty_enum() {
        let parse_result = try_parse(
            "enum Option<T> {
        }",
        );

        assert_eq!(Some(("Option".into(), vec!["T".into()], 6)), parse_result);
    }

    #[test]
    fn test() {
        let parse_result = try_parse_enum(
            "enum Option<T> {
            Empty,
            Some(value: T)
        }",
        );

        let some = ASTEnumVariantDef {
            name: "Some".into(),
            parameters: vec![ASTParameterDef {
                name: "value".into(),
                ast_type: Generic("T".into()),
                ast_index: ASTIndex::none(),
            }],
            index: ASTIndex::new(None, 2, 18),
        };

        let empty = ASTEnumVariantDef {
            name: "Empty".into(),
            parameters: vec![],
            index: ASTIndex::new(None, 2, 18),
        };

        assert_eq!(
            Some((
                ASTEnumDef {
                    name: "Option".to_string(),
                    type_parameters: vec!["T".to_string()],
                    variants: vec![empty, some],
                    index: ASTIndex::new(None, 1, 5)
                },
                15
            )),
            parse_result
        );
    }

    #[test]
    fn test_empty_variant() {
        let parse_result = try_parse_variant("Empty", &[]);

        if let Some(result) = parse_result {
            assert_eq!("Empty", result.values().first().unwrap())
        } else {
            panic!()
        }
    }

    #[test]
    // It's for a use case that caused a bug
    fn test_variant_with_a_function_call() {
        let parse_result = try_parse_variant("Empty } call(15);", &[]);

        if let Some(result) = parse_result {
            assert_eq!("Empty", result.values().first().unwrap());
            assert_eq!(1, result.next_n());
        } else {
            panic!()
        }
    }

    #[test]
    fn test_some_variant() {
        let parse_result = parse_variants("Some(value: T)}", &["T".into()], 0);

        if let Some((variants, next_i)) = parse_result {
            assert_eq!(
                variants,
                vec![ASTEnumVariantDef {
                    name: "Some".into(),
                    parameters: vec![ASTParameterDef::new(
                        "value",
                        Generic("T".into()),
                        ASTIndex::none()
                    )],
                    index: ASTIndex::new(None, 1, 5)
                }]
            );
            assert_eq!(next_i, 7);
        } else {
            panic!()
        }
    }

    #[test]
    fn test_two_parameters_variant() {
        let parse_result =
            parse_variants("Something(v: T, v1: T1)}", &["T".into(), "T1".into()], 0);

        if let Some((variants, next_i)) = parse_result {
            assert_eq!(
                variants,
                vec![ASTEnumVariantDef {
                    name: "Something".into(),
                    parameters: vec![
                        ASTParameterDef::new("v", Generic("T".into()), ASTIndex::none()),
                        ASTParameterDef::new("v1", Generic("T1".into()), ASTIndex::none()),
                    ],
                    index: ASTIndex::new(None, 1, 10)
                }]
            );
            assert_eq!(next_i, 11);
        } else {
            panic!()
        }
    }

    #[test]
    fn test_custom_type_variant_argument() {
        let parse_result = parse_variants("Full(head: T, tail: List<T>)}", &["T".into()], 0);

        if let Some((variants, next_i)) = parse_result {
            assert_eq!(
                variants,
                vec![ASTEnumVariantDef {
                    name: "Full".into(),
                    parameters: vec![
                        ASTParameterDef::new("head", Generic("T".into()), ASTIndex::none()),
                        ASTParameterDef::new(
                            "tail",
                            ASTType::Custom {
                                name: "List".into(),
                                param_types: vec![Generic("T".into())],
                                index: ASTIndex::none()
                            },
                            ASTIndex::none()
                        ),
                    ],
                    index: ASTIndex::new(None, 1, 5)
                }]
            );
            assert_eq!(next_i, 14);
        } else {
            panic!()
        }
    }

    #[test]
    fn test_variants() {
        let parse_result = parse_variants(
            "
            Empty,
            Some(value: T)
        }",
            &["T".into()],
            0,
        );

        let some = ASTEnumVariantDef {
            name: "Some".into(),
            parameters: vec![ASTParameterDef {
                name: "value".into(),
                ast_type: Generic("T".into()),
                ast_index: ASTIndex::none(),
            }],
            index: ASTIndex::new(None, 2, 18),
        };

        let empty = ASTEnumVariantDef {
            name: "Empty".into(),
            parameters: vec![],
            index: ASTIndex::new(None, 2, 18),
        };

        assert_eq!(parse_result, Some((vec![empty, some], 9)));
    }

    #[test]
    // It's for a use case that caused a bug
    fn test_no_variants_with_a_function_call() {
        let parse_result = parse_variants("Empty} hello(15);", &["T".into()], 0);

        let empty = ASTEnumVariantDef {
            name: "Empty".into(),
            parameters: vec![],
            index: ASTIndex::new(None, 1, 6),
        };

        assert_eq!(parse_result, Some((vec![empty], 2)));
    }

    #[test]
    fn test_option() {
        let source = "enum Option<T> {
            Empty,
            Some(value: T)
        }";
        let parse_result = try_parse(source);

        if let Some((_name, type_parameters, next_i)) = parse_result {
            let parse_result = parse_variants(source, &type_parameters, next_i);

            if let Some((variants, next_i)) = parse_result {
                let some = ASTEnumVariantDef {
                    name: "Some".into(),
                    parameters: vec![ASTParameterDef {
                        name: "value".into(),
                        ast_type: Generic("T".into()),
                        ast_index: ASTIndex::none(),
                    }],
                    index: ASTIndex::new(None, 2, 18),
                };

                let empty = ASTEnumVariantDef {
                    name: "Empty".into(),
                    parameters: vec![],
                    index: ASTIndex::new(None, 2, 18),
                };

                assert_eq!(vec![empty, some], variants);
                assert_eq!(15, next_i);
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_list() {
        let source = "enum List<T> {
              Full(head: T, tail: List<T>),
              Empty
        }";
        let parse_result = try_parse(source);

        if let Some((_name, type_parameters, next_i)) = parse_result {
            let parse_result = parse_variants(source, &type_parameters, next_i);

            if let Some((variants, next_i)) = parse_result {
                let full = ASTEnumVariantDef {
                    name: "Full".into(),
                    parameters: vec![
                        ASTParameterDef {
                            name: "head".into(),
                            ast_type: Generic("T".into()),
                            ast_index: ASTIndex::none(),
                        },
                        ASTParameterDef {
                            name: "tail".into(),
                            ast_type: ASTType::Custom {
                                name: "List".into(),
                                param_types: vec![Generic("T".into())],
                                index: ASTIndex::none(),
                            },
                            ast_index: ASTIndex::none(),
                        },
                    ],
                    index: ASTIndex::new(None, 2, 19),
                };

                let empty = ASTEnumVariantDef {
                    name: "Empty".into(),
                    parameters: vec![],
                    index: ASTIndex::new(None, 2, 19),
                };

                assert_eq!(variants, vec![full, empty]);
                assert_eq!(next_i, 22);
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_either() {
        let source = "enum Either<L,R> {
            Left(l: L),
            Right(r: R)
        }";
        let parse_result = try_parse(source);

        if let Some((_name, type_parameters, next_i)) = parse_result {
            let parse_result = parse_variants(source, &type_parameters, next_i);

            if let Some((variants, next_i)) = parse_result {
                let left = ASTEnumVariantDef {
                    name: "Left".into(),
                    parameters: vec![ASTParameterDef::new(
                        "l",
                        Generic("L".into()),
                        ASTIndex::none(),
                    )],
                    index: ASTIndex::new(None, 2, 17),
                };

                let right = ASTEnumVariantDef {
                    name: "Right".into(),
                    parameters: vec![ASTParameterDef::new(
                        "r",
                        Generic("R".into()),
                        ASTIndex::none(),
                    )],
                    index: ASTIndex::new(None, 2, 17),
                };

                assert_eq!(variants, vec![left, right]);
                assert_eq!(type_parameters, vec!["L", "R"]);
                assert_eq!(next_i, 22);
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    fn try_parse(source: &str) -> Option<(String, Vec<String>, usize)> {
        let parser = get_parser(source);

        let sut = EnumParser::new(&parser);

        sut.try_parse()
    }

    fn try_parse_enum(source: &str) -> Option<(ASTEnumDef, usize)> {
        let parser = get_parser(source);

        let sut = EnumParser::new(&parser);

        sut.try_parse_enum()
    }

    fn parse_variants(
        source: &str,
        type_parameters: &[String],
        n: usize,
    ) -> Option<(Vec<ASTEnumVariantDef>, usize)> {
        let parser = get_parser(source);

        let sut = EnumParser::new(&parser);

        sut.parse_variants(type_parameters, n)
    }

    fn try_parse_variant(source: &str, type_parameters: &[String]) -> Option<TokensMatcherResult> {
        let parser = get_parser(source);

        let matcher = EnumParser::variant_matcher("", Quantifier::One, type_parameters);
        matcher.match_tokens(&parser, 0)
    }
}
