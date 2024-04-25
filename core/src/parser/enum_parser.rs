use std::fmt::{Display, Formatter};

use linked_hash_map::LinkedHashMap;

use crate::lexer::tokens::{
    BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind,
};
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTModifiers, ASTNameSpace, ASTParameterDef,
};
use crate::parser::matchers::{generic_types_matcher, modifiers_matcher};
use crate::parser::tokens_matcher::{
    Quantifier, TokensMatcher, TokensMatcherResult, TokensMatcherTrait,
};
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserTrait;

pub struct EnumParser {
    matcher: TokensMatcher,
}

impl EnumParser {
    pub fn new() -> Self {
        let mut matcher = TokensMatcher::default();
        matcher.add_matcher(modifiers_matcher());
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();
        matcher.add_matcher(generic_types_matcher());
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        Self { matcher }
    }

    pub fn try_parse(
        &self,
        namespace: &ASTNameSpace,
        parser: &dyn ParserTrait,
    ) -> Option<(Token, Vec<String>, ASTModifiers, usize)> {
        self.matcher
            .match_tokens(namespace, parser, 0)
            .map(|result| {
                let param_types = result.group_alphas("type");
                let mut name_index = 1;
                let modifiers = if result.group_tokens("modifiers").is_empty() {
                    ASTModifiers::private()
                } else {
                    name_index += 1;
                    ASTModifiers::public()
                };
                let token = result.tokens().get(name_index).unwrap().clone();

                (
                    token,
                    param_types,
                    modifiers,
                    parser.get_i() + result.next_n(),
                )
            })
    }

    #[cfg(test)]
    pub fn try_parse_enum(
        &self,
        namespace: &ASTNameSpace,
        parser: &dyn ParserTrait,
    ) -> Result<Option<(ASTEnumDef, usize)>, String> {
        if let Some((token, type_parameters, modifiers, next_i)) = self.try_parse(namespace, parser)
        {
            if let Some((variants, next_i)) =
                self.parse_variants(namespace, parser, &type_parameters, next_i - parser.get_i())?
            {
                return Ok(Some((
                    ASTEnumDef {
                        namespace: namespace.clone(),
                        name: token.alpha().unwrap(),
                        type_parameters,
                        variants,
                        index: token.index().mv_left(token.alpha().unwrap().len()),
                        modifiers,
                    },
                    next_i,
                )));
            }
        }
        Ok(None)
    }

    pub fn parse_variants(
        &self,
        namespace: &ASTNameSpace,
        parser: &dyn ParserTrait,
        type_parameters: &[String],
        n: usize,
    ) -> Result<Option<(Vec<ASTEnumVariantDef>, usize)>, String> {
        //println!("parse_variants n {}", n);
        let mut enum_variants_matcher = TokensMatcher::default();
        enum_variants_matcher.add_matcher(EnumParser::variant_matcher(
            "variant",
            Quantifier::One,
            type_parameters,
        ));
        enum_variants_matcher.start_group("variant_", Quantifier::ZeroOrMore);
        enum_variants_matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        enum_variants_matcher.add_matcher(EnumParser::variant_matcher(
            "variant",
            Quantifier::One,
            type_parameters,
        ));
        enum_variants_matcher.end_group();
        enum_variants_matcher
            .add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close));

        if let Some(variants_result) = enum_variants_matcher.match_tokens(namespace, parser, n) {
            let variant_results: Vec<&TokensMatcherResult> =
                variants_result.group_results("variant");

            let variants = variant_results
                .iter()
                .map(|result| {
                    let name_token = result.tokens().first().unwrap();

                    let parameters_tokens = result.group_tokens("parameter");

                    let type_result = result.group_results("parameter_type");

                    let parameters: Result<Vec<ASTParameterDef>, String> = if parameters_tokens.is_empty() {
                        Ok(Vec::new())
                    } else {
                        let mut parameters = Vec::new();
                        for i in 0..parameters_tokens.len() {
                            //println!("parsing parameter {}, n: {}", parameters_s.get(i).unwrap(), n);
                            let parser = *type_result.get(i).unwrap();
                            let type_parser = TypeParser::new(parser);
                            if let Some((ast_type, _)) =
                                type_parser.try_parse_ast_type(namespace,0, type_parameters)?
                            {
                                if let Some(token) = parameters_tokens.get(i) {
                                    parameters.push(ASTParameterDef {
                                        name: token.alpha().unwrap(),
                                        ast_type,
                                        ast_index: token.index(),
                                    });
                                } else {
                                    return Err(parser.wrap_error(&format!(
                                        "Cannot parse type for enum variant, unexpected token {:?}:",
                                        parameters_tokens.get(i)
                                    )));
                                }
                            } else {
                                return Err(parser.wrap_error(&format!(
                                    "Cannot parse type for enum variant {:?}:",
                                    name_token
                                )));
                            }
                        }
                        Ok(parameters)
                    };

                    Ok(ASTEnumVariantDef {
                        name: name_token.alpha().unwrap(),
                        parameters: parameters?,
                        index: name_token.index(),
                    })
                })
                .collect::<Result<Vec<ASTEnumVariantDef>, String>>();

            //println!("parse_variants next_n: {}", variants_result.next_n());
            Ok(Some((variants?, parser.get_i() + variants_result.next_n())))
        } else {
            Ok(None)
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
    fn match_tokens(
        &self,
        namespace: &ASTNameSpace,
        parser: &dyn ParserTrait,
        n: usize,
    ) -> Option<TokensMatcherResult> {
        if let Some(TokenKind::AlphaNumeric(_name)) = parser.get_token_kind_n(n) {
            if let Some(TokenKind::Punctuation(PunctuationKind::Colon)) =
                parser.get_token_kind_n(n + 1)
            {
                let type_parser = TypeParser::new(parser);
                // I don't care of type since who evaluates this resul gets only the tokens
                if let Some((_ast_type, next_i)) = type_parser
                    .try_parse_ast_type(namespace, n + 2, &self.context_generic_types)
                    .unwrap()
                // TODO error handling
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
                        vec![parser.get_token_n(n).unwrap().clone()],
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
    use crate::utils::tests::test_namespace;

    use super::*;

    #[test]
    fn empty_enum() {
        let parse_result = try_parse(
            "enum Option<T> {
        }",
        );

        assert_eq!(
            parse_result,
            Some((
                Token::new(TokenKind::AlphaNumeric("Option".to_owned()), None, 1, 12),
                vec!["T".to_owned()],
                ASTModifiers::private(),
                6
            ))
        );
    }

    #[test]
    fn test() {
        let parse_result = try_parse_enum(
            &test_namespace(),
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
                ast_index: ASTIndex::new(None, 3, 23),
            }],
            index: ASTIndex::new(None, 3, 17),
        };

        let empty = ASTEnumVariantDef {
            name: "Empty".into(),
            parameters: vec![],
            index: ASTIndex::new(None, 2, 18),
        };

        assert_eq!(
            parse_result,
            Some((
                ASTEnumDef {
                    namespace: test_namespace(),
                    name: "Option".to_string(),
                    type_parameters: vec!["T".to_string()],
                    variants: vec![empty, some],
                    index: ASTIndex::new(None, 1, 6),
                    modifiers: ASTModifiers::private()
                },
                15
            )),
        );
    }

    #[test]
    fn test_empty_variant() {
        let parse_result = try_parse_variant("Empty", &[]);

        if let Some(result) = parse_result {
            assert_eq!("Empty", result.alphas().first().unwrap().clone())
        } else {
            panic!()
        }
    }

    #[test]
    // It's for a use case that caused a bug
    fn test_variant_with_a_function_call() {
        let parse_result = try_parse_variant("Empty } call(15);", &[]);

        if let Some(result) = parse_result {
            assert_eq!("Empty", result.alphas().first().unwrap().clone());
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
                        ASTIndex::new(None, 1, 11)
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
                        ASTParameterDef::new("v", Generic("T".into()), ASTIndex::new(None, 1, 12)),
                        ASTParameterDef::new(
                            "v1",
                            Generic("T1".into()),
                            ASTIndex::new(None, 1, 19)
                        ),
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
                        ASTParameterDef::new(
                            "head",
                            Generic("T".into()),
                            ASTIndex::new(None, 1, 10)
                        ),
                        ASTParameterDef::new(
                            "tail",
                            ASTType::Custom {
                                namespace: test_namespace(),
                                name: "List".into(),
                                param_types: vec![Generic("T".into())],
                                index: ASTIndex::none()
                            },
                            ASTIndex::new(None, 1, 19)
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
                ast_index: ASTIndex::new(None, 3, 23),
            }],
            index: ASTIndex::new(None, 3, 17),
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

        if let Some((_name, type_parameters, modifiers, next_i)) = parse_result {
            let parse_result = parse_variants(source, &type_parameters, next_i);

            if let Some((variants, next_i)) = parse_result {
                let some = ASTEnumVariantDef {
                    name: "Some".into(),
                    parameters: vec![ASTParameterDef {
                        name: "value".into(),
                        ast_type: Generic("T".into()),
                        ast_index: ASTIndex::new(None, 3, 23),
                    }],
                    index: ASTIndex::new(None, 3, 17),
                };

                let empty = ASTEnumVariantDef {
                    name: "Empty".into(),
                    parameters: vec![],
                    index: ASTIndex::new(None, 2, 18),
                };

                assert_eq!(vec![empty, some], variants);
                assert_eq!(modifiers, ASTModifiers::private());
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

        if let Some((_name, type_parameters, modifiers, next_i)) = parse_result {
            let parse_result = parse_variants(source, &type_parameters, next_i);

            if let Some((variants, next_i)) = parse_result {
                let full = ASTEnumVariantDef {
                    name: "Full".into(),
                    parameters: vec![
                        ASTParameterDef {
                            name: "head".into(),
                            ast_type: Generic("T".into()),
                            ast_index: ASTIndex::new(None, 2, 24),
                        },
                        ASTParameterDef {
                            name: "tail".into(),
                            ast_type: ASTType::Custom {
                                namespace: test_namespace(),
                                name: "List".into(),
                                param_types: vec![Generic("T".into())],
                                index: ASTIndex::new(None, 2, 39),
                            },
                            ast_index: ASTIndex::new(None, 2, 33),
                        },
                    ],
                    index: ASTIndex::new(None, 2, 19),
                };

                let empty = ASTEnumVariantDef {
                    name: "Empty".into(),
                    parameters: vec![],
                    index: ASTIndex::new(None, 3, 20),
                };

                assert_eq!(variants, vec![full, empty]);
                assert_eq!(modifiers, ASTModifiers::private());
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

        if let Some((_name, type_parameters, modifiers, next_i)) = parse_result {
            let parse_result = parse_variants(source, &type_parameters, next_i);

            if let Some((variants, next_i)) = parse_result {
                let left = ASTEnumVariantDef {
                    name: "Left".into(),
                    parameters: vec![ASTParameterDef::new(
                        "l",
                        Generic("L".into()),
                        ASTIndex::new(None, 2, 19),
                    )],
                    index: ASTIndex::new(None, 2, 17),
                };

                let right = ASTEnumVariantDef {
                    name: "Right".into(),
                    parameters: vec![ASTParameterDef::new(
                        "r",
                        Generic("R".into()),
                        ASTIndex::new(None, 3, 20),
                    )],
                    index: ASTIndex::new(None, 3, 18),
                };

                assert_eq!(variants, vec![left, right]);
                assert_eq!(type_parameters, vec!["L", "R"]);
                assert_eq!(modifiers, ASTModifiers::private());
                assert_eq!(next_i, 22);
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_pub() {
        let parse_result = try_parse_enum(
            &test_namespace(),
            "pub enum Option<T> {
            Empty,
            Some(value: T)
        }",
        );

        let some = ASTEnumVariantDef {
            name: "Some".into(),
            parameters: vec![ASTParameterDef {
                name: "value".into(),
                ast_type: Generic("T".into()),
                ast_index: ASTIndex::new(None, 3, 23),
            }],
            index: ASTIndex::new(None, 3, 17),
        };

        let empty = ASTEnumVariantDef {
            name: "Empty".into(),
            parameters: vec![],
            index: ASTIndex::new(None, 2, 18),
        };

        assert_eq!(
            parse_result,
            Some((
                ASTEnumDef {
                    namespace: test_namespace(),
                    name: "Option".to_string(),
                    type_parameters: vec!["T".to_string()],
                    variants: vec![empty, some],
                    index: ASTIndex::new(None, 1, 10),
                    modifiers: ASTModifiers::public()
                },
                16
            )),
        );
    }

    fn try_parse(source: &str) -> Option<(Token, Vec<String>, ASTModifiers, usize)> {
        let parser = get_parser(source);

        let sut = EnumParser::new();

        sut.try_parse(&test_namespace(), &parser)
    }

    fn try_parse_enum(namespace: &ASTNameSpace, source: &str) -> Option<(ASTEnumDef, usize)> {
        let parser = get_parser(source);

        let sut = EnumParser::new();

        sut.try_parse_enum(namespace, &parser).unwrap()
    }

    fn parse_variants(
        source: &str,
        type_parameters: &[String],
        n: usize,
    ) -> Option<(Vec<ASTEnumVariantDef>, usize)> {
        let parser = get_parser(source);

        let sut = EnumParser::new();

        sut.parse_variants(&test_namespace(), &parser, type_parameters, n)
            .unwrap()
    }

    fn try_parse_variant(source: &str, type_parameters: &[String]) -> Option<TokensMatcherResult> {
        let parser = get_parser(source);

        let matcher = EnumParser::variant_matcher("", Quantifier::One, type_parameters);
        matcher.match_tokens(&test_namespace(), &parser, 0)
    }
}
