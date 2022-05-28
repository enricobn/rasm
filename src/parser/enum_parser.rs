use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, TokenKind};
use crate::parser::ast::{ASTEnumDef, ASTEnumVariantDef, ASTParameterDef};
use crate::parser::matchers::param_types_matcher;
use crate::parser::ParserTrait;
use crate::parser::tokens_matcher::{Quantifier, TokensMatcher, TokensMatcherResult, TokensMatcherTrait};
use crate::parser::type_parser::TypeParser;

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

        matcher.match_tokens(self.parser, 0)
            .map(|result| {
                let x = result.group_values("type");
                (result.values().first().unwrap().clone(), x, self.parser.get_i() + result.next_n())
            })
    }

    pub fn try_parse_enum(&self) -> Option<(ASTEnumDef, usize)> {
        let param_types = param_types_matcher();

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();
        matcher.add_matcher(param_types);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        if let Some(result) = matcher.match_tokens(self.parser, 0) {
            let name = result.values().first().unwrap().clone();
            let type_parameters = result.group_values("type");

            if let Some((variants, next_i)) = self.parse_variants(&type_parameters, result.next_n()) {
                Some((ASTEnumDef { name, type_parameters, variants }, next_i))
            } else {
                panic!("***** NO variants");
            }
        } else {
            None
        }
    }

    pub fn parse_variants(&self, type_parameters: &[String], n: usize) -> Option<(Vec<ASTEnumVariantDef>, usize)> {
        let mut matcher = TokensMatcher::default();
        matcher.add_matcher(EnumParser::variant_matcher("variant", Quantifier::One));
        matcher.start_group("variant_", Quantifier::ZeroOrMore);
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        matcher.add_matcher(EnumParser::variant_matcher("variant", Quantifier::One));
        matcher.end_group();
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close));

        if let Some(variants_result) = matcher.match_tokens(self.parser, n) {
            let variant_results: Vec<&TokensMatcherResult> = variants_result.group_results("variant");

            let variants = variant_results.iter().map(|result| {
                let name = result.values().first().unwrap();

                let parameters_s = result.group_values("parameter");
                let type_result = result.group_results("parameter_type");

                let parameters = if parameters_s.is_empty() {
                    Vec::new()
                } else {
                    let type_parser = TypeParser::new(*type_result.first().unwrap());
                    if let Some((type_ref, next_i)) = type_parser.try_parse_type_ref(0, type_parameters) {
                        vec![ASTParameterDef { name: parameters_s.first().unwrap().clone(), type_ref }]
                    } else {
                        self.parser.panic(&format!("Cannot parse type for enum variant {}:", name));
                        panic!();
                    }
                };

                ASTEnumVariantDef { name: name.clone(), parameters }
            }).collect();

            Some((variants, self.parser.get_i() + variants_result.next_n()))
        } else {
            None
        }
    }

    // TODO it seems to be more generic: it is a list of parameters surrounded by (), it can be used to match even function declarations
    fn variant_matcher(name: &str, quantifier: Quantifier) -> TokensMatcher {
        let mut matcher = TokensMatcher::new(name, quantifier);
        matcher.add_alphanumeric();
        matcher.start_group("parameters", Quantifier::AtMostOne);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open));
        matcher.start_group("parameter_list", Quantifier::One);
        matcher.add_matcher(EnumParser::parameter_matcher());
        matcher.end_group();
        matcher.start_group("parameter_list", Quantifier::ZeroOrMore);
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        matcher.add_matcher(EnumParser::parameter_matcher());
        matcher.end_group();
        matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close));
        matcher.end_group();
        matcher
    }

    // TODO ref and function ref
    fn parameter_matcher() -> TokensMatcher {
        let mut matcher = TokensMatcher::new("parameter", Quantifier::One);
        matcher.add_alphanumeric();
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Colon));
        matcher.start_group("parameter_type", Quantifier::One);
        matcher.add_alphanumeric();
        matcher.end_group();
        matcher
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{ASTEnumVariantDef, ASTParameterDef, ASTTypeRef};
    use crate::parser::ast::ASTType::Parametric;
    use crate::parser::test_utils::get_parser;
    use crate::parser::tokens_matcher::TokensMatcherResult;
    use super::*;

    #[test]
    fn empty_enum() {
        let parse_result = try_parse("enum Option<T> {
        }");

        assert_eq!(Some(("Option".into(), vec!["T".into()], 6)), parse_result);
    }

    #[test]
    fn test() {
        let parse_result = try_parse_enum("enum Option<T> {
            Empty,
            Some(value: T)
        }");

        let some = ASTEnumVariantDef {
            name: "Some".into(),
            parameters: vec![ASTParameterDef {
                name: "value".into(),
                type_ref: ASTTypeRef { ast_type: Parametric("T".into()), ast_ref: false },
            }],
        };

        let empty = ASTEnumVariantDef {
            name: "Empty".into(),
            parameters: vec![],
        };

        assert_eq!(Some((ASTEnumDef {
            name: "Option".to_string(),
            type_parameters: vec!["T".to_string()],
            variants: vec![
                empty, some,
            ],
        }, 15)), parse_result);
    }

    #[test]
    fn test_variant() {
        let parse_result = try_parse_variant("Empty");

        if let Some(result) = parse_result {
            assert_eq!("Empty", result.values().first().unwrap())
        } else {
            panic!()
        }

        let parse_result = try_parse_variant("Some(value: T)");

        if let Some(result) = parse_result {
            assert_eq!("Some", result.values().first().unwrap());
            assert_eq!(vec!["value"], result.group_values("parameter"));
            assert_eq!(vec!["T"], result.group_values("parameter_type"));
        } else {
            panic!()
        }

        let parse_result = try_parse_variant("Either(error: E, value: T)");

        if let Some(result) = parse_result {
            assert_eq!("Either", result.values().first().unwrap());
            assert_eq!(vec!["error", "value"], result.group_values("parameter"));
            assert_eq!(vec!["E", "T"], result.group_values("parameter_type"));
        } else {
            panic!()
        }
    }

    #[test]
    fn test_variants() {
        let parse_result = parse_variants("
            Empty,
            Some(value: T)
        }", &["T".into()], 0);

        let some = ASTEnumVariantDef {
            name: "Some".into(),
            parameters: vec![ASTParameterDef {
                name: "value".into(),
                type_ref: ASTTypeRef { ast_type: Parametric("T".into()), ast_ref: false },
            }],
        };

        let empty = ASTEnumVariantDef {
            name: "Empty".into(),
            parameters: vec![],
        };

        assert_eq!(parse_result, Some((vec![empty, some], 9)));
    }

    #[test]
    fn test_try_and_variants() {
        let source = "enum Option<T> {
            Empty,
            Some(value: T)
        }";
        let parse_result = try_parse(source);

        if let Some((_name, type_parameters, next_i)) = parse_result {
            println!("next_i {}", next_i);
            let parse_result = parse_variants(source, &type_parameters, next_i);

            if let Some((variants, next_i)) = parse_result {
                let some = ASTEnumVariantDef {
                    name: "Some".into(),
                    parameters: vec![ASTParameterDef {
                        name: "value".into(),
                        type_ref: ASTTypeRef { ast_type: Parametric("T".into()), ast_ref: false },
                    }],
                };

                let empty = ASTEnumVariantDef {
                    name: "Empty".into(),
                    parameters: vec![],
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

    fn parse_variants(source: &str, type_parameters: &[String], n: usize) -> Option<(Vec<ASTEnumVariantDef>, usize)> {
        let parser = get_parser(source);

        let sut = EnumParser::new(&parser);

        sut.parse_variants(type_parameters, n)
    }

    fn try_parse_variant(source: &str) -> Option<TokensMatcherResult> {
        let parser = get_parser(source);

        let matcher = EnumParser::variant_matcher("", Quantifier::One);
        matcher.match_tokens(&parser, 0)
    }
}

