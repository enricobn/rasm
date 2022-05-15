use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, TokenKind};
use crate::parser::ast::ASTEnumDef;
use crate::parser::matchers::types_matcher;
use crate::parser::ParserTrait;
use crate::parser::tokens_matcher::{Quantifier, TokensMatcher, TokensMatcherTrait};
use crate::parser::type_params_parser::TypeParamsParser;

/*
lazy_static! {
pub static ref TYPES_MATCHER: TokensMatcher = {
    let mut param_types = TokensMatcher::new("types", Quantifier::AtMostOne);
    param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open));
    param_types.add_alphanumeric();
    param_types.start_group("type", Quantifier::ZeroOrMore);
    param_types.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
    param_types.add_alphanumeric();
    param_types.end_group();
    param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close));
    param_types
    };
}

 */

pub struct EnumParser<'a> {
    parser: &'a dyn ParserTrait,
}

impl<'a> EnumParser<'a> {
    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse(&self) -> Option<(String, Vec<String>, usize)> {
        let param_types = types_matcher();

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();
        matcher.add_matcher(param_types);

        matcher.match_tokens(self.parser, 0)
            .map(|result| (result.values().first().unwrap().clone(), result.group_values("type").clone(), result.next_n()))
    }

    pub fn try_parse_enum(&self) -> Option<(ASTEnumDef, usize)> {
        let param_types = types_matcher();

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();
        matcher.add_matcher(param_types);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        if let Some(result) = matcher.match_tokens(self.parser, 0) {
            let name = result.values().first().unwrap().clone();
            let type_parameters = result.group_values("type").clone().into_iter().collect();

            let mut matcher = TokensMatcher::default();
            matcher.start_group("variants", Quantifier::AtLeastOne);
            matcher.add_matcher(EnumParser::variant_matcher("variant", Quantifier::One));
            matcher.start_group("variant_", Quantifier::ZeroOrMore);
            matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
            matcher.add_matcher(EnumParser::variant_matcher("variant", Quantifier::One));
            matcher.end_group();
            matcher.end_group();
            matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close));
            if let Some(variants_result) = matcher.match_tokens(self.parser, result.next_n()) {
                println!("***** variants {:?}", variants_result);
                Some((ASTEnumDef { name, type_parameters, variants: vec![] }, variants_result.next_n()))
            } else {
                println!("***** NO variants");
                Some((ASTEnumDef { name, type_parameters, variants: vec![] }, result.next_n()))
            }
        } else {
            None
        }
    }

    fn variant_matcher(name: &str, quantifier: Quantifier) -> TokensMatcher {
        let mut matcher = TokensMatcher::new(name, quantifier);
        matcher.add_alphanumeric();
        matcher.start_group("parameters", Quantifier::AtMostOne);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open));
        matcher.start_group("parameter", Quantifier::One);
        matcher.add_alphanumeric();
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Colon));
        matcher.add_alphanumeric();
        matcher.end_group();
        matcher.start_group("parameter", Quantifier::AtMostOne);
        matcher.add_alphanumeric();
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Colon));
        matcher.add_alphanumeric();
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        matcher.end_group();
        matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close));
        matcher.end_group();
        matcher
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{ASTEnumVariantDef, ASTParameterDef, ASTTypeRef};
    use crate::parser::ast::ASTType::ParametricType;
    use crate::parser::test_utils::get_parser;
    use crate::parser::tokens_matcher::TokensMatcherResult;
    use super::*;

    #[test]
    fn empty_enum() {
        let parse_result = try_parse("enum Option<T> {
        }");

        assert_eq!(Some(("Option".into(), vec!["T".into()], 5)), parse_result);
    }

    #[test]
    fn test() {
        let parse_result = try_parse_enum("enum Option<T> {
            Empty,
            Some(value: i32)
        }");

        let some = ASTEnumVariantDef {
            name: "Some".into(),
            parameters: vec![ASTParameterDef {
                name: "value".into(),
                type_ref: ASTTypeRef { ast_type: ParametricType("T".into()), ast_ref: false },
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
        }, 5)), parse_result);
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
            assert_eq!(&vec!["value", "T"], result.group_values("parameter"))
        } else {
            panic!()
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

    fn try_parse_variant(source: &str) -> Option<TokensMatcherResult> {
        let parser = get_parser(source);

        let sut = EnumParser::new(&parser);

        let matcher = EnumParser::variant_matcher("", Quantifier::One);
        matcher.match_tokens(&parser, 0)
    }
}

