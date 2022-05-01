use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, TokenKind};
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
        let param_types = Self::types_matcher();

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Enum));
        matcher.add_alphanumeric();
        matcher.add_matcher(param_types);

        matcher.match_tokens(self.parser, 0)
            .map(|result| (result.values().first().unwrap().clone(), result.group_values("types").clone(), result.next_n()))
    }

    fn types_matcher() -> TokensMatcher {
        let mut param_types = TokensMatcher::new("types", Quantifier::AtMostOne);
        param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open));
        param_types.add_alphanumeric();
        param_types.start_group("type", Quantifier::ZeroOrMore);
        param_types.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        param_types.add_alphanumeric();
        param_types.end_group();
        param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close));
        param_types
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::get_parser;
    use super::*;

    #[test]
    fn empty_enum() {
        let parse_result = try_parse("enum Option<T> {
        }");

        assert_eq!(Some(("Option".into(), vec!["T".into()], 5)), parse_result);
    }

    #[test]
    fn test() {
        let parse_result = try_parse("enum Option<T> {
            Empty,
            Some(value : T)
        }");

        assert_eq!(Some(("Option".into(), vec!["T".into()], 5)), parse_result);
    }

    fn try_parse(source: &str) -> Option<(String, Vec<String>, usize)> {
        let parser = get_parser(source);

        let sut = EnumParser::new(&parser);

        sut.try_parse()
    }
}

