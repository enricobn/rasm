use crate::lexer::tokens::{KeywordKind, TokenKind};
use crate::parser::ParserTrait;
use crate::parser::type_params_parser::TypeParamsParser;

pub struct EnumParser<'a> {
    parser: &'a dyn ParserTrait
}

impl <'a> EnumParser<'a> {

    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse(&self) -> Option<(String, Vec<String>, usize)> {
        if let Some(token) = self.parser.get_token() {
            if let TokenKind::KeyWord(KeywordKind::Enum) = &token.kind {
                if let Some(token2) = self.parser.next_token() {
                    if let TokenKind::AlphaNumeric(name) = &token2.kind {
                        let type_params_parser = TypeParamsParser::new(self.parser);
                        if let Some((type_params, next_i)) = type_params_parser.try_parse(self.parser.get_i() + 2) {
                            return Some((name.into(), type_params, next_i));
                        } else {
                            return Some((name.into(), vec![], self.parser.get_i() + 2));
                        }
                    }
                }
            }
        }
        None
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

