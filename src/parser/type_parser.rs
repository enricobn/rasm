use crate::lexer::tokens::{KeywordKind, TokenKind};
use crate::parser::ast::{ASTType, BuiltinTypeKind};
use crate::parser::ast::ASTType::{BuiltinType, CustomType, ParametricType};
use crate::parser::ParserTrait;
use crate::parser::type_params_parser::TypeParamsParser;

pub struct TypeParser<'a> {
    parser: &'a dyn ParserTrait,
}

impl<'a> TypeParser<'a> {
    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse(&self, n: usize, context_param_types: &[String]) -> Option<(ASTType, usize)> {
        if let Some(kind) = self.parser.get_token_kind_n(n) {
            let next_i = self.parser.get_i() + n + 1;
            if let TokenKind::AlphaNumeric(type_name) = kind {
                if type_name == "i32" {
                    Some((BuiltinType(BuiltinTypeKind::ASTI32), next_i))
                } else if type_name == "str" {
                    Some((BuiltinType(BuiltinTypeKind::ASTString), next_i))
                } else if context_param_types.contains(type_name) {
                    Some((ParametricType(type_name.into()), next_i))
                } else {
                    let (param_types, next_i) = if let Some((param_types, next_i)) = TypeParamsParser::new(self.parser).try_parse(n + 1) {
                        (param_types, next_i)
                    } else {
                        (vec![], next_i)
                    };

                    Some((CustomType { name: type_name.into(), param_types }, next_i))
                }
            } else if let TokenKind::KeyWord(KeywordKind::Fn) = kind {
                Some((BuiltinType(BuiltinTypeKind::Lambda), self.parser.get_i() + n + 1))
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::get_parser;
    use super::*;

    #[test]
    fn test_i32() {
        let parse_result = try_parse("i32");
        assert_eq!(Some((BuiltinType(BuiltinTypeKind::ASTI32), 1)), parse_result);
    }

    #[test]
    fn test_str() {
        let parse_result = try_parse("str");
        assert_eq!(Some((BuiltinType(BuiltinTypeKind::ASTString), 1)), parse_result);
    }

    #[test]
    fn test_lambda() {
        let parse_result = try_parse("fn");
        assert_eq!(Some((BuiltinType(BuiltinTypeKind::Lambda), 1)), parse_result);
    }

    #[test]
    fn test_custom_type() {
        let parse_result = try_parse("Dummy<T,T1>");
        assert_eq!(Some((CustomType { name: "Dummy".into(),  param_types: vec!["T".into(), "T1".into()]}, 6)), parse_result);
    }

    #[test]
    fn test_param_type() {
        let parse_result = try_parse_with_context("T", &["T".into()]);
        assert_eq!(Some((ParametricType("T".into()), 1)), parse_result);
    }

    #[test]
    fn test_not_param_type() {
        let parse_result = try_parse_with_context("T", &["F".into()]);
        assert_eq!(Some((CustomType { name: "T".into(),  param_types: vec![]}, 1)), parse_result);
    }

    fn try_parse(source: &str) -> Option<(ASTType, usize)> {
        try_parse_with_context(source, &[])
    }

    fn try_parse_with_context(source: &str, context: &[String]) -> Option<(ASTType, usize)> {
        let parser = get_parser(source);

        let sut = TypeParser::new(&parser);

        sut.try_parse(0, context)
    }

}