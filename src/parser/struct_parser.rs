use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, TokenKind};
use crate::parser::ast::{ASTStructDef, ASTStructPropertyDef};
use crate::parser::enum_parser::EnumParser;
use crate::parser::matchers::param_types_matcher;
use crate::parser::ParserTrait;
use crate::parser::tokens_matcher::{Quantifier, TokensMatcher, TokensMatcherTrait};
use crate::parser::type_parser::TypeParser;

pub struct StructParser<'a> {
    parser: &'a dyn ParserTrait,
}

impl<'a> StructParser<'a> {
    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse(&self) -> Option<(String, Vec<String>, usize)> {
        let param_types = param_types_matcher();

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Struct));
        matcher.add_alphanumeric();
        matcher.add_matcher(param_types);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        matcher.match_tokens(self.parser, 0)
            .map(|result| {
                let x = result.group_values("type");
                (result.values().first().unwrap().clone(), x, self.parser.get_i() + result.next_n())
            })
    }

    pub fn try_parse_struct(&self) -> Option<(ASTStructDef, usize)> {
        if let Some((name, type_parameters, next_i)) = self.try_parse() {
            if let Some((properties, next_i)) = self.parse_properties(&type_parameters, next_i - self.parser.get_i()) {
                return Some((ASTStructDef { name, type_parameters, properties }, next_i));
            }
        }
        None
    }

    fn properties_matcher(name: &str, quantifier: Quantifier, type_parameters: &[String]) -> TokensMatcher {
        let mut matcher = TokensMatcher::new(name, quantifier);
        matcher.start_group("parameter_list", Quantifier::One);
        matcher.add_matcher(EnumParser::parameter_matcher(type_parameters));
        matcher.end_group();
        matcher.start_group("parameter_list", Quantifier::ZeroOrMore);
        matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
        matcher.add_matcher(EnumParser::parameter_matcher(type_parameters));
        matcher.end_group();
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close));
        matcher
    }

    pub fn parse_properties(&self, type_parameters: &Vec<String>, n: usize) -> Option<(Vec<ASTStructPropertyDef>, usize)> {
        if let Some(result) = Self::properties_matcher("properties", Quantifier::One, type_parameters).match_tokens(self.parser, n) {
            let parameters_s = result.group_values("parameter");
            let type_result = result.group_results("parameter_type");


            let mut parameters = Vec::new();
            for i in 0..parameters_s.len() {
                //println!("parsing parameter {}, n: {}", parameters_s.get(i).unwrap(), n);
                let parser = *type_result.get(i).unwrap();
                let type_parser = TypeParser::new(parser);
                let name = parameters_s.get(i).unwrap().clone();
                if let Some((type_ref, next_i)) = type_parser.try_parse_type_ref(0, type_parameters) {
                    parameters.push(ASTStructPropertyDef { name: name, type_ref });
                } else {
                    self.parser.panic(&format!("Cannot parse type for property {}:", name));
                    panic!();
                }
            }

            Some((parameters, self.parser.get_i() + result.next_n()))
        } else {
            panic!();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{ASTStructDef, ASTStructPropertyDef, ASTTypeRef, BuiltinTypeKind};
    use crate::parser::ast::ASTType::Builtin;
    use crate::parser::struct_parser::StructParser;
    use crate::parser::test_utils::get_parser;

    #[test]
    fn test() {
        let parse_result = try_parse_struct("struct Point {
            x: i32,
            y: i32
        }");

        let x = ASTStructPropertyDef {
            name: "x".into(),
            type_ref: ASTTypeRef { ast_type: Builtin(BuiltinTypeKind::ASTI32), ast_ref: false },
        };

        let y = ASTStructPropertyDef {
            name: "y".into(),
            type_ref: ASTTypeRef { ast_type: Builtin(BuiltinTypeKind::ASTI32), ast_ref: false },
        };

        assert_eq!(Some((ASTStructDef {
            name: "Point".to_string(),
            type_parameters: vec![],
            properties: vec![x, y],
        }, 11)), parse_result);
    }

    fn try_parse_struct(source: &str) -> Option<(ASTStructDef, usize)> {
        let parser = get_parser(source);

        let sut = StructParser::new(&parser);

        sut.try_parse_struct()
    }
}