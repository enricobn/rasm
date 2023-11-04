use crate::lexer::tokens::{
    BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind,
};
use crate::parser::ast::{ASTStructDef, ASTStructPropertyDef};
use crate::parser::enum_parser::EnumParser;
use crate::parser::matchers::generic_types_matcher;
use crate::parser::tokens_matcher::{Quantifier, TokensMatcher, TokensMatcherTrait};
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserTrait;

pub struct StructParser<'a> {
    parser: &'a dyn ParserTrait,
}

impl<'a> StructParser<'a> {
    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse(&self) -> Option<(Token, Vec<String>, usize)> {
        let param_types_matcher = generic_types_matcher();

        let mut matcher = TokensMatcher::default();
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Struct));
        matcher.add_alphanumeric();
        matcher.add_matcher(param_types_matcher);
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        matcher.match_tokens(self.parser, 0).map(|result| {
            let param_types = result.group_alphas("type");
            (
                result.tokens().get(1).unwrap().clone(),
                param_types,
                self.parser.get_i() + result.next_n(),
            )
        })
    }

    pub fn try_parse_struct(&self) -> Result<Option<(ASTStructDef, usize)>, String> {
        if let Some((token, type_parameters, next_i)) = self.try_parse() {
            if let Some(name) = token.alpha() {
                if let Some((properties, next_i)) =
                    self.parse_properties(&type_parameters, &name, next_i - self.parser.get_i())?
                {
                    return Ok(Some((
                        ASTStructDef {
                            name,
                            type_parameters,
                            properties,
                            index: self.parser.get_index(0),
                        },
                        next_i,
                    )));
                }
            } else {
                return Err(format!(
                    "Expected alphanumeric, got {:?}: {}",
                    token,
                    self.parser.get_index(0)
                ));
            }
        }
        Ok(None)
    }

    fn properties_matcher(
        name: &str,
        quantifier: Quantifier,
        type_parameters: &[String],
    ) -> TokensMatcher {
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

    pub fn parse_properties(
        &self,
        generic_types: &[String],
        name: &str,
        n: usize,
    ) -> Result<Option<(Vec<ASTStructPropertyDef>, usize)>, String> {
        if let Some(result) = Self::properties_matcher("properties", Quantifier::One, generic_types)
            .match_tokens(self.parser, n)
        {
            let parameters_tokens = result.group_tokens("parameter");
            let type_result = result.group_results("parameter_type");

            let mut parameters = Vec::new();
            for i in 0..parameters_tokens.len() {
                //println!("parsing parameter {}, n: {}", parameters_s.get(i).unwrap(), n);
                let parser = *type_result.get(i).unwrap();
                let type_parser = TypeParser::new(parser);
                let token = parameters_tokens.get(i).unwrap().clone();
                if let Some((ast_type, _next_i)) =
                    type_parser.try_parse_ast_type(0, generic_types)?
                {
                    parameters.push(ASTStructPropertyDef {
                        name: token.alpha().unwrap(),
                        ast_type,
                        index: token.index(),
                    });
                } else {
                    return Err(format!(
                        "Cannot parse type for property {:?}: {}",
                        token,
                        self.parser.get_index(0)
                    ));
                }
            }

            Ok(Some((parameters, self.parser.get_i() + result.next_n())))
        } else {
            Err(format!(
                "No properties for struct {name} : {}",
                self.parser.get_index(n)
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::ASTType::{Builtin, Generic};
    use crate::parser::ast::{ASTIndex, ASTStructDef, ASTStructPropertyDef, BuiltinTypeKind};
    use crate::parser::struct_parser::StructParser;
    use crate::parser::test_utils::get_parser;

    #[test]
    fn test() {
        let parse_result = try_parse_struct(
            "struct Point {
            x: i32,
            y: i32
        }",
        );

        let x = ASTStructPropertyDef {
            name: "x".into(),
            ast_type: Builtin(BuiltinTypeKind::I32),
            index: ASTIndex::new(None, 2, 14),
        };

        let y = ASTStructPropertyDef {
            name: "y".into(),
            ast_type: Builtin(BuiltinTypeKind::I32),
            index: ASTIndex::new(None, 3, 14),
        };

        assert_eq!(
            parse_result,
            Some((
                ASTStructDef {
                    name: "Point".to_string(),
                    type_parameters: vec![],
                    properties: vec![x, y],
                    index: ASTIndex::new(None, 1, 7)
                },
                11
            ))
        );
    }

    #[test]
    fn test_parametric() {
        let parse_result = try_parse_struct(
            "struct EnumerateEntry<T> {
            index: i32,
            value: T
        }",
        );

        let x = ASTStructPropertyDef {
            name: "index".into(),
            ast_type: Builtin(BuiltinTypeKind::I32),
            index: ASTIndex::new(None, 2, 18),
        };

        let y = ASTStructPropertyDef {
            name: "value".into(),
            ast_type: Generic("T".into()),
            index: ASTIndex::new(None, 3, 18),
        };

        assert_eq!(
            parse_result,
            Some((
                ASTStructDef {
                    name: "EnumerateEntry".to_string(),
                    type_parameters: vec!["T".into()],
                    properties: vec![x, y],
                    index: ASTIndex::new(None, 1, 7)
                },
                14
            )),
        );
    }

    fn try_parse_struct(source: &str) -> Option<(ASTStructDef, usize)> {
        let parser = get_parser(source);

        let sut = StructParser::new(&parser);

        sut.try_parse_struct().unwrap()
    }
}
