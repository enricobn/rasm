use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, Token, TokenKind};
use crate::parser::ast::ASTModifiers;
use crate::parser::matchers::{generic_types_matcher, modifiers_matcher};
use crate::parser::tokens_matcher::{TokensMatcher, TokensMatcherTrait};
use crate::parser::ParserTrait;

pub struct StructParser {
    matcher: TokensMatcher,
}

impl StructParser {
    pub fn new() -> Self {
        let mut matcher = TokensMatcher::default();
        matcher.add_matcher(modifiers_matcher());
        matcher.add_kind(TokenKind::KeyWord(KeywordKind::Struct));
        matcher.add_alphanumeric();
        matcher.add_matcher(generic_types_matcher());
        matcher.add_kind(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open));

        Self { matcher }
    }

    pub fn try_parse(
        &self,
        parser: &dyn ParserTrait,
    ) -> Option<(Token, Vec<String>, ASTModifiers, usize)> {
        self.matcher.match_tokens(parser, 0).map(|result| {
            let mut i = 1;
            let modifiers = if result.group_tokens("modifiers").is_empty() {
                ASTModifiers::private()
            } else {
                i += 1;
                ASTModifiers::public()
            };
            let param_types = result.group_alphas("type");
            (
                result.tokens().get(i).unwrap().clone(),
                param_types,
                modifiers,
                parser.get_i() + result.next_n(),
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::tokens::{BracketKind, BracketStatus, TokenKind};
    use crate::parser::ast::ASTType::{ASTBuiltinType, ASTGenericType};
    use crate::parser::ast::{
        ASTBuiltinTypeKind, ASTModifiers, ASTPosition, ASTStructDef, ASTStructPropertyDef,
    };
    use crate::parser::properties_parser::parse_properties;
    use crate::parser::struct_parser::StructParser;
    use crate::parser::test_utils::get_parser;
    use crate::parser::{ParserError, ParserTrait};

    impl StructParser {
        pub fn try_parse_struct(
            &self,
            parser: &dyn ParserTrait,
        ) -> Option<(ASTStructDef, Vec<ParserError>, usize)> {
            if let Some((token, type_parameters, modifiers, next_i)) = self.try_parse(parser) {
                if let Some(name) = token.alpha() {
                    let (properties, errors, new_n) = parse_properties(
                        parser,
                        &type_parameters,
                        next_i - parser.get_i(),
                        TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close),
                        true,
                    );
                    return Some((
                        ASTStructDef {
                            name,
                            type_parameters,
                            properties,
                            position: token.position,
                            modifiers,
                            attribute_macros: Vec::new(),
                        },
                        errors,
                        new_n - parser.get_i(),
                    ));
                }
            }
            None
        }
    }

    #[test]
    fn test() {
        let parse_result = try_parse_struct(
            "struct Point {
            x: int,
            y: int
        }",
        );

        let x = ASTStructPropertyDef {
            name: "x".into(),
            ast_type: ASTBuiltinType(ASTBuiltinTypeKind::ASTIntegerType),
            position: ASTPosition::new(2, 13),
        };

        let y = ASTStructPropertyDef {
            name: "y".into(),
            ast_type: ASTBuiltinType(ASTBuiltinTypeKind::ASTIntegerType),
            position: ASTPosition::new(3, 13),
        };

        if let Some((struct_def, errors, n)) = parse_result {
            assert!(almost_equal_structs(
                &struct_def,
                &ASTStructDef {
                    name: "Point".to_string(),
                    type_parameters: vec![],
                    properties: vec![x, y],
                    position: ASTPosition::new(1, 8),
                    modifiers: ASTModifiers::private(),
                    attribute_macros: vec![],
                }
            ));
            assert_eq!(n, 11);
            assert!(errors.is_empty());
        } else {
            panic!();
        }
    }

    #[test]
    fn test_parametric() {
        let parse_result = try_parse_struct(
            "struct EnumerateEntry<T> {
            index: int,
            value: T
        }",
        );

        let x = ASTStructPropertyDef {
            name: "index".into(),
            ast_type: ASTBuiltinType(ASTBuiltinTypeKind::ASTIntegerType),
            position: ASTPosition::new(2, 13),
        };

        let y = ASTStructPropertyDef {
            name: "value".into(),
            ast_type: ASTGenericType(ASTPosition::new(3, 21), "T".into(), Vec::new()),
            position: ASTPosition::new(3, 13),
        };

        if let Some((struct_def, errors, n)) = parse_result {
            assert!(almost_equal_structs(
                &struct_def,
                &ASTStructDef {
                    name: "EnumerateEntry".to_string(),
                    type_parameters: vec!["T".into()],
                    properties: vec![x, y],
                    position: ASTPosition::new(1, 8),
                    modifiers: ASTModifiers::private(),
                    attribute_macros: vec![],
                }
            ));
            assert_eq!(n, 14);
            assert!(errors.is_empty());
        } else {
            panic!()
        }
    }

    fn almost_equal_structs(a: &ASTStructDef, b: &ASTStructDef) -> bool {
        a.name == b.name
            && a.type_parameters == b.type_parameters
            && a.properties == b.properties
            && a.modifiers == b.modifiers
            && a.position == b.position
    }

    fn try_parse_struct(source: &str) -> Option<(ASTStructDef, Vec<ParserError>, usize)> {
        let parser = get_parser(source);

        let sut = StructParser::new();

        sut.try_parse_struct(&parser)
    }
}
