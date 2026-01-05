use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, Token, TokenKind};
use crate::parser::ParserTrait;
use crate::parser::ast::ASTModifiers;
use crate::parser::matchers::{generic_types_matcher, modifiers_matcher};
use crate::parser::modifiers_parser::try_parse_ast_modifiers_tokens;
use crate::parser::tokens_matcher::{TokensMatcher, TokensMatcherTrait};

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
        self.matcher.match_tokens(parser, 0).and_then(|result| {
            let modifiers_tokens = result.group_tokens("modifiers");

            if let Ok((modifiers, new_index)) = try_parse_ast_modifiers_tokens(modifiers_tokens) {
                let param_types = result.group_alphas("type");
                Some((
                    result.tokens().get(new_index + 1).unwrap().clone(),
                    param_types,
                    modifiers,
                    parser.get_i() + result.next_n(),
                ))
            } else {
                None
            }
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
            modifiers: None,
        };

        let y = ASTStructPropertyDef {
            name: "y".into(),
            ast_type: ASTBuiltinType(ASTBuiltinTypeKind::ASTIntegerType),
            position: ASTPosition::new(3, 13),
            modifiers: None,
        };

        if let Some((struct_def, errors, n)) = parse_result {
            assert!(almost_equal_structs(
                &struct_def,
                &ASTStructDef {
                    name: "Point".to_string(),
                    type_parameters: vec![],
                    properties: vec![x, y],
                    position: ASTPosition::new(1, 8),
                    modifiers: ASTModifiers::Private,
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
            modifiers: None,
        };

        let y = ASTStructPropertyDef {
            name: "value".into(),
            ast_type: ASTGenericType(ASTPosition::new(3, 21), "T".into(), Vec::new()),
            position: ASTPosition::new(3, 13),
            modifiers: None,
        };

        if let Some((struct_def, errors, n)) = parse_result {
            assert!(almost_equal_structs(
                &struct_def,
                &ASTStructDef {
                    name: "EnumerateEntry".to_string(),
                    type_parameters: vec!["T".into()],
                    properties: vec![x, y],
                    position: ASTPosition::new(1, 8),
                    modifiers: ASTModifiers::Private,
                    attribute_macros: vec![],
                }
            ));
            assert_eq!(n, 14);
            assert!(errors.is_empty());
        } else {
            panic!()
        }
    }

    #[test]
    fn test_internals() {
        let parse_result = try_parse_struct(
            "internal(\"lib\") struct Point {
            x: int,
            y: int
        }",
        );

        if let Some((struct_def, errors, n)) = parse_result {
            assert!(errors.is_empty());
            assert_eq!(
                struct_def.modifiers,
                ASTModifiers::Internal(Some("lib".to_owned()))
            );
            assert_eq!(n, 15);
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
