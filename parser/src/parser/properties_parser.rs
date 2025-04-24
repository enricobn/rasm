use crate::lexer::tokens::{PunctuationKind, TokenKind};

use super::{ast::ASTStructPropertyDef, type_parser::TypeParser, ParserError, ParserTrait};

enum PropertyStatus {
    Name,
    Colon,
    Type,
}

pub fn parse_properties(
    parser: &dyn ParserTrait,
    generic_types: &[String],
    n: usize,
    end_of_expression_kind: TokenKind,
) -> (Vec<ASTStructPropertyDef>, Vec<ParserError>, usize) {
    let mut properties = Vec::new();
    let mut errors = Vec::new();
    let mut new_n = n;
    let mut expected_comma = false;

    loop {
        if expected_comma {
            match parser.get_token_kind_n(new_n) {
                Some(kind) => {
                    if matches!(kind, TokenKind::Punctuation(PunctuationKind::Comma)) {
                        expected_comma = false;
                        new_n += 1;
                        continue;
                    } else if kind == &end_of_expression_kind {
                        new_n += 1;
                        break;
                    } else {
                        errors.push(parser.error(
                            new_n,
                            format!("Expected comma or end of properties, but got {kind}"),
                        ));
                        break;
                    }
                }
                None => {
                    break;
                }
            }
        }
        let (p, es, nn) = parse_property(parser, generic_types, new_n);
        errors.extend(es);
        if nn == new_n {
            break;
        }
        new_n = nn;
        if let Some(pp) = p {
            properties.push(pp);
            expected_comma = true;
        } else {
            break;
        }
    }

    (properties, errors, new_n)
}

pub fn parse_property(
    parser: &dyn ParserTrait,
    generic_types: &[String],
    n: usize,
) -> (Option<ASTStructPropertyDef>, Vec<ParserError>, usize) {
    let mut errors = Vec::new();
    let mut new_n = n;
    let mut status = PropertyStatus::Name;
    let mut property_def = ASTStructPropertyDef {
        name: String::new(),
        ast_type: super::ast::ASTType::Unit,
        position: parser.get_position(n),
    };

    loop {
        let token = match parser.get_token_n(new_n) {
            Some(t) => t,
            None => {
                errors.push(parser.error(new_n, "Expected a token".to_owned()));
                break;
            }
        };
        match status {
            PropertyStatus::Name => {
                if let TokenKind::AlphaNumeric(ref name) = token.kind {
                    property_def.name = name.clone();
                    property_def.position = token.position.clone();
                    new_n += 1;
                    status = PropertyStatus::Colon
                } else {
                    errors.push(parser.error(
                        new_n,
                        format!("Expected an identifier, but found {}", token.kind),
                    ));
                    new_n += 1;
                    break;
                }
            }
            PropertyStatus::Type => {
                let type_parser = TypeParser::new(parser);
                match type_parser.try_parse_ast_type(new_n, generic_types) {
                    Ok(type_and_i) => {
                        if let Some((ast_type, i)) = type_and_i {
                            property_def.ast_type = ast_type;
                            new_n = i - parser.get_i();
                            break;
                        } else {
                            errors.push(parser.error(new_n, "No type specified.".to_owned()));
                            break;
                        }
                    }
                    Err(err) => return (Some(property_def), vec![parser.error(new_n, err)], new_n),
                }
            }
            PropertyStatus::Colon => {
                if let TokenKind::Punctuation(PunctuationKind::Colon) = token.kind {
                    new_n += 1;
                    status = PropertyStatus::Type
                } else {
                    errors.push(
                        parser.error(new_n, format!("Expected a colon, but found {}", token.kind)),
                    );
                    break;
                }
            }
        }
    }

    (Some(property_def), errors, new_n)
}

#[cfg(test)]
mod tests {
    use rasm_utils::SliceDisplay;

    use crate::{
        lexer::tokens::{BracketKind, BracketStatus, TokenKind},
        parser::{ast::ASTType, properties_parser::parse_properties, test_utils::get_parser},
    };

    use super::parse_property;

    #[test]
    fn test_parse_property() {
        let parser = get_parser("name: Pippo");
        let (prop, errors, n) = parse_property(&parser, &[], 0);

        if let Some(def) = prop {
            assert_eq!(def.name, "name");
            assert!(errors.is_empty());
            assert_eq!(3, n);
        } else {
            panic!()
        }
    }

    #[test]
    fn test_parse_incomplete_property() {
        let parser = get_parser("name:");

        let (prop, errors, n) = parse_property(&parser, &[], 0);

        if let Some(def) = prop {
            assert_eq!(def.name, "name");
            assert!(!errors.is_empty());
            assert_eq!(2, n);
        } else {
            panic!()
        }
    }

    #[test]
    fn test_parse_incomplete_property_2() {
        let parser = get_parser("name:\n{");

        let (prop, errors, n) = parse_property(&parser, &[], 0);

        if let Some(def) = prop {
            assert_eq!(def.name, "name");
            assert!(!errors.is_empty());
            assert_eq!(2, n);
        } else {
            panic!()
        }
    }

    #[test]
    fn test_parse_properties_one() {
        let parser = get_parser("name: Pippo");

        let (mut properties, _errors, n) = parse_properties(
            &parser,
            &[],
            0,
            TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close),
        );

        assert_eq!(properties.len(), 1);
        assert_eq!(properties.remove(0).name, "name");
        assert_eq!(3, n);
    }

    #[test]
    fn test_parse_properties() {
        let parser = get_parser("name: Pippo, \n another: Pluto");

        let (mut properties, errors, n) = parse_properties(
            &parser,
            &[],
            0,
            TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close),
        );

        println!("errors {}", SliceDisplay(&errors));
        assert_eq!(properties.len(), 2);
        assert_eq!(properties.remove(0).name, "name");
        assert_eq!(properties.remove(0).name, "another");
        assert!(errors.is_empty());
        assert_eq!(7, n);
    }

    #[test]
    fn test_parse_unterminated_properties() {
        let parser = get_parser("name: Pippo, \n another: Pluto\n fn hello");

        let (mut properties, errors, n) = parse_properties(
            &parser,
            &[],
            0,
            TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close),
        );

        assert_eq!(properties.len(), 2);
        assert_eq!(properties.remove(0).name, "name");
        assert_eq!(properties.remove(0).name, "another");
        assert!(!errors.is_empty());
        assert_eq!(7, n);
    }

    #[test]
    fn test_parse_terminated_properties() {
        let parser = get_parser("name: Pippo, \n another: Pluto\n}");

        let (mut properties, errors, n) = parse_properties(
            &parser,
            &[],
            0,
            TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close),
        );

        assert_eq!(properties.len(), 2);
        assert_eq!(properties.remove(0).name, "name");
        assert_eq!(properties.remove(0).name, "another");
        assert!(errors.is_empty());
        assert_eq!(8, n);
    }

    #[test]
    fn test_parse_type_class_property() {
        let parser = get_parser("v: M<String>");

        let (property, errors, n) = parse_property(&parser, &["M".to_owned()], 0);

        let p = property.unwrap();
        println!("errors {}", SliceDisplay(&errors));
        assert_eq!(p.name, "v");
        assert_eq!(format!("{}", p.ast_type), "M<String>");
        if !matches!(p.ast_type, ASTType::Generic(_, _, _)) {
            panic!("expected a generic type");
        }
        assert!(errors.is_empty());
        assert_eq!(6, n);
    }
}
