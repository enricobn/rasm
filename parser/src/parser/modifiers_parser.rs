use rasm_utils::OptionDisplay;

use crate::{
    lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind},
    parser::{ParserTrait, ast::ASTModifiers},
};

enum InternalState {
    Element,
    Comma,
}

pub fn try_parse_ast_modifiers_tokens<'a>(
    tokens: Vec<Token>,
) -> Result<(Option<ASTModifiers>, usize), String> {
    let parser = crate::parser::Parser::new(tokens, Vec::new());
    try_parse_ast_modifiers(&parser, 0)
}

pub fn try_parse_ast_modifiers<'a>(
    parser: &'a dyn ParserTrait,
    n: usize,
) -> Result<(Option<ASTModifiers>, usize), String> {
    let mut current_n = n;
    let modifiers =
        if let Some(TokenKind::KeyWord(KeywordKind::Pub)) = parser.get_token_kind_n(current_n) {
            current_n += 1;
            Some(ASTModifiers::Public)
        } else if let Some(TokenKind::KeyWord(KeywordKind::Internal)) =
            parser.get_token_kind_n(current_n)
        {
            current_n += 1;
            let (internals, new_n) = try_parse_internals(parser, current_n)?;
            current_n = new_n;
            Some(ASTModifiers::Internal(internals))
        } else if let Some(TokenKind::KeyWord(KeywordKind::Private)) =
            parser.get_token_kind_n(current_n)
        {
            current_n += 1;
            Some(ASTModifiers::Private)
        } else {
            None
        };
    Ok((modifiers, current_n))
}

fn try_parse_internals<'a>(
    parser: &'a dyn ParserTrait,
    n: usize,
) -> Result<(Option<String>, usize), String> {
    let mut current_n = n;
    let mut result = Vec::new();
    if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
        parser.get_token_kind_n(current_n)
    {
        current_n += 1;
        let mut state = InternalState::Element;
        loop {
            match state {
                InternalState::Element => {
                    if let Some(TokenKind::StringLiteral(s)) = parser.get_token_kind_n(current_n) {
                        result.push(s.clone());
                        current_n += 1;
                        state = InternalState::Comma;
                    } else if let Some(TokenKind::Bracket(
                        BracketKind::Round,
                        BracketStatus::Close,
                    )) = parser.get_token_kind_n(current_n)
                    {
                        current_n += 1;
                        break;
                    } else {
                        return Err(format!(
                            "Expected string literal or closing bracket, but got {} : {}",
                            OptionDisplay(&parser.get_token_n(current_n)),
                            parser.get_position(current_n)
                        ));
                    }
                }
                InternalState::Comma => {
                    if let Some(TokenKind::Punctuation(PunctuationKind::Comma)) =
                        parser.get_token_kind_n(current_n)
                    {
                        current_n += 1;
                        state = InternalState::Element;
                    } else if let Some(TokenKind::Bracket(
                        BracketKind::Round,
                        BracketStatus::Close,
                    )) = parser.get_token_kind_n(current_n)
                    {
                        current_n += 1;
                        break;
                    } else {
                        return Err(format!(
                            "Expected comma or closing bracket, but got {} : {}",
                            OptionDisplay(&parser.get_token_n(current_n)),
                            parser.get_position(current_n)
                        ));
                    }
                }
            }
        }
        if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close)) =
            parser.get_token_kind_n(current_n)
        {
            current_n += 1;
        }
    }

    // we ry to parse even more internals, but for now are not supported
    let internals = if result.len() > 1 {
        return Err("Only one internal allowed".to_owned());
    } else if result.len() == 0 {
        None
    } else {
        Some(result[0].clone())
    };
    Ok((internals, current_n))
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::ASTModifiers;

    #[test]
    fn simple() {
        let (modifiers, next_n) = super::try_parse_ast_modifiers(
            &crate::parser::test_utils::get_parser("internal(\"a\")"),
            0,
        )
        .unwrap();
        assert_eq!(
            modifiers,
            Some(ASTModifiers::Internal(Some("a".to_owned())))
        );
        assert_eq!(next_n, 4);
    }

    #[test]
    fn internal_no_internals() {
        let (modifiers, next_n) =
            super::try_parse_ast_modifiers(&crate::parser::test_utils::get_parser("internal"), 0)
                .unwrap();
        assert_eq!(modifiers, Some(ASTModifiers::Internal(None)));
        assert_eq!(next_n, 1);
    }

    #[test]
    fn private() {
        let (modifiers, next_n) =
            super::try_parse_ast_modifiers(&crate::parser::test_utils::get_parser("private"), 0)
                .unwrap();
        assert_eq!(modifiers, Some(ASTModifiers::Private));
        assert_eq!(next_n, 1);
    }

    #[test]
    fn public() {
        let (modifiers, next_n) =
            super::try_parse_ast_modifiers(&crate::parser::test_utils::get_parser("pub"), 0)
                .unwrap();
        assert_eq!(modifiers, Some(ASTModifiers::Public));
        assert_eq!(next_n, 1);
    }
}
