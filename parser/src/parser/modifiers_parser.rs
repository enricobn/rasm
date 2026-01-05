use rasm_utils::OptionDisplay;

use crate::{
    lexer::{
        LexerError,
        tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind},
    },
    parser::{ParserTrait, ast::ASTModifiers},
};

enum InternalState {
    Element,
    Comma,
}

pub fn try_parse_ast_modifiers_tokens<'a>(
    tokens: Vec<Token>,
) -> Result<(ASTModifiers, usize), String> {
    let tokens = tokens
        .into_iter()
        .map(|it| (Some(it), Vec::new() as Vec<LexerError>))
        .collect::<Vec<_>>();
    let parser = crate::parser::Parser::new(tokens);
    try_parse_ast_modifiers(&parser, 0)
}

pub fn try_parse_ast_modifiers<'a>(
    parser: &'a dyn ParserTrait,
    n: usize,
) -> Result<(ASTModifiers, usize), String> {
    let mut current_n = n;
    let modifiers =
        if let Some(TokenKind::KeyWord(KeywordKind::Pub)) = parser.get_token_kind_n(current_n) {
            current_n += 1;
            ASTModifiers::Public
        } else if let Some(TokenKind::KeyWord(KeywordKind::Internal)) =
            parser.get_token_kind_n(current_n)
        {
            current_n += 1;
            let (internals, new_n) = try_parse_internals(parser, current_n)?;
            current_n = new_n;
            ASTModifiers::Internal(internals)
        } else if let Some(TokenKind::KeyWord(KeywordKind::Private)) =
            parser.get_token_kind_n(current_n)
        {
            current_n += 1;
            ASTModifiers::Private
        } else {
            ASTModifiers::Private
        };
    Ok((modifiers, current_n))
}

fn try_parse_internals<'a>(
    parser: &'a dyn ParserTrait,
    n: usize,
) -> Result<(Vec<String>, usize), String> {
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
    Ok((result, current_n))
}

#[cfg(test)]
mod tests {
    use rasm_utils::StrVecToStrings;

    use crate::parser::ast::ASTModifiers;

    #[test]
    fn simple() {
        let (modifiers, next_n) = super::try_parse_ast_modifiers(
            &crate::parser::test_utils::get_parser("internal(\"a\", \"b\")"),
            0,
        )
        .unwrap();
        assert_eq!(
            modifiers,
            ASTModifiers::Internal(vec!["a", "b"].to_strings())
        );
        assert_eq!(next_n, 6);
    }

    #[test]
    fn internal_no_internals() {
        let (modifiers, next_n) =
            super::try_parse_ast_modifiers(&crate::parser::test_utils::get_parser("internal"), 0)
                .unwrap();
        assert_eq!(modifiers, ASTModifiers::Internal(Vec::new()));
        assert_eq!(next_n, 1);
    }

    #[test]
    fn private() {
        let (modifiers, next_n) =
            super::try_parse_ast_modifiers(&crate::parser::test_utils::get_parser("private"), 0)
                .unwrap();
        assert_eq!(modifiers, ASTModifiers::Private);
        assert_eq!(next_n, 1);
    }

    #[test]
    fn public() {
        let (modifiers, next_n) =
            super::try_parse_ast_modifiers(&crate::parser::test_utils::get_parser("pub"), 0)
                .unwrap();
        assert_eq!(modifiers, ASTModifiers::Public);
        assert_eq!(next_n, 1);
    }
}
