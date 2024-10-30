use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, Token, TokenKind};
use crate::parser::ast::ASTModifiers;
use crate::parser::type_params_parser::TypeParamsParser;
use crate::parser::ParserTrait;

pub struct AsmDefParser<'a> {
    parser: &'a dyn ParserTrait,
}

impl<'a> AsmDefParser<'a> {
    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse(
        &self,
    ) -> Result<Option<(Token, bool, Vec<String>, ASTModifiers, usize)>, String> {
        let mut current_n = 0;
        let modifiers =
            if let Some(TokenKind::KeyWord(KeywordKind::Pub)) = self.parser.get_token_kind_n(0) {
                current_n += 1;
                ASTModifiers::public()
            } else {
                ASTModifiers::private()
            };
        if let Some(kind) = self.parser.get_token_kind_n(current_n) {
            if let TokenKind::KeyWord(KeywordKind::Inline) = kind {
                if let Some((function_name, type_params, next_i)) =
                    self.try_parse_no_inline(current_n + 1)?
                {
                    return Ok(Some((function_name, true, type_params, modifiers, next_i)));
                }
            } else if let Some((function_name, type_params, next_i)) =
                self.try_parse_no_inline(current_n)?
            {
                return Ok(Some((function_name, false, type_params, modifiers, next_i)));
            }
        }
        Ok(None)
    }

    fn try_parse_no_inline(&self, n: usize) -> Result<Option<(Token, Vec<String>, usize)>, String> {
        let mut current_n = n + 1;
        if let Some(TokenKind::KeyWord(KeywordKind::Native)) = self.parser.get_token_kind_n(n) {
            let name_token_o = self.parser.get_token_n(n + 1);

            if let Some(TokenKind::AlphaNumeric(_)) = name_token_o.map(|it| &it.kind) {
                let type_params_parser = TypeParamsParser::new(self.parser);

                let type_params = if let Some((type_params, next_i_t)) =
                    type_params_parser.try_parse(current_n + 1)?
                {
                    current_n = next_i_t - self.parser.get_i() - 1;
                    type_params
                } else {
                    vec![]
                };

                if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                    self.parser.get_token_kind_n(current_n + 1)
                {
                    return Ok(Some((
                        name_token_o.unwrap().clone(),
                        type_params,
                        self.parser.get_i() + current_n + 2,
                    )));
                }
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::get_parser;

    use super::*;

    #[test]
    fn test() {
        let parse_result = try_parse(
            "inline native aFun<T>(o: Option<T>) /{
        }/",
        );
        let expected_token = Token::new(TokenKind::AlphaNumeric("aFun".to_string()), 1, 19);

        assert_eq!(
            parse_result,
            Some((
                expected_token,
                true,
                vec!["T".into()],
                ASTModifiers::private(),
                7
            )),
        );
    }

    fn try_parse(source: &str) -> Option<(Token, bool, Vec<String>, ASTModifiers, usize)> {
        let parser = get_parser(source);

        let sut = AsmDefParser::new(&parser);

        sut.try_parse().unwrap()
    }
}
