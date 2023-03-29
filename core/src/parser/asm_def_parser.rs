use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, TokenKind};
use crate::parser::type_params_parser::TypeParamsParser;
use crate::parser::ParserTrait;

pub struct AsmDefParser<'a> {
    parser: &'a dyn ParserTrait,
}

impl<'a> AsmDefParser<'a> {
    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse(&self) -> Option<(String, bool, Vec<String>, usize)> {
        if let Some(kind) = self.parser.get_token_kind() {
            if let TokenKind::KeyWord(KeywordKind::Inline) = kind {
                if let Some((function_name, type_params, next_i)) = self.try_parse_no_inline(1) {
                    return Some((function_name, true, type_params, next_i));
                }
            } else if let Some((function_name, type_params, next_i)) = self.try_parse_no_inline(0) {
                return Some((function_name, false, type_params, next_i));
            }
        }
        None
    }

    fn try_parse_no_inline(&self, n: usize) -> Option<(String, Vec<String>, usize)> {
        if let Some(TokenKind::KeyWord(KeywordKind::Asm)) = self.parser.get_token_kind_n(n) {
            let mut current_n = n + 1;

            if let Some(TokenKind::AlphaNumeric(function_name)) =
                self.parser.get_token_kind_n(current_n)
            {
                let type_params_parser = TypeParamsParser::new(self.parser);

                let type_params = if let Some((type_params, next_i_t)) =
                    type_params_parser.try_parse(current_n + 1)
                {
                    current_n = next_i_t - self.parser.get_i() - 1;
                    type_params
                } else {
                    vec![]
                };

                if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                    self.parser.get_token_kind_n(current_n + 1)
                {
                    return Some((
                        function_name.clone(),
                        type_params,
                        self.parser.get_i() + current_n + 2,
                    ));
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
    fn test() {
        let parse_result = try_parse(
            "inline asm aFun<T>(o: Option<T>) /{
        }/",
        );

        assert_eq!(
            Some(("aFun".into(), true, vec!["T".into()], 7)),
            parse_result
        );
    }

    fn try_parse(source: &str) -> Option<(String, bool, Vec<String>, usize)> {
        let parser = get_parser(source);

        let sut = AsmDefParser::new(&parser);

        sut.try_parse()
    }
}
