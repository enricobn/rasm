use crate::lexer::tokens::{BracketKind, BracketStatus, PunctuationKind, TokenKind};
use crate::parser::ParserTrait;

pub struct TypeParamsParser<'a> {
    parser: &'a dyn ParserTrait
}

impl <'a> TypeParamsParser<'a> {

    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse(&self, n: usize) -> Option<(Vec<String>, usize)> {
        if let Some(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open)) = self.parser.get_token_kind_n(n) {
            let mut j = n + 1;
            let mut types = Vec::new();

            loop {
                if let Some(kind) = self.parser.get_token_kind_n(j) {
                    if let TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close) = kind {
                        break;
                    } else if let TokenKind::Punctuation(PunctuationKind::Comma) = kind {

                    } else if let TokenKind::AlphaNumeric(type_name) = kind {
                        types.push(type_name.to_string());
                    } else {
                        self.parser.panic(&format!("expected a parametric type or a comma, found {:?}", kind));
                        break;
                    }
                } else {
                    self.parser.panic("error getting parametric types");
                    break;
                }
                j += 1;
            }
            if types.is_empty() {
                self.parser.panic("cannot find parametric types");
            }
            return Some((types, self.parser.get_i() + j + 1));
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
        let option = try_parse("<T,T1>");

        assert_eq!(Some((vec!["T".into(), "T1".into()], 5)), option);
    }

    fn try_parse(source: &str) -> Option<(Vec<String>, usize)> {
        let parser = get_parser(source);

        let sut = TypeParamsParser::new(&parser);

        sut.try_parse(0)
    }

}

