use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, TokenKind};
use crate::parser::tokens_matcher::{Quantifier, StringLiteralTokenMatcher, TokensMatcher};

pub fn generic_types_matcher() -> TokensMatcher {
    let mut matcher = TokensMatcher::new("types", Quantifier::AtMostOne);
    matcher.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open));
    matcher.start_group("type", Quantifier::One);
    matcher.add_alphanumeric();
    matcher.end_group();
    matcher.start_group("type", Quantifier::ZeroOrMore);
    matcher.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
    matcher.add_alphanumeric();
    matcher.end_group();
    matcher.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close));
    matcher
}

pub fn modifiers_matcher() -> TokensMatcher {
    let mut matcher = TokensMatcher::new("modifiers", Quantifier::AtMostOne);
    matcher.start_group("public modifier", Quantifier::AtMostOne);
    matcher.add_kind(TokenKind::KeyWord(KeywordKind::Pub));
    matcher.end_group();

    matcher.start_group("internal modifier", Quantifier::AtMostOne);
    matcher.add_kind(TokenKind::KeyWord(KeywordKind::Internal));

    matcher.start_group("internals", Quantifier::AtMostOne);
    matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open));
    matcher.add_matcher(StringLiteralTokenMatcher::new());
    matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close));
    matcher.end_group();

    matcher.end_group();
    matcher
}

#[cfg(test)]
mod tests {
    use crate::parser::tokens_matcher::TokensMatcherTrait;

    #[test]
    fn simple() {
        let matcher = super::modifiers_matcher();
        let parser = crate::parser::test_utils::get_parser("pub");
        assert!(matcher.match_tokens(&parser, 0).is_some());
    }
}
