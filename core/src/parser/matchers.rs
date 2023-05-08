use crate::lexer::tokens::{BracketKind, BracketStatus, PunctuationKind, TokenKind};
use crate::parser::tokens_matcher::{Quantifier, TokensMatcher};

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
