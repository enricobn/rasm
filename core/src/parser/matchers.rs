use crate::lexer::tokens::{BracketKind, BracketStatus, PunctuationKind, TokenKind};
use crate::parser::tokens_matcher::{Quantifier, TokensMatcher};

pub fn param_types_matcher() -> TokensMatcher {
    let mut param_types = TokensMatcher::new("types", Quantifier::AtMostOne);
    param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open));
    param_types.start_group("type", Quantifier::One);
    param_types.add_alphanumeric();
    param_types.end_group();
    param_types.start_group("type", Quantifier::ZeroOrMore);
    param_types.add_kind(TokenKind::Punctuation(PunctuationKind::Comma));
    param_types.add_alphanumeric();
    param_types.end_group();
    param_types.add_kind(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close));
    param_types
}
