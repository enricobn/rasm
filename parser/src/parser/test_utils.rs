use crate::lexer::Lexer;
use crate::parser::Parser;

#[cfg(test)]
pub fn get_parser(source: &str) -> Parser {
    let (tokens, lexer_errors) = Lexer::new(source.into()).process();
    Parser::new(tokens, lexer_errors)
}
