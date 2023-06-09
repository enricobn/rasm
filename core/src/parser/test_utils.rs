use crate::lexer::Lexer;
use crate::parser::Parser;

#[cfg(test)]
pub fn get_parser(source: &str) -> Parser {
    let lexer = Lexer::new(source.into(), None);
    Parser::new(lexer, None)
}
