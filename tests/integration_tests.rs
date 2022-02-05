/*
TODO
use std::path::Path;
use crate::lexer::Lexer;
use crate::parser::Parser;

#[test]
fn test() {
    let path = Path::new("resources/test/helloworld.rasm");
    let lexer = Lexer::from_file(path).unwrap();
    let mut parser = Parser::new(lexer);
    let module = parser.parse();

    Parser::print(&module);
}

 */