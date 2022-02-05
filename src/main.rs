extern crate core;

use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use crate::codegen::gen_asm;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod codegen;

fn main() {

    let path = env::current_dir();

    println!("Current dir: {:?}", path);

    match Lexer::from_file(Path::new("resources/test/helloworld.rasm")) {
        Ok(lexer) => {
            let mut parser = Parser::new(lexer);
            let module = parser.parse();

            Parser::print(&module);

            let asm = gen_asm(module);

            let out_path = Path::new("helloworld.asm");
            File::create(out_path).unwrap().write_all(asm.as_bytes()).unwrap();
        }
        Err(err) => {
            println!("An error occurred: {}", err)
        }
    }

}
