extern crate core;

use std::env;
use std::fs::File;
use std::io::Write;
use std::ops::Add;
use std::path::Path;
use crate::codegen::backend::{Backend, Backend386};
use crate::codegen::CodeGen;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod codegen;

fn main() {

    let path = env::current_dir();

    println!("Current dir: {:?}", path);

    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    let src = args.get(1).unwrap();
    let out = args.get(2).unwrap().clone().add(".asm");
    let file_path = Path::new(src);
    match Lexer::from_file(file_path) {
        Ok(lexer) => {
            let mut parser = Parser::new(lexer);
            let module = parser.parse(file_path);

            let backend = Backend386::new();

            let mut code_gen = CodeGen::new(&backend, module);

            let asm = code_gen.asm();

            let out_path = Path::new(&out);
            File::create(out_path).unwrap().write_all(asm.as_bytes()).unwrap();

            backend.compile_and_link(out.to_string());
        }
        Err(err) => {
            println!("An error occurred: {}", err)
        }
    }

}
