extern crate core;

use std::env;
use std::path::Path;
use crate::lexer::Lexer;

pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod codegen;

fn main() {

    let path = env::current_dir();

    println!("Current dir: {:?}", path);

    match Lexer::from_file(Path::new("resources/test/helloworld.rasm")) {
        Ok(lexer) => {
            let mut count = 0;
            lexer.for_each(|it| {
                println!("{:?}", it);
                count += 1;
            });
            println!("tokens {} ", count);
        }
        Err(err) => {
            println!("An error occurred: {}", err)
        }
    }

}
