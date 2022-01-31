use std::env;
use std::path::Path;
use crate::lexer::Lexer;

mod lexer;

fn main() {

    let path = env::current_dir();

    println!("Current dir: {:?}", path);

    match Lexer::from_file(Path::new("resources/test/helloworld.rasm")) {
        Ok(lexer) => {
            lexer.for_each(|it| {
                println!("{:?}", it)
            })
        }
        Err(err) => {
            println!("An error occurred: {}", err)
        }
    }

}
