use std::fs::File;
use std::io::Write;
use std::path::Path;
use crate::codegen::backend::Backend;
use crate::codegen::backend::Backend386;
use crate::codegen::CodeGen;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub struct Compiler {
    src: String,
    out: String,
}

impl Compiler {
    pub fn compile(src: String, out: String) {
        let compiler = Compiler {
            src,
            out,
        };
        compiler._compile()
    }

    fn _compile(&self) {
        let file_path = Path::new(&self.src);
        match Lexer::from_file(file_path) {
            Ok(lexer) => {
                let mut parser = Parser::new(lexer);
                let module = parser.parse(file_path);

                let backend = Backend386::new();

                let mut code_gen = CodeGen::new(&backend, module);

                let asm = code_gen.asm();

                let out_path = Path::new(&self.out);
                File::create(out_path).unwrap().write_all(asm.as_bytes()).unwrap();

                backend.compile_and_link(self.out.to_string());
            }
            Err(err) => {
                println!("An error occurred: {}", err)
            }
        }
    }
}