use std::fs::File;
use std::io::Write;
use std::path::Path;
use log::info;
use crate::codegen::backend::Backend;
use crate::codegen::backend::BackendAsm386;
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
                info!("Lexer ended");
                let mut parser = Parser::new(lexer, file_path.to_str().map(|it| it.to_string()));
                let module = parser.parse(file_path);

                info!("Parser ended");

                let backend = BackendAsm386::new();

                let mut code_gen = CodeGen::new(&backend, module, 1024 * 1024,
                                                64 * 1024 * 1024, 1024 * 1024, false, false,
                                                true);

                let asm = code_gen.asm();

                info!("Code generation ended");

                let out_path = Path::new(&self.out);
                File::create(out_path).unwrap().write_all(asm.as_bytes()).unwrap();

                backend.compile_and_link(self.out.to_string());

                info!("Compilation and linking ended");
            }
            Err(err) => {
                panic!("An error occurred: {}", err)
            }
        }
    }
}