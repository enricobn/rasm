use std::fs::File;
use std::io::Write;
use std::path::Path;

use log::info;

use rasm_core::codegen::backend::Backend;
use rasm_core::codegen::backend::BackendAsm386;
use rasm_core::codegen::CodeGen;
use rasm_core::lexer::Lexer;
use rasm_core::parser::Parser;

pub struct Compiler {
    src: String,
    out: String,
    std_lib_path: String,
    resource_folder: String,
}

impl Compiler {
    pub fn compile(
        src: String,
        out: String,
        std_lib_path: String,
        only_compile: bool,
        resource_folder: String,
    ) {
        let compiler = Compiler {
            src,
            out,
            std_lib_path,
            resource_folder,
        };
        compiler._compile(only_compile)
    }

    fn _compile(&self, only_compile: bool) {
        let file_path = Path::new(&self.src);
        match Lexer::from_file(file_path) {
            Ok(lexer) => {
                info!("Lexer ended");
                let mut parser = Parser::new(lexer, file_path.to_str().map(|it| it.to_string()));
                let module = parser.parse(file_path, Path::new(&self.std_lib_path));

                info!("Parser ended");

                let backend = BackendAsm386::new(module.requires.clone(), module.externals.clone());

                let mut code_gen = CodeGen::new(
                    &backend,
                    module,
                    1024 * 1024,
                    64 * 1024 * 1024,
                    1024 * 1024,
                    false,
                    false,
                    true,
                    false,
                    self.resource_folder.clone(),
                );

                let asm = code_gen.asm();

                info!("Code generation ended");

                let out_path = Path::new(&self.out);
                File::create(out_path)
                    .unwrap()
                    .write_all(asm.as_bytes())
                    .unwrap();

                if only_compile {
                    backend.compile(&self.out.to_string());
                } else {
                    backend.compile_and_link(&self.out.to_string());
                }

                info!("Compilation and linking ended");
            }
            Err(err) => {
                panic!("An error occurred: {}", err)
            }
        }
    }
}
