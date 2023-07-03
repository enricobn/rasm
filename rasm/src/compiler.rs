use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

use log::info;

use rasm_core::codegen::backend::Backend;
use rasm_core::codegen::backend::BackendAsm386;
use rasm_core::codegen::CodeGen;
use rasm_core::lexer::Lexer;
use rasm_core::parser::Parser;

pub struct Compiler {
    main_src_file: PathBuf,
    out: PathBuf,
    std_lib_path: PathBuf,
    resource_folder: PathBuf,
}

impl Compiler {
    pub fn compile(
        main_src_file: PathBuf,
        out: PathBuf,
        std_lib_path: PathBuf,
        resource_folder: PathBuf,
        only_compile: bool,
    ) {
        let compiler = Compiler {
            main_src_file,
            out,
            std_lib_path,
            resource_folder,
        };
        compiler._compile(only_compile)
    }

    fn _compile(&self, only_compile: bool) {
        let file_path = Path::new(&self.main_src_file);
        match Lexer::from_file(file_path) {
            Ok(lexer) => {
                info!("Lexer ended");
                let mut parser = Parser::new(lexer, Some(file_path.to_path_buf()));
                let module = parser.parse(file_path, Path::new(&self.std_lib_path));

                info!("Parser ended");

                let debug_asm = false;

                let backend = BackendAsm386::new(
                    module.requires.clone(),
                    module.externals.clone(),
                    debug_asm,
                );

                let mut code_gen = CodeGen::new(
                    &backend,
                    module,
                    1024 * 1024,
                    64 * 1024 * 1024,
                    1024 * 1024,
                    debug_asm,
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
                    backend.compile(&self.out);
                } else {
                    backend.compile_and_link(&self.out);
                }

                info!("Compilation and linking ended");
            }
            Err(err) => {
                panic!("An error occurred: {}", err)
            }
        }
    }
}
