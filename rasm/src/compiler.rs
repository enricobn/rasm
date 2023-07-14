use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use log::info;

use rasm_core::codegen::backend::Backend;
use rasm_core::codegen::backend::BackendNasm386;
use rasm_core::codegen::CodeGen;
use rasm_core::lexer::Lexer;
use rasm_core::parser::Parser;
use rasm_core::project::project::RasmProject;
use rasm_core::transformations::enrich_module;

pub struct Compiler {
    project: RasmProject,
    out: PathBuf,
}

impl Compiler {
    pub fn compile(project: RasmProject, out: PathBuf, only_compile: bool) {
        let compiler = Compiler { project, out };
        compiler._compile(only_compile)
    }

    fn _compile(&self, only_compile: bool) {
        let start = Instant::now();

        let debug_asm = false;

        let module = self.project.get_module();
        info!("parse ended in {:?}", start.elapsed());

        let backend =
            BackendNasm386::new(module.requires.clone(), module.externals.clone(), debug_asm);

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
            self.project.resource_folder(),
        );

        let start = Instant::now();

        let asm = code_gen.asm();

        info!("code generation ended in {:?}", start.elapsed());

        let out_path = Path::new(&self.out);
        File::create(out_path)
            .unwrap_or_else(|_| panic!("cannot create file {}", out_path.to_str().unwrap()))
            .write_all(asm.as_bytes())
            .unwrap();

        if only_compile {
            backend.compile(&self.out);
        } else {
            backend.compile_and_link(&self.out);
        }
    }
}
