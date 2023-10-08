use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use log::info;

use rasm_core::codegen::backend::Backend;
use rasm_core::codegen::backend::BackendNasm386;
use rasm_core::codegen::enhanced_module::EnhancedASTModule;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::CodeGen;
use rasm_core::project::project::RasmProject;

pub struct Compiler {
    project: RasmProject,
    out: PathBuf,
}

impl Compiler {
    pub fn compile(project: RasmProject, out: PathBuf, only_compile: bool) {
        let path_buf = project.resource_folder().clone();
        let compiler = Compiler { project, out };
        compiler._compile(path_buf, only_compile)
    }

    fn _compile(&self, resource_folder: PathBuf, only_compile: bool) {
        let start = Instant::now();

        let debug_asm = false;

        let mut backend = BackendNasm386::new(debug_asm);
        let mut statics = Statics::new();
        let (modules, errors) = self.project.get_all_modules(&mut backend, &mut statics);

        let enhanced_astmodule = EnhancedASTModule::new(modules, resource_folder);

        info!("parse ended in {:?}", start.elapsed());

        let mut code_gen = CodeGen::new(
            &backend,
            statics,
            enhanced_astmodule,
            1024 * 1024,
            64 * 1024 * 1024,
            1024 * 1024,
            debug_asm,
            false,
            true,
            false,
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
