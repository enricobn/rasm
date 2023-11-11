use std::fs;
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
use rasm_core::project::RasmProject;

pub struct Compiler {
    project: RasmProject,
    out: PathBuf,
    is_test: bool,
}

impl Compiler {
    pub fn new(project: RasmProject, out: Option<&String>, is_test: bool) -> Self {
        let out = if let Some(o) = out {
            Path::new(o).to_path_buf()
        } else {
            project
                .out_file(is_test)
                .expect("undefined out in rasm.toml")
        }
        .with_extension("asm");

        info!("out: {}", out.with_extension("").to_string_lossy());

        Self {
            project,
            out,
            is_test,
        }
    }

    pub fn compile(&self, only_compile: bool) {
        if self.out.exists() {
            let _ = fs::remove_file(&self.out);
        }

        let executable = self.out.with_extension("");

        if executable.exists() {
            let _ = fs::remove_file(&executable);
        }

        let object = self.out.with_extension("o");

        if object.exists() {
            let _ = fs::remove_file(&object);
        }

        let start = Instant::now();

        let debug_asm = false;

        let mut backend = BackendNasm386::new(debug_asm);
        let mut statics = Statics::new();

        let (modules, errors) =
            self.project
                .get_all_modules(&mut backend, &mut statics, self.is_test);

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            panic!()
        }

        let enhanced_ast_module = EnhancedASTModule::new(modules, &self.project);

        info!("parse ended in {:?}", start.elapsed());

        let code_gen = CodeGen::new(
            &backend,
            &mut statics,
            enhanced_ast_module,
            1024 * 1024,
            64 * 1024 * 1024,
            1024 * 1024,
            debug_asm,
            false,
            true,
            false,
        );

        let start = Instant::now();

        let asm = code_gen.asm(statics);

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
