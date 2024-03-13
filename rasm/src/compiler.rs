use std::collections::HashSet;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use log::info;

use rasm_core::codegen::backend::{Backend, BackendNasmi386};
use rasm_core::codegen::compile_target::CompileTarget;
use rasm_core::codegen::enhanced_module::EnhancedASTModule;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::{get_typed_module, CodeGenOptions};
use rasm_core::project::RasmProject;

pub struct Compiler {
    project: RasmProject,
    out: PathBuf,
    is_test: bool,
    options: CodeGenOptions,
    target: CompileTarget,
    debug: bool,
    print_module: bool,
}

impl Compiler {
    pub fn new(
        project: RasmProject,
        out: Option<&String>,
        is_test: bool,
        options: CodeGenOptions,
        compile_target: CompileTarget,
        debug: bool,
        print_module: bool,
    ) -> Self {
        let out = if let Some(o) = out {
            Path::new(o).to_path_buf()
        } else {
            project
                .out_file(is_test)
                .expect("undefined out in rasm.toml")
        }
        .with_extension(compile_target.extension());

        info!("out: {}", out.with_extension("").to_string_lossy());

        Self {
            project,
            out,
            is_test,
            options,
            target: compile_target,
            debug,
            print_module,
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

        let mut statics = Statics::new();

        let (modules, errors) =
            self.project
                .get_all_modules(&mut statics, self.is_test, &self.target, self.debug);

        /*
        for module in modules.iter() {
            if module.namespace.to_string().contains("namespace") {
                println!("Module: {}", module.path.to_string_lossy());
                println!("-----------------------");
                Parser::print_module(&module);
                println!("-----------------------");
                println!();
            }
        }
         */

        let requires = modules
            .iter()
            .flat_map(|it| it.requires.clone())
            .collect::<HashSet<_>>();

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            panic!()
        }

        let enhanced_ast_module = EnhancedASTModule::new(
            modules,
            &self.project,
            &mut statics,
            &self.target,
            self.debug,
        );

        info!("parse ended in {:?}", start.elapsed());

        let start = Instant::now();

        let backend = BackendNasmi386::new(self.options.clone(), self.debug);

        let typed_module = get_typed_module(
            &backend,
            enhanced_ast_module,
            self.options.print_memory,
            self.options.dereference,
            self.print_module,
            &mut statics,
            &self.target,
            self.debug,
        )
        .unwrap_or_else(|e| {
            panic!("{e}");
        });

        info!("type check ended in {:?}", start.elapsed());

        let start = Instant::now();

        let native_code = self.target.generate(statics, typed_module, self.debug);

        info!("code generation ended in {:?}", start.elapsed());

        let out_path = Path::new(&self.out);
        File::create(out_path)
            .unwrap_or_else(|_| panic!("cannot create file {}", out_path.to_str().unwrap()))
            .write_all(native_code.as_bytes())
            .unwrap();

        if only_compile {
            backend.compile(&self.out);
        } else {
            backend.compile_and_link(&self.out, &requires);
        }
    }
}
