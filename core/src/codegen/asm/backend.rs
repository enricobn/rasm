use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Instant;

use auto_impl::auto_impl;
use log::info;

use crate::enh_type_check::typed_ast::ASTTypedType;

#[auto_impl(Box)]
pub trait Backend: Send + Sync {
    fn address_from_base_pointer(&self, index: i8) -> String;

    fn address_from_stack_pointer(&self, index: i8) -> String;

    fn type_len(&self, ast_typed_type: &ASTTypedType) -> u8;

    fn word_len(&self) -> usize;

    fn compile_and_link(&self, source_file: &Path, requires: &[String]);

    fn compile(&self, source_file: &Path);

    fn link(&self, path: &Path, requires: &[String]);

    fn type_size(&self, ast_typed_type: &ASTTypedType) -> Option<String>;

    fn debug_asm(&self) -> bool;
}

#[auto_impl(Box)]
pub trait BackendAsm: Backend + Send + Sync {
    fn stack_base_pointer(&self) -> &str;

    fn stack_pointer(&self) -> &str;

    fn pointer_size(&self) -> &str;

    fn word_size(&self) -> &str;
}

#[derive(Clone)]
enum Linker {
    Ld,
    Gcc,
}

#[derive(Clone)]
pub struct BackendNasmi386 {
    linker: Linker,
    libc: bool,
    debug: bool,
}

impl BackendNasmi386 {
    pub fn new(debug: bool) -> Self {
        let libc = true; //requires.contains("libc");
        Self {
            linker: if libc { Linker::Gcc } else { Linker::Ld },
            libc,
            debug,
        }
    }
}

pub fn log_command(command: &Command) {
    let mut s = String::new();
    s.push_str(&format!(
        "running command: {}",
        &command.get_program().to_str().unwrap()
    ));
    for arg in command.get_args() {
        s.push_str(&format!(" {}", &arg.to_str().unwrap()));
    }
    info!("{s}");
}

impl Backend for BackendNasmi386 {
    fn address_from_base_pointer(&self, index: i8) -> String {
        format!("[ebp+{}]", index * 4)
    }

    fn address_from_stack_pointer(&self, index: i8) -> String {
        format!("[esp+{}]", index * 4)
    }

    fn type_len(&self, _ast_tped_type: &ASTTypedType) -> u8 {
        4
    }

    fn word_len(&self) -> usize {
        4
    }

    fn compile_and_link(&self, source_file: &Path, requires: &[String]) {
        self.compile(source_file);

        self.link(source_file, requires);
    }

    fn compile(&self, source_file: &Path) {
        let start = Instant::now();
        info!("source file : '{:?}'", source_file);
        let mut nasm_command = Command::new("nasm");
        nasm_command
            .arg("-f")
            .arg("elf")
            .arg("-g")
            .arg("-F")
            .arg("dwarf")
            .arg(source_file);
        log_command(&nasm_command);
        let result = nasm_command
            .stderr(Stdio::inherit())
            .output()
            .expect("failed to execute nasm");
        info!("assembler ended in {:?}", start.elapsed());

        if !result.status.success() {
            panic!("Error running nasm")
        }
    }

    fn link(&self, path: &Path, requires: &[String]) {
        let start = Instant::now();
        let result = match self.linker {
            Linker::Ld => {
                let mut ld_command = Command::new("ld");
                ld_command
                    .arg("-m")
                    .arg("elf_i386")
                    .arg(path.with_extension("o"))
                    .arg("-lc")
                    .arg("-I")
                    .arg("/lib32/ld-linux.so.2")
                    .arg("-o")
                    .arg(path.with_extension(""));
                log_command(&ld_command);
                ld_command
                    .stderr(Stdio::inherit())
                    .output()
                    .expect("failed to execute ld")
            }
            Linker::Gcc => {
                let libraries = requires
                    .iter()
                    .filter(|it| *it != "libc")
                    .map(|it| format!("-l{it}"))
                    .collect::<Vec<String>>();

                let mut gcc_command = Command::new("gcc");
                gcc_command
                    //.arg("-Wstack-usage=16384")
                    .arg("-m32")
                    .arg("-gdwarf")
                    //.arg("-static")
                    .arg("-o")
                    .arg(path.with_extension(""))
                    .arg(path.with_extension("o"))
                    .args(libraries);
                log_command(&gcc_command);
                gcc_command
                    .stderr(Stdio::inherit())
                    .output()
                    .expect("failed to execute gcc")
            }
        };
        info!("linker ended in {:?}", start.elapsed());

        if !result.status.success() {
            panic!("Error running linker")
        }
    }

    fn type_size(&self, ast_typed_type: &ASTTypedType) -> Option<String> {
        let type_len = self.type_len(ast_typed_type);

        if type_len == 1 {
            Some("byte".to_string())
        } else if type_len == 2 {
            Some("word".to_string())
        } else if type_len == 4 {
            Some("dword".to_string())
        } else if type_len == 8 {
            Some("qword".to_string())
        } else {
            None
        }
    }

    fn debug_asm(&self) -> bool {
        self.debug
    }
}

impl BackendAsm for BackendNasmi386 {
    fn stack_base_pointer(&self) -> &str {
        "ebp"
    }

    fn stack_pointer(&self) -> &str {
        "esp"
    }

    fn pointer_size(&self) -> &str {
        "dword"
    }

    fn word_size(&self) -> &str {
        "dword"
    }
}
