use std::collections::HashSet;
use crate::codegen::CodeGen;
use crate::type_check::typed_ast::ASTTypedTypeRef;
use log::info;
use std::path::Path;
use std::process::{Command, Stdio};

pub trait Backend {
    fn address_from_base_pointer(&self, index: i8) -> String;

    fn address_from_stack_pointer(&self, index: i8) -> String;

    fn type_len(&self, type_ref: &ASTTypedTypeRef) -> u8;

    fn word_len(&self) -> usize;

    fn stack_base_pointer(&self) -> String;

    fn stack_pointer(&self) -> String;

    fn compile_and_link(&self, source_file: String);

    fn type_size(&self, type_ref: &ASTTypedTypeRef) -> Option<String>;

    fn pointer_size(&self) -> String;

    fn word_size(&self) -> String;

    fn preamble(&self, code: &mut String);
}

enum Linker {
    Ld,
    Gcc,
}

pub struct BackendAsm386 {
    requires: HashSet<String>,
    externals: HashSet<String>,
    linker: Linker,
    libc: bool,
}

impl BackendAsm386 {
    pub fn new(requires: HashSet<String>, externals: HashSet<String>) -> Self {
        let libc = requires.contains("libc");
        Self {
            requires,
            externals,
            linker: if libc { Linker::Gcc } else { Linker::Ld },
            libc,
        }
    }
}

impl Backend for BackendAsm386 {
    fn address_from_base_pointer(&self, index: i8) -> String {
        format!("[ebp+{}]", index * 4)
    }

    fn address_from_stack_pointer(&self, index: i8) -> String {
        format!("[esp+{}]", index * 4)
    }

    fn type_len(&self, _type_ref: &ASTTypedTypeRef) -> u8 {
        4
    }

    fn word_len(&self) -> usize {
        4
    }

    fn stack_base_pointer(&self) -> String {
        "ebp".to_string()
    }

    fn stack_pointer(&self) -> String {
        "esp".to_string()
    }

    fn compile_and_link(&self, source_file: String) {
        info!("source file : '{}'", source_file);
        let path = Path::new(&source_file);
        let nasm_output = Command::new("nasm")
            .arg("-f")
            .arg("elf")
            .arg("-g")
            .arg("-F")
            .arg("dwarf")
            .arg(source_file.clone())
            .stderr(Stdio::inherit())
            .output()
            .expect("failed to execute nasm");
        if nasm_output.status.success() {
            let linker_output = match self.linker {
                Linker::Ld => Command::new("ld")
                    .arg("-m")
                    .arg("elf_i386")
                    .arg(path.with_extension("o"))
                    .arg("-lc")
                    .arg("-I")
                    .arg("/lib32/ld-linux.so.2")
                    .arg("-o")
                    .arg(path.with_extension(""))
                    .stderr(Stdio::inherit())
                    .output()
                    .expect("failed to execute ld"),
                Linker::Gcc => {
                    Command::new("gcc")
                        .arg("-m32")
                        .arg("-gdwarf")
                        //.arg("-nostartfiles") // don't use the standard clib main
                        .arg("-static")
                        .arg("-o")
                        .arg(path.with_extension(""))
                        .arg(path.with_extension("o"))
                        .stderr(Stdio::inherit())
                        .output()
                        .expect("failed to execute gcc")
                }
            };

            if !linker_output.status.success() {
                panic!("Error running linker")
            }
        } else {
            panic!("Error running nasm")
        }
    }

    fn type_size(&self, type_ref: &ASTTypedTypeRef) -> Option<String> {
        let type_len = self.type_len(type_ref);

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

    fn pointer_size(&self) -> String {
        "dword".into()
    }

    fn word_size(&self) -> String {
        "dword".into()
    }

    fn preamble(&self, code: &mut String) {
        if self.libc {
            CodeGen::add(code, "%DEFINE LIBC 1", None, false);
            CodeGen::add(code, "extern exit", None, true);
        }

        for e in self.externals.iter() {
            CodeGen::add(code, &format!("extern {e}"), None, true);
        }
    }
}