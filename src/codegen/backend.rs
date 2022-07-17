use std::path::Path;
use crate::parser::ast::ASTTypeRef;
use std::process::{Command, Stdio};
use log::info;

pub trait Backend {

    fn address_from_base_pointer(&self, index: i8) -> String;

    fn address_from_stack_pointer(&self, index: i8) -> String;

    fn type_len(&self, type_ref: &ASTTypeRef) -> u8;

    fn word_len(&self) -> u8;

    fn stack_base_pointer(&self) -> String;

    fn stack_pointer(&self) -> String;

    fn compile_and_link(&self, source_file: String);

    fn type_size(&self, type_ref: &ASTTypeRef) -> Option<String>;

    fn pointer_size(&self) -> String;

    fn word_size(&self) -> String;

}

pub struct BackendAsm386 {}

impl BackendAsm386 {

    pub fn new() -> Self {
        Self {}
    }

}

impl Backend for BackendAsm386 {

    fn address_from_base_pointer(&self, index: i8) -> String {
        format!("[ebp+{}]", index * 4)
    }

    fn address_from_stack_pointer(&self, index: i8) -> String {
        format!("[esp+{}]", index * 4)
    }

    fn type_len(&self, _type_ref: &ASTTypeRef) -> u8 {
        4
    }

    fn word_len(&self) -> u8 {
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
            let ld_output = Command::new("ld")
                .arg("-m")
                .arg("elf_i386")
                .arg(path.with_extension("o"))
                .arg("-o")
                .arg(path.with_extension(""))
                .stderr(Stdio::inherit())
                .output()
                .expect("failed to execute ld");
            if !ld_output.status.success() {
               panic!("Error running ld")
            }
        } else {
            panic!("Error running nasm")
        }
    }

    fn type_size(&self, type_ref: &ASTTypeRef) -> Option<String> {
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
}