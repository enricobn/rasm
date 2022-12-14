use crate::codegen::stack::StackVals;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::TextMacroEvaluator;
use crate::codegen::CodeGen;
use crate::transformations::typed_enum_functions_creator::enum_has_references;
use crate::transformations::typed_struct_functions_creator::struct_has_references;
use crate::transformations::typed_type_functions_creator::type_has_references;
use crate::type_check::typed_ast::{ASTTypedFunctionDef, ASTTypedModule, ASTTypedType};
use log::info;
use std::collections::HashSet;
use std::path::Path;
use std::process::{Command, Stdio};

pub trait Backend {
    fn address_from_base_pointer(&self, index: i8) -> String;

    fn address_from_stack_pointer(&self, index: i8) -> String;

    fn type_len(&self, ast_typed_type: &ASTTypedType) -> u8;

    fn word_len(&self) -> usize;

    fn stack_base_pointer(&self) -> String;

    fn stack_pointer(&self) -> String;

    fn compile_and_link(&self, source_file: String);

    fn type_size(&self, ast_typed_type: &ASTTypedType) -> Option<String>;

    fn pointer_size(&self) -> String;

    fn word_size(&self) -> String;

    fn preamble(&self, code: &mut String);

    /// Returns the name of the functions called in the code
    ///
    /// # Arguments
    ///
    /// * `body`: the code to scan for function calls
    ///
    /// returns: Vec<String>
    fn called_functions(
        &self,
        function_def: Option<&ASTTypedFunctionDef>,
        body: &str,
        module: Option<&ASTTypedModule>,
    ) -> Vec<String>;

    fn remove_comments_from_line(&self, line: String) -> String;

    fn store_function_result_in_stack(&self, code: &mut String, address_relative_to_bp: i32);

    fn call_add_ref(
        &self,
        out: &mut String,
        source: &str,
        type_name: &str,
        descr: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    );

    fn call_deref(
        &self,
        source: &str,
        type_name: &str,
        descr: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String;

    fn function_preamble(&self, out: &mut String);

    fn restore_stack(&self, stack: &StackVals, out: &mut String);

    fn reserve_stack(&self, stack: &StackVals, out: &mut String);

    fn function_end(&self, out: &mut String, add_return: bool);
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

    fn log_command(command: &Command) {
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
}

impl Backend for BackendAsm386 {
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

    fn stack_base_pointer(&self) -> String {
        "ebp".to_string()
    }

    fn stack_pointer(&self) -> String {
        "esp".to_string()
    }

    fn compile_and_link(&self, source_file: String) {
        info!("source file : '{}'", source_file);
        let path = Path::new(&source_file);
        let mut nasm_command = Command::new("nasm");
        nasm_command
            .arg("-f")
            .arg("elf")
            .arg("-g")
            .arg("-F")
            .arg("dwarf")
            .arg(source_file.clone());
        BackendAsm386::log_command(&nasm_command);
        let nasm_output = nasm_command
            .stderr(Stdio::inherit())
            .output()
            .expect("failed to execute nasm");
        if nasm_output.status.success() {
            let linker_output = match self.linker {
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
                    BackendAsm386::log_command(&ld_command);
                    ld_command
                        .stderr(Stdio::inherit())
                        .output()
                        .expect("failed to execute ld")
                }
                Linker::Gcc => {
                    let libraries = self
                        .requires
                        .iter()
                        .filter(|it| *it != "libc")
                        .map(|it| format!("-l{it}"))
                        .collect::<Vec<String>>();

                    let mut gcc_command = Command::new("gcc");
                    gcc_command
                        .arg("-m32")
                        .arg("-gdwarf")
                        //.arg("-static")
                        .arg("-o")
                        .arg(path.with_extension(""))
                        .arg(path.with_extension("o"))
                        .args(libraries);
                    BackendAsm386::log_command(&gcc_command);
                    gcc_command
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

    /// Returns the name of the functions called in the code
    ///
    /// # Arguments
    ///
    /// * `body`: the code to scan for function calls
    ///
    /// returns: Vec<String>
    fn called_functions(
        &self,
        function_def: Option<&ASTTypedFunctionDef>,
        body: &str,
        module: Option<&ASTTypedModule>,
    ) -> Vec<String> {
        // TODO I don't like to create it
        let evaluator = TextMacroEvaluator::new();

        let body = evaluator.translate(
            self,
            &mut Statics::new(), // TODO I don't like to create it
            function_def,
            body,
            module,
        );

        body.lines()
            .map(|it| {
                let line = self.remove_comments_from_line(it.to_string());
                line.trim().to_string()
            })
            .filter(|it| {
                !it.starts_with(';')
                    && it.contains("call")
                    && !it.contains('[')
                    && !it.contains('$')
            })
            .map(|it| {
                let pos = it.find("call").unwrap();
                let s = it.split_at(pos + 4).1.trim();
                String::from_iter(s.chars().take_while(|it| !it.is_whitespace()))
            })
            .filter(|it| !it.is_empty() && !self.externals.contains(it))
            .collect()
    }

    fn remove_comments_from_line(&self, line: String) -> String {
        if let Some(pos) = line.find(';') {
            if pos > 0 {
                line.split_at(pos).0.to_string()
            } else {
                String::new()
            }
        } else {
            line
        }
    }

    fn store_function_result_in_stack(&self, code: &mut String, address_relative_to_bp: i32) {
        let ws = self.word_size();
        let bp = self.stack_base_pointer();

        CodeGen::add(
            code,
            &format!("mov {ws} [{bp} + {}], eax", address_relative_to_bp),
            Some(""),
            true,
        );
    }

    fn call_add_ref(
        &self,
        out: &mut String,
        source: &str,
        type_name: &str,
        descr: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) {
        //println!("add ref {descr}");
        let ws = self.word_size();
        let wl = self.word_len();

        let key = statics.add_str(descr);

        CodeGen::add(out, "", Some(&("add ref ".to_owned() + descr)), true);

        let has_references =
            if let Some(struct_def) = module.structs.iter().find(|it| it.name == type_name) {
                struct_has_references(struct_def)
            } else if let Some(enum_def) = module.enums.iter().find(|it| it.name == type_name) {
                enum_has_references(enum_def)
            } else if let Some(type_def) = module.types.iter().find(|it| it.name == type_name) {
                type_has_references(type_def)
            } else {
                true
            };

        if has_references {
            CodeGen::add(out, &format!("push     {ws} {source}"), None, true);
            CodeGen::add(out, &format!("call     {type_name}_addRef"), None, true);
            CodeGen::add(out, &format!("add      esp,{}", wl), None, true);
        } else {
            CodeGen::add(out, &format!("push  {ws} [{key}]"), None, true);
            CodeGen::add(out, &format!("push     {ws} {source}"), None, true);
            CodeGen::add(out, "call     addRef", None, true);
            CodeGen::add(out, &format!("add      esp,{}", 2 * wl), None, true);
        }
    }

    fn call_deref(
        &self,
        source: &str,
        type_name: &str,
        descr: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String {
        let ws = self.word_size();
        let wl = self.word_len();

        let mut result = String::new();

        let has_references = if "str" == type_name {
            true
        } else if let Some(struct_def) = module.structs.iter().find(|it| it.name == type_name) {
            struct_has_references(struct_def)
        } else if let Some(enum_def) = module.enums.iter().find(|it| it.name == type_name) {
            enum_has_references(enum_def)
        } else if let Some(type_def) = module.types.iter().find(|it| it.name == type_name) {
            type_has_references(type_def)
        } else {
            panic!(
                "cannot find type {descr} {type_name}: {:?}",
                module.enums.iter().map(|it| &it.name).collect::<Vec<_>>()
            );
        };

        CodeGen::add(&mut result, "", Some(&("deref ".to_owned() + descr)), true);
        if has_references {
            CodeGen::add(&mut result, &format!("push     {ws} {source}"), None, true);
            CodeGen::add(
                &mut result,
                &format!("call     {type_name}_deref"),
                None,
                true,
            );
            CodeGen::add(&mut result, &format!("add      esp,{}", wl), None, true);
        } else {
            let key = statics.add_str(descr);

            CodeGen::add(&mut result, &format!("push  {ws} [{key}]"), None, true);
            CodeGen::add(&mut result, &format!("push     {ws} {source}"), None, true);
            CodeGen::add(&mut result, "call     deref", None, true);
            CodeGen::add(&mut result, &format!("add      esp,{}", 2 * wl), None, true);
        }

        result
    }

    fn function_preamble(&self, out: &mut String) {
        let sp = self.stack_pointer();
        let bp = self.stack_base_pointer();

        CodeGen::add(out, &format!("push    {}", bp), None, true);
        CodeGen::add(out, &format!("mov     {},{}", bp, sp), None, true);
    }

    fn restore_stack(&self, stack: &StackVals, out: &mut String) {
        if stack.len_of_local_vals() > 0 {
            let sp = self.stack_pointer();
            CodeGen::add(
                out,
                &format!(
                    "add   {sp}, {}",
                    stack.len_of_local_vals() * self.word_len()
                ),
                Some("local vals (let)"),
                true,
            );
            stack.remove_all();
        }
    }

    fn reserve_stack(&self, stack: &StackVals, out: &mut String) {
        if stack.len_of_local_vals() > 0 {
            let sp = self.stack_pointer();
            CodeGen::add(
                out,
                &format!(
                    "sub   {sp}, {}",
                    stack.len_of_local_vals() * self.word_len()
                ),
                Some("local vals (let)"),
                true,
            );
        }
    }

    fn function_end(&self, out: &mut String, add_return: bool) {
        let bp = self.stack_base_pointer();
        CodeGen::add(out, &format!("pop     {}", bp), None, true);
        if add_return {
            CodeGen::add(out, "ret", None, true);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::backend::{Backend, BackendAsm386};
    use std::collections::HashSet;

    #[test]
    fn called_functions() {
        let sut = BackendAsm386::new(Default::default(), Default::default());

        assert_eq!(
            sut.called_functions(None, "call something", None),
            vec!["something".to_string()]
        );
    }

    #[test]
    fn called_functions_in_comment() {
        let sut = BackendAsm386::new(Default::default(), Default::default());

        assert_eq!(
            sut.called_functions(None, "mov    eax, 1; call something", None),
            Vec::<String>::new()
        );
    }

    #[test]
    fn called_functions_external() {
        let mut externals = HashSet::new();
        externals.insert("something".into());

        let sut = BackendAsm386::new(Default::default(), externals);

        assert_eq!(
            sut.called_functions(None, "call something", None),
            Vec::<String>::new()
        );
    }
}
