use std::collections::HashSet;
use std::panic::RefUnwindSafe;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::time::Instant;

use linked_hash_map::LinkedHashMap;
use log::{debug, info};
use pad::PadStr;

use crate::codegen::lambda::LambdaSpace;
use crate::codegen::stack::StackVals;
use crate::codegen::statics::MemoryValue::Mem;
use crate::codegen::statics::{MemoryUnit, MemoryValue, Statics};
use crate::codegen::text_macro::{MacroParam, TextMacro, TextMacroEvaluator, TypeDefProvider};
use crate::codegen::val_context::ValContext;
use crate::codegen::{CodeGen, TypedValKind, ValKind};
use crate::debug_i;
use crate::parser::ast::{ASTIndex, ASTType, BuiltinTypeKind, ValueType};
use crate::transformations::typed_enum_functions_creator::enum_has_references;
use crate::transformations::typed_struct_functions_creator::struct_has_references;
use crate::transformations::typed_type_functions_creator::type_has_references;
use crate::type_check::typed_ast::{
    ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedParameterDef, ASTTypedType,
    BuiltinTypedTypeKind, DefaultFunctionCall,
};

pub trait Backend: RefUnwindSafe {
    fn address_from_base_pointer(&self, index: i8) -> String;

    fn address_from_stack_pointer(&self, index: i8) -> String;

    fn type_len(&self, ast_typed_type: &ASTTypedType) -> u8;

    fn word_len(&self) -> usize;

    fn stack_base_pointer(&self) -> String;

    fn stack_pointer(&self) -> String;

    fn compile_and_link(&self, source_file: &PathBuf);

    fn compile(&self, source_file: &PathBuf) -> Output;

    fn link(&self, path: &Path) -> Output;

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
        context: &ValContext,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Vec<(TextMacro, DefaultFunctionCall)>;

    fn remove_comments_from_line(&self, line: String) -> String;

    fn store_function_result_in_stack(&self, code: &mut String, address_relative_to_bp: i32);

    fn call_add_ref(
        &self,
        out: &mut String,
        source: &str,
        type_name: &str,
        descr: &str,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    );

    /// add ref, but not recursively
    fn call_add_ref_simple(
        &self,
        out: &mut String,
        source: &str,
        descr: &str,
        statics: &mut Statics,
    );

    fn call_deref(
        &self,
        source: &str,
        type_name: &str,
        descr: &str,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String;

    /// deref, but not recursively
    fn call_deref_simple(&self, out: &mut String, source: &str, descr: &str, statics: &mut Statics);

    fn function_preamble(&self, out: &mut String);

    fn restore_stack(&self, stack: &StackVals, out: &mut String);

    fn reserve_stack(&self, stack: &StackVals, out: &mut String);

    fn function_end(&self, out: &mut String, add_return: bool);

    fn value_to_string(&self, value_type: &ValueType) -> String;

    fn create_lambda_addref(
        &self,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
    ) -> Option<ASTTypedFunctionDef>;

    fn create_lambda_deref(
        &self,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
    ) -> Option<ASTTypedFunctionDef>;

    fn debug_asm(&self) -> bool;

    fn generate_statics_code(&self, statics: &Statics) -> (String, String);
}

enum Linker {
    Ld,
    Gcc,
}

pub struct BackendNasm386 {
    requires: HashSet<String>,
    externals: HashSet<String>,
    linker: Linker,
    libc: bool,
    debug_asm: bool,
}

impl BackendNasm386 {
    pub fn new(requires: HashSet<String>, externals: HashSet<String>, debug_asm: bool) -> Self {
        let libc = true; //requires.contains("libc");
        Self {
            requires,
            externals,
            linker: if libc { Linker::Gcc } else { Linker::Ld },
            libc,
            debug_asm,
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

    /// little endian
    fn array_to_u32_le(array: &[u8; 4]) -> u32 {
        (array[0] as u32)
            + ((array[1] as u32) << 8)
            + ((array[2] as u32) << 16)
            + ((array[3] as u32) << 24)
    }

    fn get_type_from_typed_type(
        ast_typed_type: &ASTTypedType,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Option<ASTType> {
        match ast_typed_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => Some(ASTType::Builtin(BuiltinTypeKind::String)),
                BuiltinTypedTypeKind::I32 => Some(ASTType::Builtin(BuiltinTypeKind::I32)),
                BuiltinTypedTypeKind::Bool => Some(ASTType::Builtin(BuiltinTypeKind::Bool)),
                BuiltinTypedTypeKind::Char => Some(ASTType::Builtin(BuiltinTypeKind::Char)),
                BuiltinTypedTypeKind::F32 => Some(ASTType::Builtin(BuiltinTypeKind::F32)),
                BuiltinTypedTypeKind::Lambda { .. } => todo!(),
            },
            _ => type_def_provider.get_type_from_typed_type(ast_typed_type),
        }
    }

    fn create_lambda_add_ref_like_function(
        &self,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
        is_deref: bool,
    ) -> Option<ASTTypedFunctionDef> {
        let mut body = String::new();

        let ws = self.word_size();
        let wl = self.word_len();

        if is_deref {
            self.call_deref_simple(&mut body, "$address", &format!("main {name}"), statics);
        } else {
            self.call_add_ref_simple(&mut body, "$address", &format!("main {name}"), statics);
        }

        let mut initialized = false;
        if !lambda_space.is_empty() {
            for (i, (val_name, kind)) in lambda_space.iter().enumerate() {
                let ast_typed_type = match kind {
                    TypedValKind::ParameterRef(_, def) => &def.ast_type,
                    TypedValKind::LetRef(_, typed_type) => typed_type,
                };
                if let Some(type_name) =
                    CodeGen::get_reference_type_name(ast_typed_type, type_def_provider)
                {
                    if !initialized {
                        CodeGen::add(&mut body, "push   ebx", None, true);
                        CodeGen::add(&mut body, &format!("mov {ws} ebx, $address"), None, true);
                        CodeGen::add(&mut body, &format!("mov {ws} ebx, [ebx]"), None, true);
                        CodeGen::add(&mut body, &format!("add {ws} ebx, {}", wl * 3), None, true);
                        initialized = true;
                    }
                    if is_deref {
                        body.push_str(&self.call_deref(
                            &format!("[ebx + {}]", i * self.word_len()),
                            &type_name,
                            &format!("\"{val_name}\" in lambda context"),
                            type_def_provider,
                            statics,
                        ));
                    } else {
                        self.call_add_ref(
                            &mut body,
                            &format!("[ebx + {}]", i * self.word_len()),
                            &type_name,
                            &format!("\"{val_name}\" in lambda context"),
                            type_def_provider,
                            statics,
                        );
                    }
                }
            }
            CodeGen::add(&mut body, "pop   ebx", None, true);
        }

        if !initialized {
            return None;
        }

        let parameters = vec![
            ASTTypedParameterDef {
                name: "address".to_owned(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::I32),
                ast_index: ASTIndex::none(),
            },
            ASTTypedParameterDef {
                name: "descr".to_owned(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: ASTIndex::none(),
            },
        ];

        Some(ASTTypedFunctionDef {
            name: name.to_owned(),
            parameters,
            body: ASTTypedFunctionBody::ASMBody(body),
            return_type: None,
            generic_types: LinkedHashMap::new(),
            inline: false,
        })
    }
}

impl Backend for BackendNasm386 {
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

    fn compile_and_link(&self, source_file: &PathBuf) {
        let nasm_output = self.compile(source_file);
        if nasm_output.status.success() {
            let path = Path::new(&source_file);
            let linker_output = self.link(path);

            if !linker_output.status.success() {
                panic!("Error running linker")
            }
        } else {
            panic!("Error running nasm")
        }
    }

    fn compile(&self, source_file: &PathBuf) -> Output {
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
        BackendNasm386::log_command(&nasm_command);
        let result = nasm_command
            .stderr(Stdio::inherit())
            .output()
            .expect("failed to execute nasm");
        info!("assembler ended in {:?}", start.elapsed());
        result
    }

    fn link(&self, path: &Path) -> Output {
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
                BackendNasm386::log_command(&ld_command);
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
                BackendNasm386::log_command(&gcc_command);
                gcc_command
                    .stderr(Stdio::inherit())
                    .output()
                    .expect("failed to execute gcc")
            }
        };
        info!("linker ended in {:?}", start.elapsed());
        result
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
        CodeGen::add(code, "%macro gotoOnSome 1", None, false);
        CodeGen::add(code, "cmp dword eax,[_enum_Option_None]", None, true);
        CodeGen::add(code, "jne %1", None, true);
        CodeGen::add(code, "%endmacro", None, false);
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
        context: &ValContext,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Vec<(TextMacro, DefaultFunctionCall)> {
        let mut result = Vec::new();

        // TODO I don't like to create it
        let evaluator = TextMacroEvaluator::new();

        for (m, i) in evaluator.get_macros(self, function_def, body, type_def_provider) {
            if m.name == "call" {
                debug_i!("found call macro {:?}", m);
                let types = m
                    .parameters
                    .iter()
                    .skip(1)
                    .map(|it| {
                        let ast_type = match it {
                            MacroParam::Plain(_, opt_type, _) => match opt_type {
                                None => ASTType::Builtin(BuiltinTypeKind::I32),
                                Some(ast_type) => ast_type.clone(),
                            },
                            MacroParam::StringLiteral(_) => {
                                ASTType::Builtin(BuiltinTypeKind::String)
                            }
                            MacroParam::Ref(name, None, _) => {
                                debug_i!("found ref {name}");
                                match context.get(name.strip_prefix('$').unwrap()).unwrap() {
                                    ValKind::ParameterRef(_, par) => par.ast_type.clone(),
                                    ValKind::LetRef(_, ast_type, _) => ast_type.clone(),
                                }
                            }
                            MacroParam::Ref(name, Some(ast_type), _) => {
                                debug_i!("found ref {name} : {ast_type}");
                                ast_type.clone()
                            }
                        };

                        match &ast_type {
                            ASTType::Generic(name) => {
                                if let Some(f) = function_def {
                                    let t = type_def_provider
                                        .get_type_from_typed_type(
                                            f.generic_types.get(name).unwrap(),
                                        )
                                        .unwrap();
                                    println!("Function specified, found type {:?} for {name}", t);
                                    t
                                } else {
                                    println!("Function not specified, cannot find type {name}");
                                    ast_type.clone()
                                }
                            }
                            ASTType::Custom {
                                name,
                                param_types: _,
                                index: _,
                            } => {
                                let result = if let Some(f) = function_def {
                                    if let Some(t) = f.generic_types.get(name) {
                                        Self::get_type_from_typed_type(t, type_def_provider)
                                            .unwrap_or_else(|| panic!("name {name} t {t}"))
                                    } else if let Some(t) =
                                        type_def_provider.get_type_from_typed_type_name(name)
                                    {
                                        t
                                    } else {
                                        ast_type.clone()
                                    }
                                } else {
                                    ast_type.clone()
                                };

                                result
                            }
                            _ => ast_type,
                        }
                    })
                    .collect();

                let function_name =
                    if let Some(MacroParam::Plain(function_name, _, _)) = m.parameters.get(0) {
                        function_name
                    } else {
                        panic!("Error getting the function name");
                    };

                result.push((m.clone(), DefaultFunctionCall::new(function_name, types, i)));
            }
        }
        result
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
        descr_for_debug: &str,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) {
        //println!("add ref {descr}");
        let ws = self.word_size();
        let wl = self.word_len();

        if descr_for_debug.is_empty() {
            panic!();
        }

        let descr = if self.debug_asm { descr_for_debug } else { "" };

        let key = statics.add_str(descr);

        CodeGen::add(out, "", Some(&("add ref ".to_owned() + descr)), true);

        let (has_references, is_type) =
            if let Some(struct_def) = type_def_provider.get_struct_def_by_name(type_name) {
                (struct_has_references(struct_def, type_def_provider), false)
            } else if let Some(enum_def) = type_def_provider.get_enum_def_by_name(type_name) {
                (enum_has_references(enum_def, type_def_provider), false)
            } else if let Some(type_def) = type_def_provider.get_type_def_by_name(type_name) {
                (type_has_references(type_def), true)
            } else if "str" == type_name || "_fn" == type_name {
                (true, false)
            } else {
                panic!("cannot find type {descr} {type_name}");
            };

        if "_fn" == type_name {
            CodeGen::add(out, &format!("push     {ws} eax"), None, true);
            // tmp value to store the source since it can be eax...
            CodeGen::add(out, &format!("push     {ws} 0"), None, true);
            CodeGen::add(out, &format!("mov      {ws} eax,{source}"), None, true);
            CodeGen::add(
                out,
                &format!("mov      {ws} [{}],eax", self.stack_pointer()),
                None,
                true,
            );
            CodeGen::add(out, &format!("mov      {ws} eax,[eax]"), None, true);
            CodeGen::add(out, &format!("push     {ws} [{key}]"), None, true);
            CodeGen::add(
                out,
                &format!("push     {ws} [{} + {wl}]", self.stack_pointer()),
                None,
                true,
            );
            CodeGen::add(out, &format!("call     {ws} [eax + {wl}]"), None, true);
            // wl * 3 because we get reed even of the temp value in the stack
            CodeGen::add(out, &format!("add      esp,{}", 3 * wl), None, true);
            CodeGen::add(out, &format!("pop      {ws} eax"), None, true);
        } else if has_references {
            let call = if type_name == "str" {
                "call     str_addRef_0".to_string()
            } else {
                format!("call     {type_name}_addRef")
            };
            if is_type {
                CodeGen::add(out, &format!("push     {ws} [{key}]"), None, true);
            }
            CodeGen::add(out, &format!("push     {ws} {source}"), None, true);
            CodeGen::add(out, &call, None, true);
            CodeGen::add(out, &format!("add      esp,{}", wl), None, true);
            if is_type {
                CodeGen::add(out, &format!("add      esp,{}", wl), None, true);
            }
        } else {
            CodeGen::add(out, &format!("push  {ws} [{key}]"), None, true);
            CodeGen::add(out, &format!("push     {ws} {source}"), None, true);
            CodeGen::add(out, "call     addRef_0", None, true);
            CodeGen::add(out, &format!("add      esp,{}", 2 * wl), None, true);
        }
    }

    fn call_add_ref_simple(
        &self,
        out: &mut String,
        source: &str,
        descr_for_debug: &str,
        statics: &mut Statics,
    ) {
        //println!("add ref {descr}");
        let ws = self.word_size();
        let wl = self.word_len();

        if descr_for_debug.is_empty() {
            panic!();
        }

        let descr = if self.debug_asm { descr_for_debug } else { "" };

        let key = statics.add_str(descr);

        CodeGen::add(out, "", Some(&("add ref simple ".to_owned() + descr)), true);

        CodeGen::add(out, &format!("push  {ws} [{key}]"), None, true);
        CodeGen::add(out, &format!("push     {ws} {source}"), None, true);
        CodeGen::add(out, "call     addRef_0", None, true);
        CodeGen::add(out, &format!("add      esp,{}", 2 * wl), None, true);
    }

    fn call_deref(
        &self,
        source: &str,
        type_name: &str,
        descr_for_debug: &str,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String {
        let ws = self.word_size();
        let wl = self.word_len();

        let descr = if self.debug_asm { descr_for_debug } else { "" };

        let mut result = String::new();

        let (has_references, is_type) =
            if let Some(struct_def) = type_def_provider.get_struct_def_by_name(type_name) {
                (struct_has_references(struct_def, type_def_provider), false)
            } else if let Some(enum_def) = type_def_provider.get_enum_def_by_name(type_name) {
                (enum_has_references(enum_def, type_def_provider), false)
            } else if let Some(type_def) = type_def_provider.get_type_def_by_name(type_name) {
                (type_has_references(type_def), true)
            } else if "str" == type_name || "_fn" == type_name {
                (true, false)
            } else {
                panic!("cannot find type {descr} {type_name}");
            };

        let key = statics.add_str(descr);

        CodeGen::add(&mut result, "", Some(&("deref ".to_owned() + descr)), true);

        if "_fn" == type_name {
            CodeGen::add(&mut result, &format!("push     {ws} eax"), None, true);
            // tmp value to store the source since it can be eax...
            CodeGen::add(&mut result, &format!("push     {ws} 0"), None, true);
            CodeGen::add(
                &mut result,
                &format!("mov      {ws} eax,{source}"),
                None,
                true,
            );
            CodeGen::add(
                &mut result,
                &format!("mov      {ws} [{}],eax", self.stack_pointer()),
                None,
                true,
            );
            CodeGen::add(&mut result, &format!("mov      {ws} eax,[eax]"), None, true);
            CodeGen::add(&mut result, &format!("push     {ws} [{key}]"), None, true);
            CodeGen::add(
                &mut result,
                &format!("push     {ws} [{} + {wl}]", self.stack_pointer()),
                None,
                true,
            );
            CodeGen::add(
                &mut result,
                &format!("call     {ws} [eax + 2 * {wl}]"),
                None,
                true,
            );
            // wl * 3 because we get reed even of the temp value in the stack
            CodeGen::add(&mut result, &format!("add      esp,{}", wl * 3), None, true);
            CodeGen::add(&mut result, &format!("pop      {ws} eax"), None, true);
        } else if has_references {
            let call = if type_name == "str" {
                "call     str_deref_0".to_string()
            } else {
                format!("call     {type_name}_deref")
            };
            if is_type {
                CodeGen::add(&mut result, &format!("push     {ws} [{key}]"), None, true);
            }
            CodeGen::add(&mut result, &format!("push     {ws} {source}"), None, true);
            CodeGen::add(&mut result, &call, None, true);
            CodeGen::add(&mut result, &format!("add      esp,{}", wl), None, true);
            if is_type {
                CodeGen::add(&mut result, &format!("add      esp,{}", wl), None, true);
            }
        } else {
            CodeGen::add(&mut result, &format!("push  {ws} [{key}]"), None, true);
            CodeGen::add(&mut result, &format!("push     {ws} {source}"), None, true);
            CodeGen::add(&mut result, "call     deref_0", None, true);
            CodeGen::add(&mut result, &format!("add      esp,{}", 2 * wl), None, true);
        }

        result.push('\n');
        result
    }

    fn call_deref_simple(
        &self,
        out: &mut String,
        source: &str,
        descr_for_debug: &str,
        statics: &mut Statics,
    ) {
        let ws = self.word_size();
        let wl = self.word_len();

        let descr = if self.debug_asm { descr_for_debug } else { "" };

        let key = statics.add_str(descr);

        CodeGen::add(out, "", Some(&("deref ".to_owned() + descr)), true);
        CodeGen::add(out, &format!("push  {ws} [{key}]"), None, true);
        CodeGen::add(out, &format!("push     {ws} {source}"), None, true);
        CodeGen::add(out, "call     deref_0", None, true);
        CodeGen::add(out, &format!("add      esp,{}", 2 * wl), None, true);
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
                    "\nadd   {sp}, {}",
                    stack.len_of_local_vals() * self.word_len()
                ),
                Some("restore stack local vals (let)"),
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
                Some("reserve stack local vals (let)"),
                true,
            );
        } else {
            CodeGen::add(out, "", Some("NO local vals"), true);
        }
    }

    fn function_end(&self, out: &mut String, add_return: bool) {
        let bp = self.stack_base_pointer();
        CodeGen::add(out, &format!("\npop     {}", bp), None, true);
        if add_return {
            CodeGen::add(out, "ret", None, true);
        }
    }

    fn value_to_string(&self, value_type: &ValueType) -> String {
        match value_type {
            ValueType::Boolean(b) => if *b { "1" } else { "0" }.into(),
            ValueType::I32(n) => n.to_string(),
            ValueType::F32(n) => {
                format!("0x{:x}", n.to_bits())
            }
            ValueType::Char(c) => {
                let mut b = [0; 4];
                c.encode_utf8(&mut b);

                let result = Self::array_to_u32_le(&b);

                format!("{}", result)
            }
        }
    }

    fn create_lambda_addref(
        &self,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
    ) -> Option<ASTTypedFunctionDef> {
        self.create_lambda_add_ref_like_function(
            lambda_space,
            type_def_provider,
            statics,
            &format!("{name}_add_ref"),
            false,
        )
    }

    fn create_lambda_deref(
        &self,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
    ) -> Option<ASTTypedFunctionDef> {
        self.create_lambda_add_ref_like_function(
            lambda_space,
            type_def_provider,
            statics,
            &format!("{name}_deref"),
            true,
        )
    }

    fn debug_asm(&self) -> bool {
        self.debug_asm
    }

    fn generate_statics_code(&self, statics: &Statics) -> (String, String) {
        let mut data = String::new();
        let mut bss = String::new();

        let mut code = String::new();

        if !statics.statics().is_empty() {
            let mut keys: Vec<&String> = statics.statics().keys().collect();
            // sorted for test purposes
            keys.sort();

            for id in keys.iter() {
                let mut def = String::new();
                def.push_str(&id.pad_to_width(50));

                match statics.statics().get(*id).unwrap() {
                    MemoryValue::StringValue(s) => {
                        def.push_str("db    ");

                        let mut result = "'".to_string();

                        // TODO it is a naive way to do it: it is slow and it does not support something like \\n that should result in '\' as a char and 'n' as a char
                        for c in s.replace("\\n", "\n").replace("\\t", "\t").chars() {
                            if c.is_ascii_control() {
                                result.push_str(&format!("',{},'", c as u32));
                            } else {
                                result.push(c)
                            }
                        }

                        result.push_str("', 0h");

                        def.push_str(&result);

                        CodeGen::add(&mut data, &def, None, true);
                    }
                    MemoryValue::I32Value(i) => {
                        def.push_str("dd    ");
                        def.push_str(&format!("{}", i));
                        CodeGen::add(&mut data, &def, None, true);
                    }
                    Mem(len, unit) => {
                        match unit {
                            MemoryUnit::Bytes => def.push_str("resb "),
                            MemoryUnit::Words => def.push_str("resd "),
                        }
                        def.push_str(&format!("{}", len));
                        CodeGen::add(&mut bss, &def, None, true);
                    }
                }
            }
        }

        for (_, (key, value_key)) in statics.strings_map().iter() {
            // TODO _0
            CodeGen::add(
                &mut code,
                &format!("$call(addStaticStringToHeap_0, {value_key})"),
                None,
                true,
            );

            CodeGen::add(&mut code, &format!("mov dword [{key}], eax"), None, true);
        }

        for (label_allocation, label_memory) in statics.static_allocation().iter() {
            // TODO _0
            CodeGen::add(
                &mut code,
                &format!("$call(addStaticAllocation_0, {label_allocation}, {label_memory})"),
                None,
                true,
            );
        }

        for (label, (descr_label, value)) in statics.heap().iter() {
            // TODO _0
            CodeGen::add(
                &mut code,
                &format!("$call(addHeap_0, {label}, {descr_label}: str, {value})"),
                None,
                true,
            );
        }

        let mut declarations = String::new();
        declarations.push_str("SECTION .data\n");
        declarations.push_str(&data);
        declarations.push_str("SECTION .bss\n");
        declarations.push_str(&bss);
        declarations.push_str("SECTION .text\n");
        CodeGen::add(&mut declarations, "global  main", None, true);
        declarations.push_str("main:\n");
        (declarations, code)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::codegen::backend::{Backend, BackendNasm386};
    use crate::codegen::val_context::ValContext;
    use crate::utils::tests::DummyTypeDefProvider;

    #[test]
    fn called_functions() {
        let sut = BackendNasm386::new(Default::default(), Default::default(), false);

        assert_eq!(
            sut.called_functions(
                None,
                "$call(something)",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
            )
            .get(0)
            .unwrap()
            .1
            .name,
            "something".to_string()
        );
    }

    #[test]
    fn called_functions_in_comment() {
        let sut = BackendNasm386::new(Default::default(), Default::default(), false);

        assert!(sut
            .called_functions(
                None,
                "mov    eax, 1; $call(something)",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
            )
            .is_empty());
    }

    #[test]
    fn called_functions_external() {
        let mut externals = HashSet::new();
        externals.insert("something".into());

        let sut = BackendNasm386::new(Default::default(), externals, false);

        assert!(sut
            .called_functions(
                None,
                "call something",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
            )
            .is_empty());
    }
}
