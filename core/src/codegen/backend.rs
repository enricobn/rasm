use auto_impl::auto_impl;
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

use linked_hash_map::LinkedHashMap;
use log::info;

use crate::codegen::compile_target::CompileTarget;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::stack::{StackEntryType, StackVals};
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_reference_type_name, CodeGenOptions, TypedValKind};
use crate::parser::ast::{ASTIndex, ASTNameSpace, ValueType};
use crate::transformations::typed_functions_creator::{
    enum_has_references, struct_has_references, type_has_references,
};
use crate::type_check::typed_ast::{
    ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedParameterDef, ASTTypedType,
    BuiltinTypedTypeKind,
};

static COUNT: AtomicUsize = AtomicUsize::new(0);

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

    fn restore(&self, stack: &StackVals, out: &mut String);

    fn reserve_local_vals(&self, stack: &StackVals, out: &mut String);

    fn function_end(&self, out: &mut String, add_return: bool);

    fn value_to_string(&self, value_type: &ValueType) -> String;

    fn create_lambda_addref(
        &self,
        namespace: &ASTNameSpace,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
    ) -> Option<ASTTypedFunctionDef>;

    fn create_lambda_deref(
        &self,
        namespace: &ASTNameSpace,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
    ) -> Option<ASTTypedFunctionDef>;

    fn debug_asm(&self) -> bool;

    fn strip_ifdef(&self, code: &str, def: &str) -> String;

    fn tmp_registers(&self) -> Vec<String>;

    fn allocate_lambda_space(
        &self,
        out: &mut String,
        register_to_store_result: &str,
        slots: usize,
        statics: &mut Statics,
    );

    fn allocate_lambda_space_in_stack(
        &self,
        out: &mut String,
        register_to_store_result: &str,
        stack_vals: &StackVals,
        slots: usize,
    );

    fn push_to_scope_stack(&self, out: &mut String, what: &str, stack_vals: &StackVals) -> usize;

    fn set_return_value(&self, out: &mut String, what: &str);

    fn populate_lambda_space(
        &self,
        out: &mut String,
        lambda_space_address: &str,
        function_name: &str,
        add_ref_function: &str,
        deref_function: &str,
    );

    fn define_debug(&self, out: &mut String);
}
/*
trait CloneBackendAsm {
    fn clone_boxed_backend_asm(&self) -> Box<dyn BackendAsm>;
}

impl<T> CloneBackendAsm for T
where
    T: BackendAsm + Clone + 'static + ?Sized,
{
    fn clone_boxed_backend_asm(&self) -> Box<dyn BackendAsm> {
        println!(
            "clone_boxed_backend_asm {:p}, {:?}",
            self,
            std::any::TypeId::of::<Self>()
        );
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn BackendAsm> {
    fn clone(&self) -> Self {
        println!("clone {:p}, {:?}", self, std::any::TypeId::of::<Self>());
        self.clone_boxed_backend_asm()
    }
}

 */

#[auto_impl(Box)]
pub trait BackendAsm: Backend + Send + Sync {
    fn stack_base_pointer(&self) -> &str;

    fn stack_pointer(&self) -> &str;

    fn pointer_size(&self) -> &str;

    fn word_size(&self) -> &str;

    fn indirect_mov(
        &self,
        out: &mut String,
        source: &str,
        dest: &str,
        temporary_register: &str,
        comment: Option<&str>,
    );

    fn return_register(&self) -> &str;
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
    target: CompileTarget,
    debug: bool,
}

impl BackendNasmi386 {
    pub fn new(options: CodeGenOptions, debug: bool) -> Self {
        let libc = true; //requires.contains("libc");
        Self {
            linker: if libc { Linker::Gcc } else { Linker::Ld },
            libc,
            target: CompileTarget::Nasmi386(options.clone()),
            debug,
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

    fn create_lambda_add_ref_like_function(
        &self,
        namespace: &ASTNameSpace,
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
                if let Some(type_name) = get_reference_type_name(ast_typed_type, type_def_provider)
                {
                    if !initialized {
                        self.target.add(&mut body, "push   ebx", None, true);
                        self.target
                            .add(&mut body, &format!("mov {ws} ebx, $address"), None, true);
                        self.target
                            .add(&mut body, &format!("mov {ws} ebx, [ebx]"), None, true);
                        self.target.add(
                            &mut body,
                            &format!("add {ws} ebx, {}", wl * 3),
                            None,
                            true,
                        );
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
            self.target.add(&mut body, "pop   ebx", None, true);
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
            namespace: namespace.clone(),
            name: name.to_owned(),
            original_name: name.to_owned(),
            parameters,
            body: ASTTypedFunctionBody::NativeBody(body),
            return_type: ASTTypedType::Unit,
            generic_types: LinkedHashMap::new(),
            inline: false,
            index: ASTIndex::none(),
        })
    }
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
        BackendNasmi386::log_command(&nasm_command);
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
                BackendNasmi386::log_command(&ld_command);
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
                BackendNasmi386::log_command(&gcc_command);
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

    fn store_function_result_in_stack(&self, code: &mut String, address_relative_to_bp: i32) {
        let ws = self.word_size();
        let bp = self.stack_base_pointer();

        self.target.add(
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

        let descr = if self.debug { descr_for_debug } else { "" };

        let key = statics.add_str(descr);

        self.target
            .add(out, "", Some(&("add ref ".to_owned() + descr)), true);

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
                panic!("call_add_ref, cannot find type {descr} {type_name}");
            };

        if "_fn" == type_name {
            self.target
                .add(out, &format!("push     {ws} eax"), None, true);
            // tmp value to store the source since it can be eax...
            self.target
                .add(out, &format!("push     {ws} 0"), None, true);
            self.target
                .add(out, &format!("mov      {ws} eax,{source}"), None, true);
            self.target.add(
                out,
                &format!("mov      {ws} [{}],eax", self.stack_pointer()),
                None,
                true,
            );
            self.target
                .add(out, &format!("mov      {ws} eax,[eax]"), None, true);
            self.target
                .add(out, &format!("push     {ws} [{key}]"), None, true);
            self.target.add(
                out,
                &format!("push     {ws} [{} + {wl}]", self.stack_pointer()),
                None,
                true,
            );
            self.target
                .add(out, &format!("call     {ws} [eax + {wl}]"), None, true);
            // wl * 3 because we get reed even of the temp value in the stack
            self.target
                .add(out, &format!("add      esp,{}", 3 * wl), None, true);
            self.target
                .add(out, &format!("pop      {ws} eax"), None, true);
        } else if has_references {
            let call = if type_name == "str" {
                "call     str_addRef".to_string()
            } else {
                format!("call     {type_name}_addRef")
            };
            if is_type {
                self.target
                    .add(out, &format!("push     {ws} [{key}]"), None, true);
            }
            self.target
                .add(out, &format!("push     {ws} {source}"), None, true);
            self.target.add(out, &call, None, true);
            self.target
                .add(out, &format!("add      esp,{}", wl), None, true);
            if is_type {
                self.target
                    .add(out, &format!("add      esp,{}", wl), None, true);
            }
        } else {
            self.target
                .add(out, &format!("push  {ws} [{key}]"), None, true);
            self.target
                .add(out, &format!("push     {ws} {source}"), None, true);
            self.target.add(out, "call     addRef", None, true);
            self.target
                .add(out, &format!("add      esp,{}", 2 * wl), None, true);
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

        let descr = if self.debug { descr_for_debug } else { "" };

        let key = statics.add_str(descr);

        self.target
            .add(out, "", Some(&("add ref simple ".to_owned() + descr)), true);

        self.target
            .add(out, &format!("push  {ws} [{key}]"), None, true);
        self.target
            .add(out, &format!("push     {ws} {source}"), None, true);
        self.target.add(out, "call     addRef", None, true);
        self.target
            .add(out, &format!("add      esp,{}", 2 * wl), None, true);
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

        let descr = if self.debug { descr_for_debug } else { "" };

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
                panic!("call_deref, cannot find type {descr} {type_name}");
            };

        let key = statics.add_str(descr);

        self.target
            .add(&mut result, "", Some(&("deref ".to_owned() + descr)), true);

        if "_fn" == type_name {
            self.target
                .add(&mut result, &format!("push     {ws} eax"), None, true);
            // tmp value to store the source since it can be eax...
            self.target
                .add(&mut result, &format!("push     {ws} 0"), None, true);
            self.target.add(
                &mut result,
                &format!("mov      {ws} eax,{source}"),
                None,
                true,
            );
            self.target.add(
                &mut result,
                &format!("mov      {ws} [{}],eax", self.stack_pointer()),
                None,
                true,
            );
            self.target
                .add(&mut result, &format!("mov      {ws} eax,[eax]"), None, true);
            self.target
                .add(&mut result, &format!("push     {ws} [{key}]"), None, true);
            self.target.add(
                &mut result,
                &format!("push     {ws} [{} + {wl}]", self.stack_pointer()),
                None,
                true,
            );
            self.target.add(
                &mut result,
                &format!("call     {ws} [eax + 2 * {wl}]"),
                None,
                true,
            );
            // wl * 3 because we get reed even of the temp value in the stack
            self.target
                .add(&mut result, &format!("add      esp,{}", wl * 3), None, true);
            self.target
                .add(&mut result, &format!("pop      {ws} eax"), None, true);
        } else if has_references {
            let call = if type_name == "str" {
                "call     str_deref".to_string()
            } else {
                format!("call     {type_name}_deref")
            };
            if is_type {
                self.target
                    .add(&mut result, &format!("push     {ws} [{key}]"), None, true);
            }
            self.target
                .add(&mut result, &format!("push     {ws} {source}"), None, true);
            self.target.add(&mut result, &call, None, true);
            self.target
                .add(&mut result, &format!("add      esp,{}", wl), None, true);
            if is_type {
                self.target
                    .add(&mut result, &format!("add      esp,{}", wl), None, true);
            }
        } else {
            self.target
                .add(&mut result, &format!("push  {ws} [{key}]"), None, true);
            self.target
                .add(&mut result, &format!("push     {ws} {source}"), None, true);
            self.target.add(&mut result, "call     deref", None, true);
            self.target
                .add(&mut result, &format!("add      esp,{}", 2 * wl), None, true);
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

        let descr = if self.debug { descr_for_debug } else { "" };

        let key = statics.add_str(descr);

        self.target
            .add(out, "", Some(&("deref ".to_owned() + descr)), true);
        self.target
            .add(out, &format!("push  {ws} [{key}]"), None, true);
        self.target
            .add(out, &format!("push     {ws} {source}"), None, true);
        self.target.add(out, "call     deref", None, true);
        self.target
            .add(out, &format!("add      esp,{}", 2 * wl), None, true);
    }

    fn function_preamble(&self, out: &mut String) {
        let sp = self.stack_pointer();
        let bp = self.stack_base_pointer();

        self.target.add(out, &format!("push    {}", bp), None, true);
        self.target
            .add(out, &format!("mov     {},{}", bp, sp), None, true);
    }

    fn restore(&self, stack: &StackVals, out: &mut String) {
        let mut local_vals_words = 0;
        for entry in stack.reserved_slots().borrow().iter().rev() {
            match entry.entry_type {
                StackEntryType::LocalVal => {
                    local_vals_words += 1;
                }
                StackEntryType::TmpRegister(ref register) => {
                    self.target.add(
                        out,
                        &format!("pop {register}"),
                        Some(&format!("restoring {}", entry.desc)),
                        true,
                    );
                }
                StackEntryType::ReturnRegister => {
                    self.target.add_empty_line(out);
                    self.target
                        .add(out, "pop eax", Some("restoring return register"), true);
                }
                StackEntryType::LocalFakeAllocation(size) => {
                    local_vals_words += size;
                }
            }
        }

        if local_vals_words > 0 {
            let sp = self.stack_pointer();
            self.target.add(
                out,
                &format!("add   {sp}, {}", local_vals_words * self.word_len()),
                Some("restore stack local vals (let)"),
                true,
            );
            stack.remove_all();
        }
    }

    fn reserve_local_vals(&self, stack: &StackVals, out: &mut String) {
        if stack.len_of_local_vals() > 0 {
            let sp = self.stack_pointer();
            self.target.add(
                out,
                &format!(
                    "sub   {sp}, {}",
                    stack.len_of_local_vals() * self.word_len()
                ),
                Some("reserve stack local vals (let)"),
                true,
            );
        } else {
            self.target.add(out, "", Some("NO local vals"), true);
        }
    }

    fn function_end(&self, out: &mut String, add_return: bool) {
        let bp = self.stack_base_pointer();
        self.target.add(out, &format!("pop     {}", bp), None, true);
        if add_return {
            self.target.add(out, "ret", None, true);
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
        namespace: &ASTNameSpace,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
    ) -> Option<ASTTypedFunctionDef> {
        self.create_lambda_add_ref_like_function(
            namespace,
            lambda_space,
            type_def_provider,
            statics,
            &format!("{name}_add_ref"),
            false,
        )
    }

    fn create_lambda_deref(
        &self,
        namespace: &ASTNameSpace,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
    ) -> Option<ASTTypedFunctionDef> {
        self.create_lambda_add_ref_like_function(
            namespace,
            lambda_space,
            type_def_provider,
            statics,
            &format!("{name}_deref"),
            true,
        )
    }

    fn debug_asm(&self) -> bool {
        self.debug
    }

    fn strip_ifdef(&self, code: &str, def: &str) -> String {
        let mut result = String::new();
        let owned = code.to_owned();

        let mut state = 0;

        for line in owned.lines() {
            if line.trim() == "%endif" && state == 1 {
                state = 0;
                continue;
            } else if line.trim().contains(&format!("%ifdef {def}")) {
                if state == 1 {
                    panic!();
                }
                state = 1;
            }

            if state == 0 {
                result.push_str(line);
                result.push('\n');
            }
        }

        result
    }

    fn tmp_registers(&self) -> Vec<String> {
        vec!["edx".to_owned(), "ecx".to_owned(), "ebx".to_owned()]
    }

    fn allocate_lambda_space(
        &self,
        out: &mut String,
        register_to_store_result: &str,
        slots: usize,
        statics: &mut Statics,
    ) {
        let label = statics.add_str("lambda space");
        self.target.call_function(
            out,
            "rasmalloc",
            &[
                (&format!("{}", slots * self.word_len()), None),
                (&format!("[{label}]"), None),
            ],
            Some("lambda space allocation"),
            self.debug,
        );

        self.target.add(
            out,
            &format!("mov    dword {register_to_store_result},eax",),
            None,
            true,
        );
    }

    fn allocate_lambda_space_in_stack(
        &self,
        out: &mut String,
        register_to_store_result: &str,
        stack_vals: &StackVals,
        slots: usize,
    ) {
        let sbp = self.stack_base_pointer();

        let tmp_register = stack_vals.reserve_tmp_register(out, self, "tmp_register", &self.target);

        let address_relative_to_bp_for_lambda_allocation = stack_vals.reserve_local_space(
            &format!(
                "optimized_lambda_space_{}",
                COUNT.fetch_add(1, Ordering::Relaxed)
            ),
            5,
        );

        let address_relative_to_bp_for_lambda_space = stack_vals.reserve_local_space(
            &format!(
                "optimized_lambda_space_mem_{}",
                COUNT.fetch_add(1, Ordering::Relaxed)
            ),
            slots,
        );

        self.target.add(
            out,
            &format!("mov dword {register_to_store_result},{sbp}"),
            None,
            true,
        );
        self.target.add(
            out,
            &format!(
                "sub dword {register_to_store_result},{}",
                address_relative_to_bp_for_lambda_allocation * self.word_len()
            ),
            None,
            true,
        );
        self.target
            .add(out, &format!("mov dword {tmp_register},{sbp}"), None, true);
        self.target.add(
            out,
            &format!(
                "sub dword {tmp_register},{}",
                address_relative_to_bp_for_lambda_space * self.word_len()
            ),
            None,
            true,
        );

        self.target.add(
            out,
            &format!("mov     dword [{register_to_store_result}], {tmp_register}"),
            None,
            true,
        );
        self.target.add(
            out,
            &format!("mov     dword [{register_to_store_result} + 4], 1"),
            None,
            true,
        );
        self.target.add(
            out,
            &format!(
                "mov     dword [{register_to_store_result} + 8], {}",
                slots * self.word_len()
            ),
            None,
            true,
        );
        self.target.add(
            out,
            &format!("mov     dword [{register_to_store_result} + 12], 1"),
            None,
            true,
        );
        self.target.add(
            out,
            &format!("mov     dword [{register_to_store_result} + 16], 0"),
            None,
            true,
        );

        stack_vals.release_tmp_register(&self.target, out, "tmp_register");
    }

    fn push_to_scope_stack(&self, out: &mut String, what: &str, stack_vals: &StackVals) -> usize {
        let pos = stack_vals.reserve_local_val(&format!(
            "scope_stack_{}",
            COUNT.fetch_add(1, Ordering::Relaxed)
        )) * self.word_len();

        self.target.add(out, "; scope push", None, true);
        if what.contains('[') {
            self.target.add(out, "push    ebx", None, true);
            self.target.add(
                out,
                &format!("mov     {} ebx, {what}", self.word_size(),),
                None,
                true,
            );
            self.target.add(
                out,
                &format!(
                    "mov     {} [{} - {}], ebx",
                    self.word_size(),
                    self.stack_base_pointer(),
                    pos
                ),
                None,
                true,
            );
            self.target.add(out, "pop    ebx", None, true);
        } else {
            self.target.add(
                out,
                &format!(
                    "mov     {} [{} - {}], {what}",
                    self.word_size(),
                    self.stack_base_pointer(),
                    pos
                ),
                None,
                true,
            );
        }
        pos
    }

    fn set_return_value(&self, out: &mut String, what: &str) {
        self.target.add(
            out,
            &format!("mov {} eax, {what}", self.word_size()),
            None,
            true,
        );
    }

    fn populate_lambda_space(
        &self,
        out: &mut String,
        lambda_space_address: &str,
        function_name: &str,
        add_ref_function: &str,
        deref_function: &str,
    ) {
        let ws = self.word_size();
        let wl = self.word_len();

        self.target.add(
            out,
            &format!("mov {} [{lambda_space_address}], {}", ws, function_name),
            None,
            true,
        );

        self.target.add(
            out,
            &format!(
                "mov {} [{lambda_space_address} + {wl}], {add_ref_function}",
                ws
            ),
            None,
            true,
        );
        self.target.add(
            out,
            &format!(
                "mov {} [{lambda_space_address} + 2 * {wl}], {deref_function}",
                ws
            ),
            None,
            true,
        );
    }

    fn define_debug(&self, out: &mut String) {
        self.target.add(out, "%define LOG_DEBUG 1", None, false);
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

    fn indirect_mov(
        &self,
        out: &mut String,
        source: &str,
        dest: &str,
        temporary_register: &str,
        comment: Option<&str>,
    ) {
        self.target.add(
            out,
            &format!(
                "mov  {} {}, [{}]",
                self.word_size(),
                temporary_register,
                source
            ),
            comment,
            true,
        );
        self.target.add(
            out,
            &format!(
                "mov  {} [{}], {}",
                self.word_size(),
                dest,
                temporary_register
            ),
            comment,
            true,
        );
    }

    fn return_register(&self) -> &str {
        "eax"
    }
}
