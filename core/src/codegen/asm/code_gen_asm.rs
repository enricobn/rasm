use std::sync::atomic::{AtomicUsize, Ordering};

use linked_hash_map::LinkedHashMap;
use pad::PadStr;
use rasm_parser::parser::ast::ASTValueType;
use snailquote::unescape;

use crate::{
    codegen::{
        code_manipulator::{CodeManipulator, CodeManipulatorNasm},
        enh_ast::{EnhASTIndex, EnhASTNameSpace},
        get_reference_type_name,
        lambda::LambdaSpace,
        stack::{StackEntryType, StackVals},
        statics::{MemoryUnit, MemoryValue, Statics},
        text_macro::{AddRefMacro, RefType, TextMacroEval, TextMacroEvaluator},
        typedef_provider::TypeDefProvider,
        CodeGen, TypedValKind,
    },
    enh_type_check::typed_ast::{
        ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedModule,
        ASTTypedParameterDef, ASTTypedType, BuiltinTypedTypeKind,
    },
    transformations::typed_functions_creator::{
        enum_has_references, struct_has_references, type_has_references,
    },
};

use super::{
    backend::{Backend, BackendAsm, BackendNasmi386},
    function_call_parameters_asm::{FunctionCallParametersAsm, FunctionCallParametersAsmImpl},
    print_ref_macro_asm::AsmPrintRefMacro,
    text_macro_asm::{AsmCCallTextMacroEvaluator, AsmCallTextMacroEvaluator},
};
/// It's a marker that will be replaced by the code generator with the size (in bytes)
/// of all the vals in the stack. We need it since we know the full size only at the end of a function
/// generation, but we need that value during the code generation...   
pub const STACK_VAL_SIZE_NAME: &str = "$stack_vals_size";

static COUNT: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
pub struct AsmOptions {
    pub lambda_space_size: usize,
    pub heap_size: usize,
    pub heap_table_slots: usize,
    pub dereference: bool,
    pub optimize_unused_functions: bool,
    pub print_memory: bool,
    pub requires: Vec<String>,
    pub externals: Vec<String>,
}

impl Default for AsmOptions {
    fn default() -> Self {
        Self {
            lambda_space_size: 1024 * 1024,
            heap_size: 64 * 1024 * 1024,
            heap_table_slots: 1024 * 1024,
            print_memory: false,
            dereference: true,
            optimize_unused_functions: false,
            requires: vec!["libc".to_string()],
            externals: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct CodeGenAsm {
    backend: BackendNasmi386,
    options: AsmOptions,
    debug: bool,
    code_manipulator: CodeManipulatorNasm,
}

impl CodeGenAsm {
    pub fn new(options: AsmOptions, debug: bool) -> Self {
        /*
        crate::utils::debug_indent::INDENT.with(|indent| {
            *indent.borrow_mut() = 0;
        });

         */

        Self {
            //module,
            backend: BackendNasmi386::new(debug),
            options,
            debug,
            code_manipulator: CodeManipulatorNasm::new(),
        }
    }

    /// little endian
    fn array_to_u32_le(array: &[u8; 4]) -> u32 {
        (array[0] as u32)
            + ((array[1] as u32) << 8)
            + ((array[2] as u32) << 16)
            + ((array[3] as u32) << 24)
    }

    pub fn call_add_ref(
        &self,
        out: &mut String,
        source: &str,
        type_name: &str,
        descr_for_debug: &str,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) {
        //println!("add ref {descr}");
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

        if descr_for_debug.is_empty() {
            panic!();
        }

        let descr = if self.debug { descr_for_debug } else { "" };

        let key = statics.add_str(descr);

        self.add(out, "", Some(&("add ref ".to_owned() + descr)), true);

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
            self.add(out, &format!("push     {ws} eax"), None, true);
            // tmp value to store the source since it can be eax...
            self.add(out, &format!("push     {ws} 0"), None, true);
            self.add(out, &format!("mov      {ws} eax,{source}"), None, true);
            self.add(
                out,
                &format!("mov      {ws} [{}],eax", self.backend.stack_pointer()),
                None,
                true,
            );
            self.add(out, &format!("mov      {ws} eax,[eax]"), None, true);
            self.add(out, &format!("push     {ws} [{key}]"), None, true);
            self.add(
                out,
                &format!("push     {ws} [{} + {wl}]", self.backend.stack_pointer()),
                None,
                true,
            );
            self.add(out, &format!("call     {ws} [eax + {wl}]"), None, true);
            // wl * 3 because we get reed even of the temp value in the stack
            self.add(out, &format!("add      esp,{}", 3 * wl), None, true);
            self.add(out, &format!("pop      {ws} eax"), None, true);
        } else if has_references {
            let call = if type_name == "str" {
                "call     str_addRef".to_string()
            } else {
                format!("call     {type_name}_addRef")
            };
            if is_type {
                self.add(out, &format!("push     {ws} [{key}]"), None, true);
            }
            self.add(out, &format!("push     {ws} {source}"), None, true);
            self.add(out, &call, None, true);
            self.add(out, &format!("add      esp,{}", wl), None, true);
            if is_type {
                self.add(out, &format!("add      esp,{}", wl), None, true);
            }
        } else {
            self.add(out, &format!("push  {ws} [{key}]"), None, true);
            self.add(out, &format!("push     {ws} {source}"), None, true);
            self.add(out, "call     addRef", None, true);
            self.add(out, &format!("add      esp,{}", 2 * wl), None, true);
        }
    }

    pub fn call_add_ref_simple(
        &self,
        out: &mut String,
        source: &str,
        descr_for_debug: &str,
        statics: &mut Statics,
    ) {
        //println!("add ref {descr}");
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

        if descr_for_debug.is_empty() {
            panic!();
        }

        let descr = if self.debug { descr_for_debug } else { "" };

        let key = statics.add_str(descr);

        self.add(out, "", Some(&("add ref simple ".to_owned() + descr)), true);

        self.add(out, &format!("push  {ws} [{key}]"), None, true);
        self.add(out, &format!("push     {ws} {source}"), None, true);
        self.add(out, "call     addRef", None, true);
        self.add(out, &format!("add      esp,{}", 2 * wl), None, true);
    }

    pub fn call_deref(
        &self,
        source: &str,
        type_name: &str,
        descr_for_debug: &str,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String {
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

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

        self.add(&mut result, "", Some(&("deref ".to_owned() + descr)), true);

        if "_fn" == type_name {
            self.add(&mut result, &format!("push     {ws} eax"), None, true);
            // tmp value to store the source since it can be eax...
            self.add(&mut result, &format!("push     {ws} 0"), None, true);
            self.add(
                &mut result,
                &format!("mov      {ws} eax,{source}"),
                None,
                true,
            );
            self.add(
                &mut result,
                &format!("mov      {ws} [{}],eax", self.backend.stack_pointer()),
                None,
                true,
            );
            self.add(&mut result, &format!("mov      {ws} eax,[eax]"), None, true);
            self.add(&mut result, &format!("push     {ws} [{key}]"), None, true);
            self.add(
                &mut result,
                &format!("push     {ws} [{} + {wl}]", self.backend.stack_pointer()),
                None,
                true,
            );
            self.add(
                &mut result,
                &format!("call     {ws} [eax + 2 * {wl}]"),
                None,
                true,
            );
            // wl * 3 because we get reed even of the temp value in the stack
            self.add(&mut result, &format!("add      esp,{}", wl * 3), None, true);
            self.add(&mut result, &format!("pop      {ws} eax"), None, true);
        } else if has_references {
            let call = if type_name == "str" {
                "call     str_deref".to_string()
            } else {
                format!("call     {type_name}_deref")
            };
            if is_type {
                self.add(&mut result, &format!("push     {ws} [{key}]"), None, true);
            }
            self.add(&mut result, &format!("push     {ws} {source}"), None, true);
            self.add(&mut result, &call, None, true);
            self.add(&mut result, &format!("add      esp,{}", wl), None, true);
            if is_type {
                self.add(&mut result, &format!("add      esp,{}", wl), None, true);
            }
        } else {
            self.add(&mut result, &format!("push  {ws} [{key}]"), None, true);
            self.add(&mut result, &format!("push     {ws} {source}"), None, true);
            self.add(&mut result, "call     deref", None, true);
            self.add(&mut result, &format!("add      esp,{}", 2 * wl), None, true);
        }

        result.push('\n');
        result
    }

    pub fn call_deref_simple(
        &self,
        out: &mut String,
        source: &str,
        descr_for_debug: &str,
        statics: &mut Statics,
    ) {
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

        let descr = if self.debug { descr_for_debug } else { "" };

        let key = statics.add_str(descr);

        self.add(out, "", Some(&("deref ".to_owned() + descr)), true);
        self.add(out, &format!("push  {ws} [{key}]"), None, true);
        self.add(out, &format!("push     {ws} {source}"), None, true);
        self.add(out, "call     deref", None, true);
        self.add(out, &format!("add      esp,{}", 2 * wl), None, true);
    }

    pub fn create_lambda_addref(
        &self,
        namespace: &EnhASTNameSpace,
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

    pub fn create_lambda_deref(
        &self,
        namespace: &EnhASTNameSpace,
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

    pub fn tmp_registers(&self) -> Vec<String> {
        vec!["edx".to_owned(), "ecx".to_owned(), "ebx".to_owned()]
    }

    pub fn allocate_lambda_space(
        &self,
        out: &mut String,
        register_to_store_result: &str,
        slots: usize,
        statics: &mut Statics,
    ) {
        let label = statics.add_str("lambda space");
        self.call_function(
            out,
            "rasmalloc",
            &[
                (&format!("{}", slots * self.backend.word_len()), None),
                (&format!("[{label}]"), None),
            ],
            Some("lambda space allocation"),
            false,
            false,
        );

        self.add(
            out,
            &format!("mov    dword {register_to_store_result},eax",),
            None,
            true,
        );
    }

    pub fn allocate_lambda_space_in_stack(
        &self,
        out: &mut String,
        register_to_store_result: &str,
        stack_vals: &StackVals,
        slots: usize,
    ) {
        let sbp = self.backend.stack_base_pointer();

        let tmp_register = stack_vals.reserve_tmp_register(out, "tmp_register", self);

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

        self.add(
            out,
            &format!("mov dword {register_to_store_result},{sbp}"),
            None,
            true,
        );
        self.add(
            out,
            &format!(
                "sub dword {register_to_store_result},{}",
                address_relative_to_bp_for_lambda_allocation * self.backend.word_len()
            ),
            None,
            true,
        );
        self.add(out, &format!("mov dword {tmp_register},{sbp}"), None, true);
        self.add(
            out,
            &format!(
                "sub dword {tmp_register},{}",
                address_relative_to_bp_for_lambda_space * self.backend.word_len()
            ),
            None,
            true,
        );

        self.add(
            out,
            &format!("mov     dword [{register_to_store_result}], {tmp_register}"),
            None,
            true,
        );
        self.add(
            out,
            &format!("mov     dword [{register_to_store_result} + 4], 1"),
            None,
            true,
        );
        self.add(
            out,
            &format!(
                "mov     dword [{register_to_store_result} + 8], {}",
                slots * self.backend.word_len()
            ),
            None,
            true,
        );
        self.add(
            out,
            &format!("mov     dword [{register_to_store_result} + 12], 1"),
            None,
            true,
        );
        self.add(
            out,
            &format!("mov     dword [{register_to_store_result} + 16], 0"),
            None,
            true,
        );

        stack_vals.release_tmp_register(&self, out, "tmp_register");
    }

    pub fn push_to_scope_stack(
        &self,
        out: &mut String,
        what: &str,
        stack_vals: &StackVals,
    ) -> usize {
        let pos = stack_vals.reserve_local_val(&format!(
            "scope_stack_{}",
            COUNT.fetch_add(1, Ordering::Relaxed)
        )) * self.backend.word_len();

        self.add(out, "; scope push", None, true);
        if what.contains('[') {
            self.add(out, "push    ebx", None, true);
            self.add(
                out,
                &format!("mov     {} ebx, {what}", self.backend.word_size(),),
                None,
                true,
            );
            self.add(
                out,
                &format!(
                    "mov     {} [{} - {}], ebx",
                    self.backend.word_size(),
                    self.backend.stack_base_pointer(),
                    pos
                ),
                None,
                true,
            );
            self.add(out, "pop    ebx", None, true);
        } else {
            self.add(
                out,
                &format!(
                    "mov     {} [{} - {}], {what}",
                    self.backend.word_size(),
                    self.backend.stack_base_pointer(),
                    pos
                ),
                None,
                true,
            );
        }
        pos
    }

    pub fn set_return_value(&self, out: &mut String, what: &str) {
        self.add(
            out,
            &format!("mov {} eax, {what}", self.backend.word_size()),
            None,
            true,
        );
    }

    pub fn populate_lambda_space(
        &self,
        out: &mut String,
        lambda_space_address: &str,
        function_name: &str,
        add_ref_function: &str,
        deref_function: &str,
    ) {
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

        self.add(
            out,
            &format!("mov {} [{lambda_space_address}], {}", ws, function_name),
            None,
            true,
        );

        self.add(
            out,
            &format!(
                "mov {} [{lambda_space_address} + {wl}], {add_ref_function}",
                ws
            ),
            None,
            true,
        );
        self.add(
            out,
            &format!(
                "mov {} [{lambda_space_address} + 2 * {wl}], {deref_function}",
                ws
            ),
            None,
            true,
        );
    }

    pub fn indirect_mov(
        &self,
        out: &mut String,
        source: &str,
        dest: &str,
        temporary_register: &str,
        comment: Option<&str>,
    ) {
        self.add(
            out,
            &format!(
                "mov  {} {}, [{}]",
                self.backend.word_size(),
                temporary_register,
                source
            ),
            comment,
            true,
        );
        self.add(
            out,
            &format!(
                "mov  {} [{}], {}",
                self.backend.word_size(),
                dest,
                temporary_register
            ),
            comment,
            true,
        );
    }

    pub fn return_register(&self) -> &str {
        "eax"
    }

    pub fn stack_pointer(&self) -> &str {
        self.backend.stack_pointer()
    }
}

impl<'a> CodeGen<'a, Box<dyn FunctionCallParametersAsm + 'a>> for CodeGenAsm {
    fn options(&self) -> &AsmOptions {
        &self.options
    }

    fn end_main(&self, code: &mut String) {
        self.call_function(code, "exitMain", &[("0", None)], None, false, false);
    }

    fn transform_before(&self, stack: &StackVals, before: String) -> String {
        before.replace(
            STACK_VAL_SIZE_NAME,
            &(stack.len_of_all() * self.word_len()).to_string(),
        )
    }

    fn create_command_line_arguments(&self, generated_code: &mut String) {
        self.call_function(
            generated_code,
            "createCmdLineArguments",
            &[
                ("_rasm_args", None),
                (
                    &self.backend.stack_pointer(),
                    Some("command line arguments"),
                ),
            ],
            None,
            false,
            false,
        );
    }

    fn call_lambda_parameter(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        kind: &TypedValKind,
        call_parameters: &Box<dyn FunctionCallParametersAsm + 'a>,
        return_value: bool,
        is_inner_call: bool,
        statics: &Statics,
    ) {
        let rr = self.return_register();

        let index = match kind {
            TypedValKind::ParameterRef(index, _) => *index as i32 + 2,
            TypedValKind::LetRef(_, _) => {
                let relative_to_bp_found = stack_vals
                    .find_local_val_relative_to_bp(&function_call.function_name)
                    .unwrap();
                -(relative_to_bp_found as i32)
            }
        };

        self.add_comment(
            before,
            &format!(
                "calling lambda parameter reference to {}",
                &function_call.function_name
            ),
            true,
        );

        self.add(
            before,
            &format!(
                "mov {rr}, [{} + {}]",
                self.backend.stack_base_pointer(),
                index * self.backend.word_len() as i32
            ),
            None,
            true,
        );
        self.add(
            before,
            &format!("mov {} {rr}, [{rr}]", self.backend.word_size(),),
            None,
            true,
        );
        // we add the address to the "lambda space" as the last parameter to the lambda
        self.call_function(
            before,
            &format!("[{rr}]"),
            &[(rr, Some("address to the \"lambda space\""))],
            Some(&format!(
                "Calling function {} : {}",
                function_call.function_name, function_call.index
            )),
            false,
            false,
        );
    }

    fn call_lambda(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        index_in_lambda_space: usize,
        call_parameters: &Box<dyn FunctionCallParametersAsm + 'a>,
        ast_type_type: &ASTTypedType,
        statics: &Statics,
        return_value: bool,
        is_inner_call: bool,
    ) {
        let rr = self.return_register();

        if let Some(ref address) = stack_vals.find_tmp_register("lambda_space_address") {
            self.add(before, &format!("mov {rr}, {address}"), None, true);
        } else {
            panic!()
        }
        // we add the address to the "lambda space" as the last parameter of the lambda
        self.add(
            before,
            &format!(
                "add {rr}, {}",
                (index_in_lambda_space + 2) * self.backend.word_len()
            ),
            Some("address to the pointer to the allocation table of the lambda to call"),
            true,
        );
        self.add(
            before,
            &format!("mov {rr}, [{rr}]"),
            Some("address of the allocation table of the function to call"),
            true,
        );
        self.add(
            before,
            &format!("mov {rr}, [{rr}]"),
            Some("address to the \"lambda space\" of the function to call"),
            true,
        );

        self.call_function(
            before,
            &format!("[{rr}]"),
            &[(
                rr,
                Some("address to the \"lambda space\" of the function to call"),
            )],
            Some(&format!(
                "Calling function {} : {}",
                function_call.function_name, function_call.index
            )),
            false,
            false,
        );
    }

    fn restore_stack(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        call_parameters: &mut Box<dyn FunctionCallParametersAsm + 'a>,
    ) {
        if call_parameters.to_remove_from_stack() > 0 {
            let sp = self.backend.stack_pointer();
            let wl = self.backend.word_len();

            self.add(
                before,
                &format!(
                    "add     {},{}",
                    sp,
                    wl * (call_parameters.to_remove_from_stack())
                ),
                Some(&format!(
                    "restore stack for {}",
                    function_call.function_name
                )),
                true,
            );

            //debug!("going to remove from stack {}/{}", parent_def_description, function_call.function_name);

            //stack.remove(FunctionCallParameter, call_parameters.to_remove_from_stack());
        }
    }

    fn added_to_stack_for_call_parameter(
        &self,
        added_to_stack: &String,
        call_parameters: &Box<dyn FunctionCallParametersAsm + 'a>,
    ) -> String {
        let mut added_to_stack = added_to_stack.clone();
        added_to_stack.push_str(" + ");
        added_to_stack.push_str(&call_parameters.to_remove_from_stack_name());
        added_to_stack
    }

    fn function_call_parameters<'b, 'c>(
        &'a self,
        parameters: &'b Vec<ASTTypedParameterDef>,
        inline: bool,
        immediate: bool,
        stack_vals: &'c StackVals,
        id: usize,
    ) -> Box<dyn FunctionCallParametersAsm + 'a> {
        let fcp = FunctionCallParametersAsmImpl::new(
            &self.backend,
            parameters.clone(),
            inline,
            immediate,
            stack_vals.clone(),
            self.options().dereference,
            id,
            self,
        );

        Box::new(fcp)
    }

    fn store_function_result_in_stack(
        &self,
        code: &mut String,
        address_relative_to_bp: i32,
        name: &str,
        typed_type: &ASTTypedType,
        statics: &Statics,
    ) {
        let ws = self.backend.word_size();
        let bp = self.backend.stack_base_pointer();
        let wl = self.backend.word_len();

        self.add(
            code,
            &format!(
                "mov {ws} [{bp} + {}], eax",
                address_relative_to_bp * wl as i32
            ),
            Some(""),
            true,
        );
    }

    fn add_ref(
        &self,
        name: &str,
        statics: &mut Statics,
        body: &mut String,
        typed_module: &ASTTypedModule,
        index: &EnhASTIndex,
        type_name: &String,
    ) {
        let entry = statics.get_typed_const(name).unwrap();

        self.call_add_ref(
            body,
            &format!("[{}]", entry.key),
            &type_name,
            &format!("for const {name} : {index}"),
            typed_module,
            statics,
        );
    }

    fn call_deref_for_let_val(
        &self,
        name: &str,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
        t: &ASTTypedType,
    ) -> String {
        let bp = self.backend.stack_base_pointer();
        let wl = self.backend.word_len();

        self.call_deref(
            &format!("[{bp} - {}]", address_relative_to_bp * wl),
            &type_name,
            &format!("for let val {name}"),
            typed_module,
            statics,
        )
    }

    fn call_add_ref_for_let_val(
        &self,
        name: &str,
        index: &EnhASTIndex,
        before: &mut String,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
        t: &ASTTypedType,
    ) {
        let bp = self.backend.stack_base_pointer();
        let wl = self.backend.word_len();
        self.call_add_ref(
            before,
            &format!("[{bp} - {}]", address_relative_to_bp * wl),
            &type_name,
            &format!("for let val {name} : {index}"),
            typed_module,
            statics,
        );
    }

    fn set_let_const_for_function_call_result(
        &self,
        statics_key: &str,
        _before: &mut String,
        current: &mut String,
        _name: &str,
        _type_type: &ASTTypedType,
        _statics: &mut Statics,
    ) {
        let ws = self.backend.word_size();
        let rr = self.return_register();
        self.add(
            current,
            &format!("mov {ws} [{statics_key}], {rr}"),
            Some(""),
            true,
        );
    }

    fn set_let_for_value_ref(
        &self,
        stack: &StackVals,
        before: &mut String,
        address_relative_to_bp: usize,
        val_name: &String,
        typed_val_kind: &TypedValKind,
        statics: &Statics,
        name: &str,
    ) -> ASTTypedType {
        let ws = self.backend.word_size();
        let bp = self.backend.stack_base_pointer();
        let wl = self.backend.word_len();

        let (i, typed_type, descr) = match typed_val_kind {
            TypedValKind::ParameterRef(i, def) => (
                *i as i32 + 2,
                def.ast_type.clone(),
                format!("par {val_name}"),
            ),
            TypedValKind::LetRef(_i, def) => {
                let relative_to_bp_found = stack.find_local_val_relative_to_bp(val_name).unwrap();
                let index_in_context = -(relative_to_bp_found as i32);
                (index_in_context, def.clone(), format!("let {val_name}"))
            }
        };

        let tmp_register = stack.reserve_tmp_register(before, "set_let_for_value_ref", self);

        self.add(
            before,
            &format!(
                "mov {tmp_register}, [{} + {}]",
                self.backend.stack_base_pointer(),
                i * self.backend.word_len() as i32
            ),
            Some(&format!("let reference to {descr}")),
            true,
        );

        self.add(
            before,
            &format!(
                "mov {ws} [{bp} + {}], {tmp_register}",
                -((address_relative_to_bp * wl) as i32),
            ),
            Some(""),
            true,
        );

        stack.release_tmp_register(self, before, "set_let_for_value_ref");
        typed_type
    }

    fn set_let_for_string_literal(
        &self,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        address_relative_to_bp: usize,
        value: &String,
        typed_type: &ASTTypedType,
        stack: &StackVals,
    ) {
        let bp = self.backend.stack_base_pointer();
        let wl = self.backend.word_len();
        let label = statics.add_str(value);

        let tmp_reg = stack.reserve_tmp_register(body, "set_let_for_string_literal", self);

        if is_const {
            let key = statics.add_typed_const(name.to_owned(), typed_type.clone());

            self.indirect_mov(
                body,
                &label,
                &key,
                &tmp_reg,
                Some(&format!("const {name} string value")),
            );
        } else {
            self.indirect_mov(
                before,
                &label,
                &format!("{bp} + {}", -((address_relative_to_bp * wl) as i32),),
                &tmp_reg,
                None,
            );
        }

        stack.release_tmp_register(self, body, "set_let_for_string_literal");
    }

    fn set_let_for_value(
        &self,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        address_relative_to_bp: usize,
        value_type: &ASTValueType,
        typed_type: &ASTTypedType,
    ) {
        let bp = self.backend.stack_base_pointer();
        let ws = self.backend.word_size();
        let value = self.value_to_string(value_type);
        let wl = self.backend.word_len();

        if is_const {
            let key = statics.add_typed_const(name.to_owned(), typed_type.clone());

            self.add(
                body,
                &format!("mov {ws} [{key}], {}", value),
                Some(""),
                true,
            );
        } else {
            self.add(
                before,
                &format!(
                    "mov {ws} [{bp} + {}], {}",
                    -((address_relative_to_bp * wl) as i32),
                    value
                ),
                Some(""),
                true,
            );
        }
    }

    fn reserve_return_register(&self, out: &mut String, stack: &StackVals) {
        stack.reserve_return_register(self, out);
    }

    fn function_def(
        &'a self,
        out: &mut String,
        function_def: &ASTTypedFunctionDef,
        statics: &mut Statics,
    ) {
        self.add(out, &format!("{}:", function_def.name), None, false);
    }

    fn word_len(&self) -> usize {
        self.backend.word_len()
    }

    fn word_size(&self) -> &str {
        self.backend.word_size()
    }

    fn reserve_lambda_space(
        &self,
        before: &mut String,
        stack: &StackVals,
        statics: &mut Statics,
        lambda_space: &LambdaSpace,
        def: &ASTTypedFunctionDef,
    ) {
        let register = stack.reserve_tmp_register(before, "lambda_space_address", self);

        self.add(
            before,
            &format!(
                "mov     {register}, [{}+{}]",
                self.backend.stack_base_pointer(),
                self.backend.word_len() * 2
            ),
            Some("The address to the lambda space for inline lambda param"),
            true,
        );
    }

    fn value_as_return(
        &self,
        before: &mut String,
        value_type: &ASTValueType,
        statics: &mut Statics,
    ) {
        if let ASTValueType::String(s) = value_type {
            self.string_literal_return(statics, before, s);
        } else {
            let ws = self.backend.word_size();
            let rr = self.return_register();
            let v = self.value_to_string(value_type);
            self.add(before, &format!("mov     {ws} {rr}, {v}"), None, true);
        }
    }

    fn string_literal_return(&self, statics: &mut Statics, before: &mut String, value: &String) {
        let label = statics.add_str(value);
        let rr = self.return_register();

        self.add(
            before,
            &format!("mov     {} {rr}, [{label}]", self.backend.word_size()),
            None,
            true,
        );
    }

    fn print_memory_info(&self, native_code: &mut String, statics: &Statics) {
        self.call_function_simple(
            native_code,
            "printAllocated",
            None,
            false,
            false,
            None,
            statics,
        );
        self.call_function_simple(
            native_code,
            "printTableSlotsAllocated",
            None,
            false,
            false,
            None,
            statics,
        );
    }

    fn optimize_unused_functions(&self) -> bool {
        self.options.optimize_unused_functions
    }

    fn initialize_static_values(&self, native_code: &mut String) {
        let ws = self.backend.word_size();
        self.add_rows(
            native_code,
            vec![
                &format!("mov     {ws} [_heap], _heap_buffer"),
                &format!("mov     {ws} [_heap_table_next], _heap_table"),
                &format!("mov     {ws} [_lambda_space_stack], _lambda_space_stack_buffer"),
                &format!("mov     {ws} [_reusable_heap_table_next], _reusable_heap_table"),
            ],
            None,
            true,
        );
    }

    fn debug(&self) -> bool {
        self.debug
    }

    fn call_function_simple(
        &self,
        out: &mut String,
        function_name: &str,
        call_parameters: Option<&Box<dyn FunctionCallParametersAsm + 'a>>,
        return_value: bool,
        is_inner_call: bool,
        return_type: Option<&ASTTypedType>,
        statics: &Statics,
    ) {
        self.add(out, &format!("call    {}", function_name), None, true);
    }

    fn call_function(
        &self,
        out: &mut String,
        function_name: &str,
        args: &[(&str, Option<&str>)],
        comment: Option<&str>,
        return_value: bool,
        is_inner_call: bool,
    ) {
        if let Some(c) = comment {
            self.add_comment(out, c, true);
        }

        for (arg, comment) in args.iter().rev() {
            if let Some(c) = comment {
                self.add_comment(out, c, true);
            }
            self.add(
                out,
                &format!("push {} {arg}", self.word_size()),
                None, //*comment,
                true,
            );
        }
        self.add(out, &format!("call    {}", function_name), None, true);
        self.add(
            out,
            &format!(
                "add  {}, {}",
                self.stack_pointer(),
                self.word_len() * args.len()
            ),
            None,
            true,
        );
    }

    fn add_comment(&self, out: &mut String, comment: &str, indent: bool) {
        self.code_manipulator.add_comment(out, comment, indent);
    }

    fn add_rows(&self, out: &mut String, code: Vec<&str>, comment: Option<&str>, indent: bool) {
        self.code_manipulator.add_rows(out, code, comment, indent);
    }

    fn add(&self, out: &mut String, code: &str, comment: Option<&str>, indent: bool) {
        self.code_manipulator.add(out, code, comment, indent);
    }

    fn add_empty_line(&self, out: &mut String) {
        self.code_manipulator.add_empty_line(out);
    }

    fn remove_comments_from_line(&self, line: String) -> String {
        self.code_manipulator.remove_comments_from_line(line)
    }

    fn preamble(&self, code: &mut String) {
        self.add(code, "%macro gotoOnSome 1", None, false);
        self.add(
            code,
            "cmp dword eax,[_enum_stdlib_option_Option_None]",
            None,
            true,
        );
        self.add(code, "jne %1", None, true);
        self.add(code, "%endmacro", None, false);
        if self.options.requires.contains(&"libc".to_string()) {
            self.add(code, "%DEFINE LIBC 1", None, false);
            self.add(code, "extern exit", None, true);
        }

        for e in self.options.externals.iter() {
            self.add(code, &format!("extern {e}"), None, true);
        }
    }

    fn create_lambda_add_ref_like_function(
        &self,
        namespace: &EnhASTNameSpace,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
        is_deref: bool,
    ) -> Option<ASTTypedFunctionDef> {
        let mut body = String::new();

        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

        if is_deref {
            self.call_deref_simple(&mut body, "$address", &format!("main {name}"), statics);
        } else {
            self.call_add_ref_simple(&mut body, "$address", &format!("main {name}"), statics);
        }

        let mut initialized = false;
        if !lambda_space.is_empty() {
            for (i, (val_name, kind)) in lambda_space.iter().enumerate() {
                let ast_typed_type = kind.typed_type();
                if let Some(type_name) = get_reference_type_name(ast_typed_type, type_def_provider)
                {
                    if !initialized {
                        self.add(&mut body, "push   ebx", None, true);
                        self.add(&mut body, &format!("mov {ws} ebx, $address"), None, true);
                        self.add(&mut body, &format!("mov {ws} ebx, [ebx]"), None, true);
                        self.add(&mut body, &format!("add {ws} ebx, {}", wl * 3), None, true);
                        initialized = true;
                    }
                    if is_deref {
                        body.push_str(&self.call_deref(
                            &format!("[ebx + {}]", i * self.backend.word_len()),
                            &type_name,
                            &format!("\"{val_name}\" in lambda context"),
                            type_def_provider,
                            statics,
                        ));
                    } else {
                        self.call_add_ref(
                            &mut body,
                            &format!("[ebx + {}]", i * self.backend.word_len()),
                            &type_name,
                            &format!("\"{val_name}\" in lambda context"),
                            type_def_provider,
                            statics,
                        );
                    }
                }
            }
            self.add(&mut body, "pop   ebx", None, true);
        }

        if !initialized {
            return None;
        }

        let parameters = vec![
            ASTTypedParameterDef {
                name: "address".to_owned(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::I32),
                ast_index: EnhASTIndex::none(),
            },
            ASTTypedParameterDef {
                name: "descr".to_owned(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: EnhASTIndex::none(),
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
            index: EnhASTIndex::none(),
        })
    }

    fn reserve_local_vals(&self, stack: &StackVals, out: &mut String) {
        if stack.len_of_local_vals() > 0 {
            let sp = self.backend.stack_pointer();
            self.add(
                out,
                &format!(
                    "sub   {sp}, {}",
                    stack.len_of_local_vals() * self.backend.word_len()
                ),
                Some("reserve stack local vals (let)"),
                true,
            );
        } else {
            self.add(out, "", Some("NO local vals"), true);
        }
    }

    fn generate_statics_code(
        &self,
        statics: &Statics,
        typed_module: &ASTTypedModule,
    ) -> (String, String) {
        let mut data = String::new();
        let mut bss = String::new();

        let mut code = String::new();

        if !statics.statics().is_empty() {
            let mut keys: Vec<&String> = statics.statics().keys().collect();
            // sorted for test purposes
            keys.sort();

            for id in keys.iter() {
                let mut def = String::new();
                def.push_str(&id.pad_to_width(100));

                match statics.statics().get(*id).unwrap() {
                    MemoryValue::StringValue(s) => {
                        def.push_str("db    ");

                        let mut result = "'".to_owned();

                        // TODO it is a naive way to do it: it is slow and it does not support something like \\n that should result in '\' as a char and 'n' as a char
                        for c in s
                            .replace("\\n", "\n")
                            .replace("\\t", "\t")
                            .replace('\'', "',39,'")
                            //.replace("\\\"", "\"")
                            .chars()
                        {
                            if c.is_ascii_control() {
                                result.push_str(&format!("',{},'", c as u32));
                            } else {
                                result.push(c)
                            }
                        }

                        result.push_str("', 0h");

                        def.push_str(&result);

                        self.add(&mut data, &def, None, true);
                    }
                    MemoryValue::I32Value(i) => {
                        def.push_str("dd    ");
                        def.push_str(&format!("{}", i));
                        self.add(&mut data, &def, None, true);
                    }
                    MemoryValue::Mem(len, unit) => {
                        match unit {
                            MemoryUnit::Bytes => def.push_str("resb "),
                            MemoryUnit::Words => def.push_str("resd "),
                        }
                        def.push_str(&format!("{}", len));
                        self.add(&mut bss, &def, None, true);
                    }
                }
            }
        }

        for (_, (key, value_key)) in statics.strings_map().iter() {
            self.add(
                &mut code,
                &format!("$call(addStaticStringToHeap, {value_key})"),
                None,
                true,
            );

            self.add(&mut code, &format!("mov dword [{key}], eax"), None, true);
        }

        for (label_allocation, label_memory) in statics.static_allocation().iter() {
            self.add(
                &mut code,
                &format!(
                    "$call(addStaticAllocation, {label_allocation}, {label_memory}, {})",
                    self.backend.word_len()
                ),
                None,
                true,
            );
        }

        for (label, (descr_label, value)) in statics.heap().iter() {
            self.add(
                &mut code,
                &format!("$call(addHeap, {label}, {descr_label}: str, {value})"),
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
        self.add(&mut declarations, "global  main", None, true);
        declarations.push_str("main:\n");
        (declarations, code)
    }

    fn function_preamble(&self, out: &mut String) {
        let sp = self.backend.stack_pointer();
        let bp = self.backend.stack_base_pointer();

        self.add(out, &format!("push    {}", bp), None, true);
        self.add(out, &format!("mov     {},{}", bp, sp), None, true);
    }

    fn define_debug(&self, out: &mut String) {
        self.add(out, "%define LOG_DEBUG 1", None, false);
    }

    fn restore(&self, stack: &StackVals, out: &mut String) {
        let mut local_vals_words = 0;
        for entry in stack.reserved_slots().borrow().iter().rev() {
            match entry.entry_type {
                StackEntryType::LocalVal => {
                    local_vals_words += 1;
                }
                StackEntryType::TmpRegister(ref register) => {
                    self.add(
                        out,
                        &format!("pop {register}"),
                        Some(&format!("restoring {}", entry.desc)),
                        true,
                    );
                }
                StackEntryType::ReturnRegister => {
                    self.add_empty_line(out);
                    self.add(out, "pop eax", Some("restoring return register"), true);
                }
                StackEntryType::LocalFakeAllocation(size) => {
                    local_vals_words += size;
                }
            }
        }

        if local_vals_words > 0 {
            let sp = self.backend.stack_pointer();
            self.add(
                out,
                &format!("add   {sp}, {}", local_vals_words * self.backend.word_len()),
                Some("restore stack local vals (let)"),
                true,
            );
            stack.remove_all();
        }
    }

    fn function_end(
        &self,
        out: &mut String,
        add_return: bool,
        function_def: Option<&ASTTypedFunctionDef>,
    ) {
        let bp = self.backend.stack_base_pointer();
        self.add(out, &format!("pop     {}", bp), None, true);
        if add_return {
            self.add(out, "ret", None, true);
        }
    }

    fn add_statics(&self, statics: &mut Statics) {
        // +1 because we clean up the next allocated table slot for every new allocation to be sure that is 0..., so we want to have an extra slot
        statics.insert(
            "_heap_table".into(),
            MemoryValue::Mem((self.options.heap_table_slots + 1) * 20, MemoryUnit::Bytes),
        );
        statics.insert(
            "_heap_table_size".into(),
            MemoryValue::I32Value(self.options.heap_table_slots as i32 * 20),
        );
        statics.insert("_heap_table_next".into(), MemoryValue::I32Value(0));
        statics.insert("_heap".into(), MemoryValue::Mem(4, MemoryUnit::Bytes));
        statics.insert(
            "_heap_size".into(),
            MemoryValue::I32Value(self.options.heap_size as i32),
        );
        statics.insert(
            "_heap_buffer".into(),
            MemoryValue::Mem(self.options.heap_size, MemoryUnit::Bytes),
        );

        statics.insert(
            "_lambda_space_stack".into(),
            MemoryValue::Mem(4, MemoryUnit::Bytes),
        );
        statics.insert(
            "_lambda_space_stack_buffer".into(),
            MemoryValue::Mem(self.options.lambda_space_size, MemoryUnit::Bytes),
        );

        let reusable_heap_table_size = 16 * 1024 * 1024;
        statics.insert(
            "_reusable_heap_table".into(),
            MemoryValue::Mem(reusable_heap_table_size, MemoryUnit::Bytes),
        );
        statics.insert(
            "_reusable_heap_table_size".into(),
            MemoryValue::I32Value(reusable_heap_table_size as i32),
        );
        statics.insert("_reusable_heap_table_next".into(), MemoryValue::I32Value(0));

        // command line arguments
        statics.insert("_rasm_args".into(), MemoryValue::Mem(12, MemoryUnit::Words));
        statics.insert("_NEW_LINE".into(), MemoryValue::I32Value(10));
        statics.insert("_ESC".into(), MemoryValue::I32Value(27));
        statics.insert(
            "_for_nprint".into(),
            MemoryValue::Mem(20, MemoryUnit::Bytes),
        );
    }

    fn value_to_string(&self, value_type: &ASTValueType) -> String {
        match value_type {
            ASTValueType::Boolean(b) => if *b { "1" } else { "0" }.into(),
            ASTValueType::I32(n) => n.to_string(),
            ASTValueType::F32(n) => {
                format!("0x{:x}", n.to_bits())
            }
            ASTValueType::Char(c) => {
                let mut b = [0; 4];
                let unescaped = unescape(&format!("\"{}\"", c.replace("\"", "\\\""))).unwrap();
                unescaped.chars().next().unwrap().encode_utf8(&mut b);

                let result = Self::array_to_u32_le(&b);

                format!("{}", result)
            }
            ASTValueType::String(_) => panic!(" String value is not handled here"),
        }
    }

    fn get_text_macro_evaluator(&self) -> TextMacroEvaluator {
        let mut evaluators: LinkedHashMap<String, Box<dyn TextMacroEval>> = LinkedHashMap::new();
        let call_text_macro_evaluator = AsmCallTextMacroEvaluator::new(self.clone());
        evaluators.insert("call".into(), Box::new(call_text_macro_evaluator));

        let c_call_text_macro_evaluator = AsmCCallTextMacroEvaluator::new(self.clone());
        evaluators.insert("ccall".into(), Box::new(c_call_text_macro_evaluator));
        evaluators.insert(
            "addRef".into(),
            Box::new(AddRefMacro::new(
                self.clone(),
                RefType::AddRef,
                self.options.dereference,
            )),
        );
        evaluators.insert(
            "deref".into(),
            Box::new(AddRefMacro::new(
                self.clone(),
                RefType::Deref,
                self.options.dereference,
            )),
        );
        let print_ref_macro = AsmPrintRefMacro::new(self.clone());
        evaluators.insert("printRef".into(), Box::new(print_ref_macro));

        TextMacroEvaluator::new(evaluators, Box::new(CodeManipulatorNasm::new()))
    }

    fn create_function_definition(&self, function_def: &ASTTypedFunctionDef) -> bool {
        !function_def.inline
    }

    fn replace_inline_call_includng_source(&self) -> bool {
        true
    }
}
