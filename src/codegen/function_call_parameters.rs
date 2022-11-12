use crate::codegen::backend::Backend;
use crate::codegen::stack::{StackEntryType, StackVals};
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::TextMacroEvaluator;
use crate::codegen::{CodeGen, LambdaSpace, MemoryValue, TypedValContext, TypedValKind};
use crate::debug_i;
use crate::type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedStatement, ASTTypedType,
};
use linked_hash_map::LinkedHashMap;
use log::debug;

pub struct FunctionCallParameters<'a> {
    parameters: Vec<ASTTypedParameterDef>,
    parameters_added: usize,
    before: String,
    parameters_values: LinkedHashMap<String, String>,
    backend: &'a dyn Backend,
    inline: bool,
    immediate: bool,
    has_inline_lambda_param: bool,
    lambda_slots_to_deallocate: usize,
    stack_vals: &'a StackVals,
    after: Vec<String>,
    dereference: bool,
    id: usize,
}

impl<'a> FunctionCallParameters<'a> {
    ///
    ///
    /// # Arguments
    ///
    /// * `backend`:
    /// * `parameters`:
    /// * `inline`:
    /// * `immediate`: if true the result is not pushed on the stack, but moved to eax
    ///
    /// returns: FunctionCallParameters
    ///
    /// # Examples
    ///
    /// ```
    ///
    /// ```
    pub fn new(
        backend: &'a dyn Backend,
        parameters: Vec<ASTTypedParameterDef>,
        inline: bool,
        immediate: bool,
        stack: &'a StackVals,
        dereference: bool,
        id: usize,
    ) -> Self {
        Self {
            parameters_added: 0,
            before: String::new(),
            parameters_values: LinkedHashMap::new(),
            backend,
            inline,
            parameters,
            immediate,
            has_inline_lambda_param: false,
            lambda_slots_to_deallocate: 0,
            stack_vals: stack,
            after: Vec::new(),
            dereference,
            id,
        }
    }

    pub fn add_string_literal(&mut self, param_name: &str, label: String, comment: Option<&str>) {
        if self.inline {
            self.parameters_values
                .insert(param_name.into(), format!("[{label}]"));
        } else {
            // TODO can be optimized?
            CodeGen::add(&mut self.before, "push eax", None, true);
            CodeGen::add(
                &mut self.before,
                &format!("mov {} eax, [{label}]", self.backend.word_size()),
                comment,
                true,
            );
            let to_remove_from_stack = self.to_remove_from_stack();
            CodeGen::add(
                &mut self.before,
                &format!(
                    "mov {} [{} + {}], eax",
                    self.backend.pointer_size(),
                    self.backend.stack_pointer(),
                    (to_remove_from_stack as i32 + 1) * self.backend.word_len() as i32
                ),
                comment,
                true,
            );
            CodeGen::add(&mut self.before, "pop eax", None, true);
            self.parameter_added_to_stack();
        }
    }

    pub fn add_number(&mut self, param_name: &str, n: &i32, comment: Option<&str>) {
        if self.inline {
            self.parameters_values
                .insert(param_name.into(), format!("{}", n));
        } else {
            let to_remove_from_stack = self.to_remove_from_stack();
            CodeGen::add(
                &mut self.before,
                &format!(
                    "mov {} [{} + {}], {}",
                    self.backend.word_size(),
                    self.backend.stack_pointer(),
                    to_remove_from_stack * self.backend.word_len() as usize,
                    n
                ),
                comment,
                true,
            );
            self.parameter_added_to_stack();
        }
    }

    pub fn add_function_call(
        &mut self,
        code_gen: &mut CodeGen,
        comment: Option<&str>,
        param_type: ASTTypedType,
        descr: &str,
        statics: &mut Statics,
    ) {
        let wl = self.backend.word_len();
        let ws = self.backend.word_size();
        let sp = self.backend.stack_pointer();

        let descr = format!("add_function_call {descr}");

        CodeGen::add(
            &mut self.before,
            &format!(
                "mov {ws} [{sp} + {}], eax",
                self.parameters_added * wl as usize
            ),
            comment,
            true,
        );
        if let Some(name) = CodeGen::get_reference_type_name(&param_type) {
            self.add_code_for_reference_type(code_gen, &name, "eax", &descr, statics);
        }
        self.parameter_added_to_stack();
    }

    fn add_code_for_reference_type(
        &mut self,
        code_gen: &mut CodeGen,
        name: &str,
        source: &str,
        descr: &str,
        statics: &mut Statics,
    ) {
        // TODO I really don't know if it is correct not to add ref and deref for immediate
        if self.dereference {
            self.backend.call_add_ref(
                &mut self.before,
                source,
                name,
                descr,
                &code_gen.module.clone(),
                statics,
            );
            let pos = self.push_to_scope_stack(source);

            self.after.insert(
                0,
                Self::pop_from_scope_stack_and_deref(
                    code_gen,
                    self.backend,
                    name,
                    descr,
                    pos,
                    statics,
                ),
            );
        }
    }

    pub fn add_lambda(
        &mut self,
        def: &mut ASTTypedFunctionDef,
        parent_lambda_space: Option<&LambdaSpace>,
        context: &TypedValContext,
        comment: Option<&str>,
        statics: &mut Statics,
    ) -> LambdaSpace {
        let mut lambda_space = LambdaSpace::new(context.clone());

        let stack_base_pointer = self.backend.stack_base_pointer();
        let stack_pointer = self.backend.stack_pointer();
        let word_len = self.backend.word_len() as usize;
        let word_size = self.backend.word_size();
        let pointer_size = self.backend.pointer_size();

        if !self.body_reads_from_context(&def.body, context) {
            let key = format!("_ls_{}", def.name);

            statics.insert(key.clone(), MemoryValue::RefToLabel(def.name.clone()));

            debug_i!("lambda does not read from context: {def}");

            let to_remove_from_stack = self.to_remove_from_stack();
            CodeGen::add(
                &mut self.before,
                &format!(
                    "mov {} [{} + {}], {key}",
                    word_size,
                    stack_pointer,
                    (to_remove_from_stack) * word_len as usize
                ),
                comment,
                true,
            );
        } else {
            CodeGen::add(
                &mut self.before,
                &format!("push  {} ecx", self.backend.word_size()),
                comment,
                true,
            );

            let num_of_values_in_context = context.iter().count();

            self.lambda_slots_to_deallocate += num_of_values_in_context + 1;

            Self::allocate_lambda_space(
                self.backend,
                &mut self.before,
                "ecx",
                num_of_values_in_context + 1,
            );

            if !context.is_empty() {
                CodeGen::add(
                    &mut self.before,
                    &format!("push  {} ebx", self.backend.word_size()),
                    comment,
                    true,
                );
            }

            let mut i = 1;

            context.iter().for_each(|(name, kind)| {
                // we do not create val that are overridden by parent memcopy
                let already_in_parent = if let Some(parent_lambda) = parent_lambda_space {
                    parent_lambda.context.get(name).is_some()
                } else {
                    false
                };
                if !already_in_parent {
                    let relative_address = match kind {
                        TypedValKind::ParameterRef(index, _) => (index + 2) as i32,
                        TypedValKind::LetRef(_, _) => {
                            -(self
                                .stack_vals
                                .find_relative_to_bp(StackEntryType::LetVal, name)
                                .unwrap() as i32)
                        }
                    };
                    self.indirect_mov(
                        &format!(
                            "{}+{}",
                            stack_base_pointer,
                            relative_address * word_len as i32
                        ),
                        &format!("ecx + {}", i * word_len),
                        "ebx",
                        Some(&format!("context parameter {}", name)),
                    );
                }

                lambda_space.add_context_parameter(name.clone(), i);
                i += 1;
            });

            if !context.is_empty() {
                CodeGen::add(&mut self.before, "pop  ebx", comment, true);
            }

            // I copy the lambda space of the parent
            if let Some(parent_lambda) = parent_lambda_space {
                self.mem_copy_words(
                    &format!("[{}+8]", stack_base_pointer),
                    "ecx",
                    parent_lambda.parameters_indexes.len() + 1,
                    None,
                );
            }

            CodeGen::add(
                &mut self.before,
                &format!("mov {} [ecx], {}", pointer_size, def.name),
                None,
                true,
            );
            // + 1 due to push ecx
            let to_remove_from_stack = self.to_remove_from_stack();
            CodeGen::add(
                &mut self.before,
                &format!(
                    "mov {} [{} + {}], ecx",
                    word_size,
                    stack_pointer,
                    (to_remove_from_stack + 1) * word_len as usize
                ),
                comment,
                true,
            );

            CodeGen::add(&mut self.before, "pop  ecx", comment, true);
        }

        self.parameter_added_to_stack();

        lambda_space
    }

    pub fn add_parameter_ref(
        &mut self,
        original_param_name: String,
        val_name: &str,
        ast_typed_type: &ASTTypedType,
        index_in_context: usize,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
    ) {
        self.debug_and_before(&format!("adding val {val_name}"), indent);

        if let Some(lambda_space_index) = lambda_space.and_then(|it| it.get_index(val_name)) {
            self.add_val_from_lambda_space(&original_param_name, lambda_space_index, indent);
        } else {
            self.add_val_from_parameter(
                original_param_name,
                ast_typed_type,
                index_in_context as i32 + 2,
                indent,
            );
        }
    }

    pub fn add_let_val_ref(
        &mut self,
        original_param_name: String,
        val_name: &str,
        ast_typed_type: &ASTTypedType,
        index_in_context: usize,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
    ) {
        self.debug_and_before(&format!("adding val {val_name}"), indent);

        if let Some(lambda_space_index) = lambda_space.and_then(|it| it.get_index(val_name)) {
            self.add_val_from_lambda_space(&original_param_name, lambda_space_index, indent);
        } else {
            self.add_val_from_parameter(
                original_param_name.clone(),
                ast_typed_type,
                -(index_in_context as i32),
                indent,
            );
        }
    }

    pub fn to_remove_from_stack_name(&self) -> String {
        format!("$_to_remove_rom_stack{}", self.id)
    }

    fn body_reads_from_context(
        &self,
        body: &ASTTypedFunctionBody,
        context: &TypedValContext,
    ) -> bool {
        if let ASTTypedFunctionBody::RASMBody(statements) = body {
            statements
                .iter()
                .any(|it| self.statement_reads_from_context(it, context))
        } else {
            true
        }
    }

    fn statement_reads_from_context(
        &self,
        statement: &ASTTypedStatement,
        context: &TypedValContext,
    ) -> bool {
        match statement {
            ASTTypedStatement::Expression(expr) => {
                self.expression_reads_from_context(expr, context)
            }
            ASTTypedStatement::LetStatement(_, expr) => {
                self.expression_reads_from_context(expr, context)
            }
        }
    }

    fn expression_reads_from_context(
        &self,
        expr: &ASTTypedExpression,
        context: &TypedValContext,
    ) -> bool {
        match expr {
            ASTTypedExpression::StringLiteral(_) => false,
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                if context.get(&call.function_name).is_some() {
                    true
                } else {
                    call.parameters
                        .iter()
                        .any(|it| self.expression_reads_from_context(it, context))
                }
            }
            ASTTypedExpression::ValueRef(name, _) => context.get(name).is_some(),
            ASTTypedExpression::Value(_, _) => false,
            ASTTypedExpression::Lambda(lambda_def) => lambda_def
                .body
                .iter()
                .any(|it| self.statement_reads_from_context(it, context)),
        }
    }

    fn debug_and_before(&mut self, descr: &str, indent: usize) {
        debug!("{} {descr}", " ".repeat(indent * 4));
        CodeGen::add(&mut self.before, "", Some(descr), true);
    }

    fn add_val_from_parameter(
        &mut self,
        original_param_name: String,
        ast_typed_type: &ASTTypedType,
        index_relative_to_bp: i32,
        indent: usize,
    ) {
        self.debug_and_before(
            &format!("param {original_param_name}, index_relative_to_bp {index_relative_to_bp}"),
            indent,
        );

        let word_len = self.backend.word_len() as usize;

        let source = &format!(
            "{}+{}",
            self.backend.stack_base_pointer(),
            index_relative_to_bp * word_len as i32
        );

        let src: String = format!("[{source}]");

        if self.inline {
            self.parameters_values.insert(original_param_name, src);
        } else {
            let type_size = self
                .backend
                .type_size(ast_typed_type)
                .unwrap_or_else(|| panic!("Unsupported type size: {:?}", ast_typed_type));

            if self.immediate {
                CodeGen::add(
                    &mut self.before,
                    &format!("mov {} eax, {src}", type_size),
                    None,
                    true,
                );
            } else {
                CodeGen::add(
                    &mut self.before,
                    &format!("push  {} ebx", self.backend.word_size()),
                    None,
                    true,
                );
                self.indirect_mov(
                    source,
                    &format!(
                        "{} + {}",
                        self.backend.stack_pointer(),
                        (self.to_remove_from_stack() + 1) * self.backend.word_len() as usize
                    ),
                    "ebx",
                    None,
                );
                CodeGen::add(&mut self.before, "pop  ebx", None, true);
            }
            self.parameter_added_to_stack();
        }
    }

    fn add_val_from_lambda_space(
        &mut self,
        original_param_name: &str,
        lambda_space_index: usize,
        indent: usize,
    ) {
        self.debug_and_before(&format!("add_lambda_param_from_lambda_space, original_param_name {original_param_name}, lambda_space_index {lambda_space_index}"), indent);

        let word_len = self.backend.word_len() as usize;

        let src: String;

        if self.inline {
            self.has_inline_lambda_param = true;
            src = format!("[edx + {}]", lambda_space_index * word_len);
            self.parameters_values
                .insert(original_param_name.into(), src);
        } else {
            if self.immediate {
                CodeGen::add(
                    &mut self.before,
                    &format!(
                        "mov   {} eax,[edx + {}]",
                        self.backend.pointer_size(),
                        lambda_space_index * word_len
                    ),
                    None,
                    true,
                );
            } else {
                CodeGen::add(
                    &mut self.before,
                    &format!("push  {} ebx", self.backend.word_size()),
                    None,
                    true,
                );
                self.indirect_mov(
                    &format!("edx + {}", lambda_space_index * word_len),
                    &format!(
                        "{} + {}",
                        self.backend.stack_pointer(),
                        (self.to_remove_from_stack() + 1) * self.backend.word_len() as usize
                    ),
                    "ebx",
                    None,
                );
                CodeGen::add(&mut self.before, "pop  ebx", None, true);
            }

            self.parameter_added_to_stack();
        }
    }

    pub fn resolve_asm_parameters(
        &self,
        statics: &mut Statics,
        body: &str,
        to_remove_from_stack: String,
        ident: usize,
        function_def: Option<&ASTTypedFunctionDef>,
        module: &ASTTypedModule,
    ) -> String {
        let mut result = TextMacroEvaluator::new().translate(
            self.backend,
            statics,
            function_def,
            body,
            Some(module),
        );

        let mut i = 0;
        let word_len = self.backend.word_len() as i32;
        for par in self.parameters.iter() {
            if let Some(par_value) = self.parameters_values.get(&par.name) {
                debug!(
                    "{}found parameter {}, value: {}",
                    " ".repeat(ident * 4),
                    par.name,
                    par_value
                );
                result.push_str(&format!(
                    ";found parameter {}, value: {}\n",
                    par.name, par_value
                ));
                result = result.replace(&format!("${}", par.name), par_value);
                continue;
            }

            debug!(
                "{}cannot find parameter {} in parameters_values {:?} we take if from the stack",
                " ".repeat(ident * 4),
                par.name,
                self.parameters_values
            );
            result.push_str(&format!(
                ";cannot find parameter {} in parameters_values {:?} we take it from stack\n",
                par.name, self.parameters_values
            ));

            let relative_address = if self.inline {
                debug!(
                    "{} i {}, self.to_remove_from_stack {}, to_remove_from_stack {}",
                    " ".repeat(ident * 4),
                    i,
                    self.to_remove_from_stack(),
                    to_remove_from_stack
                );
                result.push_str(&format!(
                    ";i {}, self.to_remove_from_stack {}, to_remove_from_stack {}\n",
                    i,
                    self.to_remove_from_stack(),
                    to_remove_from_stack
                ));
                format!(
                    "{}-({})-{}",
                    (i as i32 - self.to_remove_from_stack() as i32) * word_len,
                    to_remove_from_stack,
                    crate::codegen::STACK_VAL_SIZE_NAME
                )
            } else {
                format!("{}", (i + 2) * self.backend.word_len() as i32)
            };

            let address = format!(
                "[{}+{}]",
                self.backend.stack_base_pointer(),
                relative_address
            );

            result = result.replace(&format!("${}", par.name), &address);
            i += 1;
        }

        result
    }

    fn parameter_added_to_stack(&mut self) {
        self.parameters_added += 1;
    }

    pub fn to_remove_from_stack(&self) -> usize {
        if self.immediate {
            0
        } else {
            self.parameters_added
        }
    }

    pub fn before(&self) -> String {
        let mut result = String::new();

        let word_len = self.backend.word_len() as usize;

        if self.to_remove_from_stack() > 0 {
            CodeGen::add(
                &mut result,
                &format!(
                    "sub {}, {}",
                    self.backend.stack_pointer(),
                    word_len * self.to_remove_from_stack()
                ),
                Some("Prepare stack for parameters"),
                true,
            );
        }

        result.push_str(&self.before);

        result
    }

    pub fn after(&self) -> Vec<String> {
        let mut s = String::new();
        if self.lambda_slots_to_deallocate > 0 {
            CodeGen::add(
                &mut s,
                &format!("push     {} ebx", self.backend.word_size()),
                None,
                true,
            );
            Self::deallocate_lambda_space(
                self.backend,
                &mut s,
                "ebx",
                self.lambda_slots_to_deallocate,
            );
            CodeGen::add(&mut s, "pop    ebx", None, true);
        }
        let mut result = vec![s];
        let mut after = self.after.clone();
        result.append(&mut after);
        result
    }

    fn push_to_scope_stack(&mut self, what: &str) -> usize {
        let pos = self
            .stack_vals
            .reserve(StackEntryType::RefToDereference, what)
            * self.backend.word_len();
        CodeGen::add(&mut self.before, "; scope push", None, true);
        CodeGen::add(
            &mut self.before,
            &format!(
                "mov     {} [{} - {}], {what}",
                self.backend.word_size(),
                self.backend.stack_base_pointer(),
                pos
            ),
            None,
            true,
        );
        pos
    }

    fn pop_from_scope_stack_and_deref(
        code_gen: &mut CodeGen,
        backend: &dyn Backend,
        type_name: &str,
        descr: &str,
        pos: usize,
        statics: &mut Statics,
    ) -> String {
        let mut result = String::new();
        CodeGen::add(&mut result, "; scope pop", None, true);
        result.push_str(&backend.call_deref(
            &format!("[{} - {}]", backend.stack_base_pointer(), pos),
            type_name,
            descr,
            &code_gen.module.clone(),
            statics,
        ));
        result
    }

    fn allocate_lambda_space(
        backend: &dyn Backend,
        out: &mut String,
        register_to_store_result: &str,
        slots: usize,
    ) {
        CodeGen::add(out, "; lambda space allocation", None, true);
        CodeGen::add(
            out,
            &format!("mov     {register_to_store_result},[_lambda_space_stack]"),
            None,
            true,
        );
        CodeGen::add(
            out,
            &format!(
                "add     {register_to_store_result},{}",
                slots * backend.word_len() as usize
            ),
            None,
            true,
        );
        CodeGen::add(
            out,
            &format!(
                "mov     {} [_lambda_space_stack],{register_to_store_result}",
                backend.word_size()
            ),
            None,
            true,
        );
        CodeGen::add(
            out,
            &format!(
                "sub     {register_to_store_result},{}",
                slots * backend.word_len() as usize
            ),
            None,
            true,
        );
    }

    fn deallocate_lambda_space(
        backend: &dyn Backend,
        out: &mut String,
        tmp_register: &str,
        slots: usize,
    ) {
        CodeGen::add(out, "; lambda space deallocation", None, true);
        CodeGen::add(
            out,
            &format!("mov     {tmp_register},[_lambda_space_stack]"),
            None,
            true,
        );
        CodeGen::add(
            out,
            &format!(
                "sub     {tmp_register},{}",
                slots * backend.word_len() as usize
            ),
            None,
            true,
        );
        CodeGen::add(
            out,
            &format!("mov     dword [_lambda_space_stack],{tmp_register}"),
            None,
            true,
        );
    }

    pub fn push(&mut self, s: &str) {
        self.before.push_str(s);
    }

    pub fn add_on_top_of_after(&mut self, s: &str) {
        self.after.insert(0, s.into());
    }

    fn mem_copy_words(&mut self, source: &str, dest: &str, slots: usize, comment: Option<&str>) {
        CodeGen::add(
            &mut self.before,
            &format!(
                "push {} {}",
                self.backend.pointer_size(),
                slots * self.backend.word_len()
            ),
            comment,
            true,
        );
        CodeGen::add(
            &mut self.before,
            &format!("push {} {}", self.backend.pointer_size(), dest),
            comment,
            true,
        );
        CodeGen::add(
            &mut self.before,
            &format!("push {} {}", self.backend.pointer_size(), source),
            comment,
            true,
        );
        CodeGen::add(&mut self.before, "call memcopy", comment, true);
        self.restore_stack(3, comment);
    }

    fn restore_stack(&mut self, slots: usize, comment: Option<&str>) {
        CodeGen::add(
            &mut self.before,
            &format!(
                "add {},{}",
                self.backend.stack_pointer(),
                slots * self.backend.word_len() as usize
            ),
            comment,
            true,
        );
    }

    fn indirect_mov(
        &mut self,
        source: &str,
        dest: &str,
        temporary_register: &str,
        comment: Option<&str>,
    ) {
        CodeGen::add(
            &mut self.before,
            &format!(
                "mov  {} {}, [{}]",
                self.backend.word_size(),
                temporary_register,
                source
            ),
            comment,
            true,
        );
        CodeGen::add(
            &mut self.before,
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
}
