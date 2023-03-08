use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::codegen::backend::Backend;
use crate::codegen::stack::{StackEntryType, StackVals};
use crate::codegen::statics::Statics;
use crate::codegen::{
    CodeGen, LambdaSpace, MemoryUnit, MemoryValue, TypedValContext, TypedValKind,
};
use crate::parser::ast::ASTIndex;
use crate::parser::ValueType;
use crate::type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedStatement, ASTTypedType,
};

pub struct FunctionCallParameters<'a> {
    parameters: Vec<ASTTypedParameterDef>,
    parameters_added: usize,
    before: String,
    parameters_values: LinkedHashMap<String, String>,
    backend: &'a dyn Backend,
    inline: bool,
    immediate: bool,
    has_inline_lambda_param: bool,
    //lambda_slots_to_deallocate: usize,
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
            //lambda_slots_to_deallocate: 0,
            stack_vals: stack,
            after: Vec::new(),
            dereference,
            id,
        }
    }

    pub fn add_label(&mut self, param_name: &str, label: String, comment: Option<&str>) {
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

    pub fn add_char(&mut self, param_name: &str, c: char, comment: Option<&str>) {
        let mut b = [0; 4];
        c.encode_utf8(&mut b);

        let result = Self::array_to_u32_le(&b);

        self.add_number(param_name, result.to_string(), comment);
    }

    /// little endian
    fn array_to_u32_le(array: &[u8; 4]) -> u32 {
        (array[0] as u32)
            + ((array[1] as u32) << 8)
            + ((array[2] as u32) << 16)
            + ((array[3] as u32) << 24)
    }

    pub fn add_i32(&mut self, param_name: &str, n: i32, comment: Option<&str>) {
        self.add_number(param_name, n.to_string(), comment);
    }

    fn add_number(&mut self, param_name: &str, n: String, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), n);
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
        module: &ASTTypedModule,
        comment: &str,
        param_type: ASTTypedType,
        statics: &mut Statics,
    ) {
        let wl = self.backend.word_len();
        let ws = self.backend.word_size();
        let sp = self.backend.stack_pointer();

        CodeGen::add(
            &mut self.before,
            &format!(
                "mov {ws} [{sp} + {}], eax",
                self.parameters_added * wl as usize
            ),
            Some(comment),
            true,
        );
        if self.immediate {
            panic!();
        }
        if let Some(name) = CodeGen::get_reference_type_name(&param_type, module) {
            self.add_code_for_reference_type(module, &name, "eax", comment, statics);
        }
        self.parameter_added_to_stack();
    }

    fn add_code_for_reference_type(
        &mut self,
        module: &ASTTypedModule,
        name: &str,
        source: &str,
        descr: &str,
        statics: &mut Statics,
    ) {
        // TODO I really don't know if it is correct not to add ref and deref for immediate
        if self.dereference {
            self.backend
                .call_add_ref(&mut self.before, source, name, descr, module, statics);
            let pos = self.push_to_scope_stack(source);

            self.after.insert(
                0,
                Self::pop_from_scope_stack_and_deref(
                    module,
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
        module: &ASTTypedModule,
    ) -> LambdaSpace {
        let sbp = self.backend.stack_base_pointer();
        let sp = self.backend.stack_pointer();
        let wl = self.backend.word_len() as usize;
        let ws = self.backend.word_size();
        let ptrs = self.backend.pointer_size();

        let mut references = self.body_references_to_context(&def.body, context);
        references.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));
        references.dedup_by(|(name1, _), (name2, _)| name1 == name2);

        let mut context = TypedValContext::new(None);
        for (key, kind) in references {
            context.insert(key, kind);
        }

        let mut lambda_space = LambdaSpace::new(context.clone());

        CodeGen::add(
            &mut self.before,
            &format!("push  {} ecx", self.backend.word_size()),
            comment,
            true,
        );

        /*
           If the context is empty, we optimize, using a static allocation.
           We cannot use it when there are values in the context, because the same lambda can be used recursively,
           so the values of the previous recursion are overridden.
        */
        if context.is_empty() {
            let label_allocation =
                statics.insert_static_allocation(MemoryValue::Mem(1, MemoryUnit::Words));
            CodeGen::add(
                &mut self.before,
                &format!(
                    "mov  {} ecx, [{label_allocation}]",
                    self.backend.word_size()
                ),
                comment,
                true,
            );
        } else {
            let num_of_values_in_context = context.iter().count();

            Self::allocate_lambda_space(
                self.backend,
                &mut self.before,
                "ecx",
                num_of_values_in_context + 1,
                statics,
            );

            self.add_code_for_reference_type(module, "_fn", "ecx", "lambda space", statics);

            CodeGen::add(&mut self.before, "mov    dword ecx, [ecx]", None, true);

            CodeGen::add(
                &mut self.before,
                &format!("push  {} ebx", self.backend.word_size()),
                comment,
                true,
            );
            CodeGen::add(
                &mut self.before,
                &format!("push  {} eax", self.backend.word_size()),
                comment,
                true,
            );
            CodeGen::add(
                &mut self.before,
                &format!("mov   {} eax, [{sbp}+8]", ws),
                None,
                true,
            );

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
                        &format!("{}+{}", sbp, relative_address * wl as i32),
                        &format!("ecx + {}", i * wl),
                        "ebx",
                        Some(&format!("context parameter {}", name)),
                    );
                } else if let Some(pls) = parent_lambda_space {
                    if let Some(parent_index) = pls.get_index(name) {
                        self.indirect_mov(
                            &format!("eax + {}", (parent_index) * wl),
                            &format!("ecx + {}", (i) * wl),
                            "ebx",
                            Some(&format!("context parameter from parent {}", name)),
                        );
                    } else {
                        panic!()
                    }
                } else {
                    panic!()
                }

                lambda_space.add_context_parameter(name.clone(), i);
                i += 1;
            });

            CodeGen::add(&mut self.before, "pop   eax", None, true);
            CodeGen::add(&mut self.before, "pop   ebx", comment, true);
        }

        CodeGen::add(
            &mut self.before,
            &format!("mov {} [ecx], {}", ptrs, def.name),
            None,
            true,
        );

        // + 1 due to push ecx
        let to_remove_from_stack = self.to_remove_from_stack();
        CodeGen::add(
            &mut self.before,
            &format!(
                "mov {} [{} + {}], ecx",
                ws,
                sp,
                (to_remove_from_stack + 1) * wl as usize
            ),
            comment,
            true,
        );

        CodeGen::add(&mut self.before, "pop  ecx", comment, true);

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
        index_in_context: Option<usize>,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
        ast_index: &ASTIndex,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) {
        self.debug_and_before(
            &format!("adding let val {val_name} original_param_name {original_param_name}"),
            indent,
        );

        if let Some(lambda_space_index) = lambda_space.and_then(|it| it.get_index(val_name)) {
            self.add_val_from_lambda_space(&original_param_name, lambda_space_index, indent);
            /*
            if let Some(name) = CodeGen::get_reference_type_name(ast_typed_type, module) {
                let src = format!("[edx + {}]", lambda_space_index * self.backend.word_len());

                self.add_code_for_reference_type(
                    module,
                    &name,
                    &src,
                    &format!("from lambda space {val_name} : {ast_index}"),
                    statics,
                );
            }

             */
        } else {
            self.add_val_from_parameter(
                original_param_name.clone(),
                ast_typed_type,
                -(index_in_context
                    .unwrap_or_else(|| panic!("cannot find index for {val_name} : {ast_index}"))
                    as i32),
                indent,
            );
            /*
            if let Some(name) = CodeGen::get_reference_type_name(ast_typed_type, module) {
                let source = &format!(
                    "[{}+{}]",
                    self.backend.stack_base_pointer(),
                    -(index_in_context.unwrap() as i32 * self.backend.word_len() as i32)
                );

                self.add_code_for_reference_type(
                    module,
                    &name,
                    &source,
                    &format!("HENRY par {original_param_name} : {ast_index}"),
                    statics,
                );
            }

             */
        }
    }

    pub fn to_remove_from_stack_name(&self) -> String {
        format!("$_to_remove_rom_stack{}", self.id)
    }

    pub fn add_value_type(&mut self, name: &str, value_type: &ValueType) {
        let v = self.backend.value_to_string(value_type);
        self.add_number(name, v, None);
    }

    fn body_references_to_context(
        &self,
        body: &ASTTypedFunctionBody,
        context: &TypedValContext,
    ) -> Vec<(String, TypedValKind)> {
        if let ASTTypedFunctionBody::RASMBody(statements) = body {
            statements
                .iter()
                .flat_map(|it| {
                    self.statement_references_to_context(it, context)
                        .into_iter()
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    fn statement_references_to_context(
        &self,
        statement: &ASTTypedStatement,
        context: &TypedValContext,
    ) -> Vec<(String, TypedValKind)> {
        match statement {
            ASTTypedStatement::Expression(expr) => {
                self.expression_references_to_context(expr, context)
            }
            ASTTypedStatement::LetStatement(_, expr, _is_const, _index) => {
                self.expression_references_to_context(expr, context)
            }
        }
    }

    fn expression_references_to_context(
        &self,
        expr: &ASTTypedExpression,
        context: &TypedValContext,
    ) -> Vec<(String, TypedValKind)> {
        match expr {
            ASTTypedExpression::StringLiteral(_) => Vec::new(),
            //ASTTypedExpression::CharLiteral(_) => false,
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                let mut result = Vec::new();
                if let Some(v) = context.get(&call.function_name) {
                    result.append(&mut vec![(call.function_name.clone(), v.clone())]);
                }
                result.append(
                    &mut call
                        .parameters
                        .iter()
                        .flat_map(|it| {
                            self.expression_references_to_context(it, context)
                                .into_iter()
                        })
                        .collect(),
                );

                result
            }
            ASTTypedExpression::ValueRef(name, _) => {
                if let Some(v) = context.get(name) {
                    vec![(name.clone(), v.clone())]
                } else {
                    Vec::new()
                }
            }
            ASTTypedExpression::Value(_, _) => Vec::new(),
            ASTTypedExpression::Lambda(lambda_def) => lambda_def
                .body
                .iter()
                .flat_map(|it| {
                    self.statement_references_to_context(it, context)
                        .into_iter()
                })
                .collect(),
            ASTTypedExpression::Any(_) => Vec::new(),
        }
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
            ASTTypedStatement::LetStatement(_, expr, _is_const, _index) => {
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
            //ASTTypedExpression::CharLiteral(_) => false,
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
            ASTTypedExpression::Any(_) => false,
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
            self.parameters_values
                .insert(original_param_name, src.clone());
            // TODO add ref?
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

                /*
                TODO it does not work for breakout example but probably it's not needed
                if let Some(name) = CodeGen::get_reference_type_name(ast_typed_type) {
                    self.add_code_for_reference_type(
                        module,
                        &name,
                        &src,
                        &format!("HENRY par {original_param_name} : {ast_index}"),
                        statics,
                    );
                }

                 */

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

        let src = format!("[edx + {}]", lambda_space_index * word_len);

        if self.inline {
            self.has_inline_lambda_param = true;
            self.parameters_values
                .insert(original_param_name.into(), src.clone());
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

                /*
                TODO it seems to work, but probably it's not needed
                if let Some(name) = CodeGen::get_reference_type_name(ast_typed_type) {
                    self.add_code_for_reference_type(
                        module,
                        &name,
                        &src,
                        &format!("from lambda space {original_param_name} : {ast_index}"),
                        statics,
                    );
                }
                 */
            }

            self.parameter_added_to_stack();
        }
    }

    pub fn resolve_asm_parameters(
        &self,
        body: &str,
        to_remove_from_stack: String,
        ident: usize,
    ) -> String {
        let mut result = body.to_string();

        let word_len = self.backend.word_len() as i32;

        let mut i = 0;

        let mut substitutions = Vec::new();

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
                substitutions.push((par.name.clone(), par_value.clone()));
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
                format!("{}", (i as i32 + 2) * self.backend.word_len() as i32)
            };

            let address = format!(
                "[{}+{}]",
                self.backend.stack_base_pointer(),
                relative_address
            );

            substitutions.push((par.name.clone(), address));
            i += 1;
        }

        substitutions.sort_by(|(a, _), (b, _)| a.cmp(b).reverse());

        for (par_name, value) in substitutions {
            result = result.replace(&format!("${}", par_name), &value);
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
        let s = String::new();

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
        if what.contains('[') {
            CodeGen::add(&mut self.before, "push    ebx", None, true);
            CodeGen::add(
                &mut self.before,
                &format!("mov     {} ebx, {what}", self.backend.word_size(),),
                None,
                true,
            );
            CodeGen::add(
                &mut self.before,
                &format!(
                    "mov     {} [{} - {}], ebx",
                    self.backend.word_size(),
                    self.backend.stack_base_pointer(),
                    pos
                ),
                None,
                true,
            );
            CodeGen::add(&mut self.before, "pop    ebx", None, true);
        } else {
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
        }
        pos
    }

    fn pop_from_scope_stack_and_deref(
        module: &ASTTypedModule,
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
            module,
            statics,
        ));
        result
    }

    fn allocate_lambda_space(
        backend: &dyn Backend,
        out: &mut String,
        register_to_store_result: &str,
        slots: usize,
        statics: &mut Statics,
    ) {
        let label = statics.add_str("lambda space");
        CodeGen::add(out, "; lambda space allocation", None, true);

        CodeGen::add(out, &format!("push    dword [{label}]",), None, true);

        CodeGen::add(
            out,
            &format!("push    dword {}", slots * backend.word_len()),
            None,
            true,
        );

        CodeGen::add(out, "call malloc_0", None, true);

        CodeGen::add(
            out,
            &format!("add    esp, {}", 2 * backend.word_len()),
            None,
            true,
        );
        CodeGen::add(
            out,
            &format!("mov    dword {register_to_store_result},eax",),
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
        // TODO $call
        CodeGen::add(&mut self.before, "call memcopy_0", comment, true);
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
