use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::codegen::backend::Backend;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::stack::StackVals;
use crate::codegen::statics::{MemoryUnit, MemoryValue, Statics};
use crate::codegen::val_context::TypedValContext;
use crate::codegen::{CodeGen, TypedValKind};
use crate::parser::ast::{ASTIndex, ValueType};
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
                    to_remove_from_stack * self.backend.word_len(),
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
            &format!("mov {ws} [{sp} + {}], eax", self.parameters_added * wl),
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
        def: &ASTTypedFunctionDef,
        parent_lambda_space: Option<&LambdaSpace>,
        context: &TypedValContext,
        comment: Option<&str>,
        statics: &mut Statics,
        module: &mut ASTTypedModule,
        stack_vals: &StackVals,
        optimize: bool,
    ) -> LambdaSpace {
        let sbp = self.backend.stack_base_pointer();
        let sp = self.backend.stack_pointer();
        let wl = self.backend.word_len();
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

        let mut add_ref_function = "addRef_0".to_owned();
        let mut deref_function = "deref_0".to_owned();

        let lambda_space_address = stack_vals.reserve_tmp_register(
            &mut self.before,
            self.backend,
            "this_lambda_space_address",
        );

        /*
           If the context is empty, we optimize, using a static allocation.
           We cannot do that when there are values in the context, because the same lambda can be used recursively,
           so the values of the previous recursion are overridden.
        */
        if context.is_empty() {
            let (label_allocation, label_memory) =
                statics.insert_static_allocation(MemoryValue::Mem(3, MemoryUnit::Words));

            // we save the allocation table address of the lambda space in the stack
            CodeGen::add(
                &mut self.before,
                &format!("push  {} {label_allocation}", self.backend.word_size()),
                comment,
                true,
            );

            // we put in lambda_space_address register the address of the lambda space
            CodeGen::add(
                &mut self.before,
                &format!(
                    "mov  {} {lambda_space_address}, {label_memory}",
                    self.backend.word_size()
                ),
                comment,
                true,
            );
        } else {
            let num_of_values_in_context = context.iter().count();

            if optimize {
                let tmp_register =
                    stack_vals.reserve_tmp_register(&mut self.before, self.backend, "tmp_register");

                let address_relative_to_bp_for_lambda_allocation =
                    stack_vals.reserve_local_space("optimized lambda space", 5);
                let address_relative_to_bp_for_lambda_space = stack_vals
                    .reserve_local_space("optimized lambda space", num_of_values_in_context + 3);

                CodeGen::add(
                    &mut self.before,
                    &format!("mov dword {lambda_space_address},{sbp}"),
                    None,
                    true,
                );
                CodeGen::add(
                    &mut self.before,
                    &format!(
                        "sub dword {lambda_space_address},{}",
                        address_relative_to_bp_for_lambda_allocation * wl
                    ),
                    None,
                    true,
                );
                CodeGen::add(
                    &mut self.before,
                    &format!("mov dword {tmp_register},{sbp}"),
                    None,
                    true,
                );
                CodeGen::add(
                    &mut self.before,
                    &format!(
                        "sub dword {tmp_register},{}",
                        address_relative_to_bp_for_lambda_space * wl
                    ),
                    None,
                    true,
                );
                self.backend.call_function(
                    &mut self.before,
                    "addStaticAllocation_0",
                    &[
                        (&lambda_space_address, None),
                        (&tmp_register, None),
                        (&format!("{}", (num_of_values_in_context + 3) * wl), None),
                    ],
                    None,
                );

                stack_vals.release_tmp_register(&mut self.before, "tmp_register");

                // we save the allocation table address of the lambda space in the stack
                CodeGen::add(
                    &mut self.before,
                    &format!("push  {} {lambda_space_address}", self.backend.word_size()),
                    comment,
                    true,
                );

                // we put in lambda_space_address register the address of the lambda space
                CodeGen::add(
                    &mut self.before,
                    &format!("mov    dword {lambda_space_address}, [{lambda_space_address}]"),
                    None,
                    true,
                );
            } else {
                Self::allocate_lambda_space(
                    self.backend,
                    &mut self.before,
                    &lambda_space_address,
                    num_of_values_in_context + 3,
                    statics,
                );

                // we save the allocation table address of the lambda space in the stack
                CodeGen::add(
                    &mut self.before,
                    &format!("push  {} {lambda_space_address}", self.backend.word_size()),
                    comment,
                    true,
                );

                // we put in lambda_space_address register the address of the lambda space
                CodeGen::add(
                    &mut self.before,
                    &format!("mov    dword {lambda_space_address}, [{lambda_space_address}]"),
                    None,
                    true,
                );
            }

            let tmp_register =
                stack_vals.reserve_tmp_register(&mut self.before, self.backend, "tmp_register");

            let mut need_eax = false;

            let mut i = 1;

            context.iter().for_each(|(name, kind)| {
                let already_in_parent = if let Some(parent_lambda) = parent_lambda_space {
                    parent_lambda.is_in_context(name)
                } else {
                    false
                };

                if !already_in_parent {
                    let relative_address = match kind {
                        TypedValKind::ParameterRef(index, _) => (index + 2) as i32,
                        TypedValKind::LetRef(_, _) => {
                            -(self.stack_vals.find_local_val_relative_to_bp(name).unwrap() as i32)
                        }
                    };
                    self.backend.indirect_mov(
                        &mut self.before,
                        &format!("{}+{}", sbp, relative_address * wl as i32),
                        &format!("{lambda_space_address} + {}", (i + 2) * wl),
                        &tmp_register,
                        Some(&format!("context parameter {}", name)),
                    );
                } else if let Some(pls) = parent_lambda_space {
                    if let Some(parent_index) = pls.get_index(name) {
                        if !need_eax {
                            CodeGen::add(
                                &mut self.before,
                                &format!("push  {} eax", self.backend.word_size()),
                                comment,
                                true,
                            );
                            CodeGen::add(
                                &mut self.before,
                                &format!("mov   {} eax, [{sbp} + {}]", ws, 2 * wl),
                                None,
                                true,
                            );
                            need_eax = true;
                        }
                        self.backend.indirect_mov(
                            &mut self.before,
                            &format!("eax + {}", (parent_index + 2) * wl),
                            &format!("{lambda_space_address} + {}", (i + 2) * wl),
                            &tmp_register,
                            Some(&format!("context parameter from parent {}", name)),
                        );
                    } else {
                        panic!()
                    }
                } else {
                    panic!()
                }

                lambda_space.add(name.clone(), kind.clone());
                i += 1;
            });

            if let Some(add_ref_function_def) =
                self.backend
                    .create_lambda_addref(&lambda_space, module, statics, &def.name)
            {
                add_ref_function = add_ref_function_def.name.clone();

                lambda_space.add_ref_function(add_ref_function_def);
            }

            if let Some(deref_function_def) =
                self.backend
                    .create_lambda_deref(&lambda_space, module, statics, &def.name)
            {
                deref_function = deref_function_def.name.clone();

                lambda_space.add_ref_function(deref_function_def);
            }

            if need_eax {
                CodeGen::add(&mut self.before, "pop   eax", None, true);
            }
            stack_vals.release_tmp_register(&mut self.before, "tmp_register");
        }

        CodeGen::add(
            &mut self.before,
            &format!("mov {} [{lambda_space_address}], {}", ptrs, def.name),
            None,
            true,
        );

        CodeGen::add(
            &mut self.before,
            &format!(
                "mov {} [{lambda_space_address} + {wl}], {add_ref_function}",
                ws
            ),
            None,
            true,
        );
        CodeGen::add(
            &mut self.before,
            &format!(
                "mov {} [{lambda_space_address} + 2 * {wl}], {deref_function}",
                ws
            ),
            None,
            true,
        );

        // I reuse the lambda_space_address register to store the allocation table address of the lambda space
        CodeGen::add(
            &mut self.before,
            &format!("pop   {lambda_space_address}"),
            None,
            true,
        );
        if self.immediate {
            CodeGen::add(
                &mut self.before,
                &format!("mov {ws} eax, {lambda_space_address}"),
                None,
                true,
            );
        } else {
            // + 1 due to push ecx
            let to_remove_from_stack = self.to_remove_from_stack();
            CodeGen::add(
                &mut self.before,
                &format!(
                    "mov {} [{} + {}], {lambda_space_address}",
                    ws,
                    sp,
                    (to_remove_from_stack + 1) * wl
                ),
                comment,
                true,
            );
        }

        if !context.is_empty() && !optimize && self.dereference {
            // we don't need a "deep" reference / dereference here
            self.backend.call_add_ref_simple(
                &mut self.before,
                &lambda_space_address,
                "lambda space",
                statics,
            );
            let pos = self.push_to_scope_stack(&lambda_space_address);

            let mut result = String::new();
            CodeGen::add(&mut result, "; scope pop", None, true);
            self.backend.call_deref_simple(
                &mut result,
                &format!("[{} - {}]", self.backend.stack_base_pointer(), pos),
                "lambda space",
                statics,
            );

            self.after.insert(0, result);
        }

        stack_vals.release_tmp_register(&mut self.before, "this_lambda_space_address");

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
        stack_vals: &StackVals,
    ) {
        self.debug_and_before(&format!("adding val {val_name}"), indent);

        if let Some(lambda_space_index) = lambda_space.and_then(|it| it.get_index(val_name)) {
            self.add_val_from_lambda_space(
                &original_param_name,
                lambda_space_index,
                indent,
                stack_vals,
            );
        } else {
            self.add_val_from_parameter(
                original_param_name,
                ast_typed_type,
                index_in_context as i32 + 2,
                indent,
                stack_vals,
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
        stack_vals: &StackVals,
    ) {
        self.debug_and_before(
            &format!("adding let val {val_name} original_param_name {original_param_name}"),
            indent,
        );

        if let Some(lambda_space_index) = lambda_space.and_then(|it| it.get_index(val_name)) {
            self.add_val_from_lambda_space(
                &original_param_name,
                lambda_space_index,
                indent,
                stack_vals,
            );
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
                stack_vals,
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
        stack_vals: &StackVals,
    ) {
        self.debug_and_before(
            &format!("param {original_param_name}, index_relative_to_bp {index_relative_to_bp}"),
            indent,
        );

        let word_len = self.backend.word_len();

        let source = &format!(
            "{}+{}",
            self.backend.stack_base_pointer(),
            index_relative_to_bp * word_len as i32
        );

        let src: String = format!("[{source}]");

        if self.inline {
            self.parameters_values.insert(original_param_name, src);
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
                let tmp_register =
                    stack_vals.reserve_tmp_register(&mut self.before, self.backend, "tmp_for_move");
                let to_remove_from_stack = self.to_remove_from_stack();
                self.backend.indirect_mov(
                    &mut self.before,
                    source,
                    &format!(
                        "{} + {}",
                        self.backend.stack_pointer(),
                        (to_remove_from_stack + 1) * self.backend.word_len()
                    ),
                    &tmp_register,
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

                stack_vals.release_tmp_register(&mut self.before, "tmp_for_move");
            }
            self.parameter_added_to_stack();
        }
    }

    fn add_val_from_lambda_space(
        &mut self,
        original_param_name: &str,
        lambda_space_index: usize,
        indent: usize,
        stack_vals: &StackVals,
    ) {
        self.debug_and_before(&format!("add_lambda_param_from_lambda_space, original_param_name {original_param_name}, lambda_space_index {lambda_space_index}"), indent);

        let word_len = self.backend.word_len();

        let src = if let Some(ref register) = stack_vals.find_tmp_register("lambda_space_address") {
            format!("[{register} + {}]", (lambda_space_index + 2) * word_len)
        } else {
            panic!()
        };

        if self.inline {
            self.has_inline_lambda_param = true;
            self.parameters_values
                .insert(original_param_name.into(), src);
        } else {
            if self.immediate {
                CodeGen::add(
                    &mut self.before,
                    &format!("mov   {} eax,{src}", self.backend.pointer_size(),),
                    None,
                    true,
                );
            } else if let Some(register) = stack_vals.find_tmp_register("lambda_space_address") {
                let tmp_register =
                    stack_vals.reserve_tmp_register(&mut self.before, self.backend, "tmp_for_move");
                let to_remove_from_stack = self.to_remove_from_stack();

                self.backend.indirect_mov(
                    &mut self.before,
                    &format!("{register} + {}", (lambda_space_index + 2) * word_len),
                    &format!(
                        "{} + {}",
                        self.backend.stack_pointer(),
                        (to_remove_from_stack + 1) * self.backend.word_len()
                    ),
                    &tmp_register,
                    None,
                );
                stack_vals.release_tmp_register(&mut self.before, "tmp_for_move");
            } else {
                panic!()
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
                    (i - self.to_remove_from_stack() as i32) * word_len,
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

        let word_len = self.backend.word_len();

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
        let pos = self.stack_vals.reserve_local_val(what) * self.backend.word_len();
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
        backend.call_function(
            out,
            "malloc_0",
            &[
                (&format!("{}", slots * backend.word_len()), None),
                (&format!("[{label}]"), None),
            ],
            Some("lambda space allocation"),
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
}
