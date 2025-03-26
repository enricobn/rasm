use auto_impl::auto_impl;
use linked_hash_map::LinkedHashMap;
use log::debug;
use rasm_parser::parser::ast::ASTValueType;

use crate::{
    codegen::{
        asm::code_gen_asm::STACK_VAL_SIZE_NAME,
        enh_ast::EnhASTIndex,
        enh_val_context::TypedValContext,
        function_call_parameters::FunctionCallParameters,
        get_reference_type_name,
        lambda::LambdaSpace,
        stack::StackVals,
        statics::{MemoryUnit, MemoryValue, Statics},
        typedef_provider::TypeDefProvider,
        CodeGen, TypedValKind,
    },
    enh_type_check::typed_ast::{
        ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
        ASTTypedParameterDef, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
    },
};

use super::{
    backend::BackendAsm,
    code_gen_asm::{CodeGenAsm, CodeGenAsmContext},
};

#[auto_impl(Box)]
pub trait FunctionCallParametersAsm: FunctionCallParameters<CodeGenAsmContext> {
    fn to_remove_from_stack_name(&self) -> String;

    fn to_remove_from_stack(&self) -> usize;
}

pub struct FunctionCallParametersAsmImpl<'a> {
    parameters: Vec<ASTTypedParameterDef>,
    parameters_added: usize,
    before: String,
    parameters_values: LinkedHashMap<String, String>,
    backend: &'a dyn BackendAsm,
    inline: bool,
    immediate: bool,
    has_inline_lambda_param: bool,
    //lambda_slots_to_deallocate: usize,
    stack_vals: StackVals,
    after: Vec<String>,
    dereference: bool,
    id: usize,
    code_gen: &'a CodeGenAsm,
}

impl<'a> FunctionCallParameters<CodeGenAsmContext> for FunctionCallParametersAsmImpl<'a> {
    fn add_label(
        &mut self,
        param_name: &str,
        label: String,
        value: String,
        comment: Option<&str>,
        typed_type: &ASTTypedType,
        statics: &Statics,
    ) {
        if self.inline {
            self.parameters_values
                .insert(param_name.into(), format!("[{label}]"));
        } else {
            // TODO can be optimized?
            self.code_gen.add(&mut self.before, "push eax", None, true);
            self.code_gen.add(
                &mut self.before,
                &format!("mov {} eax, [{label}]", self.backend.word_size()),
                comment,
                true,
            );
            let to_remove_from_stack = self.to_remove_from_stack();
            self.code_gen.add(
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
            self.code_gen.add(&mut self.before, "pop eax", None, true);
            self.parameter_added_to_stack();
        }
    }

    fn add_string_constant(
        &mut self,
        param_name: &str,
        value: &str,
        comment: Option<&str>,
        statics: &mut Statics,
    ) {
        let label = statics.add_str(value);
        self.add_label(
            param_name,
            label,
            value.to_string(),
            comment,
            &ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
            &statics,
        );
    }

    fn add_function_call(
        &mut self,
        module: &ASTTypedModule,
        comment: &str,
        param_type: ASTTypedType,
        statics: &mut Statics,
        name: String,
        before: String,
        current: String,
        t: &ASTTypedType,
    ) {
        if self.immediate {
            panic!();
        }

        self.push(&before);
        self.push(&current);

        let wl = self.backend.word_len();
        let ws = self.backend.word_size();
        let sp = self.backend.stack_pointer();

        self.code_gen.add(
            &mut self.before,
            &format!("mov {ws} [{sp} + {}], eax", self.parameters_added * wl),
            Some(comment),
            true,
        );

        if let Some(name) = get_reference_type_name(&param_type, module) {
            self.add_code_for_reference_type(module, &name, "eax", comment, statics);
        }
        self.parameter_added_to_stack();
    }

    fn add_lambda(
        &mut self,
        def: &mut ASTTypedFunctionDef,
        parent_lambda_space: Option<&LambdaSpace>,
        context: &TypedValContext,
        comment: Option<&str>,
        statics: &mut Statics,
        module: &ASTTypedModule,
        code_gen_context: &CodeGenAsmContext,
        lambda_in_stack: bool,
        param_type: &ASTTypedType,
        name: &str,
    ) -> LambdaSpace {
        //let optimize = false;
        let sbp = self.backend.stack_base_pointer();
        let sp = self.backend.stack_pointer();
        let wl = self.backend.word_len();
        let ws = self.backend.word_size();
        let rr = self.code_gen.return_register();

        let mut references = self.body_references_to_context(&def.body, context);
        references.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));
        references.dedup_by(|(name1, _), (name2, _)| name1 == name2);

        let mut context = TypedValContext::new(None);
        for (key, kind) in references {
            context.insert(key, kind);
        }

        let mut lambda_space = LambdaSpace::new(context.clone());

        let mut add_ref_function = "addRef".to_owned();
        let mut deref_function = "deref".to_owned();

        let lambda_space_address = code_gen_context.stack_vals.reserve_tmp_register(
            &mut self.before,
            "this_lambda_space_address",
            &self.code_gen,
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
            self.code_gen.add(
                &mut self.before,
                &format!("push  {} {label_allocation}", self.backend.word_size()),
                comment,
                true,
            );

            // we put in lambda_space_address register the address of the lambda space
            self.code_gen.add(
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

            if lambda_in_stack {
                self.code_gen.allocate_lambda_space_in_stack(
                    &mut self.before,
                    &lambda_space_address,
                    &code_gen_context.stack_vals,
                    num_of_values_in_context + 3,
                );
            } else {
                self.code_gen.allocate_lambda_space(
                    &mut self.before,
                    &lambda_space_address,
                    num_of_values_in_context + 3,
                    statics,
                );
            }

            // we save the allocation table address of the lambda space in the stack
            self.code_gen.add(
                &mut self.before,
                &format!("push  {ws} {lambda_space_address}"),
                comment,
                true,
            );

            // we put in lambda_space_address register the address of the lambda space
            self.code_gen.add(
                &mut self.before,
                &format!("mov    {ws} {lambda_space_address}, [{lambda_space_address}]"),
                None,
                true,
            );

            let tmp_register = code_gen_context.stack_vals.reserve_tmp_register(
                &mut self.before,
                "tmp_register",
                &self.code_gen,
            );

            let mut need_return_register = false;

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
                    self.code_gen.indirect_mov(
                        &mut self.before,
                        &format!("{}+{}", sbp, relative_address * wl as i32),
                        &format!("{lambda_space_address} + {}", (i + 2) * wl),
                        &tmp_register,
                        Some(&format!("context parameter {}", name)),
                    );
                } else if let Some(pls) = parent_lambda_space {
                    if let Some(parent_index) = pls.get_index(name) {
                        if !need_return_register {
                            self.code_gen.add_rows(
                                &mut self.before,
                                vec![
                                    &format!("push  {ws} {rr}"),
                                    &format!("mov   {} {rr}, [{sbp} + {}]", ws, 2 * wl),
                                ],
                                comment,
                                true,
                            );
                            need_return_register = true;
                        }
                        self.code_gen.indirect_mov(
                            &mut self.before,
                            &format!("{rr} + {}", (parent_index + 2) * wl),
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

            if let Some(add_ref_function_def) = self.code_gen.create_lambda_addref(
                &def.namespace,
                &lambda_space,
                module,
                statics,
                &def.name,
            ) {
                add_ref_function = add_ref_function_def.name.clone();

                lambda_space.add_ref_function(add_ref_function_def);
            }

            if let Some(deref_function_def) = self.code_gen.create_lambda_deref(
                &def.namespace,
                &lambda_space,
                module,
                statics,
                &def.name,
            ) {
                deref_function = deref_function_def.name.clone();

                lambda_space.add_ref_function(deref_function_def);
            }

            if need_return_register {
                self.code_gen.add(&mut self.before, "pop   eax", None, true);
            }
            code_gen_context.stack_vals.release_tmp_register(
                &self.code_gen,
                &mut self.before,
                "tmp_register",
            );
        }

        self.code_gen.populate_lambda_space(
            &mut self.before,
            &lambda_space_address,
            &def.name,
            &add_ref_function,
            &deref_function,
        );

        // I reuse the lambda_space_address register to store the allocation table address of the lambda space
        self.code_gen.add(
            &mut self.before,
            &format!("pop   {lambda_space_address}"),
            None,
            true,
        );
        if self.immediate {
            self.code_gen
                .set_return_value(&mut self.before, &lambda_space_address);
        } else {
            // + 1 due to push ecx
            let to_remove_from_stack = self.to_remove_from_stack();
            self.code_gen.add(
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

        if !context.is_empty() && !lambda_in_stack && self.dereference {
            // we don't need a "deep" reference / dereference here
            self.code_gen.call_add_ref_simple(
                &mut self.before,
                &lambda_space_address,
                "lambda space",
                statics,
            );
            let pos = self.code_gen.push_to_scope_stack(
                &mut self.before,
                &lambda_space_address,
                &self.stack_vals,
            );

            let mut result = String::new();
            self.code_gen.add_comment(&mut result, "scope pop", true);
            self.code_gen.call_deref_simple(
                &mut result,
                &format!("[{} - {}]", self.backend.stack_base_pointer(), pos),
                "lambda space",
                statics,
            );

            self.after.insert(0, result);
        }

        code_gen_context.stack_vals.release_tmp_register(
            &self.code_gen,
            &mut self.before,
            "this_lambda_space_address",
        );

        self.parameter_added_to_stack();

        lambda_space
    }

    fn add_parameter_ref(
        &mut self,
        original_param_name: String,
        val_name: &str,
        index_in_context: usize,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
        code_gen_context: &CodeGenAsmContext,
        statics: &Statics,
        type_def_provider: &dyn TypeDefProvider,
        typed_type: &ASTTypedType,
    ) {
        self.debug_and_before(&format!("adding val {val_name}"), indent);

        if let Some(lambda_space_index) = lambda_space.and_then(|it| it.get_index(val_name)) {
            self.add_val_from_lambda_space(
                &original_param_name,
                lambda_space_index,
                indent,
                &code_gen_context.stack_vals,
            );
        } else {
            self.add_val_from_parameter(
                original_param_name,
                index_in_context as i32 + 2,
                indent,
                &code_gen_context.stack_vals,
            );
        }
    }

    fn add_let_val_ref(
        &mut self,
        original_param_name: String,
        val_name: &str,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
        code_gen_context: &CodeGenAsmContext,
        ast_index: &EnhASTIndex,
        statics: &Statics,
        type_def_provider: &dyn TypeDefProvider,
        typed_type: &ASTTypedType,
    ) {
        let index_in_context = code_gen_context
            .stack_vals
            .find_local_val_relative_to_bp(val_name);
        self.debug_and_before(
            &format!("adding let val {val_name} original_param_name {original_param_name}"),
            indent,
        );

        if let Some(lambda_space_index) = lambda_space.and_then(|it| it.get_index(val_name)) {
            self.add_val_from_lambda_space(
                &original_param_name,
                lambda_space_index,
                indent,
                &code_gen_context.stack_vals,
            );
        } else {
            self.add_val_from_parameter(
                original_param_name.clone(),
                -(index_in_context
                    .unwrap_or_else(|| panic!("cannot find index for {val_name} : {ast_index}"))
                    as i32),
                indent,
                &code_gen_context.stack_vals,
            );
        }
    }

    fn add_value_type(&mut self, name: &str, value_type: &ASTValueType) {
        let v = self.code_gen.value_to_string(value_type);
        self.add_number(name, v, None);
    }

    fn push(&mut self, s: &str) {
        self.before.push_str(s);
    }

    fn add_on_top_of_after(&mut self, s: &str) {
        self.after.insert(0, s.into());
    }

    fn before(&self) -> String {
        let mut result = String::new();

        let word_len = self.backend.word_len();

        if self.to_remove_from_stack() > 0 {
            self.code_gen.add(
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

        result.replace(
            &self.to_remove_from_stack_name(),
            &(self.to_remove_from_stack() * self.backend.word_len()).to_string(),
        )
    }

    fn current(&self) -> String {
        String::new()
    }

    fn after(&self) -> Vec<String> {
        let s = String::new();

        let mut result = vec![s];
        let mut after = self.after.clone();
        result.append(&mut after);
        result
    }

    fn resolve_native_parameters(
        &self,
        body: &str,
        to_remove_from_stack: String,
        ident: usize,
        return_value: bool,
        is_inner_value: bool,
        return_type: Option<&ASTTypedType>,
        is_lambda: bool,
    ) -> String {
        let _ = return_type;
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
                substitutions.push((par.name.clone(), par_value.clone()));
                continue;
            }

            debug!(
                "{}cannot find parameter {} in parameters_values {:?} we take if from the stack",
                " ".repeat(ident * 4),
                par.name,
                self.parameters_values
            );

            // it could be a lambda when using function reference to a native function
            // in this case we have an extra parameter that is the lambda space that is
            // always empty, but we must skip it
            let lambda_adj = if is_lambda { 1 } else { 0 };

            let relative_address = if self.inline {
                debug!(
                    "{} i {}, self.to_remove_from_stack {}, to_remove_from_stack {}",
                    " ".repeat(ident * 4),
                    i,
                    self.to_remove_from_stack(),
                    to_remove_from_stack
                );
                format!(
                    "{}-({})-{}",
                    (i - self.to_remove_from_stack() as i32 + lambda_adj) * word_len,
                    to_remove_from_stack,
                    STACK_VAL_SIZE_NAME
                )
            } else {
                format!("{}", (i + 2 + lambda_adj) * self.backend.word_len() as i32)
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

    fn parameters_values(&self) -> &LinkedHashMap<String, String> {
        &self.parameters_values
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
}

impl<'a> FunctionCallParametersAsmImpl<'a> {
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
    pub fn new(
        backend: &'a dyn BackendAsm,
        parameters: Vec<ASTTypedParameterDef>,
        inline: bool,
        immediate: bool,
        stack: StackVals,
        dereference: bool,
        id: usize,
        code_gen: &'a CodeGenAsm,
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
            code_gen,
        }
    }

    fn add_number(&mut self, param_name: &str, n: String, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), n);
        } else {
            let to_remove_from_stack = self.to_remove_from_stack();
            self.code_gen.add(
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

    fn add_code_for_reference_type(
        &mut self,
        module: &ASTTypedModule,
        type_name: &str,
        source: &str,
        descr: &str,
        statics: &mut Statics,
    ) {
        // TODO I really don't know if it is correct not to add ref and deref for immediate
        if self.dereference {
            self.code_gen
                .call_add_ref(&mut self.before, source, type_name, descr, module, statics);
            let pos = self
                .code_gen
                .push_to_scope_stack(&mut self.before, source, &self.stack_vals);

            self.after.insert(
                0,
                Self::pop_from_scope_stack_and_deref(
                    module,
                    self.backend,
                    type_name,
                    descr,
                    pos,
                    statics,
                    &self.code_gen,
                ),
            );
        }
    }

    fn add_val_from_parameter(
        &mut self,
        original_param_name: String,
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
            if self.immediate {
                self.code_gen.set_return_value(&mut self.before, &src);
            } else {
                let tmp_register = stack_vals.reserve_tmp_register(
                    &mut self.before,
                    "tmp_for_move",
                    self.code_gen,
                );
                let to_remove_from_stack = self.to_remove_from_stack();
                self.code_gen.indirect_mov(
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

                stack_vals.release_tmp_register(&self.code_gen, &mut self.before, "tmp_for_move");
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
                self.code_gen.add(
                    &mut self.before,
                    &format!("mov   {} eax,{src}", self.backend.pointer_size(),),
                    None,
                    true,
                );
            } else if let Some(register) = stack_vals.find_tmp_register("lambda_space_address") {
                let tmp_register = stack_vals.reserve_tmp_register(
                    &mut self.before,
                    "tmp_for_move",
                    &self.code_gen,
                );
                let to_remove_from_stack = self.to_remove_from_stack();

                self.code_gen.indirect_mov(
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
                stack_vals.release_tmp_register(&self.code_gen, &mut self.before, "tmp_for_move");
            } else {
                panic!()
            }

            self.parameter_added_to_stack();
        }
    }

    fn debug_and_before(&mut self, descr: &str, indent: usize) {
        debug!("{} {descr}", " ".repeat(indent * 4));
        self.code_gen.add_comment(&mut self.before, descr, true);
    }

    fn parameter_added_to_stack(&mut self) {
        self.parameters_added += 1;
    }

    fn pop_from_scope_stack_and_deref(
        module: &ASTTypedModule,
        backend: &dyn BackendAsm,
        type_name: &str,
        descr: &str,
        pos: usize,
        statics: &mut Statics,
        code_gen: &CodeGenAsm,
    ) -> String {
        let mut result = String::new();
        code_gen.add(&mut result, "; scope pop", None, true);
        result.push_str(&code_gen.call_deref(
            &format!("[{} - {}]", backend.stack_base_pointer(), pos),
            type_name,
            descr,
            module,
            statics,
        ));
        result
    }
}

impl<'a> FunctionCallParametersAsm for FunctionCallParametersAsmImpl<'a> {
    fn to_remove_from_stack_name(&self) -> String {
        format!("$_to_remove_rom_stack{}", self.id)
    }

    fn to_remove_from_stack(&self) -> usize {
        if self.immediate {
            0
        } else {
            self.parameters_added
        }
    }
}
