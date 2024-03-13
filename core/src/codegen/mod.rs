use std::collections::{HashMap, HashSet};
use std::env;
use std::iter::zip;
use std::ops::Deref;

use linked_hash_map::LinkedHashMap;
use log::debug;

use enhanced_module::EnhancedASTModule;
use lambda::{LambdaCall, LambdaSpace};

use crate::codegen::backend::{Backend, BackendAsm};
use crate::codegen::compile_target::CompileTarget;
use crate::codegen::function_call_parameters::{
    FunctionCallParameters, FunctionCallParametersAsm, FunctionCallParametersAsmImpl,
};
use crate::codegen::stack::StackVals;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::val_context::{TypedValContext, ValContext};
use crate::debug_i;
use crate::errors::CompilationError;
use crate::parser::ast::{ASTIndex, ASTNameSpace, ASTParameterDef, ASTType, ValueType};
use crate::transformations::type_functions_creator::type_mandatory_functions;
use crate::transformations::typed_functions_creator::TypedFunctionsCreator;
use crate::type_check::get_new_native_call;
use crate::type_check::typed_ast::{
    convert_to_typed_module, get_default_functions, get_type_of_typed_expression,
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef,
    ASTTypedModule, ASTTypedParameterDef, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};
use crate::type_check::used_functions::UsedFunctions;
use crate::utils::OptionDisplay;

pub mod backend;
pub mod compile_target;
pub mod enhanced_module;
pub mod function_call_parameters;
pub mod lambda;
pub mod stack;
pub mod statics;
pub mod text_macro;
pub mod typedef_provider;
pub mod val_context;

/// It's a marker that will be replaced by the code generator with the size (in bytes)
/// of all the vals in the stack. We need it since we know the full size only at the end of a function
/// generation, but we need that value during the code generation...   
pub const STACK_VAL_SIZE_NAME: &str = "$stack_vals_size";

#[derive(Clone)]
pub struct CodeGenOptions {
    pub lambda_space_size: usize,
    pub heap_size: usize,
    pub heap_table_slots: usize,
    pub dereference: bool,
    pub optimize_unused_functions: bool,
    pub print_memory: bool,
    pub requires: Vec<String>,
    pub externals: Vec<String>,
}

impl Default for CodeGenOptions {
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

pub struct CodeGenAsm {
    pub module: ASTTypedModule,
    backend: Box<dyn BackendAsm>,
    options: CodeGenOptions,
    target: CompileTarget,
    debug: bool,
}

#[derive(Clone, Debug)]
pub enum ValKind {
    ParameterRef(usize, ASTParameterDef),
    LetRef(usize, ASTType, ASTIndex),
}

#[derive(Clone, Debug)]
pub enum TypedValKind {
    ParameterRef(usize, ASTTypedParameterDef),
    LetRef(usize, ASTTypedType),
}

pub fn get_typed_module(
    backend: &dyn Backend,
    module: EnhancedASTModule,
    print_memory_info: bool,
    dereference: bool,
    print_module: bool,
    statics: &mut Statics,
    target: &CompileTarget,
    debug: bool,
) -> Result<ASTTypedModule, CompilationError> {
    let mandatory_functions = type_mandatory_functions(&module);
    let default_functions = get_default_functions(print_memory_info);

    let mut typed_module = convert_to_typed_module(
        &module,
        print_module,
        mandatory_functions,
        backend,
        statics,
        dereference,
        default_functions,
        target,
        debug,
    )?;
    target
        .typed_functions_creator(debug)
        .create(&mut typed_module, statics);
    Ok(typed_module)
}

pub fn get_std_lib_path() -> String {
    let current_dir = env::current_dir().unwrap();
    env::var("RASM_STDLIB").unwrap_or(current_dir.join("stdlib").to_str().unwrap().to_owned())
}

pub trait CodeGen<'a, BACKEND: Backend, FUNCTION_CALL_PARAMETERS: FunctionCallParameters> {
    fn backend(&self) -> &BACKEND;

    fn target(&self) -> &CompileTarget;

    fn options(&self) -> &CodeGenOptions;

    fn module(&self) -> &ASTTypedModule;

    fn generate(&self, statics: Statics) -> String {
        let debug = self.debug();
        let mut statics = statics;
        let mut id: usize = 0;
        let mut body = String::new();

        let mut lambdas = Vec::new();

        // for now main has no context
        let mut context = TypedValContext::new(None);

        let stack = StackVals::new();

        let mut after = String::new();
        let mut before = String::new();

        for statement in &self.module().body {
            match statement {
                ASTTypedStatement::Expression(e) => match e {
                    ASTTypedExpression::ASTFunctionCallExpression(call) => {
                        let (bf, af, mut lambda_calls) = self.call_function(
                            &call.namespace,
                            call,
                            &context,
                            None,
                            "0".into(),
                            None,
                            0,
                            false,
                            &stack,
                            &mut id,
                            &mut statics,
                        );
                        before.push_str(&bf);

                        Self::insert_on_top(&af.join("\n"), &mut after);

                        lambdas.append(&mut lambda_calls);
                    }
                    _ => {
                        panic!("unsupported expression in body {e}");
                    }
                },
                ASTTypedStatement::LetStatement(name, expr, is_const, _let_index) => {
                    let mut new_lambda_calls = self.add_let(
                        &expr.namespace(),
                        &mut context,
                        &stack,
                        &mut after,
                        &mut before,
                        name,
                        expr,
                        None,
                        None,
                        *is_const,
                        &mut statics,
                        &mut body,
                        &mut id,
                    );
                    lambdas.append(&mut new_lambda_calls);
                }
            }
        }

        body.push_str(&before.replace(
            STACK_VAL_SIZE_NAME,
            &(stack.len_of_all() * self.backend().word_len()).to_string(),
        ));
        body.push_str(&after);

        // debug!("stack {:?}", stack);
        //        assert_eq!(stack.size(), 0);

        // TODO add a command line argument
        //Parser::print(&self.module);

        let mut functions_generated_code = self.create_lambdas(lambdas, 0, &mut id, &mut statics);

        functions_generated_code.extend(self.create_all_functions(&mut id, &mut statics));

        let mut generated_code = String::new();

        self.target().preamble(&mut generated_code);

        self.target().add_statics(&mut statics);

        let (static_declarations, static_code) =
            self.target().generate_statics_code(&statics, debug);

        generated_code.push_str(&static_declarations);

        if debug {
            self.backend().define_debug(&mut generated_code);
        }

        self.initialize_static_values(&mut generated_code);

        let code = self.translate_static_code(&static_code, debug);

        self.target().add(&mut generated_code, &code, None, true);

        self.create_command_line_arguments(&mut generated_code, debug);

        self.target().add(&mut generated_code, "", None, true);

        self.backend().function_preamble(&mut generated_code);

        self.backend()
            .reserve_local_vals(&stack, &mut generated_code);

        // probably there is not a valid body from functions
        for (_name, (_defs, bd)) in functions_generated_code.iter() {
            body.push_str(bd);
        }

        let new_body = self.translate_body(&body, &mut statics).unwrap();

        generated_code.push_str(&new_body);
        generated_code.push('\n');

        self.backend().restore(&stack, &mut generated_code);

        self.backend().function_end(&mut generated_code, false);

        if self.options().print_memory {
            self.print_memory_info(&mut generated_code);
        }

        self.target()
            .call_function(&mut generated_code, "exitMain", &[("0", None)], None, debug);

        let used_functions = self.get_used_functions(&functions_generated_code, &generated_code);

        for (_, (defs, _bd)) in used_functions {
            generated_code.push_str(&defs);
        }

        generated_code
    }

    fn create_command_line_arguments(&self, generated_code: &mut String, debug: bool);

    fn translate_static_code(&self, static_code: &String, debug: bool) -> String;

    fn call_lambda_parameter(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        kind: &TypedValKind,
    );

    fn call_function_(
        &'a self,
        namespace: &ASTNameSpace,
        function_call: &&ASTTypedFunctionCall,
        context: &TypedValContext,
        parent_def: &Option<&ASTTypedFunctionDef>,
        added_to_stack: String,
        before: &mut String,
        parameters: Vec<ASTTypedParameterDef>,
        inline: bool,
        body: Option<ASTTypedFunctionBody>,
        address_to_call: String,
        lambda_space_opt: Option<&LambdaSpace>,
        indent: usize,
        is_lambda: bool,
        stack_vals: &StackVals,
        after: &mut Vec<String>,
        id: &mut usize,
        statics: &mut Statics,
    ) -> Vec<LambdaCall> {
        let mut lambda_calls = Vec::new();

        self.target().add_empty_line(before);

        if inline {
            self.target().add_comment(
                before,
                &format!(
                    "inlining function {}, added to stack {}",
                    function_call.function_name, added_to_stack
                ),
                true,
            );
        } else {
            self.target().add_comment(
                before,
                &format!(
                    "calling function {}, added to stack {}",
                    function_call.function_name, added_to_stack
                ),
                true,
            );
        }

        let mut call_parameters =
            self.function_call_parameters(&parameters, inline, false, stack_vals, *id);

        *id += 1;

        if !function_call.parameters.is_empty() {
            // as for C calling conventions parameters are pushed in reverse order
            for (param_index, expr) in function_call.parameters.iter().enumerate() {
                let param_opt = parameters.get(param_index);
                let param_name = param_opt
                    .unwrap_or_else(|| {
                        panic!(
                            "Cannot find param {} : {:?} of function call {}",
                            param_index, expr, function_call.function_name
                        )
                    })
                    .name
                    .clone();
                let param_type = param_opt
                    .unwrap_or_else(|| {
                        panic!(
                            "Cannot find param {} of function call {}",
                            param_index, function_call.function_name
                        )
                    })
                    .ast_type
                    .clone();

                debug!(
                    "{}adding parameter {}: {:?}",
                    " ".repeat(indent * 4),
                    param_name,
                    expr
                );

                match expr {
                    ASTTypedExpression::StringLiteral(value) => {
                        let label = statics.add_str(value);
                        call_parameters.add_label(&param_name, label, None);
                    }
                    /*ASTTypedExpression::CharLiteral(c) => {
                        let label = self.statics.add_char(c);
                        call_parameters.add_string_literal(&param_name, label, None);
                    }*/
                    ASTTypedExpression::Value(value_type, _) => {
                        call_parameters.add_value_type(&param_name, value_type)
                    }
                    ASTTypedExpression::ASTFunctionCallExpression(call) => {
                        let added_to_stack = self
                            .added_to_stack_for_call_parameter(&added_to_stack, &call_parameters);

                        let (bf, af, mut inner_lambda_calls) = self.call_function(
                            namespace,
                            call,
                            context,
                            *parent_def,
                            added_to_stack,
                            lambda_space_opt,
                            indent + 1,
                            false,
                            stack_vals,
                            id,
                            statics,
                        );

                        call_parameters.push(&bf);

                        //after.insert(0, af.join("\n"));
                        call_parameters.add_on_top_of_after(&af.join("\n"));

                        call_parameters.add_function_call(
                            self.module(),
                            &format!("{param_name} = {} : {}", &call.function_name, call.index),
                            param_type.clone(),
                            statics,
                        );

                        lambda_calls.append(&mut inner_lambda_calls);
                    }
                    ASTTypedExpression::ValueRef(name, index) => {
                        let error_msg = format!(
                            "Cannot find val {}, calling function {}",
                            name, function_call.function_name
                        );
                        self.add_val(
                            context,
                            &lambda_space_opt,
                            &indent,
                            &mut call_parameters,
                            &param_name,
                            name,
                            &error_msg,
                            stack_vals,
                            index,
                            statics,
                        );
                    }
                    ASTTypedExpression::Lambda(lambda_def) => {
                        let (return_type, parameters_types) =
                            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                                return_type,
                                parameters,
                            }) = param_type
                            {
                                (return_type, parameters)
                            } else {
                                panic!("Parameter is not a lambda: {:?}", param_type);
                            };

                        if parameters_types.len() != lambda_def.parameter_names.len() {
                            panic!("Lambda parameters do not match definition");
                        }

                        let rt = return_type.deref().clone();

                        let name = format!("lambda{}", id);
                        let mut def = ASTTypedFunctionDef {
                            namespace: function_call.namespace.clone(),
                            //name: format!("{}_{}_{}_lambda{}", parent_def_description, function_call.function_name, param_name, self.id),
                            name: name.clone(),
                            original_name: name,
                            parameters: Vec::new(), // parametrs are calculated later
                            return_type: rt,
                            body: ASTTypedFunctionBody::RASMBody(lambda_def.clone().body),
                            inline: false,
                            generic_types: LinkedHashMap::new(),
                            index: lambda_def.index.clone(),
                        };

                        *id += 1;

                        debug!("{}Adding lambda {}", " ".repeat(indent * 4), param_name);

                        let function_def = self
                            .module()
                            .functions_by_name
                            .get(&function_call.function_name)
                            .unwrap();

                        let optimize = function_def.return_type.is_unit()
                            || can_optimize_lambda_space(&function_def.return_type, self.module());

                        let lambda_space = call_parameters.add_lambda(
                            &def,
                            lambda_space_opt,
                            context,
                            None,
                            statics,
                            self.module(),
                            stack_vals,
                            optimize,
                            self.target(),
                        );

                        // I add the parameters of the lambda itself
                        for i in 0..parameters_types.len() {
                            let (p_name, p_index) = lambda_def.parameter_names.get(i).unwrap();
                            def.parameters.push(ASTTypedParameterDef {
                                name: p_name.clone(),
                                ast_type: parameters_types.get(i).unwrap().clone(),
                                ast_index: p_index.clone(),
                            });
                            if context.get(p_name).is_some() {
                                panic!(
                                    "parameter {p_name} already used in this context: {p_index}"
                                );
                            }
                        }

                        let lambda_call = LambdaCall {
                            def,
                            space: lambda_space,
                        };

                        lambda_calls.push(lambda_call);
                    }
                }
            }
        }

        //before.push_str(&call_parameters.before());
        let string = call_parameters.before();
        before.push_str(&string);

        if inline {
            if let Some(ASTTypedFunctionBody::NativeBody(body)) = &body {
                /*let mut added_to_stack = added_to_stack;
                added_to_stack.push_str(&format!(
                    " + {}",
                    stack_vals.len() * self.backend.word_len()
                ));

                 */

                before.push_str(&call_parameters.resolve_native_parameters(
                    body,
                    added_to_stack,
                    indent,
                ));
                before.push('\n');
            } else {
                panic!("Only native  can be inlined, for now...");
            }
        } else if is_lambda {
            if let Some(index_in_lambda_space) =
                lambda_space_opt.and_then(|it| it.get_index(&function_call.function_name))
            {
                self.call_lambda(&function_call, before, stack_vals, index_in_lambda_space);
            } else if let Some(kind) = context.get(&function_call.function_name) {
                self.call_lambda_parameter(&function_call, before, stack_vals, kind);
            } else {
                panic!(
                    "lambda space does not contain {}",
                    function_call.function_name
                );
            }
        } else {
            self.target().add_comment(
                before,
                &format!(
                    "Calling function {} : {}",
                    function_call.function_name, function_call.index
                ),
                true,
            );

            self.target().call_function_simple(before, &address_to_call);
        }

        self.restore_stack(&function_call, before, &mut call_parameters);

        after.insert(0, call_parameters.after().join("\n"));

        if inline {
            self.target().add_comment(
                before,
                &format!("end inlining function {}", function_call.function_name),
                true,
            );
        } else {
            self.target().add_comment(
                before,
                &format!("end calling function {}", function_call.function_name),
                true,
            );
        }
        lambda_calls
    }

    fn call_lambda(
        &'a self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        index_in_lambda_space: usize,
    );

    fn restore_stack(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        call_parameters: &mut FUNCTION_CALL_PARAMETERS,
    );

    fn added_to_stack_for_call_parameter(
        &self,
        added_to_stack: &String,
        call_parameters: &FUNCTION_CALL_PARAMETERS,
    ) -> String;

    fn function_call_parameters<'b, 'c>(
        &'a self,
        parameters: &'b Vec<ASTTypedParameterDef>,
        inline: bool,
        immediate: bool,
        stack_vals: &'c StackVals,
        id: usize,
    ) -> FUNCTION_CALL_PARAMETERS;

    fn add_let(
        &self,
        namespace: &ASTNameSpace,
        context: &mut TypedValContext,
        stack: &StackVals,
        after: &mut String,
        before: &mut String,
        name: &str,
        expr: &ASTTypedExpression,
        function_def: Option<&ASTTypedFunctionDef>,
        lambda_space: Option<&LambdaSpace>,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        id: &mut usize,
    ) -> Vec<LambdaCall> {
        let wl = self.backend().word_len();
        let address_relative_to_bp = stack.reserve_local_val(name) * wl;
        let (ast_typed_type, (bf, af, new_lambda_calls), index) = match expr {
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                if let Some(kind) = context.get(&call.function_name) {
                    let typed_type = match kind {
                        TypedValKind::ParameterRef(_, def) => def.ast_type.clone(),
                        TypedValKind::LetRef(_, ast_typed_type) => ast_typed_type.clone(),
                    };

                    let typed_type: ASTTypedType =
                        if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                            parameters: _,
                            return_type,
                        }) = &typed_type
                        {
                            if return_type.deref() != &ASTTypedType::Unit {
                                return_type.deref().clone()
                            } else {
                                panic!(
                                    "Expected a return type from lambda but got None: {}",
                                    call.index
                                );
                            }
                        } else {
                            panic!("Expected lambda but got {typed_type}: {}", call.index);
                        };

                    (
                        typed_type,
                        self.call_function(
                            namespace,
                            call,
                            context,
                            function_def,
                            "0".into(),
                            lambda_space,
                            0,
                            false,
                            stack,
                            id,
                            statics,
                        ),
                        call.index.clone(),
                    )
                } else {
                    (
                        self.module()
                            .functions_by_name
                            .get(&call.function_name.replace("::", "_"))
                            .unwrap()
                            .return_type
                            .clone(),
                        self.call_function(
                            namespace,
                            call,
                            context,
                            function_def,
                            "0".into(),
                            lambda_space,
                            0,
                            false,
                            stack,
                            id,
                            statics,
                        ),
                        call.index.clone(),
                    )
                }
            }
            ASTTypedExpression::Value(value_type, index) => {
                let typed_type =
                    get_type_of_typed_expression(self.module(), context, expr, None, statics)
                        .unwrap();

                self.set_let_for_value(
                    before,
                    name,
                    is_const,
                    statics,
                    body,
                    address_relative_to_bp,
                    value_type,
                    &typed_type,
                );
                (typed_type, (String::new(), vec![], vec![]), index.clone())
            }
            ASTTypedExpression::StringLiteral(value) => {
                let typed_type = ASTTypedType::Builtin(BuiltinTypedTypeKind::String);

                self.set_let_for_string_literal(
                    before,
                    name,
                    is_const,
                    statics,
                    body,
                    address_relative_to_bp,
                    value,
                    &typed_type,
                    stack,
                );
                (
                    typed_type,
                    (String::new(), vec![], vec![]),
                    ASTIndex::none(),
                )
            }
            ASTTypedExpression::ValueRef(val_name, index) => {
                if let Some(typed_val_kind) = context.get(val_name) {
                    let typed_type = self.set_let_for_value_ref(
                        stack,
                        before,
                        address_relative_to_bp,
                        val_name,
                        typed_val_kind,
                    );

                    (typed_type, (String::new(), vec![], vec![]), index.clone())
                } else {
                    panic!("Cannot find {name} in context");
                }
            }

            _ => panic!("Unsupported let {:?}", expr),
        };

        if is_const {
            if !bf.is_empty() {
                body.push_str(&bf);

                let key = statics.add_typed_const(name.to_owned(), ast_typed_type.clone());

                self.set_let_const_for_function_call_result(&key, body);
            }

            if self.options().dereference {
                if let Some(type_name) = get_reference_type_name(&ast_typed_type, self.module()) {
                    let entry = statics.get_typed_const(name).unwrap();

                    self.backend().call_add_ref(
                        body,
                        &format!("[{}]", entry.key),
                        &type_name,
                        &format!("for const {name} : {index}"),
                        self.module(),
                        statics,
                    );
                }
            }
        } else {
            context.insert_let(
                name.into(),
                ast_typed_type.clone(),
                Some(address_relative_to_bp / self.backend().word_len()),
            );

            if !bf.is_empty() {
                before.push_str(&bf);
                self.backend()
                    .store_function_result_in_stack(before, -(address_relative_to_bp as i32));
            }

            if self.options().dereference {
                if let Some(type_name) = get_reference_type_name(&ast_typed_type, self.module()) {
                    self.call_add_ref_for_let_val(
                        &name,
                        &index,
                        before,
                        statics,
                        &address_relative_to_bp,
                        &type_name,
                    );

                    let deref_str = self.call_deref_for_let_val(
                        &name,
                        statics,
                        &address_relative_to_bp,
                        &type_name,
                    );
                    Self::insert_on_top(&deref_str, after);
                }
            }

            let not_empty_after_lines = af
                .into_iter()
                .filter(|it| !it.is_empty())
                .collect::<Vec<String>>();
            Self::insert_on_top(&not_empty_after_lines.join("\n"), after);
        }

        new_lambda_calls
    }

    fn call_deref_for_let_val(
        &self,
        name: &str,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
    ) -> String;

    fn call_add_ref_for_let_val(
        &self,
        name: &str,
        index: &ASTIndex,
        before: &mut String,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
    );

    fn set_let_const_for_function_call_result(&self, statics_key: &str, body: &mut String);

    fn set_let_for_value_ref(
        &self,
        stack: &StackVals,
        before: &mut String,
        address_relative_to_bp: usize,
        val_name: &String,
        typed_val_kind: &TypedValKind,
    ) -> ASTTypedType;

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
    );

    fn set_let_for_value(
        &self,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        address_relative_to_bp: usize,
        value_type: &ValueType,
        typed_type: &ASTTypedType,
    );

    fn add_val(
        &self,
        context: &TypedValContext,
        lambda_space_opt: &Option<&LambdaSpace>,
        indent: &usize,
        call_parameters: &mut FUNCTION_CALL_PARAMETERS,
        param_name: &str,
        val_name: &str,
        error_msg: &str,
        stack_vals: &StackVals,
        ast_index: &ASTIndex,
        statics: &Statics,
    ) {
        if let Some(val_kind) = context.get(val_name) {
            match val_kind {
                TypedValKind::ParameterRef(index, _par) => {
                    call_parameters.add_parameter_ref(
                        param_name.into(),
                        val_name,
                        *index,
                        lambda_space_opt,
                        *indent,
                        stack_vals,
                    );
                }

                TypedValKind::LetRef(_index, _ast_typed_type) => {
                    let index_in_context = stack_vals.find_local_val_relative_to_bp(val_name);

                    call_parameters.add_let_val_ref(
                        param_name.into(),
                        val_name,
                        index_in_context,
                        lambda_space_opt,
                        *indent,
                        stack_vals,
                        ast_index,
                    )
                }
            }
        } else if let Some(entry) = statics.get_typed_const(val_name) {
            call_parameters.add_label(
                param_name,
                entry.key.clone(),
                Some(&format!("static {val_name}")),
            )
        } else {
            panic!("Error adding val {}: {}", param_name, error_msg);
        }
    }

    fn add_function_def(
        &'a self,
        function_def: &ASTTypedFunctionDef,
        lambda_space: Option<&LambdaSpace>,
        parent_context: &TypedValContext,
        indent: usize,
        is_lambda: bool,
        definitions: &mut String,
        id: &mut usize,
        statics: &mut Statics,
        body: &mut String,
    ) -> Vec<LambdaCall> {
        debug!(
            "{}Adding function def {}",
            " ".repeat(indent * 4),
            function_def.name
        );

        let mut lambda_calls = Vec::new();

        if function_def.index.file_name.is_some() {
            self.target()
                .add_comment(definitions, &format!("{}", function_def.index), false);
        }
        self.target()
            .add_comment(definitions, &format!("function {}", function_def), false);
        self.target()
            .add(definitions, &format!("{}:", function_def.name), None, false);

        let mut before = String::new();

        self.backend().function_preamble(definitions);

        let stack = StackVals::new();

        if function_def.return_type.is_unit() {
            stack.reserve_return_register(self.target(), &mut before);
        }

        if is_lambda {
            self.reserve_lambda_space(&mut before, &stack);
        }

        let mut context = TypedValContext::new(Some(parent_context));

        // I think it's useless
        let mut i = if is_lambda {
            // one because the first parameter is the address to the lambda space
            1
        } else {
            0
        };

        for par in function_def.parameters.iter() {
            debug!(
                "{}Inserted parameter {} in context, offset {}",
                " ".repeat((indent + 1) * 4),
                par.name,
                i
            );
            context.insert_par(par.name.clone(), i, par.clone());
            i += 1;
        }

        let mut after = String::new();

        match &function_def.body {
            ASTTypedFunctionBody::RASMBody(calls) => {
                for statement in calls {
                    match statement {
                        ASTTypedStatement::Expression(expr) => {
                            match expr {
                                ASTTypedExpression::ASTFunctionCallExpression(call_expression) => {
                                    let (bf, af, mut lambda_calls_) = self.call_function(
                                        &function_def.namespace,
                                        call_expression,
                                        &context,
                                        Some(function_def),
                                        "0".into(),
                                        lambda_space,
                                        indent + 1,
                                        false,
                                        &stack,
                                        id,
                                        statics,
                                    );

                                    before.push_str(&bf);

                                    Self::insert_on_top(&af.join("\n"), &mut after);

                                    lambda_calls.append(&mut lambda_calls_);
                                }
                                ASTTypedExpression::ValueRef(val, index) => {
                                    // TODO I don't like to use FunctionCallParameters to do this, probably I need another struct to do only the calculation of the address to get

                                    let mut parameters = self.function_call_parameters(
                                        &Vec::new(),
                                        function_def.inline,
                                        true,
                                        &stack,
                                        *id,
                                    );

                                    *id += 1;

                                    self.add_val(
                                        &context,
                                        &lambda_space,
                                        &indent,
                                        &mut parameters,
                                        val,
                                        val,
                                        "",
                                        &stack,
                                        index,
                                        statics,
                                    );

                                    before.push_str(&parameters.before());

                                    Self::insert_on_top(&parameters.after().join("\n"), &mut after);
                                }
                                ASTTypedExpression::StringLiteral(value) => {
                                    self.string_literal_return(statics, &mut before, value);
                                }
                                ASTTypedExpression::Lambda(lambda_def) => {
                                    if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                                        parameters,
                                        return_type,
                                    }) = &function_def.return_type
                                    {
                                        let rt = if return_type.deref() != &ASTTypedType::Unit {
                                            return_type.deref().clone()
                                        } else {
                                            panic!(
                                                "Expected a return type from lambda but got None"
                                            );
                                        };

                                        let lambda_parameters = zip(
                                            lambda_def.parameter_names.iter(),
                                            parameters.iter(),
                                        )
                                        .map(|((name, index), typed_type)| ASTTypedParameterDef {
                                            name: name.clone(),
                                            ast_type: typed_type.clone(),
                                            ast_index: index.clone(),
                                        })
                                        .collect::<Vec<_>>();

                                        let name = format!("lambda{}", id);
                                        let def = ASTTypedFunctionDef {
                                            namespace: function_def.namespace.clone(),
                                            //name: format!("{}_{}_{}_lambda{}", parent_def_description, function_call.function_name, param_name, self.id),
                                            name: name.clone(),
                                            original_name: name,
                                            parameters: lambda_parameters, // parametrs are calculated later
                                            return_type: rt,
                                            body: ASTTypedFunctionBody::RASMBody(
                                                lambda_def.clone().body,
                                            ),
                                            inline: false,
                                            generic_types: LinkedHashMap::new(),
                                            index: lambda_def.index.clone(),
                                        };

                                        *id += 1;

                                        let mut parameters = self.function_call_parameters(
                                            &Vec::new(),
                                            function_def.inline,
                                            true,
                                            &stack,
                                            *id,
                                        );

                                        *id += 1;

                                        let new_lambda_space = parameters.add_lambda(
                                            &def,
                                            lambda_space,
                                            &context,
                                            None,
                                            statics,
                                            self.module(),
                                            &stack,
                                            false,
                                            self.target(),
                                        );

                                        before.push_str(&parameters.before());

                                        Self::insert_on_top(
                                            &parameters.after().join("\n"),
                                            &mut after,
                                        );
                                        lambda_calls.push(LambdaCall {
                                            def,
                                            space: new_lambda_space,
                                        });
                                    } else {
                                        panic!("Expected lambda return type");
                                    }
                                }
                                ASTTypedExpression::Value(value_type, _) => {
                                    let v = self.backend().value_to_string(value_type);

                                    self.value_as_return(&mut before, &v);
                                }
                            }
                        }
                        ASTTypedStatement::LetStatement(name, expr, is_const, _let_index) => {
                            let mut new_lambda_calls = self.add_let(
                                &function_def.namespace,
                                &mut context,
                                &stack,
                                &mut after,
                                &mut before,
                                name,
                                expr,
                                Some(function_def),
                                lambda_space,
                                *is_const,
                                statics,
                                body,
                                id,
                            );
                            lambda_calls.append(&mut new_lambda_calls);
                        }
                    }
                }
            }
            ASTTypedFunctionBody::NativeBody(body) => {
                let function_call_parameters = self.function_call_parameters(
                    &function_def.parameters,
                    false,
                    false,
                    &stack,
                    *id,
                );

                *id += 1;

                let new_body =
                    function_call_parameters.resolve_native_parameters(body, "0".into(), indent);
                before.push_str(&new_body);
            }
        }

        self.backend().reserve_local_vals(&stack, definitions);

        definitions.push_str(&before.replace(
            STACK_VAL_SIZE_NAME,
            &(stack.len_of_all() * self.backend().word_len()).to_string(),
        ));

        definitions.push_str(&after);
        definitions.push('\n');

        self.backend().restore(&stack, definitions);

        self.backend().function_end(definitions, true);

        lambda_calls
    }

    fn reserve_lambda_space(&'a self, before: &mut String, stack: &StackVals);

    fn value_as_return(&self, before: &mut String, v: &str);

    fn string_literal_return(&self, statics: &mut Statics, before: &mut String, value: &String);

    fn functions(&self) -> &LinkedHashMap<String, ASTTypedFunctionDef>;

    fn insert_on_top(src: &str, dest: &mut String) {
        for line in src.lines().rev() {
            dest.insert_str(0, &(line.to_string() + "\n"));
        }
    }

    fn call_function(
        &self,
        namespace: &ASTNameSpace,
        function_call: &ASTTypedFunctionCall,
        context: &TypedValContext,
        parent_def: Option<&ASTTypedFunctionDef>,
        added_to_stack: String,
        lambda_space: Option<&LambdaSpace>,
        indent: usize,
        is_lambda: bool,
        stack_vals: &StackVals,
        id: &mut usize,
        statics: &mut Statics,
    ) -> (String, Vec<String>, Vec<LambdaCall>);

    fn create_lambdas(
        &self,
        lambdas: Vec<LambdaCall>,
        indent: usize,
        id: &mut usize,
        statics: &mut Statics,
    ) -> HashMap<String, (String, String)>;

    fn create_all_functions(
        &self,
        id: &mut usize,
        statics: &mut Statics,
    ) -> HashMap<String, (String, String)>;

    fn translate_body(&self, body: &str, statics: &mut Statics) -> Result<String, String>;

    fn get_used_functions(
        &self,
        functions_asm: &HashMap<String, (String, String)>,
        native_code: &str,
    ) -> Vec<(String, (String, String))>;

    fn print_memory_info(&self, native_code: &mut String);

    fn initialize_static_values(&self, generated_code: &mut String);

    fn debug(&self) -> bool;
}

pub fn get_reference_type_name(
    ast_type: &ASTTypedType,
    type_def_provider: &dyn TypeDefProvider,
) -> Option<String> {
    match ast_type {
        ASTTypedType::Builtin(BuiltinTypedTypeKind::String) => Some("str".into()),
        ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda { .. }) => Some("_fn".into()),
        ASTTypedType::Enum { namespace: _, name } => Some(name.clone()),
        ASTTypedType::Struct { namespace: _, name } => Some(name.clone()),
        ASTTypedType::Type { namespace: _, name } => {
            if let Some(t) = type_def_provider.get_type_def_by_name(name) {
                if t.is_ref {
                    Some(name.clone())
                } else {
                    None
                }
            } else {
                panic!("get_reference_type_name, cannot find type {name}");
            }
        }
        _ => None,
    }
}

pub fn can_optimize_lambda_space(
    lambda_return_type: &ASTTypedType,
    type_def_provider: &dyn TypeDefProvider,
) -> bool {
    let mut already_checked = HashSet::new();
    can_optimize_lambda_space_(lambda_return_type, type_def_provider, &mut already_checked)
}

fn can_optimize_lambda_space_(
    lambda_return_type: &ASTTypedType,
    type_def_provider: &dyn TypeDefProvider,
    already_checked: &mut HashSet<String>,
) -> bool {
    match lambda_return_type {
        ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda { .. }) => false,
        ASTTypedType::Enum { namespace: _, name } => {
            if already_checked.contains(name) {
                return true;
            }

            already_checked.insert(name.to_owned());

            if let Some(e) = type_def_provider.get_enum_def_by_name(name) {
                e.variants
                    .iter()
                    .flat_map(|it| it.parameters.iter())
                    .all(|it| {
                        can_optimize_lambda_space_(&it.ast_type, type_def_provider, already_checked)
                    })
            } else {
                panic!();
            }
        }
        ASTTypedType::Struct { namespace: _, name } => {
            if already_checked.contains(name) {
                return true;
            }

            already_checked.insert(name.to_owned());
            if let Some(s) = type_def_provider.get_struct_def_by_name(name) {
                s.properties.iter().all(|it| {
                    can_optimize_lambda_space_(&it.ast_type, type_def_provider, already_checked)
                })
            } else {
                panic!()
            }
        }
        _ => true,
    }
}

impl CodeGenAsm {
    pub fn new(
        module: ASTTypedModule,
        backend: Box<impl BackendAsm + 'static>,
        options: CodeGenOptions,
        target: CompileTarget,
        debug: bool,
    ) -> Self {
        crate::utils::debug_indent::INDENT.with(|indent| {
            *indent.borrow_mut() = 0;
        });

        Self {
            module,
            backend,
            options,
            target,
            debug,
        }
    }
}

impl<'a> CodeGen<'a, Box<dyn BackendAsm>, Box<dyn FunctionCallParametersAsm + 'a>> for CodeGenAsm {
    fn backend(&self) -> &Box<dyn BackendAsm> {
        &self.backend
    }

    fn target(&self) -> &CompileTarget {
        &self.target
    }

    fn options(&self) -> &CodeGenOptions {
        &self.options
    }

    fn module(&self) -> &ASTTypedModule {
        &self.module
    }

    fn create_command_line_arguments(&self, generated_code: &mut String, debug: bool) {
        self.target().call_function(
            generated_code,
            "createCmdLineArguments",
            &[
                ("_rasm_args", None),
                (
                    &self.backend().stack_pointer(),
                    Some("command line arguments"),
                ),
            ],
            None,
            debug,
        );
    }

    fn translate_static_code(&self, static_code: &String, debug: bool) -> String {
        let mut temp_statics = Statics::new();
        let evaluator = self.target().get_evaluator(debug);

        let code = evaluator
            .translate(
                self.target(),
                &mut temp_statics,
                None,
                None,
                &static_code,
                self.options().dereference,
                false,
                self.module(),
            )
            .unwrap();
        code
    }

    fn call_lambda_parameter(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        kind: &TypedValKind,
    ) {
        let rr = self.backend.return_register();

        let index = match kind {
            TypedValKind::ParameterRef(index, _) => *index as i32 + 2,
            TypedValKind::LetRef(_, _) => {
                let relative_to_bp_found = stack_vals
                    .find_local_val_relative_to_bp(&function_call.function_name)
                    .unwrap();
                -(relative_to_bp_found as i32)
            }
        };

        self.target.add_comment(
            before,
            &format!(
                "calling lambda parameter reference to {}",
                &function_call.function_name
            ),
            true,
        );

        self.target.add(
            before,
            &format!(
                "mov {rr}, [{} + {}]",
                self.backend().stack_base_pointer(),
                index * self.backend().word_len() as i32
            ),
            None,
            true,
        );
        self.target.add(
            before,
            &format!("mov {} {rr}, [{rr}]", self.backend.word_size(),),
            None,
            true,
        );
        // we add the address to the "lambda space" as the last parameter to the lambda
        self.target.call_function(
            before,
            &format!("[{rr}]"),
            &[(rr, Some("address to the \"lambda space\""))],
            Some(&format!(
                "Calling function {} : {}",
                function_call.function_name, function_call.index
            )),
            self.debug(),
        );
    }

    fn call_lambda(
        &'a self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        index_in_lambda_space: usize,
    ) {
        let rr = self.backend.return_register();

        if let Some(ref address) = stack_vals.find_tmp_register("lambda_space_address") {
            self.target()
                .add(before, &format!("mov {rr}, {address}"), None, true);
        } else {
            panic!()
        }
        // we add the address to the "lambda space" as the last parameter of the lambda
        self.target.add(
            before,
            &format!(
                "add {rr}, {}",
                (index_in_lambda_space + 2) * self.backend().word_len()
            ),
            Some("address to the pointer to the allocation table of the lambda to call"),
            true,
        );
        self.target.add(
            before,
            &format!("mov {rr}, [{rr}]"),
            Some("address of the allocation table of the function to call"),
            true,
        );
        self.target.add(
            before,
            &format!("mov {rr}, [{rr}]"),
            Some("address to the \"lambda space\" of the function to call"),
            true,
        );

        self.target.call_function(
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
            self.debug,
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

            self.target.add(
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
            self.target.clone(),
        );

        Box::new(fcp)
    }

    fn call_deref_for_let_val(
        &self,
        name: &str,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
    ) -> String {
        let bp = self.backend.stack_base_pointer();
        self.backend.call_deref(
            &format!("[{bp} - {}]", address_relative_to_bp),
            &type_name,
            &format!("for let val {name}"),
            self.module(),
            statics,
        )
    }

    fn call_add_ref_for_let_val(
        &self,
        name: &str,
        index: &ASTIndex,
        before: &mut String,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
    ) {
        let bp = self.backend.stack_base_pointer();
        self.backend.call_add_ref(
            before,
            &format!("[{bp} - {}]", address_relative_to_bp),
            &type_name,
            &format!("for let val {name} : {index}"),
            self.module(),
            statics,
        );
    }

    fn set_let_const_for_function_call_result(&self, statics_key: &str, body: &mut String) {
        let ws = self.backend.word_size();
        let rr = self.backend.return_register();
        self.target.add(
            body,
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
    ) -> ASTTypedType {
        let ws = self.backend.word_size();
        let bp = self.backend.stack_base_pointer();
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

        let tmp_register = stack.reserve_tmp_register(
            before,
            self.backend.deref(),
            "set_let_for_value_ref",
            &self.target,
        );

        self.target.add(
            before,
            &format!(
                "mov {tmp_register}, [{} + {}]",
                self.backend().stack_base_pointer(),
                i * self.backend().word_len() as i32
            ),
            Some(&format!("let reference to {descr}")),
            true,
        );

        self.target.add(
            before,
            &format!(
                "mov {ws} [{bp} + {}], {tmp_register}",
                -(address_relative_to_bp as i32),
            ),
            Some(""),
            true,
        );

        stack.release_tmp_register(&self.target, before, "set_let_for_value_ref");
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
        let label = statics.add_str(value);

        let tmp_reg = stack.reserve_tmp_register(
            body,
            self.backend.deref(),
            "set_let_for_string_literal",
            self.target(),
        );

        if is_const {
            let key = statics.add_typed_const(name.to_owned(), typed_type.clone());

            self.backend.indirect_mov(
                body,
                &label,
                &key,
                &tmp_reg,
                Some(&format!("const {name} string value")),
            );
        } else {
            self.backend.indirect_mov(
                before,
                &label,
                &format!("{bp} + {}", -(address_relative_to_bp as i32),),
                &tmp_reg,
                None,
            );
        }

        stack.release_tmp_register(&self.target, body, "set_let_for_string_literal");
    }

    fn set_let_for_value(
        &self,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        address_relative_to_bp: usize,
        value_type: &ValueType,
        typed_type: &ASTTypedType,
    ) {
        let bp = self.backend().stack_base_pointer();
        let ws = self.backend().word_size();
        let value = self.backend().value_to_string(value_type);

        if is_const {
            let key = statics.add_typed_const(name.to_owned(), typed_type.clone());

            self.target.add(
                body,
                &format!("mov {ws} [{key}], {}", value),
                Some(""),
                true,
            );
        } else {
            self.target.add(
                before,
                &format!(
                    "mov {ws} [{bp} + {}], {}",
                    -(address_relative_to_bp as i32),
                    value
                ),
                Some(""),
                true,
            );
        }
    }

    fn reserve_lambda_space(&'a self, before: &mut String, stack: &StackVals) {
        let register = stack.reserve_tmp_register(
            before,
            &self.backend,
            "lambda_space_address",
            self.target(),
        );

        self.target.add(
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

    fn value_as_return(&self, before: &mut String, v: &str) {
        let ws = self.backend.word_size();
        let rr = self.backend.return_register();
        self.target()
            .add(before, &format!("mov     {ws} {rr}, {v}"), None, true);
    }

    fn string_literal_return(&self, statics: &mut Statics, before: &mut String, value: &String) {
        let label = statics.add_str(value);
        let rr = self.backend.return_register();

        self.target.add(
            before,
            &format!("mov     {} {rr}, [{label}]", self.backend().word_size()),
            None,
            true,
        );
    }

    fn functions(&self) -> &LinkedHashMap<String, ASTTypedFunctionDef> {
        &self.module().functions_by_name
    }

    fn call_function(
        &self,
        namespace: &ASTNameSpace,
        function_call: &ASTTypedFunctionCall,
        context: &TypedValContext,
        parent_def: Option<&ASTTypedFunctionDef>,
        added_to_stack: String,
        lambda_space: Option<&LambdaSpace>,
        indent: usize,
        is_lambda: bool,
        stack_vals: &StackVals,
        id: &mut usize,
        statics: &mut Statics,
    ) -> (String, Vec<String>, Vec<LambdaCall>) {
        let mut before = String::new();
        let mut after = Vec::new();

        let lambda_calls = if let Some(function_def) =
            self.functions().get(&function_call.function_name)
        {
            let def = function_def.clone();
            // sometimes the function name is different from the function definition name, because it is not a valid ASM name (for enum types is enu-name::enum-variant)
            let real_function_name = self
                .functions()
                .get(&function_call.function_name)
                .unwrap()
                .clone()
                .name;
            debug!(
                "{}Calling function {} context {:?}, lambda_space: {:?}",
                " ".repeat(indent * 4),
                function_call.function_name,
                context.names(),
                lambda_space
            );
            self.call_function_(
                namespace,
                &function_call,
                context,
                &parent_def,
                added_to_stack,
                &mut before,
                def.parameters,
                def.inline,
                Some(def.body),
                real_function_name,
                lambda_space,
                indent,
                is_lambda,
                stack_vals,
                &mut after,
                id,
                statics,
            )
        } else if let Some(function_def) = self
            .functions()
            .get(&function_call.function_name.replace("::", "_"))
        {
            let def = function_def.clone();
            // sometimes the function name is different from the function definition name, because it is not a valid ASM name (for enum types is enu-name::enum-variant)
            let real_function_name = self
                .functions()
                .get(&function_call.function_name.replace("::", "_"))
                .unwrap()
                .clone()
                .name;
            debug!(
                "{}Calling function {} context {:?}, lambda_space: {:?}",
                " ".repeat(indent * 4),
                function_call.function_name,
                context.names(),
                lambda_space
            );
            self.call_function_(
                namespace,
                &function_call,
                context,
                &parent_def,
                added_to_stack,
                &mut before,
                def.parameters,
                def.inline,
                Some(def.body),
                real_function_name,
                lambda_space,
                indent,
                is_lambda,
                stack_vals,
                &mut after,
                id,
                statics,
            )
        } else if let Some(kind) = context.get(&function_call.function_name) {
            let (index, ast_type) = match kind {
                TypedValKind::ParameterRef(index, par) => (*index as i32 + 2, par.ast_type.clone()),
                TypedValKind::LetRef(index, ast_type) => {
                    // TODO I think it's not really needed because every index I put here, it works...
                    let index_relative_to_bp = match stack_vals
                        .find_local_val_relative_to_bp(&function_call.function_name)
                    {
                        None => *index as i32 + 2,
                        Some(index_in_stack) => -(index_in_stack as i32),
                    };
                    (index_relative_to_bp, ast_type.clone())
                }
            };

            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                return_type: _,
                parameters,
            }) = &ast_type
            {
                let wl = self.backend.word_len();
                let bp = self.backend.stack_base_pointer();

                let parameters_defs = parameters
                    .iter()
                    .map(|it| {
                        let name = format!("p_{}", id);
                        *id += 1;
                        ASTTypedParameterDef {
                            name,
                            ast_type: it.clone(),
                            ast_index: ASTIndex::none(), // TODO I don't know...
                        }
                    })
                    .collect();

                debug!(
                    "{}Calling lambda {}",
                    " ".repeat(indent * 4),
                    function_call.function_name
                );
                debug!(
                    "{}parameters {:?}",
                    " ".repeat((indent + 1) * 4),
                    parameters
                );
                debug!("{}context {:?}", " ".repeat((indent + 1) * 4), context);
                debug!("{}index {}", " ".repeat((indent + 1) * 4), index);
                debug!(
                    "{}function_call.parameters {:?}",
                    " ".repeat((indent + 1) * 4),
                    function_call.parameters
                );
                debug!(
                    "{}parameters_defs {:?}",
                    " ".repeat((indent + 1) * 4),
                    parameters_defs
                );
                debug!(
                    "{}lambda_space {:?}",
                    " ".repeat((indent + 1) * 4),
                    lambda_space
                );

                self.call_function_(
                    namespace,
                    &function_call,
                    context,
                    &parent_def,
                    added_to_stack,
                    &mut before,
                    parameters_defs,
                    false,
                    None,
                    format!("[{}+{}]", bp, index * wl as i32),
                    lambda_space,
                    indent,
                    true,
                    stack_vals,
                    &mut after,
                    id,
                    statics,
                )
            } else {
                panic!("Cannot find function, there's a parameter with name '{}', but it's not a lambda", function_call.function_name);
            }
        } else {
            panic!(
                "Cannot find function {} in {}",
                function_call.function_name,
                OptionDisplay(&parent_def.map(|it| &it.name))
            );
        };

        (before, after, lambda_calls)
    }

    fn create_lambdas(
        &self,
        lambdas: Vec<LambdaCall>,
        indent: usize,
        id: &mut usize,
        statics: &mut Statics,
    ) -> HashMap<String, (String, String)> {
        let mut result = HashMap::new();

        let mut lambda_calls = Vec::new();
        for lambda_call in lambdas {
            let mut definitions = String::new();
            let mut body = String::new();

            //debug!("Creating lambda {}", lambda_call.def.name);
            lambda_calls.append(&mut self.add_function_def(
                &lambda_call.def,
                Some(&lambda_call.space),
                lambda_call.space.get_context(),
                indent,
                true,
                &mut definitions,
                id,
                statics,
                &mut body,
            ));

            result.insert(lambda_call.def.name.clone(), (definitions, body));

            //Parser::print_function_def(&lambda_call.def);
            lambda_calls.extend(
                lambda_call
                    .space
                    .get_ref_functions()
                    .iter()
                    .flat_map(|it| {
                        let mut definitions = String::new();
                        let mut body = String::new();

                        let ls = self.add_function_def(
                            it,
                            None,
                            lambda_call.space.get_context(),
                            indent,
                            false,
                            &mut definitions,
                            id,
                            statics,
                            &mut body,
                        );

                        result.insert(it.name.clone(), (definitions, body));

                        ls
                    })
                    .collect::<Vec<_>>(),
            );
        }
        if !lambda_calls.is_empty() {
            result.extend(self.create_lambdas(lambda_calls, indent + 1, id, statics));
        }

        result
    }

    fn create_all_functions(
        &self,
        id: &mut usize,
        statics: &mut Statics,
    ) -> HashMap<String, (String, String)> {
        let mut result = HashMap::new();

        for function_def in self.module.functions_by_name.values() {
            // ValContext ???
            if !function_def.inline {
                let mut definitions = String::new();
                let mut body = String::new();

                let lambda_calls = self.add_function_def(
                    function_def,
                    None,
                    &TypedValContext::new(None),
                    0,
                    false,
                    &mut definitions,
                    id,
                    statics,
                    &mut body,
                );
                result.extend(self.create_lambdas(lambda_calls, 0, id, statics));

                result.insert(function_def.name.clone(), (definitions, body));
            }
        }

        result
    }

    fn translate_body(&self, body: &str, statics: &mut Statics) -> Result<String, String> {
        let val_context = ValContext::new(None);

        let evaluator = self.target.get_evaluator(self.debug);

        let new_body = evaluator.translate(
            self.target(),
            statics,
            None,
            None,
            body,
            self.options.dereference,
            true,
            &self.module,
        )?;

        let result = evaluator.translate(
            self.target(),
            statics,
            None,
            None,
            &new_body,
            self.options.dereference,
            false,
            &self.module,
        )?;

        let mut lines: Vec<String> = result.lines().map(|it| it.to_owned()).collect::<Vec<_>>();

        self.target
            .called_functions(
                None,
                None,
                &result,
                &val_context,
                &self.module,
                statics,
                self.debug,
            )?
            .iter()
            .for_each(|(m, it)| {
                debug_i!("native call to {:?}, in main", it);
                if let Some(new_function_def) = self.module.functions_by_name.get(&it.name) {
                    debug_i!("converted to {new_function_def}");
                    if it.name != new_function_def.name {
                        lines[it.i] = get_new_native_call(m, &new_function_def.name);
                    }
                } else {
                    // panic!("cannot find call {function_call}");
                    // TODO I hope it is a predefined function like addRef or deref for a tstruct or enum
                    println!("translate_body: cannot find call to {}", it.name);
                }
            });

        let result = lines.join("\n");

        Ok(result)
    }

    fn get_used_functions(
        &self,
        functions_native_code: &HashMap<String, (String, String)>,
        native_code: &str,
    ) -> Vec<(String, (String, String))> {
        let result = if self.options.optimize_unused_functions {
            let mut used_functions = UsedFunctions::find(&self.module);
            // those are probably lambdas
            used_functions.extend(
                functions_native_code
                    .keys()
                    .filter(|it| !self.module.functions_by_name.contains_key(*it))
                    .cloned()
                    .collect::<Vec<_>>(),
            );

            used_functions.extend(UsedFunctions::get_used_functions(native_code));

            let mut used_functions_in_defs = HashSet::new();

            for (_name, (defs, _bd)) in functions_native_code
                .iter()
                .filter(|(it, (_, _))| used_functions.contains(*it))
            {
                used_functions_in_defs.extend(UsedFunctions::get_used_functions(defs));
            }

            used_functions.extend(used_functions_in_defs);

            functions_native_code
                .iter()
                .filter(|(it, (_, _))| {
                    let valid = used_functions.contains(*it);
                    if !valid {
                        debug!("unused function {it}");
                    }
                    valid
                })
                .collect::<Vec<_>>()
        } else {
            functions_native_code.iter().collect::<Vec<_>>()
        };
        result
            .into_iter()
            .map(|(a1, (a2, a3))| (a1.clone(), (a2.clone(), a3.clone())))
            .collect::<Vec<_>>()
    }

    fn print_memory_info(&self, native_code: &mut String) {
        self.target
            .call_function_simple(native_code, "printAllocated");
        self.target
            .call_function_simple(native_code, "printTableSlotsAllocated");
    }

    fn initialize_static_values(&self, native_code: &mut String) {
        let ws = self.backend.word_size();
        self.target.add_rows(
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
}

#[cfg(test)]
mod tests {
    use crate::codegen::compile_target::CompileTarget;
    use crate::codegen::statics::Statics;
    use crate::codegen::typedef_provider::DummyTypeDefProvider;
    use crate::codegen::val_context::ValContext;
    use crate::codegen::CodeGenOptions;

    #[test]
    fn called_functions_in_comment() {
        let sut = CompileTarget::Nasmi386(CodeGenOptions::default());
        let mut statics = Statics::new();

        assert!(sut
            .called_functions(
                None,
                None,
                "mov    eax, 1; $call(something)",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
                &mut statics,
                false
            )
            .unwrap()
            .is_empty());
    }

    #[test]
    fn called_functions_external() {
        let sut = CompileTarget::Nasmi386(CodeGenOptions::default());
        let mut statics = Statics::new();

        assert!(sut
            .called_functions(
                None,
                None,
                "call something",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
                &mut statics,
                false
            )
            .unwrap()
            .is_empty());
    }

    #[test]
    fn called_functions() {
        let sut = CompileTarget::Nasmi386(CodeGenOptions::default());
        let mut statics = Statics::new();

        assert_eq!(
            sut.called_functions(
                None,
                None,
                "$call(something)",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
                &mut statics,
                false
            )
            .unwrap()
            .get(0)
            .unwrap()
            .1
            .name,
            "something".to_string()
        );
    }
}
