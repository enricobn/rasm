use std::collections::HashSet;
use std::env;
use std::iter::zip;
use std::ops::Deref;
use std::sync::atomic::{AtomicUsize, Ordering};

use linked_hash_map::LinkedHashMap;
use log::debug;
use pad::PadStr;

use enhanced_module::EnhancedASTModule;
use lambda::{LambdaCall, LambdaSpace};

use crate::codegen::backend::{Backend, BackendAsm, BackendNasmi386};
use crate::codegen::c::any::CStructs;
use crate::codegen::code_manipulator::{CodeManipulator, CodeManipulatorNasm};
use crate::codegen::compile_target::CompileTarget;
use crate::codegen::function_call_parameters::{
    FunctionCallParameters, FunctionCallParametersAsm, FunctionCallParametersAsmImpl,
};
use crate::codegen::stack::{StackEntryType, StackVals};
use crate::codegen::statics::MemoryUnit::{Bytes, Words};
use crate::codegen::statics::MemoryValue::{I32Value, Mem};
use crate::codegen::statics::{MemoryUnit, MemoryValue, Statics};
use crate::codegen::text_macro::{
    AddRefMacro, AsmCCallTextMacroEvaluator, AsmCallTextMacroEvaluator, MacroParam, PrintRefMacro,
    RefType, TextMacro, TextMacroEval, TextMacroEvaluator,
};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::val_context::{TypedValContext, ValContext};
use crate::debug_i;
use crate::errors::CompilationError;
use crate::parser::ast::{
    ASTFunctionDef, ASTIndex, ASTNameSpace, ASTParameterDef, ASTType, BuiltinTypeKind, ValueType,
};
use crate::transformations::typed_functions_creator::{
    enum_has_references, struct_has_references, type_has_references,
};
use crate::type_check::get_new_native_call;
use crate::type_check::typed_ast::{
    convert_to_typed_module, get_type_of_typed_expression, ASTTypedExpression,
    ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
    DefaultFunctionCall,
};
use crate::type_check::used_functions::UsedFunctions;
use crate::utils::OptionDisplay;

pub mod backend;
pub mod c;
mod code_manipulator;
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

impl TypedValKind {
    pub fn typed_type(&self) -> &ASTTypedType {
        match self {
            TypedValKind::ParameterRef(_index, par) => &par.ast_type,
            TypedValKind::LetRef(_index, ast_type) => ast_type,
        }
    }
}

pub fn get_typed_module(
    module: EnhancedASTModule,
    print_memory_info: bool,
    print_module: bool,
    statics: &mut Statics,
    target: &CompileTarget,
    debug: bool,
) -> Result<ASTTypedModule, CompilationError> {
    let mandatory_functions = target.get_mandatory_functions(&module);
    let default_functions = target.get_default_functions(print_memory_info);

    convert_to_typed_module(
        &module,
        print_module,
        mandatory_functions,
        statics,
        default_functions,
        target,
        debug,
    )
}

pub fn get_std_lib_path() -> String {
    let current_dir = env::current_dir().unwrap();
    env::var("RASM_STDLIB").unwrap_or(current_dir.join("stdlib").to_str().unwrap().to_owned())
}

pub trait CodeGen<'a, FUNCTION_CALL_PARAMETERS: FunctionCallParameters> {
    fn options(&self) -> &AsmOptions;

    fn generate(&'a self, typed_module: &ASTTypedModule, statics: Statics) -> String {
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

        let len = typed_module.body.len();

        for (i, statement) in typed_module.body.iter().enumerate() {
            match statement {
                ASTTypedStatement::Expression(e) => match e {
                    ASTTypedExpression::ASTFunctionCallExpression(call) => {
                        let (bf, cur, af, mut lambda_calls) = self.generate_call_function(
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
                            typed_module,
                            i == len - 1,
                            false,
                        );
                        before.push_str(&bf);
                        before.push_str(&cur);

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
                        typed_module,
                    );
                    lambdas.append(&mut new_lambda_calls);
                }
            }
        }

        body.push_str(&self.transform_before(&stack, before));
        body.push_str(&after);

        // debug!("stack {:?}", stack);
        //        assert_eq!(stack.size(), 0);

        // TODO add a command line argument
        //Parser::print(&self.module);

        let mut functions_generated_code =
            self.create_lambdas(lambdas, 0, &mut id, &mut statics, typed_module);

        functions_generated_code.extend(self.create_all_functions(
            &mut id,
            &mut statics,
            typed_module,
        ));

        let mut generated_code = String::new();

        self.preamble(&mut generated_code);

        self.add_statics(&mut statics);

        let (static_declarations, static_code) = self.generate_statics_code(&statics, typed_module);

        generated_code.push_str(&static_declarations);

        if debug {
            self.define_debug(&mut generated_code);
        }

        self.initialize_static_values(&mut generated_code);

        let code = self.translate_static_code(static_code, typed_module);

        self.add(&mut generated_code, &code, None, true);

        self.create_command_line_arguments(&mut generated_code);

        self.add(&mut generated_code, "", None, true);

        self.function_preamble(&mut generated_code);

        self.reserve_local_vals(&stack, &mut generated_code);

        // probably there is not a valid body from functions
        for (_name, (_defs, bd)) in functions_generated_code.iter() {
            body.push_str(bd);
        }

        let new_body = self
            .translate_body(&body, &mut statics, typed_module)
            .unwrap();

        generated_code.push_str(&new_body);
        generated_code.push('\n');

        self.restore(&stack, &mut generated_code);

        if self.options().print_memory {
            self.print_memory_info(&mut generated_code, &statics);
        }

        self.end_main(&mut generated_code);

        self.function_end(&mut generated_code, false, None);

        let used_functions =
            self.get_used_functions(&functions_generated_code, &generated_code, typed_module);

        for (_, (defs, _bd)) in used_functions {
            generated_code.push_str(&defs);
        }

        generated_code
    }

    fn end_main(&self, code: &mut String);

    fn transform_before(&self, stack: &StackVals, before: String) -> String;

    fn create_command_line_arguments(&self, generated_code: &mut String);

    fn translate_static_code(&self, static_code: String, typed_module: &ASTTypedModule) -> String {
        let mut temp_statics = Statics::new();
        let evaluator = self.get_text_macro_evaluator();

        evaluator
            .translate(
                &mut temp_statics,
                None,
                None,
                &static_code,
                false,
                typed_module,
            )
            .unwrap()
    }

    fn call_lambda_parameter(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        kind: &TypedValKind,
        call_parameters: &FUNCTION_CALL_PARAMETERS,
        return_value: bool,
        is_inner_call: bool,
        statics: &Statics,
    );

    fn call_function_(
        &'a self,
        namespace: &ASTNameSpace,
        function_call: &&ASTTypedFunctionCall,
        context: &TypedValContext,
        parent_def: &Option<&ASTTypedFunctionDef>,
        added_to_stack: String,
        before: &mut String,
        current: &mut String,
        parameters: Vec<ASTTypedParameterDef>,
        inline: bool,
        body: Option<ASTTypedFunctionBody>,
        function_to_call: String,
        lambda_space_opt: Option<&LambdaSpace>,
        indent: usize,
        is_lambda: bool,
        stack_vals: &StackVals,
        after: &mut Vec<String>,
        id: &mut usize,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
        is_last: bool,
        is_inner_call: bool,
    ) -> Vec<LambdaCall> {
        let mut lambda_calls = Vec::new();

        self.add_empty_line(before);

        if self.debug() {
            if inline {
                self.add_comment(
                    before,
                    &format!(
                        "inlining function {}, added to stack {}",
                        function_call.function_name, added_to_stack
                    ),
                    true,
                );
            } else {
                self.add_comment(
                    before,
                    &format!(
                        "calling function {}, added to stack {}",
                        function_call.function_name, added_to_stack
                    ),
                    true,
                );
            }
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
                        call_parameters.add_string_constant(&param_name, value, None, statics);
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

                        let (bf, cur, af, mut inner_lambda_calls) = self.generate_call_function(
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
                            typed_module,
                            false,
                            true,
                        );

                        call_parameters.add_on_top_of_after(&af.join("\n"));

                        call_parameters.add_function_call(
                            typed_module,
                            &format!("{param_name} = {} : {}", &call.function_name, call.index),
                            param_type.clone(),
                            statics,
                            param_name,
                            bf,
                            cur,
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
                            typed_module,
                        );
                    }
                    ASTTypedExpression::Lambda(lambda_def) => {
                        let (return_type, parameters_types) =
                            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                                return_type,
                                parameters,
                            }) = &param_type
                            {
                                (return_type, parameters)
                            } else {
                                panic!("Parameter is not a lambda: {:?}", param_type);
                            };

                        if parameters_types.len() != lambda_def.parameter_names.len() {
                            panic!("Lambda parameters do not match definition");
                        }

                        let rt = return_type.deref().clone();

                        let name = format!("lambda_{}", id);
                        let mut def = ASTTypedFunctionDef {
                            namespace: function_call.namespace.clone(),
                            //name: format!("{}_{}_{}_lambda{}", parent_def_description, function_call.function_name, param_name, self.id),
                            name: name.clone(),
                            original_name: name.clone(),
                            parameters: Vec::new(), // parametrs are calculated later
                            return_type: rt,
                            body: ASTTypedFunctionBody::RASMBody(lambda_def.body.clone()),
                            inline: false,
                            generic_types: LinkedHashMap::new(),
                            index: lambda_def.index.clone(),
                        };

                        *id += 1;

                        debug!("{}Adding lambda {}", " ".repeat(indent * 4), param_name);

                        let function_def = typed_module
                            .functions_by_name
                            .get(&function_call.function_name)
                            .unwrap();

                        let optimize = function_def.return_type.is_unit()
                            || can_optimize_lambda_space(&function_def.return_type, typed_module);

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

                        let lambda_space = call_parameters.add_lambda(
                            &mut def,
                            lambda_space_opt,
                            context,
                            None,
                            statics,
                            typed_module,
                            stack_vals,
                            optimize,
                            function_def,
                            &param_type,
                            &name,
                            param_index,
                        );

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
                current.push_str(
                    &call_parameters.resolve_native_parameters(
                        body,
                        added_to_stack,
                        indent,
                        is_last
                            && parent_def
                                .map(|it| it.return_type != ASTTypedType::Unit)
                                .unwrap_or(false),
                        is_inner_call,
                        parent_def.map(|it| it.return_type.clone()).as_ref(),
                        statics,
                    ),
                );
                current.push('\n');
            } else {
                panic!("Only native  can be inlined, for now...");
            }
        } else if is_lambda {
            if let Some(index_in_lambda_space) =
                lambda_space_opt.and_then(|it| it.get_index(&function_call.function_name))
            {
                self.call_lambda(
                    function_call,
                    current,
                    stack_vals,
                    index_in_lambda_space,
                    &call_parameters,
                    lambda_space_opt
                        .unwrap()
                        .get_type(&function_call.function_name)
                        .unwrap(),
                    statics,
                    is_last
                        && parent_def
                            .map(|it| it.return_type != ASTTypedType::Unit)
                            .unwrap_or(false),
                    is_inner_call,
                );
            } else if let Some(kind) = context.get(&function_call.function_name) {
                self.call_lambda_parameter(
                    function_call,
                    current,
                    stack_vals,
                    kind,
                    &call_parameters,
                    is_last
                        && parent_def
                            .map(|it| it.return_type != ASTTypedType::Unit)
                            .unwrap_or(false),
                    is_inner_call,
                    statics,
                );
            } else {
                panic!(
                    "lambda space does not contain {}",
                    function_call.function_name
                );
            }
        } else {
            if self.debug() {
                self.add_comment(
                    before,
                    &format!(
                        "Calling function {} : {}",
                        function_call.function_name, function_call.index
                    ),
                    true,
                );
            }

            self.call_function_simple(
                current,
                &function_to_call,
                Some(&call_parameters),
                is_last
                    && parent_def
                        .map(|it| it.return_type != ASTTypedType::Unit)
                        .unwrap_or(false),
                is_inner_call,
                parent_def.map(|it| it.return_type.clone()).as_ref(),
                statics,
            );
        }

        self.restore_stack(function_call, current, &mut call_parameters);

        after.insert(0, call_parameters.after().join("\n"));

        if self.debug() {
            if inline {
                self.add_comment(
                    before,
                    &format!("end inlining function {}", function_call.function_name),
                    true,
                );
            } else {
                self.add_comment(
                    before,
                    &format!("end calling function {}", function_call.function_name),
                    true,
                );
            }
        }

        lambda_calls
    }

    fn call_lambda(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        index_in_lambda_space: usize,
        call_parameters: &FUNCTION_CALL_PARAMETERS,
        ast_type_type: &ASTTypedType,
        statics: &Statics,
        return_value: bool,
        is_inner_call: bool,
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
        &'a self,
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
        typed_module: &ASTTypedModule,
    ) -> Vec<LambdaCall> {
        let address_relative_to_bp = stack.reserve_local_val(name);

        let (ast_typed_type, (bf, mut cur, af, new_lambda_calls), index) = match expr {
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                if let Some(kind) = context.get(&call.function_name) {
                    let typed_type = kind.typed_type();

                    let typed_type: ASTTypedType =
                        if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                            parameters: _,
                            return_type,
                        }) = typed_type
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
                        self.generate_call_function(
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
                            typed_module,
                            false,
                            false,
                        ),
                        call.index.clone(),
                    )
                } else {
                    (
                        typed_module
                            .functions_by_name
                            .get(&call.function_name.replace("::", "_"))
                            .unwrap()
                            .return_type
                            .clone(),
                        self.generate_call_function(
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
                            typed_module,
                            false,
                            false,
                        ),
                        call.index.clone(),
                    )
                }
            }
            ASTTypedExpression::Value(value_type, index) => {
                let typed_type =
                    get_type_of_typed_expression(typed_module, context, expr, None, statics)
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
                (
                    typed_type,
                    (String::new(), String::new(), vec![], vec![]),
                    index.clone(),
                )
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
                    (String::new(), String::new(), vec![], vec![]),
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
                        statics,
                        name,
                    );

                    (
                        typed_type,
                        (String::new(), String::new(), vec![], vec![]),
                        index.clone(),
                    )
                } else {
                    panic!("Cannot find {name} in context");
                }
            }

            _ => panic!("Unsupported let {:?}", expr),
        };

        if is_const {
            if !bf.is_empty() || !cur.is_empty() {
                body.push_str(&bf);

                let key = statics.add_typed_const(name.to_owned(), ast_typed_type.clone());

                self.set_let_const_for_function_call_result(
                    &key,
                    body,
                    &mut cur,
                    name,
                    &ast_typed_type,
                    statics,
                );
                body.push_str(&cur);
            }

            if self.options().dereference {
                if let Some(type_name) = get_reference_type_name(&ast_typed_type, typed_module) {
                    self.add_ref(&name, statics, body, typed_module, &index, &type_name);
                }
            }
        } else {
            context.insert_let(
                name.into(),
                ast_typed_type.clone(),
                Some(address_relative_to_bp),
            );

            if !bf.is_empty() || !cur.is_empty() {
                let typed_type =
                    get_type_of_typed_expression(typed_module, context, expr, None, statics)
                        .unwrap();

                self.store_function_result_in_stack(
                    &mut cur,
                    -(address_relative_to_bp as i32),
                    name,
                    &typed_type,
                    statics,
                );
                before.push_str(&bf);
                before.push_str(&cur);
            }

            let not_empty_after_lines = af
                .into_iter()
                .filter(|it| !it.is_empty())
                .collect::<Vec<String>>();
            Self::insert_on_top(&not_empty_after_lines.join("\n"), after);

            if self.options().dereference {
                if let Some(type_name) = get_reference_type_name(&ast_typed_type, typed_module) {
                    self.call_add_ref_for_let_val(
                        name,
                        &index,
                        before,
                        statics,
                        &address_relative_to_bp,
                        &type_name,
                        typed_module,
                    );

                    let deref_str = self.call_deref_for_let_val(
                        name,
                        statics,
                        &address_relative_to_bp,
                        &type_name,
                        typed_module,
                    );
                    Self::insert_on_top(&deref_str, after);
                }
            }
        }

        new_lambda_calls
    }

    fn store_function_result_in_stack(
        &self,
        code: &mut String,
        address_relative_to_bp: i32,
        name: &str,
        typed_type: &ASTTypedType,
        statics: &Statics,
    );

    fn add_ref(
        &self,
        name: &str,
        statics: &mut Statics,
        body: &mut String,
        typed_module: &ASTTypedModule,
        index: &ASTIndex,
        type_name: &String,
    );

    fn call_deref_for_let_val(
        &self,
        name: &str,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
    ) -> String;

    fn call_add_ref_for_let_val(
        &self,
        name: &str,
        index: &ASTIndex,
        before: &mut String,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
    );

    fn set_let_const_for_function_call_result(
        &self,
        statics_key: &str,
        before: &mut String,
        current: &mut String,
        name: &str,
        typed_type: &ASTTypedType,
        statics: &mut Statics,
    );

    fn set_let_for_value_ref(
        &self,
        stack: &StackVals,
        before: &mut String,
        address_relative_to_bp: usize,
        val_name: &String,
        typed_val_kind: &TypedValKind,
        statics: &Statics,
        name: &str,
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
        typed_module: &ASTTypedModule,
    ) {
        if let Some(val_kind) = context.get(val_name) {
            match val_kind {
                TypedValKind::ParameterRef(index, par) => {
                    call_parameters.add_parameter_ref(
                        param_name.into(),
                        val_name,
                        *index,
                        lambda_space_opt,
                        *indent,
                        stack_vals,
                        statics,
                        typed_module,
                        &par.ast_type,
                    );
                }

                TypedValKind::LetRef(_index, ast_typed_type) => {
                    let index_in_context = stack_vals.find_local_val_relative_to_bp(val_name);

                    call_parameters.add_let_val_ref(
                        param_name.into(),
                        val_name,
                        index_in_context,
                        lambda_space_opt,
                        *indent,
                        stack_vals,
                        ast_index,
                        statics,
                        typed_module,
                        ast_typed_type,
                    )
                }
            }
        } else if let Some(entry) = statics.get_typed_const(val_name) {
            call_parameters.add_label(
                param_name,
                entry.key.clone(),
                val_name.to_string(),
                Some(&format!("static {val_name}")),
                &entry.ast_typed_type,
                statics,
            )
        } else {
            panic!("Error adding val {}: {}", param_name, error_msg);
        }
    }

    fn reserve_return_register(&self, out: &mut String, stack: &StackVals);

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
        typed_module: &ASTTypedModule,
    ) -> Vec<LambdaCall> {
        debug!(
            "{}Adding function def {}",
            " ".repeat(indent * 4),
            function_def.name
        );

        let mut lambda_calls = Vec::new();

        if function_def.index.file_name.is_some() {
            self.add_comment(definitions, &format!("{}", function_def.index), false);
        }
        self.add_comment(
            definitions,
            &format!("function {}", function_def.original_signature()),
            false,
        );
        self.function_def(definitions, function_def, statics);

        let mut before = String::new();

        let stack = StackVals::new();

        self.function_preamble(definitions);

        if function_def.return_type.is_unit() {
            self.reserve_return_register(&mut before, &stack);
        }

        if is_lambda {
            self.reserve_lambda_space(&mut before, &stack, statics, lambda_space.unwrap());
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
                let len = calls.len();
                for (i, statement) in calls.iter().enumerate() {
                    match statement {
                        ASTTypedStatement::Expression(expr) => {
                            match expr {
                                ASTTypedExpression::ASTFunctionCallExpression(call_expression) => {
                                    let (bf, cur, af, mut lambda_calls_) = self
                                        .generate_call_function(
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
                                            typed_module,
                                            i == len - 1,
                                            false,
                                        );

                                    before.push_str(&bf);
                                    before.push_str(&cur);

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
                                        typed_module,
                                    );

                                    before.push_str(&parameters.before());
                                    before.push_str(&parameters.current());

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
                                        let mut def = ASTTypedFunctionDef {
                                            namespace: function_def.namespace.clone(),
                                            //name: format!("{}_{}_{}_lambda{}", parent_def_description, function_call.function_name, param_name, self.id),
                                            name: name.clone(),
                                            original_name: name.clone(),
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
                                            &mut def,
                                            lambda_space,
                                            &context,
                                            None,
                                            statics,
                                            typed_module,
                                            &stack,
                                            false,
                                            function_def,
                                            &function_def.return_type,
                                            &name,
                                            0, // TODO
                                        );

                                        before.push_str(&parameters.before());
                                        before.push_str(&parameters.current());

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
                                    self.value_as_return(&mut before, value_type, statics);
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
                                typed_module,
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

                let new_body = function_call_parameters.resolve_native_parameters(
                    body,
                    "0".into(),
                    indent,
                    function_def.return_type == ASTTypedType::Unit,
                    false,
                    Some(&function_def.return_type),
                    statics,
                );
                before.push_str(&new_body);
            }
        }

        self.reserve_local_vals(&stack, definitions);

        definitions.push_str(&self.transform_before(&stack, before));

        definitions.push_str(&after);
        definitions.push('\n');

        self.restore(&stack, definitions);

        self.function_end(definitions, true, Some(function_def));

        lambda_calls
    }

    fn function_def(
        &'a self,
        out: &mut String,
        function_def: &ASTTypedFunctionDef,
        statics: &mut Statics,
    );

    fn word_len(&self) -> usize;

    fn stack_pointer(&self) -> &str;

    fn word_size(&self) -> &str;

    fn reserve_lambda_space(
        &self,
        before: &mut String,
        stack: &StackVals,
        statics: &mut Statics,
        lambda_space: &LambdaSpace,
    );

    fn value_as_return(&self, before: &mut String, value_type: &ValueType, statics: &Statics);

    fn string_literal_return(&self, statics: &mut Statics, before: &mut String, value: &String);

    fn insert_on_top(src: &str, dest: &mut String) {
        for line in src.lines().rev() {
            dest.insert_str(0, &(line.to_string() + "\n"));
        }
    }

    fn generate_call_function(
        &'a self,
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
        typed_module: &ASTTypedModule,
        is_last: bool,
        is_inner_call: bool,
    ) -> (String, String, Vec<String>, Vec<LambdaCall>) {
        // before, after, lambda calls
        let mut before = String::new();
        let mut current = String::new();
        let mut after = Vec::new();

        let lambda_calls = if let Some(function_def) = typed_module
            .functions_by_name
            .get(&function_call.function_name)
        {
            let def = function_def.clone();
            // sometimes the function name is different from the function definition name, because it is not a valid ASM name (for enum types is enu-name::enum-variant)
            let real_function_name = typed_module
                .functions_by_name
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
                &mut current,
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
                typed_module,
                is_last,
                is_inner_call,
            )
        } else if let Some(function_def) = typed_module
            .functions_by_name
            .get(&function_call.function_name.replace("::", "_"))
        {
            let def = function_def.clone();
            // sometimes the function name is different from the function definition name, because it is not a valid ASM name (for enum types is enu-name::enum-variant)
            let real_function_name = typed_module
                .functions_by_name
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
                &mut current,
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
                typed_module,
                is_last,
                is_inner_call,
            )
        } else if let Some(kind) = context.get(&function_call.function_name) {
            let ast_type = kind.typed_type();

            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                return_type: _,
                parameters,
            }) = ast_type
            {
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
                    &mut current,
                    parameters_defs,
                    false,
                    None,
                    "".to_string(),
                    lambda_space,
                    indent,
                    true,
                    stack_vals,
                    &mut after,
                    id,
                    statics,
                    typed_module,
                    is_last,
                    is_inner_call,
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

        (before, current, after, lambda_calls)
    }

    fn create_lambdas(
        &'a self,
        lambdas: Vec<LambdaCall>,
        indent: usize,
        id: &mut usize,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
    ) -> LinkedHashMap<String, (String, String)> {
        let mut result = LinkedHashMap::new();

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
                typed_module,
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
                            typed_module,
                        );

                        result.insert(it.name.clone(), (definitions, body));

                        ls
                    })
                    .collect::<Vec<_>>(),
            );
        }
        if !lambda_calls.is_empty() {
            result.extend(self.create_lambdas(lambda_calls, indent + 1, id, statics, typed_module));
        }

        result
    }

    fn create_all_functions(
        &'a self,
        id: &mut usize,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
    ) -> LinkedHashMap<String, (String, String)> {
        let mut result = LinkedHashMap::new();
        let mut lambdas = LinkedHashMap::new();

        for function_def in typed_module.functions_by_name.values() {
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
                    typed_module,
                );
                lambdas.extend(self.create_lambdas(lambda_calls, 0, id, statics, typed_module));

                result.insert(function_def.name.clone(), (definitions, body));
            }
        }
        lambdas.extend(result);

        lambdas
    }

    fn translate_body(
        &self,
        body: &str,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
    ) -> Result<String, String> {
        let val_context = ValContext::new(None);

        let evaluator = self.get_text_macro_evaluator();

        let new_body = evaluator.translate(statics, None, None, body, true, typed_module)?;

        let result = evaluator.translate(statics, None, None, &new_body, false, typed_module)?;

        let mut lines: Vec<String> = result.lines().map(|it| it.to_owned()).collect::<Vec<_>>();

        self.called_functions(None, None, &result, &val_context, typed_module, statics)?
            .iter()
            .for_each(|(m, it)| {
                debug_i!("native call to {:?}, in main", it);
                if let Some(new_function_def) = typed_module.functions_by_name.get(&it.name) {
                    debug_i!("converted to {new_function_def}");
                    if it.name != new_function_def.name {
                        lines[it.i] = get_new_native_call(m, &new_function_def.name);
                    }
                } else {
                    // panic!("cannot find call {function_call}");
                    // TODO I hope it is a predefined function like addRef or deref for a struct or enum
                    println!("translate_body: cannot find call to {}", it.name);
                }
            });

        let result = lines.join("\n");

        Ok(result)
    }

    fn get_text_macro_evaluator(&self) -> TextMacroEvaluator;

    fn print_memory_info(&self, native_code: &mut String, statics: &Statics);

    fn optimize_unused_functions(&self) -> bool;

    fn get_used_functions(
        &self,
        functions_native_code: &LinkedHashMap<String, (String, String)>,
        native_code: &str,
        typed_module: &ASTTypedModule,
    ) -> Vec<(String, (String, String))> {
        let result = if self.optimize_unused_functions() {
            let mut used_functions = UsedFunctions::find(typed_module);
            // those are probably lambdas
            used_functions.extend(
                functions_native_code
                    .keys()
                    .filter(|it| !typed_module.functions_by_name.contains_key(*it))
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

    fn initialize_static_values(&self, generated_code: &mut String);

    fn debug(&self) -> bool;

    fn call_function_simple(
        &self,
        out: &mut String,
        function_name: &str,
        call_parameters: Option<&FUNCTION_CALL_PARAMETERS>,
        return_value: bool,
        is_inner_call: bool,
        return_type: Option<&ASTTypedType>,
        statics: &Statics,
    );

    fn call_function(
        &self,
        out: &mut String,
        function_name: &str,
        args: &[(&str, Option<&str>)],
        comment: Option<&str>,
        return_value: bool,
        is_inner_call: bool,
    );

    /// the difference with call_function is that arguments are Strings and not &str
    fn call_function_owned(
        &self,
        out: &mut String,
        function_name: &str,
        args: &[(String, Option<String>)],
        comment: Option<&str>,
        return_value: bool,
        is_inner_call: bool,
    ) {
        self.call_function(
            out,
            function_name,
            &args
                .iter()
                .map(|(arg, comment)| (arg.as_str(), comment.as_deref()))
                .collect::<Vec<_>>(),
            comment,
            return_value,
            is_inner_call,
        )
    }

    fn add_comment(&self, out: &mut String, comment: &str, indent: bool);

    fn add_rows(&self, out: &mut String, code: Vec<&str>, comment: Option<&str>, indent: bool);

    fn add(&self, out: &mut String, code: &str, comment: Option<&str>, indent: bool);

    fn add_empty_line(&self, out: &mut String);

    fn remove_comments_from_line(&self, line: String) -> String;

    fn preamble(&self, code: &mut String);

    fn create_lambda_add_ref_like_function(
        &self,
        namespace: &ASTNameSpace,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
        is_deref: bool,
    ) -> Option<ASTTypedFunctionDef>;

    /// Returns the name of the functions called in the code
    ///
    /// # Arguments
    ///
    /// * `body`: the code to scan for function calls
    ///
    /// returns: Vec<String>
    fn called_functions(
        &self,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&ASTFunctionDef>,
        body: &str,
        context: &ValContext,
        type_def_provider: &dyn TypeDefProvider,
        _statics: &mut Statics,
    ) -> Result<Vec<(TextMacro, DefaultFunctionCall)>, String> {
        let mut result = Vec::new();

        let evaluator = self.get_text_macro_evaluator();

        for (m, i) in
            evaluator.get_macros(typed_function_def, function_def, body, type_def_provider)?
        {
            if m.name == "call" {
                debug_i!("found call macro {m}");
                let types: Vec<ASTType> = m
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
                                if let Some(f) = typed_function_def {
                                    let t = type_def_provider
                                        .get_type_from_custom_typed_type(
                                            f.generic_types.get(name).unwrap(),
                                        )
                                        .unwrap();
                                    debug_i!("Function specified, found type {:?} for {name}", t);
                                    Ok(t)
                                } else {
                                    debug_i!("Function not specified, cannot find type {name}");
                                    Ok(ast_type.clone())
                                }
                            }
                            ASTType::Custom {
                                namespace: _,
                                name,
                                param_types: _,
                                index: _,
                            } => {
                                let result = if let Some(f) = typed_function_def {
                                    if let Some(t) = f.generic_types.get(name) {
                                        type_def_provider
                                            .get_type_from_typed_type(t)
                                            .ok_or(format!("name {name} t {t}"))
                                    } else if let Some(t) =
                                        type_def_provider.get_type_from_typed_type_name(name)
                                    {
                                        Ok(t)
                                    } else {
                                        Ok(ast_type.clone())
                                    }
                                } else {
                                    Ok(ast_type.clone())
                                };

                                result
                            }
                            _ => Ok(ast_type),
                        }
                    })
                    .collect::<Result<Vec<ASTType>, String>>()?;

                let function_name =
                    if let Some(MacroParam::Plain(function_name, _, _)) = m.parameters.get(0) {
                        function_name
                    } else {
                        return Err(format!("Cannot find function : {i}"));
                    };

                result.push((m.clone(), DefaultFunctionCall::new(function_name, types, i)));
            }
        }
        Ok(result)
    }

    fn reserve_local_vals(&self, stack: &StackVals, out: &mut String);

    fn generate_statics_code(
        &self,
        statics: &Statics,
        typed_module: &ASTTypedModule,
    ) -> (String, String);

    fn function_preamble(&self, out: &mut String);

    fn define_debug(&self, out: &mut String);

    fn restore(&self, stack: &StackVals, out: &mut String);

    fn function_end(
        &self,
        out: &mut String,
        add_return: bool,
        function_def: Option<&ASTTypedFunctionDef>,
    );

    fn add_statics(&self, statics: &mut Statics);

    fn value_to_string(&self, value_type: &ValueType) -> String;
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
        ASTTypedType::Type {
            namespace: _,
            name,
            native_type: _,
        } => {
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

    fn call_add_ref_simple(
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

    fn call_deref_simple(
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

    fn allocate_lambda_space_in_stack(
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

    fn push_to_scope_stack(&self, out: &mut String, what: &str, stack_vals: &StackVals) -> usize {
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

    fn set_return_value(&self, out: &mut String, what: &str) {
        self.add(
            out,
            &format!("mov {} eax, {what}", self.backend.word_size()),
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

    fn indirect_mov(
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

    fn return_register(&self) -> &str {
        "eax"
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
        index: &ASTIndex,
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
        index: &ASTIndex,
        before: &mut String,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
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
        value_type: &ValueType,
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

    fn stack_pointer(&self) -> &str {
        self.backend.stack_pointer()
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

    fn value_as_return(&self, before: &mut String, value_type: &ValueType, statics: &Statics) {
        let ws = self.backend.word_size();
        let rr = self.return_register();
        let v = self.value_to_string(value_type);
        self.add(before, &format!("mov     {ws} {rr}, {v}"), None, true);
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
        namespace: &ASTNameSpace,
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
                    Mem(len, unit) => {
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
            Mem((self.options.heap_table_slots + 1) * 20, Bytes),
        );
        statics.insert(
            "_heap_table_size".into(),
            I32Value(self.options.heap_table_slots as i32 * 20),
        );
        statics.insert("_heap_table_next".into(), I32Value(0));
        statics.insert("_heap".into(), Mem(4, Bytes));
        statics.insert("_heap_size".into(), I32Value(self.options.heap_size as i32));
        statics.insert("_heap_buffer".into(), Mem(self.options.heap_size, Bytes));

        statics.insert("_lambda_space_stack".into(), Mem(4, Bytes));
        statics.insert(
            "_lambda_space_stack_buffer".into(),
            Mem(self.options.lambda_space_size, Bytes),
        );

        let reusable_heap_table_size = 16 * 1024 * 1024;
        statics.insert(
            "_reusable_heap_table".into(),
            Mem(reusable_heap_table_size, Bytes),
        );
        statics.insert(
            "_reusable_heap_table_size".into(),
            I32Value(reusable_heap_table_size as i32),
        );
        statics.insert("_reusable_heap_table_next".into(), I32Value(0));

        // command line arguments
        statics.insert("_rasm_args".into(), Mem(12, Words));
        statics.insert("_NEW_LINE".into(), I32Value(10));
        statics.insert("_ESC".into(), I32Value(27));
        statics.insert("_for_nprint".into(), Mem(20, Bytes));
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
        let print_ref_macro = PrintRefMacro::new(self.clone());
        evaluators.insert("printRef".into(), Box::new(print_ref_macro));

        TextMacroEvaluator::new(evaluators, Box::new(CodeManipulatorNasm::new()))
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::statics::Statics;
    use crate::codegen::typedef_provider::DummyTypeDefProvider;
    use crate::codegen::val_context::ValContext;
    use crate::codegen::{AsmOptions, CodeGen, CodeGenAsm};

    #[test]
    fn called_functions_in_comment() {
        let sut = CodeGenAsm::new(AsmOptions::default(), false);
        let mut statics = Statics::new();

        assert!(sut
            .called_functions(
                None,
                None,
                "mov    eax, 1; $call(something)",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
                &mut statics,
            )
            .unwrap()
            .is_empty());
    }

    #[test]
    fn called_functions_external() {
        let sut = CodeGenAsm::new(AsmOptions::default(), false);
        let mut statics = Statics::new();

        assert!(sut
            .called_functions(
                None,
                None,
                "call something",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
                &mut statics,
            )
            .unwrap()
            .is_empty());
    }

    #[test]
    fn called_functions() {
        let sut = CodeGenAsm::new(AsmOptions::default(), false);
        let mut statics = Statics::new();

        assert_eq!(
            sut.called_functions(
                None,
                None,
                "$call(something)",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
                &mut statics,
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
