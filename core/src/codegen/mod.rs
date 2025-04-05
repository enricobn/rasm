use std::env;
use std::iter::zip;
use std::ops::Deref;
use std::path::Path;

use asm::code_gen_asm::AsmOptions;
use enh_ast::EnhModuleId;
use linked_hash_map::LinkedHashMap;
use linked_hash_set::LinkedHashSet;
use log::debug;

use enhanced_module::EnhancedASTModule;
use lambda::{LambdaCall, LambdaSpace};
use rasm_parser::catalog::modules_catalog::ModulesCatalog;
use rasm_utils::{debug_i, OptionDisplay};

use crate::codegen::compile_target::CompileTarget;
use crate::codegen::enh_ast::{
    EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTParameterDef, EnhASTType,
    EnhBuiltinTypeKind,
};
use crate::codegen::enh_val_context::{EnhValContext, TypedValContext};
use crate::codegen::function_call_parameters::FunctionCallParameters;

use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{MacroParam, TextMacro, TextMacroEvaluator};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::commandline::CommandLineOptions;
use crate::enh_type_check::typed_ast::{
    convert_to_typed_module, get_type_of_typed_expression, ASTTypedExpression,
    ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
    DefaultFunctionCall,
};
use crate::enh_type_check::used_functions::UsedFunctions;
use crate::errors::CompilationError;
use crate::project::RasmProject;
use crate::type_check::ast_modules_container::ASTModulesContainer;
use crate::type_check::ast_type_checker::ASTTypeChecker;
use crate::type_check::get_new_native_call;
use rasm_parser::parser::ast::ASTValueType;

pub mod asm;
pub mod c;
mod code_manipulator;
pub mod compile_target;
pub mod enh_ast;
pub mod enh_val_context;
pub mod enhanced_module;
pub mod function_call_parameters;
pub mod lambda;
pub mod stack;
pub mod statics;
pub mod text_macro;
pub mod typedef_provider;
pub mod val_context;

#[derive(Clone, Debug)]
pub enum EnhValKind {
    ParameterRef(usize, EnhASTParameterDef),
    LetRef(usize, EnhASTType, EnhASTIndex),
}

impl EnhValKind {
    pub fn ast_type(&self) -> EnhASTType {
        match self {
            EnhValKind::ParameterRef(_, par) => par.ast_type.clone(),
            EnhValKind::LetRef(_, ast_type, _) => ast_type.clone(),
        }
    }
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
    ast_type_checker: ASTTypeChecker,
    modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    modules_container: &ASTModulesContainer,
) -> Result<ASTTypedModule, CompilationError> {
    let mandatory_functions = target.get_mandatory_functions(&module);
    let default_functions = target.get_default_functions(print_memory_info);

    convert_to_typed_module(
        module,
        print_module,
        mandatory_functions,
        statics,
        default_functions,
        target,
        debug,
        ast_type_checker,
        modules_catalog,
        modules_container,
    )
}

pub fn get_std_lib_path() -> Option<String> {
    let from_env = env::var("RASM_STDLIB");

    if let Ok(result) = from_env {
        return Some(result);
    }

    let current_dir = env::current_dir().unwrap();

    let relative_stdlib_path = current_dir.join("stdlib");
    if relative_stdlib_path.exists() {
        Some(relative_stdlib_path.to_string_lossy().to_string())
    } else {
        None
    }
}

pub trait CodeGenOptions {
    fn dereference(&self) -> bool;
}

pub trait CodeGen<'a, FCP: FunctionCallParameters<CTX>, CTX, OPTIONS: CodeGenOptions> {
    fn options(&self) -> &OPTIONS;

    fn create_code_gen_context(&self) -> CTX;

    fn generate(
        &'a self,
        project: &RasmProject,
        target: &CompileTarget,
        typed_module: &ASTTypedModule,
        statics: Statics,
        command_line_options: &CommandLineOptions,
        out_folder: &Path,
    ) -> Vec<(String, String)> {
        let mut statics = statics;
        let mut id: usize = 0;
        let mut body = String::new();

        let mut lambdas = Vec::new();

        // for now main has no context
        let mut context = TypedValContext::new(None);

        let code_gen_context = self.create_code_gen_context();

        let mut after = String::new();
        let mut before = String::new();

        let a_body = &typed_module.body;

        self.generate_function_body(
            &code_gen_context,
            typed_module,
            None,
            a_body,
            None,
            &mut lambdas,
            &mut context,
            &mut statics,
            &mut before,
            &mut after,
            &mut body,
            &mut id,
            4,
            &EnhASTNameSpace::root_namespace(&project),
        );

        body.push_str(&self.transform_before(&code_gen_context, before));
        body.push_str(&after);

        let mut functions_generated_code =
            self.create_lambdas(lambdas, 0, &mut id, &mut statics, typed_module);

        functions_generated_code.extend(self.create_all_functions(
            &mut id,
            &mut statics,
            typed_module,
        ));

        let mut generated_code = String::new();

        self.preamble(&mut generated_code);

        self.add_statics(project, &mut statics, out_folder);

        let (static_declarations, static_code) =
            self.generate_statics_code(project, &statics, typed_module, out_folder);

        generated_code.push_str(&static_declarations);

        if self.debug() {
            self.define_debug(&mut generated_code);
        }

        self.initialize_static_values(&mut generated_code);

        let code = self.translate_static_code(static_code, typed_module);

        self.add(&mut generated_code, &code, None, true);

        self.create_command_line_arguments(&mut generated_code);

        self.add(&mut generated_code, "", None, true);

        self.function_preamble(&mut generated_code);

        self.reserve_local_vals(&code_gen_context, &mut generated_code);

        // probably there is not a valid body from functions
        for (_name, (_defs, bd)) in functions_generated_code.iter() {
            // TODO if I add the body before, here I can add directly to generated_code
            body.push_str(bd);
        }

        generated_code.push_str(&body);

        generated_code.push('\n');

        if command_line_options.print_memory {
            self.print_memory_info(&mut generated_code, &statics);
        }

        self.end_main(&mut generated_code);

        self.function_end(&code_gen_context, &mut generated_code, false, None);

        let used_functions =
            self.get_used_functions(&functions_generated_code, &generated_code, typed_module);

        for (_, (defs, _bd)) in used_functions {
            generated_code.push_str(&defs);
        }

        vec![(
            format!("{}.{}", project.config.package.name, target.extension()),
            generated_code,
        )]
    }

    fn end_main(&self, code: &mut String);

    fn transform_before(&self, code_gen_context: &CTX, before: String) -> String;

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
        code_gen_context: &CTX,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        kind: &TypedValKind,
        call_parameters: &FCP,
        return_value: bool,
        is_inner_call: bool,
        statics: &Statics,
    );

    fn call_function_(
        &'a self,
        code_gen_context: &CTX,
        parent_fcp: Option<&FCP>,
        namespace: &EnhASTNameSpace,
        function_call: &&ASTTypedFunctionCall,
        context: &TypedValContext,
        parent_def: &Option<&ASTTypedFunctionDef>,
        before: &mut String,
        current: &mut String,
        parameters: Vec<ASTTypedParameterDef>,
        inline: bool,
        body: Option<ASTTypedFunctionBody>,
        function_to_call: String,
        lambda_space_opt: Option<&LambdaSpace>,
        indent: usize,
        is_lambda: bool,
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
                    &format!("inlining function {}", function_call.function_name),
                    true,
                );
            } else {
                self.add_comment(
                    before,
                    &format!("calling function {}", function_call.function_name),
                    true,
                );
            }
        }

        let mut call_parameters = self.function_call_parameters(
            code_gen_context,
            parent_fcp,
            &parameters,
            inline,
            false,
            *id,
        );

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
                    /*ASTTypedExpression::CharLiteral(c) => {
                        let label = self.statics.add_char(c);
                        call_parameters.add_string_literal(&param_name, label, None);
                    }*/
                    ASTTypedExpression::Value(value_type, _) => {
                        if let ASTValueType::String(s) = value_type {
                            call_parameters.add_string_constant(&param_name, s, None, statics);
                        } else {
                            call_parameters.add_value_type(&param_name, value_type)
                        }
                    }
                    ASTTypedExpression::ASTFunctionCallExpression(call) => {
                        let (bf, cur, af, mut inner_lambda_calls) = self.generate_call_function(
                            code_gen_context,
                            Some(&call_parameters),
                            namespace,
                            call,
                            context,
                            *parent_def,
                            lambda_space_opt,
                            indent + 1,
                            false,
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
                            &param_type,
                        );

                        lambda_calls.append(&mut inner_lambda_calls);
                    }
                    ASTTypedExpression::ValueRef(name, index) => {
                        let error_msg = format!(
                            "Cannot find val {}, calling function {}",
                            name, function_call.function_name
                        );
                        self.add_val(
                            code_gen_context,
                            context,
                            &lambda_space_opt,
                            &indent,
                            &mut call_parameters,
                            &param_name,
                            name,
                            &error_msg,
                            index,
                            statics,
                            typed_module,
                            &mut *id,
                            &mut lambda_calls,
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

                        let lambda_in_stack = function_def.return_type.is_unit()
                            || can_lambda_be_in_stack(&function_def.return_type, typed_module);

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
                            code_gen_context,
                            lambda_in_stack,
                            &param_type,
                            &name,
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

        if inline && self.replace_inline_call_including_source() {
            if let Some(ASTTypedFunctionBody::NativeBody(body)) = &body {
                current.push_str(
                    &call_parameters.resolve_native_parameters(
                        code_gen_context,
                        body,
                        indent,
                        is_last
                            && parent_def
                                .map(|it| it.return_type != ASTTypedType::Unit)
                                .unwrap_or(false),
                        is_inner_call,
                        parent_def.map(|it| it.return_type.clone()).as_ref(),
                        is_lambda,
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
                    code_gen_context,
                    function_call,
                    current,
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
                    code_gen_context,
                    function_call,
                    current,
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

        current.push_str(&call_parameters.current());

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
        code_gen_context: &CTX,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        index_in_lambda_space: usize,
        call_parameters: &FCP,
        ast_type_type: &ASTTypedType,
        statics: &Statics,
        return_value: bool,
        is_inner_call: bool,
    );

    fn function_call_parameters<'b, 'c>(
        &'a self,
        code_gen_context: &'c CTX,
        parent: Option<&FCP>,
        parameters: &'b Vec<ASTTypedParameterDef>,
        inline: bool,
        immediate: bool,
        id: usize,
    ) -> FCP;

    fn define_let(&'a self, code_gen_context: &CTX, name: &str, is_const: bool);

    fn add_let(
        &'a self,
        code_gen_context: &CTX,
        parent_fcp: Option<&FCP>,
        namespace: &EnhASTNameSpace,
        context: &mut TypedValContext,
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
        self.define_let(code_gen_context, name, is_const);

        let (return_type, (bf, mut cur, af, new_lambda_calls), index) = match expr {
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
                            code_gen_context,
                            parent_fcp,
                            namespace,
                            call,
                            context,
                            function_def,
                            lambda_space,
                            0,
                            false,
                            id,
                            statics,
                            typed_module,
                            false,
                            false,
                        ),
                        call.index.clone(),
                    )
                } else {
                    let return_type = call.return_type(context, typed_module);
                    (
                        return_type,
                        self.generate_call_function(
                            code_gen_context,
                            parent_fcp,
                            namespace,
                            call,
                            context,
                            function_def,
                            lambda_space,
                            0,
                            false,
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
                if let ASTValueType::String(s) = value_type {
                    let typed_type = ASTTypedType::Builtin(BuiltinTypedTypeKind::String);

                    self.set_let_for_string_literal(
                        code_gen_context,
                        before,
                        name,
                        is_const,
                        statics,
                        body,
                        s,
                        &typed_type,
                    );
                    (
                        typed_type,
                        (String::new(), String::new(), vec![], vec![]),
                        EnhASTIndex::none(),
                    )
                } else {
                    let typed_type =
                        get_type_of_typed_expression(typed_module, context, expr, None, statics)
                            .unwrap();

                    self.set_let_for_value(
                        code_gen_context,
                        before,
                        name,
                        is_const,
                        statics,
                        body,
                        value_type,
                        &typed_type,
                    );
                    (
                        typed_type,
                        (String::new(), String::new(), vec![], vec![]),
                        index.clone(),
                    )
                }
            }
            ASTTypedExpression::ValueRef(val_name, index) => {
                if let Some(typed_val_kind) = context.get(val_name) {
                    let typed_type = self.set_let_for_value_ref(
                        code_gen_context,
                        before,
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

                let key = statics.add_typed_const(name.to_owned(), return_type.clone());

                self.set_let_const_for_function_call_result(
                    &key,
                    body,
                    &mut cur,
                    name,
                    &return_type,
                    statics,
                );
                body.push_str(&cur);
            }

            if self.options().dereference() {
                if let Some(type_name) = get_reference_type_name(&return_type, typed_module) {
                    self.add_ref(&name, statics, body, typed_module, &index, &type_name);
                }
            }
        } else {
            self.insert_let_in_context(code_gen_context, context, name, &return_type);
            if !bf.is_empty() || !cur.is_empty() {
                self.store_function_result_in_stack(code_gen_context, &mut cur, name, &return_type);
                before.push_str(&bf);
                before.push_str(&cur);
            }

            let not_empty_after_lines = af
                .into_iter()
                .filter(|it| !it.is_empty())
                .collect::<Vec<String>>();
            self.insert_on_top(&not_empty_after_lines.join("\n"), after);

            if self.options().dereference() {
                if let Some(type_name) = get_reference_type_name(&return_type, typed_module) {
                    self.call_add_ref_for_let_val(
                        code_gen_context,
                        name,
                        &index,
                        before,
                        statics,
                        &type_name,
                        typed_module,
                        &return_type,
                    );

                    let deref_str = self.call_deref_for_let_val(
                        code_gen_context,
                        name,
                        statics,
                        &type_name,
                        typed_module,
                        &return_type,
                    );
                    self.insert_on_top(&deref_str, after);
                }
            }
        }

        new_lambda_calls
    }

    fn insert_let_in_context(
        &self,
        code_gen_context: &CTX,
        context: &mut TypedValContext,
        name: &str,
        typed_type: &ASTTypedType,
    );

    fn store_function_result_in_stack(
        &self,
        code_gen_context: &CTX,
        code: &mut String,
        name: &str,
        typed_type: &ASTTypedType,
    );

    fn add_ref(
        &self,
        name: &str,
        statics: &mut Statics,
        body: &mut String,
        typed_module: &ASTTypedModule,
        index: &EnhASTIndex,
        type_name: &String,
    );

    fn call_deref_for_let_val(
        &self,
        code_gen_context: &CTX,
        name: &str,
        statics: &mut Statics,
        type_name: &String,
        typed_module: &ASTTypedModule,
        t: &ASTTypedType,
    ) -> String;

    fn call_add_ref_for_let_val(
        &self,
        code_gen_context: &CTX,
        name: &str,
        index: &EnhASTIndex,
        before: &mut String,
        statics: &mut Statics,
        type_name: &String,
        typed_module: &ASTTypedModule,
        t: &ASTTypedType,
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
        code_gen_context: &CTX,
        before: &mut String,
        val_name: &String,
        typed_val_kind: &TypedValKind,
        statics: &Statics,
        name: &str,
    ) -> ASTTypedType;

    fn set_let_for_string_literal(
        &self,
        code_gen_context: &CTX,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        value: &String,
        typed_type: &ASTTypedType,
    );

    fn set_let_for_value(
        &self,
        code_gen_context: &CTX,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        value_type: &ASTValueType,
        typed_type: &ASTTypedType,
    );

    fn add_val(
        &self,
        code_gen_context: &CTX,
        context: &TypedValContext,
        lambda_space_opt: &Option<&LambdaSpace>,
        indent: &usize,
        call_parameters: &mut FCP,
        param_name: &str,
        val_name: &str,
        error_msg: &str,
        ast_index: &EnhASTIndex,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
        id: &mut usize,
        lambda_calls: &mut Vec<LambdaCall>,
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
                        code_gen_context,
                        statics,
                        typed_module,
                        &par.ast_type,
                    );
                }

                TypedValKind::LetRef(_index, ast_typed_type) => call_parameters.add_let_val_ref(
                    param_name.into(),
                    val_name,
                    lambda_space_opt,
                    *indent,
                    code_gen_context,
                    ast_index,
                    statics,
                    typed_module,
                    ast_typed_type,
                ),
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
            let mut def = typed_module
                .functions_by_name
                .get(val_name)
                .unwrap()
                .clone();

            let name = format!("lambda_{}", id);
            def.name = name.clone();

            let lambda_type = ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: def
                    .parameters
                    .iter()
                    .map(|it| it.ast_type.clone())
                    .collect::<Vec<_>>(),
                return_type: Box::new(def.return_type.clone()),
            });

            let lambda_in_stack =
                def.return_type.is_unit() || can_lambda_be_in_stack(&def.return_type, typed_module);

            let lambda_space = call_parameters.add_lambda(
                &mut def,
                None,
                context,
                Some(&format!("reference to function {val_name}")),
                statics,
                typed_module,
                code_gen_context,
                lambda_in_stack,
                &lambda_type,
                &name,
            );
            let lambda_call = LambdaCall {
                def,
                space: lambda_space,
            };

            lambda_calls.push(lambda_call);

            *id += 1;

            //panic!("Error adding val {}: {}", param_name, error_msg);
        }
    }

    fn reserve_return_register(&self, code_gen_context: &CTX, out: &mut String);

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
            &format!("function {}", function_def.original_signature(typed_module)),
            false,
        );
        self.function_def(definitions, function_def, statics);

        let mut before = String::new();

        let code_gen_context = self.create_code_gen_context();

        self.function_preamble(definitions);

        if function_def.return_type.is_unit() {
            self.reserve_return_register(&code_gen_context, &mut before);
        }

        if is_lambda {
            self.reserve_lambda_space(
                &code_gen_context,
                &mut before,
                statics,
                lambda_space.unwrap(),
                function_def,
            );
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
            ASTTypedFunctionBody::RASMBody(statements) => {
                self.generate_function_body(
                    &code_gen_context,
                    typed_module,
                    Some(function_def),
                    statements,
                    lambda_space,
                    &mut lambda_calls,
                    &mut context,
                    statics,
                    &mut before,
                    &mut after,
                    body,
                    id,
                    indent,
                    &function_def.namespace,
                );
            }
            ASTTypedFunctionBody::NativeBody(body) => {
                let function_call_parameters = self.function_call_parameters(
                    &code_gen_context,
                    None,
                    &function_def.parameters,
                    false,
                    false,
                    *id,
                );

                *id += 1;

                let new_body = function_call_parameters.resolve_native_parameters(
                    &code_gen_context,
                    body,
                    indent,
                    function_def.return_type == ASTTypedType::Unit,
                    false,
                    Some(&function_def.return_type),
                    is_lambda,
                );
                before.push_str(&new_body);
            }
        }

        self.reserve_local_vals(&code_gen_context, definitions);

        definitions.push_str(&self.transform_before(&code_gen_context, before));

        definitions.push_str(&after);
        definitions.push('\n');

        self.function_end(&code_gen_context, definitions, true, Some(function_def));

        lambda_calls
    }

    fn generate_function_body(
        &'a self,
        code_gen_context: &CTX,
        typed_module: &ASTTypedModule,
        function_def: Option<&ASTTypedFunctionDef>,
        a_body: &Vec<ASTTypedStatement>,
        lambda_space: Option<&LambdaSpace>,
        lambda_calls: &mut Vec<LambdaCall>,
        context: &mut TypedValContext,
        statics: &mut Statics,
        before: &mut String,
        after: &mut String,
        body: &mut String,
        id: &mut usize,
        indent: usize,
        namespace: &EnhASTNameSpace,
    ) {
        let inline = function_def.map(|it| it.inline).unwrap_or(false);
        let fun_return_type = function_def
            .map(|it| it.return_type.clone())
            .unwrap_or(ASTTypedType::Unit);
        let len = a_body.len();
        for (i, statement) in a_body.iter().enumerate() {
            match statement {
                ASTTypedStatement::Expression(expr) => {
                    match expr {
                        ASTTypedExpression::ASTFunctionCallExpression(call) => {
                            let (bf, cur, af, mut lambda_calls_) = self.generate_call_function(
                                &code_gen_context,
                                None,
                                namespace,
                                call,
                                &context,
                                function_def,
                                lambda_space,
                                indent + 1,
                                false,
                                id,
                                statics,
                                typed_module,
                                i == len - 1,
                                false,
                            );

                            before.push_str(&bf);
                            before.push_str(&cur);

                            self.insert_on_top(&af.join("\n"), after);

                            lambda_calls.append(&mut lambda_calls_);
                        }
                        ASTTypedExpression::ValueRef(val, index) => {
                            // TODO I don't like to use FunctionCallParameters to do this, probably I need another struct to do only the calculation of the address to get

                            let mut parameters = self.function_call_parameters(
                                &code_gen_context,
                                None,
                                &Vec::new(),
                                inline,
                                true,
                                *id,
                            );

                            *id += 1;

                            self.add_val(
                                &code_gen_context,
                                &context,
                                &lambda_space,
                                &indent,
                                &mut parameters,
                                val,
                                val,
                                "",
                                index,
                                statics,
                                typed_module,
                                id,
                                lambda_calls,
                            );

                            before.push_str(&parameters.before());
                            before.push_str(&parameters.current());

                            self.insert_on_top(&parameters.after().join("\n"), after);
                        }
                        ASTTypedExpression::Lambda(lambda_def) => {
                            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                                parameters,
                                return_type,
                            }) = &fun_return_type
                            {
                                let rt = return_type.deref().clone();

                                let lambda_parameters =
                                    zip(lambda_def.parameter_names.iter(), parameters.iter())
                                        .map(|((name, index), typed_type)| ASTTypedParameterDef {
                                            name: name.clone(),
                                            ast_type: typed_type.clone(),
                                            ast_index: index.clone(),
                                        })
                                        .collect::<Vec<_>>();

                                let name = format!("lambda{}", id);
                                let mut def = ASTTypedFunctionDef {
                                    namespace: namespace.clone(),
                                    //name: format!("{}_{}_{}_lambda{}", parent_def_description, function_call.function_name, param_name, self.id),
                                    name: name.clone(),
                                    original_name: name.clone(),
                                    parameters: lambda_parameters, // parametrs are calculated later
                                    return_type: rt,
                                    body: ASTTypedFunctionBody::RASMBody(lambda_def.clone().body),
                                    inline: false,
                                    generic_types: LinkedHashMap::new(),
                                    index: lambda_def.index.clone(),
                                };

                                *id += 1;

                                let mut parameters = self.function_call_parameters(
                                    &code_gen_context,
                                    None,
                                    &Vec::new(),
                                    inline,
                                    true,
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
                                    &code_gen_context,
                                    false,
                                    &fun_return_type,
                                    &name,
                                );

                                before.push_str(&parameters.before());
                                before.push_str(&parameters.current());

                                self.insert_on_top(&parameters.after().join("\n"), after);
                                lambda_calls.push(LambdaCall {
                                    def,
                                    space: new_lambda_space,
                                });
                            } else {
                                panic!("Expected lambda return type");
                            }
                        }
                        ASTTypedExpression::Value(value_type, _) => {
                            if let ASTValueType::String(s) = value_type {
                                self.string_literal_return(statics, before, s);
                            } else {
                                self.value_as_return(before, value_type, statics);
                            }
                        }
                    }
                }
                ASTTypedStatement::LetStatement(name, expr, is_const, _let_index) => {
                    let mut new_lambda_calls = self.add_let(
                        code_gen_context,
                        None,
                        namespace,
                        context,
                        after,
                        before,
                        name,
                        expr,
                        function_def,
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

    fn function_def(
        &'a self,
        out: &mut String,
        function_def: &ASTTypedFunctionDef,
        statics: &mut Statics,
    );

    fn word_len(&self) -> usize;

    fn word_size(&self) -> &str;

    fn reserve_lambda_space(
        &self,
        code_gen_context: &CTX,
        before: &mut String,
        statics: &mut Statics,
        lambda_space: &LambdaSpace,
        def: &ASTTypedFunctionDef,
    );

    fn value_as_return(
        &self,
        before: &mut String,
        value_type: &ASTValueType,
        statics: &mut Statics,
    );

    fn string_literal_return(&self, statics: &mut Statics, before: &mut String, value: &String);

    fn insert_on_top(&self, src: &str, dest: &mut String) {
        for line in src.lines().rev() {
            dest.insert_str(0, &(line.to_string() + "\n"));
        }
    }

    fn generate_call_function(
        &'a self,
        code_gen_context: &CTX,
        parent_fcp: Option<&FCP>,
        namespace: &EnhASTNameSpace,
        function_call: &ASTTypedFunctionCall,
        context: &TypedValContext,
        parent_def: Option<&ASTTypedFunctionDef>,
        lambda_space: Option<&LambdaSpace>,
        indent: usize,
        is_lambda: bool,
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
                code_gen_context,
                parent_fcp,
                namespace,
                &function_call,
                context,
                &parent_def,
                &mut before,
                &mut current,
                def.parameters,
                def.inline,
                Some(def.body),
                real_function_name,
                lambda_space,
                indent,
                is_lambda,
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
                            ast_index: EnhASTIndex::none(), // TODO I don't know...
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
                    code_gen_context,
                    parent_fcp,
                    namespace,
                    &function_call,
                    context,
                    &parent_def,
                    &mut before,
                    &mut current,
                    parameters_defs,
                    false,
                    None,
                    function_call.function_name.clone(),
                    lambda_space,
                    indent,
                    true,
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
            //if !function_def.inline {
            if self.create_function_definition(function_def) {
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
        let val_context = EnhValContext::new(None);

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

            let mut used_functions_in_defs = LinkedHashSet::new();

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
        call_parameters: Option<&FCP>,
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
        function_def: Option<&EnhASTFunctionDef>,
        body: &str,
        context: &EnhValContext,
        type_def_provider: &dyn TypeDefProvider,
        _statics: &mut Statics,
    ) -> Result<Vec<(TextMacro, DefaultFunctionCall)>, String> {
        let mut result = Vec::new();

        let evaluator = self.get_text_macro_evaluator();

        for (m, i) in evaluator.get_macros_filter(
            typed_function_def,
            function_def,
            body,
            type_def_provider,
            &|name, _parameter| name == "call",
        )? {
            debug_i!("found call macro {m}");
            let types: Vec<EnhASTType> = m
                .parameters
                .iter()
                .skip(1)
                .map(|it| {
                    let ast_type = match it {
                        MacroParam::Plain(_, opt_type, _) => match opt_type {
                            None => EnhASTType::Builtin(EnhBuiltinTypeKind::I32),
                            Some(ast_type) => ast_type.clone(),
                        },
                        MacroParam::StringLiteral(_) => {
                            EnhASTType::Builtin(EnhBuiltinTypeKind::String)
                        }
                        MacroParam::Ref(name, None, _) => {
                            debug_i!("found ref {name}");
                            match context.get(name.strip_prefix('$').unwrap()).unwrap() {
                                EnhValKind::ParameterRef(_, par) => par.ast_type.clone(),
                                EnhValKind::LetRef(_, ast_type, _) => ast_type.clone(),
                            }
                        }
                        MacroParam::Ref(name, Some(ast_type), _) => {
                            debug_i!("found ref {name} : {ast_type}");
                            ast_type.clone()
                        }
                    };

                    match &ast_type {
                        EnhASTType::Generic(_, name) => {
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
                        EnhASTType::Custom {
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
                .collect::<Result<Vec<EnhASTType>, String>>()?;

            let function_name =
                if let Some(MacroParam::Plain(function_name, _, _)) = m.parameters.get(0) {
                    function_name
                } else {
                    return Err(format!("Cannot find function : {i}"));
                };

            result.push((m.clone(), DefaultFunctionCall::new(function_name, types, i)));
        }
        Ok(result)
    }

    fn reserve_local_vals(&self, code_gen_stack: &CTX, out: &mut String);

    fn generate_statics_code(
        &self,
        project: &RasmProject,
        statics: &Statics,
        typed_module: &ASTTypedModule,
        out_folder: &Path,
    ) -> (String, String);

    fn function_preamble(&self, out: &mut String);

    fn define_debug(&self, out: &mut String);

    fn function_end(
        &self,
        code_gen_context: &CTX,
        out: &mut String,
        add_return: bool,
        function_def: Option<&ASTTypedFunctionDef>,
    );

    fn add_statics(&self, project: &RasmProject, statics: &mut Statics, out_folder: &Path);

    fn value_to_string(&self, value_type: &ASTValueType) -> String;

    fn create_function_definition(&self, function_def: &ASTTypedFunctionDef) -> bool;

    fn replace_inline_call_including_source(&self) -> bool;
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
            is_ref,
        } => {
            if *is_ref {
                Some(name.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn can_lambda_be_in_stack(
    lambda_return_type: &ASTTypedType,
    type_def_provider: &dyn TypeDefProvider,
) -> bool {
    let mut already_checked = LinkedHashSet::new();
    can_lambda_be_in_stack_(lambda_return_type, type_def_provider, &mut already_checked)
}

fn can_lambda_be_in_stack_(
    lambda_return_type: &ASTTypedType,
    type_def_provider: &dyn TypeDefProvider,
    already_checked: &mut LinkedHashSet<String>,
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
                        can_lambda_be_in_stack_(&it.ast_type, type_def_provider, already_checked)
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
                    can_lambda_be_in_stack_(&it.ast_type, type_def_provider, already_checked)
                })
            } else {
                panic!()
            }
        }
        _ => true,
    }
}

#[cfg(test)]
mod tests {

    use std::path::PathBuf;

    use tempdir::TempDir;

    use crate::codegen::asm::code_gen_asm::CodeGenAsm;
    use crate::codegen::enh_val_context::EnhValContext;
    use crate::codegen::statics::Statics;
    use crate::codegen::typedef_provider::DummyTypeDefProvider;
    use crate::codegen::{AsmOptions, CodeGen};
    use crate::commandline::CommandLineOptions;
    use crate::project::RasmProject;
    use crate::test_utils::project_to_ast_typed_module;

    use super::c::code_gen_c::CodeGenC;
    use super::c::options::COptions;
    use super::compile_target::CompileTarget;

    #[test]
    fn called_functions_in_comment() {
        let sut = CodeGenAsm::new(AsmOptions::default(), false);
        let mut statics = Statics::new();

        assert!(sut
            .called_functions(
                None,
                None,
                "mov    eax, 1; $call(something)",
                &EnhValContext::new(None),
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
                &EnhValContext::new(None),
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
                &EnhValContext::new(None),
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

    #[test]
    fn breakout_codegenc() {
        let options = COptions::default();
        let sut = CodeGenC::new(options.clone(), false);

        let project = RasmProject::new(PathBuf::from("../rasm/resources/examples/breakout"));
        let target = CompileTarget::C(options);

        let (typed_module, statics) = project_to_ast_typed_module(&project, &target).unwrap();

        let dir = TempDir::new("rasm_int_test").unwrap();

        let result = sut.generate(
            &project,
            &target,
            &typed_module,
            statics,
            &CommandLineOptions::default(),
            dir.path(),
        );

        assert_eq!(1, result.len());
        assert_eq!("breakout.c", result.get(0).unwrap().0);
    }
}
