use std::borrow::Borrow;
use std::env;
use std::iter::zip;
use std::ops::Deref;
use std::path::PathBuf;
use std::time::Instant;

use linked_hash_map::LinkedHashMap;
use log::{debug, info};

use enhanced_module::EnhancedASTModule;
use lambda::{LambdaCall, LambdaSpace};

use crate::codegen::backend::Backend;
use crate::codegen::function_call_parameters::FunctionCallParameters;
use crate::codegen::stack::{StackEntryType, StackVals};
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{TextMacroEvaluator, TypeDefProvider};
use crate::codegen::val_context::{TypedValContext, ValContext};
use crate::codegen::MemoryUnit::{Bytes, Words};
use crate::codegen::MemoryValue::{I32Value, Mem};
use crate::debug_i;
use crate::parser::ast::{ASTIndex, ASTModule, ASTParameterDef, ASTType};
use crate::transformations::enrich_module;
use crate::transformations::type_functions_creator::type_mandatory_functions;
use crate::transformations::typed_enum_functions_creator::typed_enum_functions_creator;
use crate::transformations::typed_struct_functions_creator::typed_struct_functions_creator;
use crate::transformations::typed_type_functions_creator::typed_type_functions_creator;
use crate::type_check::functions_container::TypeFilter::Exact;
use crate::type_check::get_new_native_call;
use crate::type_check::typed_ast::{
    convert_to_typed_module, get_type_of_typed_expression, ASTTypedExpression,
    ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};
use crate::type_check::typed_context::TypeConversionContext;
use crate::utils::OptionDisplay;

pub mod backend;
pub mod enhanced_module;
mod function_call_parameters;
pub mod lambda;
pub mod stack;
pub mod statics;
pub mod text_macro;
pub mod val_context;

/// It's a constant that will be replaced by the code generator with the size (in bytes)
/// of all the vals in the stack. We need it since we know the full size only at the end of a function
/// generation, but we need that value during the code generation...   
pub const STACK_VAL_SIZE_NAME: &str = "$stack_vals_size";

pub struct CodeGen<'a> {
    pub module: ASTTypedModule,
    id: usize,
    statics: Statics,
    body: String,
    definitions: String,
    lambdas: Vec<LambdaCall>,
    backend: &'a dyn Backend,
    heap_size: usize,
    heap_table_slots: usize,
    print_memory_info: bool,
    lambda_space_size: usize,
    debug_asm: bool,
    dereference: bool,
    type_conversion_context: TypeConversionContext,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemoryValue {
    StringValue(String),
    I32Value(i32),
    Mem(usize, MemoryUnit),
    RefToLabel(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemoryUnit {
    Bytes,
    Words,
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

impl<'a> CodeGen<'a> {
    pub fn new(
        backend: &'a dyn Backend,
        module: ASTModule,
        lambda_space_size: usize,
        heap_size: usize,
        heap_table_slots: usize,
        debug_asm: bool,
        print_memory_info: bool,
        dereference: bool,
        print_module: bool,
        resource_path: PathBuf,
    ) -> Self {
        let start = Instant::now();

        crate::utils::debug_indent::INDENT.with(|indent| {
            *indent.borrow_mut() = 0;
        });

        let mut statics = Statics::new();

        let mut module = module;
        enrich_module(backend, resource_path, &mut statics, &mut module);

        let (typed_module, type_conversion_context) = Self::get_typed_module(
            backend,
            module,
            debug_asm,
            print_memory_info,
            dereference,
            print_module,
            &mut statics,
        );

        info!("type check ended in {:?}", start.elapsed());

        Self {
            module: typed_module,
            body: String::new(),
            statics,
            id: 0,
            definitions: String::new(),
            lambdas: Vec::new(),
            backend,
            heap_size,
            heap_table_slots,
            print_memory_info,
            lambda_space_size,
            debug_asm,
            dereference,
            type_conversion_context,
        }
    }

    pub fn get_typed_module(
        backend: &dyn Backend,
        module: ASTModule,
        debug_asm: bool,
        print_memory_info: bool,
        dereference: bool,
        print_module: bool,
        statics: &mut Statics,
    ) -> (ASTTypedModule, TypeConversionContext) {
        let enhanced_module = EnhancedASTModule::new(module);

        let mandatory_functions = type_mandatory_functions(&enhanced_module);

        let (mut typed_module, type_conversion_context) = convert_to_typed_module(
            &enhanced_module,
            debug_asm,
            print_memory_info,
            print_module,
            mandatory_functions,
            backend,
            statics,
            dereference,
        );
        typed_enum_functions_creator(backend, &mut typed_module, statics);
        typed_struct_functions_creator(backend, &mut typed_module, statics);
        typed_type_functions_creator(backend, &mut typed_module, statics);
        (typed_module, type_conversion_context)
    }

    pub fn get_std_lib_path() -> String {
        let current_dir = env::current_dir().unwrap();
        println!("current dir {}", current_dir.to_str().unwrap());
        env::var("RASM_STDLIB").unwrap_or(current_dir.join("stdlib").to_str().unwrap().to_owned())
    }

    pub fn asm(&mut self) -> String {
        self.id = 0;
        self.body = String::new();

        self.definitions = String::new();

        // for now main has no context
        let mut context = TypedValContext::new(None);

        let stack = StackVals::new();

        let mut after = String::new();
        let mut before = String::new();

        for statement in &self.module.body.clone() {
            match statement {
                ASTTypedStatement::Expression(e) => match e {
                    ASTTypedExpression::ASTFunctionCallExpression(call) => {
                        let (bf, af, mut lambda_calls) = self.call_function(
                            call,
                            &context,
                            None,
                            "0".into(),
                            None,
                            0,
                            false,
                            &stack,
                        );
                        before.push_str(&bf);

                        Self::insert_on_top(&af.join("\n"), &mut after);

                        self.lambdas.append(&mut lambda_calls);
                    }
                    _ => {
                        panic!("unsupported expression in body {e}");
                    }
                },
                ASTTypedStatement::LetStatement(name, expr, is_const, _let_index) => {
                    let mut new_lambda_calls = self.add_let(
                        &mut context,
                        &stack,
                        &mut after,
                        &mut before,
                        name,
                        expr,
                        None,
                        None,
                        *is_const,
                    );
                    self.lambdas.append(&mut new_lambda_calls);
                }
            }
        }

        self.body.push_str(&before.replace(
            STACK_VAL_SIZE_NAME,
            &(stack.len_of_all() * self.backend.word_len()).to_string(),
        ));
        self.body.push_str(&after);

        // debug!("stack {:?}", stack);
        //        assert_eq!(stack.size(), 0);

        // TODO add a command line argument
        //Parser::print(&self.module);

        self.create_lambdas(self.lambdas.clone(), 0);

        self.create_all_functions();

        let mut asm = String::new();

        self.backend.preamble(&mut asm);

        // +1 because we cleanup the next allocated table slot for every new allocation to be sure that is 0..., so we want to have an extra slot
        self.statics.insert(
            "_heap_table".into(),
            Mem((self.heap_table_slots + 1) * 20, Bytes),
        );
        self.statics.insert(
            "_heap_table_size".into(),
            I32Value(self.heap_table_slots as i32 * 20),
        );
        self.statics.insert("_heap_table_next".into(), I32Value(0));
        self.statics.insert("_heap".into(), Mem(4, Bytes));
        self.statics
            .insert("_heap_size".into(), I32Value(self.heap_size as i32));
        self.statics
            .insert("_heap_buffer".into(), Mem(self.heap_size, Bytes));

        self.statics
            .insert("_lambda_space_stack".into(), Mem(4, Bytes));
        self.statics.insert(
            "_lambda_space_stack_buffer".into(),
            Mem(self.lambda_space_size, Bytes),
        );

        self.statics
            .insert("_reusable_heap_table".into(), Mem(16 * 1024 * 1024, Bytes));
        self.statics.insert(
            "_reusable_heap_table_size".into(),
            I32Value(16 * 1024 * 1024),
        );
        self.statics
            .insert("_reusable_heap_table_next".into(), I32Value(0));

        // command line arguments
        self.statics.insert("_rasm_args".into(), Mem(12, Words));
        self.statics.insert("_NEW_LINE".into(), I32Value(10));
        self.statics.insert("_ESC".into(), I32Value(27));
        self.statics.insert("_for_nprint".into(), Mem(20, Bytes));

        let (declarations, code) = self.statics.generate_code(self.backend.borrow());

        asm.push_str(&declarations);

        CodeGen::add(&mut asm, "SECTION .text", None, false);
        CodeGen::add(&mut asm, "global  main", None, true);
        CodeGen::add(&mut asm, "main:", None, false);

        if self.debug_asm {
            CodeGen::add(&mut asm, "%define LOG_DEBUG 1", None, false);
        }

        let ws = self.backend.word_size();
        CodeGen::add(
            &mut asm,
            &format!("mov     {ws} eax, _heap_buffer"),
            None,
            true,
        );
        CodeGen::add(&mut asm, &format!("mov     {ws} [_heap], eax"), None, true);

        CodeGen::add(
            &mut asm,
            &format!("mov     {ws} eax, _heap_table"),
            None,
            true,
        );
        CodeGen::add(
            &mut asm,
            &format!("mov     {ws} [_heap_table_next],eax"),
            None,
            true,
        );

        CodeGen::add(
            &mut asm,
            &format!("mov     {ws} eax, _lambda_space_stack_buffer"),
            None,
            true,
        );
        CodeGen::add(
            &mut asm,
            &format!("mov     {ws} [_lambda_space_stack], eax"),
            None,
            true,
        );

        CodeGen::add(
            &mut asm,
            &format!("mov    {ws} eax, _reusable_heap_table"),
            None,
            true,
        );
        CodeGen::add(
            &mut asm,
            &format!("mov    {ws} [_reusable_heap_table_next],eax"),
            None,
            true,
        );

        let mut temp_statics = Statics::new();
        let mut evaluator = TextMacroEvaluator::new();

        let code = evaluator.translate(
            self.backend,
            &mut temp_statics,
            None,
            &code,
            self.dereference,
            false,
            &self.module,
        );

        CodeGen::add(&mut asm, &code, None, true);

        // command line arguments
        CodeGen::add(
            &mut asm,
            &format!("push   {}", self.backend.stack_pointer()),
            Some("command line arguments"),
            true,
        );
        CodeGen::add(&mut asm, "push    _rasm_args", None, true);
        // TODO
        CodeGen::add(&mut asm, "call    createCmdLineArguments_0", None, true);
        CodeGen::add(
            &mut asm,
            &format!(
                "add   {},{}",
                self.backend.stack_pointer(),
                2 * self.backend.word_len()
            ),
            None,
            true,
        );

        CodeGen::add(&mut asm, "", None, true);

        self.backend.function_preamble(&mut asm);

        self.backend.reserve_stack(&stack, &mut asm);

        let body = self.body.clone();
        let new_body = self.translate_body(&body);

        asm.push_str(&new_body);

        self.backend.restore_stack(&stack, &mut asm);

        self.backend.function_end(&mut asm, false);

        if self.print_memory_info {
            Self::print_memory_info(&mut asm);
        }

        CodeGen::add(&mut asm, "push   dword 0", None, true);
        // TODO
        CodeGen::add(&mut asm, "call   exitMain_0", None, true);

        asm.push_str(&self.definitions);

        asm
    }

    fn add_let(
        &mut self,
        context: &mut TypedValContext,
        stack: &StackVals,
        after: &mut String,
        before: &mut String,
        name: &str,
        expr: &ASTTypedExpression,
        function_def: Option<&ASTTypedFunctionDef>,
        lambda_space: Option<&LambdaSpace>,
        is_const: bool,
    ) -> Vec<LambdaCall> {
        let wl = self.backend.word_len();
        let bp = self.backend.stack_base_pointer();
        let ws = self.backend.word_size();
        let address_relative_to_bp = stack.reserve(StackEntryType::LetVal, name) * wl;
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
                            if let Some(rt) = return_type {
                                rt.deref().clone()
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
                            call,
                            context,
                            function_def,
                            "0".into(),
                            lambda_space,
                            0,
                            false,
                            stack,
                        ),
                        call.index.clone(),
                    )
                } else {
                    (
                        self.module
                            .functions_by_name
                            .get(&call.function_name.replace("::", "_"))
                            .unwrap()
                            .return_type
                            .clone()
                            .unwrap(),
                        self.call_function(
                            call,
                            context,
                            function_def,
                            "0".into(),
                            lambda_space,
                            0,
                            false,
                            stack,
                        ),
                        call.index.clone(),
                    )
                }
            }
            ASTTypedExpression::Value(value_type, index) => {
                let value = self.backend.value_to_string(value_type);
                let typed_type =
                    get_type_of_typed_expression(&self.module, context, expr, None, &self.statics)
                        .unwrap();

                if is_const {
                    let key = self
                        .statics
                        .add_typed_const(name.to_owned(), typed_type.clone());

                    CodeGen::add(
                        &mut self.body,
                        &format!("mov {ws} [{key}], {}", value),
                        Some(""),
                        true,
                    );
                } else {
                    CodeGen::add(
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
                (typed_type, (String::new(), vec![], vec![]), index.clone())
            }
            ASTTypedExpression::StringLiteral(value) => {
                let label = self.statics.add_str(value);

                let typed_type = ASTTypedType::Builtin(BuiltinTypedTypeKind::String);

                if is_const {
                    let key = self
                        .statics
                        .add_typed_const(name.to_owned(), typed_type.clone());

                    CodeGen::add(
                        &mut self.body,
                        "push ebx",
                        Some(&format!("const {name} string value")),
                        true,
                    );

                    CodeGen::add(
                        &mut self.body,
                        &format!("mov {ws} ebx, [{label}]"),
                        Some(&format!("const {name} string value")),
                        true,
                    );

                    CodeGen::add(
                        &mut self.body,
                        &format!("mov {ws} [{key}], ebx"),
                        Some(&format!("const {name} string value")),
                        true,
                    );

                    CodeGen::add(
                        &mut self.body,
                        "pop ebx",
                        Some(&format!("const {name} string value")),
                        true,
                    );
                } else {
                    CodeGen::add(before, "push   ebx", None, true);

                    CodeGen::add(before, &format!("mov {ws} ebx, [{label}]",), Some(""), true);

                    CodeGen::add(
                        before,
                        &format!(
                            "mov {ws} [{bp} + {}], ebx",
                            -(address_relative_to_bp as i32),
                        ),
                        Some(""),
                        true,
                    );

                    CodeGen::add(before, "pop   ebx", None, true);
                }
                (
                    typed_type,
                    (String::new(), vec![], vec![]),
                    ASTIndex::none(),
                )
            }
            ASTTypedExpression::ValueRef(val_name, index) => {
                if let Some(typed_val_kind) = context.get(val_name) {
                    let (i, typed_type, descr) = match typed_val_kind {
                        TypedValKind::ParameterRef(i, def) => (
                            *i as i32 + 2,
                            def.ast_type.clone(),
                            format!("par {val_name}"),
                        ),
                        TypedValKind::LetRef(_i, def) => {
                            let relative_to_bp_found = stack
                                .find_relative_to_bp(StackEntryType::LetVal, val_name)
                                .unwrap();
                            let index_in_context = -(relative_to_bp_found as i32);
                            (index_in_context, def.clone(), format!("let {val_name}"))
                        }
                    };
                    CodeGen::add(before, "push   ebx", None, true);

                    CodeGen::add(
                        before,
                        &format!(
                            "mov ebx, [{} + {}]",
                            self.backend.stack_base_pointer(),
                            i * self.backend.word_len() as i32
                        ),
                        Some(&format!("let reference to {descr}")),
                        true,
                    );

                    CodeGen::add(
                        before,
                        &format!(
                            "mov {ws} [{bp} + {}], ebx",
                            -(address_relative_to_bp as i32),
                        ),
                        Some(""),
                        true,
                    );

                    CodeGen::add(before, "pop   ebx", None, true);

                    (typed_type, (String::new(), vec![], vec![]), index.clone())
                } else {
                    panic!("Cannot find {name} in context");
                }
            }

            _ => panic!("Unsupported let {:?}", expr),
        };

        if is_const {
            if !bf.is_empty() {
                self.body.push_str(&bf);

                let key = self
                    .statics
                    .add_typed_const(name.to_owned(), ast_typed_type.clone());

                CodeGen::add(
                    &mut self.body,
                    &format!("mov {ws} [{key}], eax"),
                    Some(""),
                    true,
                );
            }

            if self.dereference {
                if let Some(type_name) =
                    CodeGen::get_reference_type_name(&ast_typed_type, &self.module)
                {
                    let entry = self.statics.get_typed_const(name).unwrap();

                    self.backend.call_add_ref(
                        &mut self.body,
                        &format!("[{}]", entry.key),
                        &type_name,
                        &format!("for const {name} : {index}"),
                        &self.module,
                        &mut self.statics,
                    );
                }
            }
        } else {
            context.insert_let(
                name.into(),
                ast_typed_type.clone(),
                Some(address_relative_to_bp / self.backend.word_len()),
            );

            if !bf.is_empty() {
                before.push_str(&bf);
                self.backend
                    .store_function_result_in_stack(before, -(address_relative_to_bp as i32));
            }

            if self.dereference {
                if let Some(type_name) =
                    CodeGen::get_reference_type_name(&ast_typed_type, &self.module)
                {
                    self.backend.call_add_ref(
                        before,
                        &format!("[{bp} - {}]", address_relative_to_bp),
                        &type_name,
                        &format!("for let val {name} : {index}"),
                        &self.module,
                        &mut self.statics,
                    );

                    let deref_str = &self.backend.call_deref(
                        &format!("[{bp} - {}]", address_relative_to_bp),
                        &type_name,
                        &format!("for let val {name}"),
                        &self.module,
                        &mut self.statics,
                    );
                    Self::insert_on_top(deref_str, after);
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

    fn translate_body(&mut self, body: &str) -> String {
        let val_context = ValContext::new(None);

        let new_body = TextMacroEvaluator::new().translate(
            self.backend,
            &mut self.statics,
            None,
            body,
            self.dereference,
            true,
            &self.module,
        );

        self.type_conversion_context.debug_i();

        let mut lines: Vec<String> = new_body.lines().map(|it| it.to_owned()).collect::<Vec<_>>();

        self.backend
            .called_functions(None, &new_body, &val_context, &self.module)
            .iter()
            .for_each(|(m, it)| {
                debug_i!("native call to {:?}, in main", it);
                let filter = it
                    .param_types
                    .iter()
                    .map(|ast_type| Exact(ast_type.clone()))
                    .collect();
                if let Some(new_function_def) = self
                    .type_conversion_context
                    .find_call(&it.name, &it.name, filter, None)
                {
                    debug_i!("converted to {new_function_def}");
                    if it.name != new_function_def.name {
                        lines[it.i] = get_new_native_call(m, &new_function_def.name);
                    }
                } else {
                    // panic!("cannot find call {function_call}");
                    // TODO I hope it is a predefined function like addRef or deref for a tstruct or enum
                    println!("cannot find call to {}", it.name);
                }
            });

        TextMacroEvaluator::new().translate(
            self.backend,
            &mut self.statics,
            None,
            &lines.join("\n"),
            self.dereference,
            false,
            &self.module,
        )
    }

    fn print_memory_info(asm: &mut String) {
        CodeGen::add(asm, "call   printAllocated_0", None, true);
        CodeGen::add(asm, "call   printTableSlotsAllocated_0", None, true);
    }

    fn create_all_functions(&mut self) {
        for function_def in self.functions().clone().values() {
            // ValContext ???
            if !function_def.inline {
                let lambda_calls = self.add_function_def(
                    function_def,
                    None,
                    &TypedValContext::new(None),
                    0,
                    false,
                );
                self.create_lambdas(lambda_calls, 0);
            }
        }
    }

    fn create_lambdas(&mut self, lambdas: Vec<LambdaCall>, indent: usize) {
        let mut lambda_calls = Vec::new();
        for lambda_call in lambdas {
            //debug!("Creating lambda {}", lambda_call.def.name);
            lambda_calls.append(&mut self.add_function_def(
                &lambda_call.def,
                Some(&lambda_call.space),
                lambda_call.space.get_context(),
                indent,
                true,
            ));
            //Parser::print_function_def(&lambda_call.def);
            lambda_calls.extend(
                lambda_call
                    .space
                    .get_ref_functions()
                    .iter()
                    .flat_map(|it| {
                        self.add_function_def(
                            it,
                            None,
                            lambda_call.space.get_context(),
                            indent,
                            false,
                        )
                    })
                    .collect::<Vec<_>>(),
            );
        }
        if !lambda_calls.is_empty() {
            self.create_lambdas(lambda_calls, indent + 1);
        }
    }

    fn add_function_def(
        &mut self,
        function_def: &ASTTypedFunctionDef,
        lambda_space: Option<&LambdaSpace>,
        parent_context: &TypedValContext,
        indent: usize,
        is_lambda: bool,
    ) -> Vec<LambdaCall> {
        debug!(
            "{}Adding function def {}",
            " ".repeat(indent * 4),
            function_def.name
        );

        let mut lambda_calls = Vec::new();

        CodeGen::add(
            &mut self.definitions,
            &format!("; function {}", function_def),
            None,
            false,
        );
        CodeGen::add(
            &mut self.definitions,
            &format!("{}:", function_def.name),
            None,
            false,
        );

        self.backend.function_preamble(&mut self.definitions);
        if function_def.return_type.is_none() {
            CodeGen::add(&mut self.definitions, "push   eax", None, true);
        }

        let mut before = String::new();

        if is_lambda {
            CodeGen::add(
                &mut self.definitions,
                "push   edx",
                Some("lambda space address"),
                true,
            );
            CodeGen::add(
                &mut self.definitions,
                &format!(
                    "mov     edx, [{}+{}]",
                    self.backend.stack_base_pointer(),
                    self.backend.word_len() * 2
                ),
                Some("The address to the lambda space for inline lambda param"),
                true,
            );
            /*
            CodeGen::add(
                &mut self.definitions,
                "mov   dword edx, [edx]",
                Some("lambda space address"),
                true,
            );

             */
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

        let stack = StackVals::new();

        if is_lambda {
            stack.reserve(StackEntryType::Other, "push edx for lambda space");
        }

        if function_def.return_type.is_none() {
            stack.reserve(
                StackEntryType::Other,
                "push eax for no return type functions",
            );
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
                                        call_expression,
                                        &context,
                                        Some(function_def),
                                        "0".into(),
                                        lambda_space,
                                        indent + 1,
                                        false,
                                        &stack,
                                    );

                                    before.push_str(&bf);

                                    Self::insert_on_top(&af.join("\n"), &mut after);

                                    lambda_calls.append(&mut lambda_calls_);
                                }
                                ASTTypedExpression::ValueRef(val, index) => {
                                    // TODO I don't like to use FunctionCallParameters to do this, probably I need another struct to do only the calculation of the address to get

                                    let mut parameters = FunctionCallParameters::new(
                                        self.backend.borrow(),
                                        Vec::new(),
                                        function_def.inline,
                                        true,
                                        &stack,
                                        self.dereference,
                                        self.id,
                                    );

                                    self.id += 1;

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
                                    );

                                    before.push_str(&parameters.before());

                                    Self::insert_on_top(&parameters.after().join("\n"), &mut after);
                                }
                                ASTTypedExpression::StringLiteral(value) => {
                                    let label = self.statics.add_str(value);

                                    CodeGen::add(
                                        &mut before,
                                        &format!(
                                            "mov     {} eax, [{label}]",
                                            self.backend.word_size()
                                        ),
                                        None,
                                        true,
                                    );
                                }
                                ASTTypedExpression::Lambda(lambda_def) => {
                                    if let Some(ASTTypedType::Builtin(
                                        BuiltinTypedTypeKind::Lambda {
                                            parameters,
                                            return_type,
                                        },
                                    )) = &function_def.return_type
                                    {
                                        let rt = if let Some(rt) = return_type {
                                            rt.deref().clone()
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

                                        let def = ASTTypedFunctionDef {
                                            //name: format!("{}_{}_{}_lambda{}", parent_def_description, function_call.function_name, param_name, self.id),
                                            name: format!("lambda{}", self.id),
                                            parameters: lambda_parameters, // parametrs are calculated later
                                            return_type: Some(rt),
                                            body: ASTTypedFunctionBody::RASMBody(
                                                lambda_def.clone().body,
                                            ),
                                            inline: false,
                                            generic_types: LinkedHashMap::new(),
                                        };

                                        self.id += 1;

                                        let mut parameters = FunctionCallParameters::new(
                                            self.backend.borrow(),
                                            Vec::new(),
                                            function_def.inline,
                                            true,
                                            &stack,
                                            self.dereference,
                                            self.id,
                                        );

                                        self.id += 1;

                                        let new_lambda_space = parameters.add_lambda(
                                            &def,
                                            lambda_space,
                                            &context,
                                            None,
                                            &mut self.statics,
                                            &mut self.module,
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
                                    let v = self.backend.value_to_string(value_type);

                                    CodeGen::add(
                                        &mut before,
                                        &format!("mov     {} eax, {}", self.backend.word_size(), v),
                                        None,
                                        true,
                                    );
                                }
                                ASTTypedExpression::Any(_) => panic!(),
                            }
                        }
                        ASTTypedStatement::LetStatement(name, expr, is_const, _let_index) => {
                            let mut new_lambda_calls = self.add_let(
                                &mut context,
                                &stack,
                                &mut after,
                                &mut before,
                                name,
                                expr,
                                Some(function_def),
                                lambda_space,
                                *is_const,
                            );
                            lambda_calls.append(&mut new_lambda_calls);
                        }
                    }
                }
            }
            ASTTypedFunctionBody::ASMBody(body) => {
                let function_call_parameters = FunctionCallParameters::new(
                    self.backend.borrow(),
                    function_def.parameters.clone(),
                    false,
                    false,
                    &stack,
                    self.dereference,
                    self.id,
                );

                self.id += 1;

                let new_body =
                    function_call_parameters.resolve_asm_parameters(body, "0".into(), indent);
                before.push_str(&new_body);
            }
        }

        self.backend.reserve_stack(&stack, &mut self.definitions);

        self.definitions.push_str(&before.replace(
            STACK_VAL_SIZE_NAME,
            &(stack.len_of_all() * self.backend.word_len()).to_string(),
        ));

        self.definitions.push_str(&after);

        self.backend.restore_stack(&stack, &mut self.definitions);

        if is_lambda {
            CodeGen::add(
                &mut self.definitions,
                "pop   edx",
                Some("lambda space address"),
                true,
            );
        }

        if function_def.return_type.is_none() {
            CodeGen::add(&mut self.definitions, "\npop   eax", None, true);
        }

        self.backend.function_end(&mut self.definitions, true);

        lambda_calls
    }

    fn call_function(
        &mut self,
        function_call: &ASTTypedFunctionCall,
        context: &TypedValContext,
        parent_def: Option<&ASTTypedFunctionDef>,
        added_to_stack: String,
        lambda_space: Option<&LambdaSpace>,
        indent: usize,
        is_lambda: bool,
        stack_vals: &StackVals,
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
            )
        } else if let Some(kind) = context.get(&function_call.function_name) {
            let (index, ast_type) = match kind {
                TypedValKind::ParameterRef(index, par) => (*index as i32 + 2, par.ast_type.clone()),
                TypedValKind::LetRef(index, ast_type) => {
                    // TODO I think it's not really needed because every index I put here, it works...
                    let index_relative_to_bp = match stack_vals
                        .find_relative_to_bp(StackEntryType::LetVal, &function_call.function_name)
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
                        let name = format!("p_{}", self.id);
                        self.id += 1;
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

    fn call_function_(
        &mut self,
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
    ) -> Vec<LambdaCall> {
        /*
        let parent_def_description = if let Some(pd) = parent_def {
            &pd.name
        } else {
            "main"
        };

         */

        let mut lambda_calls = Vec::new();

        CodeGen::add_empty_line(before);

        if inline {
            CodeGen::add(
                before,
                &format!(
                    "; inlining function {}, added to stack {}",
                    function_call.function_name, added_to_stack
                ),
                None,
                true,
            );
        } else {
            CodeGen::add(
                before,
                &format!(
                    "; calling function {}, added to stack {}",
                    function_call.function_name, added_to_stack
                ),
                None,
                true,
            );
        }

        let mut call_parameters = FunctionCallParameters::new(
            self.backend.borrow(),
            parameters.clone(),
            inline,
            false,
            stack_vals,
            self.dereference,
            self.id,
        );

        self.id += 1;

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
                        let label = self.statics.add_str(value);
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
                        let mut added_to_stack = added_to_stack.clone();
                        added_to_stack.push_str(" + ");
                        added_to_stack.push_str(&call_parameters.to_remove_from_stack_name());
                        let (bf, af, mut inner_lambda_calls) = self.call_function(
                            call,
                            context,
                            *parent_def,
                            added_to_stack,
                            lambda_space_opt,
                            indent + 1,
                            false,
                            stack_vals,
                        );

                        call_parameters.push(&bf);

                        //after.insert(0, af.join("\n"));
                        call_parameters.add_on_top_of_after(&af.join("\n"));

                        let mut statics = self.statics.clone();

                        call_parameters.add_function_call(
                            &self.module,
                            &format!("{param_name} = {} : {}", &call.function_name, call.index),
                            param_type.clone(),
                            &mut statics,
                        );

                        self.statics = statics;

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

                        let rt = return_type.map(|r| r.deref().clone());

                        let mut def = ASTTypedFunctionDef {
                            //name: format!("{}_{}_{}_lambda{}", parent_def_description, function_call.function_name, param_name, self.id),
                            name: format!("lambda{}", self.id),
                            parameters: Vec::new(), // parametrs are calculated later
                            return_type: rt,
                            body: ASTTypedFunctionBody::RASMBody(lambda_def.clone().body),
                            inline: false,
                            generic_types: LinkedHashMap::new(),
                        };

                        self.id += 1;

                        debug!("{}Adding lambda {}", " ".repeat(indent * 4), param_name);

                        let lambda_space = call_parameters.add_lambda(
                            &def,
                            lambda_space_opt,
                            context,
                            None,
                            &mut self.statics,
                            &mut self.module,
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
                    ASTTypedExpression::Any(_) => panic!(),
                }
            }
        }

        //before.push_str(&call_parameters.before());
        before.push_str(&call_parameters.before().replace(
            &call_parameters.to_remove_from_stack_name(),
            &(call_parameters.to_remove_from_stack() * self.backend.word_len()).to_string(),
        ));

        if inline {
            if let Some(ASTTypedFunctionBody::ASMBody(body)) = &body {
                /*let mut added_to_stack = added_to_stack;
                added_to_stack.push_str(&format!(
                    " + {}",
                    stack_vals.len() * self.backend.word_len()
                ));

                 */

                before.push_str(&call_parameters.resolve_asm_parameters(
                    body,
                    added_to_stack,
                    indent,
                ));
            } else {
                panic!("Only asm can be inlined, for now...");
            }
        } else if is_lambda {
            if let Some(address) =
                lambda_space_opt.and_then(|it| it.get_index(&function_call.function_name))
            {
                CodeGen::add(before, "mov eax, edx", None, true);
                // we add the address to the "lambda space" as the last parameter of the lambda
                CodeGen::add(
                    before,
                    &format!("add eax, {}", (address + 2) * self.backend.word_len()),
                    Some("address to the allocation table of the \"lambda space\""),
                    true,
                );
                CodeGen::add(
                    before,
                    "mov eax, [eax]",
                    Some("address to the \"lambda space\""),
                    true,
                );
                CodeGen::add(
                    before,
                    "mov eax, [eax]",
                    Some("address to the \"lambda space\""),
                    true,
                );
                CodeGen::add(
                    before,
                    &format!("push {} eax", self.backend.pointer_size()),
                    Some("address to the \"lambda space\""),
                    true,
                );
                CodeGen::add(
                    before,
                    "call [eax]",
                    Some(&format!(
                        "Calling function {} : {}",
                        function_call.function_name, function_call.index
                    )),
                    true,
                );
                CodeGen::add(
                    before,
                    &format!(
                        "add  {}, {}",
                        self.backend.stack_pointer(),
                        self.backend.word_len()
                    ),
                    None,
                    true,
                );
            } else if let Some(kind) = context.get(&function_call.function_name) {
                let index = match kind {
                    TypedValKind::ParameterRef(index, _) => *index as i32 + 2,
                    TypedValKind::LetRef(_, _) => {
                        let relative_to_bp_found = stack_vals
                            .find_relative_to_bp(
                                StackEntryType::LetVal,
                                &function_call.function_name,
                            )
                            .unwrap();
                        -(relative_to_bp_found as i32)
                    }
                };

                CodeGen::add(
                    before,
                    "",
                    Some(&format!(
                        "calling lambda parameter reference to {}",
                        &function_call.function_name
                    )),
                    true,
                );
                CodeGen::add(
                    before,
                    &format!(
                        "mov eax, [{} + {}]",
                        self.backend.stack_base_pointer(),
                        index * self.backend.word_len() as i32
                    ),
                    None,
                    true,
                );
                CodeGen::add(
                    before,
                    &format!("mov {} eax, [eax]", self.backend.word_size(),),
                    None,
                    true,
                );
                // we add the address to the "lambda space" as the last parameter to the lambda
                CodeGen::add(
                    before,
                    &format!("push {} eax", self.backend.pointer_size()),
                    Some("address to the \"lambda space\""),
                    true,
                );
                CodeGen::add(
                    before,
                    "call [eax]",
                    Some(&format!(
                        "Calling function {} : {}",
                        function_call.function_name, function_call.index
                    )),
                    true,
                );
                CodeGen::add(
                    before,
                    &format!(
                        "add  {}, {}",
                        self.backend.stack_pointer(),
                        self.backend.word_len()
                    ),
                    None,
                    true,
                );
            } else {
                panic!(
                    "lambda space does not contain {}",
                    function_call.function_name
                );
            }
        } else {
            CodeGen::add(
                before,
                &format!("call    {}", address_to_call),
                Some(&format!(
                    "Calling function {} : {}",
                    function_call.function_name, function_call.index
                )),
                true,
            );
        }

        if call_parameters.to_remove_from_stack() > 0 {
            let sp = self.backend.stack_pointer();
            let wl = self.backend.word_len();

            CodeGen::add(
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

        after.insert(0, call_parameters.after().join("\n"));

        if inline {
            CodeGen::add(
                before,
                &format!("; end inlining function {}", function_call.function_name),
                None,
                true,
            );
        } else {
            CodeGen::add(
                before,
                &format!("; end calling function {}", function_call.function_name),
                None,
                true,
            );
        }
        CodeGen::add_empty_line(before);

        lambda_calls
    }

    fn add_val(
        &mut self,
        context: &TypedValContext,
        lambda_space_opt: &Option<&LambdaSpace>,
        indent: &usize,
        call_parameters: &mut FunctionCallParameters,
        param_name: &str,
        val_name: &str,
        error_msg: &str,
        stack_vals: &StackVals,
        ast_index: &ASTIndex,
    ) {
        if let Some(val_kind) = context.get(val_name) {
            match val_kind {
                TypedValKind::ParameterRef(index, par) => {
                    call_parameters.add_parameter_ref(
                        param_name.into(),
                        val_name,
                        &par.ast_type,
                        *index,
                        lambda_space_opt,
                        *indent,
                    );
                }

                TypedValKind::LetRef(_index, ast_typed_type) => {
                    let index_in_context =
                        stack_vals.find_relative_to_bp(StackEntryType::LetVal, val_name);

                    call_parameters.add_let_val_ref(
                        param_name.into(),
                        val_name,
                        ast_typed_type,
                        index_in_context,
                        lambda_space_opt,
                        *indent,
                        ast_index,
                        &self.module,
                        &mut self.statics,
                    )
                }
            }
        } else if let Some(entry) = self.statics.get_typed_const(val_name) {
            call_parameters.add_label(
                param_name,
                entry.key.clone(),
                Some(&format!("static {val_name}")),
            )
        } else {
            panic!("Error adding val {}: {}", param_name, error_msg);
        }
    }

    pub fn add(out: &mut String, code: &str, comment: Option<&str>, indent: bool) {
        if code.is_empty() {
            out.push('\n');
        } else {
            let max = 80;
            let s = format!("{:width$}", code, width = max);
            //assert_eq!(s.len(), max, "{}", s);
            if indent {
                out.push_str("    ");
            }
            out.push_str(&s);
        }

        if let Some(c) = comment {
            if code.is_empty() {
                out.push_str("    ");
            }
            out.push_str("; ");
            out.push_str(c);
        }
        out.push('\n');
    }

    pub fn insert_on_top(src: &str, dest: &mut String) {
        for line in src.lines().rev() {
            dest.insert_str(0, &(line.to_string() + "\n"));
        }
    }

    pub fn get_reference_type_name(
        ast_type: &ASTTypedType,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Option<String> {
        match ast_type {
            ASTTypedType::Builtin(BuiltinTypedTypeKind::String) => Some("str".into()),
            ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda { .. }) => Some("_fn".into()),
            ASTTypedType::Enum { name } => Some(name.clone()),
            ASTTypedType::Struct { name } => Some(name.clone()),
            ASTTypedType::Type { name } => {
                if let Some(t) = type_def_provider.get_type_def_by_name(name) {
                    if t.is_ref {
                        Some(name.clone())
                    } else {
                        None
                    }
                } else {
                    panic!("Cannot find type {name}");
                }
            }
            _ => None,
        }
    }

    pub fn add_empty_line(out: &mut String) {
        out.push('\n');
    }

    fn functions(&self) -> &LinkedHashMap<String, ASTTypedFunctionDef> {
        &self.module.functions_by_name
    }
}
