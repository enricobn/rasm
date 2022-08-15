mod function_call_parameters;
pub mod backend;
pub mod stack;

use std::collections::HashSet;
use std::ops::Deref;
use linked_hash_map::{Iter, LinkedHashMap};
use log::{debug, info};
use crate::codegen::backend::Backend;
use crate::codegen::function_call_parameters::FunctionCallParameters;
use crate::codegen::MemoryUnit::{Bytes, Words};
use crate::codegen::MemoryValue::Mem;
use crate::codegen::stack::Stack;
use crate::parser::ast::{ASTEnumDef, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef, ASTStructDef};

use crate::transformations::enum_functions_creator::enum_functions_creator;
use crate::transformations::struct_functions_creator::struct_functions_creator;
use crate::type_check::convert;
use crate::type_check::typed_ast::{ASTTypedEnumDef, ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedStructDef, ASTTypedType, BuiltinTypedTypeKind};

pub struct CodeGen<'a> {
    module: ASTTypedModule,
    id: usize,
    statics: LinkedHashMap<String, MemoryValue>,
    // key=memory_label
    body: String,
    definitions: String,
    lambdas: Vec<LambdaCall>,
    functions: LinkedHashMap<String, ASTTypedFunctionDef>,
    backend: &'a dyn Backend,
    functions_called: HashSet<String>,
    heap_size: usize,
    heap_table_slots: usize,
    print_memory_info: bool,
    lambda_space_size: usize,
    debug_asm: bool,
    dereference: bool
}

#[derive(Debug, Clone)]
pub enum MemoryValue {
    StringValue(String),
    I32Value(i32),
    Mem(usize, MemoryUnit),
}

#[derive(Debug, Clone)]
pub enum MemoryUnit {
    Bytes,
    Words,
}

#[derive(Clone, Debug)]
pub struct ValContext {
    pub value_to_address: LinkedHashMap<String, VarKind>,
}

impl ValContext {
    pub fn new(parent_context: Option<&ValContext>) -> Self {
        let mut map = LinkedHashMap::new();
        if let Some(pc) = parent_context {
            for (key, value) in pc.value_to_address.iter() {
                map.insert(key.clone(), value.clone());
            }
        }
        Self { value_to_address: map }
    }

    pub fn insert(&mut self, key: String, value: VarKind) -> Option<VarKind> {
        self.value_to_address.insert(key, value)
    }

    pub fn get(&self, key: &str) -> Option<&VarKind> {
        self.value_to_address.get(key)
    }

    pub fn iter(&self) -> Iter<String, VarKind> {
        self.value_to_address.iter()
    }

    pub fn names(&self) -> Vec<&String> {
        self.value_to_address.keys().collect()
    }

    fn is_empty(&self) -> bool {
        self.value_to_address.is_empty()
    }
}

#[derive(Clone, Debug)]
pub enum VarKind {
    ParameterRef(usize, ASTParameterDef)
}


#[derive(Clone, Debug)]
pub struct TypedValContext {
    pub value_to_address: LinkedHashMap<String, TypedVarKind>,
}

impl TypedValContext {
    pub fn new(parent_context: Option<&TypedValContext>) -> Self {
        let mut map = LinkedHashMap::new();
        if let Some(pc) = parent_context {
            for (key, value) in pc.value_to_address.iter() {
                map.insert(key.clone(), value.clone());
            }
        }
        Self { value_to_address: map }
    }

    pub fn insert(&mut self, key: String, value: TypedVarKind) -> Option<TypedVarKind> {
        self.value_to_address.insert(key, value)
    }

    pub fn get(&self, key: &str) -> Option<&TypedVarKind> {
        self.value_to_address.get(key)
    }

    pub fn iter(&self) -> Iter<String, TypedVarKind> {
        self.value_to_address.iter()
    }

    pub fn names(&self) -> Vec<&String> {
        self.value_to_address.keys().collect()
    }

    fn is_empty(&self) -> bool {
        self.value_to_address.is_empty()
    }
}

#[derive(Clone, Debug)]
pub enum TypedVarKind {
    ParameterRef(usize, ASTTypedParameterDef)
}

#[derive(Debug, Clone)]
struct LambdaCall {
    def: ASTTypedFunctionDef,
    space: LambdaSpace,
}

#[derive(Debug, Clone)]
pub struct LambdaSpace {
    parameters_indexes: LinkedHashMap<String, usize>,
    context: TypedValContext,
}

#[derive(Debug, Clone)]
pub struct EnhancedASTModule {
    pub body: Vec<ASTFunctionCall>,
    /// key: logical name
    pub functions_by_name: LinkedHashMap<String, ASTFunctionDef>,
    pub enums: Vec<ASTEnumDef>,
    pub structs: Vec<ASTStructDef>,
    pub native_body: String,
    pub statics: LinkedHashMap<String, MemoryValue>,
}

impl EnhancedASTModule {

    pub fn new(module: &ASTModule) -> Self {
        let mut functions_by_name = LinkedHashMap::new();

        module.functions.iter().for_each(|it| {
            functions_by_name.insert(it.name.clone(), it.clone());
        });

        Self { body: module.body.clone(), functions_by_name, enums: module.enums.clone(), structs: module.structs.clone(), native_body: String::new(), statics: LinkedHashMap::new() }
    }

}

impl LambdaSpace {
    fn new(context: TypedValContext) -> Self {
        LambdaSpace { parameters_indexes: LinkedHashMap::new(), context }
    }

    fn add_context_parameter(&mut self, name: String, index: usize) {
        self.parameters_indexes.insert(name, index);
    }

    fn get_index(&self, name: &str) -> Option<usize> {
        self.parameters_indexes.get(name).cloned()
    }
}

impl<'a> CodeGen<'a> {
    pub fn new(backend: &'a dyn Backend, module: ASTModule, lambda_space_size: usize, heap_size: usize, heap_table_slots: usize, debug_asm: bool, print_memory_info: bool, dereference: bool) -> Self {
        let module = enum_functions_creator(backend, &EnhancedASTModule::new(&module));
        let module = struct_functions_creator(backend, &module);
        let module = convert(&module, debug_asm, print_memory_info);

        Self {
            module,
            body: String::new(),
            statics: LinkedHashMap::new(),
            id: 0,
            definitions: String::new(),
            lambdas: Vec::new(),
            functions: LinkedHashMap::new(),
            backend,
            functions_called: HashSet::new(),
            heap_size,
            heap_table_slots,
            print_memory_info,
            lambda_space_size,
            debug_asm,
            dereference,
        }
    }

    pub fn asm(&mut self) -> String {
        self.id = 0;
        self.statics = self.module.statics.clone();
        self.body = self.module.native_body.clone();
        self.definitions = String::new();
        self.functions = self.module.functions_by_name.clone();

        // for now main has no context
        let main_context = TypedValContext::new(None);

        let stack = Stack::new();

        let mut after = String::new();

        for function_call in &self.module.body.clone() {
            let (bf, af, mut lambda_calls) = self.call_function(function_call, &main_context, None, 0, None,
                                                                0, false, &stack);
            self.body.push_str(&bf);

            Self::insert_on_top(&af.join("\n"), &mut after);

            self.lambdas.append(&mut lambda_calls);
        }

        self.body.push_str(&after);

        info!("stack {:?}", stack);
        assert_eq!(stack.size(), 0);

        // TODO add a command line argument
        //Parser::print(&self.module);

        self.create_lambdas(self.lambdas.clone(), 0);

        self.create_all_functions();

        // create only used functions

        //self.create_only_used_functions();

        let mut asm = String::new();

        let mut bss = String::new();

        // +1 because we cleanup the next allocated table slot for every new allocation to be sure that is 0..., so we want to have an extra slot
        self.statics.insert("_heap_table".into(), Mem((self.heap_table_slots + 1)  * 16, Bytes));
        self.statics.insert("_heap_table_size".into(), MemoryValue::I32Value(self.heap_table_slots as i32 * 16));
        self.statics.insert("_heap".into(), Mem(4, Bytes));
        self.statics.insert("_heap_size".into(), MemoryValue::I32Value(self.heap_size as i32));
        self.statics.insert("_heap_buffer".into(), Mem(self.heap_size, Bytes));

        self.statics.insert("_original_heap".into(), Mem(4, Bytes));
        self.statics.insert("_lambda_space_stack".into(), Mem(4, Bytes));
        self.statics.insert("_lambda_space_stack_buffer".into(), Mem(self.lambda_space_size, Bytes));

        self.statics.insert("_scope_stack".into(), Mem(4, Bytes));
        self.statics.insert("_scope_stack_buffer".into(), Mem(1024, Bytes));

        self.statics.insert("_rasm_buffer_10b".into(), Mem(10, Bytes));
        // command line arguments
        self.statics.insert("_rasm_args".into(), Mem(12, Words));

        if !self.statics.is_empty() {
            let mut data = String::new();

            let mut keys: Vec<&String> = self.statics.keys().collect();
            // sorted for test purposes
            keys.sort();

            for id in keys.iter() {
                let mut def = String::new();
                def.push_str(id);

                match self.statics.get(*id).unwrap() {
                    MemoryValue::StringValue(s) => {
                        def.push_str("\tdb    ");
                        def.push_str(&format!("'{}', 0h", s));
                        CodeGen::add(&mut data, &def, None, true);
                    }
                    MemoryValue::I32Value(i) => {
                        def.push_str("\tdd    ");
                        def.push_str(&format!("{}", i));
                        CodeGen::add(&mut data, &def, None, true);
                    }
                    Mem(len, unit) => {
                        match unit {
                            Bytes => {
                                def.push_str("\tresb ");
                            }
                            Words => {
                                def.push_str("\tresw ");
                            }
                        }
                        def.push_str(&format!("{}", len));
                        CodeGen::add(&mut bss, &def, None, true);
                    }
                }
            }
            asm.push_str("SECTION .data\n");
            asm.push_str("    timeval:\n");
            asm.push_str("        tv_sec  dd 0\n");
            asm.push_str("        tv_usec dd 0\n");
            asm.push_str("    _ESC	db    27, 0h\n");
            asm.push_str(&data);
        }

        CodeGen::add(&mut asm, "section .bss", None, true);
        asm.push_str(&bss);

        CodeGen::add(&mut asm, "SECTION .text", None, true);
        CodeGen::add(&mut asm, "global  main", None, true);
        CodeGen::add(&mut asm, "", None, true);
        CodeGen::add(&mut asm, "main:", None, false);

        if self.debug_asm {
            CodeGen::add(&mut asm, "%define LOG_DEBUG 1", None, false);
        }

        // command line arguments
        for i in 0..12 {
            CodeGen::add(&mut asm, &format!("mov     eax,[esp + {}]", i * self.backend.word_len()), Some(&format!("command line argument {}", i)), true);
            CodeGen::add(&mut asm, &format!("mov     [_rasm_args + {}], eax", i * self.backend.word_len()), None, true);
        }

        CodeGen::add(&mut asm, "mov     eax, _heap_buffer", None, true);
        CodeGen::add(&mut asm, "mov     [_heap], eax", None, true);
        CodeGen::add(&mut asm, "mov     [_original_heap], eax", None, true);
        CodeGen::add(&mut asm, "mov     eax, _lambda_space_stack_buffer", None, true);
        CodeGen::add(&mut asm, "mov     [_lambda_space_stack], eax", None, true);
        CodeGen::add(&mut asm, "mov     eax, _scope_stack_buffer", None, true);
        CodeGen::add(&mut asm, "mov     [_scope_stack], eax", None, true);
        CodeGen::add(&mut asm, "", None, true);
        CodeGen::add(&mut asm, "push    ebp       ; save old call frame", None, true);
        CodeGen::add(&mut asm, "mov     ebp, esp  ; initialize new call frame", None, true);

        asm.push_str(&self.body);

        CodeGen::add(&mut asm, "pop    ebp       ; restore old call frame", None, true);

        if self.print_memory_info {
            Self::print_memory_info(&mut asm);
        }

        CodeGen::add(&mut asm, "push   dword 0", None, true);
        CodeGen::add(&mut asm, "call   exit", None, true);

        asm.push_str(&self.definitions);

        asm
    }

    fn print_memory_info(mut asm: &mut String) {
        CodeGen::add(&mut asm, "call   printAllocated", None, true);
        CodeGen::add(&mut asm, "call   printTableSlotsAllocated", None, true);
    }

    fn create_all_functions(&mut self) {
        //debug!("create_all_functions, {:?}", self.functions.values().map(|it| it.name.clone()).collect::<Vec<String>>());
        println!("create_all_functions, {:?}", self.functions.values().map(|it| it.name.clone()).collect::<Vec<String>>());
        for function_def in self.functions.clone().values() {
            // VarContext ???
            let vec1 = self.add_function_def(function_def, None, &TypedValContext::new(None), 0, false);
            self.create_lambdas(vec1, 0);
        }
    }

    fn create_lambdas(&mut self, lambdas: Vec<LambdaCall>, indent: usize) {
        let mut lambda_calls = Vec::new();
        for lambda_call in lambdas {
            //debug!("Creating lambda {}", lambda_call.def.name);
            lambda_calls.append(&mut self.add_function_def(&lambda_call.def, Some(&lambda_call.space), &lambda_call.space.context, indent, true));
            //Parser::print_function_def(&lambda_call.def);
        }
        if !lambda_calls.is_empty() {
            self.create_lambdas(lambda_calls, indent + 1);
        }
    }

    fn add_function_def(&mut self, function_def: &ASTTypedFunctionDef, lambda_space: Option<&LambdaSpace>, parent_context: &TypedValContext, indent: usize, is_lambda: bool) -> Vec<LambdaCall> {
        debug!("{}Adding function def {}", " ".repeat(indent * 4), function_def.name);
        println!("add_function_def {}", function_def.name);

        let mut lambda_calls = Vec::new();
        CodeGen::add(&mut self.definitions, &format!("{}:", function_def.name), None, false);

        let sp = self.backend.stack_pointer();
        let bp = self.backend.stack_base_pointer();

        CodeGen::add(&mut self.definitions, &format!("push    {}", bp), None, true);
        CodeGen::add(&mut self.definitions, &format!("mov     {},{}", bp, sp), None, true);

        let mut context = TypedValContext::new(Some(parent_context));

        // I think it's useless
        let mut i = if is_lambda {
            // one because the first parameter is the address to the lambda space
            1
        } else {
            0
        };

        for par in function_def.parameters.iter() {
            debug!("{}Inserted parameter {} in context, offset {}", " ".repeat((indent + 1) * 4), par.name, i);
            context.insert(par.name.clone(), TypedVarKind::ParameterRef(i, par.clone()));
            i += 1;
        }

        let stack = Stack::new();

        let mut after = String::new();

        match &function_def.body {
            ASTTypedFunctionBody::RASMBody(calls) => {
                for call in calls {
                    match call {
                        ASTTypedExpression::ASTFunctionCallExpression(call_expression) => {
                            let (bf, af, mut lambda_calls_) = self.call_function(call_expression, &context, Some(function_def), 0,
                                                                                 lambda_space, indent + 1, false, &stack);

                            assert_eq!(stack.size(), 0, "function def {} calling {} stack {:?}", function_def.name, call_expression.function_name, stack);

                            self.definitions.push_str(&bf);

                            Self::insert_on_top(&af.join("\n"), &mut after);

                            lambda_calls.append(&mut lambda_calls_);
                        }
                        ASTTypedExpression::Val(val) => {
                            // TODO I don't like to use FunctionCallParameters to do this, probably I need another struct to do only the calculation of the address to get
                            let tmp_stack = Stack::new();

                            let mut parameters = FunctionCallParameters::new(self.backend, Vec::new(), function_def.inline, true, &tmp_stack, self.dereference);
                            self.add_val(&context, &lambda_space, &indent, &mut parameters, val, val, "");

                            self.definitions.push_str(&parameters.before());

                            Self::insert_on_top(&parameters.after().join("\n"), &mut after);
                        }
                        ASTTypedExpression::StringLiteral(_) => {
                            panic!("unsupported");
                        }
                        ASTTypedExpression::Number(_) => {
                            panic!("unsupported");
                        }
                        ASTTypedExpression::Lambda(_) => {
                            panic!("unsupported");
                        }
                    }
                }
            }
            ASTTypedFunctionBody::ASMBody(s) => {
                let function_call_parameters = FunctionCallParameters::new(self.backend, function_def.parameters.clone(), false, false,
                                                                           &stack, self.dereference);

                self.definitions.push_str(
                    &function_call_parameters.resolve_asm_parameters(s, 0, indent));

                // we parse asm body to collect the calls
                s.lines().filter(|it| it.contains("call") && !it.contains('[') && !it.contains('$'))
                    .map(|it| {
                        let pos = it.find("call").unwrap();
                        let s = it.split_at(pos + 4).1.trim();
                        String::from_iter(s.chars().take_while(|it| it.is_alphanumeric()))
                    }).all(|it| self.functions_called.insert(it));
            }
        }

        if let Some(ASTTypedType::Enum { name: _ }) = &function_def.return_type.clone().map(|it| it.ast_type) {
            let function_def_name = function_def.name.clone();
            let mut out = String::new();
            CodeGen::call_add_ref(&mut out, self.backend, "eax",
                                  &format!("function {} return", function_def_name));

            self.definitions.push_str(&out);
        } else if let Some(ASTTypedType::Struct { name: _ }) = &function_def.return_type.clone().map(|it| it.ast_type) {
            //println!("Parametric({name})");
            let function_def_name = function_def.name.clone();
            let mut out = String::new();
            CodeGen::call_add_ref(&mut out, self.backend, "eax",
                                  &format!("function {} return", function_def_name));

            self.definitions.push_str(&out);
        }

        for (i, param) in function_def.parameters.iter().enumerate() {
            if let ASTTypedType::Enum { name: _ } = param.type_ref.ast_type {
                /*
                self.call_deref(&format!("[{} + {}]", self.backend.stack_base_pointer(), (i + 2) * self.backend.word_len()),
                            &format!("function {} param {} : {:?}", function_def.name, param.name, param.type_ref));

                 */
            } else if let ASTTypedType::Struct { name: _ } = param.type_ref.ast_type {
                /*
                self.call_deref(&format!("[{} + {}]", self.backend.stack_base_pointer(), (i + 2) * self.backend.word_len()),
                            &format!("function {} param {} : {:?}", function_def.name, param.name, param.type_ref));

                 */
            }
        }

        self.definitions.push_str(&after);

        CodeGen::add(&mut self.definitions, &format!("pop     {}", bp), None, true);
        CodeGen::add(&mut self.definitions, "ret", None, true);
        lambda_calls
    }

    pub fn is_constructor(&self, function_name: &str) -> bool {
        self.is_enum_parametric_variant_constructor(function_name) || self.is_struct_constructor(function_name)
    }

    pub fn is_enum_parametric_variant_constructor(&self, function_name: &str) -> bool {
        self.module.enums.iter().flat_map(|enum_def| enum_def.variants.iter().filter(|variant| !variant.parameters.is_empty()).map(|variant| enum_def.variant_function_name(variant)))
            .any(|it| it == function_name)
    }

    pub fn is_struct_constructor(&self, function_name: &str) -> bool {
        self.module.structs.iter().map(|struct_def| &struct_def.name)
            .any(|it| it == function_name)
    }

    pub fn try_get_struct(&self, type_name: &str) -> Option<&ASTTypedStructDef> {
        self.module.structs.iter().find(|it| it.name == type_name)
    }

    pub fn try_get_enum(&self, type_name: &str) -> Option<&ASTTypedEnumDef> {
        self.module.enums.iter()
            .find(|enum_def| enum_def.name == type_name)
    }

    fn call_function(&mut self, function_call: &ASTTypedFunctionCall, context: &TypedValContext, parent_def: Option<&ASTTypedFunctionDef>, added_to_stack: usize,
                     lambda_space: Option<&LambdaSpace>, indent: usize, is_lambda: bool, stack: &Stack) -> (String, Vec<String>, Vec<LambdaCall>) {
        let mut before = String::new();
        let mut after = Vec::new();

        let lambda_calls = if let Some(function_def) = self.functions.get(&function_call.function_name) {
            let def = function_def.clone();
            // sometimes the function name is different from the function definition name, because it is not a valid ASM name (for enum types is enu-name::enum-variant)
            let real_function_name = self.functions.get(&function_call.function_name).unwrap().clone().name;
            debug!("{}Calling function {} context {:?}, lambda_space: {:?}", " ".repeat(indent * 4), function_call.function_name, context.names(), lambda_space);
            self.call_function_(&function_call, &context, &parent_def, added_to_stack, &mut before, def.parameters, def.inline, Some(def.body), real_function_name,
                                lambda_space, indent, is_lambda, stack, &mut after)
        } else if let Some(function_def) = self.functions.get(&function_call.function_name.replace("::", "_")) {
            let def = function_def.clone();
            // sometimes the function name is different from the function definition name, because it is not a valid ASM name (for enum types is enu-name::enum-variant)
            let real_function_name = self.functions.get(&function_call.function_name.replace("::", "_")).unwrap().clone().name;
            debug!("{}Calling function {} context {:?}, lambda_space: {:?}", " ".repeat(indent * 4), function_call.function_name, context.names(), lambda_space);
            self.call_function_(&function_call, &context, &parent_def, added_to_stack, &mut before, def.parameters, def.inline, Some(def.body), real_function_name,
                                lambda_space, indent, is_lambda, stack, &mut after,
            )
        } else if let Some(TypedVarKind::ParameterRef(index, par)) = context.get(&function_call.function_name) {
            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda { return_type: _, parameters }) = par.clone().type_ref.ast_type {
                let wl = self.backend.word_len() as usize;
                let bp = self.backend.stack_base_pointer();

                let parameters_defs = parameters.iter().map(|it| {
                    let name = format!("p_{}", self.id);
                    self.id += 1;
                    ASTTypedParameterDef { name, type_ref: it.clone() }
                }).collect();

                debug!("{}Calling lambda {}", " ".repeat(indent * 4), function_call.function_name);
                debug!("{}parameters {:?}", " ".repeat((indent + 1) * 4), parameters);
                debug!("{}context {:?}", " ".repeat((indent + 1) * 4), context);
                debug!("{}index {}", " ".repeat((indent + 1) * 4), index);
                debug!("{}function_call.parameters {:?}", " ".repeat((indent + 1) * 4), function_call.parameters);
                debug!("{}parameters_defs {:?}", " ".repeat((indent + 1) * 4), parameters_defs);
                debug!("{}lambda_space {:?}", " ".repeat((indent + 1) * 4), lambda_space);

                self.call_function_(&function_call, &context, &parent_def, added_to_stack, &mut before, parameters_defs, false, None,
                                    format!("[{}+{}+{}]", bp, wl, (index + 1) * wl), lambda_space, indent, true, stack,
                                    &mut after)
            } else {
                panic!("Cannot find function, there's a parameter with name '{}', but it's not a lambda", function_call.function_name);
            }
        } else {
            panic!("Cannot find function {} in {:?}", function_call.function_name, parent_def);
        };

        (before, after, lambda_calls)
    }

    fn call_function_(&mut self, function_call: &&ASTTypedFunctionCall, context: &TypedValContext, parent_def: &Option<&ASTTypedFunctionDef>, added_to_stack: usize, before: &mut String,
                      parameters: Vec<ASTTypedParameterDef>, inline: bool, body: Option<ASTTypedFunctionBody>, address_to_call: String,
                      lambda_space_opt: Option<&LambdaSpace>, indent: usize, is_lambda: bool, stack: &Stack,
                      after: &mut Vec<String>) -> Vec<LambdaCall> {
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
            CodeGen::add(before, &format!("; inlining function {}, added to stack {}", function_call.function_name, added_to_stack), None, true);
        } else {
            CodeGen::add(before, &format!("; calling function {}, added to stack {}", function_call.function_name, added_to_stack), None, true);
        }

        let mut call_parameters = FunctionCallParameters::new(self.backend, parameters.clone(),
                                                              inline, false, stack, self.dereference);

        if !function_call.parameters.is_empty() {
            // as for C calling conventions parameters are pushed in reverse order
            for (param_index, expr) in function_call.parameters.iter().enumerate() {
                let param_opt = parameters.get(param_index);
                let param_name = param_opt.unwrap_or_else(|| panic!("Cannot find param {} : {:?} of function call {}", param_index, expr, function_call.function_name)).name.clone();
                let param_type = param_opt.unwrap_or_else(|| panic!("Cannot find param {} of function call {}", param_index, function_call.function_name)).type_ref.clone();

                debug!("{}adding parameter {}: {:?}", " ".repeat(indent * 4), param_name, expr);

                match expr {
                    ASTTypedExpression::StringLiteral(value) => {
                        let label = format!("_rasm_s{}", self.id);
                        self.id += 1;
                        self.statics.insert(label.clone(), MemoryValue::StringValue(value.clone()));
                        call_parameters.add_string_literal(&param_name, label, None);
                    }
                    ASTTypedExpression::Number(n) => {
                        call_parameters.add_number(&param_name, n, None);
                    }
                    ASTTypedExpression::ASTFunctionCallExpression(call) => {
                        let (bf, af, mut inner_lambda_calls) =
                            self.call_function(call, context, *parent_def, added_to_stack +
                                call_parameters.to_remove_from_stack(), lambda_space_opt, indent + 1, false, stack);

                        call_parameters.push(&bf);

                        after.insert(0, af.join("\n"));

                        call_parameters.add_function_call(self, Some(&param_name),
                                                          param_type.clone());
                        lambda_calls.append(&mut inner_lambda_calls);
                    }
                    ASTTypedExpression::Val(name) => {
                        let error_msg = format!("Cannot find val {}, calling function {}", name, function_call.function_name);
                        self.add_val(context, &lambda_space_opt, &indent, &mut call_parameters, &param_name, name, &error_msg);
                    }
                    ASTTypedExpression::Lambda(lambda_def) => {
                        let (return_type, parameters_types) = if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda { return_type, parameters }) = param_type.ast_type {
                            (return_type, parameters)
                        } else {
                            panic!("Parameter is not a lambda: {:?}", param_type.ast_type);
                        };

                        if parameters_types.len() != lambda_def.parameter_names.len() {
                            panic!("Lambda parameters do not match definition");
                        }

                        let rt = return_type.map(|r| r.deref().clone());

                        let mut def = ASTTypedFunctionDef {
                            //name: format!("{}_{}_{}_lambda{}", parent_def_description, function_call.function_name, param_name, self.id),
                            name: format!("lambda{}", self.id),
                            parameters: Vec::new(), // TODO I don't remember why it does not have any parameter, but it seems to work...
                            return_type: rt,
                            body: ASTTypedFunctionBody::RASMBody(lambda_def.clone().body),
                            inline: false,
                        };

                        self.id += 1;

                        debug!("{}Adding lambda {}", " ".repeat(indent * 4), param_name);

                        let lambda_space = call_parameters.add_lambda(&mut def, lambda_space_opt, context, None);

                        // I add the parameters of the lambda itself
                        for i in 0..parameters_types.len() {
                            def.parameters.push(ASTTypedParameterDef {
                                name: lambda_def.parameter_names.get(i).unwrap().clone(),
                                type_ref: parameters_types.get(i).unwrap().clone(),
                            });
                            // TODO check if the parameter name collides with some context var
                        }

                        let lambda_call = LambdaCall { def, space: lambda_space };

                        self.functions_called.insert(lambda_call.def.name.clone());

                        lambda_calls.push(lambda_call);
                    }
                }
            }
        }

        before.push_str(&call_parameters.before());

        if inline {
            if let Some(ASTTypedFunctionBody::ASMBody(body)) = &body {
                CodeGen::add(before, &format!("; To remove from stack  {}", added_to_stack +
                    call_parameters.to_remove_from_stack()), None,
                             true);
                before.push_str(&call_parameters.resolve_asm_parameters(body, added_to_stack, indent));
            } else {
                panic!("Only asm can be inlined, for now...");
            }
        } else {
            if body.is_some() {
                self.functions_called.insert(function_call.function_name.clone());
            }
            if is_lambda {
                if let Some(address) = lambda_space_opt.and_then(|it| it.get_index(&function_call.function_name)) {
                    CodeGen::add(before, &format!("mov eax, [{} + 8]", self.backend.stack_base_pointer()), None, true);
                    // we add the address to the "lambda space" as the last parameter of the lambda
                    CodeGen::add(before, &format!("add eax, {}", address * self.backend.word_len() as usize), Some("address to the \"lambda space\""), true);
                    CodeGen::add(before, &format!("push {} [eax]", self.backend.pointer_size()), Some("address to the \"lambda space\""), true);
                    CodeGen::add(before, "mov eax, [eax]", Some("address to the \"lambda space\""), true);
                    CodeGen::add(before, "call [eax]", Some(&format!("Calling function {}", function_call.function_name)), true);
                    CodeGen::add(before, &format!("add  {}, {}", self.backend.stack_pointer(), self.backend.word_len()), None, true);
                } else if let Some(TypedVarKind::ParameterRef(index, _)) = context.get(&function_call.function_name) {
                    CodeGen::add(before, "", Some(&format!("calling lambda parameter reference to {}", &function_call.function_name)), true);
                    CodeGen::add(before, &format!("mov eax, [{} + {} + {}]", self.backend.stack_base_pointer(), self.backend.word_len(), (index + 1) * self.backend.word_len() as usize), None, true);
                    // we add the address to the "lambda space" as the last parameter to the lambda
                    CodeGen::add(before, &format!("push {} eax", self.backend.pointer_size()), Some("address to the \"lambda space\""), true);
                    CodeGen::add(before, "call [eax]", Some(&format!("Calling function {}", function_call.function_name)), true);
                    CodeGen::add(before, &format!("add  {}, {}", self.backend.stack_pointer(), self.backend.word_len()), None, true);
                } else {
                    panic!("lambda space does not contain {}", function_call.function_name);
                }
            } else {
                CodeGen::add(before, &format!("call    {}", address_to_call), Some(&format!("Calling function {}", function_call.function_name)), true);
            }
        }

        if call_parameters.to_remove_from_stack() > 0 {
            let sp = self.backend.stack_pointer();
            let wl = self.backend.word_len() as usize;
            CodeGen::add(before, &format!("add     {},{}", sp, wl * (call_parameters.to_remove_from_stack())),
                         Some(&format!("restore stack for {}", function_call.function_name)), true);

            //debug!("going to remove from stack {}/{}", parent_def_description, function_call.function_name);

            stack.remove(call_parameters.to_remove_from_stack());
        }

        for line in call_parameters.after().iter().rev() {
            after.insert(0, line.clone());
        }

        if inline {
            CodeGen::add(before, &format!("; end inlining function {}", function_call.function_name), None, true);
        } else {
            CodeGen::add(before, &format!("; end calling function {}", function_call.function_name), None, true);
        }
        CodeGen::add_empty_line(before);

        lambda_calls
    }

    fn add_val(&mut self, context: &TypedValContext, lambda_space_opt: &Option<&LambdaSpace>, indent: &usize, call_parameters: &mut FunctionCallParameters, param_name: &str, val_name: &str, error_msg: &str) {
        if let Some(var_kind) = context.get(val_name) {
            match var_kind {
                TypedVarKind::ParameterRef(index, par) => {
                    call_parameters.add_val(self, param_name.into(), val_name, par, *index, lambda_space_opt, *indent);
                }
            }
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
            out.push_str(&"; ".to_string());
            out.push_str(c);
        }
        out.push('\n');
    }

    pub fn insert_on_top(src: &String, dest: &mut String) {
        for line in src.lines().rev() {
            dest.insert_str(0, &(line.to_string() + "\n"));
        }
    }

    pub fn call_deref(&mut self, source: &str, type_name: &str, descr: &str) -> String {
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

        let mut id = self.id;

        let key = format!("_descr_{}", id);
        self.statics.insert(key.clone(), MemoryValue::StringValue(descr.to_string()));
        id += 1;

        let mut result = String::new();

        //println!("calling deref for {:?}", type_ref);
        CodeGen::add(&mut result, "", Some(&("deref ".to_owned() + descr)), true);
        CodeGen::add(&mut result, &format!("push     {ws} {key}"), None, true);
        CodeGen::add(&mut result, &format!("push     {ws} {source}"), None, true);
        CodeGen::add(&mut result, "call     deref", None, true);
        CodeGen::add(&mut result, &format!("add      esp,{}", wl * 2), None, true);

        if let Some(struct_def) = self.try_get_struct(type_name) {
            //println!("dereferencing struct {type_name}");
            CodeGen::add(&mut result, "", None, true);
            CodeGen::add(&mut result, &format!("push {ws} ebx"), None, true);
            CodeGen::add(&mut result, &format!("push {ws} {source}"), None, true);
            CodeGen::add(&mut result, "pop ebx", None, true);
            CodeGen::add(&mut result, &format!("mov {ws} ebx, [ebx]"), None, true);
            for (i, prop) in struct_def.properties.iter().enumerate() {
                /*
                if let Some(_) = Self::get_reference_type_name(&prop.type_ref.ast_type) {
                    CodeGen::add(&mut result, &format!("push     {ws} {key}"), None, true);
                    CodeGen::add(&mut result, &format!("push     {ws} [ebx + {i} * {wl}]"), None, true);
                    CodeGen::add(&mut result, "call     deref", None, true);
                    CodeGen::add(&mut result, &format!("add      esp,{}", wl * 2), None, true);
                }

                 */
            }
            CodeGen::add(&mut result, "pop ebx", None, true);
        }

        if let Some(enum_def) = self.try_get_enum(type_name) {
            //println!("dereferencing enum {type_name}");
            CodeGen::add(&mut result, &format!("push {ws} ebx"), None, true);
            CodeGen::add(&mut result, &format!("push {ws} {source}"), None, true);
            CodeGen::add(&mut result, "pop ebx", None, true);
            CodeGen::add(&mut result, &format!("mov {ws} ebx, [ebx]"), None, true);
            for (i, variant) in enum_def.variants.iter().enumerate() {
                if !variant.parameters.is_empty() {
                    CodeGen::add(&mut result, &format!("cmp {ws} [ebx], {}", i), None, true);
                    CodeGen::add(&mut result, &format!("jnz ._{type_name}_{i}_{}", id), None, true);
                    for (j, par) in variant.parameters.iter().enumerate() {
                        //println!("par {:?}", par);
                        /*
                        if let Some(_) = Self::get_reference_type_name(&par.type_ref.ast_type) {
                            CodeGen::add(&mut result, &format!("push     {ws} {key}"), None, true);
                            CodeGen::add(&mut result, &format!("push     {ws} [ebx + {wl} + {j} * {wl}]"), None, true);
                            CodeGen::add(&mut result, "call     deref", None, true);
                            CodeGen::add(&mut result, &format!("add      esp,{}", wl * 2), None, true);
                        }

                         */
                    }
                    CodeGen::add(&mut result, &format!("._{type_name}_{i}_{}:", id), None, false);
                    id += 1;
                }
            }

            CodeGen::add(&mut result, "pop ebx", None, true);
        }

        self.id = id;

        //Self::insert_on_top(&result, out);
        result
    }

    fn get_reference_type_name(ast_type: &ASTTypedType) -> Option<String> {
        match ast_type {
            ASTTypedType::Builtin(_) => { None }
            ASTTypedType::Enum { name } => { Some(name.clone()) }
            ASTTypedType::Struct { name } => { Some(name.clone()) }
        }
    }

    pub fn call_add_ref(out: &mut String, backend: &dyn Backend, source: &str, descr: &str) {
        let ws = backend.word_size();
        let wl = backend.word_len();

        //let mut id = self.id;

        //let key = format!("_descr_{}", id);

        //if let ASTType::Custom { name: _, param_types: _ } = type_ref.ast_type {
        //println!("calling deref for {:?}", type_ref);
        //CodeGen::add(out, &format!("push     {ws} {descr}"), None, true);


        //CodeGen::add(out, &format!("push     {ws} {descr}"), None, true);
        CodeGen::add(out, &format!("push     {ws} {source}"), None, true);
        CodeGen::add(out, "call     addRef", None, true);
        CodeGen::add(out, &format!("add      esp,{wl}"), None, true);

        //self.id + 1
        //}
    }

    pub fn add_empty_line(out: &mut String) {
        out.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Read;
    use std::path::Path;
    use crate::codegen::backend::BackendAsm386;

    use crate::codegen::CodeGen;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    #[ignore]
    fn test() {
        compare_asm("resources/test/helloworld");
    }

    #[test]
    #[ignore]
    fn test_fib() {
        compare_asm("resources/test/fibonacci");
    }

    #[test]
    #[ignore]
    fn test_inline() {
        compare_asm("resources/test/inline");
    }

    fn compare_asm(resource_prefix: &str) {
        let rasm_file = format!("{}.rasm", resource_prefix);
        let path = Path::new(&rasm_file);
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

        let backend = BackendAsm386::new();

        let mut gen = CodeGen::new(&backend, module, 1024 * 1024, 64 * 1024 * 1024, 1024 * 1024, false, false, false);

        let asm = gen.asm();

        let asm_file = format!("{}.asm", resource_prefix);
        let path = Path::new(&asm_file);
        let mut expected = String::new();
        File::open(path).unwrap().read_to_string(&mut expected).unwrap();

        assert_eq!(asm.trim(), expected);
    }
}
