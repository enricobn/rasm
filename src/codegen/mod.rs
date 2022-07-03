mod function_call_parameters;
pub mod backend;

use std::collections::HashSet;
use std::ops::Deref;
use linked_hash_map::{Iter, LinkedHashMap};
use crate::codegen::backend::Backend;
use crate::codegen::function_call_parameters::FunctionCallParameters;

use crate::parser::ast::{ASTEnumDef, ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef, ASTType, ASTTypeRef, BuiltinTypeKind};

pub struct CodeGen<'a> {
    module: ASTModule,
    id: usize,
    statics: LinkedHashMap<String, MemoryValue>,
    // key=memory_label
    body: String,
    definitions: String,
    lambdas: Vec<LambdaCall>,
    functions: LinkedHashMap<String, ASTFunctionDef>,
    backend: &'a dyn Backend,
    functions_called: HashSet<String>,
}

#[derive(Clone)]
enum MemoryValue {
    StringValue(String),
    I32Value(i32),
}

#[derive(Clone, Debug)]
struct VarContext {
    value_to_address: LinkedHashMap<String, VarKind>,
}

#[derive(Clone, Debug)]
enum VarKind {
    ParameterRef(usize, ASTParameterDef)
}

#[derive(Debug, Clone)]
struct LambdaCall {
    def: ASTFunctionDef,
    space: LambdaSpace,
}

#[derive(Debug, Clone)]
pub struct LambdaSpace {
    parameters_indexes: LinkedHashMap<String, usize>,
    context: VarContext,
}

impl LambdaSpace {
    fn new(context: VarContext) -> Self {
        LambdaSpace { parameters_indexes: LinkedHashMap::new(), context }
    }

    fn add_context_parameter(&mut self, name: String, index: usize) {
        self.parameters_indexes.insert(name, index);
    }

    fn get_index(&self, name: &str) -> Option<usize> {
        self.parameters_indexes.get(name).cloned()
    }

}

impl VarContext {
    fn new(parent_context: Option<&VarContext>) -> Self {
        let mut map = LinkedHashMap::new();
        if let Some(pc) = parent_context {
            for (key, value) in pc.value_to_address.iter() {
                map.insert(key.clone(), value.clone());
            }
        }
        Self { value_to_address: map }
    }

    fn insert(&mut self, key: String, value: VarKind) -> Option<VarKind> {
        self.value_to_address.insert(key, value)
    }

    fn get(&self, key: &str) -> Option<&VarKind> {
        self.value_to_address.get(key)
    }

    fn iter(&self) -> Iter<String, VarKind> {
        self.value_to_address.iter()
    }

    fn names(&self) -> Vec<&String> {
        self.value_to_address.keys().collect()
    }
}

impl<'a> CodeGen<'a> {
    pub fn new(backend: &'a dyn Backend, module: ASTModule) -> Self {
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
        }
    }

    pub fn asm(&mut self) -> String {
        self.id = 0;
        self.statics = LinkedHashMap::new();
        self.body = String::new();
        self.definitions = String::new();
        self.functions = LinkedHashMap::new();

        for function_def in &self.module.functions.clone() {
            self.functions.insert(function_def.name.clone(), function_def.clone());
        }

        for enum_def in &self.module.enums {
            let param_types: Vec<ASTTypeRef> = enum_def.type_parameters.iter().map(|it| ASTTypeRef::parametric(it, false)).collect();

            for (variant_num, variant) in enum_def.variants.iter().enumerate() {
                //println!("variant parameters for {} : {:?}", variant.name, variant.parameters);

                let ast_type = ASTType::Custom { name: enum_def.name.clone(), param_types: param_types.clone() };
                let type_ref = ASTTypeRef { ast_type, ast_ref: true };
                let return_type = Some(type_ref);
                let body_str = if variant.parameters.is_empty() {
                    let label = format!("_enum_{}_{}", enum_def.name, variant.name);
                    self.statics.insert(label.clone(), MemoryValue::I32Value(variant_num as i32));
                    format!("\tmov    eax, {}\n", label)
                } else {
                    Self::enum_parametric_variant_constructor_body(self.backend, &variant_num, &variant)
                };
                let body = ASTFunctionBody::ASMBody(body_str);
                let function_def = ASTFunctionDef { name: enum_def.name.clone() + "_" + &variant.name.clone(), parameters: variant.parameters.clone(), body, inline: false, return_type, param_types: Vec::new() };
                self.functions.insert(enum_def.name.clone() + "::" + &variant.name.clone(), function_def);
            }
            let return_type = Some(ASTTypeRef::custom(&enum_def.name, false, param_types));

            let body = Self::enum_match_body(self.backend, enum_def);

            let function_body = ASTFunctionBody::ASMBody(body);
            let param_types = enum_def.type_parameters.iter().map(|it| ASTTypeRef::parametric(it, false)).collect();
            let mut parameters = vec![ASTParameterDef { name: "value".into(), type_ref: ASTTypeRef { ast_type: ASTType::Custom { name: enum_def.name.clone(), param_types }, ast_ref: true }, from_context: false }];
            for variant in enum_def.variants.iter() {
                let ast_type = ASTType::Builtin(BuiltinTypeKind::Lambda { return_type: return_type.clone().map(Box::new), parameters: variant.parameters.iter().map(|it| it.type_ref.clone()).collect() });
                parameters.push(ASTParameterDef { name: variant.name.clone(), type_ref: ASTTypeRef { ast_type, ast_ref: true }, from_context: false });
            }
            // TODO I would like to call it Enum::match
            let function_def = ASTFunctionDef { name: enum_def.name.clone() + "Match", parameters, body: function_body, inline: false, return_type, param_types: Vec::new() };
            self.functions.insert(enum_def.name.clone() + "Match", function_def);
        }

        // for now main has no context
        let main_context = VarContext::new(None);

        for function_call in &self.module.body.clone() {
            let (s, mut lambda_calls) = self.call_function(function_call, &main_context, None, 0, None,
                                                           0, false);
            self.body.push_str(&s);
            self.lambdas.append(&mut lambda_calls);
        }

        // TODO add a command line argument
        //Parser::print(&self.module);

        self.create_lambdas(self.lambdas.clone(), 0);

        self.create_all_functions();

        // create only used functions

        //self.create_only_used_functions();

        let mut asm = String::new();

        if !self.statics.is_empty() {
            asm.push_str("SECTION .data\n");
            let mut keys: Vec<&String> = self.statics.keys().collect();
            // sorted for test purposes
            keys.sort();

            for id in keys.iter() {
                let mut def = String::new();
                def.push_str(id);

                match self.statics.get(*id).unwrap() {
                    MemoryValue::StringValue(s) => {
                        def.push_str("    db    ");
                        def.push_str(&format!("'{}', 0h", s));
                    }
                    MemoryValue::I32Value(i) => {
                        def.push_str("    dd    ");
                        def.push_str(&format!("{}", i));
                    }
                }
                CodeGen::add(&mut asm, &def, None);
            }
        }

        CodeGen::add(&mut asm, "section .bss", None);
        CodeGen::add(&mut asm, "  _heap_buffer     resb 64 * 1024 * 1024", None);
        CodeGen::add(&mut asm, "  _heap            resw 1", None);
        CodeGen::add(&mut asm, "  _rasm_buffer_10b resb 10", None);
        // command line arguments
        CodeGen::add(&mut asm, "  _rasm_args resw 12", None);

        CodeGen::add(&mut asm, "SECTION .text", None);
        CodeGen::add(&mut asm, "global  main", None);
        CodeGen::add(&mut asm, "", None);
        CodeGen::add(&mut asm, "main:", None);

        // command line arguments
        for i in 0..12 {
            CodeGen::add(&mut asm, &format!("mov     eax,[esp + {}]", i * self.backend.word_len()), Some(&format!("command line argument {}", i)));
            CodeGen::add(&mut asm, &format!("mov     [_rasm_args + {}], eax", i * self.backend.word_len()), None);
        }

        CodeGen::add(&mut asm, "mov     eax, _heap_buffer\n", None);
        CodeGen::add(&mut asm, "mov     [_heap], eax\n", None);

        asm.push_str(&self.body);

        // exit sys call
        CodeGen::add(&mut asm, "    mov     ebx, 0", None);
        CodeGen::add(&mut asm, "    mov     eax, 1", None);
        CodeGen::add(&mut asm, "    int     80h", None);
        CodeGen::add(&mut asm, "    ret", None);

        asm.push_str(&self.definitions);

        asm
    }

    fn enum_parametric_variant_constructor_body(backend: &dyn Backend, variant_num: &usize, variant: &&ASTEnumVariantDef) -> String {
        let mut body = String::new();
        CodeGen::add(&mut body, "\tpush ecx", None);
        CodeGen::add(&mut body, "\tpush ebx", None);
        CodeGen::add(&mut body, &format!("\tpush     {}", (variant.parameters.len() + 1) * backend.word_len() as usize), None);
        CodeGen::add(&mut body, "\tcall malloc", None);
        CodeGen::add(&mut body, "\tmov   ecx, eax", None);
        CodeGen::add(&mut body, &format!("\tadd esp,{}", backend.word_len()), None);
        CodeGen::add(&mut body, &format!("\tmov   [eax], word {}", variant_num), None);
        for par in variant.parameters.iter().rev() {
            CodeGen::add(&mut body, &format!("\tadd   eax, {}", backend.word_len()), Some(&format!("parameter {}", par.name)));
            CodeGen::add(&mut body, &format!("\tmov   ebx, ${}", par.name), None);
            CodeGen::add(&mut body, "\tmov   [eax], ebx", None);
        }
        CodeGen::add(&mut body, "\tmov   eax, ecx", None);
        CodeGen::add(&mut body, "\tpop   ebx", None);
        CodeGen::add(&mut body, "\tpop   ecx", None);
        body
    }

    fn enum_match_body(backend: &dyn Backend, enum_def: &ASTEnumDef) -> String {
        let word_len = backend.word_len();
        let sp = backend.stack_pointer();
        let mut body = String::new();

        CodeGen::add(&mut body, "\tmov eax, $value", None);

        for (variant_num, variant) in enum_def.variants.iter().enumerate() {
            CodeGen::add(&mut body, &format!("\tcmp [eax], word {}", variant_num), None);
            CodeGen::add(&mut body, &format!("\tjnz .variant{}", variant_num), None);

            for (i, param) in variant.parameters.iter().enumerate() {
                CodeGen::add(&mut body, &format!("\tpush dword [eax + {}]", (i + 1) * word_len as usize), Some(&format!("param {}", param.name)));
            }

            CodeGen::add(&mut body, &format!("\tcall ${}", variant.name), None);

            if !variant.parameters.is_empty() {
                CodeGen::add(&mut body, &format!("\tadd {}, {}", sp, variant.parameters.len() * word_len as usize), None);
            }

            CodeGen::add(&mut body, "\tjmp .end", None);
            CodeGen::add(&mut body, &format!(".variant{}:", variant_num), None);
        }
        CodeGen::add(&mut body, ".end:", None);
        body
    }

    fn create_all_functions(&mut self) {
        for function_def in self.functions.clone().values() {
            // VarContext ???
            let vec1 = self.add_function_def(function_def, None, &VarContext::new(None), 0);
            self.create_lambdas(vec1, 0);
        }
    }

    fn create_only_used_functions(&mut self) {
        let mut something_added = true;

        let mut already_created_functions = HashSet::new();

        while something_added {
            something_added = false;
            let functions_called = self.functions_called.clone();

            //println!("functions called {:?}", functions_called);

            for function_name in functions_called {
                if !already_created_functions.contains(&function_name) {
                    let functions = self.functions.clone();
                    if let Some(function_def) = functions.get(&function_name) {
                        // TODO VarContext??
                        let vec1 = self.add_function_def(function_def, None, &VarContext::new(None), 0);
                        self.create_lambdas(vec1, 0);
                        something_added = true;
                        already_created_functions.insert(function_name);
                    } else {
                        panic!("Cannot find function {}", function_name);
                    }
                }
            }
        }
    }

    fn create_lambdas(&mut self, lambdas: Vec<LambdaCall>, indent: usize) {
        let mut lambda_calls = Vec::new();
        for lambda_call in lambdas {
            //println!("Creating lambda {}", lambda_call.def.name);
            lambda_calls.append(&mut self.add_function_def(&lambda_call.def, Some(&lambda_call.space), &lambda_call.space.context, indent));
            //Parser::print_function_def(&lambda_call.def);
        }
        if !lambda_calls.is_empty() {
            self.create_lambdas(lambda_calls, indent + 1);
        }
    }

    fn add_function_def(&mut self, function_def: &ASTFunctionDef, lambda_space: Option<&LambdaSpace>, parent_context: &VarContext, indent: usize) -> Vec<LambdaCall> {
        let is_lambda = lambda_space.is_some();

        println!("{}Adding function def {}", " ".repeat(indent * 4), function_def.name);
        let mut lambda_calls = Vec::new();
        //println!("add_function_def {}", function_def.name);
        CodeGen::add(&mut self.definitions, &format!("{}:", function_def.name), None);

        let sp = self.backend.stack_pointer();
        let bp = self.backend.stack_base_pointer();

        CodeGen::add(&mut self.definitions, &format!("    push    {}", bp), None);
        CodeGen::add(&mut self.definitions, &format!("    mov     {},{}", bp, sp), None);

        let mut context = VarContext::new(Some(parent_context));

        let mut function_call_parameters = FunctionCallParameters::new(self.backend, function_def.parameters.clone(), false);

        // I think it's useless
        let mut i_for_context = 0;
        let mut i = if is_lambda {
            // one because the first parameter is the address to the lambda space
            1
        } else {
            0
        };
        for par in function_def.parameters.iter() {
            let offset = if par.from_context {
                i_for_context
            } else {
                i
            };

            if par.from_context {
                function_call_parameters.add_lambda_param_from_lambda_space(&par.name, lambda_space.unwrap(), Some(&format!("reference to parameter {} from context", par.name)), indent + 1);
                i_for_context += 1;
            } else {
                function_call_parameters.add_var(par.name.clone(), par, i, Some(&format!("reference to parameter {}", par.name)), indent + 1);
                i += 1;
                println!("{}Inserted var {} in context, offset {}, from context {}", " ".repeat((indent + 1) * 4), par.name, offset,
                         par.from_context);
                context.insert(par.name.clone(), VarKind::ParameterRef(offset, par.clone()));
            }
        }

        match &function_def.body {
            ASTFunctionBody::RASMBody(calls) => {
                for call in calls {
                    let (s, mut lambda_calls_) = self.call_function(call, &context, Some(function_def), 0,
                                                                    lambda_space, indent + 1, false);
                    self.definitions.push_str(&s);
                    lambda_calls.append(&mut lambda_calls_);
                }
            }
            ASTFunctionBody::ASMBody(s) => {
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

        CodeGen::add(&mut self.definitions, &format!("    pop     {}", bp), None);
        CodeGen::add(&mut self.definitions, "    ret", None);
        lambda_calls
    }

    fn call_function(&mut self, function_call: &ASTFunctionCall, context: &VarContext, parent_def: Option<&ASTFunctionDef>, added_to_stack: usize,
                     lambda_space: Option<&LambdaSpace>, indent: usize, is_lambda: bool) -> (String, Vec<LambdaCall>) {
        let mut before = String::new();

        let lambda_calls = if let Some(function_def) = self.functions.get(&function_call.function_name) {
            let def = function_def.clone();
            // sometimes the function name is different from the function definition name, because it is not a valid ASM name (for enum types is enu-name::enum-variant)
            let real_function_name = self.functions.get(&function_call.function_name).unwrap().clone().name;
            println!("{}Calling function {} context {:?}, lambda_space: {:?}", " ".repeat(indent * 4), function_call.function_name, context.names(), lambda_space);
            self.call_function_(&function_call, &context, &parent_def, &added_to_stack, &mut before, def.parameters, def.inline, Some(def.body), real_function_name,
                                lambda_space, indent, is_lambda)
        } else if let Some(VarKind::ParameterRef(index, par)) = context.get(&function_call.function_name) {
            if let ASTType::Builtin(BuiltinTypeKind::Lambda { return_type: _, parameters }) = par.clone().type_ref.ast_type {
                let wl = self.backend.word_len() as usize;
                let bp = self.backend.stack_base_pointer();

                let parameters_defs = parameters.iter().map(|it| ASTParameterDef { name: "p".into(), type_ref: it.clone(), from_context: false }).collect();

                println!("{}Calling lambda {}", " ".repeat(indent * 4), function_call.function_name);
                println!("{}parameters {:?}", " ".repeat((indent + 1) * 4), parameters);
                println!("{}context {:?}", " ".repeat((indent + 1) * 4), context);
                println!("{}index {}", " ".repeat((indent + 1) * 4), index);
                println!("{}function_call.parameters {:?}", " ".repeat((indent + 1) * 4), function_call.parameters);
                println!("{}parameters_defs {:?}", " ".repeat((indent + 1) * 4), parameters_defs);
                println!("{}lambda_space {:?}", " ".repeat((indent + 1) * 4), lambda_space);

                self.call_function_(&function_call, &context, &parent_def, &added_to_stack, &mut before, parameters_defs, false, None,
                                    format!("[{}+{}+{}]", bp, wl, (index + 1) * wl), lambda_space, indent, true)
            } else {
                panic!("Cannot find function, there's a parameter with name '{}', but it's not a lambda", function_call.function_name);
            }
        } else {
            panic!("Cannot find function '{}'", function_call.function_name);
        };

        (before, lambda_calls)
    }

    fn call_function_(&mut self, function_call: &&ASTFunctionCall, context: &VarContext, parent_def: &Option<&ASTFunctionDef>, added_to_stack: &usize, before: &mut String,
                      parameters: Vec<ASTParameterDef>, inline_def: bool, body: Option<ASTFunctionBody>, address_to_call: String,
                      lambda_space_opt: Option<&LambdaSpace>, indent: usize, is_lambda: bool) -> Vec<LambdaCall> {

        // TODO inline does not work with lambda space, so for now I disable it
        let inline = false;

        let mut lambda_calls = Vec::new();
        if inline && parent_def.is_some() {
            CodeGen::add(before, &format!("; inlining function {}", function_call.function_name), None);
        } else {
            if inline {
                CodeGen::add(before, "; function is inline, but not inside a function", None);
                CodeGen::add(before, "; so cannot be inlined.", None);
            }
            CodeGen::add(before, &format!("; calling function {}", function_call.function_name), None);
        }

        let mut to_remove_from_stack = 0;

        let mut call_parameters = FunctionCallParameters::new(self.backend, parameters.clone(),
                                                              inline && parent_def.is_some());

        if !function_call.parameters.is_empty() {
            let mut param_index = function_call.parameters.len();
            // as for C calling conventions parameters are pushed in reverse order
            for expr in function_call.parameters.iter().rev() {
                let param_opt = parameters.get(param_index - 1);
                let param_name = param_opt.unwrap_or_else(|| panic!("Cannot find param {} of function call {}", param_index - 1, function_call.function_name)).name.clone();
                let param_type = param_opt.unwrap_or_else(|| panic!("Cannot find param {} of function call {}", param_index - 1, function_call.function_name)).type_ref.clone();
                param_index -= 1;

                println!("{}adding parameter {}: {:?}", " ".repeat(indent * 4), param_name, expr);

                match expr {
                    ASTExpression::StringLiteral(value) => {
                        let label = format!("_rasm_s{}", self.id);
                        self.id += 1;
                        self.statics.insert(label.clone(), MemoryValue::StringValue(value.clone()));
                        call_parameters.add_string_literal(&param_name, label, None);
                    }
                    ASTExpression::Number(n) => {
                        call_parameters.add_number(&param_name, n, None);
                    }
                    ASTExpression::ASTFunctionCallExpression(call) => {
                        let (s, mut inner_lambda_calls) = self.call_function(call, context, *parent_def, added_to_stack +
                            to_remove_from_stack + call_parameters.to_remove_from_stack(), lambda_space_opt, indent + 1, false);
                        call_parameters.push(&s);
                        call_parameters.add_function_call(None);
                        lambda_calls.append(&mut inner_lambda_calls);
                    }
                    ASTExpression::Var(name) => {
                        if let Some(var_kind) = context.get(name) {
                            match var_kind {
                                VarKind::ParameterRef(index, par) => {
                                    CodeGen::add(before, "", Some(&format!("{}Adding parameter ref param {}, for function call {}, index {}, from context {}, lambda_space: {:?}", " ".repeat(indent * 4), name, function_call.function_name, index, par.from_context, lambda_space_opt)));
                                    println!("{}Adding parameter ref param {}, for function call {}, index {}, from context {}, lambda_space: {:?}", " ".repeat(indent * 4), name, function_call.function_name, index, par.from_context, lambda_space_opt);

                                    if lambda_space_opt.map(|it| it.get_index(&name).is_some()).unwrap_or(false) {
                                        //call_parameters.add_var(param_name, par, lambda_space_opt, *index, Some(&format!("var reference to parameter '{}' index {}", name, index)), indent);
                                        call_parameters.add_lambda_param_from_lambda_space(&name, lambda_space_opt.unwrap(),
                                                                                           Some(&format!("var reference to lambda space parameter '{}' index {}, inside def: {:?}", name, index, parent_def.map(|it| it.name.clone()))), indent);
                                    } else {
                                        call_parameters.add_var(param_name, par, *index, Some(&format!("var reference to parameter '{}' index {}", name, index)), indent);
                                    }
                                }
                            }
                            /*                        } else if lambda_space_opt.is_some() && lambda_space_opt.unwrap().get(name).is_some() {
                                                        call_parameters.add_lambda_param_from_lambda_space(name.clone(), name, &param_type, lambda_space_opt.unwrap(), None, indent);
                                                        */
                        } else {
                            panic!("Cannot find variable {}, calling function {}", name, function_call.function_name);
                        }
                    }
                    ASTExpression::Lambda(lambda_def) => {
                        let (return_type, parameters_types) = if let ASTType::Builtin(BuiltinTypeKind::Lambda { return_type, parameters }) = param_type.ast_type {
                            (return_type, parameters)
                        } else {
                            panic!("Parameter is not a lambda");
                        };

                        if parameters_types.len() != lambda_def.parameter_names.len() {
                            panic!("Lambda parameters do not match definition");
                        }

                        let rt = return_type.map(|r| r.deref().clone());

                        let mut def = ASTFunctionDef {
                            name: format!("lambda{}", self.id),
                            parameters: Vec::new(),
                            return_type: rt,
                            body: lambda_def.clone().body,
                            inline: false,
                            param_types: Vec::new(),
                        };

                        self.id += 1;

                        let mut lambda_space = LambdaSpace::new(context.clone());

                        println!("{}Preparing lambda_space and 'from_context' parameters for lambda {}", " ".repeat(indent * 4), param_name);

                        let num_of_params = context.iter().filter(|(_, kind)| {
                            matches!(kind, VarKind::ParameterRef(_, _))
                        }).count();

                        //if num_of_params > 0 {
                        let bp = self.backend.stack_base_pointer();
                        let sp = self.backend.stack_pointer();
                        let wl = self.backend.word_len() as usize;
                        let pointer_size = self.backend.pointer_size();

                        //CodeGen::add(before, &format!("    push {} eax", pointer_size), None);
                        CodeGen::add(before, &format!("    push {} ebx", pointer_size), None);
                        CodeGen::add(before, &format!("    push {} ecx", pointer_size), None);
                        CodeGen::add(before, &format!("    push {}", (num_of_params + 1) * wl), None);
                        CodeGen::add(before, "    call malloc", None);

                        CodeGen::add(before, &format!("    add {}, {}", sp, wl), None);

                        CodeGen::add(before, "    mov ecx, eax", None);

                        let mut i = 1;

                        // TODO optimize: do not create parameters that are overridden by parent memcopy
                        context.iter().for_each(|(name, kind)| {
                            CodeGen::add(before, &format!("    add  ecx, {}", wl), None);

                            if let VarKind::ParameterRef(index, par) = kind {
                                let type_size = self.backend.type_size(&par.type_ref).unwrap();

                                let address = format!("{}+{}+{}", bp, wl, (index + 1) * wl);

                                CodeGen::add(before, &format!("    mov  {} ebx, [{}]", type_size, address),
                                             Some(&format!("context parameter {}", name)));
                                CodeGen::add(before, "    mov  dword [ecx], ebx", None);

                                lambda_space.add_context_parameter(name.clone(), i);
                                def.parameters.push(ASTParameterDef { name: name.into(), type_ref: par.type_ref.clone(), from_context: true });
                                i += 1;
                            }
                        });

                        // I copy the lambda space of the parent
                        if let Some(parent_lambda) = lambda_space_opt {
                            let parent_lambda_size = parent_lambda.parameters_indexes.len() + 1;
                            CodeGen::add(before, "    push eax", None);
                            CodeGen::add(before, &format!("    push {} {}", self.backend.pointer_size(), parent_lambda_size), None);
                            CodeGen::add(before, "    push eax", None);
                            CodeGen::add(before, &format!("    push {} [{}+8]", self.backend.pointer_size(), self.backend.stack_base_pointer()), None);
                            CodeGen::add(before, "    call memcopy", None);
                            CodeGen::add(before, &format!("    add {},{}", self.backend.stack_pointer(), 3 * wl), None);
                            CodeGen::add(before, "    pop eax", None);
                        }

                        CodeGen::add(before, &format!("    mov {} [eax], {}", pointer_size, def.name), None);

                        CodeGen::add(before, "    pop ecx", None);
                        CodeGen::add(before, "    pop ebx", None);
                        //CodeGen::add(&mut after, "    pop eax", None);
                        //}

                        // I add the parameters of the lambda itself
                        for i in 0..parameters_types.len() {
                            def.parameters.push(ASTParameterDef {
                                name: lambda_def.parameter_names.get(i).unwrap().clone(),
                                type_ref: parameters_types.get(i).unwrap().clone(),
                                from_context: false,
                            });
                            // TODO check if the parameter name collides with some context var
                        }

                        if let ASTFunctionBody::RASMBody(_) = &lambda_def.body {
                            CodeGen::add(before, &format!("    push   {} eax", pointer_size), Some(&format!("reference to function {}", def.name)));

                            to_remove_from_stack += 1;

                            //println!("Pushing lambda {}", def.name);
                            let lambda_call = LambdaCall { def, space: lambda_space };
                            //println!("Lambda call {}", lambda_call.def.name);
                            self.functions_called.insert(lambda_call.def.name.clone());
                            lambda_calls.push(lambda_call);
                        } else {
                            panic!("A lambda cannot have an asm body.")
                        }
                    }
                }
            }
        }

        before.push_str(call_parameters.before());

        // I can only inline functions if are called inside another function, otherwise I cannot access to the base pointer and
        // I must access the stack pointer, but if the function, as usual, pushes some values in the stack, I cannot use it ...
        if inline && parent_def.is_some() {
            if let Some(ASTFunctionBody::ASMBody(body)) = &body {
                CodeGen::add(before, &format!("; To remove from stack  {}", added_to_stack +
                    to_remove_from_stack + call_parameters.to_remove_from_stack()), None);
                before.push_str(&call_parameters.resolve_asm_parameters(body, added_to_stack + to_remove_from_stack, indent));
            } else {
                panic!("Only asm can be inlined, for now...");
            }
        } else {
            if body.is_some() {
                self.functions_called.insert(function_call.function_name.clone());
            }
            if is_lambda {
                if let Some(address) = lambda_space_opt.and_then(|it| it.get_index(&function_call.function_name)) {
                    CodeGen::add(before, &format!("    mov eax, [{} + 8]", self.backend.stack_base_pointer()), None);
                    // we add the address to the "lambda space" as the last parameter to the lambda
                    CodeGen::add(before, &format!("add eax, {}", address * self.backend.word_len() as usize), Some("address to the \"lambda space\""));
                    CodeGen::add(before, &format!("push {} [eax]", self.backend.pointer_size()), Some("address to the \"lambda space\""));
                    CodeGen::add(before, "mov eax, [eax]", Some("address to the \"lambda space\""));
                    CodeGen::add(before, "call [eax]", Some(&format!("Calling function {}", function_call.function_name)));
                    CodeGen::add(before, &format!("add  {}, {}", self.backend.stack_pointer(), self.backend.word_len()), None);
                } else if let Some(VarKind::ParameterRef(index, _)) = context.get(&function_call.function_name) {
                    CodeGen::add(before, "", Some(&format!("calling lambda parameter reference to {}", &function_call.function_name)));
                    CodeGen::add(before, &format!("    mov eax, [{} + {} + {}]", self.backend.stack_base_pointer(), self.backend.word_len(), (index + 1) * self.backend.word_len() as usize), None);
                    // we add the address to the "lambda space" as the last parameter to the lambda
                    CodeGen::add(before, &format!("push {} eax", self.backend.pointer_size()), Some("address to the \"lambda space\""));
                    CodeGen::add(before, "call [eax]", Some(&format!("Calling function {}", function_call.function_name)));
                    CodeGen::add(before, &format!("add  {}, {}", self.backend.stack_pointer(), self.backend.word_len()), None);
                } else {
                    panic!("lambda space does not contain {}", function_call.function_name);
                }
            } else {
                CodeGen::add(before, &format!("    call    {}", address_to_call), Some(&format!("Calling function {}", function_call.function_name)));
            }
        }

        if to_remove_from_stack + call_parameters.to_remove_from_stack() > 0 {
            let sp = self.backend.stack_pointer();
            let wl = self.backend.word_len() as usize;
            CodeGen::add(before, &format!("    add     {},{}", sp, wl * (to_remove_from_stack + call_parameters.to_remove_from_stack())), None);
        }

        if inline && parent_def.is_some() {
            CodeGen::add(before, &format!("; end inlining function {}", function_call.function_name), None);
        } else {
            CodeGen::add(before, &format!("; end calling function {}", function_call.function_name), None);
        }

        lambda_calls
    }

    fn add(dest: &mut String, code: &str, comment: Option<&str>) {
        if code.is_empty() {
            let string = " ".repeat(50);
            dest.push_str(&string);
        } else {
            let s = format!("{:width$}", code, width = 50);
            assert_eq!(s.len(), 50);
            dest.push_str(&s);
        }

        if let Some(c) = comment {
            dest.push_str(&"; ".to_string());
            dest.push_str(c);
        }
        dest.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Read;
    use std::path::Path;
    use crate::codegen::backend::Backend386;

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
        let mut parser = Parser::new(lexer);
        let module = parser.parse(path);

        let backend = Backend386::new();

        let mut gen = CodeGen::new(&backend, module);

        let asm = gen.asm();

        let asm_file = format!("{}.asm", resource_prefix);
        let path = Path::new(&asm_file);
        let mut expected = String::new();
        File::open(path).unwrap().read_to_string(&mut expected).unwrap();

        assert_eq!(asm.trim(), expected);
    }
}
