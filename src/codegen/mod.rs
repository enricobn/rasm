mod function_call_parameters;
pub mod backend;

use std::collections::hash_map::Iter;
use std::collections::HashMap;
use std::ops::Add;
use crate::codegen::backend::Backend;
use crate::codegen::function_call_parameters::FunctionCallParameters;

use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef, ASTType, ASTTypeRef, BuiltinTypeKind, lambda_unit};

pub struct CodeGen<'a> {
    module: ASTModule,
    id: usize,
    statics: HashMap<String, MemoryValue>,
    // key=memory_label
    body: String,
    definitions: String,
    lambdas: Vec<LambdaCall>,
    functions: HashMap<String, ASTFunctionDef>,
    backend: &'a dyn Backend,
}

#[derive(Clone)]
enum MemoryValue {
    StringValue(String),
    I32Value(i32),
}

#[derive(Clone)]
struct VarContext {
    value_to_address: HashMap<String, VarKind>,
}

#[derive(Clone, Debug)]
enum VarKind {
    ParameterRef(usize, ASTTypeRef)
}

#[derive(Clone)]
struct LambdaCall {
    def: ASTFunctionDef,
    parameters_offset: usize,
}

impl VarContext {
    fn new() -> Self {
        Self { value_to_address: HashMap::new() }
    }

    fn insert(&mut self, key: String, value: VarKind) -> Option<VarKind> {
        self.value_to_address.insert(key, value)
    }

    fn get(&self, key: &str) -> Option<&VarKind> {
        self.value_to_address.get(key)
    }

    fn iter(&self) -> Iter<'_, String, VarKind> {
        self.value_to_address.iter()
    }

    fn len(&self) -> usize {
        self.value_to_address.len()
    }
}

impl<'a> CodeGen<'a> {
    pub fn new(backend: &'a dyn Backend, module: ASTModule) -> Self {
        Self {
            module,
            body: String::new(),
            statics: HashMap::new(),
            id: 0,
            definitions: String::new(),
            lambdas: Vec::new(),
            functions: HashMap::new(),
            backend,
        }
    }

    pub fn asm(&mut self) -> String {
        self.id = 0;
        self.statics = HashMap::new();
        self.body = String::new();
        self.definitions = String::new();
        self.functions = HashMap::new();
        for function_def in &self.module.functions.clone() {
            self.functions.insert(function_def.name.clone(), function_def.clone());
        }

        for enum_def in &self.module.enums {
            for (variant_num, variant) in enum_def.variants.iter().enumerate() {
                let ast_type = ASTType::Custom { name: enum_def.name.clone(), param_types: enum_def.type_parameters.clone() };
                let type_ref = ASTTypeRef { ast_type, ast_ref: true };
                let return_type = Some(type_ref);
                let body_str = if variant.parameters.is_empty() {
                    let label = format!("_enum_{}_{}", enum_def.name, variant.name);
                    self.id += 1;
                    self.statics.insert(label.clone(), MemoryValue::I32Value(variant_num as i32));
                    format!("\tmov    eax, {}\n", label)
                } else {
                    let mut body = String::new();
                    CodeGen::add(&mut body, "\tpush ecx");
                    CodeGen::add(&mut body, "\tpush ebx");
                    CodeGen::add(&mut body, &format!("\tpush     {}", (variant.parameters.len() + 1) * self.backend.word_len() as usize));
                    CodeGen::add(&mut body, "\tcall malloc");
                    CodeGen::add(&mut body, "\tmov   ecx, eax");
                    CodeGen::add(&mut body, &format!("\tadd esp,{}", self.backend.word_len()));
                    CodeGen::add(&mut body, &format!("\tmov   [eax], word {}", variant_num));
                    for par in variant.parameters.iter() {
                        CodeGen::add(&mut body, &format!("\tadd   eax, {}", self.backend.word_len()));
                        CodeGen::add(&mut body, &format!("\tmov   ebx, ${}", par.name));
                        CodeGen::add(&mut body, "\tmov   [eax], ebx");
                    }
                    CodeGen::add(&mut body, "\tmov   eax, ecx");
                    CodeGen::add(&mut body, "\tpop ebx");
                    CodeGen::add(&mut body, "\tpop ecx");
                    body
                };
                let body = ASTFunctionBody::ASMBody(body_str);
                let function_def = ASTFunctionDef { name: enum_def.name.clone() + "_" + &variant.name.clone(), parameters: variant.parameters.clone(), body, inline: false, return_type, param_types: Vec::new() };
                self.functions.insert(enum_def.name.clone() + "::" + &variant.name.clone(), function_def);
            }
            let ast_type = ASTType::Builtin(BuiltinTypeKind::ASTI32);
            let type_ref = ASTTypeRef { ast_type, ast_ref: true };
            let return_type = Some(type_ref);
            let mut body = String::new();

            CodeGen::add(&mut body, "\tmov eax, $value");

            for (variant_num, variant) in enum_def.variants.iter().enumerate() {
                CodeGen::add(&mut body, &format!("\tcmp [eax], word {}", variant_num));
                CodeGen::add(&mut body, &format!("\tjnz .variant{}", variant_num));
                CodeGen::add(&mut body, &format!("\tcall ${}", variant.name));
                CodeGen::add(&mut body, "\tjmp .end");
                CodeGen::add(&mut body, &format!(".variant{}:", variant_num));
            }
            CodeGen::add(&mut body, ".end:");

            let function_body = ASTFunctionBody::ASMBody(body);
            let mut parameters = vec![ASTParameterDef { name: "value".into(), type_ref: ASTTypeRef { ast_type: ASTType::Custom { name: enum_def.name.clone(), param_types: enum_def.type_parameters.clone() }, ast_ref: true } }];
            for variant in enum_def.variants.iter() {
                parameters.push(ASTParameterDef { name: variant.name.clone(), type_ref: ASTTypeRef { ast_type: lambda_unit(), ast_ref: false } });
            }
            // TODO I would like to call it Enum::match
            let function_def = ASTFunctionDef { name: enum_def.name.clone() + "Match", parameters, body: function_body, inline: false, return_type, param_types: Vec::new() };
            self.functions.insert(enum_def.name.clone() + "Match", function_def);
        }

        // for now main has no context
        let main_context = VarContext::new();

        for function_call in &self.module.body.clone() {
            let s = self.function_call(function_call, &main_context, None, 0);
            self.body.push_str(&s);
        }

        for function_def in self.functions.clone().values() {
            self.add_function_def(function_def, 0);
        }

        // TODO add a command line argument
        //Parser::print(&self.module);

        for lambda_call in &self.lambdas.clone() {
            self.add_function_def(&lambda_call.def, lambda_call.parameters_offset);
            //Parser::print_function_def(&lambda_call.def);
        }

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
                        def.push_str("    dw    ");
                        def.push_str(&format!("{}", i));
                    }
                }
                CodeGen::add(&mut asm, &def);
            }
        }

        CodeGen::add(&mut asm, "section .bss");
        CodeGen::add(&mut asm, "  _heap_buffer     resb 1024");
        CodeGen::add(&mut asm, "  _heap            resw 1");
        CodeGen::add(&mut asm, "  _rasm_buffer_10b resb 10");
        // command line arguments
        CodeGen::add(&mut asm, "  _rasm_args resw 12");

        CodeGen::add(&mut asm, "SECTION .text");
        CodeGen::add(&mut asm, "global  main");
        CodeGen::add(&mut asm, "");
        CodeGen::add(&mut asm, "main:");

        // command line arguments
        for i in 0..12 {
            CodeGen::add(&mut asm, &format!("mov     eax,[esp + {}]", i * self.backend.word_len()));
            CodeGen::add(&mut asm, &format!("mov     [_rasm_args + {}], eax", i * self.backend.word_len()));
        }

        CodeGen::add(&mut asm, "mov     eax, _heap_buffer\n");
        CodeGen::add(&mut asm, "mov     [_heap], eax\n");

        asm.push_str(&self.body);

        // exit sys call
        CodeGen::add(&mut asm, "    mov     ebx, 0");
        CodeGen::add(&mut asm, "    mov     eax, 1");
        CodeGen::add(&mut asm, "    int     80h");
        CodeGen::add(&mut asm, "    ret");

        asm.push_str(&self.definitions);
        asm
    }

    fn add_function_def(&mut self, function_def: &ASTFunctionDef, parameters_offset: usize) {
        CodeGen::add(&mut self.definitions, &format!("{}:", function_def.name));

        let sp = self.backend.stack_pointer();
        let bp = self.backend.stack_base_pointer();

        CodeGen::add(&mut self.definitions, &format!("    push    {}", bp));
        CodeGen::add(&mut self.definitions, &format!("    mov     {},{}", bp, sp));

        let mut context = VarContext::new();

        let mut function_call_parameters = FunctionCallParameters::new(self.backend, function_def.clone(), false);

        let mut i = 0;
        while i < function_def.parameters.len() {
            if let Some(par) = function_def.parameters.get(i) {
                context.insert(par.name.clone(), VarKind::ParameterRef(i + parameters_offset, par.type_ref.clone()));
                function_call_parameters.add_var(&par.name, &par.type_ref, i);
            }
            i += 1;
        }

        match &function_def.body {
            ASTFunctionBody::RASMBody(calls) => {
                for call in calls {
                    let s = self.function_call(call, &context, Some(function_def), 0);
                    self.definitions.push_str(&s);
                }
            }
            ASTFunctionBody::ASMBody(s) => self.definitions.push_str(
                &function_call_parameters.resolve_asm_parameters(s, 0))
        }

        CodeGen::add(&mut self.definitions, &format!("    pop     {}", bp));
        CodeGen::add(&mut self.definitions, "    ret");
    }

    fn function_call(&mut self, function_call: &ASTFunctionCall, context: &VarContext, parent_def: Option<&ASTFunctionDef>, added_to_stack: usize) -> String {
        let mut before = String::new();

        let call_function_def = self.functions.get(&function_call.function_name)
            .unwrap_or_else(|| panic!("Cannot find function '{}'", function_call.function_name)).clone();

        if call_function_def.inline && parent_def.is_some() {
            CodeGen::add(&mut before, &format!("; inlining function {}", function_call.function_name));
        } else {
            if call_function_def.inline {
                CodeGen::add(&mut before, "; function is inline, but not inside a function");
                CodeGen::add(&mut before, "; so cannot be inlined.");
            }
            CodeGen::add(&mut before, &format!("; calling function {}", function_call.function_name));
        }

        let has_lambda = function_call.parameters.iter().any(|it| {
            matches!(it, ASTExpression::Lambda(_))
        });

        let mut to_remove_from_stack = 0;

        if has_lambda {
            context.iter().for_each(|(_, kind)| {
                if parent_def.is_some() {
                    if let VarKind::ParameterRef(index, par_type_ref) = kind {
                        let wl = self.backend.word_len() as usize;
                        let bp = self.backend.stack_base_pointer();

                        let type_size = self.backend.type_size(par_type_ref).unwrap_or_else(|| panic!("Unsupported type size: {:?}", par_type_ref));

                        // parameters must be pushed in reverse order
                        CodeGen::add(&mut before, &format!("    push    {} [{}+{}+{}]", type_size, bp, wl, (context.len() - index) * wl));
                        to_remove_from_stack += 1;
                    }
                }
            });
        }

        let mut call_parameters = FunctionCallParameters::new(self.backend, call_function_def.clone(),
                                                              call_function_def.inline && parent_def.is_some());

        if !function_call.parameters.is_empty() {
            let mut param_index = function_call.parameters.len();
            // as for C calling conventions parameters are pushed in reverse order
            for expr in function_call.parameters.iter().rev() {
                let param_opt = call_function_def.parameters.get(param_index - 1);
                let param_name = param_opt.unwrap_or_else(|| panic!("Cannot find param {} of function call {}", param_index - 1, function_call.function_name)).name.clone();
                param_index -= 1;

                match expr {
                    ASTExpression::StringLiteral(value) => {
                        let label = format!("_rasm_s{}", self.id);
                        self.id += 1;
                        self.statics.insert(label.clone(), MemoryValue::StringValue(value.clone()));
                        call_parameters.add_string_literal(&param_name, label);
                    }
                    ASTExpression::Number(n) => {
                        call_parameters.add_number(&param_name, n);
                    }
                    ASTExpression::ASTFunctionCallExpression(call) => {
                        call_parameters.push(&self.function_call(call, context, parent_def, added_to_stack +
                            to_remove_from_stack + call_parameters.to_remove_from_stack()));
                        call_parameters.add_function_call();
                    }
                    ASTExpression::Var(name) => {
                        if let Some(var_kind) = context.get(name) {
                            match var_kind {
                                VarKind::ParameterRef(index, par_type_ref) => {
                                    call_parameters.add_var(&param_name, par_type_ref, *index);
                                }
                            }
                        } else {
                            panic!("Cannot find variable {}, calling function {}", name, function_call.function_name);
                        }
                    }
                    ASTExpression::Lambda(function_def) => {
                        let mut def = function_def.clone();
                        def.name = format!("lambda{}", self.id);
                        self.id += 1;
                        context.iter().for_each(|(name, kind)| {
                            if let VarKind::ParameterRef(_, par_type) = kind {
                                def.parameters.push(ASTParameterDef { name: name.into(), type_ref: par_type.clone() })
                            }
                        });

                        if let ASTFunctionBody::RASMBody(_) = &function_def.body {
                            CodeGen::add(&mut before, &format!("    push     {}", def.name));
                            to_remove_from_stack += 1;
                            // 2 I think is the SP or PC that has been pushed to the stack, but it's not pushed for inline functions
                            let offset = if call_function_def.inline {
                                0
                            } else {
                                2
                            };
                            self.lambdas.push(LambdaCall { def, parameters_offset: function_call.parameters.len() + offset });
                        } else {
                            panic!("A lambda cannot have an asm body.")
                        }
                    }
                }
            }
        }

        before = before.add(call_parameters.before());

        // I can only inline functions if are called inside another function, otherwise I cannot access to the base pointer and
        // I must access to the stack pointer, but if the function, as usual,
        if call_function_def.inline && parent_def.is_some() {
            if let ASTFunctionBody::ASMBody(body) = &call_function_def.body {
                CodeGen::add(&mut before, &format!("; To remove from stack  {} {}", call_function_def.name, added_to_stack +
                    to_remove_from_stack + call_parameters.to_remove_from_stack()));
                before.push_str(&call_parameters.resolve_asm_parameters(body, added_to_stack + to_remove_from_stack));
            } else {
                panic!("Only asm can be inlined, for now...");
            }
        } else {
            // sometimes the function name is different from the function definition name, because it is not a valid ASM name (for enum types is enu-name::enum-variant)
            let real_function_name = self.functions.get(&function_call.function_name).unwrap().clone().name;
            CodeGen::add(&mut before, &format!("    call    {}", real_function_name));
        }

        if to_remove_from_stack + call_parameters.to_remove_from_stack() > 0 {
            let sp = self.backend.stack_pointer();
            let wl = self.backend.word_len() as usize;
            CodeGen::add(&mut before, &format!("    add     {},{}", sp, wl * (to_remove_from_stack + call_parameters.to_remove_from_stack())));
        }

        if call_function_def.inline && parent_def.is_some() {
            CodeGen::add(&mut before, &format!("; end inlining function {}", function_call.function_name));
        } else {
            CodeGen::add(&mut before, &format!("; end calling function {}", function_call.function_name));
        }

        before
    }

    fn add(dest: &mut String, code: &str) {
        if code.is_empty() {
            let string = " ".repeat(50);
            dest.push_str(&string);
        } else {
            let s = format!("{:width$}", code, width = 50);
            assert_eq!(s.len(), 50);
            dest.push_str(&s);
        }

        dest.push_str("; generated\n");
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
