use std::collections::hash_map::Iter;
use std::collections::HashMap;

use crate::Parser;
use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef};

pub struct CodeGen {
    module: ASTModule,
    id: usize,
    statics: HashMap<String, MemoryValue>,
    // key=memory_label
    body: String,
    definitions: String,
    lambdas: Vec<LambdaCall>,
    functions: HashMap<String, ASTFunctionDef>,
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
    ParameterRef(usize)
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

    fn get(&self, key: &String) -> Option<&VarKind> {
        self.value_to_address.get(key)
    }

    fn iter(&self) -> Iter<'_, String, VarKind> {
        self.value_to_address.iter()
    }

    fn len(&self) -> usize {
        self.value_to_address.len()
    }
}

impl CodeGen {
    pub fn new(module: ASTModule) -> Self {
        Self {
            module,
            body: String::new(),
            statics: HashMap::new(),
            id: 0,
            definitions: String::new(),
            lambdas: Vec::new(),
            functions: HashMap::new(),
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


        // for now main has no context
        let main_context = VarContext::new();

        for function_call in &self.module.body.clone() {
            let s = self.function_call(function_call, &main_context, None, 0);
            self.body.push_str(&s);
        }

        for function_def in &self.module.functions.clone() {
            if !function_def.inline {
                self.add_function_def(&function_def, 0);
            }
        }

        Parser::print(&self.module);

        for lambda_call in &self.lambdas.clone() {
            self.add_function_def(&lambda_call.def, lambda_call.parameters_offset);
            Parser::print_function_def(&lambda_call.def);
        }

        let mut asm = String::new();

        if !self.statics.is_empty() {
            asm.push_str("SECTION .data\n");
            let mut keys: Vec<&String> = self.statics.keys().collect();
            // sorted for test purposes
            keys.sort();

            for id in keys.iter() {
                let mut def = String::new();
                def.push_str(&id);
                def.push_str("    db    ");
                match self.statics.get(*id).unwrap() {
                    MemoryValue::StringValue(s) => {
                        def.push_str(&format!("'{}', 0h", s));
                    }
                    MemoryValue::I32Value(_) => {
                        panic!("Not yet supported")
                    }
                }
                CodeGen::add(&mut asm, &def);
            }
        }

        CodeGen::add(&mut asm, "section .bss");
        CodeGen::add(&mut asm, "  _rasm_buffer_10b resb 10");

        CodeGen::add(&mut asm, "SECTION .text");
        CodeGen::add(&mut asm, "global  main");
        CodeGen::add(&mut asm, "");
        CodeGen::add(&mut asm, "main:");

        asm.push_str(&self.body);

        CodeGen::add(&mut asm, "    mov     ebx, 01");
        CodeGen::add(&mut asm, "    mov     eax, 1");
        CodeGen::add(&mut asm, "    int     80h");
        CodeGen::add(&mut asm, "    ret");

        asm.push_str(&self.definitions);
        asm
    }

    fn add_function_def(&mut self, function_def: &ASTFunctionDef, parameters_offset: usize) {
        CodeGen::add(&mut self.definitions, &format!("{}:", function_def.name));
        CodeGen::add(&mut self.definitions, "    push    ebp");
        CodeGen::add(&mut self.definitions, "    mov     ebp,esp");

        let mut context = VarContext::new();
        let mut i = 0;
        while i < function_def.parameters.len() {
            if let Some(par) = function_def.parameters.get(i) {
                context.insert(par.name.clone(), VarKind::ParameterRef(i + parameters_offset));
            }
            i += 1;
        }

        match &function_def.body {
            ASTFunctionBody::RASMBody(calls) => {
                for call in calls {
                    let s = self.function_call(call, &context, Some(&function_def), 0);
                    self.definitions.push_str(&s);
                }
            }
            ASTFunctionBody::ASMBody(s) => self.definitions.push_str(&Self::resolve_asm_parameters(function_def, s, 0))
        }

        CodeGen::add(&mut self.definitions, "    pop     ebp");
        CodeGen::add(&mut self.definitions, "    ret");
    }

    fn function_call(&mut self, function_call: &ASTFunctionCall, context: &VarContext, parent_def: Option<&ASTFunctionDef>, added_to_stack: usize) -> String {
        let mut before = String::new();

        let call_function_def = self.functions.get(&function_call.function_name)
            .expect(&format!("Cannot find function '{}'", function_call.function_name)).clone();

        if call_function_def.inline {
            CodeGen::add(&mut before, &format!("; inlining function {}", function_call.function_name));
        } else {
            CodeGen::add(&mut before, &format!("; calling function {}", function_call.function_name));
        }

        let has_lambda = function_call.parameters.iter().any(|it| {
            if let ASTExpression::Lambda(_) = it {
                true
            } else {
                false
            }
        });

        let mut to_remove_from_stack = 0;
        if has_lambda {
            context.iter().for_each(|(_, kind)| {
                if let Some(_) = parent_def {
                    if let VarKind::ParameterRef(index) = kind {
                        // parameters must be pushed in reverse order
                        CodeGen::add(&mut before, &format!("    push    dword[ebp+4+{}]", (context.len() - index) * 4));
                        to_remove_from_stack += 1;
                    }
                }
            });
            //CodeGen::add(&mut before, &format!("    push    dword {}", context.len()));
            //to_remove_from_stack += 1;
        }

        // as for C calling conventions parameters are pushed in reverse order
        for expr in function_call.parameters.iter().rev() {
            match expr {
                ASTExpression::StringLiteral(value) => {
                    let label = format!("_rasm_s{}", self.id);
                    self.id += 1;
                    self.statics.insert(label.clone(), MemoryValue::StringValue(value.clone()));
                    CodeGen::add(&mut before, &format!("    push    {}", label));
                    to_remove_from_stack += 1;
                }
                ASTExpression::Number(n) => {
                    CodeGen::add(&mut before, &format!("    push    {}", n));
                    to_remove_from_stack += 1;
                }
                ASTExpression::ASTFunctionCallExpression(call) => {
                    before.push_str(&self.function_call(call, context, parent_def, added_to_stack + to_remove_from_stack));
                    // TODO I must get the register that is returned, getting that from the function def of the called function
                    CodeGen::add(&mut before, &format!("    push    eax"));
                    to_remove_from_stack += 1;
                }
                ASTExpression::Var(name) => {
                    if let Some(var_kind) = context.get(name) {
                        match var_kind {
                            VarKind::ParameterRef(index) => {
                                CodeGen::add(&mut before, &format!("    push     dword [ebp+4+{}]", (index + 1) * 4));
                                to_remove_from_stack += 1;
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
                        if let Some(pdef) = parent_def {
                            if let VarKind::ParameterRef(index) = kind {
                                let par = pdef.parameters.get(*index).unwrap().clone();
                                def.parameters.push(ASTParameterDef { name: name.into(), type_ref: par.type_ref })
                            }
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

        if call_function_def.inline {
            if let ASTFunctionBody::ASMBody(body) = &call_function_def.body {
                CodeGen::add(&mut before, &format!("; To remove from stack  {} {}", call_function_def.name, added_to_stack + to_remove_from_stack));
                before.push_str(&Self::resolve_asm_parameters(&call_function_def, body, added_to_stack + to_remove_from_stack));
            } else {
                panic!("Only asm can be inlined, for now...");
            }
        } else {
            CodeGen::add(&mut before, &format!("    call    {}", function_call.function_name));
        }

        if to_remove_from_stack > 0 {
            CodeGen::add(&mut before, &format!("    add     esp,{}", 4 * to_remove_from_stack));
        }

        if call_function_def.inline {
            CodeGen::add(&mut before, &format!("; end inlining function {}", function_call.function_name));
        } else {
            CodeGen::add(&mut before, &format!("; end calling function {}", function_call.function_name));
        }
        before
    }

    fn resolve_asm_parameters(function_def: &ASTFunctionDef, body: &str, to_remove_from_stack: usize) -> String {
        let mut result = body.to_string();
        let mut i = 0;
        for par in function_def.parameters.iter() {
            let relative_address =
            if function_def.inline {
                (i as i32 -to_remove_from_stack as i32) * 4
            } else {
                (i + 2) * 4
            };
            let address = if relative_address < 0 {
                format!("[ebp-{}]", -relative_address)
            } else {
                format!("[ebp+{}]", relative_address)
            };
            result = result.replace(&format!("${}", par.name), &address);
            i += 1;
        }
        result
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

    use crate::codegen::CodeGen;
    use crate::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test() {
        let path = Path::new("resources/test/helloworld.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse(path);

        let mut gen = CodeGen::new(module);

        let asm = gen.asm();

        let path = Path::new("resources/test/helloworld.asm");
        let mut expected = String::new();
        File::open(path).unwrap().read_to_string(&mut expected).unwrap();

        assert_eq!(asm.trim(), expected);
    }

    #[test]
    fn test_fib() {
        let path = Path::new("resources/test/fibonacci.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse(path);

        let mut gen = CodeGen::new(module);

        let asm = gen.asm();

        let path = Path::new("resources/test/fibonacci.asm");
        let mut expected = String::new();
        File::open(path).unwrap().read_to_string(&mut expected).unwrap();

        assert_eq!(asm.trim(), expected);
    }
}