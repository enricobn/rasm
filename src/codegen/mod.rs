use std::collections::HashMap;

use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTModule};

pub struct CodeGen {
    module: ASTModule,
    id: usize,
    statics: HashMap<String, MemoryValue>, // key=memory_label
    body: String,
    definitions: String,
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

#[derive(Clone)]
enum VarKind {
    ParameterRef{index: usize, parameters_count: usize}
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
}

impl CodeGen {
    pub fn new(module: ASTModule) -> Self {
        Self { module, body: String::new(), statics: HashMap::new(), id: 0, definitions: String::new() }
    }

    pub fn asm(&mut self) -> String {
        self.id = 0;
        self.statics = HashMap::new();
        self.body = String::new();
        self.definitions = String::new();

        // for now main has no context
        let main_context = VarContext::new();

        for function_call in &self.module.body.clone() {
            let s = self.function_call(function_call, &main_context);
            self.body.push_str(&s);
        }

        for function_def in &self.module.functions.clone() {
            CodeGen::add(&mut self.definitions, &format!("{}:", function_def.name));
            CodeGen::add(&mut self.definitions, "    push    ebp");
            CodeGen::add(&mut self.definitions, "    mov     ebp,esp");

            let mut context = VarContext::new();
            let mut i = 0;
            while i < function_def.parameters.len() {
                if let Some(par) = function_def.parameters.get(i) {
                    context.insert(par.name.clone(), VarKind::ParameterRef{index: i, parameters_count: function_def.parameters.len()});
                }
                i += 1;
            }

            match &function_def.body {
                ASTFunctionBody::RASMBody(calls) => {
                    for call in calls {
                        let s = self.function_call(call, &context);
                        self.definitions.push_str(&s);
                    }
                }
                ASTFunctionBody::ASMBody(s) => self.definitions.push_str(&s)
            }

            CodeGen::add(&mut self.definitions, "    pop     ebp");
            CodeGen::add(&mut self.definitions, "    ret");
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

    fn function_call(&mut self, function_call: &ASTFunctionCall, context: &VarContext) -> String {
        let mut before = String::new();
        CodeGen::add(&mut before, &format!("; calling function {}", function_call.function_name));
        let mut after = String::new();
        for expr in &function_call.parameters {
            match expr {
                ASTExpression::StringLiteral(value) => {
                    let label = format!("_rasm_s{}", self.id);
                    self.id += 1;
                    self.statics.insert(label.clone(), MemoryValue::StringValue(value.clone()));
                    CodeGen::add(&mut before, &format!("    push    {}", label));
                    // after calling the function, we must remove the address from the stack
                    CodeGen::add(&mut after, "    add     esp,4");
                }
                ASTExpression::Number(n) => {
                    CodeGen::add(&mut before, &format!("    push    {}", n));
                    // after calling the function, we must remove the address from the stack
                    CodeGen::add(&mut after, "    add     esp,4");
                }
                ASTExpression::ASTFunctionCallExpression(call) => {
                    before.push_str(&self.function_call(call, context));
                    // TODO I must get the register that is returned, getting that from the function def of the called function
                    CodeGen::add(&mut before, &format!("    push    eax"));
                    CodeGen::add(&mut after, "    add     esp,4");
                }
                ASTExpression::Var(name) => {
                    if let Some(var_kind) = context.get(name) {
                        match var_kind {
                            VarKind::ParameterRef{index, parameters_count} => {
                                CodeGen::add(&mut before, "    push    eax");
                                // the parameters relative to ebp are in reverse order:
                                // push 10, push 20: [ebp+4+4] = 20, [ebp+4+8] = 10
                                CodeGen::add(&mut before, &format!("    mov     eax,[ebp+4+{}]", (parameters_count - index) * 4));
                                CodeGen::add(&mut before, "    push    eax");
                                CodeGen::add(&mut after, "    pop     eax");
                                CodeGen::add(&mut after, "    pop     eax");
                            }
                        }
                    } else {
                        panic!("Cannot find variable {}", name);
                    }
                }
            }
        }
        CodeGen::add(&mut before, &format!("    call    {}", function_call.function_name));
        before.push_str(&after);
        CodeGen::add(&mut before, &format!("; end calling function {}", function_call.function_name));
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

    use crate::codegen::CodeGen;
    use crate::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test() {
        let path = Path::new("resources/test/helloworld.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse();
        let mut gen = CodeGen::new(module);

        let asm = gen.asm();

        let path = Path::new("resources/test/helloworld.asm");
        let mut expected = String::new();
        File::open(path).unwrap().read_to_string(&mut expected).unwrap();

        assert_eq!(asm.trim(), expected);
    }
}