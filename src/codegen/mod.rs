use std::collections::HashMap;

use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTModule};

pub struct CodeGen {
    module: ASTModule,
    id: usize,
    // TODO for test purposes we should let it be an aordered map, ordered bu insertion (https://crates.io/crates/indexmap)
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
    ParameterRef(usize) // the parameter index
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
            CodeGen::add_generated(&mut self.definitions, "");
            CodeGen::add_generated(&mut self.definitions,&format!("{}:", function_def.name));
            CodeGen::add_generated(&mut self.definitions,"    push    ebp");
            CodeGen::add_generated(&mut self.definitions,"    mov     ebp,esp");

            let mut context = VarContext::new();
            let mut i = 0;
            while i < function_def.parameters.len() {
                if let Some(par) = function_def.parameters.get(i) {
                    context.insert(par.name.clone(), VarKind::ParameterRef(i));
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

            CodeGen::add_generated(&mut self.definitions,"    pop     ebp");
            CodeGen::add_generated(&mut self.definitions,"    ret");
        }

        let mut asm = String::new();

        if !self.statics.is_empty() {
            asm.push_str("SECTION .data\n");
            for (id, value) in self.statics.iter() {
                let mut def = String::new();
                def.push_str(&id);
                def.push_str("    db    ");
                match value {
                    MemoryValue::StringValue(s) => {
                        def.push_str(&format!("'{}', 0h", s));
                    }
                    MemoryValue::I32Value(_) => {
                        panic!("Not yet supported")
                    }
                }
                CodeGen::add_generated(&mut asm, &def);
            }
        }

        CodeGen::add_generated(&mut asm, "SECTION .text");
        CodeGen::add_generated(&mut asm,"global  main");
        CodeGen::add_generated(&mut asm,"");
        CodeGen::add_generated(&mut asm,"main:");

        asm.push_str(&self.body);

        CodeGen::add_generated(&mut asm,"    mov     ebx, 01");
        CodeGen::add_generated(&mut asm,"    mov     eax, 1");
        CodeGen::add_generated(&mut asm,"    int     80h");
        CodeGen::add_generated(&mut asm,"    ret");

        asm.push_str(&self.definitions);
        asm
    }

    fn function_call(&mut self, function_call: &ASTFunctionCall, context: &VarContext) -> String {
        let mut before = String::new();
        let mut after = String::new();
        for expr in &function_call.parameters {
            match expr {
                ASTExpression::StringLiteral(value) => {
                    let label = format!("_rasm_s{}", self.id);
                    self.id += 1;
                    self.statics.insert(label.clone(), MemoryValue::StringValue(value.clone()));
                    CodeGen::add_generated(&mut before, &format!("    push    {}", label));
                    // after calling the function, we must remove the address from the stack
                    CodeGen::add_generated(&mut after, "    add esp,4");
                }
                ASTExpression::ASTFunctionCallExpression(_) => {
                    panic!("Unsupported...")
                }
                ASTExpression::Var(name) => {
                    if let Some(var_kind) = context.get(name) {
                        match var_kind {
                            VarKind::ParameterRef(par_index) => {
                                CodeGen::add_generated(&mut before, "    push    eax");
                                CodeGen::add_generated(&mut before, &format!("    mov     eax,[ebp+4+{}]", (par_index + 1) * 4));
                                CodeGen::add_generated(&mut before,"    push    eax");
                                CodeGen::add_generated(&mut after,"    pop     eax");
                                CodeGen::add_generated(&mut after,"    pop     eax");
                            }
                        }
                    } else {
                        panic!("Cannot find variable {}", name);
                    }
                }
            }
        }
        CodeGen::add_generated(&mut before, &format!("    call    {}", function_call.function_name));
        before.push_str(&after);
        before
    }

    fn add_generated(dest: &mut String, code: &str) {
        dest.push_str(&format!("{:width$}", code, width=50));
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

        assert_eq!(asm, expected);
    }
}