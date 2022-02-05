use std::collections::HashMap;
use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTModule};

pub struct CodeGen {
    module: ASTModule,
    id: usize,
    statics: HashMap<String,String>,
    body: String,
    definitions: String,
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

        for function_call in &self.module.body.clone() {
            let s = self.function_call(function_call);
            self.body.push_str(&s);
        }

        for function_def in &self.module.functions {
            self.definitions.push_str("\n");
            self.definitions.push_str(&format!("{}:\n", function_def.name));
            self.definitions.push_str("push    ebp\n");
            self.definitions.push_str("mov     ebp,esp\n");

            match &function_def.body {
                ASTFunctionBody::RASMBody(_) => {}
                ASTFunctionBody::ASMBody(s) => self.definitions.push_str(&s)
            }

            self.definitions.push_str("pop     ebp\n");
            self.definitions.push_str("ret\n");
        }

        let mut asm = String::new();

        if !self.statics.is_empty() {
            asm.push_str("SECTION .data\n");
            for (id, value) in &self.statics {
                asm.push_str(&id);
                asm.push_str("    db    ");
                asm.push_str(&format!("'{}', 0h\n", value));
            }
        }

        asm.push_str("SECTION .text\n");
        asm.push_str("global  _start\n");
        asm.push_str("\n");
        asm.push_str("_start:\n");

        asm.push_str(&self.body);

        asm.push_str("    mov     ebx, 01\n");
        asm.push_str("    mov     eax, 1\n");
        asm.push_str("    int     80h\n");
        asm.push_str("    ret\n");

        asm.push_str(&self.definitions);
        asm
    }

    fn function_call(&mut self, function_call: &ASTFunctionCall) -> String {
        let mut result = String::new();
        for expr in &function_call.parameters {
            match expr {
                ASTExpression::StringLiteral(value) => {
                    let label = format!("_rasm_s{}", self.id);
                    self.id += 1;
                    self.statics.insert(label.clone(), value.clone());
                    result.push_str(&format!("    push    {}\n", label));
                }
                ASTExpression::ASTFunctionCallExpression(_) => {
                    panic!("Unsupported...")
                }
                ASTExpression::Var(_) => {
                    // TODO
                }
            }
        }
        result.push_str(&format!("    call    {}\n", function_call.function_name));
        result
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