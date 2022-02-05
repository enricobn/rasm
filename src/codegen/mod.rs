use std::collections::HashMap;
use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTModule};

pub fn gen_asm(module: ASTModule) -> String {

    let mut id: usize = 0;
    let mut statics: HashMap<String,String> = HashMap::new();
    let mut body = String::new();
    let mut definitions = String::new();

    for function_call in module.body {
        for expr in function_call.parameters {
            match expr {
                ASTExpression::StringLiteral(value) => {
                    let label = format!("_rasm_s{}", id);
                    id += 1;
                    statics.insert(label.clone(), value);
                    body.push_str(&format!("    push    {}\n", label));
                }
                ASTExpression::ASTFunctionCallExpression(_) => {
                    panic!("Unsupported...")
                }
                ASTExpression::Var(_) => {
                    // TODO
                }
            }
        }
        body.push_str(&format!("    call    {}\n", function_call.function_name));
    }

    for function_def in module.functions {
        definitions.push_str("\n");
        definitions.push_str(&format!("{}:\n", function_def.name));
        definitions.push_str("push    ebp\n");
        definitions.push_str("mov     ebp,esp\n");

        match function_def.body {
            ASTFunctionBody::RASMBody(_) => {}
            ASTFunctionBody::ASMBody(s) => definitions.push_str(&s)
        }

        definitions.push_str("pop     ebp\n");
        definitions.push_str("ret\n");
    }

    let mut asm = String::new();

    if !statics.is_empty() {
        asm.push_str("SECTION .data\n");
        for (id, value) in statics {
            asm.push_str(&id);
            asm.push_str("    db    ");
            asm.push_str(&format!("'{}', 0h\n", value));
        }
    }

    asm.push_str("SECTION .text\n");
    asm.push_str("global  _start\n");
    asm.push_str("\n");
    asm.push_str("_start:\n");

    asm.push_str(&body);

    asm.push_str("    mov     ebx, 01\n");
    asm.push_str("    mov     eax, 1\n");
    asm.push_str("    int     80h\n");
    asm.push_str("    ret\n");

    asm.push_str(&definitions);
    asm
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use crate::codegen::gen_asm;

    use crate::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test() {
        let path = Path::new("resources/test/helloworld.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse();

        let asm = gen_asm(module);

        println!("{}", asm);
    }
}