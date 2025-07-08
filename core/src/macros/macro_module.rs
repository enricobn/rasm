use core::panic;

use rasm_parser::{
    lexer::Lexer,
    parser::{ast::ASTModule, Parser},
};

use crate::macros::macro_call_extractor::MacroCallExtractor;

/// Creates a new module from a macro call extractor, with a function for each macro call and a body
/// that gets a number as an argument, that is the macro id, then calls the related function and
/// prints the result.
pub fn create_macro_module(mce: &MacroCallExtractor) -> ASTModule {
    let mut body = String::new();
    body.push_str("let id = argv(1).fmap(fn(it) { it.toi32(); }).getOrElse(-1);\n");
    body.push_str("let functionToCall = \n");

    for (i, call) in mce.calls().iter().enumerate() {
        let function_name = format!("macroCall{}", call.id());
        let conditional = if i == 0 { "if" } else { ".elseIf" };
        body.push_str(&format!("{conditional}(id.eq({i}), {function_name})\n"));
    }
    body.push_str(".else(macroEmpty);\n");
    body.push_str("print(functionToCall());\n");

    for call in mce.calls().iter() {
        let function_name = format!("macroCall{}", call.id());

        body.push_str(&format!("pub fn {function_name}() -> str {{\n"));
        body.push_str(&format!(
            "let macroResult = {};\n",
            call.transformed_macro()
        ));
        body.push_str(
            "macroResult.match(fn (module) { module.toString(); }, fn (message) { message; };\n",
        );
        //body.push_str(&format!("{}.toString();", call.transformed_macro()));
        body.push_str("}\n");
    }

    body.push_str("pub fn macroEmpty() -> str {\"\";}");

    // println!("macro body:\n{}", body);

    let (module, errors) = Parser::new(Lexer::new(body)).parse();

    // it should not happens
    if !errors.is_empty() {
        for error in errors {
            println!("{}", error);
        }
        panic!();
    }

    module
}

#[cfg(test)]
mod tests {
    use rasm_parser::{
        catalog::{ModuleId, ModuleNamespace},
        parser::ast::{ASTFunctionCall, ASTPosition},
    };

    use crate::{
        macros::macro_call_extractor::MacroCall,
        type_check::ast_modules_container::ASTModulesContainer,
    };

    use super::*;

    #[test]
    fn test_create_macro_module() {
        let mce = MacroCallExtractor {
            container: ASTModulesContainer::new(),
            calls: vec![MacroCall {
                id: 1,
                module_namespace: ModuleNamespace::global(),
                module_id: ModuleId::global(),
                position: ASTPosition::none(),
                transformed_macro: ASTFunctionCall::new(
                    "testMacroCall".to_string(),
                    vec![],
                    ASTPosition::none(),
                    vec![],
                    None,
                    false,
                ),
            }],
        };

        println!("{}", create_macro_module(&mce));
    }
}
