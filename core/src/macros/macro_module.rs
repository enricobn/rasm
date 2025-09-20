use std::{collections::HashMap, sync::atomic::AtomicUsize};

use rasm_parser::{
    catalog::modules_catalog::ModulesCatalog,
    parser::ast::{
        ASTBuiltinTypeKind, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
        ASTLambdaDef, ASTModule, ASTParameterDef, ASTStatement, ASTType, ASTValue,
    },
};

use crate::{
    codegen::enh_ast::{EnhASTNameSpace, EnhModuleId},
    macros::macro_call_extractor::{is_ast_module_first_parameter, MacroCall, MacroResultType},
    type_check::ast_modules_container::ASTModulesContainer,
};

const ID: AtomicUsize = AtomicUsize::new(0);

/// Creates a new module from a macro call extractor, with a function for each macro call and a body
/// that gets a number as an argument, that is the macro id, then calls the related function and
/// prints the result.
pub fn create_macro_module(
    container: &ASTModulesContainer,
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    calls: &Vec<&MacroCall>,
) -> String {
    let mut constants = String::new();

    let mut modules_ids = HashMap::new();

    let mut body = String::new();
    body.push_str("let id = argv(1).fmap(fn(it) { it.toInt(); }).getOrElse(-1);\n");
    body.push_str("let functionToCall = \n");

    for (i, call) in calls.iter().enumerate() {
        let function_name = format!("macroCall{}", call.id());
        let conditional = if i == 0 { "if" } else { ".elseIf" };
        body.push_str(&format!(
            "{conditional}(id.eq({}), {function_name})\n",
            call.id
        ));
    }
    body.push_str(".else(macroEmpty);\n");
    body.push_str("print(functionToCall());\n");

    for call in calls.iter() {
        let function_name = format!("macroCall{}", call.id());

        body.push_str(&format!("pub fn {function_name}() -> str {{\n"));

        if is_ast_module_first_parameter(&call.function_signature) {
            if let Some((_enh_id, enh_namespace)) = catalog.catalog_info(&call.module_id) {
                let id = modules_ids.entry(enh_namespace).or_insert_with(|| {
                    let mut module_for_namespace = ASTModule::empty();

                    catalog
                        .ids_by_namespace(enh_namespace)
                        .iter()
                        .for_each(|(_id, module_id)| {
                            if let Some(module) = container.module(module_id) {
                                module_for_namespace.add(module.clone());
                            }
                        });

                    let new_id = ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    constants.push_str(&format!(
                        "const moduleAST{} = {};\n",
                        new_id,
                        ast_module(&module_for_namespace)
                    ));
                    new_id
                });
                body.push_str(&format!("let moduleAST = moduleAST{id}.selfASTModule;\n",));
            }
        }

        body.push_str(&format!(
            "let macroResult = {};\n",
            call.transformed_macro()
        ));

        match &call.macro_result_type {
            MacroResultType::Statement => {
                body.push_str(
                    "macroResult.match(fn (statements, functions) { \"Statement\\n\".add(statements.join(\"\\n\")).add(\"\\n\").add(functions.join(\"\\n\")); }, fn (message) { \"Error\\n\".add(message); });\n",
                );
            }
            MacroResultType::Expression => {
                body.push_str(
                    "macroResult.match(fn (expr, functions) { \"Expression\\n\".append(expr).add(\";\\n\").add(functions.join(\"\\n\")); }, fn (message) { \"Error\\n\".add(message); });\n",
                );
            }
            MacroResultType::Attribute => {
                body.push_str(
                    "macroResult.match(fn (functions) { \"Attribute\\n\".add(functions.join(\"\\n\")); }, fn (message) { \"Error\\n\".add(message); });\n",
                );
            }
        }

        body.push_str("}\n");
    }

    body.push_str("pub fn macroEmpty() -> str {\"\";}");
    // TODO it's a trick since for now we cannot directly point to a const in a let
    body.push_str("fn selfASTModule(module: ASTModule) -> ASTModule {module;}");

    // println!("body:\n{}", constants.clone() + &body);

    constants + &body
}

fn ast_module(module: &ASTModule) -> String {
    // println!("create ast_module");

    format!(
        "ASTModule({}, {}, Vec(), Vec(), Vec())",
        ast_body(&module.body),
        vec_of(module.functions.iter().map(ast_function_def).collect())
    )
}

fn ast_body(body: &Vec<ASTStatement>) -> String {
    vec_of(body.iter().map(ast_statement).collect())
}

fn ast_function_def(function_def: &ASTFunctionDef) -> String {
    let body = match &function_def.body {
        ASTFunctionBody::RASMBody(body) => format!(
            "RASMBody({})",
            vec_of(body.iter().map(ast_statement).collect())
        ),
        ASTFunctionBody::NativeBody(body) => format!("NativeBody({})", body),
    };

    let parameters = vec_of(function_def.parameters.iter().map(ast_parameter).collect());

    let target = match &function_def.target {
        Some(target) => format!("Some(\"{}\")", target),
        None => "None()".to_string(),
    };

    format!(
        "ASTFunctionDef(\"{}\", {parameters}, {}, {body}, {}, ASTModifiers({}), {target})",
        function_def.name,
        ast_type(&function_def.return_type),
        vec_of(
            function_def
                .generic_types
                .iter()
                .map(|it| format!("\"{}\"", it))
                .collect()
        ),
        function_def.modifiers.public,
    )
}

fn ast_parameter(parameter: &ASTParameterDef) -> String {
    format!(
        "ASTParameterDef(\"{}\", {})",
        parameter.name,
        ast_type(&parameter.ast_type)
    )
}

fn ast_type(at: &ASTType) -> String {
    match at {
        ASTType::ASTBuiltinType(builtin) => {
            format!("ASTBuiltinType({})", ast_builtin_type(builtin))
        }
        ASTType::ASTGenericType(_, name, generics) => format!(
            "ASTGenericType(\"{name}\", {})",
            vec_of(generics.iter().map(|it| ast_type(it)).collect())
        ),
        ASTType::ASTCustomType {
            name,
            param_types,
            position: _,
        } => format!(
            "ASTCustomType(\"{name}\", {})",
            vec_of(param_types.iter().map(ast_type).collect())
        ),
        ASTType::ASTUnitType => "ASTUnitType()".to_string(),
    }
}

fn ast_builtin_type(builtin: &ASTBuiltinTypeKind) -> String {
    match builtin {
        ASTBuiltinTypeKind::ASTBooleanType => "ASTBooleanType()".to_string(),
        ASTBuiltinTypeKind::ASTCharType => "ASTCharType()".to_string(),
        ASTBuiltinTypeKind::ASTIntegerType => "ASTIntegerType()".to_string(),
        ASTBuiltinTypeKind::ASTFloatType => "ASTFloatType()".to_string(),
        ASTBuiltinTypeKind::ASTStringType => "ASTStringType()".to_string(),
        ASTBuiltinTypeKind::ASTLambdaType {
            parameters,
            return_type,
        } => format!(
            "ASTLambdaType({}, {})",
            vec_of(parameters.iter().map(ast_type).collect()),
            ast_type(return_type)
        ),
    }
}

fn ast_statement(statement: &ASTStatement) -> String {
    match statement {
        ASTStatement::ASTExpressionStatement(e, _) => {
            format!("ASTExpressionStatement({})", ast_expression(e))
        }
        ASTStatement::ASTLetStatement(name, e, _) => {
            format!("ASTLetStatement(\"{name}\", {})", ast_expression(e))
        }
        ASTStatement::ASTConstStatement(name, e, _, modifiers) => {
            format!(
                "ASTConstStatement(\"{name}\", {}, ASTModifiers({}))",
                ast_expression(e),
                modifiers.public
            )
        }
    }
}

fn ast_expression(expression: &ASTExpression) -> String {
    match expression {
        ASTExpression::ASTFunctionCallExpression(function_call) => {
            format!(
                "ASTFunctionCallExpression({})",
                ast_function_call(function_call)
            )
        }
        ASTExpression::ASTLambdaExpression(lambda) => {
            format!("ASTLambdaExpression({})", ast_lambda_def(lambda))
        }
        ASTExpression::ASTValueRefExpression(name, _) => {
            format!("ASTValueRefExpression(\"{name}\")")
        }
        ASTExpression::ASTValueExpression(value, _) => {
            format!("ASTValueExpression({})", ast_value(value))
        }
    }
}

fn ast_lambda_def(lambda: &ASTLambdaDef) -> String {
    format!(
        "ASTLambdaDef({}, {})",
        vec_of(
            lambda
                .parameter_names
                .iter()
                .map(|(name, _)| format!("\"{name}\""))
                .collect()
        ),
        vec_of(lambda.body.iter().map(ast_statement).collect())
    )
}
fn ast_function_call(function_call: &ASTFunctionCall) -> String {
    let parameters = function_call
        .parameters()
        .iter()
        .map(|e| ast_expression(e))
        .collect::<Vec<String>>();
    format!(
        "ASTFunctionCall(\"{}\", {}, Vec(), None(), {})", // TODO generics, target
        function_call.function_name(),
        vec_of(parameters),
        function_call.is_macro()
    )
}

fn vec_of(parameters: Vec<String>) -> String {
    if parameters.is_empty() {
        return "Vec()".to_string();
    } else if parameters.len() <= 5 {
        format!("vecOf({})", parameters.join(", "))
    } else {
        let mut result = format!("vecOf({})", parameters[0]);
        for partition in parameters[1..].chunks(5) {
            result += &format!(".add({})", vec_of(partition.to_vec()));
        }
        result
    }
}

fn ast_value(value: &ASTValue) -> String {
    match value {
        ASTValue::ASTStringValue(s) => format!("ASTStringValue(\"{s}\")"),
        ASTValue::ASTBooleanValue(b) => format!("ASTBooleanValue({b})"),
        ASTValue::ASTIntegerValue(i) => format!("ASTIntegerValue({i})"),
        ASTValue::ASTCharValue(c) => format!("ASTCharValue(\"{c}\")"),
        ASTValue::ASTFloatValue(f) => format!("ASTFloatValue({f})"),
    }
}

#[cfg(test)]
mod tests {
    use rasm_parser::{
        catalog::{ModuleId, ModuleNamespace},
        parser::ast::{ASTFunctionCall, ASTFunctionSignature, ASTModifiers, ASTPosition},
    };

    use crate::{
        macros::macro_call_extractor::{MacroCall, MacroCallExtractor, MacroResultType},
        project_catalog::RasmProjectCatalog,
    };

    use super::*;

    #[test]
    fn test_create_macro_module() {
        let mce = MacroCallExtractor {
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
                macro_result_type: MacroResultType::Statement,
                function_signature: ASTFunctionSignature {
                    parameters_types: vec![],
                    return_type: ASTType::ASTUnitType,
                    name: "testMacroCall".to_string(),
                    generics: vec![],
                    modifiers: ASTModifiers::public(),
                },
                in_function: None,
            }],
            attribute_macros: Vec::new(),
        };

        let container = ASTModulesContainer::new();

        println!(
            "{}",
            create_macro_module(&container, &RasmProjectCatalog::new(), &mce.calls())
        );
    }
}
