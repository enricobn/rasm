use std::iter::zip;

use log::info;
use rasm_parser::{
    catalog::{ASTIndex, ModuleId, ModuleNamespace},
    parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTPosition, ASTStatement, ASTType,
        ASTValueType,
    },
};

use crate::type_check::ast_modules_container::ASTModulesContainer;

pub fn transform_macro_calls(container: ASTModulesContainer) -> ASTModulesContainer {
    info!("transform_macro_calls");
    let mut result = ASTModulesContainer::new();
    for (id, namespace, module) in container.modules().into_iter() {
        let mut new_module = module.clone().clone();
        new_module.functions.clear();
        for function in module.functions.iter() {
            let mut new_function = function.clone();
            if let ASTFunctionBody::RASMBody(ref body) = function.body {
                let new_statements =
                    transform_macro_calls_in_body(&container, &namespace, &id, body);
                new_function.body = ASTFunctionBody::RASMBody(new_statements);
            }

            new_module.add_function(new_function);
        }

        new_module.body = transform_macro_calls_in_body(&container, &namespace, &id, &module.body);

        result.add(
            new_module,
            namespace.clone(),
            id.clone(),
            false,
            container.is_readonly_module(id),
        );
    }

    info!("end transform_macro_calls");
    result
}

fn transform_macro_calls_in_body(
    container: &ASTModulesContainer,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    body: &Vec<ASTStatement>,
) -> Vec<ASTStatement> {
    let mut new_statements = Vec::with_capacity(body.len());
    for statement in body.iter() {
        if let ASTStatement::Expression(expr) = statement {
            if let ASTExpression::ASTFunctionCallExpression(call) = expr {
                if call.is_macro() {
                    let functions_vec = container
                        .signatures()
                        .into_iter()
                        .filter(|it| &it.signature.name == call.function_name())
                        .filter(|it| {
                            it.signature.modifiers.public
                                || it.module_info().namespace() == module_namespace
                        })
                        .collect::<Vec<_>>();
                    if functions_vec.is_empty() {
                        panic!("Macro {} not found", call.function_name());
                    } else if functions_vec.len() > 1 {
                        panic!("Macro {} is ambiguous", call.function_name());
                    }

                    let function = functions_vec[0];

                    if function.signature.parameters_types.len() != call.parameters().len() {
                        panic!(
                            "Macro {} expects {} arguments, but {} were given",
                            call.function_name(),
                            function.signature.parameters_types.len(),
                            call.parameters().len()
                        );
                    }

                    let new_parameters = zip(
                        function.signature.parameters_types.iter(),
                        call.parameters().iter(),
                    )
                    .map(|(parameter_type, parameter)| {
                        let is_expression = if let ASTType::Custom {
                            name,
                            param_types: _,
                            position: _,
                        } = parameter_type
                        {
                            name == "ASTExpression"
                        } else {
                            false
                        };

                        if is_expression {
                            // convert the parameter to a rasm expression
                            convert_to_rasm_expression(
                                container,
                                module_namespace,
                                module_id,
                                parameter,
                            )
                        } else if let ASTExpression::Value(_, _) = parameter {
                            // TODO lambda is allowed)?
                            if let ASTType::Builtin(_) = parameter_type {
                                parameter.clone()
                            } else {
                                panic!(
                                    "Type {} is not allowed as a macro parameter: {}",
                                    parameter_type,
                                    function.index()
                                );
                            }
                        } else {
                            panic!(
                                "Only ASTExpression or constant is allowed as a macro parameter : {}",
                                ASTIndex::new(module_namespace.clone(), module_id.clone(), call.position().clone())
                            );
                        }
                    })
                    .collect::<Vec<_>>();

                    let new_statement = ASTStatement::Expression(
                        ASTExpression::ASTFunctionCallExpression(ASTFunctionCall::new(
                            call.function_name().clone(),
                            new_parameters,
                            call.position().clone(),
                            call.generics().clone(),
                            call.target().clone(),
                            false,
                        )),
                    );

                    println!("new_statement {new_statement}");

                    new_statements.push(new_statement);
                    continue;
                }
            }
        }
        new_statements.push(statement.clone());
    }
    new_statements
}

fn convert_to_rasm_expression(
    container: &ASTModulesContainer,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    parameter: &ASTExpression,
) -> ASTExpression {
    match parameter {
        ASTExpression::ASTFunctionCallExpression(function_call) => {
            let mut fcp = Vec::new();
            fcp.push(ASTExpression::Value(
                ASTValueType::String(function_call.function_name().clone()),
                function_call.position().mv_right(2),
            ));
            fcp.push(call_vec_of(
                container,
                module_namespace,
                module_id,
                function_call.parameters(),
                function_call.position().mv_right(3),
            ));
            fcp.push(call_empty_vec(function_call.position().mv_right(4)));
            fcp.push(call_none(function_call.position().mv_right(5)));
            simple_call(
                "ASTFunctionCallExpression",
                vec![simple_call(
                    "ASTFunctionCall",
                    fcp,
                    function_call.position().mv_right(6),
                    None,
                )],
                function_call.position().clone(),
                Some("ASTExpression".to_owned()),
            )
        }
        ASTExpression::ValueRef(name, position) => simple_call(
            "ValueRef",
            vec![ASTExpression::Value(
                ASTValueType::String(name.clone()),
                position.mv_right(1), // it must be different for cache purposes
            )],
            position.clone(),
            Some("ASTExpression".to_owned()),
        ),
        ASTExpression::Value(value_type, position) => {
            let value_function = match value_type {
                ASTValueType::String(_) => "ASTString",
                ASTValueType::Boolean(_) => "Boolean",
                ASTValueType::I32(_) => "I32",
                ASTValueType::Char(_) => "Char",
                ASTValueType::F32(_) => "F32",
            };
            simple_call(
                "Value",
                vec![simple_call(
                    value_function,
                    vec![ASTExpression::Value(
                        value_type.clone(),
                        position.mv_right(2), // it must be different for cache purposes
                    )],
                    position.mv_right(1), // it must be different for cache purposes
                    Some("ASTValueType".to_owned()),
                )],
                position.clone(),
                Some("ASTExpression".to_owned()),
            )
        }
        ASTExpression::Lambda(lambda_def) => todo!(),
    }
}

fn simple_call(
    name: &str,
    parameters: Vec<ASTExpression>,
    position: ASTPosition,
    target: Option<String>,
) -> ASTExpression {
    ASTExpression::ASTFunctionCallExpression(ASTFunctionCall::new(
        name.to_string(),
        parameters,
        position,
        Vec::new(),
        None,
        false,
    ))
}

fn call_vec_of(
    container: &ASTModulesContainer,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    parameters: &Vec<ASTExpression>,
    position: ASTPosition,
) -> ASTExpression {
    simple_call(
        "vecOf",
        parameters
            .iter()
            .map(|it| convert_to_rasm_expression(container, module_namespace, module_id, it))
            .collect(),
        position,
        None,
    )
}

fn call_empty_vec(position: ASTPosition) -> ASTExpression {
    simple_call("Vec", Vec::new(), position, None)
}

fn call_none(position: ASTPosition) -> ASTExpression {
    simple_call("None", Vec::new(), position, None)
}
