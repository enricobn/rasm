use std::iter::zip;

use log::info;
use rasm_parser::{
    catalog::{modules_catalog::ModulesCatalog, ASTIndex, ModuleId, ModuleNamespace},
    parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTPosition, ASTStatement, ASTStructDef,
        ASTType, ASTValue,
    },
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    codegen::enh_ast::{EnhASTNameSpace, EnhModuleId},
    type_check::ast_modules_container::ASTModulesContainer,
};

pub struct MacroCallExtractor {
    pub container: ASTModulesContainer,
    pub calls: Vec<MacroCall>,
    pub attribute_macros: Vec<MacroCall>,
}

impl MacroCallExtractor {
    pub fn container(&self) -> &ASTModulesContainer {
        &self.container
    }

    pub fn calls(&self) -> Vec<&MacroCall> {
        self.calls
            .iter()
            .chain(self.attribute_macros.iter())
            .collect()
    }
}

pub enum MacroType {
    StructAttribute(ASTStructDef),
    Standard,
}

pub struct MacroCall {
    pub id: usize,
    pub module_namespace: ModuleNamespace,
    pub module_id: ModuleId,
    pub position: ASTPosition,
    pub transformed_macro: ASTFunctionCall,
}

impl MacroCall {
    pub fn id(&self) -> usize {
        self.id
    }

    pub fn module_namespace(&self) -> &ModuleNamespace {
        &self.module_namespace
    }

    pub fn module_id(&self) -> &ModuleId {
        &self.module_id
    }

    pub fn position(&self) -> &ASTPosition {
        &self.position
    }

    pub fn transformed_macro(&self) -> &ASTFunctionCall {
        &self.transformed_macro
    }
}

pub fn extract_macro_calls(
    container: ASTModulesContainer,
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
) -> MacroCallExtractor {
    info!("extract macro calls");
    let mut new_container = ASTModulesContainer::new();
    let mut calls = Vec::new();
    let mut attribute_macros = Vec::new();

    let new_modules = container
        .modules()
        .into_par_iter()
        .map(|(id, namespace, module)| {
            let mut calls = Vec::new();
            let mut new_module = module.clone();
            new_module.functions.clear();
            for function in module.functions.iter() {
                let mut new_function = function.clone();
                if let ASTFunctionBody::RASMBody(ref body) = function.body {
                    let new_statements = transform_macro_calls_in_body(
                        &container, catalog, &namespace, &id, body, &mut calls,
                    );
                    new_function.body = ASTFunctionBody::RASMBody(new_statements);
                }

                new_module.add_function(new_function);
            }

            new_module.body = transform_macro_calls_in_body(
                &container,
                catalog,
                &namespace,
                &id,
                &module.body,
                &mut calls,
            );
            (id, namespace, new_module, calls)
        })
        .collect::<Vec<_>>();

    for (id, namespace, new_module, module_calls) in new_modules.into_iter() {
        calls.extend(module_calls);
        new_container.add(
            new_module,
            namespace.clone(),
            id.clone(),
            false,
            container.is_readonly_module(id),
        );
    }

    // TODO parallelize?
    for (info, s) in container.struct_defs().iter() {
        for attribute_macro in s.attribute_macros.iter() {
            let macro_call = get_macro_call(
                MacroType::StructAttribute(s.clone()),
                attribute_macro,
                &container,
                catalog,
                info.namespace(),
                info.id(),
            );
            attribute_macros.push(macro_call);
        }
    }

    info!("end extracting macro calls");
    MacroCallExtractor {
        container: new_container,
        calls,
        attribute_macros,
    }
}

use std::sync::atomic::{AtomicUsize, Ordering};

static MACRO_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn get_next_macro_id() -> usize {
    MACRO_ID_COUNTER.fetch_add(1, Ordering::SeqCst)
}

fn transform_macro_calls_in_body(
    container: &ASTModulesContainer,
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    body: &Vec<ASTStatement>,
    calls: &mut Vec<MacroCall>,
) -> Vec<ASTStatement> {
    let mut new_statements = Vec::with_capacity(body.len());
    for statement in body.iter() {
        if let ASTStatement::ASTExpressionStatement(expr) = statement {
            if let ASTExpression::ASTFunctionCallExpression(call) = expr {
                if call.is_macro() {
                    let macro_call = get_macro_call(
                        MacroType::Standard,
                        call,
                        container,
                        catalog,
                        module_namespace,
                        module_id,
                    );
                    let placeholder_statement = ASTStatement::ASTLetStatement(
                        format!("macroCall{}", macro_call.id),
                        ASTExpression::ASTValueExpression(
                            ASTValue::ASTIntegerValue(macro_call.id.try_into().unwrap()),
                            call.position().mv_right(1),
                        ),
                        call.position().clone(),
                    );

                    calls.push(macro_call);

                    new_statements.push(placeholder_statement);
                    continue;
                }
            }
        }
        new_statements.push(statement.clone());
    }
    new_statements
}

fn get_macro_call(
    macro_type: MacroType,
    call: &ASTFunctionCall,
    container: &ASTModulesContainer,
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
) -> MacroCall {
    let functions_vec = container
        .signatures()
        .into_iter()
        .filter(|it| &it.signature.name == call.function_name())
        .filter(|it| {
            (it.signature.modifiers.public || it.module_info().namespace() == module_namespace)
                && is_macro_result(&it.signature.return_type)
        })
        .collect::<Vec<_>>();
    if functions_vec.is_empty() {
        error(
            catalog,
            module_id,
            call.position(),
            format!("Macro {} not found", call.function_name()),
        );
    } else if functions_vec.len() > 1 {
        error(
            catalog,
            module_id,
            call.position(),
            format!("Macro {} is ambiguous", call.function_name()),
        );
    }

    let function = functions_vec[0];

    let fixed_parameters = match macro_type {
        MacroType::StructAttribute(s) => {
            let mut parameters = vec![ASTExpression::ASTValueExpression(
                ASTValue::ASTStringValue(s.name.clone()),
                call.position().mv_right(1),
            )];

            parameters.push(ASTExpression::ASTFunctionCallExpression(
                ASTFunctionCall::new(
                    "Vec".to_owned(),
                    Vec::new(),
                    call.position().mv_right(1),
                    Vec::new(),
                    None,
                    false,
                ),
            ));

            let mut properties = Vec::new();

            for p in s.properties.iter() {
                properties.push(ASTExpression::ASTFunctionCallExpression(
                    ASTFunctionCall::new(
                        "ASTStructPropertyDef".to_string(),
                        vec![
                            ASTExpression::ASTValueExpression(
                                ASTValue::ASTStringValue(p.name.clone()),
                                call.position().mv_right(1),
                            ),
                            ASTExpression::ASTFunctionCallExpression(ASTFunctionCall::new(
                                "ASTUnitType".to_owned(),
                                Vec::new(),
                                call.position().mv_right(1),
                                Vec::new(),
                                None,
                                false,
                            )),
                        ],
                        call.position().mv_right(1),
                        Vec::new(),
                        None,
                        false,
                    ),
                ));
            }

            parameters.push(ASTExpression::ASTFunctionCallExpression(
                ASTFunctionCall::new(
                    "vecOf".to_owned(),
                    properties,
                    call.position().mv_right(1),
                    Vec::new(),
                    None,
                    false,
                ),
            ));

            parameters.push(ASTExpression::ASTFunctionCallExpression(
                ASTFunctionCall::new(
                    "ASTModifiers".to_owned(),
                    vec![ASTExpression::ASTValueExpression(
                        ASTValue::ASTBooleanValue(s.modifiers.public),
                        call.position().mv_right(1),
                    )],
                    call.position().mv_right(1),
                    Vec::new(),
                    None,
                    false,
                ),
            ));

            vec![ASTExpression::ASTFunctionCallExpression(
                ASTFunctionCall::new(
                    "ASTStructDef".to_string(),
                    parameters,
                    call.position().mv_right(2),
                    Vec::new(),
                    None,
                    false,
                ),
            )]
        }
        MacroType::Standard => Vec::new(),
    };

    if function.signature.parameters_types.len()
        != (call.parameters().len() + fixed_parameters.len())
    {
        error(
            catalog,
            module_id,
            call.position(),
            format!(
                "Macro {} expects {} arguments, but {} were given",
                call.function_name(),
                function.signature.parameters_types.len() - fixed_parameters.len(),
                call.parameters().len()
            ),
        );
    }

    let custom_parameters = zip(
        function.signature.parameters_types.iter(),
        call.parameters().iter(),
    )
    .map(|(parameter_type, parameter)| {
        let is_expression = if let ASTType::ASTCustomType {
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
            convert_to_rasm_expression(container, module_namespace, module_id, parameter)
        } else if let ASTExpression::ASTValueExpression(_, _) = parameter {
            // TODO lambda is allowed)?
            if let ASTType::ASTBuiltinType(_) = parameter_type {
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
                ASTIndex::new(
                    module_namespace.clone(),
                    module_id.clone(),
                    call.position().clone()
                )
            );
        }
    })
    .collect::<Vec<_>>();

    let transformed_macro = ASTFunctionCall::new(
        call.function_name().clone(),
        fixed_parameters
            .into_iter()
            .chain(custom_parameters.into_iter())
            .collect(),
        call.position().clone(),
        call.generics().clone(),
        call.target().clone(),
        false,
    );

    let macro_id = get_next_macro_id();

    MacroCall {
        id: macro_id,
        module_namespace: module_namespace.clone(),
        module_id: module_id.clone(),
        position: call.position().clone(),
        transformed_macro,
    }
}

fn error(
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    module_id: &ModuleId,
    position: &ASTPosition,
    message: String,
) {
    let enh_module_id = catalog
        .catalog_info(&module_id)
        .map(|it| it.0.clone())
        .unwrap_or(EnhModuleId::none());
    panic!("{} : {}:{}", message, enh_module_id, position);
}

fn is_macro_result(ast_type: &ASTType) -> bool {
    match ast_type {
        ASTType::ASTCustomType {
            name,
            param_types: _,
            position: _,
        } => name == "MacroResult",
        _ => false,
    }
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
            fcp.push(ASTExpression::ASTValueExpression(
                ASTValue::ASTStringValue(function_call.function_name().clone()),
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
        ASTExpression::ASTValueRefExpression(name, position) => simple_call(
            "ASTValueRefExpression",
            vec![ASTExpression::ASTValueExpression(
                ASTValue::ASTStringValue(name.clone()),
                position.mv_right(1), // it must be different for cache purposes
            )],
            position.clone(),
            Some("ASTExpression".to_owned()),
        ),
        ASTExpression::ASTValueExpression(value_type, position) => {
            let value_function = match value_type {
                ASTValue::ASTStringValue(_) => "ASTStringValue",
                ASTValue::ASTBooleanValue(_) => "ASTBooleanValue",
                ASTValue::ASTIntegerValue(_) => "ASTIntegerValue",
                ASTValue::ASTCharValue(_) => "ASTCharValue",
                ASTValue::ASTFloatValue(_) => "ASTFloatValue",
            };
            simple_call(
                "ASTValueExpression",
                vec![simple_call(
                    value_function,
                    vec![ASTExpression::ASTValueExpression(
                        value_type.clone(),
                        position.mv_right(2), // it must be different for cache purposes
                    )],
                    position.mv_right(1), // it must be different for cache purposes
                    Some("ASTValue".to_owned()),
                )],
                position.clone(),
                Some("ASTExpression".to_owned()),
            )
        }
        ASTExpression::ASTLambdaExpression(lambda_def) => todo!(),
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
        target,
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
