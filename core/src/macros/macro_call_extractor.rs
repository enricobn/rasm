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
    pub calls: Vec<MacroCall>,
    pub attribute_macros: Vec<MacroCall>,
}

impl MacroCallExtractor {
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
    pub macro_result_type: MacroResultType,
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
    container: &ASTModulesContainer,
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
) -> MacroCallExtractor {
    info!("extract macro calls");
    //let mut new_container = ASTModulesContainer::new();
    let mut calls = Vec::new();
    let mut attribute_macros = Vec::new();

    let new_modules = container
        .modules()
        .into_par_iter()
        .map(|(id, namespace, module)| {
            let mut calls = Vec::new();

            for function in module.functions.iter() {
                if let ASTFunctionBody::RASMBody(ref body) = function.body {
                    extract_macro_calls_in_body(
                        &container, catalog, &namespace, &id, body, &mut calls,
                    );
                }
            }

            extract_macro_calls_in_body(
                &container,
                catalog,
                &namespace,
                &id,
                &module.body,
                &mut calls,
            );
            (id, namespace, calls)
        })
        .collect::<Vec<_>>();

    for (_, _, module_calls) in new_modules.into_iter() {
        calls.extend(module_calls);
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
        calls,
        attribute_macros,
    }
}

use std::sync::atomic::{AtomicUsize, Ordering};

static MACRO_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn get_next_macro_id() -> usize {
    MACRO_ID_COUNTER.fetch_add(1, Ordering::SeqCst)
}

fn extract_macro_calls_in_body(
    container: &ASTModulesContainer,
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    body: &Vec<ASTStatement>,
    calls: &mut Vec<MacroCall>,
) {
    for statement in body.iter() {
        match statement {
            ASTStatement::ASTExpressionStatement(expression) => extract_macro_calls_in_expression(
                container,
                catalog,
                module_namespace,
                module_id,
                expression,
                calls,
            ),
            ASTStatement::ASTLetStatement(_, expression, _) => extract_macro_calls_in_expression(
                container,
                catalog,
                module_namespace,
                module_id,
                expression,
                calls,
            ),
            ASTStatement::ASTConstStatement(_, expression, _, _) => {
                extract_macro_calls_in_expression(
                    container,
                    catalog,
                    module_namespace,
                    module_id,
                    expression,
                    calls,
                )
            }
        }
    }
}

fn extract_macro_calls_in_expression(
    container: &ASTModulesContainer,
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    expression: &ASTExpression,
    calls: &mut Vec<MacroCall>,
) {
    match expression {
        ASTExpression::ASTFunctionCallExpression(function_call) => {
            if function_call.is_macro() {
                calls.push(get_macro_call(
                    MacroType::Standard,
                    function_call,
                    container,
                    catalog,
                    module_namespace,
                    module_id,
                ));
            } else {
                function_call.parameters().iter().for_each(|it| {
                    extract_macro_calls_in_expression(
                        container,
                        catalog,
                        module_namespace,
                        module_id,
                        it,
                        calls,
                    )
                });
            }
        }
        //TODO ASTExpression::ASTLambdaExpression(astlambda_def) => todo!(),
        _ => (),
    }
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
        .map(|it| (it, get_macro_result_type(&it.signature.return_type)))
        .filter(|(entry, result_type)| {
            (entry.signature.modifiers.public
                || entry.module_info().namespace() == module_namespace)
                && result_type.is_some()
        })
        .collect::<Vec<_>>();
    if functions_vec.is_empty() {
        panic!(
            "{}",
            error(
                catalog,
                module_id,
                call.position(),
                format!("Macro {} not found", call.function_name()),
            )
        );
    } else if functions_vec.len() > 1 {
        panic!(
            "{}",
            error(
                catalog,
                module_id,
                call.position(),
                format!("Macro {} is ambiguous", call.function_name()),
            )
        );
    }

    let (function, macro_result_type) = functions_vec[0];

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

            vec![(
                ASTType::ASTCustomType {
                    name: "ASTStructDef".to_string(),
                    param_types: Vec::new(),
                    position: ASTPosition::none(),
                },
                ASTExpression::ASTFunctionCallExpression(ASTFunctionCall::new(
                    "ASTStructDef".to_string(),
                    parameters,
                    call.position().mv_right(2),
                    Vec::new(),
                    None,
                    false,
                )),
            )]
        }
        MacroType::Standard => Vec::new(),
    };

    let mut custom_parameters = Vec::new();
    let mut function_par_types = function.signature.parameters_types.iter();
    let mut call_pars = call.parameters().iter();
    let mut errors = Vec::new();

    for (ast_type, expr) in fixed_parameters {
        let next_function_par_type = function_par_types.next();
        if let Some(function_par_type) = next_function_par_type {
            if &ast_type != function_par_type {
                errors.push(error(
                    catalog,
                    module_id,
                    call.position(),
                    format!("Expected {ast_type} got {function_par_type}"),
                ));
            } else {
                custom_parameters.push(expr);
            }
        } else {
            errors.push(error(
                catalog,
                module_id,
                call.position(),
                format!("Expected {ast_type}"),
            ));
        }
    }

    while errors.is_empty() {
        let next_function_par_type = function_par_types.next();
        let mut next_call_par = call_pars.next();

        if let Some(function_par_type) = next_function_par_type {
            if is_vec_of_expressions(function_par_type) {
                let mut vec_of_expressions = Vec::new();
                while let Some(call_par) = next_call_par {
                    vec_of_expressions.push(convert_to_rasm_expression(
                        container,
                        module_namespace,
                        module_id,
                        call_par,
                    ));
                    next_call_par = call_pars.next();
                }
                if vec_of_expressions.is_empty() {
                    custom_parameters.push(simple_call(
                        "Vec",
                        Vec::new(),
                        ASTPosition::none(),
                        None,
                    ));
                } else {
                    custom_parameters.push(simple_call(
                        "vecOf",
                        vec_of_expressions,
                        ASTPosition::none(),
                        None,
                    ));
                }
            } else if let Some(call_par) = next_call_par {
                if is_expression(function_par_type) {
                    // convert the parameter to a rasm expression
                    custom_parameters.push(convert_to_rasm_expression(
                        container,
                        module_namespace,
                        module_id,
                        call_par,
                    ));
                } else if let ASTExpression::ASTValueExpression(_, _) = call_par {
                    // TODO lambda is allowed)?
                    if let ASTType::ASTBuiltinType(_) = function_par_type {
                        custom_parameters.push(call_par.clone())
                    } else {
                        errors.push(format!(
                            "Type {} is not allowed as a macro parameter: {}",
                            function_par_type,
                            function.index()
                        ));
                    }
                } else {
                    errors.push(format!(
                        "Only ASTExpression or constant is allowed as a macro parameter : {}",
                        ASTIndex::new(
                            module_namespace.clone(),
                            module_id.clone(),
                            call_par.position().clone()
                        )
                    ));
                }
            } else {
                errors.push(error(
                    catalog,
                    module_id,
                    call.position(),
                    format!("Unmatched parameters"),
                ));
            }
        } else if next_call_par.is_some() {
            errors.push(error(
                catalog,
                module_id,
                call.position(),
                format!("Unmatched parameters"),
            ));
        } else {
            break;
        }
    }

    if !errors.is_empty() {
        panic!("{}", errors[0]);
    }

    /*
        let custom_parameters = zip(
            function.signature.parameters_types.iter(),
            call.parameters().iter(),
        )
        .map(|(parameter_type, parameter)| {
            if is_expression(parameter_type) {
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
    */

    let transformed_macro = ASTFunctionCall::new(
        call.function_name().clone(),
        custom_parameters,
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
        macro_result_type: macro_result_type.unwrap(),
    }
}

fn is_vec_of_expressions(ast_type: &ASTType) -> bool {
    if let ASTType::ASTCustomType {
        name,
        param_types,
        position: _,
    } = ast_type
    {
        if name == "Vec" && param_types.len() == 1 {
            if let Some(param_type) = param_types.last() {
                return is_expression(param_type);
            }
        }
    }
    false
}

fn is_expression(ast_type: &ASTType) -> bool {
    if let ASTType::ASTCustomType {
        name,
        param_types: _,
        position: _,
    } = ast_type
    {
        if name == "ASTExpression" {
            return true;
        }
    }
    false
}

fn error(
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    module_id: &ModuleId,
    position: &ASTPosition,
    message: String,
) -> String {
    let enh_module_id = catalog
        .catalog_info(&module_id)
        .map(|it| it.0.clone())
        .unwrap_or(EnhModuleId::none());
    format!("{} : {}:{}", message, enh_module_id, position)
}

#[derive(Clone, Copy)]
pub enum MacroResultType {
    Module,
    Expression,
}

fn get_macro_result_type(ast_type: &ASTType) -> Option<MacroResultType> {
    match ast_type {
        ASTType::ASTCustomType {
            name,
            param_types: _,
            position: _,
        } => {
            if name == "MacroModuleResult" {
                Some(MacroResultType::Module)
            } else if name == "MacroExpressionResult" {
                Some(MacroResultType::Expression)
            } else {
                None
            }
        }
        _ => None,
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
