use log::info;
use rasm_parser::{
    catalog::{modules_catalog::ModulesCatalog, ASTIndex, ModuleId, ModuleNamespace},
    parser::ast::{
        ASTBuiltinTypeKind, ASTEnumDef, ASTExpression, ASTFunctionBody, ASTFunctionCall,
        ASTFunctionSignature, ASTModifiers, ASTPosition, ASTStatement, ASTStructDef, ASTType,
        ASTValue,
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
    EnumAttribute(ASTEnumDef),
    Expression,
    Statement,
}

pub struct MacroCall {
    pub id: usize,
    pub module_namespace: ModuleNamespace,
    pub module_id: ModuleId,
    pub position: ASTPosition,
    pub transformed_macro: ASTFunctionCall,
    pub macro_result_type: MacroResultType,
    pub function_signature: ASTFunctionSignature,
    pub in_function: Option<ASTFunctionSignature>,
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
                        &container,
                        catalog,
                        &namespace,
                        &id,
                        body,
                        &mut calls,
                        Some(&function.signature()),
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
                None,
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
                None,
            );
            attribute_macros.push(macro_call);
        }
    }

    for (info, e) in container.enum_defs().iter() {
        for attribute_macro in e.attribute_macros.iter() {
            let macro_call = get_macro_call(
                MacroType::EnumAttribute(e.clone()),
                attribute_macro,
                &container,
                catalog,
                info.namespace(),
                info.id(),
                None,
            );
            attribute_macros.push(macro_call);
        }
    }

    info!(
        "end extracting macro calls, found {} calls, {} attribute macros",
        calls.len(),
        attribute_macros.len()
    );
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
    in_function: Option<&ASTFunctionSignature>,
) {
    for statement in body.iter() {
        match statement {
            ASTStatement::ASTExpressionStatement(expression, _) => {
                extract_macro_calls_in_expression(
                    container,
                    catalog,
                    module_namespace,
                    module_id,
                    expression,
                    calls,
                    in_function,
                    MacroType::Statement,
                )
            }
            ASTStatement::ASTLetStatement(_, expression, _) => extract_macro_calls_in_expression(
                container,
                catalog,
                module_namespace,
                module_id,
                expression,
                calls,
                in_function,
                MacroType::Expression,
            ),
            ASTStatement::ASTConstStatement(_, expression, _, _) => {
                extract_macro_calls_in_expression(
                    container,
                    catalog,
                    module_namespace,
                    module_id,
                    expression,
                    calls,
                    in_function,
                    MacroType::Expression,
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
    in_function: Option<&ASTFunctionSignature>,
    macro_type: MacroType,
) {
    match expression {
        ASTExpression::ASTFunctionCallExpression(function_call) => {
            if function_call.is_macro() {
                calls.push(get_macro_call(
                    macro_type,
                    function_call,
                    container,
                    catalog,
                    module_namespace,
                    module_id,
                    in_function,
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
                        in_function,
                        MacroType::Expression,
                    )
                });
            }
        }
        ASTExpression::ASTLambdaExpression(lambda_def) => extract_macro_calls_in_body(
            container,
            catalog,
            module_namespace,
            module_id,
            &lambda_def.body,
            calls,
            in_function,
        ),
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
    in_function: Option<&ASTFunctionSignature>,
) -> MacroCall {
    let functions_vec = container
        .signatures()
        .into_iter()
        .filter(|it| &it.signature.name == call.function_name())
        .map(|it| (it, get_macro_result_type(&it.signature.return_type)))
        .filter(|(entry, macro_result_type)| {
            (entry.signature.modifiers.public
                || entry.module_info().namespace() == module_namespace)
                && macro_result_type.is_some()
        })
        .filter(|(entry, macro_result_type)| match &macro_type {
            MacroType::Expression => macro_result_type.unwrap() == MacroResultType::Expression,
            MacroType::Statement => macro_result_type.unwrap() == MacroResultType::Statement,
            MacroType::StructAttribute(_) => {
                if macro_result_type.unwrap() != MacroResultType::Attribute {
                    return false;
                }
                if entry.signature.parameters_types.len() == 0 {
                    return false;
                }
                if let ASTType::ASTCustomType {
                    name,
                    param_types: _,
                    position: _,
                } = &entry.signature.parameters_types[0]
                {
                    name == "ASTStructDef"
                } else {
                    false
                }
            }
            MacroType::EnumAttribute(_) => {
                if macro_result_type.unwrap() != MacroResultType::Attribute {
                    return false;
                }
                if entry.signature.parameters_types.len() == 0 {
                    return false;
                }
                if let ASTType::ASTCustomType {
                    name,
                    param_types: _,
                    position: _,
                } = &entry.signature.parameters_types[0]
                {
                    name == "ASTEnumDef"
                } else {
                    false
                }
            }
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
                call.position().copy(),
            )];

            parameters.push(call_empty_vec(call.position().copy(), ast_str_type()));

            let mut properties = Vec::new();

            for p in s.properties.iter() {
                properties.push(simple_call(
                    "ASTStructPropertyDef",
                    vec![
                        ASTExpression::ASTValueExpression(
                            ASTValue::ASTStringValue(p.name.clone()),
                            call.position().copy(),
                        ),
                        to_ast_type(&p.ast_type, p.position.copy()),
                    ],
                    p.position.copy(),
                    None,
                ));
            }

            parameters.push(vec_or_vec_of(
                properties,
                call.position().copy(),
                ast_custom_type("ASTStructPropertyDef"),
            ));

            parameters.push(ast_modifiers(&s.modifiers, call.position()));

            vec![(
                ASTType::ASTCustomType {
                    name: "ASTStructDef".to_string(),
                    param_types: Vec::new(),
                    position: call.position().copy(),
                },
                simple_call("ASTStructDef", parameters, call.position().copy(), None),
            )]
        }
        MacroType::EnumAttribute(e) => {
            let mut parameters = vec![ASTExpression::ASTValueExpression(
                ASTValue::ASTStringValue(e.name.clone()),
                call.position().copy(),
            )];

            parameters.push(call_empty_vec(call.position().copy(), ast_str_type()));

            let mut variants = Vec::new();

            for v in e.variants.iter() {
                let mut properties = Vec::new();
                properties.push(ASTExpression::ASTValueExpression(
                    ASTValue::ASTStringValue(v.name.clone()),
                    call.position().copy(),
                ));

                let mut variant_properties = Vec::new();

                for p in v.parameters.iter() {
                    variant_properties.push(simple_call(
                        "ASTParameterDef",
                        vec![
                            ASTExpression::ASTValueExpression(
                                ASTValue::ASTStringValue(p.name.clone()),
                                call.position().copy(),
                            ),
                            to_ast_type(&p.ast_type, p.position.copy()),
                        ],
                        call.position().copy(),
                        None,
                    ));
                }

                properties.push(vec_or_vec_of(
                    variant_properties,
                    call.position().copy(),
                    ast_custom_type("ASTParameterDef"),
                ));

                variants.push(simple_call(
                    "ASTEnumVariantDef",
                    properties,
                    call.position().copy(),
                    None,
                ));
            }

            parameters.push(vec_or_vec_of(
                variants,
                call.position().copy(),
                ast_custom_type("ASTEnumVariantDef"),
            ));

            parameters.push(ast_modifiers(&e.modifiers, call.position()));

            vec![(
                ASTType::ASTCustomType {
                    name: "ASTEnumDef".to_string(),
                    param_types: Vec::new(),
                    position: ASTPosition::none(),
                },
                simple_call("ASTEnumDef", parameters, call.position().copy(), None),
            )]
        }
        MacroType::Expression | MacroType::Statement => {
            if is_ast_module_first_parameter(&function.signature) {
                vec![(
                    ASTType::ASTCustomType {
                        name: "ASTModule".to_owned(),
                        param_types: Vec::new(),
                        position: call.position().copy(),
                    },
                    ASTExpression::ASTValueRefExpression(
                        "moduleAST".to_owned(),
                        call.position().copy(),
                    ),
                )]
            } else {
                Vec::new()
            }
        }
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

                custom_parameters.push(vec_or_vec_of(
                    vec_of_expressions,
                    ASTPosition::none(),
                    ast_expression_type(),
                ));
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
                            call_par.position().copy()
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

    let transformed_macro = ASTFunctionCall::new(
        call.function_name().clone(),
        custom_parameters,
        call.position().copy(),
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
        function_signature: function.signature.clone(),
        in_function: in_function.cloned(),
    }
}

pub fn is_ast_module_first_parameter(function: &ASTFunctionSignature) -> bool {
    if let Some(param) = function.parameters_types.first() {
        if let ASTType::ASTCustomType { name, .. } = param {
            name == "ASTModule"
        } else {
            false
        }
    } else {
        false
    }
}

fn ast_modifiers(m: &ASTModifiers, position: &ASTPosition) -> ASTExpression {
    simple_call(
        "ASTModifiers",
        vec![ASTExpression::ASTValueExpression(
            ASTValue::ASTBooleanValue(m.public),
            position.copy(),
        )],
        position.copy(),
        None,
    )
}

fn ast_expression_type() -> ASTType {
    ast_custom_type("ASTExpression")
}

fn ast_custom_type(name: &str) -> ASTType {
    ASTType::ASTCustomType {
        name: name.to_owned(),
        param_types: Vec::new(),
        position: ASTPosition::none(),
    }
}

fn ast_str_type() -> ASTType {
    ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTStringType)
}

fn to_ast_type(ast_type: &ASTType, position: ASTPosition) -> ASTExpression {
    match ast_type {
        ASTType::ASTBuiltinType(astbuiltin_type_kind) => {
            let kind = match astbuiltin_type_kind {
                ASTBuiltinTypeKind::ASTBooleanType => {
                    simple_call("ASTBooleanType", Vec::new(), position.copy(), None)
                }
                ASTBuiltinTypeKind::ASTCharType => {
                    simple_call("ASTCharType", Vec::new(), position.copy(), None)
                }
                ASTBuiltinTypeKind::ASTIntegerType => {
                    simple_call("ASTIntegerType", Vec::new(), position.copy(), None)
                }
                ASTBuiltinTypeKind::ASTFloatType => {
                    simple_call("ASTFloatType", Vec::new(), position.copy(), None)
                }
                ASTBuiltinTypeKind::ASTStringType => {
                    simple_call("ASTStringType", Vec::new(), position.copy(), None)
                }
                ASTBuiltinTypeKind::ASTLambdaType {
                    parameters,
                    return_type,
                } => simple_call(
                    "ASTLambdaType",
                    vec![
                        vec_or_vec_of(
                            parameters
                                .iter()
                                .map(|it| to_ast_type(it, position.copy()))
                                .collect(),
                            position.copy(),
                            ast_custom_type("ASTType"),
                        ),
                        to_ast_type(return_type, position.copy()),
                    ],
                    position.copy(),
                    None,
                ),
            };
            simple_call("ASTBuiltinType", vec![kind], position.copy(), None)
        }
        ASTType::ASTGenericType(astposition, name, asttypes) => simple_call(
            "ASTGenericType",
            vec![
                string_value(name, astposition.copy()),
                vec_or_vec_of(
                    asttypes
                        .iter()
                        .map(|it| to_ast_type(it, astposition.copy()))
                        .collect(),
                    position.copy(),
                    ast_custom_type("ASTType"),
                ),
            ],
            position.copy(),
            None,
        ),
        ASTType::ASTCustomType {
            name,
            param_types,
            position,
        } => simple_call(
            "ASTCustomType",
            vec![
                string_value(name, position.copy()),
                vec_or_vec_of(
                    param_types
                        .iter()
                        .map(|it| to_ast_type(it, position.copy()))
                        .collect(),
                    position.copy(),
                    ast_custom_type("ASTType"),
                ),
            ],
            position.copy(),
            None,
        ),
        ASTType::ASTUnitType => simple_call("ASTUnitType", Vec::new(), position.copy(), None),
    }
}

fn vec_or_vec_of(
    parameters: Vec<ASTExpression>,
    position: ASTPosition,
    ast_type: ASTType,
) -> ASTExpression {
    if parameters.is_empty() {
        call_empty_vec(position.copy(), ast_type)
    } else {
        let start = call_empty_vec(position.copy(), ast_type);

        parameters.iter().fold(start, |prev, actual| {
            simple_call("push", vec![prev, actual.clone()], position.copy(), None)
        })
    }
}

fn string_value(s: &str, position: ASTPosition) -> ASTExpression {
    ASTExpression::ASTValueExpression(ASTValue::ASTStringValue(s.to_string()), position)
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum MacroResultType {
    Statement,
    Expression,
    Attribute,
}

fn get_macro_result_type(ast_type: &ASTType) -> Option<MacroResultType> {
    match ast_type {
        ASTType::ASTCustomType {
            name,
            param_types: _,
            position: _,
        } => {
            if name == "MacroStatementResult" {
                Some(MacroResultType::Statement)
            } else if name == "MacroExpressionResult" {
                Some(MacroResultType::Expression)
            } else if name == "MacroAttributeResult" {
                Some(MacroResultType::Attribute)
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
    expr: &ASTExpression,
) -> ASTExpression {
    match expr {
        ASTExpression::ASTFunctionCallExpression(function_call) => {
            let mut fcp = Vec::new();
            fcp.push(ASTExpression::ASTValueExpression(
                ASTValue::ASTStringValue(function_call.function_name().clone()),
                function_call.position().copy(),
            ));
            fcp.push(call_vec_of(
                container,
                module_namespace,
                module_id,
                function_call.parameters(),
                function_call.position().copy(),
            ));
            fcp.push(call_empty_vec(
                function_call.position().copy(),
                ASTType::ASTCustomType {
                    name: "ASTType".to_owned(),
                    param_types: Vec::new(),
                    position: function_call.position().copy(),
                },
            ));
            fcp.push(call_none(function_call.position().copy()));
            fcp.push(ASTExpression::ASTValueExpression(
                ASTValue::ASTBooleanValue(function_call.is_macro()),
                function_call.position().copy(),
            ));
            simple_call(
                "ASTFunctionCallExpression",
                vec![simple_call(
                    "ASTFunctionCall",
                    fcp,
                    function_call.position().copy(),
                    None,
                )],
                function_call.position().copy(),
                Some("ASTExpression".to_owned()),
            )
        }
        ASTExpression::ASTValueRefExpression(name, position) => simple_call(
            "ASTValueRefExpression",
            vec![ASTExpression::ASTValueExpression(
                ASTValue::ASTStringValue(name.clone()),
                position.copy(), // it must be different for cache purposes
            )],
            position.copy(),
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
                        position.copy(),
                    )],
                    position.copy(),
                    Some("ASTValue".to_owned()),
                )],
                position.copy(),
                Some("ASTExpression".to_owned()),
            )
        }
        ASTExpression::ASTLambdaExpression(def) => {
            let parameters_names = def
                .parameter_names
                .iter()
                .map(|it| string_value(&it.0, it.1.copy()))
                .collect();
            let body = def
                .body
                .iter()
                .map(|it| convert_to_rasm_statement(container, module_namespace, module_id, it))
                .collect::<Vec<_>>();

            let lambda_def = simple_call(
                "ASTLambdaDef",
                vec![
                    vec_or_vec_of(parameters_names, def.position.copy(), ast_str_type()),
                    vec_or_vec_of(body, def.position.copy(), ast_custom_type("ASTStatement")),
                ],
                def.position.copy(),
                None,
            );
            simple_call(
                "ASTLambdaExpression",
                vec![lambda_def],
                def.position.copy(),
                None,
            )
        }
    }
}

fn convert_to_rasm_statement(
    container: &ASTModulesContainer,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    stmt: &ASTStatement,
) -> ASTExpression {
    match stmt {
        ASTStatement::ASTExpressionStatement(astexpression, astposition) => simple_call(
            "ASTExpressionStatement",
            vec![convert_to_rasm_expression(
                container,
                module_namespace,
                module_id,
                astexpression,
            )],
            astposition.copy(),
            None,
        ),
        ASTStatement::ASTLetStatement(name, astexpression, astposition) => simple_call(
            "ASTLetStatement",
            vec![
                string_value(name, astposition.copy()),
                convert_to_rasm_expression(container, module_namespace, module_id, astexpression),
            ],
            astposition.copy(),
            None,
        ),
        ASTStatement::ASTConstStatement(name, astexpression, astposition, astmodifiers) => {
            simple_call(
                "ASTConstStatement",
                vec![
                    string_value(name, astposition.copy()),
                    convert_to_rasm_expression(
                        container,
                        module_namespace,
                        module_id,
                        astexpression,
                    ),
                    ast_modifiers(astmodifiers, &astposition),
                ],
                astposition.copy(),
                None,
            )
        }
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
    vec_or_vec_of(
        parameters
            .iter()
            .map(|it| convert_to_rasm_expression(container, module_namespace, module_id, it))
            .collect(),
        position,
        ast_expression_type(),
    )
}

fn call_empty_vec(position: ASTPosition, ast_type: ASTType) -> ASTExpression {
    ASTExpression::ASTFunctionCallExpression(ASTFunctionCall::new(
        "Vec".to_owned(),
        Vec::new(),
        position,
        vec![ast_type],
        None,
        false,
    ))
}

fn call_none(position: ASTPosition) -> ASTExpression {
    simple_call("None", Vec::new(), position, None)
}
