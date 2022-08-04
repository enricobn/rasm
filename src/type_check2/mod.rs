use crate::codegen::{EnhancedASTModule, VarContext, VarKind};
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
    ASTLambdaDef, ASTParameterDef, ASTStructDef, ASTType, ASTTypeRef, BuiltinTypeKind,
};
use crate::type_check2::resolved_ast::{
    convert_to_typed_module, ASTTypedEnumDef, ASTTypedEnumVariantDef, ASTTypedExpression,
    ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedLambdaDef,
    ASTTypedModule, ASTTypedParameterDef, ASTTypedStructDef, ASTTypedType, ASTTypedTypeRef,
    BuiltinTypedTypeKind,
};
use log::{debug, info};
use std::collections::HashMap;
use linked_hash_map::LinkedHashMap;

pub mod resolved_ast;

pub fn convert(module: &EnhancedASTModule) -> ASTTypedModule {
    let mut body = Vec::new();

    let context = VarContext::new(None);

    let mut new_function_defs = LinkedHashMap::new();
    let mut used_untyped_function_defs = LinkedHashMap::new();

    for call in module.body.iter() {
        body.push(convert_call(
            module,
            &context,
            call,
            &mut new_function_defs,
            &mut used_untyped_function_defs,
        ));
    }

    let mut all_function_defs = new_function_defs.clone();

    for (name, function_def) in used_untyped_function_defs.iter() {
        all_function_defs.insert(name.clone(), function_def.clone());
    }

    for function_def in all_function_defs.values() {
        debug!("converting function {}", function_def.name);

        let mut context = VarContext::new(None);

        for (i, par) in function_def.parameters.iter().enumerate() {
            debug!("inserting par {} in context", par.name);
            context.insert(par.name.clone(), VarKind::ParameterRef(i, par.clone()));
        }

        match &function_def.body {
            ASTFunctionBody::RASMBody(body) => {
                let mut new_function_def = function_def.clone();
                new_function_def.body = ASTFunctionBody::RASMBody(
                    body.iter()
                        .map(|it| {
                            convert_expr(
                                module,
                                it,
                                &context,
                                &mut new_function_defs,
                                &mut used_untyped_function_defs,
                            )
                        })
                        .collect(),
                );

                used_untyped_function_defs.insert(function_def.name.clone(), new_function_def);
            }
            ASTFunctionBody::ASMBody(body) => {}
        }

        if let Some(return_type) = &function_def.return_type {
            //let mut new_function_def = function_def.clone();

            if !get_parametric_types(&return_type.ast_type).is_empty() {
                panic!()
            }
        }
    }

    convert_to_typed_module(module, body, new_function_defs, used_untyped_function_defs)
}

fn get_parametric_types(ast_type: &ASTType) -> Vec<String> {
    return match ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::ASTString => {
                vec![]
            }
            BuiltinTypeKind::ASTI32 => {
                vec![]
            }
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => {
                let mut par_types: Vec<String> = parameters
                    .iter()
                    .flat_map(|it| get_parametric_types(&it.ast_type))
                    .collect();
                if let Some(rt) = return_type {
                    par_types.append(&mut get_parametric_types(&rt.as_ref().ast_type));
                }
                par_types.sort();
                par_types.dedup();
                par_types
            }
        },
        ASTType::Parametric(p) => {
            vec![p.into()]
        }
        ASTType::Custom {
            name: _,
            param_types: pt,
        } => {
            let mut result: Vec<String> = pt
                .iter()
                .flat_map(|it| match it.clone().ast_type {
                    ASTType::Parametric(name) => {
                        vec![name]
                    }
                    _ => get_parametric_types(&it.ast_type),
                })
                .collect();
            result.sort();
            result.dedup();
            result
        }
    };
}

fn convert_expr(
    module: &EnhancedASTModule,
    expr: &ASTExpression,
    context: &VarContext,
    new_function_defs: &mut LinkedHashMap<String, ASTFunctionDef>,
    used_untyped_function_defs: &mut LinkedHashMap<String, ASTFunctionDef>,
) -> ASTExpression {
    match expr {
        ASTExpression::StringLiteral(_) => expr.clone(),
        ASTExpression::ASTFunctionCallExpression(call) => {
            let new_call = convert_call(
                module,
                context,
                call,
                new_function_defs,
                used_untyped_function_defs,
            );
            ASTExpression::ASTFunctionCallExpression(new_call)
        }
        ASTExpression::Val(p) => {
            /*
            if let Some(kind) = context.get(p) {
                match kind {
                    VarKind::ParameterRef(i, par) => {

                      todo!()
                    }
                }
            } else {
                todo!()
            }

             */
            expr.clone()
        }
        ASTExpression::Number(_) => expr.clone(),
        ASTExpression::Lambda(_) => expr.clone(),
    }
}

fn convert_call(
    module: &EnhancedASTModule,
    context: &VarContext,
    call: &ASTFunctionCall,
    new_function_defs: &mut LinkedHashMap<String, ASTFunctionDef>,
    used_untyped_function_defs: &mut LinkedHashMap<String, ASTFunctionDef>,
) -> ASTFunctionCall {
    let mut expressions = Vec::new();
    let mut parameters = Vec::new();

    //let context = VarContext::new(Some(context));

    let function_def = module
        .functions_by_name
        .get(&call.function_name)
        .unwrap_or_else(|| panic!("function {} not found {:?}", call.function_name, module.functions_by_name.keys().collect::<Vec<&String>>()));

    if function_def.param_types.is_empty() {
        info!(
            "TODO check for not parameterized function {}",
            call.function_name
        );
        used_untyped_function_defs.insert(function_def.name.clone(), function_def.clone());
        return call.clone();
    }

    let mut resolved_param_types = HashMap::new();

    for (i, expr) in call.parameters.iter().enumerate() {
        let par = function_def.parameters.get(i).unwrap_or_else(|| {
            panic!(
                "Cannot find parameter {i} when calling function {}",
                call.function_name
            )
        });
        match expr {
            ASTExpression::StringLiteral(_) => {
                update(
                    ASTType::Builtin(BuiltinTypeKind::ASTString),
                    expr.clone(),
                    par,
                    &mut resolved_param_types,
                    &mut parameters,
                    &mut expressions,
                );
            }
            ASTExpression::ASTFunctionCallExpression(call) => {
                let ast_function_call = convert_call(
                    module,
                    context,
                    call,
                    new_function_defs,
                    used_untyped_function_defs,
                );

                let mut all_functions = new_function_defs.clone();

                for (name, function_def) in used_untyped_function_defs.iter() {
                    all_functions.insert(name.clone(), function_def.clone());
                }

                //info!("new_function_defs {:?} used_untyped_function_defs {:?}", new_function_defs, used_untyped_function_defs);

                let inner_function_def = all_functions
                    .get(&ast_function_call.function_name)
                    .unwrap_or_else(|| {
                        panic!("Cannot find function {}", ast_function_call.function_name)
                    });

                update(
                    inner_function_def.return_type.clone().unwrap().ast_type,
                    ASTExpression::ASTFunctionCallExpression(ast_function_call),
                    par,
                    &mut resolved_param_types,
                    &mut parameters,
                    &mut expressions,
                );
            }
            ASTExpression::Val(v) => {
                let result_type = match context.get(v).unwrap_or_else(|| {
                    panic!("cannot find val {v}, actual context {:?}", context.names())
                }) {
                    VarKind::ParameterRef(_, referenced_parameter_def) => {
                        referenced_parameter_def.type_ref.clone()
                    }
                };

                update(
                    result_type.ast_type,
                    expr.clone(),
                    par,
                    &mut resolved_param_types,
                    &mut parameters,
                    &mut expressions,
                );
            }
            ASTExpression::Number(_) => {
                update(
                    ASTType::Builtin(BuiltinTypeKind::ASTI32),
                    expr.clone(),
                    par,
                    &mut resolved_param_types,
                    &mut parameters,
                    &mut expressions,
                );
            }
            ASTExpression::Lambda(lambda) => {
                let mut context = VarContext::new(Some(context));

                for (inner_i, name) in lambda.parameter_names.iter().enumerate() {
                    match &par.type_ref.ast_type {
                        ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters,
                            return_type,
                        }) => {
                            let pp = parameters.get(inner_i).unwrap();

                            context.insert(
                                name.clone(),
                                VarKind::ParameterRef(
                                    inner_i,
                                    ASTParameterDef {
                                        name: name.clone(),
                                        type_ref: pp.clone(),
                                    },
                                ),
                            );
                        }
                        _ => {
                            panic!()
                        }
                    }
                }

                let new_lambda = ASTLambdaDef {
                    body: lambda
                        .body
                        .iter()
                        .map(|it| {
                            convert_expr(
                                module,
                                it,
                                &context,
                                new_function_defs,
                                used_untyped_function_defs,
                            )
                        })
                        .collect(),
                    parameter_names: lambda.parameter_names.clone(),
                };

                let new_expr = ASTExpression::Lambda(new_lambda);

                update(
                    ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters: Vec::new(),
                        return_type: None,
                    }),
                    new_expr,
                    par,
                    &mut resolved_param_types,
                    &mut parameters,
                    &mut expressions,
                );
            }
        }
    }

    let new_function_name = format!("{}_{}", call.function_name, new_function_defs.len() + 1);

    info!("created new function {new_function_name}");

    let new_function_def = ASTFunctionDef {
        name: new_function_name.clone(),
        parameters,
        return_type: function_def
            .return_type
            .clone()
            .map(|it| substitute(&it, &resolved_param_types)),
        body: function_def.body.clone(),
        param_types: Vec::new(),
        inline: function_def.inline,
    };

    new_function_defs.insert(new_function_name.clone(), new_function_def);

    ASTFunctionCall {
        function_name: new_function_name,
        parameters: Vec::new(), // TODO
    }
}

fn substitute(
    ast_type: &ASTTypeRef,
    resolved_param_types: &HashMap<String, ASTType>,
) -> ASTTypeRef {
    let new_ast_type = match &ast_type.ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters: parameters
                    .iter()
                    .map(|it| substitute(it, resolved_param_types))
                    .collect(),
                return_type: return_type
                    .clone()
                    .map(|it| Box::new(substitute(&it, resolved_param_types))),
            }),
            _ => ast_type.ast_type.clone(),
        },
        ASTType::Parametric(p) => resolved_param_types
            .get(p)
            .unwrap_or_else(|| panic!("cannot resolve parametric type {p}"))
            .clone(),
        ASTType::Custom { name, param_types } => ASTType::Custom {
            name: name.clone(),
            param_types: param_types
                .iter()
                .map(|it| substitute(it, resolved_param_types))
                .collect(),
        },
    };

    ASTTypeRef {
        ast_type: new_ast_type,
        ast_ref: ast_type.ast_ref,
    }
}

fn update(
    result_type: ASTType,
    expr: ASTExpression,
    par: &ASTParameterDef,
    resolved_param_types: &mut HashMap<String, ASTType>,
    parameters: &mut Vec<ASTParameterDef>,
    expressions: &mut Vec<ASTExpression>,
) {
    if let ASTType::Parametric(p) = &par.type_ref.ast_type {
        if let Some(old_type) = resolved_param_types.insert(p.clone(), result_type.clone()) {

            if old_type != result_type && !matches!(old_type, ASTType::Parametric(_)) {
                panic!(
                    "Parameter {p} has multiple values: {:?}, {:?}",
                    old_type, result_type
                );
            }
        }
    }

    expressions.push(expr);

    parameters.push(ASTParameterDef {
        name: par.name.clone(),
        type_ref: ASTTypeRef {
            ast_type: result_type,
            ast_ref: par.type_ref.ast_ref,
        },
    });
}

#[cfg(test)]
mod tests {
    use crate::codegen::backend::BackendAsm386;
    use crate::codegen::EnhancedASTModule;
    use crate::lexer::Lexer;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule,
        ASTParameterDef, ASTTypeRef,
    };
    use crate::parser::Parser;
    use crate::transformations::struct_functions_creator::struct_functions_creator;
    use crate::type_check2::convert;
    use crate::type_check2::resolved_ast::ASTTypedModule;
    use std::path::Path;
    use test_env_log::test;
    use crate::transformations::enum_functions_creator::enum_functions_creator;

    #[test]
    fn test() {
        let parameter = ASTExpression::Number(10);

        let call = ASTFunctionCall {
            function_name: "consume".into(),
            parameters: vec![parameter],
        };

        let function_def = ASTFunctionDef {
            name: "consume".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: vec![ASTParameterDef {
                name: "v".into(),
                type_ref: ASTTypeRef::parametric("T", false),
            }],
            inline: false,
            return_type: None,
            param_types: vec!["T".into()],
        };

        let module = ASTModule {
            structs: Vec::new(),
            enums: Vec::new(),
            body: vec![call],
            functions: vec![function_def],
        };

        let new_module = convert(&EnhancedASTModule::new(&module));

        assert_eq!(new_module.body.get(0).unwrap().function_name, "consume_1");

        print(new_module);
    }

    #[test]
    fn test_list() {
        test_file("list.rasm");
    }

    #[test]
    fn test_list_fmap() {
        test_file("list_fmap.rasm");
    }

    fn test_file(file_name: &str) {
        println!("file_name {file_name}");

        let resource = format!("resources/test/{}", file_name);
        let path = Path::new(&resource);
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

        let backend = BackendAsm386::new();
        let new_module = convert(&struct_functions_creator(
            &backend,
            &enum_functions_creator(&backend, &EnhancedASTModule::new(&module)),
        ));

        print(new_module);
    }

    fn print(new_module: ASTTypedModule) {
        println!(
            "functions {:?}",
            new_module
                .functions_by_name
                .values()
                .map(|it| &it.name)
                .collect::<Vec<&String>>()
        );

        println!(
            "enums {:?}",
            new_module
                .enums
                .iter()
                .map(|it| &it.name)
                .collect::<Vec<&String>>()
        );
    }
}
