use std::ops::Deref;

use log::debug;

use type_check_error::TypeCheckError;

use crate::codegen::text_macro::{MacroParam, TextMacro};
use crate::parser::ast::{ASTIndex, MyToString};
use crate::parser::ast::{ASTType, BuiltinTypeKind};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::{debug_i, dedent, indent};

pub mod call_stack;
pub mod functions_container;
pub mod resolved_generic_types;
pub mod type_check_error;
pub mod typed_ast;

pub mod traverse_typed_ast;
pub mod used_functions;

pub fn get_new_native_call(m: &TextMacro, to_function: &str) -> String {
    let p = m
        .parameters
        .iter()
        .enumerate()
        .filter(|(i, _p)| *i > 0)
        .map(|(_, it)| match it {
            MacroParam::Plain(value, ast_type, _) => match ast_type {
                None => value.to_string(),
                // TODO duplicated code
                Some(t) => {
                    if matches!(t, ASTType::Builtin(BuiltinTypeKind::Lambda { .. })) {
                        value.to_string()
                    } else {
                        format!("{it}")
                    }
                }
            },
            MacroParam::StringLiteral(_) => format!("{it}"),
            MacroParam::Ref(value, ast_type, _) => match ast_type {
                None => value.to_string(),
                // TODO duplicated code
                Some(t) => {
                    if matches!(t, ASTType::Builtin(BuiltinTypeKind::Lambda { .. })) {
                        value.to_string()
                    } else {
                        format!("{it}")
                    }
                }
            },
        })
        .collect::<Vec<_>>()
        .join(",");

    if p.is_empty() {
        format!("$call({to_function})")
    } else {
        format!("$call({to_function},{p})")
    }
}

pub fn is_generic_type(ast_type: &ASTType) -> bool {
    return match ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::String => false,
            BuiltinTypeKind::I32 => false,
            BuiltinTypeKind::Bool => false,
            BuiltinTypeKind::Char => false,
            BuiltinTypeKind::F32 => false,
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => {
                let mut par_types: bool = parameters.iter().any(is_generic_type);
                if !return_type.is_unit() {
                    par_types = par_types || is_generic_type(return_type.deref());
                }
                par_types
            }
        },
        ASTType::Generic(_p) => true,
        ASTType::Custom {
            name: _,
            param_types: pt,
            index: _,
        } => pt.iter().any(|it| match it {
            ASTType::Generic(_name) => true,
            _ => is_generic_type(it),
        }),
        ASTType::Unit => false,
    };
}

pub fn resolve_generic_types_from_effective_type(
    generic_type: &ASTType,
    effective_type: &ASTType,
) -> Result<ResolvedGenericTypes, TypeCheckError> {
    let mut result = ResolvedGenericTypes::new();
    if generic_type == effective_type || !is_generic_type(generic_type) {
        return Ok(result);
    }

    debug_i!("extract_generic_types_from_effective_type: generic_type {generic_type} effective_type  {effective_type}");
    indent!();

    match generic_type {
        ASTType::Builtin(kind) => {
            match kind {
                BuiltinTypeKind::String => {}
                BuiltinTypeKind::I32 => {}
                BuiltinTypeKind::Bool => {}
                BuiltinTypeKind::Char => {}
                BuiltinTypeKind::F32 => {}
                BuiltinTypeKind::Lambda {
                    parameters: p_parameters,
                    return_type: p_return_type,
                } => match effective_type {
                    ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters: e_parameters,
                        return_type: e_return_type,
                    }) => {
                        if e_parameters.len() != p_parameters.len() {
                            return Err(type_check_error("Invalid parameters count.".to_string()));
                        }
                        for (i, p_p) in p_parameters.iter().enumerate() {
                            let e_p = e_parameters.get(i).unwrap();
                            let inner_result = resolve_generic_types_from_effective_type(p_p, e_p)
                            .map_err(|e| e.add(ASTIndex::none(), format!("lambda param gen type {generic_type}eff. type {effective_type}"), Vec::new()))?;

                            result
                                .extend(inner_result)
                                .map_err(|it| type_check_error(it.clone()))?;
                        }

                        /*
                        for p_t in p_return_type {
                            if let Some(e_t) = e_return_type {
                                let inner_result = resolve_generic_types_from_effective_type(p_t, e_t)
                                    .map_err(|e| format!("{} in return type gen type {generic_type}eff. type {effective_type}", e))?;

                                result.extend(inner_result.into_iter());
                            } else {
                                dedent!();
                                if let ASTType::Generic(p) = p_t.as_ref() {
                                    return Err(format!("Found generic type {p} that is (). For now we cannot handle it").into());
                                }
                                return Err("Expected some type but got None".into());
                            }
                        }

                         */
                        let inner_result = resolve_generic_types_from_effective_type(p_return_type, e_return_type)
                        .map_err(|e| e.add(ASTIndex::none(), format!("in return type gen type {generic_type}eff. type {effective_type}"), Vec::new()))?;

                        result
                            .extend(inner_result)
                            .map_err(|it| type_check_error(it.clone()))?;
                    }
                    _ => {
                        dedent!();
                        return Err(type_check_error("unmatched types".to_string()));
                    }
                },
            }
        }
        ASTType::Generic(p) => {
            let ignore = if let ASTType::Generic(p1) = effective_type {
                p == p1
            } else {
                false
            };
            if !ignore {
                debug_i!("resolved generic type {p} to {effective_type}");
                result.insert(p.clone(), effective_type.clone());
            }
        }
        ASTType::Custom {
            name: p_name,
            param_types: p_param_types,
            index: _,
        } => match effective_type {
            ASTType::Custom {
                name: e_name,
                param_types: e_param_types,
                index: _,
            } => {
                if p_name != e_name {
                    dedent!();
                    return Err(type_check_error(format!(
                        "unmatched custom type name {p_name} {e_name}"
                    )));
                }

                for (i, p_p) in p_param_types.iter().enumerate() {
                    let e_p = e_param_types.get(i).unwrap();
                    let inner_result = resolve_generic_types_from_effective_type(p_p, e_p)
                        .map_err(|e| e.add(ASTIndex::none(), format!("in custom type gen type {generic_type} eff type {effective_type}"), Vec::new()))?;

                    result.extend(inner_result).map_err(|it| {
                        TypeCheckError::new(
                            ASTIndex::none(),
                            format!(
                                "in custom type gen type {generic_type} eff type {effective_type}"
                            ),
                            Vec::new(),
                        )
                    })?;
                }
            }
            ASTType::Generic(_) => {}
            _ => {
                dedent!();
                return Err(type_check_error(format!(
                    "unmatched types generic type: {:?}, effective type: {:?}",
                    generic_type, effective_type
                )));
            }
        },
        ASTType::Unit => {}
    }

    debug_i!("result {}", result.my_to_string());
    dedent!();
    Ok(result)
}

fn type_check_error(message: String) -> TypeCheckError {
    TypeCheckError::new(ASTIndex::none(), message, Vec::new())
}

pub fn substitute(
    ast_type: &ASTType,
    resolved_param_types: &ResolvedGenericTypes,
) -> Option<ASTType> {
    if !is_generic_type(ast_type) {
        return None;
    }

    let result = match &ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => {
                let mut something_substituted = false;
                let new_parameters = match substitute_types(parameters, resolved_param_types) {
                    None => parameters.clone(),
                    Some(new_parameters) => {
                        something_substituted = true;
                        new_parameters
                    }
                };

                let new_return_type =
                    if let Some(new_t) = substitute(return_type, resolved_param_types) {
                        something_substituted = true;
                        Box::new(new_t)
                    } else {
                        return_type.clone()
                    };

                if something_substituted {
                    Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters: new_parameters,
                        return_type: new_return_type,
                    }))
                } else {
                    None
                }
            }
            _ => None,
        },
        ASTType::Generic(p) => {
            if resolved_param_types.contains_key(p) {
                resolved_param_types.get(p).cloned()
            } else {
                None
            }
        }
        ASTType::Custom {
            name,
            param_types,
            index,
        } => {
            substitute_types(param_types, resolved_param_types).map(|new_param_types| {
                // TODO it's a bit heuristic
                let new_index = if new_param_types.is_empty() {
                    index.clone()
                } else if let Some(ASTType::Custom {
                    name: _,
                    param_types: _,
                    index: ast_index,
                }) = new_param_types.last()
                {
                    ast_index.mv(1)
                } else {
                    index.clone()
                };
                ASTType::Custom {
                    name: name.clone(),
                    param_types: new_param_types,
                    index: new_index,
                }
            })
        }
        ASTType::Unit => None,
    };

    if let Some(r) = &result {
        debug_i!("something substituted {ast_type} -> {r}");
    }
    result
}

fn substitute_types(
    types: &[ASTType],
    resolved_param_types: &ResolvedGenericTypes,
) -> Option<Vec<ASTType>> {
    let mut something_substituted = false;
    let new_types = types
        .iter()
        .map(|it| {
            if let Some(new_t) = substitute(it, resolved_param_types) {
                something_substituted = true;
                new_t
            } else {
                it.clone()
            }
        })
        .collect();

    if something_substituted {
        Some(new_types)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{ASTIndex, ASTType, BuiltinTypeKind};
    use crate::type_check::resolve_generic_types_from_effective_type;
    use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
    use crate::type_check::type_check_error::TypeCheckError;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_simple() -> Result<(), TypeCheckError> {
        let generic_type = generic("T");
        let effective_type = i32();
        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = ResolvedGenericTypes::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_custom() -> Result<(), TypeCheckError> {
        let generic_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![generic("T")],
            index: ASTIndex::none(),
        };
        let effective_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![i32()],
            index: ASTIndex::none(),
        };

        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = ResolvedGenericTypes::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda() -> Result<(), TypeCheckError> {
        let generic_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![generic("T")],
            return_type: Box::new(generic("T")),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![generic("T")],
            return_type: Box::new(i32()),
        });

        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = ResolvedGenericTypes::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda1() -> Result<(), TypeCheckError> {
        let generic_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![generic("T")],
            return_type: Box::new(generic("T")),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![i32()],
            return_type: Box::new(generic("T")),
        });

        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = ResolvedGenericTypes::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);
        Ok(())
    }

    fn generic(name: &str) -> ASTType {
        ASTType::Generic(name.into())
    }

    fn i32() -> ASTType {
        ASTType::Builtin(BuiltinTypeKind::I32)
    }

    /*
    TODO cannot work without a source file or folder
    #[test]
    fn test() {
        init();

        let parameter = ASTExpression::Value(
            ValueType::I32(10),
            ASTIndex {
                file_name: None,
                row: 0,
                column: 0,
            },
        );

        let call = ASTStatement::Expression(ASTFunctionCallExpression(ASTFunctionCall {
            original_function_name: "consume".into(),
            function_name: "consume".into(),
            parameters: vec![parameter],
            index: ASTIndex {
                file_name: None,
                row: 0,
                column: 0,
            },
        }));

        let function_def = ASTFunctionDef {
            name: "consume".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: vec![ASTParameterDef {
                name: "v".into(),
                ast_type: ASTType::Generic("T".into()),
                ast_index: ASTIndex::none(),
            }],
            inline: false,
            return_type: ASTType::Unit,
            generic_types: vec!["T".into()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            original_name: "consume".into(),
            index: ASTIndex::none(),
        };

        let module = ASTModule {
            path: PathBuf::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            body: vec![call],
            functions: vec![function_def],
            requires: Default::default(),
            externals: Default::default(),
            types: Vec::new(),
        };

        let new_module = convert_to_typed_module(
            &EnhancedASTModule::new(vec![module], PathBuf::from("resources")),
            false,
            Vec::new(),
            &BackendNasm386::new(false),
            &mut Statics::new(),
            true,
            Vec::new(),
        )
        .unwrap();

        let par = if let Some(ASTTypedStatement::Expression(
            ASTTypedExpression::ASTFunctionCallExpression(e),
        )) = new_module.body.get(1)
        {
            Some(e)
        } else {
            None
        };

        assert_eq!(par.unwrap().original_function_name, "consume");
        assert!(new_module.functions_by_name.get("consume_0").is_some());
    }
     */

    /*
    TODO
    #[test]
    fn test_replace_native_call() {
        let body = "$call(nprint,10)";
        assert_eq!(
            "$call(nprint_2,10)".to_string(),
            replace_native_call(body, "nprint", "nprint_2", 0)
        );
    }

    #[test]
    fn test_replace_native_call1() {
        let body = "$call( nprint ,10)";
        assert_eq!(
            "$call(nprint_2,10)".to_string(),
            replace_native_call(body, "nprint", "nprint_2", 0)
        );
    }

    #[test]
    fn test_replace_native_call2() {
        let body = "$call(nprintln,10)";
        assert_eq!(
            "$call(nprintln,10)".to_string(),
            replace_native_call(body, "nprint", "nprint_2", 0)
        );
    }

    #[test]
    fn test_replace_native_call3() {
        let body = "$call(dummy)";
        assert_eq!(
            "$call(dummy_2)".to_string(),
            replace_native_call(body, "dummy", "dummy_2", 0)
        );
    }

     */
}
