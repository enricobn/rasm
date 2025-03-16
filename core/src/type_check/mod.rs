use crate::codegen::enh_ast::EnhASTIndex;
use crate::codegen::enh_ast::{EnhASTType, EnhBuiltinTypeKind};
use crate::codegen::text_macro::{MacroParam, TextMacro};
use crate::enh_type_check::enh_resolved_generic_types::EnhResolvedGenericTypes;
use crate::enh_type_check::enh_type_check_error::EnhTypeCheckError;
use rasm_utils::{debug_i, dedent, indent};

pub mod ast_generic_types_resolver;
pub mod ast_modules_container;
pub mod ast_type_checker;
pub mod functions_dependencies;

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
                    if matches!(t, EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda { .. })) {
                        value.to_string()
                    } else {
                        it.render()
                    }
                }
            },
            MacroParam::StringLiteral(_) => it.render(),
            MacroParam::Ref(value, ast_type, _) => match ast_type {
                None => value.to_string(),
                // TODO duplicated code
                Some(t) => {
                    if matches!(t, EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda { .. })) {
                        value.to_string()
                    } else {
                        it.render()
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

pub fn resolve_generic_types_from_effective_type(
    generic_type: &EnhASTType,
    effective_type: &EnhASTType,
) -> Result<EnhResolvedGenericTypes, EnhTypeCheckError> {
    let mut result = EnhResolvedGenericTypes::new();
    if generic_type == effective_type || !generic_type.is_generic() {
        return Ok(result);
    }

    debug_i!("resolve_generic_types_from_effective_type: generic_type {generic_type} effective_type  {effective_type}");
    //println!("resolve_generic_types_from_effective_type: generic_type {generic_type} effective_type  {effective_type}");
    indent!();

    match generic_type {
        EnhASTType::Builtin(kind) => {
            match kind {
                EnhBuiltinTypeKind::String => {}
                EnhBuiltinTypeKind::I32 => {}
                EnhBuiltinTypeKind::Bool => {}
                EnhBuiltinTypeKind::Char => {}
                EnhBuiltinTypeKind::F32 => {}
                EnhBuiltinTypeKind::Lambda {
                    parameters: p_parameters,
                    return_type: p_return_type,
                } => match effective_type {
                    EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                        parameters: e_parameters,
                        return_type: e_return_type,
                    }) => {
                        if e_parameters.len() != p_parameters.len() {
                            return Err(type_check_error("Invalid parameters count.".to_string()));
                        }
                        for (i, p_p) in p_parameters.iter().enumerate() {
                            let e_p = e_parameters.get(i).unwrap();

                            let inner_result = resolve_generic_types_from_effective_type(p_p, e_p)
                            .map_err(|e| e.add(EnhASTIndex::none(), format!("lambda param gen type {generic_type}, eff. type {effective_type}"), Vec::new()))?;

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
                                    return Err(format!("Found generic type {p} that is (). For now, we cannot handle it").into());
                                }
                                return Err("Expected some type but got None".into());
                            }
                        }

                         */
                        let inner_result = resolve_generic_types_from_effective_type(p_return_type, e_return_type)
                        .map_err(|e| e.add(EnhASTIndex::none(), format!("in return type gen type {generic_type}, eff. type {effective_type}"), Vec::new()))?;

                        result
                            .extend(inner_result)
                            .map_err(|it| type_check_error(it.clone()))?;
                    }
                    _ => {
                        dedent!();
                        return Err(type_check_error(format!("unmatched types, generic type is {generic_type}, real type is {effective_type}")));
                    }
                },
            }
        }
        EnhASTType::Generic(_, p) => {
            let ignore = if let EnhASTType::Generic(_, p1) = effective_type {
                p == p1
            } else {
                false
            };
            if !ignore {
                debug_i!("resolved generic type {p} to {effective_type}");
                result.insert(p.clone(), effective_type.clone());
            }
        }
        EnhASTType::Custom {
            namespace: _,
            name: g_name,
            param_types: g_param_types,
            index: _,
        } => match effective_type {
            EnhASTType::Custom {
                namespace: _,
                name: e_name,
                param_types: e_param_types,
                index: _,
            } => {
                if g_name != e_name {
                    dedent!();
                    return Err(type_check_error(format!(
                        "unmatched custom type name {g_name} != {e_name}"
                    )));
                }

                for (i, p_p) in g_param_types.iter().enumerate() {
                    let e_p = if let Some(p) = e_param_types.get(i) {
                        p
                    } else {
                        return Err(EnhTypeCheckError::new(
                            EnhASTIndex::none(),
                            format!("Cannot find parameter {i}"),
                            Vec::new(),
                        ));
                    };
                    let inner_result = resolve_generic_types_from_effective_type(p_p, e_p)
                        .map_err(|e| e.add(EnhASTIndex::none(), format!("in custom type gen type {generic_type} eff type {effective_type}"), Vec::new()))?;

                    result.extend(inner_result).map_err(|it| {
                        EnhTypeCheckError::new(
                            EnhASTIndex::none(),
                            format!(
                                "{it}: in custom type gen type {generic_type} eff type {effective_type}"
                            ),
                            Vec::new(),
                        )
                    })?;
                }
            }
            EnhASTType::Generic(_, _) => {}
            _ => {
                dedent!();
                return Err(type_check_error(format!(
                    "unmatched types, generic type is {generic_type}, real type is {effective_type}")));
            }
        },
        EnhASTType::Unit => {}
    }

    debug_i!("result {result}");
    dedent!();
    Ok(result)
}

fn type_check_error(message: String) -> EnhTypeCheckError {
    EnhTypeCheckError::new(EnhASTIndex::none(), message, Vec::new())
}

pub fn substitute(
    ast_type: &EnhASTType,
    resolved_param_types: &EnhResolvedGenericTypes,
) -> Option<EnhASTType> {
    if !ast_type.is_generic() {
        return None;
    }

    let result = match &ast_type {
        EnhASTType::Builtin(kind) => match kind {
            EnhBuiltinTypeKind::Lambda {
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
                    Some(EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                        parameters: new_parameters,
                        return_type: new_return_type,
                    }))
                } else {
                    None
                }
            }
            _ => None,
        },
        EnhASTType::Generic(_, p) => {
            if resolved_param_types.contains_key(p) {
                resolved_param_types.get(p).cloned()
            } else {
                None
            }
        }
        EnhASTType::Custom {
            namespace,
            name,
            param_types,
            index,
        } => {
            substitute_types(param_types, resolved_param_types).map(|new_param_types| {
                // TODO it's a bit heuristic
                let new_index = if new_param_types.is_empty() {
                    index.clone()
                } else if let Some(EnhASTType::Custom {
                    namespace: _,
                    name: _,
                    param_types: _,
                    index: ast_index,
                }) = new_param_types.last()
                {
                    ast_index.mv_right(1)
                } else {
                    index.clone()
                };
                EnhASTType::Custom {
                    namespace: namespace.clone(),
                    name: name.clone(),
                    param_types: new_param_types,
                    index: new_index,
                }
            })
        }
        EnhASTType::Unit => None,
    };

    if let Some(r) = &result {
        debug_i!("something substituted {ast_type} -> {r}");
    }
    result
}

fn substitute_types(
    types: &[EnhASTType],
    resolved_param_types: &EnhResolvedGenericTypes,
) -> Option<Vec<EnhASTType>> {
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
    use crate::codegen::enh_ast::{EnhASTIndex, EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind};
    use crate::enh_type_check::enh_resolved_generic_types::EnhResolvedGenericTypes;
    use crate::enh_type_check::enh_type_check_error::EnhTypeCheckError;
    use crate::type_check::resolve_generic_types_from_effective_type;

    #[test]
    fn test_extract_generic_types_from_effective_type_simple() -> Result<(), EnhTypeCheckError> {
        let generic_type = generic("T");
        let effective_type = i32();
        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = EnhResolvedGenericTypes::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_custom() -> Result<(), EnhTypeCheckError> {
        let generic_type = EnhASTType::Custom {
            namespace: EnhASTNameSpace::global(),
            name: "List".into(),
            param_types: vec![generic("T")],
            index: EnhASTIndex::none(),
        };
        let effective_type = EnhASTType::Custom {
            namespace: EnhASTNameSpace::global(),
            name: "List".into(),
            param_types: vec![i32()],
            index: EnhASTIndex::none(),
        };

        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = EnhResolvedGenericTypes::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda() -> Result<(), EnhTypeCheckError> {
        let generic_type = EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
            parameters: vec![generic("T")],
            return_type: Box::new(generic("T")),
        });

        let effective_type = EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
            parameters: vec![generic("T")],
            return_type: Box::new(i32()),
        });

        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = EnhResolvedGenericTypes::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda1() -> Result<(), EnhTypeCheckError> {
        let generic_type = EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
            parameters: vec![generic("T")],
            return_type: Box::new(generic("T")),
        });

        let effective_type = EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
            parameters: vec![i32()],
            return_type: Box::new(generic("T")),
        });

        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = EnhResolvedGenericTypes::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);
        Ok(())
    }

    fn generic(name: &str) -> EnhASTType {
        EnhASTType::Generic(EnhASTIndex::none(), name.into())
    }

    fn i32() -> EnhASTType {
        EnhASTType::Builtin(EnhBuiltinTypeKind::I32)
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
