use crate::codegen::enh_ast::{EnhASTType, EnhBuiltinTypeKind};
use crate::codegen::text_macro::{MacroParam, TextMacro};
use crate::enh_type_check::enh_resolved_generic_types::EnhResolvedGenericTypes;
use rasm_utils::debug_i;

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
        EnhASTType::Generic(_, p, var_types) => {
            if resolved_param_types.contains_key(p, var_types) {
                resolved_param_types.get(p, var_types).cloned()
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
    // TODO
}
