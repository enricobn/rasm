use std::fmt::{Display, Formatter};

use linked_hash_map::LinkedHashMap;
use rasm_parser::{
    catalog::ASTIndex,
    parser::ast::{ASTType, BuiltinTypeKind},
};
use rasm_utils::{debug_i, dedent, indent, LinkedHashMapDisplay, SliceDisplay};

use super::ast_type_checker::{ASTTypeCheckErroKind, ASTTypeCheckError};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTResolvedGenericTypes {
    map: LinkedHashMap<String, LinkedHashMap<Vec<ASTType>, ASTType>>,
}

impl Display for ASTResolvedGenericTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (name, inner) in self.map.iter() {
            for (var_types, t) in inner.iter() {
                write!(f, "{name}<{}>={t},", SliceDisplay(&var_types))?;
            }
        }
        Ok(())
    }
}

impl ASTResolvedGenericTypes {
    pub fn new() -> Self {
        Self {
            map: LinkedHashMap::new(),
        }
    }

    pub fn resolve_generic_types_from_effective_type(
        generic_type: &ASTType,
        effective_type: &ASTType,
    ) -> Result<Self, ASTTypeCheckError> {
        let mut result = ASTResolvedGenericTypes::new();
        if generic_type == effective_type {
            return Ok(result);
        }

        debug_i!("resolve_generic_types_from_effective_type {generic_type} ==> {effective_type}");
        //println!("resolve_generic_types_from_effective_type: generic_type {generic_type} effective_type  {effective_type}");
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
                                dedent!();
                                return Err(Self::type_check_error(
                                    ASTTypeCheckErroKind::Error,
                                    "Invalid parameters count.".to_string(),
                                ));
                            }
                            for (i, p_p) in p_parameters.iter().enumerate() {
                                let e_p = e_parameters.get(i).unwrap();

                                let inner_result = Self::resolve_generic_types_from_effective_type(p_p, e_p)
                                .map_err(|e| {
                                    dedent!();
                                    e.add(ASTTypeCheckErroKind::Error, ASTIndex::none(), format!("lambda param gen type {generic_type}, eff. type {effective_type}"))})?;

                                result.extend(inner_result).map_err(|it| {
                                    dedent!();
                                    Self::type_check_error(ASTTypeCheckErroKind::Error, it.clone())
                                })?;
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
                            let inner_result = Self::resolve_generic_types_from_effective_type(p_return_type, e_return_type)
                            .map_err(|e| {
                                dedent!();
                                e.add(ASTTypeCheckErroKind::Error, ASTIndex::none(), format!("in return type gen type {generic_type}, eff. type {effective_type}"))})?;

                            result.extend(inner_result).map_err(|it| {
                                dedent!();
                                Self::type_check_error(ASTTypeCheckErroKind::Error, it.clone())
                            })?;
                        }
                        _ => {
                            dedent!();
                            return Err(Self::type_check_error(ASTTypeCheckErroKind::Error, format!("unmatched types, generic type is {generic_type}, real type is {effective_type}")));
                        }
                    },
                }
            }
            ASTType::Generic(_, p, var_types) => {
                debug_i!("resolved generic type {p} to {effective_type}");
                result.insert(p.clone(), var_types.clone(), effective_type.clone());
            }
            ASTType::Custom {
                name: g_name,
                param_types: g_param_types,
                position: _,
            } => match effective_type {
                ASTType::Custom {
                    name: e_name,
                    param_types: e_param_types,
                    position: _,
                } => {
                    if g_name != e_name {
                        dedent!();
                        return Err(Self::type_check_error(
                            ASTTypeCheckErroKind::Error,
                            format!("unmatched custom type name {g_name} != {e_name}"),
                        ));
                    }

                    for (i, p_p) in g_param_types.iter().enumerate() {
                        let e_p = if let Some(p) = e_param_types.get(i) {
                            p
                        } else {
                            return Err(ASTTypeCheckError::new(
                                ASTTypeCheckErroKind::Error,
                                ASTIndex::none(),
                                format!("Cannot find parameter {i}"),
                            ));
                        };
                        let inner_result = Self::resolve_generic_types_from_effective_type(p_p, e_p)
                            .map_err(|e| { dedent!();
                        e.add(ASTTypeCheckErroKind::Error, ASTIndex::none(), format!("in custom type gen type {generic_type} eff type {effective_type}"))})?;

                        result.extend(inner_result).map_err(|it| {
                            dedent!();
                            ASTTypeCheckError::new(
                                ASTTypeCheckErroKind::Error,
                                ASTIndex::none(),
                                format!(
                                    "{it}: in custom type gen type {generic_type} eff type {effective_type}"
                                ),
                            )
                        })?;
                    }
                }
                ASTType::Generic(_, _, _) => {}
                _ => {
                    dedent!();
                    return Err(Self::type_check_error(ASTTypeCheckErroKind::Error, format!(
                        "unmatched types, generic type is {generic_type}, real type is {effective_type}")));
                }
            },
            ASTType::Unit => {}
        }

        dedent!();
        Ok(result)
    }

    fn type_check_error(kind: ASTTypeCheckErroKind, message: String) -> ASTTypeCheckError {
        ASTTypeCheckError::new(kind, ASTIndex::none(), message)
    }

    pub fn get(&self, key: &String, var_types: &Vec<ASTType>) -> Option<&ASTType> {
        self.map.get(key).and_then(|it| it.get(var_types))
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn contains_key(&self, key: &String, var_types: &Vec<ASTType>) -> bool {
        self.map
            .get(key)
            .map(|it| it.contains_key(var_types))
            .unwrap_or(false)
    }

    pub fn extend(&mut self, other: ASTResolvedGenericTypes) -> Result<(), String> {
        for (key, new_type) in other.map.into_iter() {
            let inner = self.map.entry(key.clone()).or_insert(LinkedHashMap::new());
            for (key1, t) in new_type.into_iter() {
                if let Some(prev_type) = inner.get(&key1) {
                    if &t != prev_type && t.is_generic() {
                        return Err(format!(
                            "Already resolved generic {key}<{}>, prev {prev_type}, new {t}",
                            SliceDisplay(&key1)
                        ));
                    }
                }
                inner.insert(key1, t);
            }
        }
        Ok(())
    }

    pub fn insert(
        &mut self,
        key: String,
        var_types: Vec<ASTType>,
        value: ASTType,
    ) -> Option<ASTType> {
        let new = self.map.entry(key).or_insert(LinkedHashMap::new());
        if let Some(t) = new.get(&var_types) {
            assert_eq!(t, &value);
        }

        new.insert(var_types, value)
    }

    pub fn substitute(&self, ast_type: &ASTType) -> Option<ASTType> {
        if !ast_type.is_generic() {
            return None;
        }

        let result = match &ast_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let mut something_substituted = false;
                    let new_parameters = match self.substitute_types(parameters) {
                        None => parameters.clone(),
                        Some(new_parameters) => {
                            something_substituted = true;
                            new_parameters
                        }
                    };

                    let new_return_type = if let Some(new_t) = self.substitute(return_type) {
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
            ASTType::Generic(_, p, var_types) => {
                if self.contains_key(p, var_types) {
                    self.get(p, var_types).cloned()
                } else {
                    None
                }
            }
            ASTType::Custom {
                name,
                param_types,
                position,
            } => {
                self.substitute_types(param_types).map(|new_param_types| {
                    // TODO calculating the new position it's a bit heuristic, and probably it's not needed
                    /*let new_index = if new_param_types.is_empty() {
                        position.clone()
                    } else if let Some(ASTType::Custom {
                        name: _,
                        param_types: _,
                        position: ast_index,
                    }) = new_param_types.last()
                    {
                        ast_index.mv_right(1)
                    } else {
                        position.clone()
                    };
                    */
                    ASTType::Custom {
                        name: name.clone(),
                        param_types: new_param_types,
                        position: position.clone(),
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

    fn substitute_types(&self, types: &[ASTType]) -> Option<Vec<ASTType>> {
        let mut something_substituted = false;
        let new_types = types
            .iter()
            .map(|it| {
                if let Some(new_t) = self.substitute(it) {
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
}
