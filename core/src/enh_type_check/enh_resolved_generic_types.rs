/*
 *     RASM compiler.
 *     Copyright (C) 2022-2023  Enrico Benedetti
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

use std::{
    fmt::{Display, Formatter},
    iter::zip,
};

use linked_hash_map::LinkedHashMap;
use rasm_parser::parser::ast::ASTType;
use rasm_utils::{debug_i, dedent, indent, SliceDisplay};

use crate::{
    codegen::{
        enh_ast::{EnhASTIndex, EnhASTType, EnhBuiltinTypeKind},
        enhanced_module::EnhancedASTModule,
    },
    enh_type_check::enh_functions_container::EnhTypeFilter,
};

use super::enh_type_check_error::EnhTypeCheckError;

#[derive(Debug, Clone, PartialEq)]
pub struct EnhResolvedGenericTypes {
    map: LinkedHashMap<String, LinkedHashMap<Vec<EnhASTType>, EnhASTType>>,
}

impl EnhResolvedGenericTypes {
    pub fn new() -> Self {
        Self {
            map: LinkedHashMap::new(),
        }
    }

    pub fn get(&self, key: &str, var_types: &Vec<EnhASTType>) -> Option<&EnhASTType> {
        self.map.get(key).and_then(|it| it.get(var_types))
    }

    pub fn resolve_generic_types_from_effective_type(
        generic_type: &EnhASTType,
        effective_type: &EnhASTType,
        enhanced_astmodule: &EnhancedASTModule,
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
                    EnhBuiltinTypeKind::Integer => {}
                    EnhBuiltinTypeKind::Boolean => {}
                    EnhBuiltinTypeKind::Char => {}
                    EnhBuiltinTypeKind::Float => {}
                    EnhBuiltinTypeKind::Lambda {
                        parameters: p_parameters,
                        return_type: p_return_type,
                    } => match effective_type {
                        EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                            parameters: e_parameters,
                            return_type: e_return_type,
                        }) => {
                            if e_parameters.len() != p_parameters.len() {
                                return Err(type_check_error(
                                    "Invalid parameters count.".to_string(),
                                ));
                            }
                            for (i, p_p) in p_parameters.iter().enumerate() {
                                let e_p = e_parameters.get(i).unwrap();

                                let inner_result = Self::resolve_generic_types_from_effective_type(p_p, e_p, enhanced_astmodule)
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
                            let inner_result = Self::resolve_generic_types_from_effective_type(p_return_type, e_return_type, enhanced_astmodule)
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
            EnhASTType::Generic(_, gen_type_gen_name, var_types) => {
                let ignore = if let EnhASTType::Generic(_, eff_type_gen_name, var_types_1) =
                    effective_type
                {
                    gen_type_gen_name == eff_type_gen_name && var_types == var_types_1
                } else {
                    false
                };
                if !ignore {
                    debug_i!("resolved generic type {gen_type_gen_name} to {effective_type}");

                    if let EnhASTType::Custom {
                        namespace: _,
                        name: _,
                        param_types,
                        index: _,
                    } = effective_type
                    {
                        if zip(param_types, var_types).all(|(p, v)| {
                            EnhTypeFilter::Exact(p.clone())
                                .almost_equal(v, enhanced_astmodule)
                                .unwrap()
                        }) {
                            for (param_type, var_type) in zip(param_types, var_types) {
                                if let EnhASTType::Generic(_, _g_name, _g_var_types) = var_type {
                                    result
                                        .extend(Self::resolve_generic_types_from_effective_type(
                                            var_type,
                                            param_type,
                                            enhanced_astmodule,
                                        )?)
                                        .map_err(|it| type_check_error(it.to_owned()))?;
                                }
                            }

                            result.insert(
                                gen_type_gen_name.clone(),
                                var_types.clone(),
                                effective_type.clone(),
                            );
                        }
                    } else {
                        result.insert(
                            gen_type_gen_name.clone(),
                            var_types.clone(),
                            effective_type.clone(),
                        );
                    }
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
                        let inner_result = Self::resolve_generic_types_from_effective_type(p_p, e_p, enhanced_astmodule)
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
                EnhASTType::Generic(_, _, _) => {}
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

    pub fn extend(&mut self, other: EnhResolvedGenericTypes) -> Result<(), String> {
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

    /*
    pub fn check(&self, other: &EnhResolvedGenericTypes) -> Option<String> {
        for (key, t) in other.iter() {
            if let Some(et) = self.get(key) {
                if t != et {
                    return Some(format!("Incompatible types {t} {et}"));
                }
            }
        }
        None
    }
    */

    pub fn insert(
        &mut self,
        key: String,
        var_types: Vec<EnhASTType>,
        value: EnhASTType,
    ) -> Option<EnhASTType> {
        let new = self.map.entry(key).or_insert(LinkedHashMap::new());
        if let Some(t) = new.get(&var_types) {
            assert_eq!(t, &value);
        }

        new.insert(var_types, value)
    }

    pub fn iter(&self) -> impl Iterator<Item = ((String, Vec<EnhASTType>), &EnhASTType)> {
        self.map.iter().flat_map(|(key, inner_map)| {
            inner_map
                .iter()
                .map(move |(vec_key, val)| ((key.clone(), vec_key.clone()), val))
        })
    }

    pub fn into_iter(self) -> impl IntoIterator<Item = ((String, Vec<EnhASTType>), EnhASTType)> {
        self.map.into_iter().flat_map(|(key, inner_map)| {
            inner_map
                .into_iter()
                .map(move |(vec_key, val)| ((key.clone(), vec_key.clone()), val))
        })
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn contains_key(&self, key: &str, var_types: &Vec<EnhASTType>) -> bool {
        self.map
            .get(key)
            .map(|it| it.contains_key(var_types))
            .unwrap_or(false)
    }

    /*
    pub fn keys(&self) -> Vec<String> {
        self.map.keys().cloned().collect()
    }
    */

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        let mut new = LinkedHashMap::new();

        for (name, inner) in self.map.into_iter() {
            let inner_new = new.entry(name).or_insert(LinkedHashMap::new());

            for (var_types, t) in inner.into_iter() {
                inner_new
                    .entry(
                        var_types
                            .into_iter()
                            .map(|it| it.fix_namespaces(enhanced_module))
                            .collect(),
                    )
                    .or_insert(t.fix_namespaces(enhanced_module));
            }
        }
        let result = EnhResolvedGenericTypes { map: new };

        result
    }

    pub fn fix_generics(self, generics_prefix: &dyn Display) -> Self {
        let mut new = LinkedHashMap::new();

        for (name, inner) in self.map.into_iter() {
            let inner_new = new.entry(name).or_insert(LinkedHashMap::new());

            for (var_types, t) in inner.into_iter() {
                inner_new
                    .entry(
                        var_types
                            .into_iter()
                            .map(|it| it.fix_generics(generics_prefix))
                            .collect(),
                    )
                    .or_insert(t.fix_generics(generics_prefix));
            }
        }
        let result = EnhResolvedGenericTypes { map: new };

        result
    }

    pub fn remove_generics_prefix(self) -> Self {
        let mut new = LinkedHashMap::new();

        for (name, inner) in self.map.into_iter() {
            let inner_new = new
                .entry(ASTType::get_original_generic(&name).unwrap().to_owned())
                .or_insert(LinkedHashMap::new());

            for (var_types, t) in inner.into_iter() {
                inner_new
                    .entry(
                        var_types
                            .into_iter()
                            .map(|it| it.remove_generics_prefix())
                            .collect(),
                    )
                    .or_insert(t.remove_generics_prefix());
            }
        }
        EnhResolvedGenericTypes { map: new }
    }
}

impl Display for EnhResolvedGenericTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (name, inner) in self.map.iter() {
            for (var_types, t) in inner.iter() {
                write!(f, "{name}<{}>={t},", SliceDisplay(&var_types))?;
            }
        }
        Ok(())
    }
}

fn type_check_error(message: String) -> EnhTypeCheckError {
    EnhTypeCheckError::new(EnhASTIndex::none(), message, Vec::new())
}

#[cfg(test)]
mod tests {
    use crate::{
        codegen::{
            enh_ast::{EnhASTIndex, EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind},
            enhanced_module::EnhancedASTModule,
        },
        enh_type_check::{
            enh_resolved_generic_types::EnhResolvedGenericTypes,
            enh_type_check_error::EnhTypeCheckError,
        },
    };

    #[test]
    fn test_extract_generic_types_from_effective_type_simple() -> Result<(), EnhTypeCheckError> {
        let generic_type = generic("T");
        let effective_type = i32();
        let result = EnhResolvedGenericTypes::resolve_generic_types_from_effective_type(
            &generic_type,
            &effective_type,
            &EnhancedASTModule::empty(),
        )?;

        let mut expected_result = EnhResolvedGenericTypes::new();
        expected_result.insert("T".into(), Vec::new(), i32());

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

        let result = EnhResolvedGenericTypes::resolve_generic_types_from_effective_type(
            &generic_type,
            &effective_type,
            &EnhancedASTModule::empty(),
        )?;

        let mut expected_result = EnhResolvedGenericTypes::new();
        expected_result.insert("T".into(), Vec::new(), i32());

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

        let result = EnhResolvedGenericTypes::resolve_generic_types_from_effective_type(
            &generic_type,
            &effective_type,
            &EnhancedASTModule::empty(),
        )?;

        let mut expected_result = EnhResolvedGenericTypes::new();
        expected_result.insert("T".into(), Vec::new(), i32());

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

        let result = EnhResolvedGenericTypes::resolve_generic_types_from_effective_type(
            &generic_type,
            &effective_type,
            &EnhancedASTModule::empty(),
        )?;

        let mut expected_result = EnhResolvedGenericTypes::new();
        expected_result.insert("T".into(), Vec::new(), i32());

        assert_eq!(result, expected_result);
        Ok(())
    }

    fn generic(name: &str) -> EnhASTType {
        EnhASTType::Generic(EnhASTIndex::none(), name.into(), Vec::new())
    }

    fn i32() -> EnhASTType {
        EnhASTType::Builtin(EnhBuiltinTypeKind::Integer)
    }
}
