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

use std::fmt::{Display, Formatter};

use linked_hash_map::LinkedHashMap;
use rasm_utils::SliceDisplay;

use crate::{codegen::enh_ast::EnhASTType, codegen::enhanced_module::EnhancedASTModule};

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

    pub fn get(&self, key: &String, var_types: &Vec<EnhASTType>) -> Option<&EnhASTType> {
        self.map.get(key).and_then(|it| it.get(var_types))
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
        /*
        for t in var_types.iter() {
            if let EnhASTType::Generic(_, g_name, g_var_types) = t {
                if let EnhASTType::Custom {
                    namespace,
                    name,
                    param_types,
                    index,
                } = &value
                {
                    if !param_types.is_empty()
                        && !matches!(param_types.get(0).unwrap(), EnhASTType::Generic(_, _, _))
                    {
                        println!(
                            "implicit resolution of generic type {g_name} with {}",
                            param_types.get(0).unwrap()
                        );
                        self.insert(
                            g_name.to_owned(),
                            Vec::new(),
                            param_types.get(0).unwrap().clone(),
                        );
                    }
                }
            }
        }
        */

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

    pub fn contains_key(&self, key: &String, var_types: &Vec<EnhASTType>) -> bool {
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
        EnhResolvedGenericTypes { map: new }
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
