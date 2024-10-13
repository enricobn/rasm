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

use linked_hash_map::{IntoIter, Iter, LinkedHashMap};

use crate::{codegen::eh_ast::EnhASTType, codegen::enhanced_module::EnhancedASTModule};

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedGenericTypes {
    map: LinkedHashMap<String, EnhASTType>,
}

impl ResolvedGenericTypes {
    pub fn new() -> Self {
        Self {
            map: LinkedHashMap::new(),
        }
    }

    pub fn get(&self, key: &String) -> Option<&EnhASTType> {
        self.map.get(key)
    }

    pub fn extend(&mut self, other: ResolvedGenericTypes) -> Result<(), String> {
        for (key, new_type) in other.into_iter() {
            if let Some(prev_type) = self.get(&key) {
                if &new_type != prev_type && new_type.is_generic() {
                    return Err(format!(
                        "Already resolved generic {key}, prev {prev_type}, new {new_type}"
                    ));
                }
            }
            self.map.insert(key, new_type);
        }
        Ok(())
    }

    pub fn check(&self, other: &ResolvedGenericTypes) -> Option<String> {
        for (key, t) in other.iter() {
            if let Some(et) = self.get(key) {
                if t != et {
                    return Some(format!("Incompatible types {t} {et}"));
                }
            }
        }
        None
    }

    pub fn insert(&mut self, key: String, value: EnhASTType) -> Option<EnhASTType> {
        if let Some(t) = self.map.get(&key) {
            assert_eq!(t, &value);
        }
        self.map.insert(key, value)
    }

    pub fn iter(&self) -> Iter<String, EnhASTType> {
        self.map.iter()
    }

    pub fn into_iter(self) -> IntoIter<String, EnhASTType> {
        self.map.into_iter()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn contains_key(&self, key: &String) -> bool {
        self.map.contains_key(key)
    }

    pub fn keys(&self) -> Vec<String> {
        self.map.keys().cloned().collect()
    }

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        Self {
            map: self
                .map
                .into_iter()
                .map(|(key, t)| (key, t.fix_namespaces(enhanced_module)))
                .collect(),
        }
    }

    pub fn fix_generics(self, generics_prefix: &dyn Display) -> Self {
        Self {
            map: self
                .map
                .into_iter()
                .map(|(key, t)| (key, t.fix_generics(generics_prefix)))
                .collect(),
        }
    }
}

impl Display for ResolvedGenericTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = self
            .iter()
            .map(|it| format!("{}={}", it.0, it.1))
            .collect::<Vec<_>>()
            .join(",");
        f.write_str(&s)
    }
}
