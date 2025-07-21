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
use log::debug;
use rasm_utils::{find_one, SliceDisplay};
use std::iter::zip;

use crate::codegen::enh_ast::{EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind};
use crate::enh_type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedStructDef, ASTTypedType, ASTTypedTypeDef, BuiltinTypedTypeKind,
    CustomTypedTypeDef,
};
use rasm_parser::parser::ast::ASTModifiers;

pub trait TypeDefProvider {
    fn enums(&self) -> &[ASTTypedEnumDef];

    fn structs(&self) -> &[ASTTypedStructDef];

    fn types(&self) -> &[ASTTypedTypeDef];

    fn get_real_namespace(&self, ast_type: &EnhASTType) -> Option<EnhASTNameSpace> {
        if let EnhASTType::Custom {
            namespace: _,
            name,
            param_types: _,
            index: _,
        } = ast_type
        {
            let enums = self
                .enums()
                .iter()
                .filter(|it| {
                    it.custom_ast_type_name()
                        .map(|n| &n == name)
                        .unwrap_or(false)
                        && (it.modifiers.public || &it.namespace == ast_type.namespace())
                })
                .map(|it| it.namespace().clone())
                .collect::<Vec<_>>();

            let structs = self
                .structs()
                .iter()
                .filter(|it| {
                    it.custom_ast_type_name()
                        .map(|n| &n == name)
                        .unwrap_or(false)
                        && (it.modifiers.public || &it.namespace == ast_type.namespace())
                })
                .map(|it| it.namespace().clone())
                .collect::<Vec<_>>();

            let types = self
                .types()
                .iter()
                .filter(|it| {
                    it.custom_ast_type_name()
                        .map(|n| &n == name)
                        .unwrap_or(false)
                        && (it.modifiers.public || &it.namespace == ast_type.namespace())
                })
                .map(|it| it.namespace().clone())
                .collect::<Vec<_>>();

            let mut all = Vec::new();
            all.extend(enums);
            all.extend(structs);
            all.extend(types);

            all.sort();
            all.dedup();

            if all.len() == 1 {
                all.first().cloned()
            } else {
                debug!("Not found custom type {name} {}", SliceDisplay(&all));
                None
            }
        } else {
            None
        }
    }

    fn get_enum_def_by_name(&self, name: &str) -> Option<&ASTTypedEnumDef> {
        self.enums().iter().find(|it| it.name == name)
    }

    fn get_struct_def_by_name(&self, name: &str) -> Option<&ASTTypedStructDef> {
        self.structs().iter().find(|it| it.name == name)
    }

    fn get_type_def_by_name(&self, name: &str) -> Option<&ASTTypedTypeDef> {
        self.types().iter().find(|it| it.name == name)
    }

    fn get_modifier_by_typed_type(&self, typed_type: &ASTTypedType) -> Option<&ASTModifiers> {
        match typed_type {
            ASTTypedType::Builtin(_) => None,
            ASTTypedType::Enum { namespace: _, name } => {
                Some(&self.get_enum_def_by_name(name).unwrap().modifiers)
            }
            ASTTypedType::Struct { namespace: _, name } => {
                Some(&self.get_struct_def_by_name(name).unwrap().modifiers)
            }
            ASTTypedType::Type {
                namespace: _,
                name,
                body: _,
            } => Some(&self.get_type_def_by_name(name).unwrap().modifiers),
            ASTTypedType::Unit => None,
        }
    }

    fn get_enum_def_like_name(
        &self,
        namespace: &EnhASTNameSpace,
        name: &str,
    ) -> Option<&ASTTypedEnumDef> {
        find_one(self.enums().iter(), |it| {
            (it.modifiers.public || &it.namespace == namespace) && it.name.starts_with(name)
        })
    }

    fn get_struct_def_like_name(
        &self,
        namespace: &EnhASTNameSpace,
        name: &str,
    ) -> Option<&ASTTypedStructDef> {
        find_one(self.structs().iter(), |it| {
            (it.modifiers.public || &it.namespace == namespace) && it.name.starts_with(name)
        })
    }

    fn get_type_def_like_name(
        &self,
        namespace: &EnhASTNameSpace,
        name: &str,
    ) -> Option<&ASTTypedTypeDef> {
        find_one(self.types().iter(), |it| {
            (it.modifiers.public || &it.namespace == namespace) && it.name.starts_with(name)
        })
    }

    fn get_type_from_custom_typed_type(
        &self,
        typed_type_to_find: &ASTTypedType,
    ) -> Option<EnhASTType> {
        if let Some(e) = find_one(self.enums().iter(), |it| {
            &it.ast_typed_type == typed_type_to_find
        }) {
            Some(e.clone().ast_type)
        } else if let Some(s) = find_one(self.structs().iter(), |it| {
            &it.ast_typed_type == typed_type_to_find
        }) {
            Some(s.clone().ast_type)
        } else {
            let result = find_one(self.types().iter(), |it| {
                &it.ast_typed_type == typed_type_to_find
            })
            .map(|t| t.clone().ast_type);

            result
        }
    }

    fn get_ast_typed_type_from_type_name(&self, name: &str) -> Option<ASTTypedType> {
        if let Some(e) = find_one(self.enums().iter(), |it| {
            if let EnhASTType::Custom {
                namespace,
                name: ast_type_name,
                param_types: _,
                index: _,
            } = &it.ast_type
            {
                (it.modifiers.public || &it.namespace == namespace) && ast_type_name == name
            } else {
                panic!()
            }
        }) {
            Some(e.clone().ast_typed_type)
        } else if let Some(s) = find_one(self.structs().iter(), |it| {
            if let EnhASTType::Custom {
                namespace,
                name: ast_type_name,
                param_types: _,
                index: _,
            } = &it.ast_type
            {
                (it.modifiers.public || &it.namespace == namespace) && ast_type_name == name
            } else {
                panic!()
            }
        }) {
            Some(s.clone().ast_typed_type)
        } else {
            find_one(self.types().iter(), |it| {
                if let EnhASTType::Custom {
                    namespace: _,
                    name: ast_type_name,
                    param_types: _,
                    index: _,
                } = &it.ast_type
                {
                    ast_type_name == name
                } else {
                    panic!()
                }
            })
            .map(|t| t.clone().ast_typed_type)
        }
    }

    fn get_ast_typed_type_from_ast_type(&self, ast_type: &EnhASTType) -> Option<ASTTypedType> {
        let result = match ast_type {
            EnhASTType::Builtin(kind) => match kind {
                EnhBuiltinTypeKind::Bool => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Bool)),
                EnhBuiltinTypeKind::Char => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Char)),
                EnhBuiltinTypeKind::I32 => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::I32)),
                EnhBuiltinTypeKind::F32 => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::F32)),
                EnhBuiltinTypeKind::String => {
                    Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::String))
                }
                EnhBuiltinTypeKind::Lambda {
                    parameters: _,
                    return_type: _,
                } => {
                    None
                    /*
                    let new_parameters = parameters
                        .iter()
                        .map(|it| self.get_ast_typed_type_from_ast_type(it).unwrap())
                        .collect_vec();
                    let new_return_type = self
                        .get_ast_typed_type_from_ast_type(&return_type)
                        .expect(&format!("Cannot find typed type for {return_type}"));
                    Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                        parameters: new_parameters,
                        return_type: Box::new(new_return_type),
                    }))
                    */
                }
            },
            EnhASTType::Generic(_, _, _) => None,
            EnhASTType::Custom {
                namespace: _,
                name: _,
                param_types,
                index: _,
            } => {
                /*
                if FOUND_THE_FUNCTION.load(std::sync::atomic::Ordering::Relaxed) {
                    println!("{ast_type} is custom {}", self.types().len());
                    for t in self.types().iter() {
                        println!("{} -> {}", t, t.ast_type);
                    }
                }
                */

                if let Some(e) = find_one(self.enums().iter(), |it| {
                    self.get_ast_typed_type_from_ast_type_filter(*it, ast_type, param_types)
                }) {
                    Some(e.clone().ast_typed_type)
                } else if let Some(s) = find_one(self.structs().iter(), |it| {
                    self.get_ast_typed_type_from_ast_type_filter(*it, ast_type, param_types)
                }) {
                    Some(s.clone().ast_typed_type)
                } else {
                    find_one(self.types().iter(), |it| {
                        self.get_ast_typed_type_from_ast_type_filter(*it, ast_type, param_types)
                    })
                    .map(|it| it.ast_typed_type().clone())
                }
            }
            EnhASTType::Unit => Some(ASTTypedType::Unit),
        };

        result
    }

    fn get_typed_type_def_from_type_name(&self, type_to_find: &str) -> Option<ASTTypedTypeDef> {
        //println!("get_typed_type_def_from_type_name({type_to_find})");

        find_one(self.types().iter(), |it| {
            //println!("inside find_one {it}");
            match &it.ast_type {
                EnhASTType::Builtin(_) => false,
                EnhASTType::Generic(_, _, _) => false,
                EnhASTType::Custom {
                    namespace: ast_type_namespace,
                    name: _,
                    param_types: _,
                    index: _,
                } => {
                    it.original_name == type_to_find
                        && (it.modifiers.public || &it.namespace == ast_type_namespace)
                }
                EnhASTType::Unit => false,
            }
        })
        .cloned()
    }

    fn get_type_from_typed_type(&self, ast_typed_type: &ASTTypedType) -> Option<EnhASTType> {
        match ast_typed_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => {
                    Some(EnhASTType::Builtin(EnhBuiltinTypeKind::String))
                }
                BuiltinTypedTypeKind::I32 => Some(EnhASTType::Builtin(EnhBuiltinTypeKind::I32)),
                BuiltinTypedTypeKind::Bool => Some(EnhASTType::Builtin(EnhBuiltinTypeKind::Bool)),
                BuiltinTypedTypeKind::Char => Some(EnhASTType::Builtin(EnhBuiltinTypeKind::Char)),
                BuiltinTypedTypeKind::F32 => Some(EnhASTType::Builtin(EnhBuiltinTypeKind::F32)),
                BuiltinTypedTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let vec = parameters
                        .iter()
                        .map(|it| self.get_type_from_typed_type(it))
                        .collect::<Option<Vec<_>>>();
                    let o_return_type = self.get_type_from_typed_type(return_type);
                    if let (Some(v), Some(rt)) = (vec, o_return_type) {
                        Some(EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                            parameters: v,
                            return_type: Box::new(rt),
                        }))
                    } else {
                        None
                    }
                }
            },
            ASTTypedType::Unit => Some(EnhASTType::Unit),
            _ => self.get_type_from_custom_typed_type(ast_typed_type),
        }
    }

    fn get_ast_typed_type_from_ast_type_filter(
        &self,
        custom_typed_type_def: &dyn CustomTypedTypeDef,
        ast_type: &EnhASTType,
        param_types: &Vec<EnhASTType>,
    ) -> bool {
        if let EnhASTType::Custom {
            namespace: _,
            name: _,
            param_types: it_pt,
            index: _,
        } = custom_typed_type_def.ast_type()
        {
            if custom_typed_type_def.ast_type() == ast_type {
                zip(it_pt.iter(), param_types.iter()).all(|(a, b)| a == b)
            } else {
                false
            }
        } else {
            false
        }
    }

    fn get_type_from_typed_type_name(&self, typed_type_to_find: &str) -> Option<EnhASTType> {
        if let Some(t) = self.get_enum_def_by_name(typed_type_to_find) {
            Some(t.ast_type.clone())
        } else if let Some(t) = self.get_struct_def_by_name(typed_type_to_find) {
            Some(t.ast_type.clone())
        } else {
            self.get_type_def_by_name(typed_type_to_find)
                .map(|t| t.ast_type.clone())
        }
    }

    fn name(&self) -> String;
}

#[derive(Debug)]
pub struct DummyTypeDefProvider {
    enums: Vec<ASTTypedEnumDef>,
    structs: Vec<ASTTypedStructDef>,
    types: Vec<ASTTypedTypeDef>,
}

impl TypeDefProvider for DummyTypeDefProvider {
    fn enums(&self) -> &[ASTTypedEnumDef] {
        &self.enums
    }

    fn structs(&self) -> &[ASTTypedStructDef] {
        &self.structs
    }

    fn types(&self) -> &[ASTTypedTypeDef] {
        &self.types
    }

    fn name(&self) -> String {
        "DummyTypeDefProvider".to_owned()
    }
}

impl DummyTypeDefProvider {
    pub fn new(
        enums: Vec<ASTTypedEnumDef>,
        structs: Vec<ASTTypedStructDef>,
        types: Vec<ASTTypedTypeDef>,
    ) -> Self {
        Self {
            enums,
            structs,
            types,
        }
    }

    pub fn empty() -> Self {
        Self {
            enums: Vec::new(),
            structs: Vec::new(),
            types: Vec::new(),
        }
    }
}
