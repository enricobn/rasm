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
use crate::parser::ast::{ASTNameSpace, ASTType, BuiltinTypeKind};
use crate::type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedStructDef, ASTTypedType, ASTTypedTypeDef, BuiltinTypedTypeKind,
};
use crate::utils::find_one;

pub trait TypeDefProvider {
    fn enums(&self) -> &[ASTTypedEnumDef];

    fn structs(&self) -> &[ASTTypedStructDef];

    fn types(&self) -> &[ASTTypedTypeDef];

    fn get_enum_def_by_name(&self, name: &str) -> Option<&ASTTypedEnumDef> {
        self.enums().iter().find(|it| it.name == name)
    }

    fn get_struct_def_by_name(&self, name: &str) -> Option<&ASTTypedStructDef> {
        self.structs().iter().find(|it| it.name == name)
    }

    fn get_type_def_by_name(&self, name: &str) -> Option<&ASTTypedTypeDef> {
        self.types().iter().find(|it| it.name == name)
    }

    fn get_enum_def_like_name(
        &self,
        namespace: &ASTNameSpace,
        name: &str,
    ) -> Option<&ASTTypedEnumDef> {
        find_one(self.enums().iter(), |it| {
            (it.modifiers.public || &it.namespace == namespace) && it.name.starts_with(name)
        })
    }

    fn get_struct_def_like_name(
        &self,
        namespace: &ASTNameSpace,
        name: &str,
    ) -> Option<&ASTTypedStructDef> {
        find_one(self.structs().iter(), |it| {
            (it.modifiers.public || &it.namespace == namespace) && it.name.starts_with(name)
        })
    }

    fn get_type_def_like_name(
        &self,
        namespace: &ASTNameSpace,
        name: &str,
    ) -> Option<&ASTTypedTypeDef> {
        find_one(self.types().iter(), |it| {
            (it.modifiers.public || &it.namespace == namespace) && it.name.starts_with(name)
        })
    }

    fn get_type_from_custom_typed_type(
        &self,
        typed_type_to_find: &ASTTypedType,
    ) -> Option<ASTType> {
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
            if let ASTType::Custom {
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
            if let ASTType::Custom {
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
                if let ASTType::Custom {
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

    fn get_ast_typed_type_from_ast_type(&self, ast_type: &ASTType) -> Option<ASTTypedType> {
        match ast_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::Bool => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Bool)),
                BuiltinTypeKind::Char => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Char)),
                BuiltinTypeKind::I32 => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::I32)),
                BuiltinTypeKind::F32 => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::F32)),
                BuiltinTypeKind::String => {
                    Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::String))
                }
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => todo!(),
            },
            ASTType::Generic(_) => None,
            ASTType::Custom {
                namespace,
                name,
                param_types,
                index,
            } => {
                if let Some(e) = find_one(self.enums().iter(), |it| {
                    it.ast_type.equals_excluding_namespace(ast_type)
                        && (it.modifiers.public || it.namespace == ast_type.namespace())
                }) {
                    Some(e.clone().ast_typed_type)
                } else if let Some(s) = find_one(self.structs().iter(), |it| {
                    it.ast_type.equals_excluding_namespace(ast_type)
                        && (it.modifiers.public || it.namespace == ast_type.namespace())
                }) {
                    Some(s.clone().ast_typed_type)
                } else {
                    find_one(self.types().iter(), |it| {
                        // println!("found type {it}");
                        it.ast_type.equals_excluding_namespace(ast_type)
                            && (it.modifiers.public || it.namespace == ast_type.namespace())
                    })
                    .map(|t| t.clone().ast_typed_type)
                }
            }
            ASTType::Unit => Some(ASTTypedType::Unit),
        }
    }

    fn get_typed_type_def_from_type_name(&self, type_to_find: &str) -> Option<ASTTypedTypeDef> {
        //println!("get_typed_type_def_from_type_name({type_to_find})");

        find_one(self.types().iter(), |it| {
            //println!("inside find_one {it}");
            match &it.ast_type {
                ASTType::Builtin(_) => false,
                ASTType::Generic(_) => false,
                ASTType::Custom {
                    namespace: ast_type_namespace,
                    name: _,
                    param_types: _,
                    index: _,
                } => {
                    it.original_name == type_to_find
                        && (it.modifiers.public || &it.namespace == ast_type_namespace)
                }
                ASTType::Unit => false,
            }
        })
        .cloned()
    }

    fn get_type_from_typed_type(&self, ast_typed_type: &ASTTypedType) -> Option<ASTType> {
        match ast_typed_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => Some(ASTType::Builtin(BuiltinTypeKind::String)),
                BuiltinTypedTypeKind::I32 => Some(ASTType::Builtin(BuiltinTypeKind::I32)),
                BuiltinTypedTypeKind::Bool => Some(ASTType::Builtin(BuiltinTypeKind::Bool)),
                BuiltinTypedTypeKind::Char => Some(ASTType::Builtin(BuiltinTypeKind::Char)),
                BuiltinTypedTypeKind::F32 => Some(ASTType::Builtin(BuiltinTypeKind::F32)),
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
                        Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters: v,
                            return_type: Box::new(rt),
                        }))
                    } else {
                        None
                    }
                }
            },
            ASTTypedType::Unit => Some(ASTType::Unit),
            _ => self.get_type_from_custom_typed_type(ast_typed_type),
        }
    }

    fn get_type_from_typed_type_name(&self, typed_type_to_find: &str) -> Option<ASTType> {
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
pub struct DummyTypeDefProvider {}

impl TypeDefProvider for DummyTypeDefProvider {
    fn enums(&self) -> &[ASTTypedEnumDef] {
        &[]
    }

    fn structs(&self) -> &[ASTTypedStructDef] {
        &[]
    }

    fn types(&self) -> &[ASTTypedTypeDef] {
        &[]
    }

    fn name(&self) -> String {
        "DummyTypeDefProvider".to_owned()
    }
}

impl DummyTypeDefProvider {
    pub fn new() -> Self {
        Self {}
    }
}
