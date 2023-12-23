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
use crate::utils::{find_one, SliceDisplay};

pub trait TypeDefProvider {
    fn enums(&self) -> &[ASTTypedEnumDef];

    fn structs(&self) -> &[ASTTypedStructDef];

    fn types(&self) -> &[ASTTypedTypeDef];

    fn get_enum_def_by_name(
        &self,
        namespace: &ASTNameSpace,
        name: &str,
    ) -> Option<&ASTTypedEnumDef> {
        self.enums()
            .iter()
            .find(|it| (it.modifiers.public || &it.namespace == namespace) && it.name == name)
    }

    fn get_struct_def_by_name(
        &self,
        namespace: &ASTNameSpace,
        name: &str,
    ) -> Option<&ASTTypedStructDef> {
        self.structs()
            .iter()
            .find(|it| (it.modifiers.public || &it.namespace == namespace) && it.name == name)
    }

    fn get_type_def_by_name(
        &self,
        namespace: &ASTNameSpace,
        name: &str,
    ) -> Option<&ASTTypedTypeDef> {
        self.types()
            .iter()
            .find(|it| (it.modifiers.public || &it.namespace == namespace) && it.name == name)
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

    fn get_type_def_like_name(&self, name: &str) -> Option<&ASTTypedTypeDef> {
        find_one(self.types().iter(), |it| it.name == name)
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

            if result.is_none() {
                println!(
                    "{} enums: {}",
                    typed_type_to_find,
                    SliceDisplay(self.enums())
                )
            }

            result
        }
    }

    fn get_ast_typed_type_from_type_name(
        &self,
        namespace: &ASTNameSpace,
        name: &str,
    ) -> Option<ASTTypedType> {
        if let Some(e) = find_one(self.enums().iter(), |it| {
            if let ASTType::Custom {
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
        if let Some(e) = find_one(self.enums().iter(), |it| &it.ast_type == ast_type) {
            Some(e.clone().ast_typed_type)
        } else if let Some(s) = find_one(self.structs().iter(), |it| &it.ast_type == ast_type) {
            Some(s.clone().ast_typed_type)
        } else {
            find_one(self.types().iter(), |it| &it.ast_type == ast_type)
                .map(|t| t.clone().ast_typed_type)
        }
    }

    fn get_typed_type_def_from_type_name(&self, type_to_find: &str) -> Option<ASTTypedTypeDef> {
        find_one(self.types().iter(), |it| match &it.ast_type {
            ASTType::Builtin(_) => false,
            ASTType::Generic(_) => false,
            ASTType::Custom {
                name,
                param_types: _,
                index: _,
            } => name == type_to_find,
            ASTType::Unit => false,
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

    fn get_type_from_typed_type_name(
        &self,
        namespace: &ASTNameSpace,
        typed_type_to_find: &str,
    ) -> Option<ASTType> {
        if let Some(t) = self.get_enum_def_by_name(namespace, typed_type_to_find) {
            Some(t.ast_type.clone())
        } else if let Some(t) = self.get_struct_def_by_name(namespace, typed_type_to_find) {
            Some(t.ast_type.clone())
        } else {
            self.get_type_def_by_name(namespace, typed_type_to_find)
                .map(|t| t.ast_type.clone())
        }
    }

    fn get_namespace_from_typed_type_name(
        &self,
        namespace: &ASTNameSpace,
        typed_type_to_find: &str,
    ) -> Option<ASTNameSpace> {
        if let Some(t) = self.get_enum_def_by_name(namespace, typed_type_to_find) {
            Some(t.namespace.clone())
        } else if let Some(t) = self.get_struct_def_by_name(namespace, typed_type_to_find) {
            Some(t.namespace.clone())
        } else {
            self.get_type_def_by_name(namespace, typed_type_to_find)
                .map(|t| t.namespace.clone().clone())
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
