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
use crate::parser::ast::{ASTType, BuiltinTypeKind};
use crate::type_check::typed_ast::{
    get_type_of_typed_expression, ASTTypedEnumDef, ASTTypedStructDef, ASTTypedType,
    ASTTypedTypeDef, BuiltinTypedTypeKind,
};

pub trait TypeDefProvider {
    fn get_enum_def_by_name(&self, name: &str) -> Option<&ASTTypedEnumDef>;
    fn get_struct_def_by_name(&self, name: &str) -> Option<&ASTTypedStructDef>;
    fn get_type_def_by_name(&self, name: &str) -> Option<&ASTTypedTypeDef>;
    fn get_enum_def_like_name(&self, name: &str) -> Option<&ASTTypedEnumDef>;
    fn get_struct_def_like_name(&self, name: &str) -> Option<&ASTTypedStructDef>;
    fn get_type_def_like_name(&self, name: &str) -> Option<&ASTTypedTypeDef>;
    fn get_type_from_custom_typed_type(&self, typed_type_to_find: &ASTTypedType)
        -> Option<ASTType>;

    fn get_type_from_typed_type(&self, ast_typed_type: &ASTTypedType) -> Option<ASTType> {
        match ast_typed_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => Some(ASTType::Builtin(BuiltinTypeKind::String)),
                BuiltinTypedTypeKind::I32 => Some(ASTType::Builtin(BuiltinTypeKind::I32)),
                BuiltinTypedTypeKind::Bool => Some(ASTType::Builtin(BuiltinTypeKind::Bool)),
                BuiltinTypedTypeKind::Char => Some(ASTType::Builtin(BuiltinTypeKind::Char)),
                BuiltinTypedTypeKind::F32 => Some(ASTType::Builtin(BuiltinTypeKind::F32)),
                BuiltinTypedTypeKind::Lambda { .. } => todo!(),
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

    fn get_ast_typed_type_from_type_name(&self, name: &str) -> Option<ASTTypedType>;
    fn get_ast_typed_type_from_ast_type(&self, ast_type: &ASTType) -> Option<ASTTypedType>;

    fn get_typed_type_def_from_type_name(&self, type_to_find: &str) -> Option<ASTTypedTypeDef>;

    fn name(&self) -> String;
}

#[derive(Debug)]
pub struct DummyTypeDefProvider {}

impl TypeDefProvider for DummyTypeDefProvider {
    fn get_enum_def_by_name(&self, _name: &str) -> Option<&ASTTypedEnumDef> {
        None
    }

    fn get_struct_def_by_name(&self, _name: &str) -> Option<&ASTTypedStructDef> {
        None
    }

    fn get_type_def_by_name(&self, _name: &str) -> Option<&ASTTypedTypeDef> {
        None
    }

    fn get_enum_def_like_name(&self, _name: &str) -> Option<&ASTTypedEnumDef> {
        None
    }

    fn get_struct_def_like_name(&self, _name: &str) -> Option<&ASTTypedStructDef> {
        None
    }

    fn get_type_def_like_name(&self, _name: &str) -> Option<&ASTTypedTypeDef> {
        None
    }

    fn get_type_from_custom_typed_type(
        &self,
        _typed_type_to_find: &ASTTypedType,
    ) -> Option<ASTType> {
        None
    }

    fn get_ast_typed_type_from_type_name(&self, _name: &str) -> Option<ASTTypedType> {
        None
    }

    fn get_ast_typed_type_from_ast_type(&self, _ast_type: &ASTType) -> Option<ASTTypedType> {
        None
    }

    fn get_typed_type_def_from_type_name(&self, _type_to_find: &str) -> Option<ASTTypedTypeDef> {
        None
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
