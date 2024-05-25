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

use crate::codegen::statics::Statics;
use crate::codegen::{get_reference_type_name, CodeGen};
use crate::transformations::typed_functions_creator::{
    enum_has_references, struct_has_references, type_has_references, TypedFunctionsCreator,
};
use crate::type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionDef, ASTTypedModule, ASTTypedStructDef, ASTTypedTypeDef,
};

use super::code_gen_c::CodeGenC;

pub struct TypedFunctionsCreatorC {
    code_gen: CodeGenC,
}

impl TypedFunctionsCreatorC {
    pub fn new(code_gen: CodeGenC) -> Self {
        Self { code_gen }
    }
}

impl TypedFunctionsCreator for TypedFunctionsCreatorC {
    fn create_struct_free_body(
        &self,
        struct_def: &ASTTypedStructDef,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String {
        let mut body = String::new();

        for property in struct_def.properties.iter() {
            if let Some(type_name) = get_reference_type_name(&property.ast_type, module) {
                let source = format!(
                    "({}) address->{}",
                    CodeGenC::type_to_string(&property.ast_type, statics),
                    property.name
                );
                if function_name == "deref" {
                    self.code_gen
                        .call_deref(&mut body, &source, &type_name, "", module, statics);
                } else {
                    self.code_gen
                        .call_add_ref(&mut body, &source, &type_name, "", module, statics);
                }
            }
        }

        let source = format!(
            "({}) address",
            CodeGenC::type_to_string(&struct_def.ast_typed_type, statics)
        );

        if function_name == "deref" {
            self.code_gen
                .call_deref_simple(&mut body, &source, "", statics);
        } else {
            self.code_gen
                .call_add_ref_simple(&mut body, &source, "", statics);
        }

        body
    }

    fn create_enum_free_body(
        &self,
        enum_def: &ASTTypedEnumDef,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String {
        let mut body = String::new();

        for (i, variant) in enum_def.variants.iter().enumerate() {
            self.code_gen.add(
                &mut body,
                &format!("if (address->variant_num == {i}) {{"),
                None,
                true,
            );
            for parameter in &variant.parameters {
                if let Some(type_name) = get_reference_type_name(&parameter.ast_type, module) {
                    let source = format!(
                        "({}) address->variant",
                        CodeGenC::type_to_string(&parameter.ast_type, statics)
                    );
                    if function_name == "deref" {
                        self.code_gen
                            .call_deref(&mut body, &source, &type_name, "", module, statics);
                    } else {
                        self.code_gen
                            .call_add_ref(&mut body, &source, &type_name, "", module, statics);
                    }
                }
            }
            self.code_gen.add(&mut body, &format!("}}"), None, true);
        }

        let source = format!(
            "({}) address",
            CodeGenC::type_to_string(&enum_def.ast_typed_type, statics)
        );

        if function_name == "deref" {
            self.code_gen
                .call_deref_simple(&mut body, &source, "", statics);
        } else {
            self.code_gen
                .call_add_ref_simple(&mut body, &source, "", statics);
        }

        body
    }

    fn create_type_free_body(
        &self,
        type_def: &ASTTypedTypeDef,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String {
        String::new()
    }
}
