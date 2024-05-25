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

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::codegen::lambda::LambdaSpace;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_reference_type_name, CodeGen};
use crate::parser::ast::ASTNameSpace;
use crate::transformations::typed_functions_creator::TypedFunctionsCreator;
use crate::type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule, ASTTypedStructDef,
    ASTTypedTypeDef,
};

use super::code_gen_c::CodeGenC;

static REF_FUNCTIONS_ID: AtomicUsize = AtomicUsize::new(0);

pub struct TypedFunctionsCreatorC {
    code_gen: CodeGenC,
}

impl TypedFunctionsCreatorC {
    pub fn new(code_gen: CodeGenC) -> Self {
        Self { code_gen }
    }

    fn create_lambda_free_body(
        &self,
        c_lambda_name: &str,
        function_name: &str,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &Statics,
    ) -> String {
        let mut body = String::new();

        let lambda_type = format!("struct {}*", c_lambda_name);
        self.code_gen.add(
            &mut body,
            &format!("{lambda_type} lambda_ = ({lambda_type}) address;"),
            None,
            true,
        );

        for (i, (name, kind)) in lambda_space.iter().enumerate() {
            let t = kind.typed_type();

            if let Some(type_name) = get_reference_type_name(t, type_def_provider) {
                let source = format!(
                    "({}) lambda_->args[{i}]",
                    CodeGenC::type_to_string(t, statics)
                );
                if function_name == "deref" {
                    self.code_gen.call_deref(
                        &mut body,
                        &source,
                        &type_name,
                        "",
                        type_def_provider,
                        statics,
                    );
                } else {
                    self.code_gen.call_add_ref(
                        &mut body,
                        &source,
                        &type_name,
                        "",
                        type_def_provider,
                        statics,
                    );
                }
            }
        }

        if function_name == "deref" {
            self.code_gen
                .call_deref_simple(&mut body, "lambda_", "", statics);
        } else {
            self.code_gen
                .call_add_ref_simple(&mut body, "lambda_", "", statics);
        }

        body
    }

    pub fn create_lambda_free(
        &self,
        c_lambda_name: &str,
        lambda_space: &LambdaSpace,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &Statics,
    ) -> ASTTypedFunctionDef {
        let body_str = self.create_lambda_free_body(
            c_lambda_name,
            function_name,
            lambda_space,
            module,
            statics,
        );
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        let fun_name = format!(
            "lambda_{function_name}_{}",
            REF_FUNCTIONS_ID.fetch_add(1, Ordering::SeqCst)
        );

        self.create_function(
            &ASTNameSpace::global(),
            &fun_name,
            crate::type_check::typed_ast::ASTTypedType::Type {
                namespace: ASTNameSpace::global(),
                name: "CTargetLambda".to_string(),
                native_type: Some("void **".to_string()),
            },
            body,
            false,
        )
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
