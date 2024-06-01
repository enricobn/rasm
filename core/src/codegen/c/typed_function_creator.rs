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
    ASTTypedType, ASTTypedTypeDef, BuiltinTypedTypeKind,
};

use super::any::{CLambda, CLambdas, CStructs};
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
        statics: &mut Statics,
    ) -> String {
        let mut body = String::new();

        let lambda_type = format!("struct {}*", c_lambda_name);
        self.code_gen.add(
            &mut body,
            &format!("{lambda_type} lambda_ = ({lambda_type}) address;"),
            None,
            true,
        );

        let lambda_space_type_name = CStructs::add_lambda_space_to_statics(statics, lambda_space);

        self.code_gen.add(
            &mut body,
            &format!("struct {lambda_space_type_name} *lambda_space = (struct {lambda_space_type_name} *) lambda_->lambda_space;"),
            None,
            true,
        );

        for (i, (name, kind)) in lambda_space.iter().enumerate() {
            let t = kind.typed_type();

            if let Some(type_name) = get_reference_type_name(t, type_def_provider) {
                let source = format!(
                    "({}) lambda_space->{name}",
                    CodeGenC::type_to_string(t, statics)
                );
                if function_name == "deref" {
                    self.code_gen.call_deref(
                        &mut body,
                        &source,
                        &type_name,
                        &type_name,
                        type_def_provider,
                        statics,
                    );
                } else {
                    self.code_gen.call_add_ref(
                        &mut body,
                        &source,
                        &type_name,
                        &type_name,
                        type_def_provider,
                        statics,
                    );
                }
            }
        }

        if function_name == "deref" {
            self.code_gen
                .call_deref_simple(&mut body, "lambda_space", c_lambda_name, statics);
            self.code_gen
                .call_deref_simple(&mut body, "lambda_", c_lambda_name, statics);
        } else {
            self.code_gen
                .call_add_ref_simple(&mut body, "lambda_space", c_lambda_name, statics);
            self.code_gen
                .call_add_ref_simple(&mut body, "lambda_", c_lambda_name, statics);
        }

        body
    }

    pub fn create_lambda_free(
        &self,
        c_lambda_name: &str,
        lambda_space: &LambdaSpace,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
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
                CLambdas::add_to_statics_if_lambda(&property.ast_type, statics);
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
                .call_deref_simple(&mut body, &source, &struct_def.name, statics);
        } else {
            self.code_gen
                .call_add_ref_simple(&mut body, &source, &struct_def.name, statics);
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
        let enum_type_name = format!("{}_{}", enum_def.namespace.safe_name(), enum_def.name);

        for (i, variant) in enum_def.variants.iter().enumerate() {
            self.code_gen.add(
                &mut body,
                &format!("if (address->variant_num == {i}) {{"),
                None,
                true,
            );

            let variant_type_name = format!("{enum_type_name}_{}", variant.name);
            self.code_gen.add(
                &mut body,
                &format!(
                    "struct {variant_type_name} *variant = (struct {variant_type_name}*) address->variant;"
                ),
                None,
                true,
            );

            for parameter in &variant.parameters {
                if let Some(type_name) = get_reference_type_name(&parameter.ast_type, module) {
                    CLambdas::add_to_statics_if_lambda(&parameter.ast_type, statics);
                    let source = format!(
                        "({}) variant->{}",
                        CodeGenC::type_to_string(&parameter.ast_type, statics),
                        parameter.name
                    );
                    if function_name == "deref" {
                        self.code_gen.call_deref(
                            &mut body, &source, &type_name, &type_name, module, statics,
                        );
                    } else {
                        self.code_gen.call_add_ref(
                            &mut body, &source, &type_name, &type_name, module, statics,
                        );
                    }
                }
            }
            if function_name == "deref" {
                self.code_gen
                    .call_deref_simple(&mut body, "variant", &variant_type_name, statics);
            } else {
                self.code_gen.call_add_ref_simple(
                    &mut body,
                    "variant",
                    &variant_type_name,
                    statics,
                );
            }
            self.code_gen.add(&mut body, &format!("}}"), None, true);
        }

        let source = format!(
            "({}) address",
            CodeGenC::type_to_string(&enum_def.ast_typed_type, statics)
        );

        if function_name == "deref" {
            self.code_gen
                .call_deref_simple(&mut body, &source, &enum_type_name, statics);
        } else {
            self.code_gen
                .call_add_ref_simple(&mut body, &source, &enum_type_name, statics);
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
        let mut body = String::new();

        if type_def.generic_types.iter().any(|(_, generic_type_def)| {
            get_reference_type_name(generic_type_def, module).is_some()
        }) {
            self.code_gen.add_rows(
                &mut body,
                vec!["int length;", "void ** array;", "void ** vec;"],
                None,
                true,
            );
            for (i, (_generic_name, generic_type_def)) in type_def.generic_types.iter().enumerate()
            {
                self.code_gen.add(&mut body, "vec = ", None, true);

                self.code_gen.call_function(
                    &mut body,
                    &format!("{}References", type_def.original_name),
                    &[("address", None), (&format!("{i}"), None)],
                    None,
                    false,
                    false,
                );
                // TODO the result of *References function is not freed, but for now only Vec in the std lib is a type with
                //   references and VecReferences returns itself, so it is freed at the end, probably better should be that it returns a copy of itself,
                //   but it's slower. However it's not a good design that types with references relies on a type (Vec), that is itself a custom
                //   type, but it's in an external library. And it's not a good design that, in the definition of a type, we must declare if it has
                //   references and the native type, it could have non meaning for some compiler / target, for example, for Nasm the native type
                //   is not used. We must find another solution for types in general and in particular for handling references for those compilers/ target
                //   that must manage memory

                self.code_gen.add_rows(
                    &mut body,
                    vec![
                        "length = *((int*)vec[0]);",
                        "array = ((void **)vec[1]);",
                        "for (int i = 0; i < length; i++) {",
                    ],
                    None,
                    true,
                );

                if let Some(name) = get_reference_type_name(generic_type_def, module) {
                    if function_name == "deref" {
                        self.code_gen
                            .call_deref(&mut body, "array[i]", &name, &name, module, statics);
                    } else {
                        self.code_gen
                            .call_add_ref(&mut body, "array[i]", &name, &name, module, statics);
                    }
                }
                self.code_gen.add(&mut body, "}", None, true);
            }
        }

        self.code_gen.add(
            &mut body,
            "void ** addressArray = (void **)address[1];",
            None,
            true,
        );

        if function_name == "deref" {
            self.code_gen
                .call_deref_simple(&mut body, "addressArray", &type_def.name, statics);
            self.code_gen
                .call_deref_simple(&mut body, "address", &type_def.name, statics);
        } else {
            self.code_gen
                .call_add_ref_simple(&mut body, "address", &type_def.name, statics);
            self.code_gen
                .call_add_ref_simple(&mut body, "addressArray", &type_def.name, statics);
        }

        body
    }
}
