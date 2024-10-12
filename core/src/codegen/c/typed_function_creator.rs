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

use linked_hash_map::LinkedHashMap;

use crate::codegen::eh_ast::{ASTFunctionBody, ASTNameSpace};
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_reference_type_name, CodeGen};
use crate::transformations::typed_functions_creator::TypedFunctionsCreator;
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedStructDef, ASTTypedType,
    ASTTypedTypeDef,
};

use super::any::{CLambdas, CStructs};
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
        optimize_lambda: bool,
        optimize_lambda_space: bool,
    ) -> String {
        let mut body = String::new();

        let lambda_type = format!("struct {}*", c_lambda_name);
        self.code_gen.add(
            &mut body,
            &format!("{lambda_type} lambda_ = ({lambda_type}) address->address;"),
            None,
            true,
        );

        if !optimize_lambda_space {
            let lambda_space_type_name =
                CStructs::add_lambda_space_to_statics(statics, lambda_space);

            self.code_gen.add(
                    &mut body,
                    &format!("struct RasmPointer_ *lambda_space_ = (struct RasmPointer_ *) lambda_->lambda_space;"),
                    None,
                    true,
                );

            self.code_gen.add(
                &mut body,
                &format!("struct {lambda_space_type_name} *lambda_space = (struct {lambda_space_type_name} *) lambda_->lambda_space->address;"),
                None,
                true,
            );
            /*
            for (i, (name, kind)) in lambda_space.iter().enumerate() {
                let t = kind.typed_type();

                if let Some(type_name) = get_reference_type_name(t, type_def_provider) {
                    let mut source = format!("lambda_space->{name}");

                    if &type_name == "_fn" {
                        Self::addref_deref_lambda(
                            &mut body,
                            function_name,
                            &source,
                            t,
                            type_def_provider,
                            &self.code_gen,
                            &statics,
                        );
                    } else {
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
            }
            */
        }

        if function_name == "deref" {
            if !optimize_lambda_space {
                self.code_gen
                    .call_deref_simple(&mut body, "lambda_space_", c_lambda_name, statics);
                self.code_gen
                    .add(&mut body, "if (lambda_space_->count == 0) {", None, true);
            }
            if !optimize_lambda {
                self.code_gen
                    .call_deref_simple(&mut body, "address", c_lambda_name, statics);
            }
        } else {
            if !optimize_lambda_space {
                self.code_gen.call_add_ref_simple(
                    &mut body,
                    "lambda_space_",
                    c_lambda_name,
                    statics,
                );
                self.code_gen
                    .add(&mut body, "if (lambda_space_->count == 1) {", None, true);
            }
            if !optimize_lambda {
                self.code_gen
                    .call_add_ref_simple(&mut body, "address", c_lambda_name, statics);
            }
        }

        if !optimize_lambda_space {
            for (i, (name, kind)) in lambda_space.iter().enumerate() {
                let t = kind.typed_type();

                if let Some(type_name) = get_reference_type_name(t, type_def_provider) {
                    let source = format!("lambda_space->{name}");

                    if &type_name == "_fn" {
                        Self::addref_deref_lambda(
                            &mut body,
                            function_name,
                            &source,
                            t,
                            type_def_provider,
                            &self.code_gen,
                            &statics,
                        );
                    } else {
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
            }
            self.code_gen.add(&mut body, "}", None, true);
        }

        body
    }

    pub fn addref_deref_lambda(
        body: &mut String,
        function_name: &str,
        orig_source: &str,
        t: &ASTTypedType,
        type_def_provider: &dyn TypeDefProvider,
        code_gen: &CodeGenC,
        statics: &Statics,
    ) {
        let ts = CodeGenC::type_to_string(t, statics);

        let source = if function_name == "deref" {
            code_gen.add(
                body,
                &format!("if ((({ts}){orig_source}->address)->deref_function != NULL) {{"),
                None,
                true,
            );
            format!("(({ts}){orig_source}->address)->deref_function({orig_source});")
        } else {
            code_gen.add(
                body,
                &format!("if ((({ts}){orig_source}->address)->addref_function != NULL) {{"),
                None,
                true,
            );
            format!("(({ts}){orig_source}->address)->addref_function({orig_source});")
        };

        if function_name == "deref" {
            code_gen.call_deref(body, &source, "_fn", "_fn", type_def_provider, statics);
        } else {
            code_gen.call_add_ref(body, &source, "_fn", "_fn", type_def_provider, statics);
        }
        code_gen.add(body, "}", None, true);
    }

    pub fn create_lambda_free(
        &self,
        c_lambda_name: &str,
        lambda_space: &LambdaSpace,
        function_name: &str,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        optimize_lambda: bool,
        optimize_lambda_space: bool,
    ) -> ASTTypedFunctionDef {
        let body_str = self.create_lambda_free_body(
            c_lambda_name,
            function_name,
            lambda_space,
            type_def_provider,
            statics,
            optimize_lambda,
            optimize_lambda_space,
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
                is_ref: true,
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
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String {
        let mut body = String::new();

        /*
        for property in struct_def.properties.iter() {
            if let Some(type_name) = get_reference_type_name(&property.ast_type, type_def_provider)
            {
                CLambdas::add_to_statics_if_lambda(&property.ast_type, statics);
                let source = format!("($castAddress($address))->{}", property.name);

                if type_name == "_fn" {
                    TypedFunctionsCreatorC::addref_deref_lambda(
                        &mut body,
                        function_name,
                        &source,
                        &property.ast_type,
                        type_def_provider,
                        &self.code_gen,
                        statics,
                    );
                } else {
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
        }
        */

        if function_name == "deref" {
            self.code_gen
                .call_deref_simple(&mut body, "address", &struct_def.name, statics);
            self.code_gen
                .add(&mut body, "if (address->count == 0) {", None, true);
        } else {
            self.code_gen
                .call_add_ref_simple(&mut body, "address", &struct_def.name, statics);
            self.code_gen
                .add(&mut body, "if (address->count == 1) {", None, true);
        }

        for property in struct_def.properties.iter() {
            if let Some(type_name) = get_reference_type_name(&property.ast_type, type_def_provider)
            {
                CLambdas::add_to_statics_if_lambda(&property.ast_type, statics);
                let source = format!("($castAddress($address))->{}", property.name);

                if type_name == "_fn" {
                    TypedFunctionsCreatorC::addref_deref_lambda(
                        &mut body,
                        function_name,
                        &source,
                        &property.ast_type,
                        type_def_provider,
                        &self.code_gen,
                        statics,
                    );
                } else {
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
        }

        self.code_gen.add(&mut body, "}", None, true);

        body
    }

    // we override the default implementation that checks if the enum has references, we want to do it anyway, since
    // enum itself and variant are two separated allocations and we always have to addref/deref both
    fn for_enum(
        &self,
        module: &EnhancedASTModule,
        type_def_provider: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
        enum_def: &ASTTypedEnumDef,
    ) {
        self.create_enum_free(
            enum_def,
            "deref",
            type_def_provider,
            functions_by_name,
            statics,
        );
        self.create_enum_free(
            enum_def,
            "addRef",
            type_def_provider,
            functions_by_name,
            statics,
        );
    }

    fn create_enum_free_body(
        &self,
        enum_def: &ASTTypedEnumDef,
        function_name: &str,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String {
        let mut body = String::new();
        let enum_type_name = format!("{}_{}", enum_def.namespace.safe_name(), enum_def.name);

        self.code_gen.add(
            &mut body,
            "struct Enum *e = (struct Enum*)address->address;",
            None,
            true,
        );

        for (i, variant) in enum_def.variants.iter().enumerate() {
            if i == 0 {
                self.code_gen.add(
                    &mut body,
                    &format!("if (e->variant_num == {i}) {{"),
                    None,
                    true,
                );
            } else {
                self.code_gen.add(
                    &mut body,
                    &format!("else if (e->variant_num == {i}) {{"),
                    None,
                    true,
                );
            }

            if variant.parameters.is_empty() {
                self.code_gen.add(&mut body, "return;", None, true);
                self.code_gen.add(&mut body, "}", None, true);
                continue;
            }

            let variant_type_name = format!("{enum_type_name}_{}", variant.name);
            self.code_gen.add(
                &mut body,
                &format!(
                    "struct {variant_type_name} *variant = (struct {variant_type_name}*) e->variant->address;"
                ),
                None,
                true,
            );

            /*
            for parameter in &variant.parameters {
                if let Some(type_name) =
                    get_reference_type_name(&parameter.ast_type, type_def_provider)
                {
                    CLambdas::add_to_statics_if_lambda(&parameter.ast_type, statics);
                    let source = format!("variant->{}", parameter.name);

                    if type_name == "_fn" {
                        TypedFunctionsCreatorC::addref_deref_lambda(
                            &mut body,
                            function_name,
                            &source,
                            &parameter.ast_type,
                            type_def_provider,
                            &self.code_gen,
                            statics,
                        );
                    } else {
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
            }
            */

            if function_name == "deref" {
                self.code_gen.call_deref_simple(
                    &mut body,
                    "e->variant",
                    &variant_type_name,
                    statics,
                );
                self.code_gen
                    .add(&mut body, "if (e->variant->count == 0) {", None, true);
            } else {
                self.code_gen.call_add_ref_simple(
                    &mut body,
                    "e->variant",
                    &variant_type_name,
                    statics,
                );
                self.code_gen
                    .add(&mut body, "if (e->variant->count == 1) {", None, true);
            }

            for parameter in &variant.parameters {
                if let Some(type_name) =
                    get_reference_type_name(&parameter.ast_type, type_def_provider)
                {
                    CLambdas::add_to_statics_if_lambda(&parameter.ast_type, statics);
                    let source = format!("variant->{}", parameter.name);

                    if type_name == "_fn" {
                        TypedFunctionsCreatorC::addref_deref_lambda(
                            &mut body,
                            function_name,
                            &source,
                            &parameter.ast_type,
                            type_def_provider,
                            &self.code_gen,
                            statics,
                        );
                    } else {
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
            }

            self.code_gen.add(&mut body, "}", None, true);
            self.code_gen.add(&mut body, "}", None, true);
        }

        // TODO else with arror

        if function_name == "deref" {
            self.code_gen
                .call_deref_simple(&mut body, "address", &enum_type_name, statics);
        } else {
            self.code_gen
                .call_add_ref_simple(&mut body, "address", &enum_type_name, statics);
        }

        body
    }

    fn create_type_free_body(
        &self,
        module: &EnhancedASTModule,
        type_def: &ASTTypedTypeDef,
        function_name: &str,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String {
        let suffix = if function_name == "deref" {
            "Deref"
        } else {
            "AddRef"
        };

        // we are looking for a function that the "user" must define to add / remove a reference
        let ref_function_name = format!("{}{suffix}", type_def.original_name);

        let original_function = module
            .functions_by_name
            .find_function_by_original_name(&ref_function_name)
            .expect(&format!(
                "Cannot find {ref_function_name}. It's needed to handle references."
            ));

        // the function should have a native body
        if let ASTFunctionBody::NativeBody(body) = &original_function.body {
            let evaluator = self.code_gen.get_text_macro_evaluator();

            let mut function = original_function.clone();

            let mut dummy = self.create_function(
                &type_def.namespace,
                "dummy",
                type_def.ast_typed_type.clone(),
                ASTTypedFunctionBody::NativeBody(String::new()),
                false,
            );

            function.resolved_generic_types = ResolvedGenericTypes::new();
            for (name, t) in type_def.generic_types.iter() {
                function.resolved_generic_types.insert(
                    name.clone(),
                    type_def_provider.get_type_from_typed_type(&t).unwrap(),
                );
                dummy.generic_types.insert(name.clone(), t.clone());
            }

            evaluator
                .translate(
                    statics,
                    Some(&dummy),
                    Some(&function),
                    body,
                    false,
                    type_def_provider,
                )
                .unwrap()
        } else {
            panic!("{ref_function_name} should be a native function.");
        }
    }
}
