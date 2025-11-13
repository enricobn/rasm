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

use std::iter::zip;
use std::sync::atomic::{AtomicUsize, Ordering};

use linked_hash_map::LinkedHashMap;

use crate::codegen::c::any::CLamdaAddRefDerefFunctions;
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::enh_ast::{EnhASTFunctionBody, EnhASTNameSpace};
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::RefType;
use crate::codegen::type_def_body::TypeDefBodyTarget;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_reference_type_name, CodeGen};
use crate::enh_type_check::enh_resolved_generic_types::EnhResolvedGenericTypes;
use crate::enh_type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedStructDef, ASTTypedType,
    ASTTypedTypeDef,
};
use crate::transformations::typed_functions_creator::TypedFunctionsCreator;

use super::any::{CLambdas, CStructs};
use super::code_gen_c::{CCodeManipulator, CodeGenC};

static REF_FUNCTIONS_ID: AtomicUsize = AtomicUsize::new(0);

pub struct TypedFunctionsCreatorC {
    code_gen: CodeGenC,
    code_manipulator: CCodeManipulator,
}

impl TypedFunctionsCreatorC {
    pub fn new(code_gen: CodeGenC) -> Self {
        Self {
            code_gen,
            code_manipulator: CCodeManipulator::new(),
        }
    }

    fn create_lambda_free_body(
        &self,
        c_lambda_name: &str,
        ref_type: RefType,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        optimize_lambda: bool,
        optimize_lambda_space: bool,
    ) -> String {
        let mut body = String::new();

        let lambda_type = format!("struct {}*", c_lambda_name);

        if !optimize_lambda_space {
            self.code_gen.add(
                &mut body,
                &format!("{lambda_type} lambda_ = ({lambda_type}) address->address;"),
                None,
                true,
            );
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
        }

        if ref_type == RefType::Deref {
            if !optimize_lambda_space {
                CodeGenC::call_deref_simple(
                    &self.code_manipulator,
                    &mut body,
                    "lambda_space_",
                    c_lambda_name,
                );
                self.code_gen
                    .add(&mut body, "if (lambda_space_->count == 0) {", None, true);
            }
            if !optimize_lambda {
                CodeGenC::call_deref_simple(
                    &self.code_manipulator,
                    &mut body,
                    "address",
                    c_lambda_name,
                );
            }
        } else {
            if !optimize_lambda_space {
                CodeGenC::call_add_ref_simple(
                    &self.code_manipulator,
                    &mut body,
                    "lambda_space_",
                    c_lambda_name,
                );
                self.code_manipulator.add(
                    &mut body,
                    "if (lambda_space_->count == 1) {",
                    None,
                    true,
                );
            }
            if !optimize_lambda {
                CodeGenC::call_add_ref_simple(
                    &self.code_manipulator,
                    &mut body,
                    "address",
                    c_lambda_name,
                );
            }
        }

        if !optimize_lambda_space {
            for (_i, (name, kind)) in lambda_space.iter().enumerate() {
                let t = kind.typed_type();

                if let Some(type_name) = get_reference_type_name(t, &TypeDefBodyTarget::C) {
                    let source = format!("lambda_space->{name}");

                    if &type_name == "_fn" {
                        Self::addref_deref_lambda(
                            &self.code_manipulator,
                            &mut body,
                            ref_type,
                            &source,
                            t,
                            &statics,
                        );
                    } else {
                        if ref_type == RefType::Deref {
                            CodeGenC::call_deref(
                                &self.code_manipulator,
                                &mut body,
                                &source,
                                &type_name,
                                &type_name,
                                type_def_provider,
                            );
                        } else {
                            CodeGenC::call_add_ref(
                                &self.code_manipulator,
                                &mut body,
                                &source,
                                &type_name,
                                &type_name,
                                type_def_provider,
                            );
                        }
                    }
                }
            }
            self.code_gen.add(&mut body, "}", None, true);
        }
        //self.code_gen.add(&mut body, "return NULL;", None, true);

        body
    }

    pub fn addref_deref_lambda(
        code_manipulator: &CCodeManipulator,
        body: &mut String,
        ref_type: RefType,
        orig_source: &str,
        t: &ASTTypedType,
        statics: &Statics,
    ) {
        let ts = CodeGenC::type_to_string(t, statics);

        let function = if ref_type == RefType::Deref {
            format!("(({ts}){orig_source}->address)->deref_function")
        } else {
            format!("(({ts}){orig_source}->address)->addref_function")
        };

        code_manipulator.add_rows(
            body,
            vec![
                &format!("if ({function} != NULL) {{"),
                &format!("    {function}({orig_source});"),
                "}",
            ],
            None,
            true,
        );
    }

    pub fn create_or_get_lambda_free(
        &self,
        c_lambda_name: &str,
        lambda_space: &mut LambdaSpace,
        ref_type: RefType,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        optimize_lambda: bool,
        optimize_lambda_space: bool,
    ) -> String {
        CLamdaAddRefDerefFunctions::add_to_statics(
            statics,
            ref_type,
            c_lambda_name.to_string(),
            optimize_lambda,
            optimize_lambda_space,
            lambda_space,
            type_def_provider,
            self,
        )
    }

    pub fn create_lambda_free(
        &self,
        c_lambda_name: &str,
        lambda_space: &LambdaSpace,
        ref_type: RefType,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        optimize_lambda: bool,
        optimize_lambda_space: bool,
    ) -> ASTTypedFunctionDef {
        let body_str = self.create_lambda_free_body(
            c_lambda_name,
            ref_type,
            lambda_space,
            type_def_provider,
            statics,
            optimize_lambda,
            optimize_lambda_space,
        );
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        let fun_name = format!(
            "lambda_{}_{}",
            ref_type.function_name(),
            REF_FUNCTIONS_ID.fetch_add(1, Ordering::SeqCst)
        );

        self.create_function(
            &EnhASTNameSpace::global(),
            &fun_name,
            crate::enh_type_check::typed_ast::ASTTypedType::Type {
                namespace: EnhASTNameSpace::global(),
                name: "CTargetLambda".to_string(),
                body: "hasReferences=true,nativeType=void **".to_owned(),
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
        ref_type: RefType,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String {
        let mut body = String::new();

        if ref_type == RefType::Deref {
            CodeGenC::call_deref_simple(
                &self.code_manipulator,
                &mut body,
                "address",
                &struct_def.name,
            );
            self.code_gen
                .add(&mut body, "if (address->count == 0) {", None, true);
        } else {
            CodeGenC::call_add_ref_simple(
                &self.code_manipulator,
                &mut body,
                "address",
                &struct_def.name,
            );
            self.code_gen
                .add(&mut body, "if (address->count == 1) {", None, true);
        }

        for property in struct_def.properties.iter() {
            if let Some(type_name) =
                get_reference_type_name(&property.ast_type, &TypeDefBodyTarget::C)
            {
                CLambdas::add_to_statics_if_lambda(&property.ast_type, statics);
                let source = format!("($castAddress($address))->{}", property.name);

                if type_name == "_fn" {
                    TypedFunctionsCreatorC::addref_deref_lambda(
                        &self.code_manipulator,
                        &mut body,
                        ref_type,
                        &source,
                        &property.ast_type,
                        statics,
                    );
                } else {
                    if ref_type == RefType::Deref {
                        CodeGenC::call_deref(
                            &self.code_manipulator,
                            &mut body,
                            &source,
                            &type_name,
                            "",
                            type_def_provider,
                        );
                    } else {
                        CodeGenC::call_add_ref(
                            &self.code_manipulator,
                            &mut body,
                            &source,
                            &type_name,
                            "",
                            type_def_provider,
                        );
                    }
                }
            }
        }

        self.code_gen.add(&mut body, "}", None, true);
        //self.code_gen.add(&mut body, "return NULL;", None, true);

        body
    }

    // we override the default implementation that checks if the enum has references,
    // we want to do it anyway, unless variants are all parameterless, since
    // enum itself and variant are two separated allocations and we always have to addref/deref both
    fn for_enum(
        &self,
        _module: &EnhancedASTModule,
        type_def_provider: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
        enum_def: &ASTTypedEnumDef,
    ) {
        // if there are no parameters in variants we don't need to create addref and deref
        // because the are static
        if enum_def.variants.iter().all(|it| it.parameters.is_empty()) {
            return;
        }
        self.create_enum_free(
            enum_def,
            RefType::Deref,
            type_def_provider,
            functions_by_name,
            statics,
        );
        self.create_enum_free(
            enum_def,
            RefType::AddRef,
            type_def_provider,
            functions_by_name,
            statics,
        );
    }

    fn create_enum_free_body(
        &self,
        enum_def: &ASTTypedEnumDef,
        ref_type: RefType,
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
                self.code_gen.add(&mut body, "return NULL;", None, true);
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

            if ref_type == RefType::Deref {
                CodeGenC::call_deref_simple(
                    &self.code_manipulator,
                    &mut body,
                    "e->variant",
                    &variant_type_name,
                );
            } else {
                CodeGenC::call_add_ref_simple(
                    &self.code_manipulator,
                    &mut body,
                    "e->variant",
                    &variant_type_name,
                );
            }

            let mut inner_body = String::new();

            for parameter in &variant.parameters {
                if let Some(type_name) =
                    get_reference_type_name(&parameter.ast_type, &TypeDefBodyTarget::C)
                {
                    CLambdas::add_to_statics_if_lambda(&parameter.ast_type, statics);
                    let source = format!("variant->{}", parameter.name);

                    if type_name == "_fn" {
                        TypedFunctionsCreatorC::addref_deref_lambda(
                            &self.code_manipulator,
                            &mut body,
                            ref_type,
                            &source,
                            &parameter.ast_type,
                            statics,
                        );
                    } else {
                        if ref_type == RefType::Deref {
                            CodeGenC::call_deref(
                                &self.code_manipulator,
                                &mut inner_body,
                                &source,
                                &type_name,
                                &type_name,
                                type_def_provider,
                            );
                        } else {
                            CodeGenC::call_add_ref(
                                &self.code_manipulator,
                                &mut inner_body,
                                &source,
                                &type_name,
                                &type_name,
                                type_def_provider,
                            );
                        }
                    }
                }
            }

            if !inner_body.is_empty() {
                if ref_type == RefType::Deref {
                    self.code_gen
                        .add(&mut body, "if (e->variant->count == 0) {", None, true);
                } else {
                    self.code_gen
                        .add(&mut body, "if (e->variant->count == 1) {", None, true);
                }
                body.push_str(&inner_body);
                self.code_gen.add(&mut body, "}", None, true);
            }
            self.code_gen.add(&mut body, "}", None, true);
        }

        // TODO else with error

        if ref_type == RefType::Deref {
            CodeGenC::call_deref_simple(
                &self.code_manipulator,
                &mut body,
                "address",
                &enum_type_name,
            );
        } else {
            CodeGenC::call_add_ref_simple(
                &self.code_manipulator,
                &mut body,
                "address",
                &enum_type_name,
            );
        }
        //self.code_gen.add(&mut body, "return NULL;", None, true);

        //println!("create enum {} free body:\n{}", enum_def.name, body);

        body
    }

    fn create_type_free_body(
        &self,
        module: &EnhancedASTModule,
        type_def: &ASTTypedTypeDef,
        ref_type: RefType,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String {
        let suffix = if ref_type == RefType::Deref {
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
        if let EnhASTFunctionBody::NativeBody(body) = &original_function.body {
            let evaluator = self.code_gen.get_text_macro_evaluator();

            let mut function = original_function.clone();

            if function.parameters.len() != 1
                || function.parameters.get(0).unwrap().name != "address"
            {
                panic!(
                    "function {ref_function_name} must have a parameter named 'address' : {}",
                    function.index
                );
            }

            let mut dummy = self.create_function(
                &original_function.namespace,
                &original_function.name,
                type_def.ast_typed_type.clone(),
                ASTTypedFunctionBody::NativeBody(String::new()),
                false,
            );

            function.resolved_generic_types = EnhResolvedGenericTypes::new();
            for (((_gen_name, var_types), t), orig_gen_name) in
                zip(type_def.generic_types.iter(), function.generic_types.iter())
            {
                function.resolved_generic_types.insert(
                    orig_gen_name.clone(),
                    var_types.clone(),
                    type_def_provider.get_type_from_typed_type(t).unwrap(),
                );
                dummy
                    .resolved_generic_types
                    .insert(orig_gen_name.clone(), var_types, t.clone());
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
                .map_err(|error| format!("{error} in function : {}", function.index))
                .unwrap()
        } else {
            panic!("{ref_function_name} should be a native function.");
        }
    }

    fn type_def_body_target(&self) -> TypeDefBodyTarget {
        TypeDefBodyTarget::C
    }

    fn addref_with_descr(&self) -> bool {
        false
    }
}
