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

use core::panic;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use crate::codegen::c::any::{
    CConsts, CFunctionsDeclarations, CInclude, CLambdas, CStrings, CStructs,
};
use crate::codegen::c::function_call_parameters_c::CFunctionCallParameters;
use crate::codegen::c::options::COptions;
use crate::codegen::c::text_macro_c::{
    CCallMacro, CEnumDeclarationMacro, CEnumVariantAssignmentMacro, CEnumVariantDeclarationMacro,
    CEnumVariantMacro, CIncludeMacro, CRealTypeNameMacro, CStructDeclarationMacro,
    CStructTypeMacro,
};
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::enh_ast::{EnhASTIndex, EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind};
use crate::codegen::enh_val_context::TypedValContext;
use crate::codegen::function_call_parameters::FunctionCallParameters;
use crate::codegen::lambda::{LambdaCall, LambdaSpace};
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{InlineMacro, InlineRegistry, RefType, TextMacroEvaluator};
use crate::codegen::type_def_body::{TypeDefBodyCache, TypeDefBodyTarget};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_reference_type_name, CodeGen, CodeGenOptions, TypedValKind};
use crate::enh_type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef,
    ASTTypedModule, ASTTypedParameterDef, ASTTypedType, BuiltinTypedTypeKind, CustomTypedTypeDef,
};
use crate::project::RasmProject;
use crate::transformations::typed_functions_creator::struct_has_references;

use log::info;
use rasm_parser::parser::ast::{ASTModifiers, ASTValue};
use walkdir::WalkDir;

use super::c_compiler::CLibAssets;
use super::text_macro_c::{
    CAddRefMacro, CCastAddress, CEnumSimpleMacro, CIsRefMacro, CTypeNameMacro,
};
use super::typed_function_creator_c::TypedFunctionsCreatorC;

#[derive(Clone)]
pub struct CCodeManipulator;

impl CCodeManipulator {
    pub fn new() -> Self {
        Self {}
    }
}

impl CodeManipulator for CCodeManipulator {
    fn add_comment(&self, out: &mut String, comment: &str, indent: bool) {
        if !comment.is_empty() {
            self.add(out, &format!("// {comment}"), None, indent);
        }
    }

    fn push_comment(&self, out: &mut String, comment: &str, indent: bool) {
        if !comment.is_empty() {
            self.push(out, &format!("// {comment}"), None, indent);
        }
    }

    fn remove_comments_from_line(&self, line: String) -> String {
        if let Some(pos) = line.find("//") {
            if pos > 0 {
                line.split_at(pos).0.to_string()
            } else {
                String::new()
            }
        } else {
            line
        }
    }
}

#[derive(Clone)]
pub struct CodeGenC {
    code_manipulator: CCodeManipulator,
    c_options: COptions,
    debug: bool,
}

impl CodeGenC {
    pub fn new(options: COptions, debug: bool) -> Self {
        Self {
            code_manipulator: CCodeManipulator,
            c_options: options,
            debug,
        }
    }

    pub fn type_to_string(ast_type: &ASTTypedType, statics: &Statics) -> String {
        match ast_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => "char*".to_string(),
                BuiltinTypedTypeKind::Integer => "long".to_string(),
                BuiltinTypedTypeKind::Boolean => "char".to_string(),
                BuiltinTypedTypeKind::Char => "char*".to_string(),
                BuiltinTypedTypeKind::Float => "double".to_string(),
                BuiltinTypedTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    if let Some(name) =
                        CLambdas::find_name_in_statics(statics, parameters, return_type)
                    {
                        format!("struct {name}*")
                    } else {
                        panic!("Cannot find lambda def");
                    }
                }
            },
            ASTTypedType::Unit => "struct Void_*".to_string(),
            ASTTypedType::Struct { namespace, name } => {
                format!("struct {}_{name}*", namespace.safe_name())
            }
            ASTTypedType::Enum {
                namespace: _,
                name: _,
            } => {
                format!("struct Enum*")
            }
            ASTTypedType::Type {
                namespace: _,
                name: _,
                body,
            } => TypeDefBodyCache::get_c(body).native_type.clone(),
        }
    }

    pub fn real_type_to_string(ast_type: &ASTTypedType) -> String {
        match ast_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => "struct RasmPointer_*".to_string(),
                BuiltinTypedTypeKind::Integer => "long".to_string(),
                BuiltinTypedTypeKind::Boolean => "char".to_string(),
                BuiltinTypedTypeKind::Char => "struct RasmPointer_*".to_string(),
                BuiltinTypedTypeKind::Float => "double".to_string(),
                BuiltinTypedTypeKind::Lambda {
                    parameters: _,
                    return_type: _,
                } => "struct RasmPointer_*".to_string(),
            },
            ASTTypedType::Unit => "struct Void_*".to_string(),
            ASTTypedType::Struct {
                namespace: _,
                name: _,
            } => "struct RasmPointer_*".to_string(),
            ASTTypedType::Enum {
                namespace: _,
                name: _,
            } => "struct RasmPointer_*".to_string(),
            ASTTypedType::Type {
                namespace: _,
                name: _,
                body,
            } => {
                let type_def_body = TypeDefBodyCache::get_c(body);
                if type_def_body.has_references {
                    "struct RasmPointer_*".to_string()
                } else {
                    type_def_body.native_type.clone()
                }
            }
        }
    }

    pub fn escape_string(s: &str) -> String {
        let mut result = s.to_string();
        result = result.replace("\\\\", "\\");
        result = result.replace("\"", "\\\"");
        result
    }

    pub fn call_add_ref(
        code_manipulator: &CCodeManipulator,
        out: &mut String,
        source: &str,
        type_name: &str,
        descr_for_debug: &str,
        type_def_provider: &dyn TypeDefProvider,
    ) {
        let (has_references, is_static) = if let Some(struct_def) =
            type_def_provider.get_struct_def_by_name(type_name)
        {
            (
                struct_has_references(struct_def, TypeDefBodyTarget::C),
                false,
            )
        } else if let Some(enum_def) = type_def_provider.get_enum_def_by_name(type_name) {
            let enum_has_parametric_variants = Self::enum_has_parametric_variants(enum_def);
            (enum_has_parametric_variants, !enum_has_parametric_variants)
        } else if let Some(type_def) = type_def_provider.get_type_def_by_name(type_name) {
            (
                TypeDefBodyCache::type_body_has_references(&type_def.body, &TypeDefBodyTarget::C),
                false,
            )
        } else if "char" == type_name || "str" == type_name || "_fn" == type_name {
            (false, false)
        } else {
            panic!("call_add_ref, cannot find type {type_name}");
        };

        if has_references {
            code_manipulator.add(
                out,
                &format!("{type_name}_addRef({source});"),
                Some(descr_for_debug),
                true,
            );
        } else {
            if "_fn" == type_name {
                code_manipulator.add(out, &source, None, true);
            } else if !is_static {
                Self::call_add_ref_simple(code_manipulator, out, source, descr_for_debug);
            }
        }
    }

    pub fn call_add_ref_simple(
        code_manipulator: &CCodeManipulator,
        out: &mut String,
        source: &str,
        descr_for_debug: &str,
    ) {
        code_manipulator.add(
            out,
            &format!("addRef({source});"),
            Some(descr_for_debug),
            true,
        );
    }

    fn enum_has_parametric_variants(enum_def: &ASTTypedEnumDef) -> bool {
        enum_def.variants.iter().any(|it| !it.parameters.is_empty())
    }

    pub fn call_deref(
        code_manipulator: &CCodeManipulator,
        out: &mut String,
        source: &str,
        type_name: &str,
        descr_for_debug: &str,
        type_def_provider: &dyn TypeDefProvider,
    ) {
        let (has_references, is_static) = if let Some(struct_def) =
            type_def_provider.get_struct_def_by_name(type_name)
        {
            (
                struct_has_references(struct_def, TypeDefBodyTarget::C),
                false,
            )
        } else if let Some(enum_def) = type_def_provider.get_enum_def_by_name(type_name) {
            let enum_has_parametric_variants = Self::enum_has_parametric_variants(enum_def);
            (enum_has_parametric_variants, !enum_has_parametric_variants)
        } else if let Some(type_def) = type_def_provider.get_type_def_by_name(type_name) {
            (
                TypeDefBodyCache::type_body_has_references(&type_def.body, &TypeDefBodyTarget::C),
                false,
            )
        } else if "char" == type_name || "str" == type_name || "_fn" == type_name {
            (false, false)
        } else {
            panic!("call_add_ref, cannot find type {type_name}");
        };

        if has_references {
            code_manipulator.add(
                out,
                &format!("{type_name}_deref({source});"),
                Some(descr_for_debug),
                true,
            );
        } else {
            if "_fn" == type_name {
                code_manipulator.add(out, &source, None, true);
            } else if !is_static {
                Self::call_deref_simple(code_manipulator, out, source, descr_for_debug);
            }
        }
    }

    pub fn call_deref_simple(
        code_manipulator: &CCodeManipulator,
        out: &mut String,
        source: &str,
        descr_for_debug: &str,
    ) {
        code_manipulator.add(
            out,
            &format!("deref({source});"),
            Some(descr_for_debug),
            true,
        );
    }

    pub fn variant_const_name(namespace: &EnhASTNameSpace, e: &str, v: &str) -> String {
        format!("{}_{}_{}_value_", namespace.safe_name(), e, v)
    }
}

pub struct CodeGenCContext {}

impl<'a> CodeGen<'a, Box<CFunctionCallParameters>, CodeGenCContext, COptions> for CodeGenC {
    fn options(&self) -> &COptions {
        &self.c_options
    }

    fn end_main(&self, code: &mut String) {
        self.add(code, "freeReferences();", None, true);
    }

    fn transform_before_in_function_def(&self, _stack: &CodeGenCContext, before: String) -> String {
        before
    }

    fn main_init(&self, _generated_code: &mut String) {}

    fn call_lambda_parameter(
        &self,
        _code_gen_context: &CodeGenCContext,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        kind: &TypedValKind,
        call_parameters: &Box<CFunctionCallParameters>,
        return_value: bool,
        is_inner_call: bool,
        statics: &Statics,
    ) {
        let mut args = call_parameters
            .parameters_values()
            .iter()
            .map(|(_name, value)| (value.as_str(), None))
            .collect::<Vec<_>>();
        args.push((function_call.function_name.as_str(), None));

        if return_value {
            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: _,
                return_type,
            }) = kind.typed_type()
            {
                let t = CodeGenC::real_type_to_string(&return_type.as_ref());
                self.add(before, &format!("{t} return_value_ = "), None, true);
            } else {
                panic!("expected lambda : {}", function_call.index);
            }
        }
        self.call_function(
            before,
            &format!(
                "(({}){}->address)->functionPtr",
                CodeGenC::type_to_string(kind.typed_type(), statics),
                function_call.function_name
            ),
            &args,
            None,
            false,
            is_inner_call,
        );
    }

    fn call_lambda_from_lambda_space(
        &self,
        _code_gen_context: &CodeGenCContext,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        _index_in_lambda_space: usize,
        call_parameters: &Box<CFunctionCallParameters>,
        ast_type_type: &ASTTypedType,
        statics: &Statics,
        return_value: bool,
        is_inner_call: bool,
    ) {
        let mut args = call_parameters
            .parameters_values()
            .iter()
            .map(|(_name, value)| (value.as_str(), None))
            .collect::<Vec<_>>();
        let (casted_lambda, return_type) =
            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters,
                return_type,
            }) = &ast_type_type
            {
                let lambda_type_name =
                    CLambdas::find_name_in_statics(statics, parameters, return_type.as_ref())
                        .unwrap();
                (
                    format!(
                        "((struct {}*)((struct RasmPointer_*)lambda_space->{})->address)",
                        lambda_type_name, function_call.function_name
                    ),
                    CodeGenC::real_type_to_string(&return_type.as_ref()),
                )
            } else {
                panic!();
            };

        let arg = format!("lambda_space->{}", function_call.function_name);

        args.push((&arg, None));

        if return_value {
            self.add(
                before,
                &format!("{return_type} return_value_ =  // CodgenC.call_lambda"),
                None,
                true,
            );
            self.call_function(
                before,
                &format!("{casted_lambda}->functionPtr"),
                &args,
                None,
                false,
                is_inner_call,
            );
        } else {
            self.call_function(
                before,
                &format!("{casted_lambda}->functionPtr"),
                &args,
                None,
                return_value,
                is_inner_call,
            );
        }
    }

    fn function_call_parameters<'b, 'c>(
        &self,
        _code_gen_context: &'c CodeGenCContext,
        _parent_fcp: Option<&Box<CFunctionCallParameters>>,
        parameters: &'b Vec<ASTTypedParameterDef>,
        _inline: bool,
        immediate: bool,
        _id: usize,
    ) -> Box<CFunctionCallParameters> {
        Box::new(CFunctionCallParameters::new(parameters.clone(), immediate))
    }

    fn insert_let_in_context(
        &self,
        _code_gen_context: &CodeGenCContext,
        context: &mut TypedValContext,
        name: &str,
        typed_type: &ASTTypedType,
    ) {
        context.insert_let(name.into(), typed_type.clone(), None);
    }

    fn store_function_result(
        &self,
        _code_gen_context: &CodeGenCContext,
        code: &mut String,
        name: &str,
        typed_type: &ASTTypedType,
    ) {
        code.insert_str(
            0,
            &format!("{} {} = ", Self::real_type_to_string(typed_type), name),
        );
    }

    fn add_const_ref(
        &self,
        name: &str,
        statics: &mut Statics,
        body: &mut String,
        typed_module: &ASTTypedModule,
        _index: &EnhASTIndex,
        type_name: &String,
        namespace: &EnhASTNameSpace,
        _modifiers: &ASTModifiers,
    ) {
        let entry = statics.get_typed_const(name, namespace).unwrap();
        Self::call_add_ref(
            &self.code_manipulator,
            body,
            &entry.key,
            type_name,
            &type_name,
            typed_module,
        );
    }

    fn call_deref_for_let_val(
        &self,
        _code_gen_context: &CodeGenCContext,
        name: &str,
        statics: &mut Statics,
        type_name: &String,
        typed_module: &ASTTypedModule,
        t: &ASTTypedType,
    ) -> String {
        let mut result = String::new();

        if type_name == "_fn" {
            TypedFunctionsCreatorC::addref_deref_lambda(
                &self.code_manipulator,
                &mut result,
                "deref",
                name,
                t,
                typed_module,
                statics,
            );
        } else {
            Self::call_deref(
                &self.code_manipulator,
                &mut result,
                name,
                type_name,
                "",
                typed_module,
            );
        }

        result
    }

    fn call_add_ref_for_let_val(
        &self,
        _code_gen_context: &CodeGenCContext,
        name: &str,
        _index: &EnhASTIndex,
        before: &mut String,
        statics: &mut Statics,
        type_name: &String,
        typed_module: &ASTTypedModule,
        t: &ASTTypedType,
    ) {
        if type_name == "_fn" {
            TypedFunctionsCreatorC::addref_deref_lambda(
                &self.code_manipulator,
                before,
                "addRef",
                name,
                t,
                typed_module,
                statics,
            );
        } else {
            Self::call_add_ref(
                &self.code_manipulator,
                before,
                name,
                type_name,
                "",
                typed_module,
            );
        }
    }

    fn set_let_const_for_function_call_result(
        &self,
        statics_key: &str,
        before: &mut String,
        _current: &mut String,
        _name: &str,
        typed_type: &ASTTypedType,
        statics: &mut Statics,
    ) {
        // TODO should be const? But in this way I get a warning. Should all pointer be consts? But can we release them?
        CConsts::add_to_statics(
            statics,
            statics_key.to_owned(),
            format!(
                "{} {statics_key};",
                CodeGenC::real_type_to_string(typed_type)
            ),
            None,
        );
        self.add(before, &format!("{statics_key} = ",), None, true);
    }

    fn set_let_for_value_ref(
        &self,
        _code_gen_context: &CodeGenCContext,
        before: &mut String,
        val_name: &String,
        typed_val_kind: &TypedValKind,
        statics: &Statics,
        name: &str,
    ) -> ASTTypedType {
        // todo!("{val_name}")
        let t = typed_val_kind.typed_type();
        self.add(
            before,
            &format!("{} {name} = {val_name};", Self::type_to_string(t, statics)),
            None,
            true,
        );
        t.clone()
    }

    fn set_let_for_string_literal(
        &self,
        _code_gen_context: &CodeGenCContext,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        value: &String,
        _typed_type: &ASTTypedType,
        namespace: &EnhASTNameSpace,
        _modifiers: Option<&ASTModifiers>,
    ) {
        if is_const {
            let entry = statics.get_typed_const(name, namespace).unwrap();
            let key = entry.key.clone();
            CConsts::add_to_statics(
                statics,
                key.clone(),
                format!("struct RasmPointer_* {}", &key),
                None,
            );
            self.add(
                body,
                &format!(
                    "{} = addStaticStringToHeap(\"{}\");",
                    &key,
                    Self::escape_string(value)
                ),
                None,
                true,
            );
        } else {
            let string_const = CStrings::add_to_statics(statics, value.to_owned());
            self.add(
                before,
                &format!("struct RasmPointer_* {name} = {string_const};"),
                None,
                true,
            );
        }
    }

    fn set_let_for_value(
        &self,
        _code_gen_context: &CodeGenCContext,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        _body: &mut String,
        value_type: &ASTValue,
        typed_type: &ASTTypedType,
        namespace: &EnhASTNameSpace,
        _modifiers: Option<&ASTModifiers>,
    ) {
        let value = self.value_to_string(statics, value_type);

        if is_const {
            let entry = statics.get_typed_const(name, namespace).unwrap();
            CConsts::add_to_statics(
                statics,
                entry.key.clone(),
                format!(
                    "{} {}",
                    CodeGenC::real_type_to_string(typed_type),
                    entry.key
                ),
                Some(value),
            );
        } else {
            self.add(
                before,
                &format!(
                    "{} {name} = {};",
                    CodeGenC::real_type_to_string(typed_type),
                    value
                ),
                None,
                true,
            );
        }
    }

    fn function_def(
        &'a self,
        _code_gen_context: &CodeGenCContext,
        out: &mut String,
        function_def: &ASTTypedFunctionDef,
        statics: &mut Statics,
    ) {
        let mut args = Vec::new();
        for par in function_def.parameters.iter() {
            // probably sometimes we need to add the lambda def here
            CLambdas::add_to_statics_if_lambda(&par.ast_type, statics);

            let arg_type = Self::real_type_to_string(&par.ast_type);
            args.push(format!("{arg_type} {}", par.name));
        }

        // probably sometimes we need to add the lambda def here
        CLambdas::add_to_statics_if_lambda(&function_def.return_type, statics);

        let inline = InlineRegistry::is_inline(statics, function_def)
            || &function_def.name == "addRef"
            || &function_def.name == "deref";

        let inline_str = if inline { " inline " } else { "" };

        self.add(
            out,
            &format!(
                "{inline_str}{} {}({}) {{",
                Self::real_type_to_string(&function_def.return_type),
                function_def.name,
                args.join(", ")
            ),
            None,
            false,
        );

        CFunctionsDeclarations::add_to_statics(
            statics,
            format!(
                "extern {} {}({});",
                Self::real_type_to_string(&function_def.return_type),
                function_def.name,
                args.join(", ")
            ),
        );
    }

    fn word_len(&self) -> usize {
        todo!()
    }

    fn word_size(&self) -> &str {
        todo!()
    }

    fn reserve_lambda_space(
        &self,
        _code_gen_context: &CodeGenCContext,
        before: &mut String,
        statics: &mut Statics,
        lambda_space: &LambdaSpace,
        def: &ASTTypedFunctionDef,
    ) {
        if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
            parameters,
            return_type,
        }) = &def.parameters.last().unwrap().ast_type
        {
            let lambda_name =
                CLambdas::find_name_in_statics(statics, parameters, return_type).unwrap();

            let lambda_space_type_name =
                CStructs::add_lambda_space_to_statics(statics, lambda_space);
            if !lambda_space.is_empty() {
                self.add(
                    before,
                    &format!(
                        "struct {lambda_space_type_name} *lambda_space = (struct {lambda_space_type_name} *) ((struct {lambda_name}*)_lambda->address)\n->lambda_space->address;"
                    ),
                    None,
                    true,
                );
            }
        } else {
            panic!("expected lambda as last argument of {def}");
        }
    }

    fn value_as_return(&self, before: &mut String, value_type: &ASTValue, statics: &mut Statics) {
        if let ASTValue::ASTStringValue(s) = value_type {
            self.string_literal_return(statics, before, s);
        } else {
            let v = self.value_to_string(statics, value_type);
            let t = value_type_to_typed_type(value_type);
            self.code_manipulator.add(
                before,
                &format!("{} return_value_ = {v};", CodeGenC::real_type_to_string(&t)),
                None,
                true,
            );
        }
    }

    fn string_literal_return(&self, statics: &mut Statics, before: &mut String, value: &String) {
        let string_const = CStrings::add_to_statics(statics, value.to_owned());
        self.code_manipulator.add(
            before,
            &format!("struct RasmPointer_ *return_value_ = {string_const};"),
            None,
            true,
        );
    }

    fn get_text_macro_evaluator(&self) -> TextMacroEvaluator {
        let mut evaluator = TextMacroEvaluator::new(CCodeManipulator::new());
        evaluator.add("call", CCallMacro);
        evaluator.add("include", CIncludeMacro);
        evaluator.add("structDeclaration", CStructDeclarationMacro);
        evaluator.add("structType", CStructTypeMacro);
        evaluator.add("enumVariantDeclaration", CEnumVariantDeclarationMacro);
        evaluator.add("enumDeclaration", CEnumDeclarationMacro);
        evaluator.add("enumVariantAssignment", CEnumVariantAssignmentMacro);
        evaluator.add("enumVariant", CEnumVariantMacro);
        evaluator.add(
            "addRef",
            CAddRefMacro::new(
                CCodeManipulator::new(),
                RefType::AddRef,
                self.c_options.dereference(),
            ),
        );
        evaluator.add(
            "deref",
            CAddRefMacro::new(
                CCodeManipulator::new(),
                RefType::Deref,
                self.c_options.dereference(),
            ),
        );
        evaluator.add("typeName", CTypeNameMacro::new());
        evaluator.add("realTypeName", CRealTypeNameMacro::new());
        evaluator.add("castAddress", CCastAddress::new());
        evaluator.add("enumSimple", CEnumSimpleMacro::new());
        evaluator.add("isRef", CIsRefMacro::new());
        evaluator.add("inline", InlineMacro::new());

        evaluator
    }

    fn print_memory_info(&self, _native_code: &mut String, _statics: &Statics) {
        todo!()
    }

    fn optimize_unused_functions(&self) -> bool {
        false // TODO
    }

    fn initialize_static_values(&self, _generated_code: &mut String) {
        // TODO
    }

    fn debug(&self) -> bool {
        false
    }

    fn call_function_simple(
        &self,
        out: &mut String,
        function_name: &str,
        call_parameters: Option<&Box<CFunctionCallParameters>>,
        return_value: bool,
        is_inner_call: bool,
        return_type: Option<&ASTTypedType>,
        _statics: &Statics,
    ) {
        let args = call_parameters
            .unwrap()
            .parameters_values()
            .iter()
            .map(|(_name, value)| (value.as_str(), None))
            .collect::<Vec<_>>();
        if return_value {
            if let Some(rt) = return_type {
                self.add(
                    out,
                    &format!("{} return_value_ = ", CodeGenC::real_type_to_string(rt)),
                    None,
                    true,
                );
            } else {
                panic!("cannot set return value without a type");
            }
        }
        self.call_function(out, function_name, &args, None, false, is_inner_call);
        if return_value {
            self.add(out, ";", None, true);
        }
    }

    fn call_function(
        &self,
        out: &mut String,
        function_name: &str,
        args: &[(&str, Option<&str>)],
        _comment: Option<&str>,
        return_value: bool,
        is_inner_call: bool,
    ) {
        let arg_vec = args
            .to_vec()
            .iter()
            .map(|it| it.0.to_string())
            .collect::<Vec<_>>();

        let prefix = if return_value {
            "// call_function return\nreturn_value = "
        } else {
            ""
        };
        let suffix = if is_inner_call { "" } else { ";" };

        let inner_call = format!("{prefix}{function_name}({}){suffix}", arg_vec.join(", "));

        if is_inner_call {
            out.push_str(&inner_call);
        } else {
            self.add(out, &inner_call, None, true);
        }
    }

    fn preamble(&self, _code: &mut String) {}

    fn reserve_local_vals(&self, _code_gen_context: &CodeGenCContext, _out: &mut String) {}

    fn generate_statics_code(
        &self,
        _project: &RasmProject,
        statics: &Statics,
        typed_module: &ASTTypedModule,
        _out_folder: &Path,
    ) -> (String, String, String) {
        let mut include = String::new();
        let mut before = String::new();
        let mut after = String::new();

        if let Some(includes) = statics.any::<CInclude>() {
            for inc in includes.unique() {
                self.add(&mut include, &format!("#include {inc}"), None, false);
            }
            self.add_empty_line(&mut include);
        }

        if self.debug {
            self.add(&mut include, "#define __RASM_DEBUG__", None, false);
        }

        if let Some(strings) = statics.any::<CStrings>() {
            for (value, name) in strings.map.iter() {
                self.add(
                    &mut before,
                    &format!("struct RasmPointer_* {name};"),
                    None,
                    false,
                );
                self.add(
                    &mut after,
                    &format!(
                        "{name} = addStaticStringToHeap(\"{}\");",
                        Self::escape_string(value)
                    ),
                    None,
                    true,
                );
                CodeGenC::call_add_ref(
                    &self.code_manipulator,
                    &mut after,
                    &name,
                    "str",
                    "",
                    typed_module,
                );
                self.add(
                    &mut include,
                    &format!("extern struct RasmPointer_ *{name};"),
                    None,
                    false,
                );
            }
        }

        if let Some(consts) = statics.any::<CConsts>() {
            for (name, def, value) in consts.vec.iter() {
                self.add(&mut include, &format!("extern {def};"), None, false);
                self.add(&mut before, &format!("{def};"), None, false);

                if let Some(v) = value {
                    self.add(&mut after, &format!("{name} = {v};"), None, true);
                }
            }
            self.add_empty_line(&mut include);
        }

        let mut variant_consts = HashSet::new();

        for s in typed_module.enums.iter() {
            for variant in s.variants.iter() {
                if variant.parameters.is_empty() {
                    if let EnhASTType::Custom {
                        namespace: _,
                        name,
                        param_types: _,
                        index: _,
                    } = &s.ast_type
                    {
                        let variant_const_name =
                            Self::variant_const_name(s.namespace(), name, &variant.name);

                        if variant_consts.contains(&variant_const_name) {
                            continue;
                        }
                        variant_consts.insert(variant_const_name.clone());

                        self.add(
                            &mut include,
                            &format!("extern struct RasmPointer_ *{variant_const_name};"),
                            None,
                            false,
                        );
                        self.add(
                            &mut before,
                            &format!("struct RasmPointer_ *{variant_const_name};"),
                            None,
                            false,
                        );
                    } else {
                        panic!();
                    }
                } else {
                    self.add(
                        &mut include,
                        &format!(
                            "struct {}_{}_{} {{",
                            s.namespace.safe_name(),
                            s.name,
                            variant.name
                        ),
                        None,
                        false,
                    );

                    for property in variant.parameters.iter() {
                        self.add(
                            &mut include,
                            &format!(
                                "{} {};",
                                CodeGenC::real_type_to_string(&property.ast_type),
                                property.name
                            ),
                            None,
                            true,
                        );
                    }

                    self.add(&mut include, "};", None, false);
                }
            }
        }

        if !typed_module.enums.is_empty() {
            self.add_empty_line(&mut include);
        }

        for s in typed_module.structs.iter() {
            self.add(
                &mut include,
                &format!("struct {}_{} {{", s.namespace.safe_name(), s.name),
                None,
                false,
            );
            for property in s.properties.iter() {
                self.add(
                    &mut include,
                    &format!(
                        "{} {};",
                        CodeGenC::real_type_to_string(&property.ast_type),
                        property.name
                    ),
                    None,
                    true,
                );
            }
            self.add(&mut include, "};", None, false);
        }

        if !typed_module.structs.is_empty() {
            self.add_empty_line(&mut include);
        }

        if let Some(clambdas) = statics.any::<CLambdas>() {
            for (_i, clambda) in clambdas.lambdas.values().enumerate() {
                self.add(
                    &mut include,
                    &format!("struct {} {{", clambda.name),
                    None,
                    false,
                );
                self.add(
                    &mut include,
                    "struct RasmPointer_ *lambda_space;",
                    None,
                    true,
                );
                let mut args = clambda
                    .args
                    .iter()
                    .map(|it| Self::real_type_to_string(it))
                    .collect::<Vec<_>>()
                    .join(", ");

                if !args.is_empty() {
                    args.push_str(", ");
                }

                self.add(
                    &mut include,
                    &format!(
                        "{} (*functionPtr)({args}struct RasmPointer_*);",
                        Self::real_type_to_string(&clambda.return_type)
                    ),
                    None,
                    true,
                );

                self.add(
                    &mut include,
                    "struct Void_* (*addref_function)(struct RasmPointer_ *);",
                    None,
                    true,
                );
                self.add(
                    &mut include,
                    "struct Void_* (*deref_function)(struct RasmPointer_ *);",
                    None,
                    true,
                );

                self.add(&mut include, "};", None, false);
                self.add_empty_line(&mut include);
            }
            self.add_empty_line(&mut include);
        }

        if let Some(cstructs) = statics.any::<CStructs>() {
            for cstruct in cstructs.structs.iter() {
                include.push_str(&cstruct.generate(&self.code_manipulator))
            }
        }

        if let Some(declarations) = statics.any::<CFunctionsDeclarations>() {
            for c in declarations.vec.iter() {
                self.add(&mut include, c, None, false);
            }
            self.add_empty_line(&mut include);
        }

        self.add_rows(
            &mut include,
            vec![
                "extern int argc_;",
                "extern char **argv_;",
                "extern struct RCTable *rasm_rc_table;",
            ],
            None,
            false,
        );

        self.add_rows(
            &mut before,
            vec![
                "int argc_;",
                "char **argv_;",
                "struct RCTable *rasm_rc_table;",
                "",
                "int main(int argc, char **argv)",
                "{",
                "    initRasmReferences();",
                "    argc_ = argc;",
                "    argv_ = argv;",
            ],
            None,
            false,
        );

        for s in typed_module.enums.iter() {
            for (i, variant) in s.variants.iter().enumerate() {
                if variant.parameters.is_empty() {
                    if let EnhASTType::Custom {
                        namespace: _,
                        name,
                        param_types: _,
                        index: _,
                    } = &s.ast_type
                    {
                        let variant_const_name =
                            Self::variant_const_name(s.namespace(), name, &variant.name);
                        self.add(
                            &mut before,
                            &format!("{variant_const_name} = rasmMalloc(sizeof(struct Enum));"),
                            None,
                            true,
                        );
                        self.add(
                            &mut before,
                            &format!(
                                "((struct Enum *){variant_const_name}->address)->variant_num = {};",
                                i
                            ),
                            None,
                            true,
                        );
                        self.add(
                            &mut before,
                            &format!("{variant_const_name}->count = 1;"),
                            None,
                            true,
                        );
                    } else {
                        panic!();
                    }
                }
            }
        }

        (include, before, after)
    }

    fn function_preamble(
        &self,
        _code_gen_context: &CodeGenCContext,
        _function_def: Option<&ASTTypedFunctionDef>,
        _out: &mut String,
    ) {
        // TODO
    }

    fn define_debug(&self, _out: &mut String) {
        todo!()
    }

    fn function_end(
        &self,
        _code_gen_context: &CodeGenCContext,
        out: &mut String,
        add_return: bool,
        function_def: Option<&ASTTypedFunctionDef>,
    ) {
        if add_return {
            if let Some(fd) = function_def {
                if matches!(fd.return_type, ASTTypedType::Unit) {
                    self.add(out, "return NULL;", None, true);
                } else if matches!(fd.body, ASTTypedFunctionBody::RASMBody(_)) {
                    self.add(out, "return return_value_;", None, true);
                }
            }
        }
        self.add(out, "}", None, false);
    }

    fn add_statics(&self, project: &RasmProject, statics: &mut Statics, out_folder: &Path) {
        project.all_projects().iter().for_each(|dependency| {
            if let Some(native_source_folder) = dependency.main_native_source_folder("c") {
                if native_source_folder.exists() {
                    WalkDir::new(native_source_folder)
                        .into_iter()
                        .filter_map(Result::ok)
                        .filter(|it| {
                            it.file_name().to_string_lossy().ends_with(".h")
                                || it.file_name().to_string_lossy().ends_with(".c")
                        })
                        .for_each(|it| {
                            CInclude::add_to_statics(
                                statics,
                                format!("\"{}\"", it.clone().file_name().to_string_lossy()),
                            );

                            let dest = out_folder
                                .to_path_buf()
                                .join(Path::new(it.file_name().to_string_lossy().as_ref()));

                            info!("including file {}", it.path().to_string_lossy());

                            fs::copy(it.clone().into_path(), dest).unwrap();
                        });
                }
            }
        });

        CLibAssets::iter()
            .filter(|it| it.ends_with(".h"))
            .for_each(|it| {
                let dest = out_folder.to_path_buf().join(it.to_string());
                info!("Including {}", dest.to_string_lossy());
                CInclude::add_to_statics(statics, format!("\"{it}\""));
                if let Some(asset) = CLibAssets::get(&it) {
                    fs::write(dest, asset.data).unwrap();
                } else {
                    panic!()
                }
            });
    }

    fn value_to_string(&self, statics: &mut Statics, value_type: &ASTValue) -> String {
        match value_type {
            ASTValue::ASTStringValue(v) => CStrings::add_to_statics(statics, v.to_owned()),
            ASTValue::ASTBooleanValue(b) => if *b { "1" } else { "0" }.to_string(),
            ASTValue::ASTIntegerValue(v) => format!("{v}"),
            ASTValue::ASTCharValue(v) => CStrings::add_to_statics(statics, v.to_owned()),
            ASTValue::ASTFloatValue(v) => format!("{v}"),
        }
    }

    fn create_function_definition(
        &self,
        _statics: &Statics,
        _function_def: &ASTTypedFunctionDef,
    ) -> bool {
        true
    }

    fn replace_inline_call_including_source(&self) -> bool {
        false
    }

    fn create_code_gen_context(&self) -> CodeGenCContext {
        CodeGenCContext {}
    }

    fn define_let(
        &'a self,
        _code_gen_context: &CodeGenCContext,
        _name: &str,
        _is_const: bool,
        _statics: &Statics,
        _namespace: &EnhASTNameSpace,
    ) {
    }

    fn code_manipulator(&self) -> &dyn CodeManipulator {
        &self.code_manipulator
    }

    fn type_def_body_target(&self) -> TypeDefBodyTarget {
        TypeDefBodyTarget::C
    }

    fn get_reference_type_name(&self, ast_type: &ASTTypedType) -> Option<String> {
        get_reference_type_name(ast_type, &TypeDefBodyTarget::C)
    }

    fn split_source(&self) -> usize {
        0
        //num_cpus::get_physical() - 2
    }

    fn include_file(&self, file: &str) -> String {
        format!("#include \"{file}\"")
    }

    fn set_let_for_ref_in_lambda_space(
        &self,
        _code_gen_context: &CodeGenCContext,
        _index_in_lambda_space: usize,
        before: &mut String,
        val_name: &String,
        typed_val_kind: &TypedValKind,
        statics: &Statics,
        name: &str,
    ) {
        self.code_manipulator.add(
            before,
            &format!(
                "{} {name} = lambda_space->{val_name};",
                Self::type_to_string(typed_val_kind.typed_type(), statics)
            ),
            None,
            true,
        );
    }

    fn set_let_for_function_ref(
        &self,
        code_gen_context: &CodeGenCContext,
        before: &mut String,
        val_name: &String,
        def: &mut ASTTypedFunctionDef,
        statics: &mut Statics,
        name: &str,
        id: &mut usize,
        function_reference_lambdas: &mut HashMap<String, LambdaCall>,
        typed_module: &ASTTypedModule,
        lambda_calls: &mut Vec<LambdaCall>,
    ) -> ASTTypedType {
        let mut fcp =
            self.function_call_parameters(code_gen_context, None, &Vec::new(), false, false, *id);
        if let Some(l) = function_reference_lambdas.get(val_name) {
            let lambda_type = ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: l
                    .def
                    .parameters
                    .iter()
                    .map(|it| it.ast_type.clone())
                    .collect::<Vec<_>>(),
                return_type: Box::new(l.def.return_type.clone()),
            });

            fcp.add_lambda(
                &mut l.def.clone(), // original def must not be changed
                None,
                &TypedValContext::new(None),
                Some(&format!("reference to function {val_name}")),
                statics,
                typed_module,
                code_gen_context,
                true, // TODO
                &lambda_type,
                &l.def.name,
            );

            before.push_str(&fcp.before());

            let lambda_var = fcp
                .parameters_values()
                .iter()
                .find(|it| it.0 == &def.name)
                .map(|it| it.1)
                .unwrap();

            before.push_str(&format!("struct RasmPointer_ *{name} = {lambda_var};"));

            return lambda_type;
        }

        let lambda_name = format!("lambda_{}", id);
        def.name = lambda_name.clone();
        *id += 1;

        let lambda_type = ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
            parameters: def
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect::<Vec<_>>(),
            return_type: Box::new(def.return_type.clone()),
        });

        let lambda_space = fcp.add_lambda(
            def,
            None,
            &TypedValContext::new(None),
            Some(&format!("reference to function {val_name}")),
            statics,
            typed_module,
            code_gen_context,
            true, // TODO
            &lambda_type,
            &lambda_name,
        );
        let lambda_call = LambdaCall {
            def: def.clone(),
            space: lambda_space,
        };

        function_reference_lambdas.insert(val_name.to_owned(), lambda_call.clone());
        lambda_calls.push(lambda_call);

        before.push_str(&fcp.before());

        let lambda_var = fcp
            .parameters_values()
            .iter()
            .find(|it| it.0 == &def.name)
            .map(|it| it.1)
            .unwrap();

        before.push_str(&format!("struct RasmPointer_ *{name} = {lambda_var};"));

        lambda_type
    }
}

pub fn value_type_to_enh_type(value_type: &ASTValue) -> EnhASTType {
    match value_type {
        ASTValue::ASTStringValue(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::String),
        ASTValue::ASTBooleanValue(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::Boolean),
        ASTValue::ASTIntegerValue(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::Integer),
        ASTValue::ASTCharValue(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::Char),
        ASTValue::ASTFloatValue(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::Float),
    }
}

pub fn value_type_to_typed_type(value_type: &ASTValue) -> ASTTypedType {
    match value_type {
        ASTValue::ASTStringValue(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
        ASTValue::ASTBooleanValue(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Boolean),
        ASTValue::ASTIntegerValue(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Integer),
        ASTValue::ASTCharValue(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
        ASTValue::ASTFloatValue(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Float),
    }
}

#[cfg(test)]
mod tests {
    use rasm_utils::SliceDisplay;

    use crate::codegen::c::code_gen_c::CodeGenC;
    use crate::codegen::c::options::COptions;
    use crate::codegen::enh_val_context::EnhValContext;
    use crate::codegen::statics::Statics;
    use crate::codegen::typedef_provider::DummyTypeDefProvider;
    use crate::codegen::CodeGen;

    #[test]
    fn called_functions() {
        let sut = CodeGenC::new(COptions::default(), false);
        let mut statics = Statics::new();

        let functions = sut
            .called_functions(
                None,
                None,
                //"$call(Option::Some, value:T)",
                "$call(Option::None<T>)",
                &EnhValContext::new(None),
                &DummyTypeDefProvider::empty(),
                &mut statics,
            )
            .unwrap();

        println!(
            "called functions {}",
            SliceDisplay(&functions.iter().map(|it| &it.1).collect::<Vec<_>>())
        );

        assert!(!functions.is_empty());
    }

    #[test]
    fn test_escape() {
        assert_eq!(CodeGenC::escape_string("a\"a"), "a\\\"a");
    }
}
