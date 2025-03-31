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

use crate::codegen::c::any::{CConsts, CFunctionsDeclarations, CInclude, CLambdas, CStructs};
use crate::codegen::c::function_call_parameters_c::CFunctionCallParameters;
use crate::codegen::c::options::COptions;
use crate::codegen::c::text_macro_c::{
    CCallMacro, CEnumDeclarationMacro, CEnumVariantAssignmentMacro, CEnumVariantDeclarationMacro,
    CIncludeMacro, CStructDeclarationMacro, CStructTypeMacro,
};
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::enh_ast::{EnhASTIndex, EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind};
use crate::codegen::enh_val_context::TypedValContext;
use crate::codegen::function_call_parameters::FunctionCallParameters;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{RefType, TextMacroEval, TextMacroEvaluator};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{CodeGen, TypedValKind};
use crate::enh_type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef,
    ASTTypedModule, ASTTypedParameterDef, ASTTypedType, BuiltinTypedTypeKind, CustomTypedTypeDef,
};
use crate::transformations::typed_functions_creator::struct_has_references;
use linked_hash_map::LinkedHashMap;
use rasm_parser::parser::ast::ASTValueType;

use super::text_macro_c::{CAddRefMacro, CCastAddress, CEnumSimpleMacro, CTypeNameMacro};
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
        self.add(out, &format!("// {comment}"), None, indent);
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
                BuiltinTypedTypeKind::I32 => "int".to_string(),
                BuiltinTypedTypeKind::Bool => "int".to_string(),
                BuiltinTypedTypeKind::Char => "char*".to_string(),
                BuiltinTypedTypeKind::F32 => "float".to_string(),
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
                namespace,
                name,
                native_type,
                is_ref: _,
            } => native_type
                .clone()
                .unwrap_or_else(|| {
                    panic!("type in C must define a native type: {namespace}:{name}")
                })
                .to_string(),
        }
    }

    pub fn real_type_to_string(ast_type: &ASTTypedType) -> String {
        match ast_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => "char*".to_string(),
                BuiltinTypedTypeKind::I32 => "int".to_string(),
                BuiltinTypedTypeKind::Bool => "int".to_string(),
                BuiltinTypedTypeKind::Char => "char*".to_string(),
                BuiltinTypedTypeKind::F32 => "float".to_string(),
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
                namespace,
                name,
                native_type,
                is_ref,
            } => {
                if *is_ref {
                    "struct RasmPointer_*".to_string()
                } else {
                    native_type
                        .clone()
                        .unwrap_or_else(|| {
                            panic!("type in C must define a native type: {namespace}:{name}")
                        })
                        .to_string()
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
        &self,
        out: &mut String,
        source: &str,
        type_name: &str,
        descr_for_debug: &str,
        type_def_provider: &dyn TypeDefProvider,
    ) {
        let (has_references, is_type, is_static) =
            if let Some(struct_def) = type_def_provider.get_struct_def_by_name(type_name) {
                (
                    struct_has_references(struct_def, type_def_provider),
                    false,
                    false,
                )
            } else if let Some(enum_def) = type_def_provider.get_enum_def_by_name(type_name) {
                let enum_has_parametric_variants = Self::enum_has_parametric_variants(enum_def);
                (
                    enum_has_parametric_variants,
                    false,
                    !enum_has_parametric_variants,
                )
            } else if let Some(type_def) = type_def_provider.get_type_def_by_name(type_name) {
                (type_def.is_ref, true, false)
            } else if "str" == type_name || "_fn" == type_name {
                (false, false, false)
            } else {
                panic!("call_add_ref, cannot find type {type_name}");
            };

        if has_references {
            if is_type {
                // it has an extra argument for description
                self.add(
                    out,
                    &format!("{type_name}_addRef({source}, \"\");"),
                    Some(descr_for_debug),
                    true,
                );
            } else {
                self.add(
                    out,
                    &format!("{type_name}_addRef({source});"),
                    Some(descr_for_debug),
                    true,
                );
            }
        } else {
            if "_fn" == type_name {
                self.add(out, &source, None, true);
            // TODO handle str, for now it's not possible since there's no difference,
            //   between heap ans static allocated strings
            } else if "str" != type_name && !is_static {
                self.call_add_ref_simple(out, source, descr_for_debug);
            }
        }
    }

    pub fn call_add_ref_simple(&self, out: &mut String, source: &str, descr_for_debug: &str) {
        self.add(
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
        &self,
        out: &mut String,
        source: &str,
        type_name: &str,
        descr_for_debug: &str,
        type_def_provider: &dyn TypeDefProvider,
    ) {
        let (has_references, is_type, is_static) =
            if let Some(struct_def) = type_def_provider.get_struct_def_by_name(type_name) {
                (
                    struct_has_references(struct_def, type_def_provider),
                    false,
                    false,
                )
            } else if let Some(enum_def) = type_def_provider.get_enum_def_by_name(type_name) {
                let enum_has_parametric_variants = Self::enum_has_parametric_variants(enum_def);
                (
                    enum_has_parametric_variants,
                    false,
                    !enum_has_parametric_variants,
                )
            } else if let Some(type_def) = type_def_provider.get_type_def_by_name(type_name) {
                (type_def.is_ref, true, false)
            } else if "str" == type_name || "_fn" == type_name {
                (false, false, false)
            } else {
                panic!("call_add_ref, cannot find type {type_name}");
            };

        if has_references {
            if is_type {
                // it has an extra argument for description
                self.add(
                    out,
                    &format!("{type_name}_deref({source}, \"\");"),
                    Some(descr_for_debug),
                    true,
                );
            } else {
                self.add(
                    out,
                    &format!("{type_name}_deref({source});"),
                    Some(descr_for_debug),
                    true,
                );
            }
        } else {
            if "_fn" == type_name {
                self.add(out, &source, None, true);
            // TODO handle str, for now it's not possible since there's no difference,
            //   between heap and static allocated strings
            } else if "str" != type_name && !is_static {
                self.call_deref_simple(out, source, descr_for_debug);
            }
        }
    }

    pub fn call_deref_simple(&self, out: &mut String, source: &str, descr_for_debug: &str) {
        self.add(
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

    fn transform_before(&self, stack: &CodeGenCContext, before: String) -> String {
        before
    }

    fn create_command_line_arguments(&self, generated_code: &mut String) {}

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
            .map(|(name, value)| (value.as_str(), None))
            .collect::<Vec<_>>();
        args.push((function_call.function_name.as_str(), None));

        if return_value {
            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters,
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

    fn call_lambda(
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
            .map(|(name, value)| (value.as_str(), None))
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

    fn store_function_result_in_stack(
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

    fn add_ref(
        &self,
        name: &str,
        statics: &mut Statics,
        body: &mut String,
        typed_module: &ASTTypedModule,
        index: &EnhASTIndex,
        type_name: &String,
    ) {
        self.call_add_ref(body, name, type_name, &type_name, typed_module);
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
                &mut result,
                "deref",
                name,
                t,
                typed_module,
                self,
                statics,
            );
        } else {
            self.call_deref(&mut result, name, type_name, "", typed_module);
        }

        result
    }

    fn call_add_ref_for_let_val(
        &self,
        _code_gen_context: &CodeGenCContext,
        name: &str,
        index: &EnhASTIndex,
        before: &mut String,
        statics: &mut Statics,
        type_name: &String,
        typed_module: &ASTTypedModule,
        t: &ASTTypedType,
    ) {
        if type_name == "_fn" {
            TypedFunctionsCreatorC::addref_deref_lambda(
                before,
                "addRef",
                name,
                t,
                typed_module,
                self,
                statics,
            );
        } else {
            self.call_add_ref(before, name, type_name, "", typed_module);
        }
    }

    fn set_let_const_for_function_call_result(
        &self,
        statics_key: &str,
        before: &mut String,
        _current: &mut String,
        name: &str,
        typed_type: &ASTTypedType,
        statics: &mut Statics,
    ) {
        // TODO should be const? But in this way I get a warning. Should all pointer be consts? But can we release them?
        CConsts::add_to_statics(
            statics,
            format!("{} {name};", CodeGenC::real_type_to_string(typed_type)),
        );
        self.add(before, &format!("{name} = ",), None, true);
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
        _body: &mut String,
        value: &String,
        _typed_type: &ASTTypedType,
    ) {
        if is_const {
            CConsts::add_to_statics(
                statics,
                format!("char* {name} = \"{}\";", Self::escape_string(value)),
            );
        } else {
            self.add(
                before,
                &format!("char* {name} = \"{}\";", Self::escape_string(value)),
                None,
                true,
            );
        }
    }

    fn set_let_for_value(
        &self,
        code_gen_context: &CodeGenCContext,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        value_type: &ASTValueType,
        typed_type: &ASTTypedType,
    ) {
        let value = self.value_to_string(value_type);

        if is_const {
            CConsts::add_to_statics(
                statics,
                format!(
                    "{} {name} = {};",
                    CodeGenC::type_to_string(typed_type, statics),
                    value
                ),
            );
        } else {
            self.add(
                before,
                &format!(
                    "{} {name} = {};",
                    CodeGenC::type_to_string(typed_type, statics),
                    value
                ),
                None,
                true,
            );
        }
    }

    fn reserve_return_register(&self, _code_gen_context: &CodeGenCContext, _out: &mut String) {
        // TODO
    }

    fn function_def(
        &'a self,
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

        let inline =
            function_def.inline || &function_def.name == "addRef" || &function_def.name == "deref";

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
                "{} {}({});",
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

    fn value_as_return(
        &self,
        before: &mut String,
        value_type: &ASTValueType,
        statics: &mut Statics,
    ) {
        if let ASTValueType::String(s) = value_type {
            self.string_literal_return(statics, before, s);
        } else {
            let v = self.value_to_string(value_type);
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
        self.code_manipulator.add(
            before,
            &format!("char* return_value_ = \"{value}\";"),
            None,
            true,
        );
    }

    fn get_text_macro_evaluator(&self) -> TextMacroEvaluator {
        let mut evaluators: LinkedHashMap<String, Box<dyn TextMacroEval>> = LinkedHashMap::new();
        evaluators.insert("call".to_string(), Box::new(CCallMacro));
        evaluators.insert("include".to_string(), Box::new(CIncludeMacro));
        evaluators.insert(
            "structDeclaration".to_string(),
            Box::new(CStructDeclarationMacro),
        );
        evaluators.insert("structType".to_string(), Box::new(CStructTypeMacro));
        evaluators.insert(
            "enumVariantDeclaration".to_string(),
            Box::new(CEnumVariantDeclarationMacro),
        );
        evaluators.insert(
            "enumDeclaration".to_string(),
            Box::new(CEnumDeclarationMacro),
        );

        evaluators.insert(
            "enumVariantAssignment".to_string(),
            Box::new(CEnumVariantAssignmentMacro),
        );

        evaluators.insert(
            "addRef".to_string(),
            Box::new(CAddRefMacro::new(self.clone(), RefType::AddRef, true)),
        );

        evaluators.insert(
            "deref".to_string(),
            Box::new(CAddRefMacro::new(self.clone(), RefType::Deref, true)),
        );

        evaluators.insert("typeName".to_string(), Box::new(CTypeNameMacro::new()));
        evaluators.insert("castAddress".to_string(), Box::new(CCastAddress::new()));
        evaluators.insert("enumSimple".to_string(), Box::new(CEnumSimpleMacro::new()));
        TextMacroEvaluator::new(evaluators, Box::new(CCodeManipulator::new()))
    }

    fn print_memory_info(&self, native_code: &mut String, statics: &Statics) {
        todo!()
    }

    fn optimize_unused_functions(&self) -> bool {
        false // TODO
    }

    fn initialize_static_values(&self, generated_code: &mut String) {
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
        statics: &Statics,
    ) {
        let args = call_parameters
            .unwrap()
            .parameters_values()
            .iter()
            .map(|(name, value)| (value.as_str(), None))
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
        comment: Option<&str>,
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

    fn add_comment(&self, out: &mut String, comment: &str, indent: bool) {
        self.code_manipulator.add_comment(out, comment, indent);
    }

    fn add_rows(&self, out: &mut String, code: Vec<&str>, comment: Option<&str>, indent: bool) {
        self.code_manipulator.add_rows(out, code, comment, indent);
    }

    fn add(&self, out: &mut String, code: &str, comment: Option<&str>, indent: bool) {
        self.code_manipulator.add(out, code, comment, indent);
    }

    fn add_empty_line(&self, out: &mut String) {
        self.code_manipulator.add_empty_line(out);
    }

    fn remove_comments_from_line(&self, line: String) -> String {
        self.code_manipulator.remove_comments_from_line(line)
    }

    fn preamble(&self, code: &mut String) {}

    fn reserve_local_vals(&self, _code_gen_context: &CodeGenCContext, _out: &mut String) {}

    fn generate_statics_code(
        &self,
        statics: &Statics,
        typed_module: &ASTTypedModule,
    ) -> (String, String) {
        let mut before = String::new();
        let after = String::new();

        if let Some(includes) = statics.any::<CInclude>() {
            for inc in includes.unique() {
                self.add(&mut before, &format!("#include {inc}"), None, false);
            }
            self.add_empty_line(&mut before);
        }

        if self.debug {
            self.add(&mut before, "#define __RASM_DEBUG__", None, false);
        }

        if let Some(consts) = statics.any::<CConsts>() {
            for c in consts.vec.iter() {
                self.add(&mut before, c, None, false);
            }
            self.add_empty_line(&mut before);
        }

        for s in typed_module.enums.iter() {
            for variant in s.variants.iter() {
                if variant.parameters.is_empty() {
                    if let EnhASTType::Custom {
                        namespace,
                        name,
                        param_types,
                        index,
                    } = &s.ast_type
                    {
                        let variant_const_name =
                            Self::variant_const_name(s.namespace(), name, &variant.name);
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
                        &mut before,
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
                            &mut before,
                            &format!(
                                "{} {};",
                                CodeGenC::real_type_to_string(&property.ast_type),
                                property.name
                            ),
                            None,
                            true,
                        );
                    }

                    self.add(&mut before, "};", None, false);
                }
            }
        }

        if !typed_module.enums.is_empty() {
            self.add_empty_line(&mut before);
        }

        for s in typed_module.structs.iter() {
            self.add(
                &mut before,
                &format!("struct {}_{} {{", s.namespace.safe_name(), s.name),
                None,
                false,
            );
            for property in s.properties.iter() {
                self.add(
                    &mut before,
                    &format!(
                        "{} {};",
                        CodeGenC::real_type_to_string(&property.ast_type),
                        property.name
                    ),
                    None,
                    true,
                );
            }
            self.add(&mut before, "};", None, false);
        }

        if !typed_module.structs.is_empty() {
            self.add_empty_line(&mut before);
        }

        if let Some(clambdas) = statics.any::<CLambdas>() {
            for (i, clambda) in clambdas.lambdas.values().enumerate() {
                self.add(
                    &mut before,
                    &format!("struct {} {{", clambda.name),
                    None,
                    false,
                );
                self.add(
                    &mut before,
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
                    &mut before,
                    &format!(
                        "{} (*functionPtr)({args}struct RasmPointer_*);",
                        Self::real_type_to_string(&clambda.return_type)
                    ),
                    None,
                    true,
                );

                self.add(
                    &mut before,
                    "struct Void_* (*addref_function)(struct RasmPointer_ *);",
                    None,
                    true,
                );
                self.add(
                    &mut before,
                    "struct Void_* (*deref_function)(struct RasmPointer_ *);",
                    None,
                    true,
                );

                self.add(&mut before, "};", None, false);
                self.add_empty_line(&mut before);
            }
            self.add_empty_line(&mut before);
        }

        if let Some(cstructs) = statics.any::<CStructs>() {
            for cstruct in cstructs.structs.iter() {
                before.push_str(&cstruct.generate(&self.code_manipulator))
            }
        }

        if let Some(declarations) = statics.any::<CFunctionsDeclarations>() {
            for c in declarations.vec.iter() {
                self.add(&mut before, c, None, false);
            }
            self.add_empty_line(&mut before);
        }

        self.add_rows(
            &mut before,
            vec![
                "static int argc_;",
                "static char **argv_;",
                "static struct RCTable *rasm_rc_table;",
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
                        namespace,
                        name,
                        param_types,
                        index,
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

        (before, after)
    }

    fn function_preamble(&self, out: &mut String) {
        // TODO
    }

    fn define_debug(&self, out: &mut String) {
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
                if matches!(fd.body, ASTTypedFunctionBody::RASMBody(_))
                    && !matches!(fd.return_type, ASTTypedType::Unit)
                {
                    self.add(out, "return return_value_;", None, true);
                }
            }
        }
        self.add(out, "}", None, false);
    }

    fn add_statics(&self, statics: &mut Statics) {
        /*
        let mut map = LinkedHashMap::new();
        map.insert("address".to_string(), "void *".to_string());
        map.insert("count".to_string(), "int".to_string());
        CStructs::add_struct_to_statics(statics, "RasmPointer".to_string(), map)
        */
    }

    fn value_to_string(&self, value_type: &ASTValueType) -> String {
        match value_type {
            ASTValueType::String(v) => format!("\"{}\"", CodeGenC::escape_string(&v)),
            ASTValueType::Boolean(b) => if *b { "1" } else { "0" }.to_string(),
            ASTValueType::I32(v) => format!("{v}"),
            ASTValueType::Char(v) => format!("\"{}\"", CodeGenC::escape_string(&v.to_string())),
            ASTValueType::F32(v) => format!("{v}"),
        }
    }

    fn create_function_definition(&self, _function_def: &ASTTypedFunctionDef) -> bool {
        true
    }

    fn replace_inline_call_including_source(&self) -> bool {
        false
    }

    fn create_code_gen_context(&self) -> CodeGenCContext {
        CodeGenCContext {}
    }

    fn define_let(&'a self, _code_gen_context: &CodeGenCContext, _name: &str, _is_const: bool) {}
}

pub fn value_type_to_enh_type(value_type: &ASTValueType) -> EnhASTType {
    match value_type {
        ASTValueType::String(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::String),
        ASTValueType::Boolean(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::Bool),
        ASTValueType::I32(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::I32),
        ASTValueType::Char(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::Char),
        ASTValueType::F32(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::F32),
    }
}

pub fn value_type_to_typed_type(value_type: &ASTValueType) -> ASTTypedType {
    match value_type {
        ASTValueType::String(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
        ASTValueType::Boolean(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Bool),
        ASTValueType::I32(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::I32),
        ASTValueType::Char(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
        ASTValueType::F32(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::F32),
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
                &DummyTypeDefProvider::new(),
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
