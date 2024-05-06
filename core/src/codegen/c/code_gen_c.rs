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

use crate::codegen::c::function_call_parameters::CFunctionCallParameters;
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::lambda::{LambdaCall, LambdaSpace};
use crate::codegen::stack::StackVals;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::TextMacro;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::val_context::{TypedValContext, ValContext};
use crate::codegen::{AsmOptions, CodeGen, TypedValKind};
use crate::parser::ast::{ASTFunctionDef, ASTIndex, ASTNameSpace, ValueType};
use crate::type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedType, BuiltinTypedTypeKind, DefaultFunctionCall,
};
use std::collections::HashMap;

#[derive(Clone)]
pub struct CodeManipulatorC;

impl CodeManipulatorC {
    pub fn new() -> Self {
        Self {}
    }
}

impl CodeManipulator for CodeManipulatorC {
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

pub struct CodeGenC {
    code_manipulator: CodeManipulatorC,
    options: AsmOptions,
}

impl CodeGenC {
    pub fn new() -> Self {
        Self {
            code_manipulator: CodeManipulatorC,
            options: AsmOptions::default(),
        }
    }

    fn type_to_string(ast_type: &ASTTypedType) -> String {
        match ast_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => "char*".to_string(),
                BuiltinTypedTypeKind::I32 => "int".to_string(),
                BuiltinTypedTypeKind::Bool => "int".to_string(),
                BuiltinTypedTypeKind::Char => "char".to_string(),
                BuiltinTypedTypeKind::F32 => "float".to_string(),
                _ => todo!("{kind:?}"),
            },
            ASTTypedType::Unit => "void".to_string(),
            _ => todo!("{ast_type}"),
        }
    }
}

impl<'a> CodeGen<'a, Box<CFunctionCallParameters>> for CodeGenC {
    fn options(&self) -> &AsmOptions {
        &self.options // TODO
    }

    fn end_main(&self, code: &mut String) {}

    fn transform_before(&self, stack: &StackVals, before: String) -> String {
        before
    }

    fn create_command_line_arguments(&self, generated_code: &mut String) {
        // TODO
    }

    fn translate_static_code(&self, static_code: String, typed_module: &ASTTypedModule) -> String {
        // TODO
        static_code
    }

    fn call_lambda_parameter(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        kind: &TypedValKind,
    ) {
        todo!()
    }

    fn call_lambda(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        stack_vals: &StackVals,
        index_in_lambda_space: usize,
    ) {
        todo!()
    }

    fn restore_stack(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        call_parameters: &mut Box<CFunctionCallParameters>,
    ) {
        todo!()
    }

    fn added_to_stack_for_call_parameter(
        &self,
        added_to_stack: &String,
        call_parameters: &Box<CFunctionCallParameters>,
    ) -> String {
        todo!()
    }

    fn function_call_parameters<'b, 'c>(
        &self,
        parameters: &'b Vec<ASTTypedParameterDef>,
        inline: bool,
        immediate: bool,
        stack_vals: &'c StackVals,
        id: usize,
    ) -> Box<CFunctionCallParameters> {
        Box::new(CFunctionCallParameters::new(parameters.clone()))
    }

    fn store_function_result_in_stack(&self, code: &mut String, address_relative_to_bp: i32) {
        todo!()
    }

    fn add_ref(
        &self,
        name: &&str,
        statics: &mut Statics,
        body: &mut String,
        typed_module: &ASTTypedModule,
        index: &ASTIndex,
        type_name: &String,
    ) {
    }

    fn call_deref_for_let_val(
        &self,
        name: &str,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
    ) -> String {
        "// call_deref_for_let_val".to_string()
    }

    fn call_add_ref_for_let_val(
        &self,
        name: &str,
        index: &ASTIndex,
        before: &mut String,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
    ) {
        todo!()
    }

    fn set_let_const_for_function_call_result(&self, statics_key: &str, body: &mut String) {
        todo!()
    }

    fn set_let_for_value_ref(
        &self,
        stack: &StackVals,
        before: &mut String,
        address_relative_to_bp: usize,
        val_name: &String,
        typed_val_kind: &TypedValKind,
    ) -> ASTTypedType {
        todo!()
    }

    fn set_let_for_string_literal(
        &self,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        address_relative_to_bp: usize,
        value: &String,
        typed_type: &ASTTypedType,
        stack: &StackVals,
    ) {
        if is_const {
            statics.add_custom(
                "const".to_string(),
                format!("const char* {name} = \"{value}\";"),
            );
        } else {
            todo!()
        }
    }

    fn set_let_for_value(
        &self,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        address_relative_to_bp: usize,
        value_type: &ValueType,
        typed_type: &ASTTypedType,
    ) {
        todo!()
    }

    fn reserve_return_register(&self, out: &mut String, stack: &StackVals) {
        // TODO
    }

    fn function_def(&'a self, out: &mut String, function_def: &ASTTypedFunctionDef) {
        let mut args = Vec::new();
        for par in function_def.parameters.iter() {
            let arg_type = Self::type_to_string(&par.ast_type);
            args.push(format!("{arg_type} {}", par.name));
        }

        self.add(
            out,
            &format!(
                "{} {}({}) {{",
                Self::type_to_string(&function_def.return_type),
                function_def.name,
                args.join(", ")
            ),
            None,
            false,
        );
    }

    fn word_len(&self) -> usize {
        todo!()
    }

    fn stack_pointer(&self) -> &str {
        todo!()
    }

    fn word_size(&self) -> &str {
        todo!()
    }

    fn reserve_lambda_space(&self, before: &mut String, stack: &StackVals) {
        todo!()
    }

    fn value_as_return(&self, before: &mut String, v: &str) {
        todo!()
    }

    fn string_literal_return(&self, statics: &mut Statics, before: &mut String, value: &String) {
        todo!()
    }

    fn generate_call_function(
        &self,
        namespace: &ASTNameSpace,
        function_call: &ASTTypedFunctionCall,
        context: &TypedValContext,
        parent_def: Option<&ASTTypedFunctionDef>,
        added_to_stack: String,
        lambda_space: Option<&LambdaSpace>,
        indent: usize,
        is_lambda: bool,
        stack_vals: &StackVals,
        id: &mut usize,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
        is_last: bool,
        is_inner_call: bool,
    ) -> (String, Vec<String>, Vec<LambdaCall>) {
        let mut before = String::new();
        let mut after = Vec::new();
        let mut lambda_calls = Vec::new();

        let mut arg_values = Vec::new();
        for expression in function_call.parameters.iter() {
            match expression {
                ASTTypedExpression::StringLiteral(s) => arg_values.push(format!("\"{s}\"")),
                ASTTypedExpression::ValueRef(name, index) => arg_values.push(name.to_string()),
                ASTTypedExpression::Value(value_type, index) => match value_type {
                    ValueType::Boolean(b) => {
                        arg_values.push((if *b { "0" } else { "1" }).to_string())
                    }
                    ValueType::I32(i) => arg_values.push(format!("{i}")),
                    ValueType::Char(c) => arg_values.push(format!("'{c}'")),
                    ValueType::F32(f) => arg_values.push(format!("{f}")),
                },
                ASTTypedExpression::ASTFunctionCallExpression(innerCall) => {
                    let (bf, af, mut inner_lambda_calls) = self.generate_call_function(
                        namespace,
                        innerCall,
                        context,
                        parent_def,
                        added_to_stack.clone(),
                        lambda_space,
                        indent,
                        is_lambda,
                        stack_vals,
                        id,
                        statics,
                        typed_module,
                        false,
                        true,
                    );

                    lambda_calls.extend(inner_lambda_calls);

                    arg_values.push(bf.trim().replace('\n', ""));
                }
                _ => {
                    println!("call {function_call}, expression {expression}");
                    todo!()
                }
            }
        }

        let mut code = String::new();

        let return_type = if let Some(function_def) = typed_module
            .functions_by_name
            .get(&function_call.function_name)
        {
            &function_def.return_type
        } else {
            panic!("Cannot determine function return type");
        };

        if is_last && !return_type.is_unit() {
            code.push_str("return ");
        }

        code.push_str(&format!(
            "{}({})",
            function_call.function_name,
            arg_values.join(", ")
        ));

        if !is_inner_call {
            code.push(';');
        }

        self.add(&mut before, &code, None, true);

        (before, after, lambda_calls)
    }

    fn create_lambdas(
        &self,
        lambdas: Vec<LambdaCall>,
        indent: usize,
        id: &mut usize,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
    ) -> HashMap<String, (String, String)> {
        if !lambdas.is_empty() {
            todo!()
        } else {
            HashMap::new()
        }
    }

    fn translate_body(
        &self,
        body: String,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
    ) -> Result<String, String> {
        // TODO
        Ok(body)
    }

    fn print_memory_info(&self, native_code: &mut String) {
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

    fn call_function_simple(&self, out: &mut String, function_name: &str) {
        todo!()
    }

    fn call_function(
        &self,
        out: &mut String,
        function_name: &str,
        args: &[(&str, Option<&str>)],
        comment: Option<&str>,
    ) {
        let arg_vec = args
            .to_vec()
            .iter()
            .map(|it| it.0.to_string())
            .collect::<Vec<_>>();

        self.add(
            out,
            &format!("{function_name}({});", arg_vec.join(", ")),
            None,
            true,
        );
    }

    fn call_function_owned(
        &self,
        out: &mut String,
        function_name: &str,
        args: &[(String, Option<String>)],
        comment: Option<&str>,
    ) {
        todo!()
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

    fn create_lambda_add_ref_like_function(
        &self,
        namespace: &ASTNameSpace,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
        is_deref: bool,
    ) -> Option<ASTTypedFunctionDef> {
        todo!()
    }

    fn called_functions(
        &self,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&ASTFunctionDef>,
        body: &str,
        context: &ValContext,
        type_def_provider: &dyn TypeDefProvider,
        _statics: &mut Statics,
    ) -> Result<Vec<(TextMacro, DefaultFunctionCall)>, String> {
        todo!()
    }

    fn reserve_local_vals(&self, stack: &StackVals, out: &mut String) {
        // TODO
    }

    fn generate_statics_code(&self, statics: &Statics) -> (String, String) {
        // TODO
        let mut before = String::new();
        let mut after = String::new();

        if let Some(includes) = statics.custom().get("include") {
            let mut includes = includes.clone();
            includes.sort();
            includes.dedup();

            for inc in includes {
                self.add(&mut before, &format!("#include {inc}"), None, false);
            }
        }

        if let Some(consts) = statics.custom().get("const") {
            for c in consts {
                self.add(&mut before, &format!("{c}"), None, false);
            }
        }

        self.add(&mut before, "int main()", None, false);
        self.add(&mut before, "{", None, false);

        (before, after)
    }

    fn function_preamble(&self, out: &mut String) {
        // TODO
    }

    fn define_debug(&self, out: &mut String) {
        todo!()
    }

    fn restore(&self, stack: &StackVals, out: &mut String) {
        // TODO
    }

    fn function_end(&self, out: &mut String, add_return: bool) {
        self.add(out, "}", None, false);
    }

    fn add_statics(&self, statics: &mut Statics) {
        // TODO
    }

    fn value_to_string(&self, value_type: &ValueType) -> String {
        todo!()
    }
}
