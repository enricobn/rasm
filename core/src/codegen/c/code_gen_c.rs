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
    ASTTypedParameterDef, ASTTypedType, DefaultFunctionCall,
};
use std::collections::HashMap;

pub struct CodeGenC;

impl CodeGenC {
    /*
    pub fn generate(&self, typed_module: &ASTTypedModule, statics: Statics) -> String {
        let mut result = String::new();

        let custom = statics.custom();
        if let Some(includes) = custom.get("include") {
            let mut cloned_includes = includes.clone();
            cloned_includes.sort();
            cloned_includes.dedup();

            for i in cloned_includes {
                result.push_str(&format!("#include {i}\n"));
            }

            if !includes.is_empty() {
                result.push('\n');
            }
        }

        result.push_str("int main () {\n");
        result.push_str("  printf(\"%s\", \"Hello world\\n\");\n");
        result.push_str("  return 0;\n");
        result.push_str("}\n");
        result
    }

     */
}

impl<'a> CodeGen<'a, Box<crate::codegen::c::function_call_parameters::CFunctionCallParameters>>
    for CodeGenC
{
    fn options(&self) -> &AsmOptions {
        todo!()
    }

    fn create_command_line_arguments(&self, generated_code: &mut String) {
        todo!()
    }

    fn translate_static_code(&self, static_code: &String, typed_module: &ASTTypedModule) -> String {
        todo!()
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
        &'a self,
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
        &'a self,
        parameters: &'b Vec<ASTTypedParameterDef>,
        inline: bool,
        immediate: bool,
        stack_vals: &'c StackVals,
        id: usize,
    ) -> Box<CFunctionCallParameters> {
        todo!()
    }

    fn add_let(
        &self,
        namespace: &ASTNameSpace,
        context: &mut TypedValContext,
        stack: &StackVals,
        after: &mut String,
        before: &mut String,
        name: &str,
        expr: &ASTTypedExpression,
        function_def: Option<&ASTTypedFunctionDef>,
        lambda_space: Option<&LambdaSpace>,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        id: &mut usize,
        typed_module: &ASTTypedModule,
    ) -> Vec<LambdaCall> {
        println!("let {name}");
        todo!()
    }

    fn call_deref_for_let_val(
        &self,
        name: &str,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
    ) -> String {
        todo!()
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
        todo!()
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

    fn reserve_return_register(&'a self, out: &mut String, stack: &StackVals) {
        todo!()
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

    fn reserve_lambda_space(&'a self, before: &mut String, stack: &StackVals) {
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
    ) -> (String, Vec<String>, Vec<LambdaCall>) {
        todo!()
    }

    fn create_lambdas(
        &self,
        lambdas: Vec<LambdaCall>,
        indent: usize,
        id: &mut usize,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
    ) -> HashMap<String, (String, String)> {
        todo!()
    }

    fn create_all_functions(
        &self,
        id: &mut usize,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
    ) -> HashMap<String, (String, String)> {
        todo!()
    }

    fn translate_body(
        &self,
        body: &str,
        statics: &mut Statics,
        typed_module: &ASTTypedModule,
    ) -> Result<String, String> {
        todo!()
    }

    fn get_used_functions(
        &self,
        functions_asm: &HashMap<String, (String, String)>,
        native_code: &str,
        typed_module: &ASTTypedModule,
    ) -> Vec<(String, (String, String))> {
        todo!()
    }

    fn print_memory_info(&self, native_code: &mut String) {
        todo!()
    }

    fn initialize_static_values(&self, generated_code: &mut String) {
        todo!()
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
        todo!()
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
        todo!()
    }

    fn add_rows(&self, out: &mut String, code: Vec<&str>, comment: Option<&str>, indent: bool) {
        todo!()
    }

    fn add(&self, out: &mut String, code: &str, comment: Option<&str>, indent: bool) {
        todo!()
    }

    fn add_empty_line(&self, out: &mut String) {
        todo!()
    }

    fn remove_comments_from_line(&self, line: String) -> String {
        todo!()
    }

    fn preamble(&self, code: &mut String) {
        todo!()
    }

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
        todo!()
    }

    fn generate_statics_code(&self, statics: &Statics) -> (String, String) {
        todo!()
    }

    fn function_preamble(&self, out: &mut String) {
        todo!()
    }

    fn define_debug(&self, out: &mut String) {
        todo!()
    }

    fn restore(&self, stack: &StackVals, out: &mut String) {
        todo!()
    }

    fn function_end(&self, out: &mut String, add_return: bool) {
        todo!()
    }

    fn add_statics(&self, statics: &mut Statics) {
        todo!()
    }

    fn value_to_string(&self, value_type: &ValueType) -> String {
        todo!()
    }
}
