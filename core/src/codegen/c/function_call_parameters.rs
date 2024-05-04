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

use crate::codegen::function_call_parameters::FunctionCallParameters;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::stack::StackVals;
use crate::codegen::statics::Statics;
use crate::codegen::val_context::TypedValContext;
use crate::parser::ast::{ASTIndex, ValueType};
use crate::type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedType,
};
use linked_hash_map::LinkedHashMap;
use log::debug;

pub struct CFunctionCallParameters {
    parameters: Vec<ASTTypedParameterDef>,
    parameters_values: LinkedHashMap<String, String>,
}

impl CFunctionCallParameters {
    pub fn new(parameters: Vec<ASTTypedParameterDef>) -> Self {
        Self {
            parameters,
            parameters_values: LinkedHashMap::new(),
        }
    }
}

impl FunctionCallParameters for CFunctionCallParameters {
    fn add_label(&mut self, param_name: &str, label: String, comment: Option<&str>) {
        todo!()
    }

    fn add_function_call(
        &mut self,
        module: &ASTTypedModule,
        comment: &str,
        param_type: ASTTypedType,
        statics: &mut Statics,
    ) {
        todo!()
    }

    fn add_lambda(
        &mut self,
        def: &ASTTypedFunctionDef,
        parent_lambda_space: Option<&LambdaSpace>,
        context: &TypedValContext,
        comment: Option<&str>,
        statics: &mut Statics,
        module: &ASTTypedModule,
        stack_vals: &StackVals,
        optimize: bool,
    ) -> LambdaSpace {
        todo!()
    }

    fn add_parameter_ref(
        &mut self,
        original_param_name: String,
        val_name: &str,
        index_in_context: usize,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
        stack_vals: &StackVals,
    ) {
        todo!()
    }

    fn add_let_val_ref(
        &mut self,
        original_param_name: String,
        val_name: &str,
        index_in_context: Option<usize>,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
        stack_vals: &StackVals,
        ast_index: &ASTIndex,
    ) {
        todo!()
    }

    fn add_value_type(&mut self, name: &str, value_type: &ValueType) {
        todo!()
    }

    fn push(&mut self, s: &str) {
        todo!()
    }

    fn add_on_top_of_after(&mut self, s: &str) {
        todo!()
    }

    fn after(&self) -> Vec<String> {
        todo!()
    }

    fn before(&self) -> String {
        todo!()
    }

    fn resolve_native_parameters(
        &self,
        body: &str,
        to_remove_from_stack: String,
        ident: usize,
    ) -> String {
        let mut result = body.to_string();

        let mut substitutions = Vec::new();

        // TODO optimize, collect parameter names, in reverse then substitute $par_name with par_name
        for par in self.parameters.iter() {
            substitutions.push((par.name.clone(), par.name.clone()));
        }

        substitutions.sort_by(|(a, _), (b, _)| a.cmp(b).reverse());

        for (par_name, value) in substitutions {
            result = result.replace(&format!("${}", par_name), &value);
        }

        result
    }
}
