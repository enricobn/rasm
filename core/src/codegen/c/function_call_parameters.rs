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

use crate::codegen::c::any::{CInclude, CLambda, CLambdas};
use crate::codegen::c::code_gen_c::{CCodeManipulator, CodeGenC};
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::function_call_parameters::FunctionCallParameters;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::stack::StackVals;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::val_context::TypedValContext;
use crate::codegen::{get_reference_type_name, CodeGen};
use crate::parser::ast::{ASTIndex, ValueType};
use crate::type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedType,
};
use linked_hash_map::LinkedHashMap;
use log::debug;
use std::sync::atomic::{AtomicUsize, Ordering};

static ID: AtomicUsize = AtomicUsize::new(0);

pub struct CFunctionCallParameters {
    parameters: Vec<ASTTypedParameterDef>,
    parameters_values: LinkedHashMap<String, String>,
    before: String,
    current: String,
    after: Vec<String>,
    code_manipulator: CCodeManipulator,
    inline: bool,
    stack_vals: StackVals,
    immediate: bool,
    code_gen_c: CodeGenC,
}

impl CFunctionCallParameters {
    pub fn new(
        parameters: Vec<ASTTypedParameterDef>,
        inline: bool,
        stack_vals: StackVals,
        immediate: bool,
    ) -> Self {
        Self {
            parameters,
            parameters_values: LinkedHashMap::new(),
            before: String::new(),
            current: String::new(),
            after: Vec::new(),
            code_manipulator: CCodeManipulator,
            inline,
            stack_vals,
            immediate,
            code_gen_c: CodeGenC::new(),
        }
    }

    fn get_value_from_lambda_space(
        statics: &Statics,
        type_def_provider: &dyn TypeDefProvider,
        i: usize,
        ast_typed_type: &ASTTypedType,
        dereference: bool,
    ) -> String {
        let is_ref_type = get_reference_type_name(&ast_typed_type, type_def_provider).is_some();

        let deref_s = if dereference { "*" } else { "" };

        let value = if is_ref_type {
            format!(
                " (({})_lambda->args[{i}])",
                CodeGenC::type_to_string(ast_typed_type, statics)
            )
        } else {
            format!(
                " {deref_s}(({}*)_lambda->args[{i}])",
                CodeGenC::type_to_string(ast_typed_type, statics)
            )
        };
        value
    }
}

impl FunctionCallParameters for CFunctionCallParameters {
    fn add_label(&mut self, param_name: &str, label: String, value: String, comment: Option<&str>) {
        self.parameters_values.insert(
            param_name.to_string(),
            format!("{}", CodeGenC::escape_string(&value)),
        );
    }

    fn add_function_call(
        &mut self,
        module: &ASTTypedModule,
        comment: &str,
        param_type: ASTTypedType,
        statics: &mut Statics,
        name: String,
        before: String,
        current: String,
    ) {
        if current.is_empty() {
            self.parameters_values
                .insert(name, before.replace('\n', ""));
        } else {
            self.push(&before);
            self.parameters_values
                .insert(name, current.replace('\n', ""));
        }
    }

    fn add_lambda(
        &mut self,
        def: &mut ASTTypedFunctionDef,
        parent_lambda_space: Option<&LambdaSpace>,
        context: &TypedValContext,
        comment: Option<&str>,
        statics: &mut Statics,
        module: &ASTTypedModule,
        stack_vals: &StackVals,
        optimize: bool,
        function_def: &ASTTypedFunctionDef,
        param_type: &ASTTypedType,
        name: &str,
        param_index: usize,
    ) -> LambdaSpace {
        // for malloc
        CInclude::add_to_statics(statics, "<stdlib.h>".to_string());

        let c_lambda = CLambda::new(
            def.parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect(),
            def.return_type.clone(),
        );

        let c_lambda_name = CLambdas::add_to_statics(statics, c_lambda);

        def.parameters.push(ASTTypedParameterDef::new(
            "_lambda",
            param_type.clone(),
            ASTIndex::none(),
        ));

        // debug!("{}Adding lambda {}", " ".repeat(indent * 4), param_name);

        // TODO parent_lambda space

        let mut lambda_space = LambdaSpace::new(context.clone());

        for (name, kind) in context.iter() {
            lambda_space.add(name.clone(), kind.clone());
        }

        let lambda_var_name = format!("lambda_{}", ID.fetch_add(1, Ordering::SeqCst));

        self.code_manipulator.add(
            &mut self.before,
            &format!("struct {c_lambda_name} {lambda_var_name};"),
            None,
            true,
        );

        self.code_manipulator.add(
            &mut self.before,
            &format!(
                "{lambda_var_name}.args = malloc(sizeof(void *) * {});",
                lambda_space.size()
            ),
            None,
            true,
        );
        for (i, (name, kind)) in lambda_space.iter().enumerate() {
            let t = kind.typed_type();

            let name_prefix = if get_reference_type_name(t, module).is_some() {
                ""
            } else {
                "&"
            };

            let value = if let Some(idx) = parent_lambda_space.and_then(|it| it.get_index(name)) {
                let t = parent_lambda_space.unwrap().get_type(name).unwrap();
                Self::get_value_from_lambda_space(statics, module, idx - 1, t, false)
            } else {
                format!("{name_prefix}{name}")
            };

            self.code_manipulator.add(
                &mut self.before,
                &format!("{lambda_var_name}.args[{i}] = {value};"),
                None,
                true,
            );
        }
        self.code_manipulator.add(
            &mut self.before,
            &format!("{lambda_var_name}.functionPtr = &{name};"),
            None,
            true,
        );

        //arg_values.push(format!("lambda{param_index}"));
        self.parameters_values
            .insert(name.to_string(), format!("&{lambda_var_name}"));

        lambda_space
    }

    fn add_parameter_ref(
        &mut self,
        original_param_name: String,
        val_name: &str,
        index_in_context: usize,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
        stack_vals: &StackVals,
        statics: &Statics,
        type_def_provider: &dyn TypeDefProvider,
    ) {
        if let Some(ls) = lambda_space {
            if let Some(index_in_lambda_space) = ls.get_index(val_name) {
                let ast_typed_type = ls.get_type(val_name).unwrap();
                let value = Self::get_value_from_lambda_space(
                    statics,
                    type_def_provider,
                    index_in_lambda_space - 1,
                    ast_typed_type,
                    true,
                );

                if self.immediate {
                    self.code_manipulator.add(
                        &mut self.before,
                        &format!("return {value};"),
                        None,
                        true,
                    );
                } else {
                    self.parameters_values
                        .insert(original_param_name.to_string(), value);
                }
                return;
            }
        }

        if self.immediate {
            self.code_manipulator
                .add(&mut self.before, &format!("return {val_name};"), None, true);
        } else {
            self.parameters_values
                .insert(original_param_name.to_string(), val_name.to_string());
        }
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
        statics: &Statics,
        type_def_provider: &dyn TypeDefProvider,
    ) {
        if let Some(index_in_lambda) = lambda_space.and_then(|it| it.get_index(val_name)) {
            self.parameters_values.insert(
                original_param_name.to_string(),
                Self::get_value_from_lambda_space(
                    statics,
                    type_def_provider,
                    index_in_lambda - 1,
                    lambda_space.unwrap().get_type(val_name).unwrap(),
                    false,
                ),
            );
        } else {
            self.parameters_values
                .insert(original_param_name.to_string(), val_name.to_string());
        }
    }

    fn add_value_type(&mut self, name: &str, value_type: &ValueType) {
        let value = self.code_gen_c.value_to_string(value_type);

        self.parameters_values.insert(name.to_string(), value);
    }

    fn push(&mut self, s: &str) {
        self.before.push_str(s);
    }

    fn add_on_top_of_after(&mut self, s: &str) {
        self.after.insert(0, s.into());
    }

    fn after(&self) -> Vec<String> {
        self.after.clone()
    }

    fn before(&self) -> String {
        self.before.clone()
    }

    fn resolve_native_parameters(
        &self,
        body: &str,
        to_remove_from_stack: String,
        indent: usize,
        return_value: bool,
        is_inner_call: bool,
    ) -> String {
        let prefix = if return_value && self.inline {
            "return "
        } else {
            ""
        };
        let suffix = if is_inner_call { "" } else { ";" };

        let mut result = format!("{prefix}{}{suffix}", body);

        let mut substitutions = Vec::new();

        // TODO optimize, collect parameter names, in reverse then substitute $par_name with par_name
        for par in self.parameters.iter() {
            if let Some(par_value) = self.parameters_values.get(&par.name) {
                debug!(
                    "{}found parameter {}, value: {}",
                    " ".repeat(indent * 4),
                    par.name,
                    par_value
                );
                substitutions.push((par.name.clone(), par_value.clone()));
                continue;
            }
            substitutions.push((par.name.clone(), par.name.clone()));
        }

        substitutions.sort_by(|(a, _), (b, _)| a.cmp(b).reverse());

        for (par_name, value) in substitutions {
            result = result.replace(&format!("${}", par_name), &value);
        }

        result
    }

    fn parameters_values(&self) -> &LinkedHashMap<String, String> {
        &self.parameters_values
    }

    fn current(&self) -> String {
        self.current.clone()
    }

    fn add_string_constant(
        &mut self,
        param_name: &str,
        value: &str,
        comment: Option<&str>,
        statics: &mut Statics,
    ) {
        self.parameters_values.insert(
            param_name.to_string(),
            format!("\"{}\"", CodeGenC::escape_string(value)),
        );
    }
}
