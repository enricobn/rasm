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

use crate::codegen::c::code_gen_c::{CodeGenC, CodeManipulatorC};
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::function_call_parameters::FunctionCallParameters;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::stack::StackVals;
use crate::codegen::statics::Statics;
use crate::codegen::val_context::TypedValContext;
use crate::codegen::TypedValKind;
use crate::parser::ast::{ASTIndex, ValueType};
use crate::type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedType,
};
use linked_hash_map::LinkedHashMap;
use log::debug;
use std::sync::atomic::{AtomicUsize, Ordering};

static id: AtomicUsize = AtomicUsize::new(0);

pub struct CFunctionCallParameters {
    parameters: Vec<ASTTypedParameterDef>,
    parameters_values: LinkedHashMap<String, String>,
    before: String,
    after: Vec<String>,
    code_manipulator: CodeManipulatorC,
}

impl CFunctionCallParameters {
    pub fn new(parameters: Vec<ASTTypedParameterDef>) -> Self {
        Self {
            parameters,
            parameters_values: LinkedHashMap::new(),
            before: String::new(),
            after: Vec::new(),
            code_manipulator: CodeManipulatorC,
        }
    }
}

impl FunctionCallParameters for CFunctionCallParameters {
    fn add_label(&mut self, param_name: &str, label: String, value: String, comment: Option<&str>) {
        self.parameters_values
            .insert(param_name.to_string(), format!("\"{value}\""));
    }

    fn add_function_call(
        &mut self,
        module: &ASTTypedModule,
        comment: &str,
        param_type: ASTTypedType,
        statics: &mut Statics,
        name: String,
        before: String,
    ) {
        println!(
            "add_function_call {name} : {param_type} before `{}`",
            self.before
        );

        //let tmp_name = format!("tmp_{}", id.fetch_add(1, Ordering::SeqCst));
        self.parameters_values
            .insert(name, before.replace('\n', ""));

        /*
        self.before.push_str(&format!(
            "{} {tmp_name} = {before}",
            CodeGenC::type_to_string(&param_type)
        ));

         */
        // TODO
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
        def.parameters.push(ASTTypedParameterDef::new(
            "lambda",
            param_type.clone(),
            ASTIndex::none(),
        ));

        // debug!("{}Adding lambda {}", " ".repeat(indent * 4), param_name);

        // TODO parent_lambda space

        let mut lambda_space = LambdaSpace::new(context.clone());

        for (name, kind) in context.iter() {
            lambda_space.add(name.clone(), kind.clone());
        }

        self.code_manipulator
            .add(&mut self.before, "// TODO create lambda", None, true);

        self.code_manipulator.add(
            &mut self.before,
            &format!("struct Lambda lambda_{param_index};"),
            None,
            true,
        );
        self.code_manipulator.add(
            &mut self.before,
            &format!(
                "lambda_{param_index}.args = malloc(sizeof(void *) * {});",
                lambda_space.size()
            ),
            None,
            true,
        );
        for (i, (name, kind)) in lambda_space.iter().enumerate() {
            self.code_manipulator.add(
                &mut self.before,
                &format!("lambda_{param_index}.args[{i}] = &{name};"),
                None,
                true,
            );
        }
        self.code_manipulator.add(
            &mut self.before,
            &format!("lambda_{param_index}.functionPtr = &{name};"),
            None,
            true,
        );

        //arg_values.push(format!("lambda{param_index}"));
        self.parameters_values
            .insert(name.to_string(), format!("lambda_{param_index}"));

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
    ) {
        if let Some(ls) = lambda_space {
            if ls.get_index(val_name).is_some() {
                if let Some(kind) = ls.get_context().get(val_name) {
                    let (i, ast_typed_type) = match kind {
                        TypedValKind::ParameterRef(i, pd) => (*i, pd.ast_type.clone()),
                        TypedValKind::LetRef(i, t) => (*i, t.clone()),
                    };

                    self.parameters_values.insert(
                        original_param_name.to_string(),
                        format!(
                            " *(({}*)lambda.args[{i}])",
                            CodeGenC::type_to_string(&ast_typed_type)
                        ),
                    );

                    return;
                }
            }
        }
        self.parameters_values
            .insert(original_param_name.to_string(), val_name.to_string());
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
        todo!("Adding let_val_ref {original_param_name}, {val_name}")
    }

    fn add_value_type(&mut self, name: &str, value_type: &ValueType) {
        let value = match value_type {
            ValueType::Boolean(b) => (if *b { "0" } else { "1" }).to_string(),
            ValueType::I32(i) => format!("{i}"),
            ValueType::Char(c) => format!("'{c}'"),
            ValueType::F32(f) => format!("{f}"),
        };

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
        ident: usize,
    ) -> String {
        let mut result = body.to_string();

        let mut substitutions = Vec::new();

        // TODO optimize, collect parameter names, in reverse then substitute $par_name with par_name
        for par in self.parameters.iter() {
            if let Some(par_value) = self.parameters_values.get(&par.name) {
                debug!(
                    "{}found parameter {}, value: {}",
                    " ".repeat(ident * 4),
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
}
