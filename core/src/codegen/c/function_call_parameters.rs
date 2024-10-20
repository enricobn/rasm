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
use crate::codegen::c::options::COptions;
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::enh_ast::EnhASTIndex;
use crate::codegen::function_call_parameters::FunctionCallParameters;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::stack::StackVals;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::enh_val_context::TypedValContext;
use crate::codegen::{get_reference_type_name, CodeGen};
use crate::parser::ast::ASTValueType;
use crate::type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedType,
};
use linked_hash_map::LinkedHashMap;
use log::debug;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::any::CStructs;
use super::typed_function_creator::TypedFunctionsCreatorC;

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
            code_gen_c: CodeGenC::new(COptions::default(), false),
        }
    }

    fn get_value_from_lambda_space(
        statics: &Statics,
        type_def_provider: &dyn TypeDefProvider,
        i: usize,
        ast_typed_type: &ASTTypedType,
        dereference: bool,
        name: &str,
    ) -> String {
        let is_ref_type = get_reference_type_name(&ast_typed_type, type_def_provider).is_some();

        let deref_s = if dereference { "*" } else { "" };

        /*
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
        */
        format!(" lambda_space->{name}")
    }

    fn add_code_for_reference_type(
        &mut self,
        module: &ASTTypedModule,
        type_name: &str,
        source: &str,
        descr: &str,
        statics: &mut Statics,
    ) {
        /*
        self.code_gen_c
            .call_add_ref(&mut self.before, source, type_name, descr, module, statics);

        let mut after = String::new();
        self.code_gen_c
            .call_deref(&mut after, source, type_name, descr, module, statics);

        self.after.insert(0, after);

         */
    }
}

impl FunctionCallParameters for CFunctionCallParameters {
    fn add_label(
        &mut self,
        param_name: &str,
        label: String,
        value: String,
        comment: Option<&str>,
        typed_type: &ASTTypedType,
        statics: &Statics,
    ) {
        if self.immediate {
            self.code_manipulator.add(
                &mut self.current,
                &format!(
                    "{} return_value_ = {value};",
                    CodeGenC::real_type_to_string(typed_type, statics)
                ),
                None,
                true,
            );
        } else {
            self.parameters_values
                .insert(param_name.to_string(), CodeGenC::escape_string(&value));
        }
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

    fn add_function_call(
        &mut self,
        module: &ASTTypedModule,
        comment: &str,
        param_type: ASTTypedType,
        statics: &mut Statics,
        name: String,
        before: String,
        current: String,
        t: &ASTTypedType,
    ) {
        let tmp_val_name = format!("call_{}_", ID.fetch_add(1, Ordering::SeqCst));

        let code = if current.is_empty() {
            before
        } else {
            self.push(&before);
            current
        };

        CLambdas::add_to_statics_if_lambda(&param_type, statics);

        self.code_manipulator.add(
            &mut self.before,
            &format!(
                "{} {tmp_val_name} = {};",
                CodeGenC::real_type_to_string(&param_type, statics),
                code.replace('\n', "")
            ),
            None,
            true,
        );

        if let Some(type_name) = get_reference_type_name(&param_type, module) {
            if type_name == "_fn" {
                TypedFunctionsCreatorC::addref_deref_lambda(
                    &mut self.before,
                    "addRef",
                    &tmp_val_name,
                    t,
                    module,
                    &self.code_gen_c,
                    statics,
                );
            } else {
                self.code_gen_c.call_add_ref(
                    &mut self.before,
                    &tmp_val_name,
                    &type_name,
                    "",
                    module,
                    statics,
                );
            }
        }

        if let Some(type_name) = get_reference_type_name(&param_type, module) {
            let mut deref_code = String::new();

            if type_name == "_fn" {
                TypedFunctionsCreatorC::addref_deref_lambda(
                    &mut deref_code,
                    "deref",
                    &tmp_val_name,
                    t,
                    module,
                    &self.code_gen_c,
                    statics,
                );
            } else {
                self.code_gen_c.call_deref(
                    &mut deref_code,
                    &tmp_val_name,
                    &type_name,
                    &type_name,
                    module,
                    statics,
                );
            }
            self.add_on_top_of_after(&deref_code);
        }

        self.parameters_values.insert(name, tmp_val_name);

        if let Some(name) = get_reference_type_name(&param_type, module) {
            self.add_code_for_reference_type(module, &name, &name, comment, statics);
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
        lambda_in_stack: bool,
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
            EnhASTIndex::none(),
        ));

        // debug!("{}Adding lambda {}", " ".repeat(indent * 4), param_name);

        let mut references = self.body_references_to_context(&def.body, context);
        references.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));
        references.dedup_by(|(name1, _), (name2, _)| name1 == name2);

        let optimize_lambda_space = references.is_empty();

        let no_ref_count_for_lambda_space = lambda_in_stack
            && references
                .iter()
                .all(|it| get_reference_type_name(it.1.typed_type(), module).is_none());

        let mut context = TypedValContext::new(None);
        for (key, kind) in references {
            context.insert(key, kind);
        }

        let mut lambda_space = LambdaSpace::new(context.clone());

        for (name, kind) in context.iter() {
            lambda_space.add(name.clone(), kind.clone());
        }

        let lambda_var_name = format!("lambda_var_{}", ID.fetch_add(1, Ordering::SeqCst));

        let (pointer_operator, dereference_operator) = if lambda_in_stack {
            self.code_manipulator.add(
                &mut self.before,
                &format!("struct {c_lambda_name} {lambda_var_name};"),
                None,
                true,
            );
            self.code_manipulator.add(
                &mut self.before,
                &format!("struct RasmPointer_ {lambda_var_name}_;"),
                None,
                true,
            );
            self.code_manipulator.add(
                &mut self.before,
                &format!("{lambda_var_name}_.address = &{lambda_var_name};"),
                None,
                true,
            );
            (".", "&")
        } else {
            self.code_manipulator.add(
                &mut self.before,
                &format!(
                    "struct RasmPointer_ *{lambda_var_name}_ = rasmMalloc(sizeof(struct {c_lambda_name}));"
                ),
                None,
                true,
            );
            self.code_manipulator.add(
                &mut self.before,
                &format!(
                    "struct {c_lambda_name} *{lambda_var_name} = (struct {c_lambda_name}*) {lambda_var_name}_->address;"
                ),
                None,
                true,
            );
            ("->", "")
        };

        self.code_manipulator.add(
            &mut self.before,
            &format!("{lambda_var_name}{pointer_operator}functionPtr = &{name};"),
            None,
            true,
        );

        if !optimize_lambda_space {
            let lambda_space_struct_name =
                CStructs::add_lambda_space_to_statics(statics, &lambda_space);

            let lambda_space_name = format!("lambda_space_{}", ID.fetch_add(1, Ordering::SeqCst));

            let lambda_space_pointer_operator = if no_ref_count_for_lambda_space {
                self.code_manipulator.add(
                    &mut self.before,
                    &format!("struct RasmPointer_ {lambda_space_name}_;"),
                    None,
                    true,
                );

                self.code_manipulator.add(
                    &mut self.before,
                    &format!("struct {} {lambda_space_name};", lambda_space_struct_name),
                    None,
                    true,
                );

                self.code_manipulator.add(
                    &mut self.before,
                    &format!("{lambda_space_name}_.address = &{lambda_space_name};"),
                    None,
                    true,
                );

                self.code_manipulator.add(
                    &mut self.before,
                    &format!(
                        "{lambda_var_name}{pointer_operator}lambda_space = &{lambda_space_name}_;",
                    ),
                    None,
                    true,
                );
                "."
            } else {
                self.code_manipulator.add(
                    &mut self.before,
                    &format!(
                        "struct RasmPointer_ *{lambda_space_name}_ = rasmMalloc(sizeof(struct {lambda_space_struct_name}));",
                    ),
                    None,
                    true,
                );

                self.code_manipulator.add(
                    &mut self.before,
                    &format!(
                        "struct {lambda_space_struct_name} *{lambda_space_name} = (struct {lambda_space_struct_name}*) {lambda_space_name}_->address;"
                    ),
                    None,
                    true,
                );

                self.code_manipulator.add(
                    &mut self.before,
                    &format!(
                        "{lambda_var_name}{pointer_operator}lambda_space = {lambda_space_name}_;",
                    ),
                    None,
                    true,
                );
                "->"
            };

            for (i, (name, _kind)) in lambda_space.iter().enumerate() {
                let value = if let Some(_idx) =
                    parent_lambda_space.and_then(|it| it.get_index(name))
                {
                    let pls = parent_lambda_space.unwrap();
                    let t = pls.get_type(name).unwrap();

                    let parent_ls_type_name = CStructs::add_lambda_space_to_statics(statics, pls);

                    format!(" lambda_space->{name}")

                    //Self::get_value_from_lambda_space(statics, module, idx - 1, t, false, name)
                } else {
                    name.to_string()
                };

                self.code_manipulator.add(
                    &mut self.before,
                    &format!("{lambda_space_name}{lambda_space_pointer_operator}{name} = {value};"),
                    None,
                    true,
                );
            }
        }

        if !lambda_in_stack || !(optimize_lambda_space || no_ref_count_for_lambda_space) {
            let typed_function_creator = TypedFunctionsCreatorC::new(self.code_gen_c.clone());

            let addref_function = typed_function_creator.create_lambda_free(
                &c_lambda_name,
                &lambda_space,
                "addRef",
                module,
                statics,
                lambda_in_stack,
                optimize_lambda_space || no_ref_count_for_lambda_space,
            );

            let deref_function = typed_function_creator.create_lambda_free(
                &c_lambda_name,
                &lambda_space,
                "deref",
                module,
                statics,
                lambda_in_stack,
                optimize_lambda_space || no_ref_count_for_lambda_space,
            );

            self.code_manipulator.add(
                &mut self.before,
                &format!(
                    "{lambda_var_name}{pointer_operator}addref_function = {};",
                    addref_function.name
                ),
                None,
                true,
            );

            self.code_manipulator.add(
                &mut self.before,
                &format!(
                    "{lambda_var_name}{pointer_operator}deref_function = {};",
                    deref_function.name
                ),
                None,
                true,
            );

            //arg_values.push(format!("lambda{param_index}"));
            if self.immediate {
                self.code_manipulator.add(
                    &mut self.current,
                    &format!(
                    "struct RasmPointer_ *return_value_ = {dereference_operator}{lambda_var_name}_; // return lambda"
                ),
                    None,
                    true,
                );
            } else {
                self.code_manipulator.add(
                    &mut self.before,
                    &format!(
                        "{}({dereference_operator}{lambda_var_name}_);",
                        addref_function.name
                    ),
                    None,
                    true,
                );

                self.add_on_top_of_after(&format!(
                    "{}({dereference_operator}{lambda_var_name}_);",
                    deref_function.name
                ));

                self.parameters_values.insert(
                    name.to_string(),
                    format!("{dereference_operator}{lambda_var_name}_"),
                );
            }

            lambda_space.add_ref_function(addref_function);
            lambda_space.add_ref_function(deref_function);
        } else {
            self.code_manipulator.add(
                &mut self.before,
                &format!("{lambda_var_name}{pointer_operator}addref_function = NULL;"),
                None,
                true,
            );

            self.code_manipulator.add(
                &mut self.before,
                &format!("{lambda_var_name}{pointer_operator}deref_function = NULL;"),
                None,
                true,
            );

            if self.immediate {
                self.code_manipulator.add(
                    &mut self.current,
                    &format!(
                    "struct RasmPointer_ *return_value_ = {dereference_operator}{lambda_var_name}_; // return lambda"
                ),
                    None,
                    true,
                );
            } else {
                self.parameters_values.insert(
                    name.to_string(),
                    format!("{dereference_operator}{lambda_var_name}_"),
                );
            }
        }

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
        typed_type: &ASTTypedType,
    ) {
        if let Some(ls) = lambda_space {
            if let Some(index_in_lambda_space) = ls.get_index(val_name) {
                let ast_typed_type = ls.get_type(val_name).unwrap();
                let value = Self::get_value_from_lambda_space(
                    statics,
                    type_def_provider,
                    index_in_lambda_space - 1,
                    ast_typed_type,
                    false,
                    val_name,
                );

                if self.immediate {
                    self.code_manipulator.add(
                        &mut self.before,
                        &format!(
                            "{} return_value_ = {value};",
                            CodeGenC::real_type_to_string(ast_typed_type, statics)
                        ),
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
            self.code_manipulator.add(
                &mut self.before,
                &format!(
                    "{} return_value_ = {val_name};",
                    CodeGenC::real_type_to_string(typed_type, statics)
                ),
                None,
                true,
            );
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
        ast_index: &EnhASTIndex,
        statics: &Statics,
        type_def_provider: &dyn TypeDefProvider,
        typed_type: &ASTTypedType,
    ) {
        let value =
            if let Some(index_in_lambda) = lambda_space.and_then(|it| it.get_index(val_name)) {
                let value_from_lambda_space = Self::get_value_from_lambda_space(
                    statics,
                    type_def_provider,
                    index_in_lambda - 1,
                    lambda_space.unwrap().get_type(val_name).unwrap(),
                    false,
                    val_name,
                );

                value_from_lambda_space
            } else {
                val_name.to_string()
            };

        if self.immediate {
            self.code_manipulator.add(
                &mut self.before,
                &format!(
                    "{} return_value_ = {value};",
                    CodeGenC::real_type_to_string(typed_type, statics)
                ),
                None,
                true,
            );
        } else {
            self.parameters_values
                .insert(original_param_name.to_string(), value);
        }
    }

    fn add_value_type(&mut self, name: &str, value_type: &ASTValueType) {
        let value = self.code_gen_c.value_to_string(value_type);

        self.parameters_values.insert(name.to_string(), value);
    }

    fn push(&mut self, s: &str) {
        self.before.push_str(s);
    }

    fn add_on_top_of_after(&mut self, s: &str) {
        self.after.insert(0, s.into());
    }

    fn before(&self) -> String {
        self.before.clone()
    }

    fn current(&self) -> String {
        self.current.clone()
    }

    fn after(&self) -> Vec<String> {
        self.after.clone()
    }

    fn resolve_native_parameters(
        &self,
        body: &str,
        to_remove_from_stack: String,
        indent: usize,
        return_value: bool,
        is_inner_call: bool,
        return_type: Option<&ASTTypedType>,
        statics: &Statics,
    ) -> String {
        let suffix = if is_inner_call { "" } else { ";" };

        let mut result = if return_value {
            if let Some(rt) = return_type {
                if !matches!(rt, ASTTypedType::Unit) {
                    format!(
                        "{} return_value_ =  {body};",
                        CodeGenC::real_type_to_string(rt, statics),
                    )
                } else {
                    format!("{body};")
                }
            } else {
                panic!("return value without return type");
            }
        } else {
            format!("{}{suffix}", body)
        };

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
}
