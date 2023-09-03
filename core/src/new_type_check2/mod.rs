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

use log::{debug, info};

use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::val_context::ValContext;
use crate::codegen::ValKind;
use crate::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTModule,
    ASTStatement, ASTType, BuiltinTypeKind, ValueType,
};
use crate::type_check::functions_container::{FunctionsContainer, TypeFilter};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::type_check_error::TypeCheckError;
use crate::type_check::typed_ast::get_default_functions;
use crate::type_check::{resolve_generic_types_from_effective_type, substitute};
use crate::utils::SliceDisplay;

type InputModule = EnhancedASTModule;
type OutputModule = EnhancedASTModule;

pub struct TypeCheck {
    module: OutputModule,
}

struct ModuleFinder {
    ast_module: ASTModule,
}

impl ModuleFinder {
    fn new(ast_module: ASTModule) -> Self {
        Self { ast_module }
    }

    fn body(&self) -> &Vec<ASTStatement> {
        &self.ast_module.body
    }

    fn find_call(
        &self,
        function_name: &str,
        parameters_filters: Vec<TypeFilter>,
    ) -> Vec<&ASTFunctionDef> {
        self.ast_module
            .functions
            .iter()
            .filter(|it| {
                if it.original_name == function_name
                    && it.parameters.len() == parameters_filters.len()
                {
                    zip(parameters_filters.iter(), it.parameters.iter())
                        .all(|(filter, par)| filter.almost_equal(&par.ast_type))
                } else {
                    false
                }
            })
            .collect()
    }
}

impl TypeCheck {
    pub fn new() -> Self {
        let typed_module = EnhancedASTModule {
            body: vec![],
            functions_by_name: FunctionsContainer::new(),
            enums: vec![],
            structs: vec![],
            requires: Default::default(),
            externals: Default::default(),
            types: vec![],
        };

        Self {
            module: typed_module,
        }
    }

    pub fn type_check(mut self, ast_module: ASTModule) -> Result<OutputModule, TypeCheckError> {
        //let finder = ModuleFinder::new(ast_module);

        let module = EnhancedASTModule::new(ast_module);
        let mut val_context = ValContext::new(None);
        let mut statics = Statics::new();

        let new_body =
            self.transform_statements(&module, &module.body, &mut val_context, &mut statics)?;
        self.module.body = new_body;

        let default_functions = get_default_functions(false); // TODO print_allocation

        for default_function in default_functions {
            self.transform_call(
                &module,
                &default_function.to_call(),
                &mut val_context,
                &mut statics,
            )?;
        }

        let mut cloned_module = self.module.clone();

        while true {
            info!("Type check loop {}", self.module.functions().len());
            for function in cloned_module.functions_mut() {
                let new_body = self
                    .transform_function(&module, &mut statics, function)
                    .map_err(|it| {
                        TypeCheckError::from(format!(
                            "{} converting function {function} : {}",
                            it, function.index
                        ))
                    })?;

                self.module
                    .functions_by_name
                    .replace_body(function, new_body);
            }

            if cloned_module.functions().len() == self.module.functions().len() {
                break;
            }
            cloned_module = self.module.clone();
        }

        Ok(self.module)
    }

    fn transform_statement(
        &mut self,
        module: &InputModule,
        statement: &ASTStatement,
        val_context: &mut ValContext,
        statics: &mut Statics,
    ) -> Result<ASTStatement, TypeCheckError> {
        match statement {
            ASTStatement::Expression(e) => self
                .transform_expression(module, e, val_context, statics)
                .map(ASTStatement::Expression),
            ASTStatement::LetStatement(name, e, is_const, index) => self
                .transform_expression(module, e, val_context, statics)
                .map(|it| ASTStatement::LetStatement(name.clone(), it, *is_const, index.clone())),
        }
    }

    fn transform_expression(
        &mut self,
        module: &InputModule,
        expression: &ASTExpression,
        val_context: &mut ValContext,
        statics: &mut Statics,
    ) -> Result<ASTExpression, TypeCheckError> {
        match expression {
            ASTExpression::StringLiteral(s) => Ok(expression.clone()),
            ASTExpression::ASTFunctionCallExpression(call) => self
                .transform_call(module, call, val_context, statics)
                .map(ASTExpression::ASTFunctionCallExpression),
            ASTExpression::ValueRef(name, index) => Ok(expression.clone()),
            ASTExpression::Value(value_type, index) => Ok(expression.clone()),
            ASTExpression::Lambda(lambda_def) => self
                .transform_lambda_def(module, lambda_def, val_context, statics)
                .map(ASTExpression::Lambda),
            ASTExpression::Any(ast_type) => self
                .transform_ast_type(module, ast_type)
                .map(ASTExpression::Any),
        }
        .map_err(|it| {
            format!(
                "{} converting expression {expression} : {}",
                it,
                expression.get_index()
            )
            .into()
        })
    }

    fn transform_ast_type(
        &mut self,
        module: &InputModule,
        ast_type: &ASTType,
    ) -> Result<ASTType, TypeCheckError> {
        todo!()
    }

    fn transform_call(
        &mut self,
        module: &InputModule,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut Statics,
    ) -> Result<ASTFunctionCall, TypeCheckError> {
        debug_i!("transform_call {call}");
        indent!();
        let already_converted = self
            .module
            .functions_by_name
            .has_function(&call.original_function_name, &call.function_name);

        if already_converted {
            debug_i!("already converted");
            dedent!();
            return Ok(call.clone());
        }

        let filters = {
            let not_converted_filters = call
                .parameters
                .iter()
                .map(|it| self.type_of_expression(module, it, val_context, statics))
                .collect::<Result<Vec<_>, TypeCheckError>>()?;

            let all_exact = not_converted_filters
                .iter()
                .all(|it| matches!(it, TypeFilter::Exact(_)));

            if all_exact {
                debug_i!("all exact");
                not_converted_filters
            } else {
                let new_expressions: Vec<ASTExpression> = call
                    .parameters
                    .iter()
                    .map(|it| self.transform_expression(module, it, val_context, statics))
                    .collect::<Result<Vec<_>, TypeCheckError>>()
                    .map_err(|it| {
                        format!(
                            "{} converting expressions in call {call} : {}",
                            it, call.index
                        )
                    })?;

                new_expressions
                    .iter()
                    .map(|it| self.type_of_expression(module, it, val_context, statics))
                    .collect::<Result<Vec<_>, TypeCheckError>>()?
            }
        };

        let mut new_call = call.clone();

        /*
        let converted_functions = self
            .module
            .find_call_vec(call, &filters, None)
            .map_err(|it| format!("{} converting call {call} : {}", it, call.index))?;


        match converted_functions.len().cmp(&1usize) {
            Ordering::Less => {}
            Ordering::Equal => {
                debug_i!("found converted function");
                let new_expressions: Vec<ASTExpression> = call
                    .parameters
                    .iter()
                    .map(|it| self.transform_expression(module, it, val_context, statics))
                    .collect::<Result<Vec<_>, TypeCheckError>>()
                    .map_err(|it| {
                        format!(
                            "{} converting expressions in call {call} : {}",
                            it, call.index
                        )
                    })?;
                new_call.parameters = new_expressions;
                new_call.function_name = converted_functions.first().unwrap().name.clone();
                dedent!();
                return Ok(new_call);
            }
            Ordering::Greater => {
                dedent!();
                return Err(TypeCheckError::from(format!(
                    "Found more than one function for {call} : {}",
                    call.index
                )));
            }
        }

        debug_i!("cannot find a converted function");

         */

        let original_functions = module
            .find_call_vec(call, &filters, None)
            .map_err(|it| format!("{} converting call {call} : {}", it, call.index))?;
        let mut new_function_def = if original_functions.len() != 1 {
            if let Some(function_def) = Self::disambiguate_functions(&original_functions, &filters)
            {
                debug_i!("found disambiguate_function {function_def}");
                function_def
            } else {
                return Err(TypeCheckError::from(format!(
                    "call {call} : {}\nfound more than one function {}",
                    call.index,
                    SliceDisplay(&original_functions)
                )));
            }
        } else {
            original_functions.first().unwrap().clone()
        };

        let new_function_name = format!(
            "{}_{}",
            new_function_def.original_name,
            self.module.functions_by_name.len()
        );
        new_function_def.name = new_function_name.clone();

        if !new_function_def.generic_types.is_empty() {
            let mut resolved_generic_types = ResolvedGenericTypes::new();

            for (f, p) in zip(filters.iter(), new_function_def.parameters.iter())
                .collect::<Vec<_>>()
                .iter()
            {
                match f {
                    TypeFilter::Exact(t) => {
                        resolved_generic_types
                            .extend(resolve_generic_types_from_effective_type(&p.ast_type, t)?);
                    }
                    TypeFilter::Any => {}
                    TypeFilter::Lambda(_) => {}
                    TypeFilter::NotALambda => {}
                }
            }

            for p in new_function_def.parameters.iter_mut() {
                if let Some(new_t) = substitute(&p.ast_type, &resolved_generic_types) {
                    p.ast_type = new_t;
                }
            }

            if let Some(new_t) = substitute(&new_function_def.return_type, &resolved_generic_types)
            {
                new_function_def.return_type = new_t;
            }

            new_function_def.resolved_generic_types = resolved_generic_types;
        }

        let converted_functions = self.module.find_call_vec(call, &filters, None)?;

        if converted_functions.len() == 1 {
            new_call.function_name = converted_functions.first().unwrap().name.clone();
        } else {
            new_call.function_name = new_function_name;

            self.module
                .functions_by_name
                .add_function(new_function_def.original_name.clone(), new_function_def);
        }

        dedent!();
        Ok(new_call)
    }

    fn transform_function(
        &mut self,
        module: &InputModule,
        statics: &mut Statics,
        new_function_def: &ASTFunctionDef,
    ) -> Result<ASTFunctionBody, TypeCheckError> {
        let mut val_context = ValContext::new(None);

        for parameter in new_function_def.parameters.iter() {
            val_context.insert_par(parameter.name.clone(), parameter.clone());
        }

        let new_body = match &new_function_def.body {
            ASTFunctionBody::RASMBody(statements) => {
                let new_statements =
                    self.transform_statements(module, statements, &mut val_context, statics)?;
                ASTFunctionBody::RASMBody(new_statements)
            }
            ASTFunctionBody::ASMBody(asm_body) => {
                // TODO
                new_function_def.body.clone()
            }
        };

        Ok(new_body)
    }

    fn disambiguate_functions(
        candidate_functions: &[ASTFunctionDef],
        filters: &[TypeFilter],
    ) -> Option<ASTFunctionDef> {
        // we want to find the unique function that has the max number of equal non generic parameters
        let not_generic = candidate_functions
            .iter()
            .map(|it| {
                (
                    it,
                    zip(it.parameters.iter(), filters.iter())
                        .filter(|(p, filter)| {
                            !matches!(p.ast_type, ASTType::Generic(_))
                                && matches!(filter, TypeFilter::Exact(_))
                                // I think it's redundant, candidate functions should already be the ones that matches the filters
                                && filter.almost_equal(&p.ast_type)
                        })
                        .count(),
                )
            })
            .collect::<Vec<_>>();

        let max_not_generic = not_generic.iter().map(|it| it.1).max();

        if let Some(max) = max_not_generic {
            if max > 0 {
                let candidate_functions = not_generic
                    .iter()
                    .filter(|it| it.1 == max)
                    .map(|it| it.0)
                    .collect::<Vec<_>>();
                if candidate_functions.len() == 1 {
                    candidate_functions.first().copied().cloned()
                } else {
                    // there are more functions with the same number of non generic parameters
                    None
                }
            } else {
                // there are no functions with non generic parameters
                None
            }
        } else {
            // we have no candidate functions
            None
        }
    }

    fn transform_statements(
        &mut self,
        module: &InputModule,
        statements: &Vec<ASTStatement>,
        val_context: &mut ValContext,
        statics: &mut Statics,
    ) -> Result<Vec<ASTStatement>, TypeCheckError> {
        statements
            .iter()
            .map(|it| {
                let new_statement = self.transform_statement(module, it, val_context, statics);

                if let Ok(ASTStatement::LetStatement(name, expr, is_cons, index)) = &new_statement {
                    if let Ok(type_of_expr) =
                        self.type_of_expression(module, expr, val_context, statics)
                    {
                        if let TypeFilter::Exact(ast_type) = type_of_expr {
                            if *is_cons {
                                statics.add_const(name.clone(), ast_type);
                            } else {
                                val_context.insert_let(name.clone(), ast_type, index);
                            }
                        } else {
                            return Err(TypeCheckError::from(format!(
                                "Cannot determine type of {expr} : {index}"
                            )));
                        }
                    } else {
                        return Err(TypeCheckError::from(""));
                    }
                }

                new_statement
            })
            .collect::<Result<Vec<_>, TypeCheckError>>()
    }

    fn type_of_expression(
        &self,
        module: &InputModule,
        typed_expression: &ASTExpression,
        val_context: &mut ValContext,
        statics: &mut Statics,
    ) -> Result<TypeFilter, TypeCheckError> {
        let result = match typed_expression {
            ASTExpression::StringLiteral(_) => {
                TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::String))
            }
            ASTExpression::ASTFunctionCallExpression(call) => {
                if let Some(converted_function) = self
                    .module
                    .functions_by_name
                    .find_function(&call.function_name)
                {
                    TypeFilter::Exact(converted_function.return_type.clone())
                } else {
                    TypeFilter::Any
                    /*.ok_or(TypeCheckError::from(format!(
                        "Cannot find function {} : {}",
                        call.function_name, call.index
                    )))

                     */
                }
            }
            ASTExpression::ValueRef(name, _) => match val_context.get(name) {
                None => {
                    if let Some(c) = statics.get_const(name) {
                        TypeFilter::Exact(c.ast_type.clone())
                    } else {
                        return Err(TypeCheckError::from(format!(
                            "Cannot find reference to {name}"
                        )));
                    }
                }
                Some(ValKind::LetRef(_, t, index)) => TypeFilter::Exact(t.clone()),
                Some(ValKind::ParameterRef(_, par)) => {
                    // TODO I must convert the type
                    TypeFilter::Exact(par.ast_type.clone())
                }
            },
            ASTExpression::Value(value_type, _) => match value_type {
                ValueType::Boolean(_) => TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::Bool)),
                ValueType::I32(_) => TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::I32)),
                ValueType::Char(_) => TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::Char)),
                ValueType::F32(_) => TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::F32)),
            },
            ASTExpression::Lambda(call) => TypeFilter::Lambda(call.parameter_names.len()),
            ASTExpression::Any(_) => TypeFilter::Any,
        };

        Ok(result)
    }

    fn transform_lambda_def(
        &mut self,
        module: &InputModule,
        lambda_def: &ASTLambdaDef,
        val_context: &mut ValContext,
        statics: &mut Statics,
    ) -> Result<ASTLambdaDef, TypeCheckError> {
        let mut new_lambda = lambda_def.clone();
        for (name, index) in lambda_def.parameter_names.iter() {
            // TODO
            //val_context.insert_par(name.to_owned(), ASTParameterDef{name: name.to_owned(), ast_type: ASTType::Builtin(BuiltinTypeKind::)});
        }
        new_lambda.body =
            self.transform_statements(module, &lambda_def.body, val_context, statics)?;

        Ok(new_lambda)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::env;
    use std::io::Write;
    use std::path::PathBuf;

    use env_logger::Builder;

    use crate::codegen::backend::BackendNasm386;
    use crate::codegen::statics::Statics;
    use crate::new_type_check2::TypeCheck;
    use crate::parser::ast::ASTModule;
    use crate::project::project::RasmProject;
    use crate::transformations::enrich_module;
    use crate::transformations::type_functions_creator::type_mandatory_functions;
    use crate::type_check::type_check_error::TypeCheckError;
    use crate::type_check::typed_ast::{convert_to_typed_module, print_typed_module};

    #[test]
    pub fn fibonacci() {
        let project = file_to_project("fibonacci.rasm");
        test_project(project).unwrap_or_else(|e| panic!("{e}"))
    }

    #[test]
    pub fn breakout() {
        let project = dir_to_project("../rasm/resources/examples/breakout");
        test_project(project).unwrap_or_else(|e| panic!("{e}"))
    }

    fn test_project(project: RasmProject) -> Result<(), TypeCheckError> {
        let (module, backend, mut statics) = to_ast_module(project);

        let type_check = TypeCheck::new();

        let resolved_module = type_check.type_check(module)?;

        resolved_module.print();

        let mandatory_functions = type_mandatory_functions(&resolved_module);

        let typed_module = convert_to_typed_module(
            &resolved_module,
            false,
            false,
            true,
            mandatory_functions,
            &backend,
            &mut statics,
            true,
        );

        print_typed_module(&typed_module.0);
        Ok(())
    }

    fn file_to_project(test_file: &str) -> RasmProject {
        init();
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let file_name = PathBuf::from(&format!("../rasm/resources/test/{test_file}"));

        RasmProject::new(file_name)
    }

    /*
    fn to_ast_module(test_file: &str) -> (ASTModule, BackendNasm386, Statics) {
        let project = file_to_project(test_file);

        let mut statics = Statics::new();
        let mut module = project.get_module();

        let backend = BackendNasm386::new(HashSet::new(), HashSet::new(), false);

        enrich_module(
            &backend,
            project.resource_folder(),
            &mut statics,
            &mut module,
        );

        (module, backend, statics)
    }

     */

    fn dir_to_project(test_folder: &str) -> RasmProject {
        init();
        let file_name = PathBuf::from(test_folder);

        RasmProject::new(file_name)
    }

    fn to_ast_module(project: RasmProject) -> (ASTModule, BackendNasm386, Statics) {
        let mut statics = Statics::new();
        let mut module = project.get_module();

        let backend = BackendNasm386::new(HashSet::new(), HashSet::new(), false);

        enrich_module(
            &backend,
            project.resource_folder(),
            &mut statics,
            &mut module,
        );

        (module, backend, statics)
    }

    fn init() {
        Builder::from_default_env()
            .format(|buf, record| {
                writeln!(
                    buf,
                    "{} [{}] - {}",
                    chrono::Local::now().format("%Y-%m-%d %H:%M:%S.%3f"),
                    record.level(),
                    record.args()
                )
            })
            .try_init()
            .unwrap_or(());
    }
}
