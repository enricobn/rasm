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
use std::ops::Deref;

use log::{debug, info};

use crate::codegen::backend::Backend;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::DummyTypeDefProvider;
use crate::codegen::val_context::ValContext;
use crate::codegen::ValKind;
use crate::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTModule,
    ASTParameterDef, ASTStatement, ASTType, BuiltinTypeKind, ValueType,
};
use crate::transformations::type_functions_creator::type_mandatory_functions;
use crate::type_check::functions_container::{FunctionsContainer, TypeFilter};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::type_check_error::TypeCheckError;
use crate::type_check::typed_ast::get_default_functions;
use crate::type_check::typed_context::TypeConversionContext;
use crate::type_check::{is_generic_type, resolve_generic_types_from_effective_type, substitute};
use crate::utils::{OptionDisplay, SliceDisplay};

type InputModule = EnhancedASTModule;
type OutputModule = EnhancedASTModule;

pub struct TypeCheck {
    module: OutputModule,
    pub type_conversion_context: TypeConversionContext,
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
            type_conversion_context: TypeConversionContext::new(),
        }
    }

    pub fn type_check(
        mut self,
        ast_module: ASTModule,
        backend: &dyn Backend,
        mut statics: &mut Statics,
    ) -> Result<(OutputModule, TypeConversionContext), TypeCheckError> {
        //let finder = ModuleFinder::new(ast_module);

        let mut val_context = ValContext::new(None);

        let mut default_functions = get_default_functions(false); // TODO print_allocation

        let module = EnhancedASTModule::new(ast_module);
        default_functions.extend(type_mandatory_functions(&module));

        self.module = module.clone();
        self.module.functions_by_name = FunctionsContainer::new();

        for default_function in default_functions {
            let call = default_function.to_call();

            if let Some(f) = module.find_precise_function(&call.function_name, &call.function_name)
            {
                // TODO check error
                self.type_conversion_context
                    .add_function(call.function_name.clone(), f.clone());

                self.module
                    .functions_by_name
                    .add_function(call.function_name, f.clone());
            } else {
                return Err(TypeCheckError::from(format!(
                    "Cannot find default function {}",
                    call.function_name
                )));
            }

            /*
            self.transform_call(
                &module,
                &call,
                &mut val_context,
                &mut statics,
                None,
            )?;

             */
        }

        let new_body =
            self.transform_statements(&module, &module.body, &mut val_context, statics, None)?;
        self.module.body = new_body;

        let mut cloned_module = self.module.clone();

        while true {
            info!("Type check loop {}", self.module.functions().len());
            for function in cloned_module.functions_mut() {
                let new_body = self
                    .transform_function(&module, &mut statics, function, backend)
                    .map_err(|it| {
                        TypeCheckError::from(format!(
                            "{} converting function {function} : {}",
                            it, function.index
                        ))
                    })?;

                self.type_conversion_context
                    .replace_body(function, new_body.clone());

                self.module
                    .functions_by_name
                    .replace_body(function, new_body);
            }

            if cloned_module.functions().len() == self.module.functions().len() {
                break;
            }
            cloned_module = self.module.clone();
        }

        Ok((self.module, self.type_conversion_context))
    }

    fn transform_statement(
        &mut self,
        module: &InputModule,
        statement: &ASTStatement,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_return_type: Option<&ASTType>,
    ) -> Result<ASTStatement, TypeCheckError> {
        match statement {
            ASTStatement::Expression(e) => self
                .transform_expression(
                    module,
                    e,
                    val_context,
                    statics,
                    expected_return_type,
                    expected_return_type,
                )
                .map(ASTStatement::Expression),
            ASTStatement::LetStatement(name, e, is_const, index) => self
                .transform_expression(module, e, val_context, statics, None, None)
                .map(|it| ASTStatement::LetStatement(name.clone(), it, *is_const, index.clone())),
        }
    }

    fn transform_expression(
        &mut self,
        module: &InputModule,
        expression: &ASTExpression,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
        expected_return_type: Option<&ASTType>,
    ) -> Result<ASTExpression, TypeCheckError> {
        match expression {
            ASTExpression::StringLiteral(s) => Ok(expression.clone()),
            ASTExpression::ASTFunctionCallExpression(call) => self
                .transform_call(module, call, val_context, statics, expected_type)
                .map(ASTExpression::ASTFunctionCallExpression),
            ASTExpression::ValueRef(name, index) => Ok(expression.clone()),
            ASTExpression::Value(value_type, index) => Ok(expression.clone()),
            ASTExpression::Lambda(lambda_def) => self
                .transform_lambda_def(
                    module,
                    lambda_def,
                    val_context,
                    statics,
                    expected_type,
                    expected_return_type,
                )
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
        // TODO
        Ok(ast_type.clone())
    }

    fn transform_call(
        &mut self,
        module: &InputModule,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_return_type: Option<&ASTType>,
    ) -> Result<ASTFunctionCall, TypeCheckError> {
        debug_i!(
            "transform_call {call} expected_return_type {}",
            OptionDisplay(&expected_return_type)
        );
        indent!();
        let already_converted = call.function_name.contains('_')
            || self
                .module
                .functions_by_name
                .has_function(&call.original_function_name, &call.function_name);
        /*call.original_function_name != call.function_name
        || self
            .module
            .functions_by_name
            .has_function(&call.original_function_name, &call.function_name);*/

        if already_converted {
            debug_i!("already converted");
            dedent!();
            return Ok(call.clone());
        }

        if val_context.is_lambda(&call.function_name) {
            dedent!();
            return Ok(call.clone());
        }

        /*
        if let Some(v) = val_context.get(&call.function_name) {
            match v {
                ValKind::ParameterRef(_, par) => {}
                ValKind::LetRef(_, t, index) => {}
            }
        }

         */

        /*
        let new_expressions: Vec<ASTExpression> = call
            .parameters
            .iter()
            .map(|it| self.transform_expression(module, it, val_context, statics, None))
            .collect::<Result<Vec<_>, TypeCheckError>>()
            .map_err(|it| {
                format!(
                    "{} converting expressions in call {call} : {}",
                    it, call.index
                )
            })?;

         */

        let filters = call
            .parameters
            .iter()
            .map(|it| self.type_of_expression(module, it, val_context, statics, None))
            .collect::<Result<Vec<_>, TypeCheckError>>()?;

        /*
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

         */

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

        let mut new_expressions_filters = filters.clone();

        let original_functions = module
            .find_call_vec(call, &filters, None)
            .map_err(|it| format!("{} converting call {call} : {}", it, call.index))?;

        let mut new_function_def = if !original_functions.is_empty() {
            /*if let Some(function_def) = Self::disambiguate_functions(&original_functions, &filters)
            {
                debug_i!("found disambiguate_function {function_def}");
                function_def
            } else {

             */

            let mut valid_functions = Vec::new();

            for function in original_functions {
                debug_i!("verifying function {function}");
                indent!();

                let mut fake_resolved_generic_types_for_f = ResolvedGenericTypes::new();
                let mut inverse_fake_resolved_generic_types_for_f = ResolvedGenericTypes::new();
                let mut fake_generic_types_for_f = Vec::new();

                let mut f = if function.generic_types.is_empty() {
                    function
                } else {
                    for (i, name) in function.generic_types.iter().enumerate() {
                        let new_name = format!("{name}__{i}");
                        fake_resolved_generic_types_for_f
                            .insert(name.clone(), ASTType::Generic(new_name.clone()));
                        inverse_fake_resolved_generic_types_for_f
                            .insert(new_name.clone(), ASTType::Generic(name.clone()));
                        fake_generic_types_for_f.push(new_name);
                    }

                    let mut result = function.clone();

                    Self::resolve_generic_types_for_function(
                        &mut result,
                        &fake_resolved_generic_types_for_f,
                    );

                    result.generic_types = fake_generic_types_for_f;

                    result
                };

                let mut resolved_generic_types = ResolvedGenericTypes::new();

                if let Some(rt) = expected_return_type {
                    if !is_generic_type(rt) {
                        if let Ok(result) =
                            resolve_generic_types_from_effective_type(&f.return_type, rt)
                        {
                            resolved_generic_types.extend(result).map_err(|e| {
                                e.add(format!(
                                    "resolving generic type {} with {rt}",
                                    f.return_type
                                ))
                            });
                        }
                    } /*else if !is_generic_type(&new_function_def.return_type) {
                          resolved_generic_types.extend(resolve_generic_types_from_effective_type(
                              rt,
                              &new_function_def.return_type,
                          )?)?;
                      }
                      */
                }

                let filters: Result<Vec<TypeFilter>, TypeCheckError> =
                    zip(call.parameters.iter(), f.parameters.iter())
                        .map(|(expr, param)| {
                            debug_i!("expr {expr}");
                            // TODO optimize
                            let expr = if let ASTExpression::Any(t) = expr {
                                ASTExpression::Any(
                                    substitute(t, &fake_resolved_generic_types_for_f)
                                        .unwrap_or(t.clone()),
                                )
                            } else {
                                expr.clone()
                            };

                            // TODO optimize
                            let param_type =
                                substitute(&param.ast_type, &fake_resolved_generic_types_for_f)
                                    .unwrap_or(param.ast_type.clone());
                            let param_type = substitute(&param_type, &resolved_generic_types)
                                .unwrap_or(param_type.clone());
                            debug_i!("real expression : {expr}");
                            debug_i!("real type of expression : {param_type}");

                            let e = self.transform_expression(
                                module,
                                &expr,
                                val_context,
                                statics,
                                Some(&param_type),
                                None,
                            )?;

                            let t = self.type_of_expression(
                                module,
                                &e,
                                val_context,
                                statics,
                                Some(&param_type),
                            )?;
                            if let TypeFilter::Exact(et) = &t {
                                resolved_generic_types.extend(
                                    resolve_generic_types_from_effective_type(&param_type, et)?,
                                )?;
                            }
                            debug_i!("filter {t}");
                            Ok(t)
                        })
                        .collect::<Result<Vec<_>, TypeCheckError>>();
                //.map_err(|it| it.add(format!("converting expressions verifying {f}")));

                if let Err(e) = &filters {
                    debug_i!("ignored function due to {e}");
                    dedent!();
                    continue;
                }

                let filters = filters?;

                /*
                debug_i!("with new expressions: {}", SliceDisplay(&new_expressions));

                let filters = zip(new_expressions.iter(), f.parameters.iter())
                    .map(|(it, p)| {
                        self.type_of_expression(module, it, val_context, statics, Some(&p.ast_type))
                    })
                    .collect::<Result<Vec<_>, TypeCheckError>>()?;

                 */

                debug_i!("with filters: {}", SliceDisplay(&filters));

                let mut valid = zip(f.parameters.iter(), filters.iter())
                    .all(|(p, f)| f.almost_equal(&p.ast_type).unwrap());

                if valid {
                    if let Some(rt) = expected_return_type {
                        //let rt = substitute(rt, &resolved_generic_types).unwrap_or(rt.clone());
                        valid = valid
                            && TypeFilter::Exact(rt.clone())
                                .almost_equal(&f.return_type)
                                .unwrap_or(false);
                    }
                }

                if valid {
                    let non_generic_types: usize = f
                        .parameters
                        .iter()
                        .map(|it| if is_generic_type(&it.ast_type) { 0 } else { 1 })
                        .sum();
                    new_expressions_filters = filters;

                    Self::resolve_generic_types_for_function(
                        &mut f,
                        &inverse_fake_resolved_generic_types_for_f,
                    );
                    let new_generic_type = f
                        .generic_types
                        .iter()
                        .filter(|it| inverse_fake_resolved_generic_types_for_f.contains_key(it))
                        .map(|it| {
                            if let Some(ASTType::Generic(g)) =
                                inverse_fake_resolved_generic_types_for_f.get(it)
                            {
                                g
                            } else {
                                panic!("It should not happen");
                            }
                        })
                        .cloned()
                        .collect::<Vec<_>>();
                    f.generic_types = new_generic_type;

                    valid_functions.push((f, non_generic_types));
                    debug_i!("it's valid with non_generic_types {non_generic_types}");
                }
                dedent!();
            }

            if valid_functions.is_empty() {
                dedent!();
                return Err(TypeCheckError::from(format!(
                    "call {call} : {}\ncannot find a valid function",
                    call.index,
                    //SliceDisplay(&original_functions)
                )));
            } else if valid_functions.len() > 1 {
                let max = valid_functions.iter().map(|it| it.1).max().unwrap();

                let valid_functions = valid_functions
                    .iter()
                    .filter(|it| it.1 == max)
                    .collect::<Vec<_>>();

                if valid_functions.is_empty() {
                    // I think it should not happen
                    dedent!();
                    return Err(TypeCheckError::from(format!(
                        "call {call} : {}\ncannot find a valid function",
                        call.index,
                        //SliceDisplay(&original_functions)
                    )));
                } else if valid_functions.len() > 1 {
                    dedent!();
                    return Err(TypeCheckError::from(format!(
                        "call {call} : {}\nfound more than one valid function {}",
                        call.index,
                        SliceDisplay(&valid_functions.iter().map(|it| &it.0).collect::<Vec<_>>())
                    )));
                } else {
                    valid_functions.first().unwrap().clone().clone().0.clone()
                }
            } else {
                valid_functions.first().unwrap().clone().clone().0.clone()
            }

            //}
        } else {
            dedent!();
            return Err(TypeCheckError::from(format!(
                "call {call} : {}\ncannot find function for filters {}",
                call.index,
                SliceDisplay(&filters)
            )));
            //        } else {
            //            original_functions.first().unwrap().clone()
        };

        debug_i!("found valid function {new_function_def}");

        let new_function_name = format!(
            "{}_{}",
            new_function_def.original_name,
            self.module.functions_by_name.len()
        );
        new_function_def.name = new_function_name.clone();

        if !new_function_def.generic_types.is_empty() {
            let mut resolved_generic_types = ResolvedGenericTypes::new();

            for (f, p) in zip(
                new_expressions_filters.iter(),
                new_function_def.parameters.iter(),
            )
            .collect::<Vec<_>>()
            .iter()
            {
                match f {
                    TypeFilter::Exact(t) => {
                        resolved_generic_types
                            .extend(resolve_generic_types_from_effective_type(&p.ast_type, t)?)?;
                    }
                    TypeFilter::Any => {}
                    TypeFilter::Lambda(_, effective_return_type) => {
                        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters,
                            return_type: def_return_type,
                        }) = &p.ast_type
                        {
                            if let Some(bert) = effective_return_type.deref() {
                                if let TypeFilter::Exact(ert) = bert.deref() {
                                    resolved_generic_types
                                        .extend(resolve_generic_types_from_effective_type(
                                            def_return_type.deref(),
                                            ert,
                                        )?)
                                        .map_err(|e| {
                                            e.add(format!(
                                                "resolving generic type {} with {ert}",
                                                def_return_type.deref()
                                            ))
                                        })?;
                                }
                            }
                        }
                    }
                    TypeFilter::NotALambda => {}
                }
            }

            if let Some(rt) = expected_return_type {
                if !is_generic_type(rt) {
                    resolved_generic_types
                        .extend(resolve_generic_types_from_effective_type(
                            &new_function_def.return_type,
                            rt,
                        )?)
                        .map_err(|e| {
                            e.add(format!(
                                "resolving generic type {} with {rt}",
                                new_function_def.return_type
                            ))
                        })?;
                } /*else if !is_generic_type(&new_function_def.return_type) {
                      resolved_generic_types.extend(resolve_generic_types_from_effective_type(
                          rt,
                          &new_function_def.return_type,
                      )?)?;
                  }
                  */
            }

            for p in new_function_def.parameters.iter_mut() {
                if let Some(new_t) = substitute(&p.ast_type, &resolved_generic_types) {
                    p.ast_type = new_t;
                }
                if is_generic_type(&p.ast_type) {
                    dedent!();
                    return Err(TypeCheckError::from(format!(
                        "Unresolved generic type {} : {resolved_generic_types}",
                        p.ast_type,
                    )));
                }
            }

            if let Some(new_t) = substitute(&new_function_def.return_type, &resolved_generic_types)
            {
                new_function_def.return_type = new_t;
            }

            if is_generic_type(&new_function_def.return_type) {
                dedent!();
                return Err(TypeCheckError::from(format!(
                    "Unresolved generic return type {}, expected return type {}",
                    new_function_def.return_type,
                    OptionDisplay(&expected_return_type)
                )));
            }

            new_function_def.resolved_generic_types = resolved_generic_types;
            new_function_def.generic_types = Vec::new();
        }

        let new_expressions: Vec<ASTExpression> =
            zip(call.parameters.iter(), new_function_def.parameters.iter())
                .map(|(it, param)| {
                    self.transform_expression(
                        module,
                        it,
                        val_context,
                        statics,
                        Some(&param.ast_type),
                        None,
                    )
                })
                .collect::<Result<Vec<_>, TypeCheckError>>()
                .map_err(|it| {
                    format!(
                        "{} converting expressions in call {call} : {}",
                        it, call.index
                    )
                })?;

        new_call.parameters = new_expressions;

        let filters = new_function_def
            .parameters
            .iter()
            .map(|it| TypeFilter::Exact(it.ast_type.clone()))
            .collect::<Vec<_>>();

        let converted_functions = self.module.find_call_vec(
            call,
            &filters,
            Some(new_function_def.return_type.clone()),
        )?;

        if converted_functions.len() == 1 {
            new_call.function_name = converted_functions.first().unwrap().name.clone();
            debug_i!("already added function {}", new_call.function_name);
        } else {
            new_call.function_name = new_function_name;

            debug_i!("adding new function {}", new_function_def);

            // TODO check error
            self.type_conversion_context.add_function(
                new_function_def.original_name.clone(),
                new_function_def.clone(),
            );

            self.module
                .functions_by_name
                .add_function(new_function_def.original_name.clone(), new_function_def);
        }

        dedent!();
        Ok(new_call)
    }

    fn resolve_generic_types_for_function(
        result: &mut ASTFunctionDef,
        resolved_generic_types: &ResolvedGenericTypes,
    ) {
        let new_parameters = result
            .parameters
            .iter()
            .map(|it| {
                if let Some(new_t) = substitute(&it.ast_type, resolved_generic_types) {
                    let mut new_parameter = it.clone();
                    new_parameter.ast_type = new_t;
                    new_parameter
                } else {
                    it.clone()
                }
            })
            .collect::<Vec<_>>();

        result.parameters = new_parameters;

        if let Some(new_t) = substitute(&result.return_type, resolved_generic_types) {
            result.return_type = new_t;
        }
    }

    fn transform_function(
        &mut self,
        module: &InputModule,
        statics: &mut Statics,
        new_function_def: &ASTFunctionDef,
        backend: &dyn Backend,
    ) -> Result<ASTFunctionBody, TypeCheckError> {
        debug_i!("transform_function {new_function_def}");
        debug_i!(
            "generic_types {}",
            SliceDisplay(&new_function_def.generic_types)
        );
        debug_i!(
            "resolved generic_types {}",
            new_function_def.resolved_generic_types
        );
        indent!();
        let mut val_context = ValContext::new(None);

        for parameter in new_function_def.parameters.iter() {
            val_context.insert_par(parameter.name.clone(), parameter.clone());
        }

        let new_body = match &new_function_def.body {
            ASTFunctionBody::RASMBody(statements) => {
                let new_statements = self.transform_statements(
                    module,
                    statements,
                    &mut val_context,
                    statics,
                    Some(&new_function_def.return_type),
                )?;
                ASTFunctionBody::RASMBody(new_statements)
            }
            ASTFunctionBody::ASMBody(asm_body) => {
                let mut lines: Vec<String> =
                    asm_body.lines().map(|it| it.to_owned()).collect::<Vec<_>>();

                let type_def_provider = DummyTypeDefProvider::new();
                let called_functions = backend.called_functions(
                    None,
                    Some(new_function_def),
                    asm_body,
                    &val_context,
                    &type_def_provider,
                );

                for (m, f) in called_functions.iter() {
                    let call = f.to_call(new_function_def);
                    let new_call =
                        self.transform_call(module, &call, &mut val_context, statics, None)?;
                    debug_i!("old line {}", lines[f.i]);

                    let new_line = lines[f.i].replace(&call.function_name, &new_call.function_name);
                    debug_i!("new line {}", new_line);
                    lines[f.i] = new_line;
                }

                // TODO optimize if no transformations

                ASTFunctionBody::ASMBody(lines.join("\n"))

                //new_function_def.body.clone()
            }
        };
        dedent!();
        Ok(new_body)
    }

    fn transform_statements(
        &mut self,
        module: &InputModule,
        statements: &[ASTStatement],
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_return_type: Option<&ASTType>,
    ) -> Result<Vec<ASTStatement>, TypeCheckError> {
        debug_i!(
            "transform_statements expected_return_type {}",
            OptionDisplay(&expected_return_type)
        );
        indent!();
        let result = statements
            .iter()
            .enumerate()
            .map(|(i, it)| {
                let er = if i == statements.len() - 1 {
                    expected_return_type
                } else {
                    None
                };
                let new_statement = self.transform_statement(module, it, val_context, statics, er);

                if let Ok(ASTStatement::LetStatement(name, expr, is_cons, index)) = &new_statement {
                    if let Ok(type_of_expr) =
                        self.type_of_expression(module, expr, val_context, statics, None)
                    {
                        if let TypeFilter::Exact(ast_type) = type_of_expr {
                            if *is_cons {
                                statics.add_const(name.clone(), ast_type);
                            } else {
                                val_context.insert_let(name.clone(), ast_type, index);
                            }
                        } else {
                            dedent!();
                            return Err(TypeCheckError::from(format!(
                                "Cannot determine type of {expr} : {index}"
                            )));
                        }
                    } else {
                        dedent!();
                        return Err(TypeCheckError::from(""));
                    }
                }

                new_statement
            })
            .collect::<Result<Vec<_>, TypeCheckError>>();
        dedent!();
        result
    }

    fn type_of_expression(
        &mut self,
        module: &InputModule,
        typed_expression: &ASTExpression,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
    ) -> Result<TypeFilter, TypeCheckError> {
        debug_i!(
            "type_of_expression {typed_expression} expected type {}",
            OptionDisplay(&expected_type)
        );
        indent!();
        let result = match typed_expression {
            ASTExpression::StringLiteral(_) => {
                TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::String))
            }
            ASTExpression::ASTFunctionCallExpression(call) => {
                if val_context.is_lambda(&call.function_name) {
                    if let Some(v) = val_context.get(&call.function_name) {
                        let lambda = match v {
                            ValKind::ParameterRef(_, p) => p.ast_type.clone(),
                            ValKind::LetRef(_, t, index) => t.clone(),
                        };

                        dedent!();
                        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters: _,
                            return_type,
                        }) = lambda
                        {
                            return Ok(TypeFilter::Exact(return_type.deref().clone()));
                        } else {
                            return Err("It should not happen!!!".into());
                        }
                    };

                    //return Ok(call.clone());
                }
                if let Some(f) = self
                    .module
                    .find_precise_function(&call.original_function_name, &call.function_name)
                {
                    dedent!();
                    return Ok(TypeFilter::Exact(f.return_type.clone()));
                }

                if let Some(f) =
                    module.find_precise_function(&call.original_function_name, &call.function_name)
                {
                    dedent!();
                    return Ok(TypeFilter::Exact(f.return_type.clone()));
                }

                // I cannot go deep in determining the type
                if expected_type.is_none() {
                    dedent!();
                    return Ok(TypeFilter::Any);
                }

                if let Ok(new_call) =
                    self.transform_call(module, call, val_context, statics, expected_type)
                {
                    if let Some(function) = self.module.find_precise_function(
                        &new_call.original_function_name,
                        &new_call.function_name,
                    ) {
                        let result = TypeFilter::Exact(function.return_type.clone());
                        debug_i!("found from converted functions: {result}");
                        dedent!();
                        return Ok(result);
                    }
                }

                let filters = call
                    .parameters
                    .iter()
                    .map(|it| self.type_of_expression(module, it, val_context, statics, None))
                    .collect::<Result<Vec<_>, TypeCheckError>>()?;
                let functions = module
                    .functions_by_name
                    .find_call_vec(call, &filters, None, false)?;

                if functions.len() == 1 {
                    if let Some(f) = functions.first() {
                        let result = TypeFilter::Exact(f.return_type.clone());
                        debug_i!("Found from module functions: {result}");
                        dedent!();
                        return Ok(result);
                    }
                } else {
                    debug_i!(
                        "Found no or more functions for {call} : {}",
                        SliceDisplay(&functions)
                    );
                }

                TypeFilter::Any
            }
            ASTExpression::ValueRef(name, _) => match val_context.get(name) {
                None => {
                    if let Some(c) = statics.get_const(name) {
                        TypeFilter::Exact(c.ast_type.clone())
                    } else {
                        dedent!();
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
            ASTExpression::Lambda(def) => {
                // I cannot go deep in determining the type
                if expected_type.is_none() {
                    dedent!();
                    return Ok(TypeFilter::Lambda(def.parameter_names.len(), None));
                }

                let mut return_type = None;
                let mut lambda_val_context = ValContext::new(Some(val_context));

                Self::add_lambda_parameters_to_val_context(
                    def,
                    &expected_type,
                    &mut lambda_val_context,
                )?;

                for (i, statement) in def.body.iter().enumerate() {
                    if let ASTStatement::LetStatement(name, expr, is_cons, index) = statement {
                        let type_of_expr = self.type_of_expression(
                            module,
                            expr,
                            &mut lambda_val_context,
                            statics,
                            None,
                        )?;

                        if let TypeFilter::Exact(ast_type) = type_of_expr {
                            if *is_cons {
                                dedent!();
                                return Err(TypeCheckError::from(format!(
                                    "Const not allowed here {expr} : {index}"
                                )));
                            } else {
                                lambda_val_context.insert_let(name.clone(), ast_type, index);
                            }
                        } else {
                            dedent!();
                            return Ok(TypeFilter::Lambda(def.parameter_names.len(), None));
                            /*
                            return Err(TypeCheckError::from(format!(
                                "Cannot determine type of {expr}, got {} : {index}",
                                type_of_expr
                            )));

                             */
                        }
                    }

                    if i == def.body.len() - 1 {
                        if let ASTStatement::Expression(last) = statement {
                            return_type = Some(Box::new(self.type_of_expression(
                                module,
                                last,
                                &mut lambda_val_context,
                                statics,
                                None,
                            )?));
                        }
                    }
                }
                info!("lambda return type {}", OptionDisplay(&return_type));
                TypeFilter::Lambda(def.parameter_names.len(), return_type)
            }
            ASTExpression::Any(t) => TypeFilter::Exact(t.clone()),
        };

        debug_i!("found type {result}");
        dedent!();
        Ok(result)
    }

    fn transform_lambda_def(
        &mut self,
        module: &InputModule,
        lambda_def: &ASTLambdaDef,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
        expected_return_type: Option<&ASTType>,
    ) -> Result<ASTLambdaDef, TypeCheckError> {
        let mut new_lambda = lambda_def.clone();

        let mut val_context = ValContext::new(Some(val_context));

        Self::add_lambda_parameters_to_val_context(lambda_def, &expected_type, &mut val_context)?;

        let ert = if let Some(ert) = expected_return_type {
            Ok(Some(ert))
        } else if let Some(et) = expected_type {
            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            }) = et
            {
                Ok(Some(return_type.deref()))
            } else {
                Err(TypeCheckError::from(format!(
                    "Expected lambda but got {et}"
                )))
            }
        } else {
            Ok(None)
        };

        new_lambda.body =
            self.transform_statements(module, &lambda_def.body, &mut val_context, statics, ert?)?;

        Ok(new_lambda)
    }

    fn add_lambda_parameters_to_val_context(
        lambda_def: &ASTLambdaDef,
        expected_type: &Option<&ASTType>,
        val_context: &mut ValContext,
    ) -> Result<(), TypeCheckError> {
        if !lambda_def.parameter_names.is_empty() {
            if let Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            })) = expected_type
            {
                for ((name, index), t) in zip(lambda_def.parameter_names.iter(), parameters.iter())
                {
                    val_context.insert_par(
                        name.to_owned(),
                        ASTParameterDef {
                            name: name.to_owned(),
                            ast_type: t.clone(),
                            ast_index: index.clone(),
                        },
                    );
                }
            } else {
                /*
                return Err(TypeCheckError::from(format!(
                    "Expecting lambda but got {}",
                    OptionDisplay(&expected_type)
                )));

                 */
            }
        }
        Ok(())
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
    use crate::type_check::type_check_error::TypeCheckError;
    use crate::type_check::typed_ast::convert_to_typed_module_2;

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

    #[test]
    pub fn xmllib() {
        let project = dir_to_project("/home/enrico/development/rasm/xmllib");
        test_project(project).unwrap_or_else(|e| panic!("{e}"))
    }

    fn test_project(project: RasmProject) -> Result<(), TypeCheckError> {
        let (module, backend, mut statics) = to_ast_module(project);

        let type_check = TypeCheck::new();

        let (resolved_module, type_conversion_context) =
            type_check.type_check(module, &backend, &mut statics)?;

        //resolved_module.print();

        let typed_module = convert_to_typed_module_2(
            &resolved_module,
            type_conversion_context,
            false,
            //mandatory_functions,
            &backend,
            &mut statics,
            true,
        );

        //print_typed_module(&typed_module.0);
        Ok(())
    }

    fn file_to_project(test_file: &str) -> RasmProject {
        init();
        env::set_var("RASM_STDLIB", "../../../stdlib");

        let current_path = env::current_dir().unwrap();

        let file_name = if current_path.ends_with("rasm/core") {
            PathBuf::from(&format!("../rasm/resources/test/{test_file}"))
        // for debugging in Codium
        } else if current_path.ends_with("rasm") {
            PathBuf::from(&format!("rasm/resources/test/{test_file}"))
        } else {
            panic!(
                "cannot handle current path: {}",
                current_path.to_str().unwrap()
            );
        };

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
