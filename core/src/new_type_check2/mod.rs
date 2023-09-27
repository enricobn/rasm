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

use std::collections::HashMap;
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
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTParameterDef,
    ASTStatement, ASTType, BuiltinTypeKind, ValueType,
};
use crate::type_check::functions_container::{FunctionsContainer, TypeFilter};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::type_check_error::TypeCheckError;
use crate::type_check::typed_ast::DefaultFunction;
use crate::type_check::{is_generic_type, resolve_generic_types_from_effective_type, substitute};
use crate::utils::{OptionDisplay, SliceDisplay};

type InputModule = EnhancedASTModule;
type OutputModule = EnhancedASTModule;

pub struct TypeCheck {
    module: OutputModule,
    stack: Vec<String>,
    functions_stack: HashMap<String, Vec<String>>,
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
            stack: vec![],
            functions_stack: Default::default(),
        }
    }

    pub fn type_check(
        mut self,
        module: &EnhancedASTModule,
        backend: &dyn Backend,
        statics: &mut Statics,
        default_functions: Vec<DefaultFunction>,
        mandatory_functions: Vec<DefaultFunction>,
    ) -> Result<OutputModule, TypeCheckError> {
        let mut val_context = ValContext::new(None);

        let mut default_functions = default_functions; // TODO print_allocation

        default_functions.extend(mandatory_functions);

        self.module.types = module.types.clone();
        self.module.enums = module.enums.clone();
        self.module.structs = module.structs.clone();
        self.module.functions_by_name = FunctionsContainer::new();
        self.module.externals = module.externals.clone();
        self.module.requires = module.requires.clone();

        for default_function in default_functions {
            let call = default_function.to_call();

            if let Some(f) = module.find_precise_function(&call.function_name, &call.function_name)
            {
                // TODO check error
                let mut def = f.clone();

                let new_function_name = self.new_function_name(&call);
                def.name = new_function_name.clone();

                self.module
                    .functions_by_name
                    .add_function(call.function_name.clone(), def);
                self.functions_stack.insert(new_function_name, vec![]);
            } else {
                return Err(TypeCheckError::from(format!(
                    "Cannot find default function {}",
                    call.function_name
                )));
            }
        }

        let new_body =
            self.transform_statements(module, &module.body, &mut val_context, statics, None)?;
        self.module.body = new_body;

        loop {
            let functions_len = self.module.functions().len();
            info!("Type check loop {}", functions_len);

            let new_function_names = self.functions_stack.keys().cloned().collect::<Vec<_>>();

            for function_name in new_function_names {
                if let Some(stack) = self.functions_stack.remove(&function_name) {
                    self.stack = stack;
                } else {
                    continue;
                }

                let function = self
                    .module
                    .functions_by_name
                    .find_function(&function_name)
                    .unwrap()
                    .clone();

                if let Some(new_body) = self
                    .transform_function(module, statics, &function, backend)
                    .map_err(|it| {
                        it.add(format!(
                            "transforming function {function} : {}\n{}",
                            function.index,
                            self.stack.join("\n")
                        ))
                    })?
                {
                    self.module
                        .functions_by_name
                        .replace_body(&function, new_body);
                }
            }

            if self.module.functions().len() == functions_len {
                break;
            }
        }

        Ok(self.module)
    }

    fn new_function_name(&self, call: &ASTFunctionCall) -> String {
        let count = self
            .module
            .functions_by_name
            .count_by_original_name(&call.original_function_name);

        let new_function_name = format!(
            "{}_{}",
            call.original_function_name.replace("::", "_"),
            count
        );
        new_function_name
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
                .transform_expression(module, e, val_context, statics, expected_return_type)
                .map(ASTStatement::Expression),
            ASTStatement::LetStatement(name, e, is_const, index) => self
                .transform_expression(module, e, val_context, statics, None)
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
    ) -> Result<ASTExpression, TypeCheckError> {
        match expression {
            ASTExpression::ASTFunctionCallExpression(call) => self
                .transform_call(module, call, val_context, statics, expected_type)
                .map(ASTExpression::ASTFunctionCallExpression),
            ASTExpression::Lambda(lambda_def) => self
                .transform_lambda_def(module, lambda_def, val_context, statics, expected_type)
                .map(ASTExpression::Lambda),
            _ => Ok(expression.clone()),
        }
        .map_err(|it| {
            it.add(format!(
                "converting expression, expected_type {} : {}",
                OptionDisplay(&expected_type),
                expression.get_index()
            ))
        })
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

        if call.function_name.contains('_') {
            debug_i!("already converted");
            dedent!();
            return Ok(call.clone());
        }

        if val_context.is_lambda(&call.function_name) {
            dedent!();
            return Ok(call.clone());
        }

        self.stack.push(format!("{}", call.index));

        let (mut new_function_def, resolved_generic_types) =
            self.get_valid_function(module, call, val_context, statics, expected_return_type)?;

        debug_i!("found valid function {new_function_def}");

        let new_function_name = self.new_function_name(call);
        new_function_def.name = new_function_name.clone();

        if !new_function_def.generic_types.is_empty() {
            for p in new_function_def.parameters.iter_mut() {
                if let Some(new_t) = substitute(&p.ast_type, &resolved_generic_types) {
                    p.ast_type = new_t;
                }
                if is_generic_type(&p.ast_type) {
                    self.stack.pop();
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
                self.stack.pop();
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
                    )
                })
                .collect::<Result<Vec<_>, TypeCheckError>>()
                .map_err(|it| {
                    it.add(format!(
                        "converting expressions in call {} : {}",
                        call.original_function_name, call.index
                    ))
                })?;

        let mut new_call = call.clone();
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
            new_call.function_name = new_function_name.clone();

            debug_i!("adding new function {}", new_function_def);

            // TODO check error
            self.module
                .functions_by_name
                .add_function(new_function_def.original_name.clone(), new_function_def);
            self.functions_stack
                .insert(new_function_name, self.stack.clone());
        }

        dedent!();
        self.stack.pop();
        Ok(new_call)
    }

    fn get_valid_function(
        &mut self,
        module: &InputModule,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_return_type: Option<&ASTType>,
    ) -> Result<(ASTFunctionDef, ResolvedGenericTypes), TypeCheckError> {
        let mut valid_functions = Vec::new();
        let mut errors = Vec::new();

        let original_functions = module
            .find_functions_by_original_name(&call.original_function_name)
            .iter()
            .filter(|it| it.parameters.len() == call.parameters.len());

        for function in original_functions {
            debug_i!("verifying function {function}");
            indent!();

            let mut resolved_generic_types = ResolvedGenericTypes::new();

            if let Some(rt) = expected_return_type {
                if !TypeFilter::Exact(function.return_type.clone()).almost_equal(rt)? {
                    dedent!();
                    continue;
                }
                if !is_generic_type(rt) {
                    if let Ok(result) =
                        resolve_generic_types_from_effective_type(&function.return_type, rt)
                    {
                        resolved_generic_types.extend(result).map_err(|e| {
                            e.add(format!(
                                "resolving generic type {} with {rt}",
                                function.return_type
                            ))
                        })?;
                    }
                }
            }

            let mut something_resolved = true;

            let mut count = 0;
            let mut valid = true;
            let mut result = Vec::new();
            while something_resolved & valid {
                debug_i!("calculating filters loop {count}");
                indent!();
                count += 1;
                something_resolved = false;

                result = zip(call.parameters.iter(), function.parameters.iter())
                    .map(|(expr, param)| {
                        if !valid {
                            return Ok(());
                        }
                        let resolved_count = resolved_generic_types.len();
                        debug_i!("expr {expr}");

                        // TODO optimize
                        let param_type = substitute(&param.ast_type, &resolved_generic_types)
                            .unwrap_or(param.ast_type.clone());
                        debug_i!("real expression : {expr}");
                        debug_i!("real type of expression : {param_type}");

                        let t = self.get_filter(
                            module,
                            val_context,
                            statics,
                            &mut resolved_generic_types,
                            expr,
                            &param_type,
                        )?;
                        if resolved_count != resolved_generic_types.len() {
                            something_resolved = true;
                        }
                        debug_i!("filter {t}");
                        if !t.almost_equal(&param_type)? {
                            valid = false
                        }
                        Ok(())
                    })
                    .collect();
                dedent!();
            }

            let result = result
                .into_iter()
                .collect::<Result<Vec<_>, TypeCheckError>>();

            if let Err(e) = result {
                errors.push(e.clone());
                debug_i!("ignored function due to {e}");
                dedent!();
                continue;
            }

            if valid {
                if let Some(rt) = expected_return_type {
                    //let rt = substitute(rt, &resolved_generic_types).unwrap_or(rt.clone());
                    valid = valid
                        && TypeFilter::Exact(rt.clone())
                            .almost_equal(&function.return_type)
                            .unwrap_or(false);
                }
            }

            if valid {
                let generic_types_coeff: usize = Self::function_generic_coeff(function);

                valid_functions.push((
                    function.clone(),
                    generic_types_coeff,
                    resolved_generic_types,
                ));
                debug_i!("it's valid with generic_types_coeff {generic_types_coeff}");
            }
            dedent!();
        }

        if valid_functions.is_empty() {
            self.stack.pop();
            dedent!();
            Err(TypeCheckError::from(format!(
                "call {} : {}\ncannot find a valid function\n{}",
                call.original_function_name,
                call.index,
                SliceDisplay(&errors.iter().map(|it| format!("{it}")).collect::<Vec<_>>())
            )))
        } else if valid_functions.len() > 1 {
            // we must disambiguate.
            // For now we get the function that has the minimal generic coefficient, if there's only one with that coefficient.
            // The coefficient is the sum of the parameter's type generic coefficient, the coefficient
            // of <T> is more generic than Option<T> that is more generic than Option<List<T>>
            let min = valid_functions.iter().map(|it| it.1).min().unwrap();

            let mut dis_valid_functions = valid_functions
                .into_iter()
                .filter(|it| it.1 == min)
                .collect::<Vec<_>>();

            if dis_valid_functions.is_empty() {
                self.stack.pop();
                // I think it should not happen
                dedent!();
                return Err(TypeCheckError::from(format!(
                    "call {call} : {}\ncannot find a valid function",
                    call.index,
                )));
            } else if dis_valid_functions.len() > 1 {
                self.stack.pop();
                dedent!();
                return Err(TypeCheckError::from(format!(
                    "call {call} : {}\nfound more than one valid function {}",
                    call.index,
                    SliceDisplay(
                        &dis_valid_functions
                            .iter()
                            .map(|it| &it.0)
                            .collect::<Vec<_>>()
                    )
                )));
            }
            let (valid_function, _x, resolved_generic_types) = dis_valid_functions.remove(0);
            Ok((valid_function, resolved_generic_types))
        } else {
            let (valid_function, _x, resolved_generic_types) = valid_functions.remove(0);
            Ok((valid_function, resolved_generic_types))
        }
    }

    pub fn function_generic_coeff(function: &ASTFunctionDef) -> usize {
        function
            .parameters
            .iter()
            .map(|it| Self::generic_type_coeff(&it.ast_type))
            .sum()
    }

    fn get_filter(
        &mut self,
        module: &InputModule,
        val_context: &mut ValContext,
        statics: &mut Statics,
        resolved_generic_types: &mut ResolvedGenericTypes,
        expr: &ASTExpression,
        param_type: &ASTType,
    ) -> Result<TypeFilter, TypeCheckError> {
        let e = self.transform_expression(module, expr, val_context, statics, Some(&param_type))?;

        let t = self.type_of_expression(module, &e, val_context, statics, Some(&param_type))?;
        if let TypeFilter::Exact(et) = &t {
            if !is_generic_type(et) {
                resolved_generic_types
                    .extend(resolve_generic_types_from_effective_type(&param_type, et)?)?;
            }
        } else if let TypeFilter::Lambda(_, Some(lrt)) = &t {
            if let TypeFilter::Exact(et) = lrt.deref() {
                if !is_generic_type(et) {
                    if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters: _,
                        return_type,
                    }) = param_type
                    {
                        resolved_generic_types.extend(
                            resolve_generic_types_from_effective_type(return_type.deref(), et)?,
                        )?;
                    }
                }
            }
        }
        Ok(t)
    }

    ///
    /// return a coefficient that is higher for how the type is generic
    ///
    pub fn generic_type_coeff(ast_type: &ASTType) -> usize {
        Self::generic_type_coeff_internal(ast_type, usize::MAX / 100)
    }

    fn generic_type_coeff_internal(ast_type: &ASTType, coeff: usize) -> usize {
        if is_generic_type(ast_type) {
            match ast_type {
                ASTType::Builtin(_) => 0,
                ASTType::Generic(_) => coeff,
                ASTType::Custom {
                    name: _,
                    param_types,
                    index: _,
                } => param_types
                    .iter()
                    .map(|it| Self::generic_type_coeff_internal(it, coeff / 100))
                    .sum(),
                ASTType::Unit => 0,
            }
        } else {
            0
        }
    }

    fn transform_function(
        &mut self,
        module: &InputModule,
        statics: &mut Statics,
        new_function_def: &ASTFunctionDef,
        backend: &dyn Backend,
    ) -> Result<Option<ASTFunctionBody>, TypeCheckError> {
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
                Some(ASTFunctionBody::RASMBody(new_statements))
            }
            ASTFunctionBody::ASMBody(asm_body) => {
                let type_def_provider = DummyTypeDefProvider::new();
                let called_functions = backend.called_functions(
                    None,
                    Some(new_function_def),
                    asm_body,
                    &val_context,
                    &type_def_provider,
                );

                if called_functions.is_empty() {
                    None
                } else {
                    let mut lines: Vec<String> =
                        asm_body.lines().map(|it| it.to_owned()).collect::<Vec<_>>();

                    for (_m, f) in called_functions.iter() {
                        let call = f.to_call(new_function_def);
                        let new_call =
                            self.transform_call(module, &call, &mut val_context, statics, None)?;
                        debug_i!("old line {}", lines[f.i]);

                        let new_line =
                            lines[f.i].replace(&call.function_name, &new_call.function_name);
                        debug_i!("new line {}", new_line);
                        lines[f.i] = new_line;
                    }

                    Some(ASTFunctionBody::ASMBody(lines.join("\n")))
                }
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
        result.map_err(|it| {
            it.add(format!(
                "transforming expressions, expected_return_type {}",
                OptionDisplay(&expected_return_type)
            ))
        })
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
                            ValKind::LetRef(_, t, _index) => t.clone(),
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
                }

                if let Some(f) = self
                    .module
                    .find_precise_function(&call.original_function_name, &call.function_name)
                {
                    dedent!();
                    return Ok(TypeFilter::Exact(f.return_type.clone()));
                }

                if let Ok(transformed_call) =
                    self.transform_call(module, call, val_context, statics, expected_type)
                {
                    if let Some(f) = self
                        .module
                        .find_precise_function(
                            &transformed_call.original_function_name,
                            &transformed_call.function_name,
                        )
                        .cloned()
                    {
                        dedent!();
                        return Ok(TypeFilter::Exact(f.return_type.clone()));
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
                Some(ValKind::LetRef(_, t, _index)) => TypeFilter::Exact(t.clone()),
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
                debug_i!("lambda return type {}", OptionDisplay(&return_type));
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
    ) -> Result<ASTLambdaDef, TypeCheckError> {
        let mut new_lambda = lambda_def.clone();

        let mut val_context = ValContext::new(Some(val_context));

        Self::add_lambda_parameters_to_val_context(lambda_def, &expected_type, &mut val_context)?;

        let ert = if let Some(et) = expected_type {
            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters: _,
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
                return_type: _,
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
                return Err(TypeCheckError::from(format!(
                    "Expecting lambda but got {}",
                    OptionDisplay(expected_type)
                )));
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
    use crate::codegen::enhanced_module::EnhancedASTModule;
    use crate::codegen::statics::Statics;
    use crate::codegen::val_context::ValContext;
    use crate::new_type_check2::TypeCheck;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTLambdaDef,
        ASTModule, ASTParameterDef, ASTStatement, ASTType, BuiltinTypeKind, ValueType,
    };
    use crate::project::project::RasmProject;
    use crate::transformations::enrich_module;
    use crate::transformations::type_functions_creator::type_mandatory_functions;
    use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
    use crate::type_check::type_check_error::TypeCheckError;
    use crate::type_check::typed_ast::{convert_to_typed_module, get_default_functions};

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

    #[test]
    pub fn test_generic_type_coeff() {
        assert_eq!(
            usize::MAX / 100,
            TypeCheck::generic_type_coeff(&ASTType::Generic("".to_owned()))
        );
    }

    #[test]
    pub fn test_generic_type_coeff_1() {
        assert_eq!(
            0,
            TypeCheck::generic_type_coeff(&ASTType::Builtin(BuiltinTypeKind::I32))
        );
    }

    #[test]
    pub fn test_generic_type_coeff_2() {
        assert_eq!(
            0,
            TypeCheck::generic_type_coeff(&ASTType::Custom {
                param_types: vec![ASTType::Builtin(BuiltinTypeKind::I32)],
                name: "".to_owned(),
                index: ASTIndex::none()
            },)
        );
    }

    #[test]
    pub fn test_generic_type_coeff_3() {
        assert_eq!(
            usize::MAX / 100 / 100,
            TypeCheck::generic_type_coeff(&ASTType::Custom {
                param_types: vec![ASTType::Generic("".to_owned())],
                name: "".to_owned(),
                index: ASTIndex::none()
            },)
        );
    }

    #[test]
    pub fn test_add() {
        init();
        let mut check = TypeCheck::new();

        let mut module = EnhancedASTModule::new(ASTModule::new());
        let function_def = ASTFunctionDef {
            original_name: "add".to_string(),
            name: "add".to_string(),
            parameters: vec![
                ASTParameterDef {
                    name: "v1".to_string(),
                    ast_type: ASTType::Builtin(BuiltinTypeKind::I32),
                    ast_index: ASTIndex::none(),
                },
                ASTParameterDef {
                    name: "v2".to_string(),
                    ast_type: ASTType::Builtin(BuiltinTypeKind::I32),
                    ast_index: ASTIndex::none(),
                },
            ],
            return_type: ASTType::Builtin(BuiltinTypeKind::I32),
            body: ASTFunctionBody::ASMBody("".to_owned()),
            inline: false,
            generic_types: vec![],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
        };
        module.add_function("add".to_owned(), function_def);

        let inner_call = ASTFunctionCall {
            original_function_name: "add".to_string(),
            function_name: "add".to_string(),
            parameters: vec![
                ASTExpression::Value(ValueType::I32(1), ASTIndex::none()),
                ASTExpression::Value(ValueType::I32(2), ASTIndex::none()),
            ],
            index: ASTIndex::none(),
        };

        let call = ASTFunctionCall {
            original_function_name: "add".to_string(),
            function_name: "add".to_string(),
            parameters: vec![
                ASTExpression::ASTFunctionCallExpression(inner_call.clone()),
                ASTExpression::ASTFunctionCallExpression(inner_call.clone()),
            ],
            index: ASTIndex::none(),
        };

        let mut val_context = ValContext::new(None);
        let mut statics = Statics::new();

        let transformed_call = check
            .transform_call(&module, &call, &mut val_context, &mut statics, None)
            .unwrap();

        println!("{transformed_call}");
        check.module.print();
    }

    #[test]
    pub fn test_option_none() {
        init();

        let mut check = TypeCheck::new();

        let mut module = EnhancedASTModule::new(ASTModule::new());
        let option_t = ASTType::Custom {
            name: "Option".to_owned(),
            param_types: vec![ASTType::Generic("T".to_owned())],
            index: ASTIndex::none(),
        };

        let function_def = ASTFunctionDef {
            original_name: "orElse".to_string(),
            name: "orElse".to_string(),
            parameters: vec![
                ASTParameterDef {
                    name: "v1".to_string(),
                    ast_type: option_t.clone(),
                    ast_index: ASTIndex::none(),
                },
                ASTParameterDef {
                    name: "v2".to_string(),
                    ast_type: ASTType::Builtin(BuiltinTypeKind::Lambda {
                        return_type: Box::new(option_t.clone()),
                        parameters: vec![],
                    }),
                    ast_index: ASTIndex::none(),
                },
            ],
            return_type: option_t.clone(),
            body: ASTFunctionBody::ASMBody("".to_owned()),
            inline: false,
            generic_types: vec!["T".to_owned()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
        };
        module.add_function("orElse".to_owned(), function_def);

        let function_def = ASTFunctionDef {
            original_name: "Option::None".to_string(),
            name: "Option::None".to_string(),
            parameters: vec![],
            return_type: option_t.clone(),
            body: ASTFunctionBody::ASMBody("".to_owned()),
            inline: false,
            generic_types: vec!["T".to_owned()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
        };
        module.add_function("Option::None".to_owned(), function_def);

        let function_def = ASTFunctionDef {
            original_name: "Option::Some".to_string(),
            name: "Option::Some".to_string(),
            parameters: vec![ASTParameterDef {
                name: "v1".to_string(),
                ast_type: ASTType::Generic("T".to_owned()),
                ast_index: ASTIndex::none(),
            }],
            return_type: option_t.clone(),
            body: ASTFunctionBody::ASMBody("".to_owned()),
            inline: false,
            generic_types: vec!["T".to_owned()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
        };
        module.add_function("Option::Some".to_owned(), function_def);

        module.print();

        let call_to_none = ASTFunctionCall {
            original_function_name: "Option::None".to_string(),
            function_name: "Option::None".to_string(),
            parameters: vec![],
            index: ASTIndex::none(),
        };

        let call_to_some = ASTFunctionCall {
            original_function_name: "Option::Some".to_string(),
            function_name: "Option::Some".to_string(),
            parameters: vec![ASTExpression::Value(ValueType::I32(10), ASTIndex::none())],
            index: ASTIndex::none(),
        };

        let call = ASTFunctionCall {
            original_function_name: "orElse".to_string(),
            function_name: "orElse".to_string(),
            parameters: vec![
                ASTExpression::ASTFunctionCallExpression(call_to_none.clone()),
                ASTExpression::Lambda(ASTLambdaDef {
                    body: vec![ASTStatement::Expression(
                        ASTExpression::ASTFunctionCallExpression(call_to_some),
                    )],
                    parameter_names: vec![],
                    index: ASTIndex::none(),
                }),
            ],
            index: ASTIndex::none(),
        };

        let mut val_context = ValContext::new(None);
        let mut statics = Statics::new();

        let transformed_call = check
            .transform_call(&module, &call, &mut val_context, &mut statics, None)
            .unwrap();

        println!("{transformed_call}");
        check.module.print();
    }

    fn test_project(project: RasmProject) -> Result<(), TypeCheckError> {
        let (module, backend, mut statics) = to_ast_module(project);

        let module = EnhancedASTModule::new(module);

        let mandatory_functions = type_mandatory_functions(&module);

        let default_functions = get_default_functions(false);
        //resolved_module.print();

        let typed_module = convert_to_typed_module(
            &module,
            false,
            mandatory_functions,
            &backend,
            &mut statics,
            true,
            default_functions,
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
