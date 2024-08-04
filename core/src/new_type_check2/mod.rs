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

use itertools::Itertools;
use linked_hash_map::LinkedHashMap;
use linked_hash_set::LinkedHashSet;
use std::iter::zip;
use std::ops::Deref;

use crate::codegen::compile_target::CompileTarget;
use log::{debug, info};

use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::DummyTypeDefProvider;
use crate::codegen::val_context::ValContext;
use crate::codegen::ValKind;
use crate::errors::{CompilationError, CompilationErrorKind};
use crate::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTLambdaDef,
    ASTNameSpace, ASTParameterDef, ASTStatement, ASTType, BuiltinTypeKind, ValueType,
};
use crate::type_check::functions_container::{FunctionsContainer, TypeFilter};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::type_check_error::{TypeCheckError, TypeCheckErrorKind};
use crate::type_check::typed_ast::DefaultFunction;
use crate::type_check::{resolve_generic_types_from_effective_type, substitute};
use crate::utils::{OptionDisplay, SliceDisplay};

type InputModule = EnhancedASTModule;
type OutputModule = EnhancedASTModule;

pub struct TypeCheck {
    module: OutputModule,
    stack: Vec<ASTIndex>,
    functions_stack: LinkedHashMap<String, Vec<ASTIndex>>,
}

impl TypeCheck {
    pub fn new(body_namespace: &ASTNameSpace) -> Self {
        let typed_module = EnhancedASTModule {
            body: vec![],
            functions_by_name: FunctionsContainer::new(),
            enums: vec![],
            structs: vec![],
            types: vec![],
            body_namespace: body_namespace.clone(),
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
        statics: &mut Statics,
        default_functions: Vec<DefaultFunction>,
        mandatory_functions: Vec<DefaultFunction>,
        target: &CompileTarget,
        debug: bool,
    ) -> Result<OutputModule, CompilationError> {
        let mut val_context = ValContext::new(None);

        let mut default_functions = default_functions; // TODO print_allocation

        default_functions.extend(mandatory_functions);

        self.module.types = module.types.clone();
        self.module.enums = module.enums.clone();
        self.module.structs = module.structs.clone();
        self.module.functions_by_name = FunctionsContainer::new();

        for default_function in default_functions {
            let call = default_function.to_call(&ASTNameSpace::global());

            if let Some(f) =
                module.find_precise_function(&call.original_function_name, &call.function_name)
            {
                // TODO check error
                self.module
                    .functions_by_name
                    .add_function(call.original_function_name.clone(), f.clone());
                self.functions_stack.insert(f.name.clone(), vec![]);
            } else {
                return Err(Self::compilation_error(format!(
                    "Cannot find default function {}",
                    call.function_name
                )));
            }
        }

        let mut new_functions = Vec::new();

        let new_body = self
            .transform_statements(
                module,
                &module.body,
                &mut val_context,
                statics,
                None,
                &module.body_namespace,
                None,
                &mut new_functions,
            )
            .map_err(|it| CompilationError {
                index: ASTIndex::none(),
                error_kind: CompilationErrorKind::TypeCheck(
                    format!(
                        "Error transforming statements for module body, body namespace {}",
                        module.body_namespace
                    ),
                    vec![it],
                ),
            })?;

        for (f, s) in new_functions {
            let new_function_name = f.name.clone();
            if !self.functions_stack.contains_key(&new_function_name) {
                self.module
                    .functions_by_name
                    .add_function(f.original_name.clone(), f);
                self.functions_stack.insert(new_function_name, s);
            }
        }

        self.module.body = new_body;

        let mut deleted = false;

        loop {
            let functions_len = self.module.functions().len();
            info!("Type check loop {}", functions_len);

            let new_function_names = self.functions_stack.keys().cloned().collect::<Vec<_>>();

            deleted = false;

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

                let mut new_functions = Vec::new();
                match self
                    .transform_function(
                        module,
                        statics,
                        &function,
                        target,
                        debug,
                        &mut new_functions,
                    )
                    .map_err(|it| CompilationError {
                        index: function.index.clone(),
                        error_kind: CompilationErrorKind::TypeCheck(
                            format!(
                                "Error in function {} : {}",
                                function.original_name, function.index
                            ),
                            vec![
                                TypeCheckError::new(
                                    function.index.clone(),
                                    format!(
                                        "Error in function {} ({})",
                                        function.name, function.original_name
                                    ),
                                    self.stack.clone(),
                                ),
                                it,
                            ],
                        ),
                    })? {
                    Some(new_body) => self
                        .module
                        .functions_by_name
                        .replace_body(&function, new_body),
                    None => {}
                }
                for (f, s) in new_functions {
                    let new_function_name = f.name.clone();
                    if !self.functions_stack.contains_key(&new_function_name) {
                        self.module
                            .functions_by_name
                            .add_function(f.original_name.clone(), f);
                        self.functions_stack.insert(new_function_name, s);
                    }
                }
            }

            if self.module.functions().len() == functions_len {
                break;
            }
        }

        Ok(self.module)
    }

    fn compilation_error(message: String) -> CompilationError {
        CompilationError {
            index: ASTIndex::none(),
            error_kind: CompilationErrorKind::TypeCheck(message, Vec::new()),
        }
    }

    fn transform_statement(
        &mut self,
        module: &InputModule,
        statement: &ASTStatement,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_return_type: Option<&ASTType>,
        namespace: &ASTNameSpace,
        inside_function: Option<&ASTFunctionDef>,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
    ) -> Result<ASTStatement, TypeCheckError> {
        match statement {
            ASTStatement::Expression(e) => self
                .transform_expression(
                    module,
                    e,
                    val_context,
                    statics,
                    expected_return_type,
                    namespace,
                    inside_function,
                    new_functions,
                    true,
                )
                .map(ASTStatement::Expression),
            ASTStatement::LetStatement(name, e, is_const, index) => self
                .transform_expression(
                    module,
                    e,
                    val_context,
                    statics,
                    None,
                    namespace,
                    inside_function,
                    new_functions,
                    true,
                )
                .map(|it| ASTStatement::LetStatement(name.clone(), it, *is_const, index.clone())),
        }
    }

    fn transform_expression(
        &mut self,
        module: &InputModule,
        expression: &ASTExpression,
        val_context: &ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
        namespace: &ASTNameSpace,
        inside_function: Option<&ASTFunctionDef>,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
        strict: bool,
    ) -> Result<ASTExpression, TypeCheckError> {
        match expression {
            ASTExpression::ASTFunctionCallExpression(call) => self
                .transform_call(
                    module,
                    call,
                    val_context,
                    statics,
                    expected_type,
                    namespace,
                    inside_function,
                    new_functions,
                    strict,
                )
                .map(ASTExpression::ASTFunctionCallExpression),
            ASTExpression::Lambda(lambda_def) => self
                .transform_lambda_def(
                    module,
                    lambda_def,
                    val_context,
                    statics,
                    expected_type,
                    namespace,
                    inside_function,
                    new_functions,
                )
                .map(ASTExpression::Lambda),
            _ => Ok(expression.clone()),
        } /*
          .map_err(|it| {
              it.add(
                  expression.get_index(),
                  format!(
                      "converting expression, expected_type {}",
                      OptionDisplay(&expected_type),
                  ),
                  self.stack.clone(),
              )
          })*/
    }

    fn transform_call(
        &mut self,
        module: &InputModule,
        call: &ASTFunctionCall,
        val_context: &ValContext,
        statics: &mut Statics,
        expected_return_type: Option<&ASTType>,
        namespace: &ASTNameSpace,
        inside_function: Option<&ASTFunctionDef>,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
        strict: bool,
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

        if let Some((_return_type, parameters_types)) = val_context.get_lambda(&call.function_name)
        {
            let new_expressions: Vec<ASTExpression> =
                zip(call.parameters.iter(), parameters_types.clone().iter())
                    .map(|(it, ast_type)| {
                        self.transform_expression(
                            module,
                            it,
                            val_context,
                            statics,
                            Some(ast_type),
                            namespace,
                            inside_function,
                            new_functions,
                            strict,
                        )
                    })
                    .collect::<Result<Vec<_>, TypeCheckError>>()
                    .map_err(|it| {
                        dedent!();
                        it.add(
                            call.index.clone(),
                            format!(
                                "converting expressions in call {}",
                                call.original_function_name
                            ),
                            self.stack.clone(),
                        )
                    })?;

            let mut new_call = call.clone();
            new_call.parameters = new_expressions;
            dedent!();
            return Ok(new_call);
        }

        self.stack.push(call.index.clone());

        let (mut new_function_def, resolved_generic_types, new_expressions) = self
            .get_valid_function(
                module,
                call,
                val_context,
                statics,
                expected_return_type,
                namespace,
                inside_function,
                new_functions,
                strict,
            )
            .map_err(|it| {
                self.stack.pop();
                dedent!();
                it.clone()
            })?;

        debug_i!("found valid function {new_function_def}");

        if !new_function_def.generic_types.is_empty() {
            for p in new_function_def.parameters.iter_mut() {
                if let Some(new_t) = substitute(&p.ast_type, &resolved_generic_types) {
                    p.ast_type = new_t;
                }
                if p.ast_type.is_generic() {
                    dedent!();
                    if strict {
                        let result = Err(TypeCheckError::new(
                            p.ast_index.clone(),
                            format!(
                                "Unresolved generic type {} : {resolved_generic_types}",
                                p.ast_type,
                            ),
                            self.stack.clone(),
                        ));
                        self.stack.pop();
                        return result;
                    } else {
                        self.stack.pop();
                        return Ok(call.clone());
                    }
                }
            }

            if let Some(new_t) = substitute(&new_function_def.return_type, &resolved_generic_types)
            {
                new_function_def.return_type = new_t;
            }

            if new_function_def.return_type.is_generic() {
                dedent!();
                if strict {
                    let result = Err(TypeCheckError::new(
                        new_function_def.index.clone(),
                        format!(
                            "Unresolved generic return type {}, expected return type {}",
                            new_function_def.return_type,
                            OptionDisplay(&expected_return_type)
                        ),
                        self.stack.clone(),
                    ));
                    self.stack.pop();
                    return result;
                } else {
                    self.stack.pop();
                    return Ok(call.clone());
                }
            }

            new_function_def.resolved_generic_types = resolved_generic_types;
            new_function_def.generic_types = Vec::new();
        }

        let new_function_name = Self::unique_function_name(&new_function_def);

        new_function_def.name = new_function_name.clone();

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

            new_functions.push((new_function_def, self.stack.clone()));
            // TODO check error

            /*
            self.module
                .functions_by_name
                .add_function(new_function_def.original_name.clone(), new_function_def);
            self.functions_stack
                .insert(new_function_name, self.stack.clone());
            */
        }
        dedent!();
        self.stack.pop();
        Ok(new_call)
    }

    fn unique_function_name(new_function_def: &ASTFunctionDef) -> String {
        let namespace = new_function_def.namespace.safe_name();
        let name = new_function_def.original_name.replace("::", "_");

        format!(
            "{namespace}_{name}_{}_{}",
            new_function_def
                .parameters
                .iter()
                .map(|it| Self::unique_type_name(&it.ast_type))
                .collect::<Vec<_>>()
                .join("_"),
            Self::unique_type_name(&new_function_def.return_type)
        )
    }

    fn unique_type_name(ast_type: &ASTType) -> String {
        match ast_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    format!(
                        "fn{}{}",
                        parameters
                            .iter()
                            .map(Self::unique_type_name)
                            .collect::<Vec<_>>()
                            .join(""),
                        Self::unique_type_name(return_type)
                    )
                }
                _ => format!("{ast_type}"),
            },
            ASTType::Generic(_name) => panic!(),
            ASTType::Custom {
                namespace,
                name,
                param_types,
                index: _,
            } => {
                format!(
                    "{}{name}{}",
                    namespace.safe_name(),
                    param_types
                        .iter()
                        .map(Self::unique_type_name)
                        .collect::<Vec<_>>()
                        .join("")
                )
            }
            ASTType::Unit => "Unit".to_string(),
        }
    }

    pub fn get_valid_function(
        &mut self,
        module: &InputModule,
        call: &ASTFunctionCall,
        val_context: &ValContext,
        statics: &mut Statics,
        expected_return_type: Option<&ASTType>,
        namespace: &ASTNameSpace,
        inside_function: Option<&ASTFunctionDef>,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
        strict: bool,
    ) -> Result<(ASTFunctionDef, ResolvedGenericTypes, Vec<ASTExpression>), TypeCheckError> {
        debug_i!(
            "get_valid_function call {call} expected_return_type {}: {}",
            OptionDisplay(&expected_return_type),
            call.index
        );
        indent!();

        let mut valid_functions: Vec<(
            ASTFunctionDef,
            usize,
            ResolvedGenericTypes,
            Vec<ASTExpression>,
        )> = Vec::new();
        let mut errors = Vec::new();

        /*
        let same_in_new_functions = new_functions
            .iter()
            .map(|(f, _i)| {
                let mut ff = f.clone();
                ff.rank += 1;
                ff
            })
            .filter(|it| it.original_name == call.original_function_name)
            .collect_vec();
        */

        let first_type = Self::get_first_type(call, val_context);

        let original_functions = module
            .find_functions_by_original_name(&call.original_function_name)
            .iter()
            //.chain(same_in_new_functions.iter())
            .filter(|it| {
                (it.modifiers.public || &it.namespace == namespace)
                    && it.parameters.len() == call.parameters.len()
            })
            .filter(|it| {
                if let Some(ref ft) = first_type {
                    match TypeFilter::Exact(ft.clone())
                        .almost_equal(&it.parameters.get(0).unwrap().ast_type, module)
                    {
                        Ok(b) => b,
                        Err(_) => false,
                    }
                } else {
                    true
                }
            })
            .sorted_by(|fn1, fn2| fn1.rank.cmp(&fn2.rank));
        /*
        if new_functions.iter().any(|(f, i)| {
            f.original_name == call.original_function_name
                && (f.modifiers.public || &f.namespace == namespace)
                && f.parameters.len() == call.parameters.len()
        }) {
            println!("found a function");
        }
        */

        let filters_resolved_generic_types = ResolvedGenericTypes::new();
        /*

        let mut filters_new_functions = Vec::new();

        let filters: Result<Vec<(TypeFilter, ASTExpression)>, TypeCheckError> = call
            .parameters
            .iter()
            .filter(|e| !matches!(e, ASTExpression::Lambda(_)))
            .map(|expr| {
                let new_filter = self.get_filter(
                    module,
                    val_context,
                    statics,
                    &mut filters_resolved_generic_types,
                    expr,
                    None,
                    namespace,
                    inside_function,
                    &mut filters_new_functions,
                );

                /*
                if let Ok((nf, _)) = &new_filter {
                    println!("filter for {expr} : {}", expr.get_index());
                    println!("  {nf}");
                }
                */

                new_filter
            })
            .collect();
        */

        let mut ok_inner_new_functions = Vec::new();

        for function in original_functions {
            let mut inner_new_functions = Vec::new();

            debug_i!("verifying function {function}");

            if let Some((old_function, _, _, _)) = valid_functions.get(0) {
                // if we have already found a valid function, check another function only if it has the same rank,
                // because if two functions are valid, but with the same rank, there's a problem: we cannot identify
                // the right function to call
                if old_function.rank != function.rank {
                    debug_i!("A valid function has already been found and the current one has a different rank.");
                    break;
                }
            }

            indent!();

            let mut resolved_generic_types = ResolvedGenericTypes::new();

            if !call.generics.is_empty() {
                if call.generics.len() != function.generic_types.len() {
                    errors.push(TypeCheckError::new(
                        function.index.clone(),
                        format!(
                            "Not matching generics expected {} but got {}",
                            call.generics.len(),
                            function.generic_types.len()
                        ),
                        self.stack.clone(),
                    ));
                    dedent!();
                    continue;
                }
                zip(function.generic_types.iter(), call.generics.iter()).for_each(|(g, t)| {
                    let ast_type = if let Some(ifun) = inside_function {
                        if let Some(st) = substitute(t, &ifun.resolved_generic_types) {
                            st
                        } else {
                            t.clone()
                        }
                    } else {
                        t.clone()
                    };

                    resolved_generic_types.insert(g.to_string(), ast_type);
                });
            }

            if let Some(rt) = expected_return_type {
                if !TypeFilter::Exact(function.return_type.clone())
                    .almost_equal(rt, module)
                    .map_err(|it| {
                        dedent!();
                        dedent!();
                        it.clone()
                    })?
                {
                    errors.push(TypeCheckError::new(
                        function.index.clone(),
                        format!(
                            "Function {function}, unmatching return type expected {rt} but got {} : {}",
                            function.return_type,
                            function.index
                        ),
                        self.stack.clone(),
                    ));
                    dedent!();
                    continue;
                }

                if !rt.is_generic() && function.return_type.is_generic() {
                    if let Ok(result) =
                        resolve_generic_types_from_effective_type(&function.return_type, rt)
                    {
                        if let Err(e) = resolved_generic_types.extend(result) {
                            errors.push(TypeCheckError::new(
                                function.index.clone(),
                                format!(
                                    "{e} resolving generic type {} with {rt}",
                                    function.return_type
                                ),
                                self.stack.clone(),
                            ));
                            dedent!();
                            continue;
                        }
                    }
                }
            }

            /*
            if let Ok(filters) = &filters {
                if filters.iter().all(|(f, _)| {
                    if let TypeFilter::Exact(t) = f {
                        !matches!(t, ASTType::Generic(_))
                    } else {
                        false
                    }
                }) && function.generic_types.is_empty()
                {
                    if zip(filters.iter(), function.parameters.iter())
                        .all(|((f, _), p)| f.almost_equal(&p.ast_type, module).unwrap_or(false))
                    {
                        /*
                        println!(
                            "found good function {function} for {}",
                            SliceDisplay(&filters.iter().map(|(f, _)| f).collect::<Vec<_>>())
                        );
                        */

                        new_functions.append(&mut filters_new_functions);

                        valid_functions.push((
                            function.clone(),
                            function.rank,
                            filters_resolved_generic_types,
                            filters.iter().map(|(_, e)| e.clone()).collect(),
                        ));
                        dedent!();
                        break;
                    }
                }
            }
            */

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
                            return Err(TypeCheckError::dummy());
                        }
                        let resolved_count = resolved_generic_types.len();
                        debug_i!("expr {expr}");

                        let substituted_type = substitute(&param.ast_type, &resolved_generic_types);

                        let param_type = if let Some(ast_type) = &substituted_type {
                            ast_type
                        } else {
                            &param.ast_type
                        };
                        debug_i!("real expression : {expr}");

                        debug_i!("real type of expression : {param_type}");

                        let (t, e) = self
                            .get_filter(
                                module,
                                val_context,
                                statics,
                                &mut resolved_generic_types,
                                expr,
                                Some(param_type),
                                namespace,
                                inside_function,
                                &mut inner_new_functions,
                                strict
                            )?/*
                            .map_err(|it| {
                                it.add(
                                    expr.get_index(),
                                    format!(
                                        "getting filter from expression {expr} for {param_type}"
                                    ),
                                    self.stack.clone(),
                                )
                            })?*/;
                        if resolved_count != resolved_generic_types.len() {
                            something_resolved = true;
                        }
                        debug_i!("filter {t}");
                        if !t.almost_equal(param_type, module)? {
                            valid = false;
                            return Err(TypeCheckError::new_ignorable(
                                expr.get_index(),
                                format!("not matching type expected {t} got {param_type}"),
                                self.stack.clone(),
                            ));
                        }
                        Ok(e)
                    })
                    .filter(|it| !it.as_ref().is_err_and(|e| e.is_dummy()))
                    .collect();
                dedent!();
            }

            let result = result
                .into_iter()
                .collect::<Result<Vec<_>, TypeCheckError>>();

            if let Err(e) = result {
                debug_i!("ignored function due to {e}");

                let error = if matches!(e.kind, TypeCheckErrorKind::Ignorable) {
                    TypeCheckError::new_ignorable(
                        call.index.clone(),
                        format!("ignoring function {function} : {}", function.index),
                        self.stack.clone(),
                    )
                    .add_errors(vec![e])
                } else {
                    TypeCheckError::new(
                        call.index.clone(),
                        format!("ignoring function {function} : {}", function.index),
                        self.stack.clone(),
                    )
                    .add_errors(vec![e])
                };

                errors.push(error);
                dedent!();
                continue;
            }

            if valid {
                if let Some(rt) = expected_return_type {
                    //let rt = substitute(rt, &resolved_generic_types).unwrap_or(rt.clone());
                    valid = valid
                        && TypeFilter::Exact(rt.clone())
                            .almost_equal(&function.return_type, module)
                            .unwrap_or(false);
                }
            }

            if valid {
                if let Some((old_function, _, _, _)) = valid_functions.get(0) {
                    if old_function.rank == function.rank {
                        dedent!();
                        let error = TypeCheckError::new(
                            call.index.clone(),
                            format!("ignoring function {function} : {} because it has the same ranking of {old_function} : {}", function.index, old_function.index),
                            self.stack.clone(),
                        );
                        //self.stack.pop();
                        return Err(error);
                    }

                    // since function are already sorted by rank, we don't need to look at the other functions
                    dedent!();
                    break;
                } else {
                    ok_inner_new_functions = inner_new_functions;

                    valid_functions.push((
                        function.clone(),
                        function.rank,
                        resolved_generic_types,
                        result.unwrap(),
                    ));
                    debug_i!("it's valid with rank {}", function.rank);
                }
            }
            dedent!();
        }

        if valid_functions.is_empty() {
            dedent!();
            let result = Err(TypeCheckError::new(
                call.index.clone(),
                format!(
                    "cannot find a valid function from namespace {namespace} for call {}. Expected return type {}",
                    call.original_function_name,
                    OptionDisplay(&expected_return_type)),
                self.stack.clone(),
            ).add_errors(errors));
            //self.stack.pop();
            result
        } else if valid_functions.len() > 1 {
            // we must disambiguate, but it should not happen because we break when we found a valid function.
            // TODO we don't consider when two functions have the same coefficient...
            // We get the function that has the minimal rank, if there's only one with that coefficient.
            panic!();
            let min = valid_functions.iter().map(|it| it.1).min().unwrap();

            let mut dis_valid_functions = valid_functions
                .into_iter()
                .filter(|it| it.1 == min)
                .collect::<Vec<_>>();

            if dis_valid_functions.is_empty() {
                //self.stack.pop();
                // I think it should not happen
                dedent!();
                return Err(TypeCheckError::new(
                    call.index.clone(),
                    format!("call {call} cannot find a valid function",),
                    self.stack.clone(),
                ));
            } else if dis_valid_functions.len() > 1 {
                //self.stack.pop();
                dedent!();
                return Err(TypeCheckError::new(
                    call.index.clone(),
                    format!(
                        "call {call}\nfound more than one valid function {}",
                        SliceDisplay(
                            &dis_valid_functions
                                .iter()
                                .map(|it| &it.0)
                                .collect::<Vec<_>>()
                        )
                    ),
                    self.stack.clone(),
                ));
            }
            //self.stack.pop();
            dedent!();
            let (valid_function, _x, resolved_generic_types, expressions) =
                dis_valid_functions.remove(0);
            Ok((valid_function, resolved_generic_types, expressions))
        } else {
            new_functions.append(&mut ok_inner_new_functions);
            //self.stack.pop();
            dedent!();
            let (valid_function, _x, resolved_generic_types, expressions) =
                valid_functions.remove(0);
            Ok((valid_function, resolved_generic_types, expressions))
        }
    }

    fn get_first_type(call: &ASTFunctionCall, val_context: &ValContext) -> Option<ASTType> {
        if call.parameters.len() > 0 {
            if let Some(p) = call.parameters.get(0) {
                match p {
                    ASTExpression::StringLiteral(_, _) => {
                        Some(ASTType::Builtin(BuiltinTypeKind::String))
                    }
                    ASTExpression::ValueRef(name, _) => {
                        if let Some(t) = val_context.get(name) {
                            match t {
                                ValKind::ParameterRef(_, par) => Some(par.ast_type.clone()),
                                ValKind::LetRef(_, t, _) => Some(t.clone()),
                            }
                        } else {
                            None
                        }
                    }
                    ASTExpression::Value(vt_, _) => match vt_ {
                        ValueType::Boolean(_) => Some(ASTType::Builtin(BuiltinTypeKind::Bool)),
                        ValueType::I32(_) => Some(ASTType::Builtin(BuiltinTypeKind::I32)),
                        ValueType::Char(_) => Some(ASTType::Builtin(BuiltinTypeKind::Char)),
                        ValueType::F32(_) => Some(ASTType::Builtin(BuiltinTypeKind::F32)),
                    },
                    ASTExpression::Any(t) => Some(t.clone()),
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    ///
    /// lower means a better precedence
    ///
    pub fn function_precedence_coeff(function: &ASTFunctionDef) -> usize {
        let generic_coeff: usize = function
            .parameters
            .iter()
            .map(|it| Self::generic_type_coeff(&it.ast_type))
            .sum();

        generic_coeff
            + if matches!(function.body, ASTFunctionBody::NativeBody(_)) {
                0usize
            } else {
                1usize
            }
    }

    fn get_filter(
        &mut self,
        module: &InputModule,
        val_context: &ValContext,
        statics: &mut Statics,
        resolved_generic_types: &mut ResolvedGenericTypes,
        expr: &ASTExpression,
        param_type: Option<&ASTType>,
        namespace: &ASTNameSpace,
        inside_function: Option<&ASTFunctionDef>,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
        strict: bool,
    ) -> Result<(TypeFilter, ASTExpression), TypeCheckError> {
        let e = self.transform_expression(
            module,
            expr,
            val_context,
            statics,
            param_type,
            namespace,
            inside_function,
            new_functions,
            strict,
        )?;

        let t = self.type_of_expression(
            module,
            &e,
            val_context,
            statics,
            param_type,
            namespace,
            new_functions,
            strict,
        )?;
        if let TypeFilter::Exact(et) = &t {
            if !et.is_generic() {
                if let Some(pt) = param_type {
                    resolved_generic_types
                        .extend(resolve_generic_types_from_effective_type(pt, et)?)
                        .map_err(|it| {
                            TypeCheckError::new(
                                expr.get_index(),
                                format!("cannot resolve {pt} with {et}, {it}"),
                                self.stack.clone(),
                            )
                        })?;
                }
            }
        } else if let TypeFilter::Lambda(_, Some(lrt)) = &t {
            if let TypeFilter::Exact(et) = lrt.deref() {
                if !et.is_generic() {
                    if let Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters: _,
                        return_type,
                    })) = param_type
                    {
                        resolved_generic_types
                            .extend(resolve_generic_types_from_effective_type(
                                return_type.deref(),
                                et,
                            )?)
                            .map_err(|it| {
                                TypeCheckError::new(
                                    expr.get_index(),
                                    format!("cannot resolve {return_type} with {et}, {it}"),
                                    self.stack.clone(),
                                )
                            })?;
                    }
                }
            }
        }
        Ok((t, e))
    }

    ///
    /// return a coefficient that is higher for how the type is generic
    ///
    pub fn generic_type_coeff(ast_type: &ASTType) -> usize {
        Self::generic_type_coeff_internal(ast_type, usize::MAX / 100)
    }

    fn generic_type_coeff_internal(ast_type: &ASTType, coeff: usize) -> usize {
        if ast_type.is_generic() {
            match ast_type {
                ASTType::Builtin(_) => 0,
                ASTType::Generic(_) => coeff,
                ASTType::Custom {
                    namespace: _,
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
        target: &CompileTarget,
        debug: bool,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
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
            val_context
                .insert_par(parameter.name.clone(), parameter.clone())
                .map_err(|e| {
                    TypeCheckError::new(parameter.ast_index.clone(), e.clone(), self.stack.clone())
                })?;
        }

        let new_body = match &new_function_def.body {
            ASTFunctionBody::RASMBody(statements) => {
                let new_statements = self.transform_statements(
                    module,
                    statements,
                    &mut val_context,
                    statics,
                    Some(&new_function_def.return_type),
                    &new_function_def.namespace,
                    Some(&new_function_def),
                    new_functions,
                )?;
                Some(ASTFunctionBody::RASMBody(new_statements))
            }
            ASTFunctionBody::NativeBody(asm_body) => {
                let type_def_provider = DummyTypeDefProvider::new();

                let evaluator = target.get_evaluator(debug);
                let text_macro_names = evaluator
                    .get_macros(None, Some(new_function_def), asm_body, &type_def_provider)
                    .map_err(|it| {
                        dedent!();
                        TypeCheckError::new(
                            new_function_def.index.clone(),
                            format!("Error getting macros for {new_function_def}, {it}"),
                            self.stack.clone(),
                        )
                    })?
                    .iter()
                    .map(|(m, _i)| m.name.clone())
                    .collect::<LinkedHashSet<_>>();

                for text_macro_name in text_macro_names {
                    let default_function_calls = evaluator
                        .default_function_calls(&text_macro_name)
                        .map_err(|it| {
                            dedent!();
                            TypeCheckError::new(
                                new_function_def.index.clone(),
                                format!("Error getting macros for {new_function_def}, {it}"),
                                self.stack.clone(),
                            )
                        })?;
                    for f in default_function_calls {
                        let call = f.to_call(new_function_def);
                        let _new_call = self
                            .transform_call(
                                module,
                                &call,
                                &mut val_context,
                                statics,
                                None,
                                &new_function_def.namespace,
                                Some(new_function_def),
                                new_functions,
                                true,
                            )
                            .map_err(|it| {
                                dedent!();
                                it.clone()
                            })?;
                    }
                }

                let called_functions = target
                    .called_functions(
                        None,
                        Some(new_function_def),
                        asm_body,
                        &val_context,
                        &type_def_provider,
                        statics,
                        debug,
                    )
                    .map_err(|it| {
                        dedent!();
                        TypeCheckError::new(
                            new_function_def.index.clone(),
                            format!(
                                "Error determining function calls for {new_function_def}, {it}"
                            ),
                            self.stack.clone(),
                        )
                    })?;

                if called_functions.is_empty() {
                    None
                } else {
                    let mut lines: Vec<String> =
                        asm_body.lines().map(|it| it.to_owned()).collect::<Vec<_>>();

                    for (_m, f) in called_functions.iter() {
                        let call = f.to_call(new_function_def);
                        let new_call = self
                            .transform_call(
                                module,
                                &call,
                                &mut val_context,
                                statics,
                                None,
                                &new_function_def.namespace,
                                Some(new_function_def),
                                new_functions,
                                true,
                            )
                            .map_err(|it| {
                                dedent!();
                                it.clone()
                            })?;
                        debug_i!("old line {}", lines[f.i]);

                        let new_line =
                            lines[f.i].replace(&call.function_name, &new_call.function_name);
                        debug_i!("new line {}", new_line);
                        lines[f.i] = new_line;
                    }

                    Some(ASTFunctionBody::NativeBody(lines.join("\n")))
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
        namespace: &ASTNameSpace,
        inside_function: Option<&ASTFunctionDef>,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
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
                let new_statement = self.transform_statement(
                    module,
                    it,
                    val_context,
                    statics,
                    er,
                    namespace,
                    inside_function,
                    new_functions,
                );

                if let Ok(ASTStatement::LetStatement(name, expr, is_cons, index)) = &new_statement {
                    let type_of_expr = self.type_of_expression(
                        module,
                        expr,
                        val_context,
                        statics,
                        None,
                        namespace,
                        new_functions,
                        true,
                    )?;

                    if let TypeFilter::Exact(ast_type) = type_of_expr {
                        if *is_cons {
                            statics.add_const(name.clone(), ast_type);
                        } else {
                            val_context
                                .insert_let(name.clone(), ast_type, index)
                                .map_err(|it| {
                                    TypeCheckError::new(index.clone(), it, self.stack.clone())
                                })?;
                        }
                    } else {
                        return Err(TypeCheckError::new(
                            index.clone(),
                            format!("Cannot determine type of {expr}, type_of_epr {type_of_expr}"),
                            self.stack.clone(),
                        ));
                    }
                }

                new_statement
            })
            .collect::<Result<Vec<_>, TypeCheckError>>();
        dedent!();
        result.map_err(|it| {
            it.add(
                ASTIndex::none(),
                format!(
                    "transforming expressions, expected_return_type {}",
                    OptionDisplay(&expected_return_type)
                ),
                self.stack.clone(),
            )
        })
    }

    pub fn type_of_expression(
        &mut self,
        module: &InputModule,
        typed_expression: &ASTExpression,
        val_context: &ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
        namespace: &ASTNameSpace,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
        strict: bool,
    ) -> Result<TypeFilter, TypeCheckError> {
        debug_i!(
            "type_of_expression {typed_expression} expected type {}",
            OptionDisplay(&expected_type)
        );
        indent!();
        let result = match typed_expression {
            ASTExpression::StringLiteral(_, _) => {
                TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::String))
            }
            ASTExpression::ASTFunctionCallExpression(call) => {
                if val_context.is_lambda(&call.function_name) {
                    if let Some(v) = val_context.get(&call.function_name) {
                        let lambda = match v {
                            ValKind::ParameterRef(_, p) => p.ast_type.clone(),
                            ValKind::LetRef(_, t, _index) => t.clone(),
                        };

                        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters: _,
                            return_type,
                        }) = lambda
                        {
                            dedent!();
                            return Ok(TypeFilter::Exact(return_type.deref().clone()));
                        } else {
                            dedent!();
                            return Err(TypeCheckError::new(
                                call.index.clone(),
                                "It should not happen!!!".to_string(),
                                self.stack.clone(),
                            ));
                        }
                    };
                }

                if let Some(f) = self
                    .module
                    .find_precise_function(&call.original_function_name, &call.function_name)
                {
                    dedent!();
                    return Ok(TypeFilter::Exact(f.return_type.clone()));
                } else {
                    if let Some((f, _)) = new_functions
                        .iter()
                        .find(|(f, _)| f.name == call.function_name)
                    {
                        dedent!();
                        return Ok(TypeFilter::Exact(f.return_type.clone()));
                    }
                }

                match self.get_valid_function(
                    module,
                    call,
                    val_context,
                    statics,
                    expected_type,
                    namespace,
                    None,
                    new_functions,
                    strict,
                ) {
                    Ok((found_function, resolved_generic_types, _)) => {
                        let return_ast_type = if let Some(new_t) =
                            substitute(&found_function.return_type, &resolved_generic_types)
                        {
                            new_t
                        } else {
                            found_function.return_type
                        };

                        TypeFilter::Exact(return_ast_type)
                    }
                    Err(e) => {
                        debug_i!("{e}");
                        TypeFilter::Any
                    }
                }
            }
            ASTExpression::ValueRef(name, index) => match val_context.get(name) {
                None => {
                    if let Some(c) = statics.get_const(name) {
                        TypeFilter::Exact(c.ast_type.clone())
                    } else {
                        dedent!();
                        return Err(TypeCheckError::new(
                            index.clone(),
                            format!("Cannot find reference to {name}"),
                            self.stack.clone(),
                        ));
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
                if def.body.is_empty() {
                    dedent!();
                    return Ok(TypeFilter::Lambda(
                        def.parameter_names.len(),
                        Some(Box::new(TypeFilter::Exact(ASTType::Unit))),
                    ));
                }
                // I cannot go deep in determining the type
                if expected_type.is_none() {
                    dedent!();
                    return Ok(TypeFilter::Lambda(def.parameter_names.len(), None));
                }

                let mut return_type = Some(Box::new(TypeFilter::Exact(ASTType::Unit)));
                let mut lambda_val_context = ValContext::new(Some(val_context));

                self.add_lambda_parameters_to_val_context(
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
                            namespace,
                            new_functions,
                            strict,
                        )?;

                        if let TypeFilter::Exact(ast_type) = type_of_expr {
                            if *is_cons {
                                dedent!();
                                return Err(TypeCheckError::new(
                                    index.clone(),
                                    format!("Const not allowed here {expr}"),
                                    self.stack.clone(),
                                ));
                            } else {
                                lambda_val_context
                                    .insert_let(name.clone(), ast_type, index)
                                    .map_err(|it| {
                                        TypeCheckError::new(index.clone(), it, self.stack.clone())
                                    })?;
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
                                namespace,
                                new_functions,
                                strict,
                            )?));
                        }
                    }
                }
                debug_i!("lambda return type {}", OptionDisplay(&return_type));
                if def.parameter_names.is_empty() {
                    if let Some(TypeFilter::Exact(exact_return_type)) = return_type.as_deref() {
                        TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters: Vec::new(),
                            return_type: Box::new(exact_return_type.clone()),
                        }))
                    } else {
                        TypeFilter::Lambda(def.parameter_names.len(), return_type)
                    }
                } else {
                    TypeFilter::Lambda(def.parameter_names.len(), return_type)
                }
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
        val_context: &ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
        namespace: &ASTNameSpace,
        inside_function: Option<&ASTFunctionDef>,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
    ) -> Result<ASTLambdaDef, TypeCheckError> {
        let mut new_lambda = lambda_def.clone();

        let mut val_context = ValContext::new(Some(val_context));

        self.add_lambda_parameters_to_val_context(lambda_def, &expected_type, &mut val_context)?;

        let ert = if let Some(et) = expected_type {
            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters: _,
                return_type,
            }) = et
            {
                Ok(Some(return_type.deref()))
            } else if let ASTType::Generic(_name) = et {
                Ok(None)
            } else {
                Err(TypeCheckError::new(
                    lambda_def.index.clone(),
                    format!("Expected lambda but got {et}"),
                    self.stack.clone(),
                ))
            }
        } else {
            Ok(None)
        };

        new_lambda.body = self.transform_statements(
            module,
            &lambda_def.body,
            &mut val_context,
            statics,
            ert?,
            namespace,
            inside_function,
            new_functions,
        )?;

        Ok(new_lambda)
    }

    fn add_lambda_parameters_to_val_context(
        &self,
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
                    val_context
                        .insert_par(
                            name.to_owned(),
                            ASTParameterDef {
                                name: name.to_owned(),
                                ast_type: t.clone(),
                                ast_index: index.clone(),
                            },
                        )
                        .map_err(|e| {
                            TypeCheckError::new(index.clone(), e.clone(), self.stack.clone())
                        })?;
                }
            } else {
                return Err(TypeCheckError::new(
                    lambda_def.index.clone(),
                    format!("Expecting lambda but got {}", OptionDisplay(expected_type)),
                    self.stack.clone(),
                ));
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Write;
    use std::path::PathBuf;

    use env_logger::Builder;

    use crate::codegen::compile_target::CompileTarget;
    use crate::codegen::enhanced_module::EnhancedASTModule;
    use crate::codegen::statics::Statics;
    use crate::codegen::AsmOptions;
    use crate::commandline::CommandLineOptions;
    use crate::new_type_check2::TypeCheck;
    use crate::parser::ast::{
        ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModifiers, ASTNameSpace, ASTParameterDef,
        ASTType, BuiltinTypeKind,
    };
    use crate::project::RasmProject;
    use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
    use crate::type_check::type_check_error::TypeCheckError;
    use crate::type_check::typed_ast::convert_to_typed_module;
    use crate::utils::tests::test_namespace;

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
                namespace: test_namespace(),
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
                namespace: test_namespace(),
                param_types: vec![ASTType::Generic("".to_owned())],
                name: "".to_owned(),
                index: ASTIndex::none()
            },)
        );
    }

    #[test]
    fn test_generic_function_coeff() {
        // this is "more" generic
        let function1 = simple_function(vec![ASTType::Generic("T".to_string())], false);

        let function2 = simple_function(
            vec![ASTType::Custom {
                namespace: ASTNameSpace::global(),
                name: "".to_string(),
                param_types: vec![ASTType::Generic("T".to_string())],
                index: ASTIndex::none(),
            }],
            false,
        );

        let coeff1 = TypeCheck::function_precedence_coeff(&function1);
        let coeff2 = TypeCheck::function_precedence_coeff(&function2);

        assert!(coeff1 > coeff2)
    }

    #[test]
    fn test_generic_native_function_coeff() {
        // not native function have lower priority (higher coeff)
        let function1 = simple_function(vec![ASTType::Generic("T".to_string())], false);
        let function2 = simple_function(vec![ASTType::Generic("T".to_string())], true);

        let coeff1 = TypeCheck::function_precedence_coeff(&function1);
        let coeff2 = TypeCheck::function_precedence_coeff(&function2);

        assert!(coeff1 > coeff2)
    }
    fn simple_function(parameters: Vec<ASTType>, native: bool) -> ASTFunctionDef {
        ASTFunctionDef {
            original_name: "".to_string(),
            name: "".to_string(),
            parameters: parameters
                .iter()
                .map(|it| ASTParameterDef::new("", it.clone(), ASTIndex::none()))
                .collect(),
            return_type: ASTType::Unit,
            body: if native {
                ASTFunctionBody::NativeBody(String::new())
            } else {
                ASTFunctionBody::RASMBody(Vec::new())
            },
            inline: false,
            generic_types: vec![],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
            modifiers: ASTModifiers { public: false },
            namespace: ASTNameSpace::global(),
            rank: 0,
        }
    }

    /*
    TODO cannot work without a source file or folder
    #[test]
    pub fn test_add() {
        init();
        let mut check = TypeCheck::new();

        let mut module = EnhancedASTModule::new(
            vec![ASTModule::new(PathBuf::new())],
            PathBuf::from("resources"),
        );
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
            body: ASTFunctionBody::NativeBody("".to_owned()),
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

        assert_eq!(
            format!("{transformed_call}"),
            "add_0(add_0(1,2),add_0(1,2))"
        );

        assert_eq!(
            format!("{}", check.module.functions().get(0).unwrap()),
            "add_0(v1:i32,v2:i32) -> i32"
        );
    }
     */

    /*
    TODO cannot work without a source file or folder
    #[test]
    pub fn test_option_none() {
        init();

        let mut check = TypeCheck::new();

        let mut module = EnhancedASTModule::new(
            vec![ASTModule::new(PathBuf::new())],
            PathBuf::from("resources"),
        );
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
            body: ASTFunctionBody::NativeBody("".to_owned()),
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
            body: ASTFunctionBody::NativeBody("".to_owned()),
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
            body: ASTFunctionBody::NativeBody("".to_owned()),
            inline: false,
            generic_types: vec!["T".to_owned()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
        };
        module.add_function("Option::Some".to_owned(), function_def);

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

        assert_eq!(
            format!("{transformed_call}"),
            "orElse_0(Option_None_0(),{  -> Option_Some_0(10);\n; })"
        );
        assert_eq!(
            format!("{}", check.module.functions().get(0).unwrap()),
            "Option_Some_0(v1:i32) -> Option<i32>"
        );
    }

     */

    fn test_project(project: RasmProject) -> Result<(), TypeCheckError> {
        let target = CompileTarget::Nasmi386(AsmOptions::default());
        let mut statics = Statics::new();

        let (modules, _errors) = project.get_all_modules(
            &mut statics,
            false,
            &target,
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        );

        let (module, _) = EnhancedASTModule::new(modules, &project, &mut statics, &target, false);

        let mandatory_functions = target.get_mandatory_functions(&module);

        let default_functions = target.get_default_functions(false);
        //resolved_module.print();

        let _ = convert_to_typed_module(
            &module,
            false,
            mandatory_functions,
            &mut statics,
            default_functions,
            &target,
            false,
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

    fn dir_to_project(test_folder: &str) -> RasmProject {
        init();
        let file_name = PathBuf::from(test_folder);

        RasmProject::new(file_name)
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
