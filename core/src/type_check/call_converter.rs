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

use std::cell::RefCell;
use std::ops::Deref;
use std::panic;

use linked_hash_map::LinkedHashMap;
use log::debug;
use strum_macros::Display;

use crate::codegen::backend::Backend;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::val_context::ValContext;
use crate::codegen::ValKind;
use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
use crate::parser::ast::{
    ASTExpression, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTLambdaDef, ASTParameterDef,
    ASTStatement, ASTType, BuiltinTypeKind,
};
use crate::type_check::call_converter::ConvertCallResult::{
    Converted, NothingToConvert, SomethingConverted,
};
use crate::type_check::call_stack::CallStack;
use crate::type_check::functions_container::{FunctionsContainer, TypeFilter};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::type_check_error::TypeCheckError;
use crate::type_check::typed_context::TypeConversionContext;
use crate::utils::OptionDisplay;
use crate::utils::SliceDisplay;
use crate::{debug_i, dedent, indent, type_check};

#[derive(Display)]
pub enum ConvertCallResult {
    NothingToConvert,
    SomethingConverted,
    Converted(ASTFunctionCall),
}

pub struct CallConverter<'a> {
    module: &'a EnhancedASTModule,
    context: &'a ValContext,
    typed_context: &'a RefCell<TypeConversionContext>,
    backend: &'a dyn Backend,
    statics: &'a Statics,
}

impl<'a> CallConverter<'a> {
    pub fn new(
        module: &'a EnhancedASTModule,
        context: &'a ValContext,
        typed_context: &'a RefCell<TypeConversionContext>,
        backend: &'a dyn Backend,
        statics: &'a Statics,
    ) -> CallConverter<'a> {
        CallConverter {
            module,
            context,
            typed_context,
            backend,
            statics,
        }
    }

    pub fn convert_call(
        &self,
        call: &ASTFunctionCall,
        expected_return_type: Option<ASTType>,
        call_stack: &mut CallStack,
    ) -> Result<ConvertCallResult, TypeCheckError> {
        debug_i!(
            "converting call {}: {} expected return type: {:?}",
            call,
            call.index,
            expected_return_type
        );

        indent!();

        if self.context.is_lambda(&call.function_name) {
            debug_i!("found function in context parameters");
            dedent!();
            return Ok(NothingToConvert);
        }

        let (function_def, function_def_from_module) = get_called_function(
            self.module,
            self.context,
            call,
            self.typed_context,
            &expected_return_type,
            self.backend,
            None,
            false,
            self.statics,
            call_stack,
        )?
        .unwrap_or_else(|| {
            self.typed_context.borrow().debug_i("context");
            self.module.debug_i();
            panic!(
                "Cannot find function {}: {}",
                call.function_name, call.index
            )
        });

        let mut resolved_generic_types = function_def.resolved_generic_types.clone();

        let expected_return_type = if let Some(ret) = expected_return_type {
            if !type_check::is_generic_type(&ret)
                && type_check::is_generic_type(&function_def.return_type)
            {
                let generic_types_from_effective_type =
                    type_check::resolve_generic_types_from_effective_type(
                        &function_def.return_type,
                        &ret,
                    )?;

                resolved_generic_types.extend(generic_types_from_effective_type);

                Some(ret)
            } else {
                None
            }
        } else {
            None
        };

        let mut converted_expressions = Vec::new();
        let mut converted_parameters = Vec::new();

        let mut something_converted = false;
        let mut something_converted_in_loop = false;
        let call_parameters = &call.parameters;
        let function_parameters = &function_def.parameters;

        for (i, expr) in call_parameters.iter().enumerate() {
            let par = function_parameters.get(i).unwrap_or_else(|| {
                panic!(
                    "Cannot find parameter {i} when calling function {} effective function {} : {}",
                    &function_def.name, call.function_name, call.index
                )
            });

            debug_i!("converting parameter {par}");
            debug_i!("resolved generics {:?}", resolved_generic_types);

            match expr {
                ASTExpression::Lambda(lambda) => {
                    something_converted_in_loop = self.convert_lambda_expr(
                        &mut resolved_generic_types,
                        &mut converted_expressions,
                        &mut converted_parameters,
                        &par,
                        lambda,
                        call_stack,
                    )? || something_converted_in_loop
                }
                ASTExpression::StringLiteral(_) => {
                    debug_i!("calling update for StringLiteral");

                    something_converted_in_loop = update(
                        &ASTType::Builtin(BuiltinTypeKind::String),
                        expr.clone(),
                        par,
                        &mut resolved_generic_types,
                        &mut converted_parameters,
                        &mut converted_expressions,
                    )? || something_converted_in_loop;
                }
                ASTFunctionCallExpression(call) => {
                    something_converted_in_loop = self.convert_call_expr(
                        &mut resolved_generic_types,
                        &mut converted_expressions,
                        &mut converted_parameters,
                        expr,
                        &par,
                        &call,
                        call_stack,
                    )? || something_converted_in_loop
                }
                ASTExpression::ValueRef(v, _) => {
                    let result_type = match self.context.get(v) {
                        Some(ValKind::ParameterRef(_, referenced_parameter_def)) => {
                            // TODO the generic types are not the same of those in this function
                            //   so if there's some generic type, I cannot "resolve" the ref
                            if !type_check::is_generic_type(&referenced_parameter_def.ast_type) {
                                Some(referenced_parameter_def.ast_type.clone())
                            } else {
                                None
                            }
                        }
                        Some(ValKind::LetRef(_, ast_type, _)) => {
                            if !type_check::is_generic_type(ast_type) {
                                Some(ast_type.clone())
                            } else {
                                None
                            }
                        }
                        None => self
                            .statics
                            .get_const(v)
                            .map(|it| Some(it.ast_type.clone()))
                            .unwrap_or_else(|| {
                                panic!(
                                    "cannot find val {v}, actual context {:?}, consts: {:?}",
                                    self.context.names(),
                                    self.statics.const_names()
                                )
                            }),
                    };

                    if let Some(t) = &result_type {
                        debug_i!("calling update for Val {v}");
                        something_converted_in_loop = update(
                            t,
                            expr.clone(),
                            par,
                            &mut resolved_generic_types,
                            &mut converted_parameters,
                            &mut converted_expressions,
                        )? || something_converted_in_loop;
                    } else {
                        converted_parameters.push(par.clone());
                        converted_expressions.push(expr.clone());
                    }
                }
                ASTExpression::Value(val_type, _index) => {
                    let ast_type = val_type.to_type();
                    something_converted_in_loop = update(
                        &ast_type,
                        expr.clone(),
                        par,
                        &mut resolved_generic_types,
                        &mut converted_parameters,
                        &mut converted_expressions,
                    )? || something_converted_in_loop;
                }
                ASTExpression::Any(ast_type) => {
                    something_converted_in_loop = update(
                        ast_type,
                        expr.clone(),
                        par,
                        &mut resolved_generic_types,
                        &mut converted_parameters,
                        &mut converted_expressions,
                    )? || something_converted_in_loop;
                }
            }

            something_converted |= something_converted_in_loop;
        }

        if function_def.name != call.function_name {
            debug_i!("something converted: new function name");
            something_converted = true;
        }

        if function_def_from_module {
            debug_i!("something converted: function from module");
            something_converted = true;
        }

        let mut remaining_generic_types = Vec::new();

        let mut parameters = Vec::new();

        for par in converted_parameters {
            if type_check::is_generic_type(&par.ast_type) {
                if let Some(new_ref) =
                    type_check::substitute(&par.ast_type, &resolved_generic_types.clone())
                {
                    remaining_generic_types.append(&mut type_check::get_generic_types(&new_ref));
                    parameters.push(ASTParameterDef {
                        name: par.name.clone(),
                        ast_type: new_ref,
                        ast_index: par.ast_index,
                    });
                } else {
                    remaining_generic_types
                        .append(&mut type_check::get_generic_types(&par.ast_type));
                    parameters.push(par);
                }
            } else {
                parameters.push(par);
            }
        }

        let new_return_type = if let Some(er) = &expected_return_type {
            er.clone()
        } else {
            let t = if let Some(new_t) =
                type_check::substitute(&function_def.return_type, &resolved_generic_types)
            {
                debug_i!("converted return type {new_t}");
                something_converted = true;
                new_t
            } else {
                function_def.return_type.clone()
            };

            remaining_generic_types.append(&mut type_check::get_generic_types(&t));
            t
        };

        remaining_generic_types.sort();
        remaining_generic_types.dedup();

        if !remaining_generic_types.is_empty() {
            debug_i!(
                "remaining generic types for {} {:?}, {something_converted}",
                call.function_name,
                remaining_generic_types
            );
            dedent!();
            return if something_converted {
                Ok(SomethingConverted)
            } else {
                Ok(NothingToConvert)
            };
        }

        if !something_converted {
            debug_i!("nothing converted for {call}");

            if function_def.generic_types.is_empty() {
                debug_i!(
                    "TODO check for not parameterized function {}",
                    call.function_name
                );

                return if function_def_from_module {
                    if let Some(f) = self
                        .typed_context
                        .borrow_mut()
                        .try_add_new(&call.original_function_name, &function_def)
                    {
                        debug_i!("function added or different name {f}");
                        let mut function_call = call.clone();
                        function_call.function_name = f.name;
                        dedent!();
                        Ok(Converted(function_call))
                    } else {
                        debug_i!(
                            "function added, same function from call ({})",
                            function_def.name
                        );
                        dedent!();
                        Ok(NothingToConvert)
                    }
                } else {
                    debug_i!("function from context, function ({})", function_def.name);

                    // println!("function from context, function ({})", function_def.name);

                    dedent!();
                    if function_def.name != call.function_name {
                        let mut function_call = call.clone();
                        function_call.function_name = function_def.name;
                        Ok(Converted(function_call))
                    } else {
                        Ok(NothingToConvert)
                    }
                };
            }

            let parameters_to_convert = parameters
                .iter()
                .any(|par| type_check::is_generic_type(&par.ast_type));

            if parameters_to_convert {
                panic!();
            }

            dedent!();
            return Ok(NothingToConvert);
        }
        assert!(
            FunctionsContainer::almost_same_type(
                &new_return_type,
                &TypeFilter::Exact(function_def.return_type.clone()),
                &mut LinkedHashMap::new(),
                &ASTIndex::none()
            )?,
            "Expected return type {}, but got {new_return_type} : {} {}",
            function_def.return_type,
            call.index,
            function_def.index
        );

        let new_function_def = ASTFunctionDef {
            original_name: function_def.original_name.clone(),
            name: function_def.name.clone(),
            parameters,
            return_type: new_return_type.clone(),
            body: function_def.body.clone(),
            generic_types: remaining_generic_types,
            inline: function_def.inline,
            resolved_generic_types: resolved_generic_types.clone(),
            index: function_def.index.clone(),
        };

        /*
        if function_def.original_name == "Pair" {
            println!(
                "Expected return type {}, but got {new_return_type} : {} {}",
                function_def.return_type, call.index, function_def.index
            );
            println!("function_def {function_def}");
            if let Some(er) = &expected_return_type {
                println!("expected_return_type {er}");
            }
        }

         */

        let result = if let Some(f) = self
            .typed_context
            .borrow_mut()
            .try_add_new(&call.original_function_name, &new_function_def)
        {
            debug_i!("effective function {f}");

            let new_function_call = ASTFunctionCall {
                original_function_name: call.original_function_name.clone(),
                function_name: f.name,
                parameters: converted_expressions,
                index: call.index.clone(),
            };
            debug_i!("new_function_call {new_function_call}");
            Converted(new_function_call)
        } else if function_def.name != call.function_name {
            let new_function_call = ASTFunctionCall {
                original_function_name: call.original_function_name.clone(),
                function_name: function_def.name,
                parameters: converted_expressions,
                index: call.index.clone(),
            };
            debug_i!("new_function_call {new_function_call}");
            Converted(new_function_call)
        } else {
            SomethingConverted
        };

        dedent!();

        Ok(result)
    }

    fn convert_call_expr(
        &self,
        mut resolved_generic_types: &mut ResolvedGenericTypes,
        mut converted_expressions: &mut Vec<ASTExpression>,
        mut converted_parameters: &mut Vec<ASTParameterDef>,
        expr: &ASTExpression,
        par: &ASTParameterDef,
        call: &ASTFunctionCall,
        call_stack: &mut CallStack,
    ) -> Result<bool, TypeCheckError> {
        let mut something_converted = false;

        let par_type =
            if let Some(new_r_t) = type_check::substitute(&par.ast_type, &resolved_generic_types) {
                new_r_t
            } else {
                par.ast_type.clone()
            };
        let expected_return_type = if !type_check::is_generic_type(&par_type) {
            Some(par_type.clone())
        } else {
            None
        };

        let convert_call_result = self.convert_call(call, expected_return_type, call_stack)?;

        if let SomethingConverted = convert_call_result {
            debug_i!("something partially converted in call {call}");
            something_converted = true;
        }
        let typed_context_ptr = self.typed_context.borrow();
        if let Converted(ast_function_call) = convert_call_result {
            debug_i!("converted call {ast_function_call}");
            something_converted = true;
            //info!("new_function_defs {:?} used_untyped_function_defs {:?}", new_function_defs, used_untyped_function_defs);

            let inner_function_def = typed_context_ptr
                .find_function(&ast_function_call.function_name)
                .unwrap_or_else(|| {
                    panic!("Cannot find function {}", ast_function_call.function_name)
                });

            if !&inner_function_def.return_type.is_unit() {
                // the generic types of the inner function are not the same of the this function
                let result_type = if !type_check::is_generic_type(&inner_function_def.return_type) {
                    inner_function_def.return_type.clone()
                } else {
                    par.clone().ast_type
                };
                debug_i!("calling update for ASTFunctionCallExpression");
                debug_i!(
                    "expression {}",
                    ASTExpression::ASTFunctionCallExpression(ast_function_call.clone())
                );
                update(
                    &result_type,
                    ASTFunctionCallExpression(ast_function_call),
                    par,
                    &mut resolved_generic_types,
                    &mut converted_parameters,
                    &mut converted_expressions,
                )?;
            } else {
                debug_i!("calling update for ASTFunctionCallExpression");
                debug_i!(
                    "expression {}",
                    ASTExpression::ASTFunctionCallExpression(ast_function_call.clone())
                );

                /*
                if let Some(rt) = &inner_function_def.return_type {
                    update(
                        rt,
                        ASTFunctionCallExpression(ast_function_call),
                        par,
                        &mut resolved_generic_types,
                        &mut converted_parameters,
                        &mut converted_expressions,
                    )?;
                } else {

                 */
                converted_parameters.push(par.clone());
                converted_expressions.push(ASTFunctionCallExpression(ast_function_call));
                //}
            }
        } else if type_check::is_generic_type(&par.ast_type) {
            if let Some(inner_function_def) = typed_context_ptr.find_function(&call.function_name) {
                if !&inner_function_def.return_type.is_unit() {
                    // the generic types of the inner function are not the same of the this function
                    let result_type =
                        if !type_check::is_generic_type(&inner_function_def.return_type) {
                            inner_function_def.return_type.clone()
                        } else {
                            par.clone().ast_type
                        };
                    debug_i!("calling update for ASTFunctionCallExpression");
                    debug_i!(
                        "expression {}",
                        ASTExpression::ASTFunctionCallExpression(call.clone())
                    );
                    something_converted = update(
                        &result_type,
                        ASTFunctionCallExpression(call.clone()),
                        par,
                        &mut resolved_generic_types,
                        &mut converted_parameters,
                        &mut converted_expressions,
                    )? || something_converted;
                } else {
                    panic!("A Void result is not supported");
                }
            } else if self.context.is_lambda(&call.function_name) {
                if let Ok(Some(ast_type)) = type_check::get_type_of_expression(
                    self.module,
                    self.context,
                    expr,
                    self.typed_context,
                    None,
                    self.backend,
                    self.statics,
                    call_stack,
                ) {
                    something_converted = update(
                        &ast_type,
                        ASTFunctionCallExpression(call.clone()),
                        par,
                        &mut resolved_generic_types,
                        &mut converted_parameters,
                        &mut converted_expressions,
                    )? || something_converted;
                } else {
                    converted_parameters.push(par.clone());
                    converted_expressions.push(expr.clone());
                }
            } else {
                converted_parameters.push(par.clone());
                converted_expressions.push(expr.clone());
            }
        } else {
            converted_parameters.push(par.clone());
            converted_expressions.push(expr.clone());
        }

        Ok(something_converted)
    }

    fn convert_lambda_expr(
        &self,
        resolved_generic_types: &mut ResolvedGenericTypes,
        converted_expressions: &mut Vec<ASTExpression>,
        converted_parameters: &mut Vec<ASTParameterDef>,
        par: &ASTParameterDef,
        lambda: &ASTLambdaDef,
        call_stack: &mut CallStack,
    ) -> Result<bool, TypeCheckError> {
        let mut something_converted = false;

        let lambda_type =
            if let Some(new_type) = type_check::substitute(&par.ast_type, resolved_generic_types) {
                new_type
            } else {
                par.ast_type.clone()
            };
        let mut context = type_check::get_context_from_lambda(
            self.context,
            lambda,
            &lambda_type,
            resolved_generic_types,
        )?;

        let effective_lambda = if let Some(new_lambda) = type_check::convert_lambda(
            self.module,
            &par.ast_type,
            lambda,
            &context,
            self.typed_context,
            resolved_generic_types,
            self.backend,
            self.statics,
            call_stack,
        )? {
            debug_i!("lambda something converted");
            something_converted = true;
            new_lambda
        } else {
            lambda.clone()
        };

        let new_return_type = match &par.ast_type {
            ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters: _lambda_parameters,
                return_type, // TODO I cannot convert the return type at this stage
            }) => {
                if !return_type.is_unit() {
                    if let Some(new_t) =
                        type_check::substitute(&return_type, &resolved_generic_types)
                    {
                        debug_i!("lambda something converted in return type new {new_t}");
                        something_converted = true;
                        new_t
                    } else if let Some(last) = effective_lambda.body.last() {
                        for statement in effective_lambda.body.iter() {
                            if let ASTStatement::LetStatement(name, expr, is_const, let_index) =
                                statement
                            {
                                let ast_type = type_check::get_type_of_expression(
                                    self.module,
                                    &context,
                                    expr,
                                    self.typed_context,
                                    None,
                                    self.backend,
                                    self.statics,
                                    call_stack,
                                )?
                                .unwrap();

                                if *is_const {
                                    panic!("const not allowed here")
                                } else {
                                    context.insert_let(name.clone(), ast_type, let_index);
                                }
                            }
                        }

                        if let Some(result_type) = type_check::get_type_of_statement(
                            self.module,
                            &context,
                            last,
                            self.typed_context,
                            self.backend,
                            self.statics,
                            call_stack,
                        )? {
                            if !type_check::is_generic_type(&result_type) {
                                result_type
                            } else {
                                return_type.deref().clone()
                            }
                        } else {
                            return_type.deref().clone()
                        }
                    } else {
                        return_type.deref().clone()
                    }
                } else {
                    ASTType::Unit
                }
            }
            _ => {
                panic!()
            }
        };

        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters,
            return_type: _,
        }) = &par.ast_type
        {
            let new_parameters: Vec<ASTType> = parameters
                .iter()
                .map(
                    |it| match type_check::substitute(it, resolved_generic_types) {
                        None => it.clone(),
                        Some(p) => {
                            debug_i!("lambda something converted in parameter {p}");
                            something_converted = true;
                            p
                        }
                    },
                )
                .collect();
            something_converted = update(
                &ASTType::Builtin(BuiltinTypeKind::Lambda {
                    return_type: Box::new(new_return_type),
                    parameters: new_parameters,
                }),
                ASTExpression::Lambda(effective_lambda),
                par,
                resolved_generic_types,
                converted_parameters,
                converted_expressions,
            )? || something_converted;
        } else {
            dedent!();
            return Err(format!("Expected Lambda but found {}", &par.ast_type).into());
        }

        Ok(something_converted)
    }
}

fn update(
    result_type: &ASTType,
    expr: ASTExpression,
    par: &ASTParameterDef,
    resolved_param_types: &mut ResolvedGenericTypes,
    parameters: &mut Vec<ASTParameterDef>,
    expressions: &mut Vec<ASTExpression>,
) -> Result<bool, TypeCheckError> {
    if !type_check::is_generic_type(&par.ast_type) {
        expressions.push(expr);
        parameters.push(par.clone());
        return Ok(false);
    }

    let generic_types_from_effective_type =
        type_check::resolve_generic_types_from_effective_type(&par.ast_type, result_type)
            .map_err(|e| format!("{} in update par {par} result_type {result_type}", e))?;

    let len_before = resolved_param_types.len();

    if let Some(err) = resolved_param_types.check(&generic_types_from_effective_type) {
        return Err(err.into());
    }

    resolved_param_types.extend(generic_types_from_effective_type.clone());

    let some_generic_tye_added = len_before != resolved_param_types.len();

    let mut message = String::new();

    if some_generic_tye_added {
        message += &format!(
            "added resolved types {:?}",
            generic_types_from_effective_type
        );
    }

    let result = if let Some(t) = type_check::substitute(&par.ast_type, resolved_param_types) {
        expressions.push(expr);

        message += &format!(" converted type {}", t);

        parameters.push(ASTParameterDef {
            name: par.name.clone(),
            ast_type: t,
            ast_index: par.ast_index.clone(),
        });
        true
    } else {
        expressions.push(expr);
        parameters.push(par.clone());
        false
    } || some_generic_tye_added;

    if result {
        debug_i!(
            "something updated: result_type {result_type} par.type {}: {message}",
            par.ast_type
        );
    }

    Ok(result)
}

pub fn get_type_of_call(
    module: &EnhancedASTModule,
    context: &ValContext,
    call: &ASTFunctionCall,
    typed_context: &RefCell<TypeConversionContext>,
    backend: &dyn Backend,
    statics: &Statics,
    call_stack: &mut CallStack,
) -> Result<Option<ASTType>, TypeCheckError> {
    if let Some(ValKind::ParameterRef(_i, par)) = context.get(&call.function_name) {
        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
            return_type,
            parameters: _,
        }) = &par.ast_type
        {
            return Ok(Some(return_type.deref().clone()));
        }
    }

    if let Some(ValKind::LetRef(
        _i,
        ASTType::Builtin(BuiltinTypeKind::Lambda {
            return_type,
            parameters: _,
        }),
        _,
    )) = context.get(&call.function_name)
    {
        return Ok(Some(return_type.deref().clone()));
    }

    if let Some((function, _)) = get_called_function(
        module,
        context,
        call,
        typed_context,
        &None,
        backend,
        None,
        false,
        statics,
        call_stack,
    )? {
        if !function.return_type.is_unit() {
            if !type_check::is_generic_type(&function.return_type) {
                Ok(Some(function.return_type))
            } else {
                let convert_call_result =
                    CallConverter::new(module, context, typed_context, backend, statics)
                        .convert_call(call, None, call_stack)?;

                if let Converted(new_call) = convert_call_result {
                    if let Some((function, _)) = get_called_function(
                        module,
                        context,
                        &new_call,
                        typed_context,
                        &None,
                        backend,
                        None,
                        false,
                        statics,
                        call_stack,
                    )? {
                        Ok(Some(function.return_type))
                    } else {
                        Ok(Some(function.return_type))
                    }
                } else {
                    Ok(Some(function.return_type))
                }
            }
        } else {
            Ok(Some(ASTType::Unit))
        }
    } else {
        Ok(None)
    }
}

fn get_called_function(
    module: &EnhancedASTModule,
    context: &ValContext,
    call: &ASTFunctionCall,
    typed_context: &RefCell<TypeConversionContext>,
    expected_return_type: &Option<ASTType>,
    backend: &dyn Backend,
    verify_function: Option<&ASTFunctionDef>,
    only_from_module: bool,
    statics: &Statics,
    call_stack: &mut CallStack,
) -> Result<Option<(ASTFunctionDef, bool)>, TypeCheckError> {
    if let Some(f) = verify_function {
        if f.parameters.len() != call.parameters.len() {
            return Err("function parameters are not the same of the call".into());
        }
        debug_i!(
            "verifying function {f} for {call} return type {}",
            OptionDisplay(expected_return_type)
        );
    } else {
        debug_i!(
            "trying to find function for {call} return type {}",
            OptionDisplay(expected_return_type)
        );
    }
    indent!();
    let mut function_def_from_module = false;

    if let Some(function_def) = module.find_function_by_original_name(&call.original_function_name)
    {
        if function_def.generic_types.is_empty() {
            if let Some(function_in_context) = typed_context
                .borrow()
                .find_function_by_original_name(&call.original_function_name)
            {
                dedent!();
                return Ok(Some((function_in_context.clone(), false)));
            } else {
                dedent!();
                return Ok(Some((function_def.clone().clone(), true)));
            }
        }
    }

    call_stack.push(format!("{} {}", call, call.index));

    let call_parameters_types = call
        .parameters
        .iter()
        .enumerate()
        .map(|(index, it)| {
            let lambda = if let Some(f) = verify_function {
                let par = f.parameters.get(index).unwrap();
                if let ASTType::Builtin(BuiltinTypeKind::Lambda { .. }) = par.ast_type {
                    Some(par.ast_type.clone())
                } else {
                    None
                }
            } else {
                None
            };
            match type_check::get_type_of_expression(
                module,
                context,
                it,
                typed_context,
                lambda.as_ref(),
                backend,
                statics,
                call_stack,
            )? {
                Some(ast_type) => Ok(TypeFilter::Exact(ast_type)),
                None => {
                    if let ASTExpression::Lambda(def) = it {
                        Ok(TypeFilter::Lambda(def.parameter_names.len(), None))
                    } else {
                        Ok(TypeFilter::Any)
                    }
                } /* HENRY
                  Some(ast_type) => {
                      if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                          parameters,
                          return_type,
                      }) = &ast_type
                      {
                          if parameters.iter().any(is_generic_type)
                              || is_generic_type(return_type.deref())
                          {
                              Ok(TypeFilter::Lambda(parameters.len()))
                          } else {
                              Ok(TypeFilter::Exact(ast_type))
                          }
                      } else if is_generic_type(&ast_type) {
                          Ok(TypeFilter::Any)
                      } else {
                          Ok(TypeFilter::Exact(ast_type))
                      }
                  }
                  None => {
                      if let ASTExpression::Lambda(def) = it {
                          Ok(TypeFilter::Lambda(def.parameter_names.len()))
                      } else {
                          Ok(TypeFilter::Any)
                      }
                  }

                   */
            }
        })
        .collect::<Result<Vec<_>, TypeCheckError>>()?;

    let mut candidate_functions = if only_from_module {
        Vec::new()
    } else {
        typed_context.borrow().find_call_vec(
            call,
            &call_parameters_types,
            expected_return_type.clone(),
            true,
        )?
    };

    if candidate_functions.is_empty() {
        debug_i!("No function from context");
        function_def_from_module = true;
        candidate_functions =
            module.find_call_vec(call, &call_parameters_types, expected_return_type.clone())?;
    } else {
        debug_i!("candidate_functions {}", SliceDisplay(&candidate_functions));
    }

    if candidate_functions.is_empty() {
        typed_context.borrow().debug_i("context");
        module.debug_i();
        call_stack.pop();
        dedent!();
        return Err(format!(
            "Cannot find function for {call} with filters {} with return type {} in: \n{}\n{}",
            SliceDisplay(&call_parameters_types),
            OptionDisplay(expected_return_type),
            call.index,
            call_stack
                .stack()
                .iter()
                .rev()
                .map(|it| it.as_str())
                .collect::<Vec<_>>()
                .join("\n")
        )
        .into());
    }

    let function_def = if candidate_functions.len() == 1 {
        /*
        println!(
            "call {call} expected {} from_module {function_def_from_module}",
            OptionOptionDisplay(expected_return_type)
        );

         */
        candidate_functions.get(0).cloned()
    } else if call_parameters_types
        .iter()
        .any(|it| matches!(it, TypeFilter::Any) || matches!(it, TypeFilter::Lambda(_, _)))
    {
        let mut found_function_def = None;
        for f_def in candidate_functions {
            debug_i!(
                "Trying to find the right function {f_def} filter {}",
                SliceDisplay(&call_parameters_types)
            );
            let mut skip_function = false;
            let new_call_parameters_types = call_parameters_types
                .iter()
                .enumerate()
                .map(|(i, filter)| {
                    debug_i!("filter {i} {}", filter);
                    match filter {
                        TypeFilter::Any | TypeFilter::Lambda(_, _) => {
                            let par = f_def.parameters.get(i).unwrap();
                            let expr = call.parameters.get(i).unwrap();
                            debug_i!("found None filter for {par}");

                            match &par.ast_type {
                                ASTType::Builtin(BuiltinTypeKind::Lambda {
                                    parameters: _,
                                    return_type: _,
                                }) => match expr {
                                    ASTExpression::Lambda(lambda_def) => {
                                        if let Some(la) = type_check::get_type_of_expression(
                                            module,
                                            context,
                                            expr,
                                            typed_context,
                                            Some(&par.ast_type),
                                            backend,
                                            statics,
                                            call_stack,
                                        )? {
                                            Ok(TypeFilter::Exact(la))
                                        } else {
                                            Ok(TypeFilter::Lambda(
                                                lambda_def.parameter_names.len(),
                                                None,
                                            ))
                                        }
                                    }
                                    _ => {
                                        skip_function = true;
                                        Ok(TypeFilter::Any)
                                    }
                                },
                                _ => {
                                    if let Some(la) = type_check::get_type_of_expression(
                                        module,
                                        context,
                                        expr,
                                        typed_context,
                                        Some(&par.ast_type),
                                        backend,
                                        statics,
                                        call_stack,
                                    )? {
                                        Ok(TypeFilter::Exact(la))
                                    } else {
                                        Ok(TypeFilter::NotALambda)
                                    }
                                }
                            }
                        }
                        TypeFilter::Exact(f) => Ok(TypeFilter::Exact(f.clone())),
                        TypeFilter::NotALambda => {
                            let par = f_def.parameters.get(i).unwrap();
                            if matches!(
                                par.ast_type,
                                ASTType::Builtin(BuiltinTypeKind::Lambda {
                                    parameters: _,
                                    return_type: _,
                                })
                            ) {
                                skip_function = true;
                                Ok(TypeFilter::Any)
                            } else {
                                Ok(TypeFilter::NotALambda)
                            }
                        }
                    }
                })
                .collect::<Result<Vec<_>, TypeCheckError>>()?;

            if skip_function {
                debug_i!("skipping function {f_def}");
                continue;
            }

            debug_i!(
                "new call parameters types {}",
                SliceDisplay(&new_call_parameters_types)
            );
            let mut new_function_def_opt = typed_context.borrow().find_call_vec(
                call,
                &new_call_parameters_types,
                expected_return_type.clone(),
                true,
            )?;

            if new_function_def_opt.is_empty() {
                function_def_from_module = true;
                new_function_def_opt = module.find_call_vec(
                    call,
                    &new_call_parameters_types,
                    expected_return_type.clone(),
                )?;
            }

            if new_function_def_opt.len() == 1 {
                if found_function_def.is_none() {
                    let real_function = new_function_def_opt.get(0).cloned().unwrap();
                    debug_i!("real function {real_function}");
                    found_function_def = Some(real_function);
                    break;
                } else {
                    debug_i!("found more than one function");
                }
            }
        }

        found_function_def
        // .unwrap_or_else(|| panic!("Cannot find function for {call}: {}", call.index))
    } else if !function_def_from_module && candidate_functions.len() > 1 {
        debug_i!(
            "function_def_opt {:?}",
            candidate_functions
                .iter()
                .map(|it| format!("{it}"))
                .collect::<Vec<_>>()
        );

        if let Some(ex) = expected_return_type {
            debug_i!("expected_return_type {:?}", ex);
            let mut resolved_generic_types = LinkedHashMap::new();
            candidate_functions = candidate_functions
                .into_iter()
                .map(|it| {
                    FunctionsContainer::almost_same_return_type(
                        &it.return_type,
                        ex,
                        &mut resolved_generic_types,
                        &call.index,
                    )
                    .map(|v| (it, v))
                })
                .filter(|it| it.as_ref().map(|r| r.1).unwrap_or(true))
                .collect::<Result<Vec<_>, TypeCheckError>>()?
                .into_iter()
                .map(|(f, valid)| f)
                .collect::<Vec<_>>();
        }
        if candidate_functions.len() > 1 {
            candidate_functions = candidate_functions
                .into_iter()
                .filter(|it| it.name == call.function_name)
                .collect::<Vec<_>>();
        }
        if candidate_functions.len() == 1 {
            candidate_functions.first().cloned()
        } else {
            debug_i!(
                "cannot find function, function_def_opt {:?}",
                candidate_functions
                    .iter()
                    .map(|it| format!("{it}"))
                    .collect::<Vec<_>>()
            );
            None
        }
    } else {
        // we want to find the unique function that has the max number of non generic parameters
        // TODO I don't really like the code, refactor it
        let max_not_generic = candidate_functions
            .iter()
            .map(|it| {
                it.parameters
                    .iter()
                    .filter(|p| !matches!(p.ast_type, ASTType::Generic(_)))
                    .count()
            })
            .max();

        let result = if let Some(max) = max_not_generic {
            if max > 0 {
                let candidate_functions = candidate_functions
                    .iter()
                    .filter(|it| {
                        it.parameters
                            .iter()
                            .filter(|p| !matches!(p.ast_type, ASTType::Generic(_)))
                            .count()
                            == max
                    })
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
        };

        if result.is_none() {
            panic!(
                "Cannot find one single function for {call} but {:?}: {}",
                candidate_functions
                    .iter()
                    .map(|it| format!("{it}"))
                    .collect::<Vec<_>>(),
                call.index
            );
        }

        result
    };

    if let Some(fd) = function_def {
        debug_i!("found function def {fd}");

        //println!("found function def {fd}");
        if fd.original_name == "print"
            && call_parameters_types.get(1).map_or(false, |it| match it {
                TypeFilter::Exact(t) => match t {
                    ASTType::Custom {
                        name,
                        param_types,
                        index,
                    } => name == "File",
                    _ => false,
                },
                TypeFilter::Any => false,
                TypeFilter::Lambda(_, _) => false,
                TypeFilter::NotALambda => false,
            })
        {
            println!("found function def {fd}");
        }

        // HENRY "&& !only_from_module && !function_def_from_module" was commented
        if verify_function.is_none() && !only_from_module && !function_def_from_module {
            if let Some((f, new_function_def_from_module)) = get_called_function(
                module,
                context,
                call,
                typed_context,
                expected_return_type,
                backend,
                Some(&fd),
                false,
                statics,
                call_stack,
            )? {
                if f != fd {
                    // TODO it's really a guess...
                    call_stack.pop();
                    return Ok(Some((f, new_function_def_from_module)));
                    //return Err(format!("different function {f} {fd}").into());
                }
            } else {
                // panic!("invalid function");
                let result1 = get_called_function(
                    module,
                    context,
                    call,
                    typed_context,
                    expected_return_type,
                    backend,
                    None,
                    true,
                    statics,
                    call_stack,
                );
                call_stack.pop();
                return result1;
            }
        }
        call_stack.pop();
        dedent!();
        Ok(Some((fd.clone(), function_def_from_module)))
    } else {
        debug_i!("cannot find function");
        call_stack.pop();
        dedent!();
        Ok(None)
    }

    // Ok((function_def.clone(), function_def_from_module))
}
