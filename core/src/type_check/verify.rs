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

use crate::codegen::enh_ast::EnhASTIndex;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::enh_val_context::TypedValContext;
use crate::codegen::TypedValKind;
use crate::errors::CompilationError;
use crate::errors::CompilationErrorKind::Verify;
use crate::type_check::typed_ast;
use crate::type_check::typed_ast::{
    get_type_of_typed_expression, ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionCall,
    ASTTypedModule, ASTTypedParameterDef, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};
use crate::utils::OptionDisplay;
use crate::{debug_i, dedent, indent};
use std::iter::zip;
use std::ops::Deref;

pub fn verify(module: &ASTTypedModule, statics: &mut Statics) -> Result<(), CompilationError> {
    // println!("printing typed module");
    // print_typed_module(module);

    let mut context = TypedValContext::new(None);

    verify_statements(
        module,
        statics,
        &mut context,
        &module.body,
        &ASTTypedType::Unit,
        EnhASTIndex::none(),
    )?;

    for function_def in module.functions_by_name.values() {
        let mut context = TypedValContext::new(None);

        for (i, par) in function_def.parameters.iter().enumerate() {
            context.insert_par(par.name.clone(), i, par.clone());
        }

        if let ASTTypedFunctionBody::RASMBody(expressions) = &function_def.body {
            verify_statements(
                module,
                statics,
                &mut context,
                expressions,
                &function_def.return_type,
                function_def.index.clone(),
            )?;
        }
    }
    Ok(())
}

fn verify_statements(
    module: &ASTTypedModule,
    statics: &mut Statics,
    context: &mut TypedValContext,
    expressions: &Vec<ASTTypedStatement>,
    expected_return_type: &ASTTypedType,
    initial_index: EnhASTIndex,
) -> Result<(), CompilationError> {
    let mut last_index = initial_index;
    for (i, statement) in expressions.iter().enumerate() {
        let ert = if i != expressions.len() - 1 {
            &ASTTypedType::Unit
        } else {
            expected_return_type
        };

        if let Some(li) = statement.get_index() {
            last_index = li;
        }

        /*
        match statement {
            ASTTypedStatement::Expression(_) => {}
            ASTTypedStatement::LetStatement(name, let_expr, is_const, index) => {
                if *is_const {
                    return Err(verify_error(
                        index.clone(),
                        "const not allowed here".to_string(),
                    ));
                }

                let type_of_expr =
                    get_type_of_typed_expression(module, context, let_expr, None, statics)?;

                context.insert_let(name.to_string(), type_of_expr, None);
            }
        }

         */

        verify_statement(module, context, statement, statics, Some(ert))?;
    }
    let real_return_type = if let Some(last) = expressions.iter().last() {
        match last {
            ASTTypedStatement::Expression(e) => get_type_of_typed_expression(
                module,
                context,
                e,
                Some(expected_return_type),
                statics,
            )?,
            ASTTypedStatement::LetStatement(_, _e, _is_const, _let_index) => ASTTypedType::Unit,
        }
    } else {
        ASTTypedType::Unit
    };

    if expected_return_type != &real_return_type {
        return Err(verify_error(
            last_index,
            format!(
                "Expected return type {} but got {}",
                expected_return_type, real_return_type
            ),
        ));
    }

    Ok(())
}

pub fn verify_error(index: EnhASTIndex, message: String) -> CompilationError {
    CompilationError {
        index,
        error_kind: Verify(message),
    }
}

pub fn verify_statement(
    module: &ASTTypedModule,
    context: &mut TypedValContext,
    statement: &ASTTypedStatement,
    statics: &mut Statics,
    expected_return_type: Option<&ASTTypedType>,
) -> Result<(), CompilationError> {
    match statement {
        ASTTypedStatement::Expression(e) => {
            verify_expression(module, context, e, statics, expected_return_type)
        }
        ASTTypedStatement::LetStatement(name, e, is_const, _let_index) => {
            verify_expression(module, context, e, statics, None)?;
            if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                let ast_typed_type = if let Some(function_def) =
                    module.functions_by_name.get(&call.function_name)
                {
                    function_def.return_type.clone()
                } else if let Some(function_def) = module.functions_by_name.get(&call.function_name)
                {
                    function_def.return_type.clone()
                } else if let Some(TypedValKind::ParameterRef(_, parameter_ref)) =
                    context.get(&call.function_name)
                {
                    if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                        parameters: _,
                        return_type,
                    }) = &parameter_ref.ast_type
                    {
                        return_type.deref().clone()
                    } else {
                        return Err(verify_error(
                            call.index.clone(),
                            format!("{} is not a lambda", call.function_name),
                        ));
                    }
                } else if let Some(TypedValKind::LetRef(_, ast_type)) =
                    context.get(&call.function_name)
                {
                    if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                        parameters: _,
                        return_type,
                    }) = &ast_type
                    {
                        return_type.deref().clone()
                    } else {
                        return Err(verify_error(
                            call.index.clone(),
                            format!("{} is not a lambda", call.function_name),
                        ));
                    }
                } else {
                    return Err(verify_error(
                        call.index.clone(),
                        format!("Cannot find call to {}", call.original_function_name),
                    ));
                };

                if *is_const {
                    statics.add_typed_const(name.to_owned(), ast_typed_type);
                } else {
                    context.insert_let(name.clone(), ast_typed_type, None);
                }
            } else {
                let ast_typed_type =
                    typed_ast::get_type_of_typed_expression(module, context, e, None, statics)?;
                if ast_typed_type != ASTTypedType::Unit {
                    if *is_const {
                        statics.add_typed_const(name.to_owned(), ast_typed_type);
                    } else {
                        context.insert_let(name.clone(), ast_typed_type, None);
                    }
                } else {
                    panic!("unsupported let")
                }
            }

            /*
            if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                verify_function_call(module, context, call, statics)?;
            }

             */
            Ok(())
        }
    }
}

fn verify_expression(
    module: &ASTTypedModule,
    context: &mut TypedValContext,
    expr: &ASTTypedExpression,
    statics: &mut Statics,
    expected_type: Option<&ASTTypedType>,
) -> Result<(), CompilationError> {
    debug_i!(
        "verify_expression {expr} expected_type {}",
        OptionDisplay(&expected_type)
    );
    match expr {
        ASTTypedExpression::Lambda(def) => {
            if let Some(et) = expected_type {
                if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                    parameters,
                    return_type,
                }) = et
                {
                    let mut lambda_context = TypedValContext::new(Some(context));

                    if def.parameter_names.len() != parameters.len() {
                        return Err(verify_error(
                            def.index.clone(),
                            format!(
                                "Expected {} params, but {}",
                                def.parameter_names.len(),
                                parameters.len()
                            ),
                        ));
                    }

                    for (i, ((param_name, param_index), param_type)) in
                        zip(def.parameter_names.iter(), parameters.iter()).enumerate()
                    {
                        lambda_context.insert(
                            param_name.clone(),
                            TypedValKind::ParameterRef(
                                i,
                                ASTTypedParameterDef {
                                    name: param_name.clone(),
                                    ast_type: param_type.clone(),
                                    ast_index: param_index.clone(),
                                },
                            ),
                        )
                    }

                    verify_statements(
                        module,
                        statics,
                        &mut lambda_context,
                        &def.body,
                        return_type.deref(),
                        def.index.clone(),
                    )
                } else {
                    Err(verify_error(
                        def.index.clone(),
                        format!("Expected lambda but got {}", OptionDisplay(&expected_type)),
                    ))
                }
            } else {
                Err(verify_error(
                    def.index.clone(),
                    "Expected type but got None".to_string(),
                ))
            }
        }
        _ => {
            if let ASTTypedExpression::ASTFunctionCallExpression(call) = expr {
                verify_function_call(module, context, call, statics)?;
            }
            if let Some(et) = expected_type {
                let real_type =
                    get_type_of_typed_expression(module, context, expr, expected_type, statics)?;

                if &real_type != et {
                    Err(verify_error(
                        expr.get_index().unwrap_or(EnhASTIndex::none()),
                        format!(
                            "Expected {} ({}) but got {} ({})",
                            et,
                            module.get_type_from_typed_type(et).unwrap(),
                            real_type,
                            module.get_type_from_typed_type(&real_type).unwrap(),
                        ),
                    ))
                } else {
                    Ok(())
                }
            } else {
                Ok(())
            }
        }
    }
}

fn verify_function_call(
    module: &ASTTypedModule,
    context: &mut TypedValContext,
    call: &ASTTypedFunctionCall,
    statics: &mut Statics,
) -> Result<(), CompilationError> {
    debug_i!("verify_function_call {call}");
    indent!();

    let parameters_types =
        if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
            function_def
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect::<Vec<ASTTypedType>>()
        } else if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
            function_def
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect::<Vec<ASTTypedType>>()
        } else if let Some(TypedValKind::ParameterRef(_, parameter_ref)) =
            context.get(&call.function_name)
        {
            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters,
                return_type: _,
            }) = &parameter_ref.ast_type
            {
                parameters.to_vec()
            } else {
                return Err(verify_error(
                    call.index.clone(),
                    format!("{} is not a lambda", call.function_name),
                ));
            }
        } else if let Some(TypedValKind::LetRef(_, ast_type)) = context.get(&call.function_name) {
            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters,
                return_type: _,
            }) = &ast_type
            {
                parameters.to_vec()
            } else {
                dedent!();
                return Err(verify_error(
                    call.index.clone(),
                    format!("{} is not a lambda", call.function_name),
                ));
            }
        } else {
            dedent!();
            return Err(verify_error(
                call.index.clone(),
                format!("cannot find function for call {call}"),
            ));
        };

    for (i, expr) in call.parameters.iter().enumerate() {
        let par_type = parameters_types.get(i).unwrap().clone();

        verify_expression(module, context, expr, statics, Some(&par_type))?;

        let typed_type = typed_ast::get_type_of_typed_expression(
            module,
            context,
            expr,
            Some(&par_type),
            statics,
        )?;

        debug_i!(
            "expected {par_type}, got {typed_type} in {call} : {} for parameter {i}",
            call.index
        );
        if typed_type != par_type {
            dedent!();
            return Err(verify_error(call.index.clone(), format!("expected {par_type}, but got {typed_type} expression in call {} for parameter {i}", call.original_function_name)));
        }
    }

    dedent!();
    Ok(())
}

fn expression_return_value_is_not_used(statement: &ASTTypedStatement) -> CompilationError {
    verify_error(
        statement.get_index().unwrap(),
        "Expression return value is not used".to_string(),
    )
}
