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

use crate::codegen::statics::Statics;
use crate::codegen::val_context::TypedValContext;
use crate::codegen::TypedValKind;
use crate::errors::CompilationError;
use crate::errors::CompilationErrorKind::Verify;
use crate::parser::ast::ASTIndex;
use crate::type_check::typed_ast;
use crate::type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef,
    ASTTypedModule, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};
use log::debug;
use std::ops::Deref;

pub fn verify(module: &ASTTypedModule, statics: &mut Statics) -> Result<(), CompilationError> {
    let mut context = TypedValContext::new(None);

    for statement in module.body.iter() {
        verify_statement(module, &mut context, statement, statics)?;
        if let ASTTypedStatement::Expression(e) = statement {
            let typed_type =
                typed_ast::get_type_of_typed_expression(module, &context, e, None, statics)?;

            if typed_type != ASTTypedType::Unit {
                return Err(expression_return_value_is_not_used(statement));
            }
        }
    }

    for function_def in module.functions_by_name.values() {
        let mut context = TypedValContext::new(None);

        for (i, par) in function_def.parameters.iter().enumerate() {
            context.insert_par(par.name.clone(), i, par.clone());
        }

        if let ASTTypedFunctionBody::RASMBody(expressions) = &function_def.body {
            verify_statements(module, statics, &function_def, &mut context, expressions)?;
        }
    }
    Ok(())
}

fn verify_statements(
    module: &ASTTypedModule,
    statics: &mut Statics,
    function_def: &ASTTypedFunctionDef,
    mut context: &mut TypedValContext,
    expressions: &Vec<ASTTypedStatement>,
) -> Result<(), CompilationError> {
    for (i, statement) in expressions.iter().enumerate() {
        verify_statement(module, &mut context, statement, statics)?;
        if i != expressions.len() - 1 {
            if let ASTTypedStatement::Expression(e) = statement {
                let typed_type =
                    typed_ast::get_type_of_typed_expression(module, &context, e, None, statics)?;

                if typed_type != ASTTypedType::Unit {
                    return Err(expression_return_value_is_not_used(statement));
                }
            }
        }
    }
    let real_return_type = if let Some(last) = expressions.iter().last() {
        match last {
            ASTTypedStatement::Expression(e) => typed_ast::get_type_of_typed_expression(
                module,
                &context,
                e,
                Some(&function_def.return_type),
                statics,
            )?,
            ASTTypedStatement::LetStatement(_, e, _is_const, _let_index) => {
                typed_ast::get_type_of_typed_expression(
                    module,
                    &context,
                    e,
                    Some(&ASTTypedType::Unit),
                    statics,
                )?
            }
        }
    } else {
        ASTTypedType::Unit
    };

    if function_def.return_type != real_return_type {
        return Err(verify_error(
            function_def.index.clone(),
            format!(
                "Expected return type {} but got {}",
                function_def.return_type, real_return_type
            ),
        ));
    }

    Ok(())
}

pub fn verify_error(index: ASTIndex, message: String) -> CompilationError {
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
    //expected_return_type: Option<ASTTypedType>,
) -> Result<(), CompilationError> {
    match statement {
        ASTTypedStatement::Expression(e) => verify_expression(module, context, e, statics),
        ASTTypedStatement::LetStatement(name, e, is_const, _let_index) => {
            verify_expression(module, context, e, statics)?;
            if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                let ast_typed_type =
                    if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
                        function_def.return_type.clone()
                    } else if let Some(function_def) = module
                        .functions_by_name
                        .get(&call.function_name.replace("::", "_"))
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
    //    expected_return_type: Option<ASTTypedType>
) -> Result<(), CompilationError> {
    match expr {
        ASTTypedExpression::StringLiteral(_) => Ok(()),
        ASTTypedExpression::ASTFunctionCallExpression(call) => {
            verify_function_call(module, context, call, statics)?;
            Ok(())
        }
        ASTTypedExpression::ValueRef(_, _) => Ok(()),
        ASTTypedExpression::Value(_, _) => Ok(()),
        ASTTypedExpression::Lambda(def) => {
            //TODO HENRY
            Ok(())
        }
    }
}

fn verify_function_call(
    module: &ASTTypedModule,
    context: &TypedValContext,
    call: &ASTTypedFunctionCall,
    statics: &mut Statics,
) -> Result<(), CompilationError> {
    debug!("verify_function_call {call}");

    let parameters_types =
        if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
            function_def
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect::<Vec<ASTTypedType>>()
        } else if let Some(function_def) = module
            .functions_by_name
            .get(&call.function_name.replace("::", "_"))
        {
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
                return Err(verify_error(
                    call.index.clone(),
                    format!("{} is not a lambda", call.function_name),
                ));
            }
        } else {
            return Err(verify_error(
                call.index.clone(),
                format!("cannot find function for call {call}"),
            ));
        };

    for (i, expr) in call.parameters.iter().enumerate() {
        let par_type = parameters_types.get(i).unwrap().clone();
        let typed_type = typed_ast::get_type_of_typed_expression(
            module,
            context,
            expr,
            Some(&par_type),
            statics,
        )?;

        debug!(
            "expected {par_type}, got {typed_type} in {call} : {} for parameter {i}",
            call.index
        );
        if typed_type != par_type {
            return Err(verify_error(call.index.clone(), format!("expected {par_type}, but got {typed_type} expression in call {} for parameter {i}", call.original_function_name)));
        }
    }

    Ok(())
}

fn expression_return_value_is_not_used(statement: &ASTTypedStatement) -> CompilationError {
    verify_error(
        statement.get_index().unwrap(),
        "Expression return value is not used".to_string(),
    )
}
