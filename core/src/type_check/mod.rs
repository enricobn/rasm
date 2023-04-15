use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::panic;

use backtrace::Backtrace;
use linked_hash_map::LinkedHashMap;
use log::{debug, log_enabled, Level};
use strum_macros::Display;

use crate::codegen::backend::Backend;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{MacroParam, TextMacro};
use crate::codegen::{ValContext, ValKind};
use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
use crate::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTParameterDef,
    ASTType, BuiltinTypeKind,
};
use crate::parser::ast::{ASTStatement, MyToString};
use crate::parser::ValueType;
use crate::type_check::call_stack::CallStack;
use crate::type_check::functions_container::FunctionsContainer;
use crate::type_check::functions_container::TypeFilter;
use crate::type_check::typed_context::TypeConversionContext;
use crate::type_check::ConvertCallResult::{Converted, NothingToConvert, SomethingConverted};
use crate::utils::OptionOptionDisplay;
use crate::utils::SliceDisplay;
use crate::{debug_i, dedent, indent};

pub mod call_stack;
pub mod functions_container;
pub mod typed_ast;
pub mod typed_context;

#[derive(Debug)]
pub struct TypeCheckError {
    pub message: String,
}

impl Display for TypeCheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let bt = Backtrace::new();
        println!("{:?}", bt);
        f.write_str(&format!("TypeCheckError({})", &self.message))
    }
}

impl From<&str> for TypeCheckError {
    fn from(s: &str) -> Self {
        TypeCheckError { message: s.into() }
    }
}

impl From<String> for TypeCheckError {
    fn from(s: String) -> Self {
        TypeCheckError { message: s }
    }
}

fn convert_function_def(
    backend: &dyn Backend,
    module: &EnhancedASTModule,
    type_conversion_context: &RefCell<TypeConversionContext>,
    resolved_generic_types: &LinkedHashMap<String, ASTType>,
    function_def: &ASTFunctionDef,
    statics: &Statics,
) -> Result<Option<ASTFunctionDef>, TypeCheckError> {
    let mut context = ValContext::new(None);

    for par in function_def.parameters.iter() {
        //debug_i!("inserting par {} in context", par.name);
        context.insert_par(par.name.clone(), par.clone());
    }

    match &function_def.body {
        ASTFunctionBody::RASMBody(body) => {
            let call_stack = CallStack::new();

            let return_type = function_def.return_type.clone();

            if let Some(new_body) = convert_body(
                backend,
                module,
                type_conversion_context,
                &mut context,
                body,
                &call_stack,
                return_type,
                statics,
                resolved_generic_types,
            )? {
                let mut new_function_def = function_def.clone();
                new_function_def.body = ASTFunctionBody::RASMBody(new_body);

                new_function_def.resolved_generic_types = resolved_generic_types.clone();

                type_conversion_context
                    .borrow_mut()
                    .replace_body(&new_function_def);

                Ok(Some(new_function_def))
            } else {
                Ok(None)
            }
        }
        ASTFunctionBody::ASMBody(_) => Ok(None),
    }
}

fn convert_body(
    backend: &dyn Backend,
    module: &EnhancedASTModule,
    type_conversion_context: &RefCell<TypeConversionContext>,
    context: &ValContext,
    body: &Vec<ASTStatement>,
    call_stack: &CallStack,
    return_type: Option<ASTType>,
    statics: &Statics,
    resolved_generic_types: &LinkedHashMap<String, ASTType>,
) -> Result<Option<Vec<ASTStatement>>, TypeCheckError> {
    debug_i!("converting body return type {:?}", return_type);
    indent!();
    let mut context = context.clone();
    let mut something_to_convert = false;
    let new_body = body
        .iter()
        .enumerate()
        .map(|(index, it)| {
            debug_i!("convertin statement {it} {:?}", return_type);

            let converted_statement = if index == body.len() - 1 && return_type.is_some() {
                convert_last_statement_in_body(
                    module,
                    it,
                    &context,
                    type_conversion_context,
                    return_type.clone(),
                    backend,
                    call_stack,
                    statics,
                    resolved_generic_types,
                )
            } else {
                convert_statement_in_body(
                    module,
                    it,
                    &mut context,
                    type_conversion_context,
                    backend,
                    call_stack,
                    statics,
                )
            };

            let new_statement = match converted_statement? {
                Some(new_expr) => {
                    debug_i!("converted expr {}", new_expr);
                    something_to_convert = true;
                    new_expr
                }
                None => {
                    debug_i!("nothing converted");
                    it.clone()
                }
            };
            Ok(new_statement)
        })
        .collect::<Result<Vec<ASTStatement>, TypeCheckError>>()?;

    dedent!();
    if something_to_convert {
        Ok(Some(new_body))
    } else {
        Ok(None)
    }
}

pub fn get_new_native_call(m: &TextMacro, to_function: &str) -> String {
    let p = m
        .parameters
        .iter()
        .enumerate()
        .filter(|(i, p)| *i > 0)
        .map(|(_, it)| match it {
            MacroParam::Plain(value, ast_type, _) => match ast_type {
                None => value.to_string(),
                // TODO duplicated code
                Some(t) => {
                    if matches!(t, ASTType::Builtin(BuiltinTypeKind::Lambda { .. })) {
                        value.to_string()
                    } else {
                        format!("{it}")
                    }
                }
            },
            MacroParam::StringLiteral(_) => format!("{it}"),
            MacroParam::Ref(value, ast_type, _) => match ast_type {
                None => value.to_string(),
                // TODO duplicated code
                Some(t) => {
                    if matches!(t, ASTType::Builtin(BuiltinTypeKind::Lambda { .. })) {
                        value.to_string()
                    } else {
                        format!("{it}")
                    }
                }
            },
        })
        .collect::<Vec<_>>()
        .join(",");

    if p.is_empty() {
        format!("$call({to_function})")
    } else {
        format!("$call({to_function},{p})")
    }
}

fn convert_statement(
    module: &EnhancedASTModule,
    context: &mut ValContext,
    type_conversion_context: &RefCell<TypeConversionContext>,
    new_body: &mut Vec<ASTStatement>,
    statement: &ASTStatement,
    backend: &dyn Backend,
    call_stack: &CallStack,
    statics: &mut Statics,
) -> bool {
    let mut something_to_convert = false;

    match statement {
        ASTStatement::Expression(expr) => {
            if let ASTFunctionCallExpression(call @ ASTFunctionCall { .. }) = expr {
                let converted_call = convert_call(
                    module,
                    context,
                    call,
                    type_conversion_context,
                    None,
                    backend,
                    call_stack,
                    statics,
                );

                match converted_call {
                    Err(e) => {
                        panic!("{e} expression: {}", expr);
                    }
                    Ok(NothingToConvert) => new_body.push(statement.clone()),
                    Ok(SomethingConverted) => {
                        new_body.push(statement.clone());
                        something_to_convert = true;
                    }
                    Ok(Converted(new_call)) => {
                        something_to_convert = true;
                        debug_i!(
                            "converted call {} in {}",
                            call.function_name,
                            new_call.function_name
                        );
                        new_body.push(ASTStatement::Expression(ASTFunctionCallExpression(
                            new_call,
                        )));
                    }
                }
            } else {
                panic!("unsupported {statement}");
            }
        }
        ASTStatement::LetStatement(name, expr, is_const, let_index) => {
            if let ASTFunctionCallExpression(call @ ASTFunctionCall { .. }) = expr {
                let converted_call = convert_call(
                    module,
                    context,
                    call,
                    type_conversion_context,
                    None,
                    backend,
                    call_stack,
                    statics,
                );

                match converted_call {
                    Err(e) => {
                        panic!("{e}");
                    }
                    Ok(NothingToConvert) => {
                        let ast_type = type_conversion_context
                            .borrow()
                            .find_function(&call.function_name)
                            .unwrap_or_else(|| panic!("{} : {}", &call.function_name, &call.index))
                            .return_type
                            .clone()
                            .unwrap();

                        if *is_const {
                            statics.add_const(name.clone(), ast_type);
                        } else {
                            context.insert_let(name.clone(), ast_type, let_index);
                        }
                        new_body.push(statement.clone())
                    }
                    Ok(SomethingConverted) => {
                        something_to_convert = true;
                        let ast_type = type_conversion_context
                            .borrow()
                            .find_function(&call.function_name)
                            .unwrap_or_else(|| panic!("{} : {}", &call.function_name, &call.index))
                            .return_type
                            .clone()
                            .unwrap();

                        if *is_const {
                            statics.add_const(name.to_owned(), ast_type);
                        } else {
                            context.insert_let(name.clone(), ast_type, let_index);
                        }
                        new_body.push(statement.clone())
                    }
                    Ok(Converted(new_call)) => {
                        something_to_convert = true;
                        debug_i!(
                            "converted call {} in {}",
                            call.function_name,
                            new_call.function_name
                        );

                        let ast_type = type_conversion_context
                            .borrow()
                            .find_function(&new_call.function_name)
                            .unwrap()
                            .return_type
                            .clone()
                            .unwrap();

                        if *is_const {
                            statics.add_const(name.to_owned(), ast_type);
                        } else {
                            context.insert_let(name.clone(), ast_type, let_index);
                            // TODO index is not really it
                        }

                        new_body.push(ASTStatement::LetStatement(
                            name.clone(),
                            ASTFunctionCallExpression(new_call),
                            *is_const,
                            let_index.clone(),
                        ))
                    }
                }
            } else if let ASTExpression::Value(value_type, index) = expr {
                if *is_const {
                    statics.add_const(name.to_owned(), get_value_type(value_type));
                } else {
                    context.insert_let(name.clone(), get_value_type(value_type), index);
                }
                new_body.push(ASTStatement::LetStatement(
                    name.clone(),
                    ASTExpression::Value(value_type.clone(), index.clone()),
                    *is_const,
                    let_index.clone(),
                ))
            } else if let ASTExpression::StringLiteral(value) = expr {
                if *is_const {
                    statics.add_const(name.to_owned(), ASTType::Builtin(BuiltinTypeKind::String));
                } else {
                    context.insert_let(
                        name.clone(),
                        ASTType::Builtin(BuiltinTypeKind::String),
                        let_index,
                    );
                }
                new_body.push(ASTStatement::LetStatement(
                    name.clone(),
                    ASTExpression::StringLiteral(value.clone()),
                    *is_const,
                    let_index.clone(),
                ))
            } else {
                panic!("unsupported {statement}")
            }
        }
    }

    something_to_convert
}

/*
fn unknown_function_in_expr(
    type_conversion_context: &TypeConversionContext,
    expr: &ASTExpression,
) -> bool {
    match expr {
        ASTExpression::ASTFunctionCallExpression(call) => {
            type_conversion_context.get(&call.function_name).is_none()
                || call
                    .parameters
                    .iter()
                    .any(|it| unknown_function_in_expr(type_conversion_context, it))
        }
        ASTExpression::Lambda(lambda) => lambda
            .body
            .iter()
            .any(|it| unknown_function_in_expr(type_conversion_context, it)),
        _ => false,
    }
}

 */

fn get_generic_types(ast_type: &ASTType) -> Vec<String> {
    return match ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::String => {
                vec![]
            }
            BuiltinTypeKind::I32 => {
                vec![]
            }
            BuiltinTypeKind::Bool => {
                vec![]
            }
            BuiltinTypeKind::Char => {
                vec![]
            }
            BuiltinTypeKind::F32 => {
                vec![]
            }
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => {
                let mut par_types: Vec<String> =
                    parameters.iter().flat_map(get_generic_types).collect();
                if let Some(rt) = return_type {
                    par_types.append(&mut get_generic_types(rt.as_ref()));
                }
                par_types.sort();
                par_types.dedup();
                par_types
            }
        },
        ASTType::Generic(p) => {
            vec![p.into()]
        }
        ASTType::Custom {
            name: _,
            param_types: pt,
        } => {
            let mut result: Vec<String> = pt
                .iter()
                .flat_map(|it| match it.clone() {
                    ASTType::Generic(name) => {
                        vec![name]
                    }
                    _ => get_generic_types(it),
                })
                .collect();
            result.sort();
            result.dedup();
            result
        }
    };
}

fn convert_statement_in_body(
    module: &EnhancedASTModule,
    statement: &ASTStatement,
    context: &mut ValContext,
    typed_context: &RefCell<TypeConversionContext>,
    backend: &dyn Backend,
    call_stack: &CallStack,
    statics: &Statics,
) -> Result<Option<ASTStatement>, TypeCheckError> {
    match statement {
        ASTStatement::Expression(e) => convert_expr_in_body(
            module,
            e,
            context,
            typed_context,
            backend,
            call_stack,
            statics,
        )
        .map(|ito| ito.map(ASTStatement::Expression)),
        ASTStatement::LetStatement(name, e, is_const, let_index) => {
            let result = convert_expr_in_body(
                module,
                e,
                context,
                typed_context,
                backend,
                call_stack,
                statics,
            )
            .map(|ito| {
                ito.map(|it| {
                    ASTStatement::LetStatement(name.clone(), it, *is_const, let_index.clone())
                })
            });

            let ast_type = get_type_of_expression(
                module,
                context,
                e,
                typed_context,
                None,
                call_stack,
                backend,
                statics,
            )?
            .unwrap_or_else(|| panic!("cannot get type of expression {e}: {let_index}"));

            if *is_const {
                panic!("const not allowed here");
            } else {
                context.insert_let(name.into(), ast_type, let_index);
            }
            result
        }
    }
}

fn convert_expr_in_body(
    module: &EnhancedASTModule,
    expr: &ASTExpression,
    context: &ValContext,
    typed_context: &RefCell<TypeConversionContext>,
    backend: &dyn Backend,
    call_stack: &CallStack,
    statics: &Statics,
) -> Result<Option<ASTExpression>, TypeCheckError> {
    debug_i!("converting expr in body {expr}");

    indent!();

    let result = match expr {
        ASTFunctionCallExpression(call) => {
            match convert_call(
                module,
                context,
                call,
                typed_context,
                None,
                backend,
                call_stack,
                statics,
            )? {
                NothingToConvert => None,
                // TODO is right?
                SomethingConverted => Some(expr.clone()),
                Converted(new_call) => Some(ASTFunctionCallExpression(new_call)),
            }
        }
        ASTExpression::StringLiteral(_) => None,
        ASTExpression::ValueRef(_, _) => None,
        ASTExpression::Value(_, _index) => None,
        ASTExpression::Lambda(_) => None,
        ASTExpression::Any(_) => None,
    };

    if result.is_some() {
        debug_i!("something converted in convert_expr_in_body");
    }
    dedent!();
    Ok(result)
}

fn convert_last_statement_in_body(
    module: &EnhancedASTModule,
    statement: &ASTStatement,
    context: &ValContext,
    typed_context: &RefCell<TypeConversionContext>,
    return_type: Option<ASTType>,
    backend: &dyn Backend,
    call_stack: &CallStack,
    statics: &Statics,
    resolved_generic_types: &LinkedHashMap<String, ASTType>,
) -> Result<Option<ASTStatement>, TypeCheckError> {
    match statement {
        ASTStatement::Expression(e) => convert_last_expr_in_body(
            module,
            e,
            context,
            typed_context,
            return_type,
            backend,
            call_stack,
            statics,
            resolved_generic_types,
        )
        .map(|ito| ito.map(ASTStatement::Expression)),
        ASTStatement::LetStatement(_name, _e, _is_const, let_index) => Err(format!(
            "let/const statement cannot be the last expression : {}",
            let_index
        )
        .into()),
    }
}

fn convert_last_expr_in_body(
    module: &EnhancedASTModule,
    expr: &ASTExpression,
    context: &ValContext,
    typed_context: &RefCell<TypeConversionContext>,
    return_type: Option<ASTType>,
    backend: &dyn Backend,
    call_stack: &CallStack,
    statics: &Statics,
    resolved_generic_types: &LinkedHashMap<String, ASTType>,
) -> Result<Option<ASTExpression>, TypeCheckError> {
    debug_i!(
        "converting last expr in body {expr} return_type {:?} resolved_generic_types {:?}",
        return_type,
        resolved_generic_types
    );

    indent!();

    let result = match expr {
        ASTFunctionCallExpression(call) => {
            if let Some(ast_type) = &return_type {
                let result = match convert_call(
                    module,
                    context,
                    call,
                    typed_context,
                    Some(return_type),
                    backend,
                    call_stack,
                    statics,
                )? {
                    NothingToConvert => None,
                    SomethingConverted => Some(expr.clone()),
                    Converted(new_call) => Some(ASTFunctionCallExpression(new_call)),
                };

                dedent!();
                return Ok(result);
            }

            None
        }
        ASTExpression::StringLiteral(_) => None,
        ASTExpression::ValueRef(_, _index) => None,

        ASTExpression::Value(_, _index) => None,
        ASTExpression::Lambda(lambda_def) => {
            let resolved_generic_types = LinkedHashMap::new();
            let rt = return_type.unwrap();
            let context =
                get_context_from_lambda(context, lambda_def, &rt, &resolved_generic_types)?;
            convert_lambda(
                module,
                &rt,
                lambda_def,
                &context,
                typed_context,
                &resolved_generic_types,
                backend,
                call_stack,
                statics,
            )?
            .map(ASTExpression::Lambda)
        }
        ASTExpression::Any(_) => None,
    };

    dedent!();
    Ok(result)
}

#[derive(Display)]
pub enum ConvertCallResult {
    NothingToConvert,
    SomethingConverted,
    Converted(ASTFunctionCall),
}

/// return None if nothing has been converted, Some((call, something converted))
pub fn convert_call(
    module: &EnhancedASTModule,
    context: &ValContext,
    call: &ASTFunctionCall,
    typed_context: &RefCell<TypeConversionContext>,
    expected_return_type: Option<Option<ASTType>>,
    backend: &dyn Backend,
    call_stack: &CallStack,
    statics: &Statics,
) -> Result<ConvertCallResult, TypeCheckError> {
    debug_i!(
        "converting call {}: {} expected return type: {:?}",
        call,
        call.index,
        expected_return_type
    );

    indent!();

    if context.get(&call.function_name).is_some() {
        debug_i!("found function in context parameters");
        dedent!();
        return Ok(NothingToConvert);
    }

    let call_stack = call_stack.add(call.clone());

    let (function_def, _) = get_called_function(
        module,
        context,
        call,
        typed_context,
        &expected_return_type,
        &call_stack,
        backend,
        None,
        false,
        statics,
    )?
    .unwrap_or_else(|| {
        typed_context.borrow().debug_i();
        module.debug_i();
        panic!(
            "Cannot find function {}: {}",
            call.function_name, call.index
        )
    });

    let mut resolved_generic_types = function_def.resolved_generic_types.clone();

    let expected_return_type = if let Some(Some(ret)) = expected_return_type {
        if get_generic_types(&ret).is_empty() {
            let generic_types_from_effective_type = resolve_generic_types_from_effective_type(
                &function_def.return_type.unwrap(),
                &ret,
            )?;

            resolved_generic_types.extend(generic_types_from_effective_type);

            Some(Some(ret))
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
                "Cannot find parameter {i} when calling function {} effective function {}",
                &function_def.name, call.function_name
            )
        });

        debug_i!("converting parameter {par}");
        debug_i!("resolved generics {:?}", resolved_generic_types);

        match expr {
            ASTExpression::Lambda(lambda) => {
                let lambda_type =
                    if let Some(new_type) = substitute(&par.ast_type, &resolved_generic_types) {
                        new_type
                    } else {
                        par.ast_type.clone()
                    };
                let mut context = get_context_from_lambda(
                    context,
                    lambda,
                    &lambda_type,
                    &resolved_generic_types,
                )?;

                let effective_lambda = if let Some(new_lambda) = convert_lambda(
                    module,
                    &par.ast_type,
                    lambda,
                    &context,
                    typed_context,
                    &resolved_generic_types,
                    backend,
                    &call_stack,
                    statics,
                )? {
                    debug_i!("lambda something converted");
                    something_converted_in_loop = true;
                    new_lambda
                } else {
                    lambda.clone()
                };

                let new_return_type = match &par.ast_type {
                    ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters: _lambda_parameters,
                        return_type, // TODO I cannot convert the return type at this stage
                    }) => {
                        if let Some(rt) = return_type {
                            if let Some(new_t) = substitute(rt, &resolved_generic_types) {
                                debug_i!("lambda something converted in return type new {new_t}");
                                something_converted_in_loop = true;
                                Some(new_t)
                            } else if let Some(last) = effective_lambda.body.last() {
                                for statement in effective_lambda.body.iter() {
                                    if let ASTStatement::LetStatement(
                                        name,
                                        expr,
                                        is_const,
                                        let_index,
                                    ) = statement
                                    {
                                        let ast_type = get_type_of_expression(
                                            module,
                                            &context,
                                            expr,
                                            typed_context,
                                            None,
                                            &call_stack,
                                            backend,
                                            statics,
                                        )?
                                        .unwrap();

                                        if *is_const {
                                            panic!("const not allowed here")
                                        } else {
                                            context.insert_let(name.clone(), ast_type, let_index);
                                        }
                                    }
                                }

                                let result_type = get_type_of_statement(
                                    module,
                                    &context,
                                    last,
                                    typed_context,
                                    &call_stack,
                                    backend,
                                    statics,
                                )?;

                                // the generic types of the expression do not belong to this
                                if result_type
                                    .clone()
                                    .map(|it| get_generic_types(&it).is_empty())
                                    .unwrap_or(true)
                                {
                                    result_type
                                } else {
                                    Some(rt.as_ref().clone())
                                }
                            } else {
                                Some(rt.as_ref().clone())
                            }
                        } else {
                            None
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
                        .map(|it| match substitute(it, &resolved_generic_types) {
                            None => it.clone(),
                            Some(p) => {
                                debug_i!("lambda something converted in parameter {p}");
                                something_converted_in_loop = true;
                                p
                            }
                        })
                        .collect();
                    something_converted_in_loop = update(
                        &ASTType::Builtin(BuiltinTypeKind::Lambda {
                            return_type: new_return_type.map(Box::new),
                            parameters: new_parameters,
                        }),
                        ASTExpression::Lambda(effective_lambda),
                        par,
                        &mut resolved_generic_types,
                        &mut converted_parameters,
                        &mut converted_expressions,
                    )? || something_converted_in_loop;
                } else {
                    dedent!();
                    return Err(format!("Expected Lambda but found {}", &par.ast_type).into());
                }
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
                let par_type =
                    if let Some(new_r_t) = substitute(&par.ast_type, &resolved_generic_types) {
                        new_r_t
                    } else {
                        par.ast_type.clone()
                    };
                let expected_return_type = if get_generic_types(&par_type).is_empty() {
                    Some(Some(par_type.clone()))
                } else {
                    None
                };

                let convert_call_result = convert_call(
                    module,
                    context,
                    call,
                    typed_context,
                    expected_return_type,
                    backend,
                    &call_stack,
                    statics,
                )?;

                if let SomethingConverted = convert_call_result {
                    debug_i!("something partially converted in call {call}");
                    something_converted_in_loop = true;
                }
                let typed_context_ptr = typed_context.borrow();
                if let Converted(ast_function_call) = convert_call_result {
                    debug_i!("converted call {ast_function_call}");
                    something_converted_in_loop = true;
                    //info!("new_function_defs {:?} used_untyped_function_defs {:?}", new_function_defs, used_untyped_function_defs);

                    let inner_function_def = typed_context_ptr
                        .find_function(&ast_function_call.function_name)
                        .unwrap_or_else(|| {
                            panic!("Cannot find function {}", ast_function_call.function_name)
                        });

                    if let Some(rt) = &inner_function_def.return_type {
                        // the generic types of the inner function are not the same of the this function
                        let result_type = if get_generic_types(rt).is_empty() {
                            inner_function_def.return_type.clone().unwrap()
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
                            converted_parameters.push(par.clone());
                            converted_expressions
                                .push(ASTFunctionCallExpression(ast_function_call));
                        }
                    }
                } else if !get_generic_types(&par.ast_type).is_empty() {
                    if let Some(inner_function_def) =
                        typed_context_ptr.find_function(&call.function_name)
                    {
                        if let Some(rt) = &inner_function_def.return_type {
                            // the generic types of the inner function are not the same of the this function
                            let result_type = if get_generic_types(rt).is_empty() {
                                inner_function_def.return_type.clone().unwrap()
                            } else {
                                par.clone().ast_type
                            };
                            debug_i!("calling update for ASTFunctionCallExpression");
                            debug_i!(
                                "expression {}",
                                ASTExpression::ASTFunctionCallExpression(call.clone())
                            );
                            something_converted_in_loop = update(
                                &result_type,
                                ASTFunctionCallExpression(call.clone()),
                                par,
                                &mut resolved_generic_types,
                                &mut converted_parameters,
                                &mut converted_expressions,
                            )? || something_converted_in_loop;
                        } else {
                            panic!("A Void result is not supported");
                        }
                    } else if context.get(&call.function_name).is_some() {
                        if let Ok(Some(ast_type)) = get_type_of_expression(
                            module,
                            context,
                            expr,
                            typed_context,
                            None,
                            &call_stack,
                            backend,
                            statics,
                        ) {
                            something_converted_in_loop = update(
                                &ast_type,
                                ASTFunctionCallExpression(call.clone()),
                                par,
                                &mut resolved_generic_types,
                                &mut converted_parameters,
                                &mut converted_expressions,
                            )? || something_converted_in_loop;
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
            }
            ASTExpression::ValueRef(v, _) => {
                let result_type = match context.get(v) {
                    Some(ValKind::ParameterRef(_, referenced_parameter_def)) => {
                        // TODO the generic types are not the same of those in this function
                        //   so if there's some generic type, I cannot "resolve" the ref
                        let gen_types = get_generic_types(&referenced_parameter_def.ast_type);

                        if gen_types.is_empty() {
                            Some(referenced_parameter_def.ast_type.clone())
                        } else {
                            None
                        }
                    }
                    Some(ValKind::LetRef(_, ast_type)) => {
                        let gen_types = get_generic_types(ast_type);

                        if gen_types.is_empty() {
                            Some(ast_type.clone())
                        } else {
                            None
                        }
                    }
                    None => statics
                        .get_const(v)
                        .map(|it| Some(it.ast_type.clone()))
                        .unwrap_or_else(|| {
                            panic!(
                                "cannot find val {v}, actual context {:?}, consts: {:?}",
                                context.names(),
                                statics.const_names()
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
                let ast_type = get_value_type(val_type);
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

    let (function_def, function_def_from_module) = get_called_function(
        module,
        context,
        call,
        typed_context,
        &expected_return_type,
        &call_stack,
        backend,
        None,
        false,
        statics,
    )?
    .unwrap_or_else(|| {
        panic!(
            "cannot find function {}: {}",
            call.original_function_name, call.index
        )
    });

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
        if !get_generic_types(&par.ast_type).is_empty() {
            if let Some(new_ref) = substitute(&par.ast_type, &resolved_generic_types.clone()) {
                remaining_generic_types.append(&mut get_generic_types(&new_ref));
                parameters.push(ASTParameterDef {
                    name: par.name.clone(),
                    ast_type: new_ref,
                    ast_index: par.ast_index,
                });
            } else {
                remaining_generic_types.append(&mut get_generic_types(&par.ast_type));
                parameters.push(par);
            }
        } else {
            parameters.push(par);
        }
    }

    let new_return_type = if let Some(er) = expected_return_type {
        er
    } else {
        function_def.return_type.clone().map(|it| {
            let t = if let Some(new_t) = substitute(&it, &resolved_generic_types) {
                debug_i!("converted return type {new_t}");
                something_converted = true;
                new_t
            } else {
                it.clone()
            };

            remaining_generic_types.append(&mut get_generic_types(&t));
            t
        })
    };

    remaining_generic_types.sort();
    remaining_generic_types.dedup();

    if !remaining_generic_types.is_empty() {
        debug_i!(
            "remaining parametric types for {} {:?}, {something_converted}",
            call.function_name,
            remaining_generic_types
        );
        dedent!();
        if something_converted {
            return Ok(SomethingConverted);
        } else {
            return Ok(NothingToConvert);
        }
    }

    if !something_converted {
        debug_i!("nothing converted for {call}");

        if function_def.generic_types.is_empty() {
            debug_i!(
                "TODO check for not parameterized function {}",
                call.function_name
            );

            return if function_def_from_module {
                if let Some(f) = typed_context
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
            .any(|par| !get_generic_types(&par.ast_type).is_empty());

        if parameters_to_convert {
            //panic!();
        }

        dedent!();
        return Ok(NothingToConvert);
    }

    let new_function_def = ASTFunctionDef {
        original_name: function_def.original_name.clone(),
        name: function_def.name.clone(),
        parameters,
        return_type: new_return_type,
        body: function_def.body.clone(),
        generic_types: remaining_generic_types,
        inline: function_def.inline,
        resolved_generic_types: resolved_generic_types.clone(),
    };

    let result = if let Some(f) = typed_context
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

fn get_value_type(val_type: &ValueType) -> ASTType {
    let ast_type = match val_type {
        ValueType::Boolean(_) => ASTType::Builtin(BuiltinTypeKind::Bool),
        ValueType::I32(_) => ASTType::Builtin(BuiltinTypeKind::I32),
        ValueType::Char(_) => ASTType::Builtin(BuiltinTypeKind::Char),
        ValueType::F32(_) => ASTType::Builtin(BuiltinTypeKind::F32),
    };
    ast_type
}

fn get_called_function(
    module: &EnhancedASTModule,
    context: &ValContext,
    call: &ASTFunctionCall,
    typed_context: &RefCell<TypeConversionContext>,
    expected_return_type: &Option<Option<ASTType>>,
    call_stack: &CallStack,
    backend: &dyn Backend,
    verify_function: Option<&ASTFunctionDef>,
    only_from_module: bool,
    statics: &Statics,
) -> Result<Option<(ASTFunctionDef, bool)>, TypeCheckError> {
    if let Some(f) = verify_function {
        if f.parameters.len() != call.parameters.len() {
            return Err("function parameters are not the same of the call".into());
        }
        debug_i!(
            "verifying function {f} for {call} return type {}",
            OptionOptionDisplay(expected_return_type)
        );
    } else {
        debug_i!(
            "trying to find function for {call} return type {}",
            OptionOptionDisplay(expected_return_type)
        );
    }
    indent!();
    let mut function_def_from_module = false;

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
            match get_type_of_expression(
                module,
                context,
                it,
                typed_context,
                lambda.as_ref(),
                call_stack,
                backend,
                statics,
            ) {
                Ok(Some(ast_type)) => Ok(TypeFilter::Exact(ast_type)),
                Ok(None) => {
                    if matches!(it, ASTExpression::Lambda(_)) {
                        Ok(TypeFilter::Lambda)
                    } else {
                        Ok(TypeFilter::Any)
                    }
                }
                Err(e) => Err(e),
            }
        })
        .collect::<Result<Vec<_>, TypeCheckError>>()?;

    let mut candidate_functions = if only_from_module {
        Vec::new()
    } else {
        typed_context.borrow().find_call_vec(
            call,
            call_parameters_types.clone(),
            expected_return_type.clone(),
        )
    };

    if candidate_functions.is_empty() {
        debug_i!("No function from context");
        function_def_from_module = true;
        candidate_functions = module.find_call_vec(
            call,
            call_parameters_types.clone(),
            expected_return_type.clone(),
        );
    } else {
        debug_i!("candidate_functions {}", SliceDisplay(&candidate_functions));
    }

    if candidate_functions.is_empty() {
        typed_context.borrow().debug_i();
        module.debug_i();
        return Err(format!(
            "Cannot find function for {call} with filters {:?} with return type {:?} in: {}",
            call_parameters_types, expected_return_type, call.index
        )
        .into());
    }

    let function_def = if candidate_functions.len() == 1 {
        candidate_functions.get(0).cloned()
    } else if call_parameters_types
        .iter()
        .any(|it| matches!(it, TypeFilter::Any))
    {
        let mut found_function_def = None;
        for f_def in candidate_functions {
            debug_i!(
                "Trying to find the right function {f_def} filter {:?}",
                call_parameters_types
            );
            let new_call_parameters_types = call_parameters_types
                .iter()
                .enumerate()
                .map(|(i, filter)| {
                    debug_i!("filter {i} {:?}", filter);
                    match filter {
                        TypeFilter::Any | TypeFilter::Lambda => {
                            let par = f_def.parameters.get(i).unwrap();
                            let expr = call.parameters.get(i).unwrap();
                            debug_i!("found None filter for {par}");
                            match &par.ast_type {
                                ASTType::Builtin(BuiltinTypeKind::Lambda {
                                    parameters: _,
                                    return_type: _,
                                }) => match expr {
                                    ASTExpression::Lambda(_lambda_def) => {
                                        if let Some(la) = get_type_of_expression(
                                            module,
                                            context,
                                            expr,
                                            typed_context,
                                            Some(&par.ast_type),
                                            call_stack,
                                            backend,
                                            statics,
                                        )? {
                                            Ok(TypeFilter::Exact(la))
                                        } else {
                                            Ok(TypeFilter::Lambda)
                                        }
                                    }
                                    _ => Err("Expected lambda".into()),
                                },
                                _ => Ok(TypeFilter::Any),
                            }
                        }
                        TypeFilter::Exact(f) => Ok(TypeFilter::Exact(f.clone())),
                    }
                })
                .collect::<Result<Vec<_>, TypeCheckError>>()?;

            debug_i!("new call parameters types {:?}", new_call_parameters_types);
            let mut new_function_def_opt = typed_context.borrow().find_call_vec(
                call,
                new_call_parameters_types.clone(),
                expected_return_type.clone(),
            );

            if new_function_def_opt.is_empty() {
                function_def_from_module = true;
                new_function_def_opt = module.find_call_vec(
                    call,
                    new_call_parameters_types.clone(),
                    expected_return_type.clone(),
                );
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
                .filter(|it| {
                    FunctionsContainer::almost_same_return_type(
                        &it.return_type,
                        ex,
                        &mut resolved_generic_types,
                    )
                })
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
        let all_not_parametric = candidate_functions
            .iter()
            .filter(|it| {
                it.parameters
                    .iter()
                    .all(|p| !matches!(p.ast_type, ASTType::Generic(_)))
            })
            .collect::<Vec<_>>();

        if all_not_parametric.len() == 1 {
            all_not_parametric.first().copied().cloned()
        } else {
            panic!(
                "Cannot find one single function for {call} but {:?}: {}",
                candidate_functions
                    .iter()
                    .map(|it| format!("{it}"))
                    .collect::<Vec<_>>(),
                call.index
            );
        }
    };

    dedent!();
    if let Some(fd) = function_def {
        debug_i!("found function def {fd}");

        if verify_function.is_none() && !only_from_module && !function_def_from_module {
            if let Ok(Some((f, new_function_def_from_module))) = get_called_function(
                module,
                context,
                call,
                typed_context,
                expected_return_type,
                call_stack,
                backend,
                Some(&fd),
                false,
                statics,
            ) {
                if f != fd {
                    // TODO it's really a guess...
                    return Ok(Some((f, new_function_def_from_module)));
                    //return Err(format!("different function {f} {fd}").into());
                }
            } else {
                // panic!("invalid function");
                return get_called_function(
                    module,
                    context,
                    call,
                    typed_context,
                    expected_return_type,
                    call_stack,
                    backend,
                    None,
                    true,
                    statics,
                );
            }
        }

        Ok(Some((fd, function_def_from_module)))
    } else {
        debug_i!("cannot find function");
        Ok(None)
    }

    // Ok((function_def.clone(), function_def_from_module))
}

fn get_type_of_statement(
    module: &EnhancedASTModule,
    context: &ValContext,
    statement: &ASTStatement,
    typed_context: &RefCell<TypeConversionContext>,
    call_stack: &CallStack,
    backend: &dyn Backend,
    statics: &Statics,
) -> Result<Option<ASTType>, TypeCheckError> {
    match statement {
        ASTStatement::Expression(e) => get_type_of_expression(
            module,
            context,
            e,
            typed_context,
            None,
            call_stack,
            backend,
            statics,
        ),
        ASTStatement::LetStatement(_, e, _is_const, _let_index) => get_type_of_expression(
            module,
            context,
            e,
            typed_context,
            None,
            call_stack,
            backend,
            statics,
        ),
    }
}

fn get_type_of_expression(
    module: &EnhancedASTModule,
    context: &ValContext,
    expr: &ASTExpression,
    typed_context: &RefCell<TypeConversionContext>,
    lambda: Option<&ASTType>,
    call_stack: &CallStack,
    backend: &dyn Backend,
    statics: &Statics,
) -> Result<Option<ASTType>, TypeCheckError> {
    debug_i!("get_type_of_expression {expr} lambda {:?}", lambda);
    indent!();

    let result: Result<Option<ASTType>, TypeCheckError> = match expr {
        ASTExpression::StringLiteral(_) => Ok(Some(ASTType::Builtin(BuiltinTypeKind::String))),
        ASTFunctionCallExpression(call) => get_type_of_call(
            module,
            context,
            call,
            typed_context,
            call_stack,
            backend,
            statics,
        ),
        ASTExpression::ValueRef(v, index) => {
            if let Some(value) = context.get(v) {
                match value {
                    ValKind::ParameterRef(_i, par) => Ok(Some(par.ast_type.clone())),
                    ValKind::LetRef(_name, ast_type) => Ok(Some(ast_type.clone())),
                }
            } else if let Some(entry) = statics.get_const(v) {
                Ok(Some(entry.ast_type.clone()))
            } else {
                Err(format!(
                    "Unknown val {v} context {:?} statics: {:?}: {index}",
                    context.names(),
                    statics.const_names()
                )
                .into())
            }
        }
        ASTExpression::Value(val_type, _) => Ok(match val_type {
            ValueType::Boolean(_) => Some(ASTType::Builtin(BuiltinTypeKind::Bool)),
            ValueType::I32(_) => Some(ASTType::Builtin(BuiltinTypeKind::I32)),
            ValueType::Char(_) => Some(ASTType::Builtin(BuiltinTypeKind::Char)),
            ValueType::F32(_) => Some(ASTType::Builtin(BuiltinTypeKind::F32)),
        }),
        ASTExpression::Any(ast_type) => Ok(Some(ast_type.clone())),
        ASTExpression::Lambda(def) => {
            let mut lambda_val_context = if let Some(lambda_type) = lambda {
                get_context_from_lambda(context, def, lambda_type, &LinkedHashMap::new()).unwrap()
            } else {
                if !def.parameter_names.is_empty() {
                    return Ok(None);
                }
                context.clone()
            };
            let mut lamda_return_type = None;

            let len = def.body.len();

            let result = def
                .body
                .iter()
                .enumerate()
                .map(|(i, statement)| match statement {
                    ASTStatement::LetStatement(name, let_statement, is_const, let_index) => {
                        // let mut new_call_stack = call_stack.clone();

                        let skip = if let ASTFunctionCallExpression(inner_call) = let_statement {
                            debug_i!("adding inner call {inner_call}");
                            // new_call_stack = call_stack.add(inner_call.clone());
                            if call_stack.exists(inner_call) {
                                //panic!("loop");
                                false
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        if !skip {
                            if let Some(let_return_type) = get_type_of_expression(
                                module,
                                &lambda_val_context,
                                let_statement,
                                typed_context,
                                None,
                                call_stack,
                                backend,
                                statics,
                            )? {
                                if *is_const {
                                    // TODO I don't think it should happen
                                    panic!("const not allowed here")
                                } else {
                                    lambda_val_context.insert_let(
                                        name.clone(),
                                        let_return_type,
                                        let_index,
                                    );
                                }
                                Ok(())
                            } else {
                                Err("expected expression value".into())
                            }
                        } else {
                            Ok(())
                        }
                    }
                    ASTStatement::Expression(body_expr) => {
                        // let mut new_call_stack = call_stack.clone();

                        let skip = if let ASTFunctionCallExpression(inner_call) = body_expr {
                            debug_i!("addin inner call {inner_call}");
                            // new_call_stack = call_stack.add(inner_call.clone());
                            if call_stack.exists(inner_call) {
                                // panic!("loop");
                                false
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        if !skip && i == len - 1 {
                            lamda_return_type = get_type_of_expression(
                                module,
                                &lambda_val_context,
                                body_expr,
                                typed_context,
                                None,
                                call_stack,
                                backend,
                                statics,
                            )?;
                            Ok(())
                        } else {
                            Ok(())
                        }
                    }
                })
                .collect::<Vec<Result<(), TypeCheckError>>>();

            if let Err(err) = result
                .into_iter()
                .filter(|it| it.is_err())
                .collect::<Result<Vec<()>, TypeCheckError>>()
            {
                return Err(err);
            }

            if log_enabled!(Level::Debug) {
                debug_i!("lambda return type: {:?}", lamda_return_type);
                for (par_name, _) in &def.parameter_names {
                    debug_i!("par {par_name}: {:?}", lambda_val_context.get(par_name));
                }
            }
            if lamda_return_type.is_some() {
                Ok(Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                    return_type: lamda_return_type.map(Box::new),
                    parameters: def
                        .parameter_names
                        .iter()
                        .map(|(it, _)| match lambda_val_context.get(it).unwrap() {
                            ValKind::ParameterRef(_, def) => def.ast_type.clone(),
                            ValKind::LetRef(_, def) => def.clone(),
                        })
                        .collect::<Vec<_>>(),
                })))
            } else {
                Ok(None)
            }
            // Some(ASTType::Builtin(BuiltinTypeKind::Lambda { return_type: None, parameters: vec![]}))
        }
    };

    debug_i!("result {:?}", result);

    dedent!();

    result
}

fn get_type_of_call(
    module: &EnhancedASTModule,
    context: &ValContext,
    call: &ASTFunctionCall,
    typed_context: &RefCell<TypeConversionContext>,
    call_stack: &CallStack,
    backend: &dyn Backend,
    statics: &Statics,
) -> Result<Option<ASTType>, TypeCheckError> {
    if let Some(ValKind::ParameterRef(_i, par)) = context.get(&call.function_name) {
        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
            return_type,
            parameters: _,
        }) = &par.ast_type
        {
            Ok(return_type.clone().map(|it| it.as_ref().clone()))
        } else {
            Err("Expected a lambda".into())
        }
    } else if let Some(ValKind::LetRef(_i, ast_type)) = context.get(&call.function_name) {
        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
            return_type,
            parameters: _,
        }) = ast_type
        {
            Ok(return_type.clone().map(|it| it.as_ref().clone()))
        } else {
            Err("Expected a lambda".into())
        }
    } else if let Some((function, _)) = get_called_function(
        module,
        context,
        call,
        typed_context,
        &None,
        call_stack,
        backend,
        None,
        false,
        statics,
    )? {
        if let Some(return_type) = function.return_type {
            if get_generic_types(&return_type).is_empty() {
                Ok(Some(return_type))
            } else {
                let convert_call_result = convert_call(
                    module,
                    context,
                    call,
                    typed_context,
                    None,
                    backend,
                    call_stack,
                    statics,
                );

                if let Ok(Converted(new_call)) = convert_call_result {
                    if let Some((function, _)) = get_called_function(
                        module,
                        context,
                        &new_call,
                        typed_context,
                        &None,
                        call_stack,
                        backend,
                        None,
                        false,
                        statics,
                    )? {
                        Ok(function.return_type)
                    } else {
                        Ok(Some(return_type))
                    }
                } else {
                    Ok(Some(return_type))
                }
            }
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

fn resolve_generic_types_from_effective_type(
    generic_type: &ASTType,
    effective_type: &ASTType,
) -> Result<LinkedHashMap<String, ASTType>, TypeCheckError> {
    let mut result: LinkedHashMap<String, ASTType> = LinkedHashMap::new();

    if generic_type == effective_type || get_generic_types(generic_type).is_empty() {
        return Ok(result);
    }

    debug_i!("extract_generic_types_from_effective_type: parametric_type {generic_type} effective_type  {effective_type}");
    indent!();

    match generic_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::String => {}
            BuiltinTypeKind::I32 => {}
            BuiltinTypeKind::Bool => {}
            BuiltinTypeKind::Char => {}
            BuiltinTypeKind::F32 => {}
            BuiltinTypeKind::Lambda {
                parameters: p_parameters,
                return_type: p_return_type,
            } => match effective_type {
                ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters: e_parameters,
                    return_type: e_return_type,
                }) => {
                    for (i, p_p) in p_parameters.iter().enumerate() {
                        let e_p = e_parameters.get(i).unwrap();
                        let inner_result = resolve_generic_types_from_effective_type(p_p, e_p)
                            .map_err(|e| format!("{} in lambda param gen type {generic_type}eff. type {effective_type}", e))?;

                        result.extend(inner_result.into_iter());
                    }

                    for p_t in p_return_type {
                        if let Some(e_t) = e_return_type {
                            let inner_result = resolve_generic_types_from_effective_type(p_t, e_t)
                                .map_err(|e| format!("{} in return type gen type {generic_type}eff. type {effective_type}", e))?;

                            result.extend(inner_result.into_iter());
                        } else {
                            dedent!();
                            if let ASTType::Generic(p) = p_t.as_ref() {
                                return Err(format!("Found parametric type {p} that is (). For now we cannot handle it").into());
                            }
                            return Err("Expected some type but got None".into());
                        }
                    }
                }
                _ => {
                    dedent!();
                    return Err("unmatched types".into());
                }
            },
        },
        ASTType::Generic(p) => {
            debug_i!("resolved generic type {p} to {effective_type}");
            result.insert(p.clone(), effective_type.clone());
        }
        ASTType::Custom {
            name: p_name,
            param_types: p_param_types,
        } => match effective_type {
            ASTType::Custom {
                name: e_name,
                param_types: e_param_types,
            } => {
                if p_name != e_name {
                    dedent!();
                    return Err(format!("unmatched custom type name {p_name} {e_name}").into());
                }

                for (i, p_p) in p_param_types.iter().enumerate() {
                    let e_p = e_param_types.get(i).unwrap();
                    let inner_result = resolve_generic_types_from_effective_type(p_p, e_p)
                        .map_err(|e|format!("{}\nin custom type gen type {generic_type} eff type {effective_type}", e.message))?;

                    result.extend(inner_result.into_iter());
                }
            }
            ASTType::Generic(_) => {}
            _ => {
                dedent!();
                return Err(format!(
                    "unmatched types generic type: {:?}, effective type: {:?}",
                    generic_type, effective_type
                )
                .into());
            }
        },
    }

    debug_i!("result {}", result.my_to_string());
    dedent!();
    Ok(result)
}

fn get_context_from_lambda(
    context: &ValContext,
    lambda: &ASTLambdaDef,
    lambda_type: &ASTType,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
) -> Result<ValContext, TypeCheckError> {
    let mut context = ValContext::new(Some(context));

    for (inner_i, (name, index)) in lambda.parameter_names.iter().enumerate() {
        match lambda_type {
            ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters,
                return_type: _, // I cannot convert the return type at this stage
            }) => {
                let pp = parameters
                    .get(inner_i)
                    .expect(&format!("cannot find parameter {inner_i}: {}", index));

                let p = if let Some(ct) = substitute(pp, resolved_param_types) {
                    ct
                } else {
                    pp.clone()
                };

                context.insert_par(
                    name.clone(),
                    ASTParameterDef {
                        name: name.clone(),
                        ast_type: p,
                        ast_index: index.clone(),
                    },
                );
            }
            _ => {
                return Err(format!("expected lambda but got {lambda_type}").into());
            }
        }
    }

    Ok(context)
}

fn convert_lambda(
    module: &EnhancedASTModule,
    lambda_type: &ASTType,
    lambda: &ASTLambdaDef,
    context: &ValContext,
    typed_context: &RefCell<TypeConversionContext>,
    resolved_generic_types: &LinkedHashMap<String, ASTType>,
    backend: &dyn Backend,
    call_stack: &CallStack,
    statics: &Statics,
) -> Result<Option<ASTLambdaDef>, TypeCheckError> {
    debug_i!(
        "converting lambda_type {lambda_type}, lambda {lambda}, resolved_param_types {:?}",
        resolved_generic_types
    );
    indent!();

    let (_parameters, return_type) = match &lambda_type {
        ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters,
            return_type, // I cannot convert the return type at this stage
        }) => (parameters, return_type),
        _ => {
            dedent!();
            return Err(format!("expected lambda but got {lambda_type}").into());
        }
    };

    debug_i!("return type {:?}", return_type);

    let rt = return_type.clone().and_then(|it| {
        substitute(it.as_ref(), resolved_generic_types)
            .or_else(|| return_type.clone().map(|rt| rt.as_ref().clone()))
    });

    let result = if let Some(new_body) = convert_body(
        backend,
        module,
        typed_context,
        context,
        &lambda.body,
        call_stack,
        rt,
        statics,
        resolved_generic_types,
    )? {
        debug_i!("lambda type something converted");
        Some(ASTLambdaDef {
            body: new_body,
            parameter_names: lambda.parameter_names.clone(),
        })
    } else {
        debug_i!("lambda type nothing converted");
        None
    };

    dedent!();

    Ok(result)
}

fn substitute(
    ast_type: &ASTType,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
) -> Option<ASTType> {
    if get_generic_types(ast_type).is_empty() {
        return None;
    }

    let result = match &ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => {
                let mut something_substituted = false;
                let new_parameters = match substitute_types(parameters, resolved_param_types) {
                    None => parameters.clone(),
                    Some(new_parameters) => {
                        something_substituted = true;
                        new_parameters
                    }
                };

                let new_return_type = return_type.clone().map(|it| {
                    if let Some(new_t) = substitute(&it, resolved_param_types) {
                        something_substituted = true;
                        Box::new(new_t)
                    } else {
                        it
                    }
                });

                if something_substituted {
                    Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters: new_parameters,
                        return_type: new_return_type,
                    }))
                } else {
                    None
                }
            }
            _ => None,
        },
        ASTType::Generic(p) => {
            if resolved_param_types.contains_key(p) {
                resolved_param_types.get(p).cloned()
            } else {
                None
            }
        }
        ASTType::Custom { name, param_types } => {
            substitute_types(param_types, resolved_param_types).map(|new_param_types| {
                ASTType::Custom {
                    name: name.clone(),
                    param_types: new_param_types,
                }
            })
        }
    };

    if let Some(r) = &result {
        debug_i!("something substituted {ast_type} -> {r}");
    }
    result
}

fn substitute_types(
    types: &[ASTType],
    resolved_param_types: &LinkedHashMap<String, ASTType>,
) -> Option<Vec<ASTType>> {
    let mut something_substituted = false;
    let new_types = types
        .iter()
        .map(|it| {
            if let Some(new_t) = substitute(it, resolved_param_types) {
                something_substituted = true;
                new_t
            } else {
                it.clone()
            }
        })
        .collect();

    if something_substituted {
        Some(new_types)
    } else {
        None
    }
}

fn update(
    result_type: &ASTType,
    expr: ASTExpression,
    par: &ASTParameterDef,
    resolved_param_types: &mut LinkedHashMap<String, ASTType>,
    parameters: &mut Vec<ASTParameterDef>,
    expressions: &mut Vec<ASTExpression>,
) -> Result<bool, TypeCheckError> {
    if get_generic_types(&par.ast_type).is_empty() {
        expressions.push(expr);
        parameters.push(par.clone());
        return Ok(false);
    }

    let generic_types_from_effective_type =
        resolve_generic_types_from_effective_type(&par.ast_type, result_type)
            .map_err(|e| format!("{} in update par {par} result_type {result_type}", e))?;

    let len_before = resolved_param_types.len();

    resolved_param_types.extend(generic_types_from_effective_type.clone());

    let some_generic_tye_added = len_before != resolved_param_types.len();

    let mut message = String::new();

    if some_generic_tye_added {
        message += &format!(
            "added resolved types {:?}",
            generic_types_from_effective_type
        );
    }

    let result = if let Some(t) = substitute(&par.ast_type, resolved_param_types) {
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

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use linked_hash_map::LinkedHashMap;

    use crate::codegen::backend::BackendAsm386;
    use crate::codegen::enhanced_module::EnhancedASTModule;
    use crate::codegen::statics::Statics;
    use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTModule,
        ASTParameterDef, ASTStatement, ASTType, BuiltinTypeKind,
    };
    use crate::parser::ValueType;
    use crate::type_check::typed_ast::{
        convert_to_typed_module, ASTTypedExpression, ASTTypedStatement,
    };
    use crate::type_check::{resolve_generic_types_from_effective_type, TypeCheckError};

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_simple() -> Result<(), TypeCheckError> {
        let parametric_type = parametric("T");
        let effective_type = i32();
        let result = resolve_generic_types_from_effective_type(&parametric_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_custom() -> Result<(), TypeCheckError> {
        let parametric_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![parametric("T")],
        };
        let effective_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![i32()],
        };

        let result = resolve_generic_types_from_effective_type(&parametric_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda() -> Result<(), TypeCheckError> {
        let parametric_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![parametric("T")],
            return_type: Some(Box::new(parametric("T"))),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![parametric("T")],
            return_type: Some(Box::new(i32())),
        });

        let result = resolve_generic_types_from_effective_type(&parametric_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda1() -> Result<(), TypeCheckError> {
        let parametric_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![parametric("T")],
            return_type: Some(Box::new(parametric("T"))),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![i32()],
            return_type: Some(Box::new(parametric("T"))),
        });

        let result = resolve_generic_types_from_effective_type(&parametric_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);
        Ok(())
    }

    fn parametric(name: &str) -> ASTType {
        ASTType::Generic(name.into())
    }

    fn i32() -> ASTType {
        ASTType::Builtin(BuiltinTypeKind::I32)
    }

    #[test]
    #[ignore]
    // it cannot works since it need some default functions, like addref
    fn test() {
        init();

        let parameter = ASTExpression::Value(
            ValueType::I32(10),
            ASTIndex {
                file_name: None,
                row: 0,
                column: 0,
            },
        );

        let call = ASTStatement::Expression(ASTFunctionCallExpression(ASTFunctionCall {
            original_function_name: "consume".into(),
            function_name: "consume".into(),
            parameters: vec![parameter],
            index: ASTIndex {
                file_name: None,
                row: 0,
                column: 0,
            },
        }));

        let function_def = ASTFunctionDef {
            name: "consume".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: vec![ASTParameterDef {
                name: "v".into(),
                ast_type: ASTType::Generic("T".into()),
                ast_index: ASTIndex::none(),
            }],
            inline: false,
            return_type: None,
            generic_types: vec!["T".into()],
            resolved_generic_types: LinkedHashMap::new(),
            original_name: "consume".into(),
        };

        let module = ASTModule {
            structs: Vec::new(),
            enums: Vec::new(),
            body: vec![call],
            functions: vec![function_def],
            requires: Default::default(),
            externals: Default::default(),
            types: Vec::new(),
        };

        let (new_module, _) = convert_to_typed_module(
            &EnhancedASTModule::new(module),
            false,
            false,
            false,
            Vec::new(),
            &BackendAsm386::new(HashSet::new(), HashSet::new()),
            &mut Statics::new(),
            true,
        );

        let par = if let Some(ASTTypedStatement::Expression(
            ASTTypedExpression::ASTFunctionCallExpression(e),
        )) = new_module.body.get(0)
        {
            Some(e)
        } else {
            None
        };

        assert_eq!(par.unwrap().function_name, "consume");
        assert!(new_module.functions_by_name.get("consume_0").is_some());
    }

    /*
    TODO
    #[test]
    fn test_replace_native_call() {
        let body = "$call(nprint,10)";
        assert_eq!(
            "$call(nprint_2,10)".to_string(),
            replace_native_call(body, "nprint", "nprint_2", 0)
        );
    }

    #[test]
    fn test_replace_native_call1() {
        let body = "$call( nprint ,10)";
        assert_eq!(
            "$call(nprint_2,10)".to_string(),
            replace_native_call(body, "nprint", "nprint_2", 0)
        );
    }

    #[test]
    fn test_replace_native_call2() {
        let body = "$call(nprintln,10)";
        assert_eq!(
            "$call(nprintln,10)".to_string(),
            replace_native_call(body, "nprint", "nprint_2", 0)
        );
    }

    #[test]
    fn test_replace_native_call3() {
        let body = "$call(dummy)";
        assert_eq!(
            "$call(dummy_2)".to_string(),
            replace_native_call(body, "dummy", "dummy_2", 0)
        );
    }

     */
}
