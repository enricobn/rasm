use std::cell::RefCell;
use std::panic;

use linked_hash_map::LinkedHashMap;
use log::{debug, log_enabled, Level};

use call_converter::ConvertCallResult::{Converted, NothingToConvert, SomethingConverted};
use type_check_error::TypeCheckError;

use crate::codegen::backend::Backend;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{MacroParam, TextMacro};
use crate::codegen::val_context::ValContext;
use crate::codegen::ValKind;
use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
use crate::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTParameterDef,
    ASTType, BuiltinTypeKind,
};
use crate::parser::ast::{ASTStatement, MyToString};
use crate::type_check::call_converter::CallConverter;
use crate::type_check::typed_context::TypeConversionContext;
use crate::{debug_i, dedent, indent};

mod call_converter;
pub mod functions_container;
pub mod type_check_error;
pub mod typed_ast;
pub mod typed_context;

fn convert_function_def(
    backend: &dyn Backend,
    module: &EnhancedASTModule,
    type_conversion_context: &RefCell<TypeConversionContext>,
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
            if let Some(new_body) = convert_body(
                backend,
                module,
                type_conversion_context,
                &context,
                body,
                &function_def.return_type,
                statics,
            )? {
                let new_function_def = type_conversion_context
                    .borrow_mut()
                    .replace_body(function_def, ASTFunctionBody::RASMBody(new_body));

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
    return_type: &Option<ASTType>,
    statics: &Statics,
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
                    statics,
                )
            } else {
                convert_statement_in_body(
                    module,
                    it,
                    &mut context,
                    type_conversion_context,
                    backend,
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
    statement: ASTStatement,
    backend: &dyn Backend,
    statics: &mut Statics,
) -> bool {
    let mut something_to_convert = false;

    match &statement {
        ASTStatement::Expression(expr) => {
            if let ASTFunctionCallExpression(call @ ASTFunctionCall { .. }) = expr {
                let converted_call =
                    CallConverter::new(module, context, type_conversion_context, backend, statics)
                        .convert_call(call, None);

                match converted_call {
                    Err(e) => {
                        panic!("{e} expression: {}", expr);
                    }
                    Ok(NothingToConvert) => new_body.push(statement),
                    Ok(SomethingConverted) => {
                        new_body.push(statement);
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
                let converted_call =
                    CallConverter::new(module, context, type_conversion_context, backend, statics)
                        .convert_call(call, None);

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
                        new_body.push(statement)
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
                        new_body.push(statement)
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
                    statics.add_const(name.to_owned(), value_type.to_type());
                } else {
                    context.insert_let(name.clone(), value_type.to_type(), index);
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
            vec![p.to_owned()]
        }
        ASTType::Custom {
            name: _,
            param_types: pt,
            index: _,
        } => {
            let mut result: Vec<String> = pt
                .iter()
                .flat_map(|it| match it {
                    ASTType::Generic(name) => {
                        vec![name.clone()]
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

fn is_generic_type(ast_type: &ASTType) -> bool {
    return match ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::String => false,
            BuiltinTypeKind::I32 => false,
            BuiltinTypeKind::Bool => false,
            BuiltinTypeKind::Char => false,
            BuiltinTypeKind::F32 => false,
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => {
                let mut par_types: bool = parameters.iter().any(is_generic_type);
                if let Some(rt) = return_type {
                    par_types = par_types || is_generic_type(rt.as_ref());
                }
                par_types
            }
        },
        ASTType::Generic(p) => true,
        ASTType::Custom {
            name: _,
            param_types: pt,
            index: _,
        } => pt.iter().any(|it| match it {
            ASTType::Generic(name) => true,
            _ => is_generic_type(it),
        }),
    };
}

fn convert_statement_in_body(
    module: &EnhancedASTModule,
    statement: &ASTStatement,
    context: &mut ValContext,
    typed_context: &RefCell<TypeConversionContext>,
    backend: &dyn Backend,
    statics: &Statics,
) -> Result<Option<ASTStatement>, TypeCheckError> {
    match statement {
        ASTStatement::Expression(e) => {
            convert_expr_in_body(module, e, context, typed_context, backend, statics)
                .map(|ito| ito.map(ASTStatement::Expression))
        }
        ASTStatement::LetStatement(name, e, is_const, let_index) => {
            let result = convert_expr_in_body(module, e, context, typed_context, backend, statics)
                .map(|ito| {
                    ito.map(|it| {
                        ASTStatement::LetStatement(name.clone(), it, *is_const, let_index.clone())
                    })
                });

            let ast_type =
                get_type_of_expression(module, context, e, typed_context, None, backend, statics)?
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
    statics: &Statics,
) -> Result<Option<ASTExpression>, TypeCheckError> {
    debug_i!("converting expr in body {expr}");

    indent!();

    let result = match expr {
        ASTFunctionCallExpression(call) => {
            match CallConverter::new(module, context, typed_context, backend, statics)
                .convert_call(call, None)?
            {
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
    statics: &Statics,
) -> Result<Option<ASTStatement>, TypeCheckError> {
    match statement {
        ASTStatement::Expression(e) => convert_last_expr_in_body(
            module,
            e,
            context,
            typed_context,
            return_type,
            backend,
            statics,
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
    statics: &Statics,
) -> Result<Option<ASTExpression>, TypeCheckError> {
    debug_i!(
        "converting last expr in body {expr} return_type {:?}",
        return_type
    );

    indent!();

    let result = match expr {
        ASTFunctionCallExpression(call) => {
            if let Some(ast_type) = &return_type {
                let result =
                    match CallConverter::new(module, context, typed_context, backend, statics)
                        .convert_call(call, Some(return_type))?
                    {
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
                statics,
            )?
            .map(ASTExpression::Lambda)
        }
        ASTExpression::Any(_) => None,
    };

    dedent!();
    Ok(result)
}

fn get_type_of_statement(
    module: &EnhancedASTModule,
    context: &ValContext,
    statement: &ASTStatement,
    typed_context: &RefCell<TypeConversionContext>,
    backend: &dyn Backend,
    statics: &Statics,
) -> Result<Option<ASTType>, TypeCheckError> {
    match statement {
        ASTStatement::Expression(e) => {
            get_type_of_expression(module, context, e, typed_context, None, backend, statics)
        }
        ASTStatement::LetStatement(_, e, _is_const, _let_index) => {
            get_type_of_expression(module, context, e, typed_context, None, backend, statics)
        }
    }
}

fn get_type_of_expression(
    module: &EnhancedASTModule,
    context: &ValContext,
    expr: &ASTExpression,
    typed_context: &RefCell<TypeConversionContext>,
    lambda: Option<&ASTType>,
    backend: &dyn Backend,
    statics: &Statics,
) -> Result<Option<ASTType>, TypeCheckError> {
    debug_i!("get_type_of_expression {expr} lambda {:?}", lambda);
    indent!();

    let result: Result<Option<ASTType>, TypeCheckError> = match expr {
        ASTExpression::StringLiteral(_) => Ok(Some(ASTType::Builtin(BuiltinTypeKind::String))),
        ASTFunctionCallExpression(call) => {
            call_converter::get_type_of_call(module, context, call, typed_context, backend, statics)
        }
        ASTExpression::ValueRef(v, index) => {
            if let Some(value) = context.get(v) {
                match value {
                    ValKind::ParameterRef(_i, par) => Ok(Some(par.ast_type.clone())),
                    ValKind::LetRef(_name, ast_type, _) => Ok(Some(ast_type.clone())),
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
        ASTExpression::Value(val_type, _) => Ok(Some(val_type.to_type())),
        ASTExpression::Any(ast_type) => Ok(Some(ast_type.clone())),
        ASTExpression::Lambda(lambda_def) => {
            let mut lambda_val_context = if let Some(lambda_type) = lambda {
                get_context_from_lambda(context, lambda_def, lambda_type, &LinkedHashMap::new())
                    .unwrap()
            } else {
                if !lambda_def.parameter_names.is_empty() {
                    return Ok(None);
                }
                context.clone()
            };
            let mut lamda_return_type = None;

            let len = lambda_def.body.len();

            lambda_def
                .body
                .iter()
                .enumerate()
                .map(|(i, statement)| match statement {
                    ASTStatement::LetStatement(name, let_statement, is_const, let_index) => {
                        if let Some(let_return_type) = get_type_of_expression(
                            module,
                            &lambda_val_context,
                            let_statement,
                            typed_context,
                            None,
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
                    }
                    ASTStatement::Expression(body_expr) => {
                        if i == len - 1 {
                            lamda_return_type = get_type_of_expression(
                                module,
                                &lambda_val_context,
                                body_expr,
                                typed_context,
                                None,
                                backend,
                                statics,
                            )?;
                            Ok(())
                        } else {
                            Ok(())
                        }
                    }
                })
                .filter(|it| it.is_err())
                .collect::<Result<Vec<()>, TypeCheckError>>()?;

            if log_enabled!(Level::Debug) {
                debug_i!("lambda return type: {:?}", lamda_return_type);
                for (par_name, _) in &lambda_def.parameter_names {
                    debug_i!("par {par_name}: {:?}", lambda_val_context.get(par_name));
                }
            }
            if lamda_return_type.is_some() {
                Ok(Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                    return_type: lamda_return_type.map(Box::new),
                    parameters: lambda_def
                        .parameter_names
                        .iter()
                        .map(|(it, _)| match lambda_val_context.get(it).unwrap() {
                            ValKind::ParameterRef(_, def) => def.ast_type.clone(),
                            ValKind::LetRef(_, ast_type, _) => ast_type.clone(),
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

fn resolve_generic_types_from_effective_type(
    generic_type: &ASTType,
    effective_type: &ASTType,
) -> Result<LinkedHashMap<String, ASTType>, TypeCheckError> {
    let mut result: LinkedHashMap<String, ASTType> = LinkedHashMap::new();

    if generic_type == effective_type || !is_generic_type(generic_type) {
        return Ok(result);
    }

    debug_i!("extract_generic_types_from_effective_type: generic_type {generic_type} effective_type  {effective_type}");
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
                                return Err(format!("Found generic type {p} that is (). For now we cannot handle it").into());
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
            index: _,
        } => match effective_type {
            ASTType::Custom {
                name: e_name,
                param_types: e_param_types,
                index: _,
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
                    .unwrap_or_else(|| panic!("cannot find parameter {inner_i}: {}", index));

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

    let rt = if let Some(rt) = return_type {
        if let Some(srt) = substitute(rt.as_ref(), resolved_generic_types) {
            Some(srt)
        } else {
            Some(rt.as_ref().clone())
        }
    } else {
        None
    };

    let result = if let Some(new_body) = convert_body(
        backend,
        module,
        typed_context,
        context,
        &lambda.body,
        &rt,
        statics,
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
    if !is_generic_type(ast_type) {
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
        ASTType::Custom {
            name,
            param_types,
            index,
        } => {
            substitute_types(param_types, resolved_param_types).map(|new_param_types| {
                // TODO it's a bit heuristic
                let new_index = if new_param_types.is_empty() {
                    index.clone()
                } else if let Some(ASTType::Custom {
                    name,
                    param_types,
                    index: ast_index,
                }) = new_param_types.last()
                {
                    ast_index.mv(1)
                } else {
                    index.clone()
                };
                ASTType::Custom {
                    name: name.clone(),
                    param_types: new_param_types,
                    index: new_index,
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

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use linked_hash_map::LinkedHashMap;

    use crate::codegen::backend::BackendNasm386;
    use crate::codegen::enhanced_module::EnhancedASTModule;
    use crate::codegen::statics::Statics;
    use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTModule,
        ASTParameterDef, ASTStatement, ASTType, BuiltinTypeKind, ValueType,
    };
    use crate::type_check::resolve_generic_types_from_effective_type;
    use crate::type_check::type_check_error::TypeCheckError;
    use crate::type_check::typed_ast::{
        convert_to_typed_module, ASTTypedExpression, ASTTypedStatement,
    };

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_simple() -> Result<(), TypeCheckError> {
        let generic_type = generic("T");
        let effective_type = i32();
        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_custom() -> Result<(), TypeCheckError> {
        let generic_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![generic("T")],
            index: ASTIndex::none(),
        };
        let effective_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![i32()],
            index: ASTIndex::none(),
        };

        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda() -> Result<(), TypeCheckError> {
        let generic_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![generic("T")],
            return_type: Some(Box::new(generic("T"))),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![generic("T")],
            return_type: Some(Box::new(i32())),
        });

        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda1() -> Result<(), TypeCheckError> {
        let generic_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![generic("T")],
            return_type: Some(Box::new(generic("T"))),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![i32()],
            return_type: Some(Box::new(generic("T"))),
        });

        let result = resolve_generic_types_from_effective_type(&generic_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);
        Ok(())
    }

    fn generic(name: &str) -> ASTType {
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
            index: ASTIndex::none(),
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
            &BackendNasm386::new(HashSet::new(), HashSet::new(), false),
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
