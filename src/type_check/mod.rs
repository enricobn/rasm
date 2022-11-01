use linked_hash_map::LinkedHashMap;
use log::{debug, info};
use std::fmt::{Display, Formatter};

use crate::codegen::backend::Backend;
use crate::codegen::{EnhancedASTModule, ValContext, ValKind};
use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
use crate::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTParameterDef,
    ASTType, ASTTypeRef, BuiltinTypeKind,
};
use crate::parser::ast::{ASTStatement, MyToString};
use crate::type_check::typed_ast::{convert_to_typed_module, ASTTypedModule};
use crate::type_check::typed_context::TypeConversionContext;
use crate::{debug_i, dedent, indent};

pub mod typed_ast;
pub mod typed_context;

#[derive(Debug)]
pub struct TypeCheckError {
    pub message: String,
}

impl Display for TypeCheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

pub fn convert(
    backend: &dyn Backend,
    module: &EnhancedASTModule,
    debug_asm: bool,
    print_allocation: bool,
    print_module: bool,
) -> ASTTypedModule {
    //unsafe {
    crate::utils::debug_indent::INDENT.with(|indent| {
        *indent.borrow_mut() = 0;
    });
    //}

    let mut body = module.body.clone();

    let mut context = ValContext::new(None);

    let mut type_conversion_context = TypeConversionContext::new();

    let mut something_to_convert = true;

    let mut count = 0;

    while something_to_convert {
        count += 1;

        if count > 100 {
            panic!();
        }

        debug_i!("convert loop {count}");
        debug_i!("------------------------");

        indent!();

        something_to_convert = false;

        let mut new_body = Vec::new();

        for statement in body.iter() {
            let resolved_param_types = LinkedHashMap::new();

            match statement {
                ASTStatement::Expression(expr) => {
                    if let ASTFunctionCallExpression(call @ ASTFunctionCall { .. }) = expr {
                        let converted_call = convert_call(
                            module,
                            &context,
                            call,
                            &mut type_conversion_context,
                            &resolved_param_types,
                            None,
                        );

                        match converted_call {
                            Ok(Some(new_call)) => {
                                something_to_convert = true;
                                debug_i!(
                                    "converted call {} in {}",
                                    call.function_name,
                                    new_call.function_name
                                );
                                new_body.push(ASTStatement::Expression(ASTFunctionCallExpression(
                                    new_call,
                                )))
                            }
                            Ok(None) => new_body.push(statement.clone()),
                            Err(e) => {
                                panic!("{e}");
                            }
                        }
                    } else {
                        panic!("unsupported {statement}");
                    }
                }
                ASTStatement::LetStatement(name, expr) => {
                    if let ASTFunctionCallExpression(call @ ASTFunctionCall { .. }) = expr {
                        let converted_call = convert_call(
                            module,
                            &context,
                            call,
                            &mut type_conversion_context,
                            &resolved_param_types,
                            None,
                        );

                        match converted_call {
                            Ok(Some(new_call)) => {
                                something_to_convert = true;
                                debug_i!(
                                    "converted call {} in {}",
                                    call.function_name,
                                    new_call.function_name
                                );

                                let type_ref = type_conversion_context
                                    .get(&new_call.function_name)
                                    .unwrap()
                                    .return_type
                                    .clone()
                                    .unwrap();

                                context.insert_let(name.clone(), type_ref);

                                new_body.push(ASTStatement::LetStatement(
                                    name.clone(),
                                    ASTFunctionCallExpression(new_call),
                                ))
                            }
                            Ok(None) => {
                                let type_ref = type_conversion_context
                                    .get(&call.function_name)
                                    .unwrap_or_else(|| panic!("{}", &call.function_name))
                                    .return_type
                                    .clone()
                                    .unwrap();
                                context.insert_let(name.clone(), type_ref);
                                new_body.push(statement.clone())
                            }
                            Err(e) => {
                                panic!("{e}");
                            }
                        }
                    } else {
                        panic!("unsupported {statement}")
                    }
                }
            }
        }

        body = new_body.clone();

        let len_before = type_conversion_context.len();

        for function_def in type_conversion_context.clone().iter() {
            debug_i!("converting function {}", function_def);
            indent!();

            let mut context = ValContext::new(None);

            for par in function_def.parameters.iter() {
                //debug_i!("inserting par {} in context", par.name);
                context.insert_par(par.name.clone(), par.clone());
            }

            match &function_def.body {
                ASTFunctionBody::RASMBody(body) => {
                    let mut new_function_def = function_def.clone();

                    new_function_def.body = ASTFunctionBody::RASMBody(
                        body.iter()
                            .enumerate()
                            .map(|(index, it)| {
                                //println!("statement {it}");
                                let converted_statement = convert_statement_in_body(
                                    module,
                                    it,
                                    &mut context,
                                    &mut type_conversion_context,
                                    &LinkedHashMap::new(),
                                );

                                let new_statement = match converted_statement {
                                    Ok(Some(new_expr)) => {
                                        debug_i!("converted expr {}", new_expr);
                                        something_to_convert = true;
                                        new_expr
                                    }
                                    Ok(None) => {
                                        if index == body.len() - 1 {
                                            if let Ok(Some(new_expr)) =
                                                convert_last_statement_in_body(
                                                    module,
                                                    it,
                                                    &context,
                                                    &mut type_conversion_context,
                                                    &LinkedHashMap::new(),
                                                    new_function_def.return_type.clone(),
                                                )
                                            {
                                                debug_i!(
                                                    "converted last expr in body {}",
                                                    new_expr
                                                );
                                                new_expr
                                            } else {
                                                it.clone()
                                            }
                                        } else {
                                            it.clone()
                                        }
                                    }
                                    Err(e) => {
                                        panic!("Error converting {it} in {function_def} : {e}");
                                    }
                                };

                                match &new_statement {
                                    ASTStatement::Expression(_) => {}
                                    ASTStatement::LetStatement(name, expr) => match expr {
                                        ASTFunctionCallExpression(call) => {
                                            let type_ref = type_conversion_context
                                                .get(&call.function_name)
                                                .unwrap()
                                                .return_type
                                                .clone()
                                                .unwrap();
                                            context.insert_let(name.clone(), type_ref);
                                        }
                                        _ => {
                                            panic!("unsupported let value {expr}")
                                        }
                                    },
                                }

                                new_statement
                            })
                            .collect(),
                    );

                    type_conversion_context.replace_body(&new_function_def);
                }
                ASTFunctionBody::ASMBody(body) => {
                    backend
                        .called_functions(body)
                        .iter()
                        .for_each(|function_name| {
                            let function_call = ASTFunctionCall {
                                original_function_name: function_name.clone(),
                                function_name: function_name.clone(),
                                parameters: Vec::new(),
                            };

                            if let Ok(Some(_call)) = convert_call(
                                module,
                                &context,
                                &function_call,
                                &mut type_conversion_context,
                                &LinkedHashMap::new(),
                                None,
                            ) {
                                something_to_convert = true;
                            }
                        });
                }
            }

            dedent!();
        }

        dedent!();

        if len_before != type_conversion_context.len() && !something_to_convert {
            panic!();
        }
    }

    let typed_module = convert_to_typed_module(
        module,
        body,
        &mut type_conversion_context,
        debug_asm,
        print_allocation,
        print_module,
    );

    info!("Type check ended ({count} passes)");
    typed_module
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
            BuiltinTypeKind::ASTString => {
                vec![]
            }
            BuiltinTypeKind::ASTI32 => {
                vec![]
            }
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => {
                let mut par_types: Vec<String> = parameters
                    .iter()
                    .flat_map(|it| get_generic_types(&it.ast_type))
                    .collect();
                if let Some(rt) = return_type {
                    par_types.append(&mut get_generic_types(&rt.as_ref().ast_type));
                }
                par_types.sort();
                par_types.dedup();
                par_types
            }
        },
        ASTType::Parametric(p) => {
            vec![p.into()]
        }
        ASTType::Custom {
            name: _,
            param_types: pt,
        } => {
            let mut result: Vec<String> = pt
                .iter()
                .flat_map(|it| match it.clone().ast_type {
                    ASTType::Parametric(name) => {
                        vec![name]
                    }
                    _ => get_generic_types(&it.ast_type),
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
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
) -> Result<Option<ASTStatement>, TypeCheckError> {
    match statement {
        ASTStatement::Expression(e) => {
            convert_expr_in_body(module, e, context, typed_context, resolved_param_types)
                .map(|ito| ito.map(ASTStatement::Expression))
        }
        ASTStatement::LetStatement(name, e) => {
            convert_expr_in_body(module, e, context, typed_context, resolved_param_types)
                .map(|ito| ito.map(|it| ASTStatement::LetStatement(name.clone(), it)))
        }
    }
}

fn convert_expr_in_body(
    module: &EnhancedASTModule,
    expr: &ASTExpression,
    context: &ValContext,
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
) -> Result<Option<ASTExpression>, TypeCheckError> {
    debug_i!("converting expr in body {expr}");

    indent!();

    let result = match expr {
        ASTFunctionCallExpression(call) => convert_call(
            module,
            context,
            call,
            typed_context,
            resolved_param_types,
            None,
        )?
        .map(ASTFunctionCallExpression),
        ASTExpression::StringLiteral(_) => None,
        ASTExpression::Val(_) => {
            /*
            if let Some(kind) = context.get(p) {
                match kind {
                    VarKind::ParameterRef(i, par) => {

                      todo!()
                    }
                }
            } else {
                todo!()
            }

             */
            None
        }
        ASTExpression::Number(_) => None,
        ASTExpression::Lambda(_) => None,
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
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
    return_type: Option<ASTTypeRef>,
) -> Result<Option<ASTStatement>, TypeCheckError> {
    match statement {
        ASTStatement::Expression(e) => convert_last_expr_in_body(
            module,
            e,
            context,
            typed_context,
            resolved_param_types,
            return_type,
        )
        .map(|ito| ito.map(ASTStatement::Expression)),
        ASTStatement::LetStatement(name, e) => convert_last_expr_in_body(
            module,
            e,
            context,
            typed_context,
            resolved_param_types,
            return_type,
        )
        .map(|ito| ito.map(|it| ASTStatement::LetStatement(name.clone(), it))),
    }
}

fn convert_last_expr_in_body(
    module: &EnhancedASTModule,
    expr: &ASTExpression,
    context: &ValContext,
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
    return_type: Option<ASTTypeRef>,
) -> Result<Option<ASTExpression>, TypeCheckError> {
    debug_i!("converting last expr in body {expr}");

    indent!();

    let result = match expr {
        ASTFunctionCallExpression(call) => {
            //extract_generic_types_from_effective_type()

            if let Some(ASTTypeRef {
                ast_ref: _,
                ast_type,
            }) = &return_type
            {
                if get_generic_types(ast_type).is_empty() {
                    let result = convert_call(
                        module,
                        context,
                        call,
                        typed_context,
                        resolved_param_types,
                        Some(return_type),
                    )?
                    .map(ASTFunctionCallExpression);

                    debug_i!("converted call {:?}", result);
                    dedent!();
                    return Ok(result);
                }
            }

            None
        }
        ASTExpression::StringLiteral(_) => None,
        ASTExpression::Val(_) => {
            /*
            if let Some(kind) = context.get(p) {
                match kind {
                    VarKind::ParameterRef(i, par) => {

                      todo!()
                    }
                }
            } else {
                todo!()
            }

             */
            None
        }
        ASTExpression::Number(_) => None,
        ASTExpression::Lambda(_) => None,
    };

    dedent!();
    Ok(result)
}

fn convert_call(
    module: &EnhancedASTModule,
    context: &ValContext,
    call: &ASTFunctionCall,
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
    expected_return_type: Option<Option<ASTTypeRef>>,
) -> Result<Option<ASTFunctionCall>, TypeCheckError> {
    debug_i!("converting call {}", call);

    indent!();

    if context.get(&call.function_name).is_some() {
        debug_i!("found function in context parameters");
        dedent!();
        return Ok(None);
    }

    //let context = VarContext::new(Some(context));

    let cloned_typed_context = typed_context.clone();

    let mut function_def_from_module = true;

    let function_def = module
        .functions_by_name
        .get(&call.function_name)
        .unwrap_or_else(|| {
            function_def_from_module = false;
            cloned_typed_context
                .get(&call.function_name)
                .unwrap_or_else(|| panic!("function {}", call.function_name))
        });

    debug_i!("function to convert {}", function_def);

    //let mut resolved_param_types = HashMap::new();

    let mut expressions = Vec::new();
    let mut converted_parameters = Vec::new();

    let mut something_converted = false;
    let mut something_to_convert = true;
    let mut call_parameters = call.parameters.clone();
    let mut function_parameters = function_def.parameters.clone();
    let mut count = 0;

    let mut resolved_param_types = resolved_param_types.clone();

    // TODO I don't like count, probably something seems to be converted,but is not...
    while something_to_convert {
        if count > 100 {
            panic!("Count exceeded converting {call}");
        }

        something_to_convert = false;

        expressions.clear();
        converted_parameters.clear();
        for (i, expr) in call_parameters.iter().enumerate() {
            let par = function_parameters.get(i).unwrap_or_else(|| {
                panic!(
                    "Cannot find parameter {i} when calling function {}",
                    call.function_name
                )
            });

            debug_i!("converting parameter {par}");
            debug_i!("resolved generics {:?}", resolved_param_types);

            /*
            if get_generic_types(&par.type_ref.ast_type).is_empty() {
                converted_parameters.push(par.clone());
                expressions.push(expr.clone());
                continue;
            }

             */

            match expr {
                ASTExpression::StringLiteral(_) => {
                    debug_i!("calling update for StringLiteral");

                    something_to_convert = update(
                        ASTType::Builtin(BuiltinTypeKind::ASTString),
                        expr.clone(),
                        par,
                        &mut resolved_param_types,
                        &mut converted_parameters,
                        &mut expressions,
                    )? || something_to_convert;
                }
                ASTFunctionCallExpression(call) => {
                    if let Some(ast_function_call) = convert_call(
                        module,
                        context,
                        call,
                        typed_context,
                        &LinkedHashMap::new(),
                        None,
                    )? {
                        something_to_convert = true;
                        //info!("new_function_defs {:?} used_untyped_function_defs {:?}", new_function_defs, used_untyped_function_defs);

                        let inner_function_def = typed_context
                            .get(&ast_function_call.function_name)
                            .unwrap_or_else(|| {
                                panic!("Cannot find function {}", ast_function_call.function_name)
                            });

                        if let Some(rt) = &inner_function_def.return_type {
                            // the generic types of the inner function are not the same of the this function
                            let result_type = if get_generic_types(&rt.ast_type).is_empty() {
                                inner_function_def.return_type.clone().unwrap()
                            } else {
                                par.clone().type_ref
                            };
                            debug_i!("calling update for ASTFunctionCallExpression");
                            debug_i!(
                                "expression {}",
                                ASTExpression::ASTFunctionCallExpression(ast_function_call.clone())
                            );
                            update(
                                result_type.ast_type,
                                ASTFunctionCallExpression(ast_function_call),
                                par,
                                &mut resolved_param_types,
                                &mut converted_parameters,
                                &mut expressions,
                            )?;
                        } else {
                            debug_i!("calling update for ASTFunctionCallExpression");
                            debug_i!(
                                "expression {}",
                                ASTExpression::ASTFunctionCallExpression(ast_function_call.clone())
                            );

                            if let Some(rt) = &inner_function_def.return_type {
                                update(
                                    rt.clone().ast_type,
                                    ASTFunctionCallExpression(ast_function_call),
                                    par,
                                    &mut resolved_param_types,
                                    &mut converted_parameters,
                                    &mut expressions,
                                )?;
                            } else {
                                converted_parameters.push(par.clone());
                                expressions.push(ASTFunctionCallExpression(ast_function_call));
                            }
                        }
                    } else if !get_generic_types(&par.type_ref.ast_type).is_empty() {
                        if let Some(inner_function_def) = typed_context.get(&call.function_name) {
                            if let Some(rt) = &inner_function_def.return_type {
                                // the generic types of the inner function are not the same of the this function
                                let result_type = if get_generic_types(&rt.ast_type).is_empty() {
                                    inner_function_def.return_type.clone().unwrap()
                                } else {
                                    par.clone().type_ref
                                };
                                debug_i!("calling update for ASTFunctionCallExpression");
                                debug_i!(
                                    "expression {}",
                                    ASTExpression::ASTFunctionCallExpression(call.clone())
                                );
                                something_to_convert = update(
                                    result_type.ast_type,
                                    ASTFunctionCallExpression(call.clone()),
                                    par,
                                    &mut resolved_param_types,
                                    &mut converted_parameters,
                                    &mut expressions,
                                )? || something_to_convert;
                            } else {
                                panic!("A Void result is not supported");
                            }
                        } else {
                            converted_parameters.push(par.clone());
                            expressions.push(expr.clone());
                        }
                    } else {
                        // Above we try to deduce the type of the parameter from the expression, now we try the contrary..
                        let mut converted = false;
                        if let Some(te) =
                            get_type_of_expression(module, context, expr, typed_context)
                        {
                            if !get_generic_types(&te).is_empty() {
                                let map = extract_generic_types_from_effective_type(
                                    &te,
                                    &par.type_ref.ast_type,
                                )?;
                                if !map.is_empty() {
                                    if let Some(new_call) = convert_call(
                                        module,
                                        context,
                                        call,
                                        typed_context,
                                        &map,
                                        None,
                                    )? {
                                        debug_i!("new_call {new_call}");
                                        something_to_convert = true;
                                        converted_parameters.push(par.clone());
                                        expressions.push(ASTFunctionCallExpression(new_call));
                                        converted = true;
                                    }

                                    //todo!("Now we have to substitute {:?} it in the function... ", map);
                                }
                            }
                        }

                        if !converted {
                            converted_parameters.push(par.clone());
                            expressions.push(expr.clone());
                        }
                    }
                }
                ASTExpression::Val(v) => {
                    let result_type = match context.get(v).unwrap_or_else(|| {
                        panic!("cannot find val {v}, actual context {:?}", context.names())
                    }) {
                        ValKind::ParameterRef(_, referenced_parameter_def) => {
                            // TODO the generic types are not the same of those in this function
                            //   so if there's some generic type, I cannot "resolve" the ref
                            let gen_types =
                                get_generic_types(&referenced_parameter_def.type_ref.ast_type);

                            if gen_types.is_empty() {
                                Some(referenced_parameter_def.type_ref.clone())
                            } else {
                                None
                            }
                        }
                        ValKind::LetRef(_, type_ref) => {
                            let gen_types = get_generic_types(&type_ref.ast_type);

                            if gen_types.is_empty() {
                                Some(type_ref.clone())
                            } else {
                                None
                            }
                        }
                    };

                    if let Some(t) = result_type {
                        debug_i!("calling update for Val {v}");
                        something_to_convert = update(
                            t.ast_type,
                            expr.clone(),
                            par,
                            &mut resolved_param_types,
                            &mut converted_parameters,
                            &mut expressions,
                        )? || something_to_convert;
                    } else {
                        converted_parameters.push(par.clone());
                        expressions.push(expr.clone());
                    }
                }
                ASTExpression::Number(_) => {
                    debug_i!("calling update for Number");
                    something_to_convert = update(
                        ASTType::Builtin(BuiltinTypeKind::ASTI32),
                        expr.clone(),
                        par,
                        &mut resolved_param_types,
                        &mut converted_parameters,
                        &mut expressions,
                    )? || something_to_convert;
                }
                ASTExpression::Lambda(lambda) => {
                    let mut effective_lambda = if let Some(new_lambda) = convert_lambda(
                        module,
                        &par.type_ref.ast_type,
                        lambda,
                        context,
                        typed_context,
                        &resolved_param_types,
                    )? {
                        something_to_convert = true;
                        new_lambda
                    } else {
                        lambda.clone()
                    };

                    let new_return_type = match &par.type_ref.ast_type {
                        ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters: _lambda_parameters,
                            return_type, // TODO I cannot convert the return type at this stage
                        }) => {
                            if let Some(rt) = return_type {
                                if let Some(new_t) = substitute(rt, &resolved_param_types) {
                                    something_to_convert = true;
                                    Some(new_t)
                                } else if let Some(last) = effective_lambda.body.last() {
                                    let result_type =
                                        get_type_of_statement(module, context, last, typed_context);

                                    // the generic types of the expression do not belong to this
                                    if result_type
                                        .clone()
                                        .map(|it| get_generic_types(&it).is_empty())
                                        .unwrap_or(true)
                                    {
                                        result_type.map(|it| ASTTypeRef {
                                            ast_ref: par.type_ref.ast_ref,
                                            ast_type: it,
                                        })
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

                    // we try to convert the last expression
                    if let Some(rt) = &new_return_type {
                        let mut context = get_context_from_lambda(
                            context,
                            &effective_lambda,
                            &par.type_ref.ast_type,
                        )?;

                        let new_body: Result<Vec<ASTStatement>, TypeCheckError> = effective_lambda
                            .body
                            .iter()
                            .enumerate()
                            .map(|(i, it)| {
                                //println!("statement {it}");
                                if i == effective_lambda.body.len() - 1 {
                                    if let Some(te) =
                                        get_type_of_statement(module, &context, it, typed_context)
                                    {
                                        let result = extract_generic_types_from_effective_type(
                                            &te,
                                            &rt.ast_type,
                                        )?;

                                        let converted_expr = convert_statement_in_body(
                                            module,
                                            it,
                                            &mut context,
                                            typed_context,
                                            &result,
                                        );

                                        let new_statement = match converted_expr {
                                            Ok(Some(new_expr)) => {
                                                something_converted = true;
                                                new_expr
                                            }
                                            Ok(None) => it.clone(),
                                            Err(e) => panic!("error converting {it}: {e}"),
                                        };

                                        Ok(new_statement)
                                    } else {
                                        Ok(it.clone())
                                    }
                                } else {
                                    match &it {
                                        ASTStatement::Expression(_) => {}
                                        ASTStatement::LetStatement(name, expr) => {
                                            //heprintln!("added let {name}");
                                            match expr {
                                                ASTFunctionCallExpression(call) => {
                                                    let type_ref = typed_context
                                                        .get(&call.function_name)
                                                        .unwrap()
                                                        .return_type
                                                        .clone()
                                                        .unwrap();
                                                    context.insert_let(name.clone(), type_ref);
                                                }
                                                _ => {
                                                    panic!("unsupported let value {expr}")
                                                }
                                            }
                                        }
                                    }

                                    Ok(it.clone())
                                }
                            })
                            .collect();

                        effective_lambda.body = new_body?;
                    }

                    if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters,
                        return_type: _,
                    }) = &par.type_ref.ast_type
                    {
                        let new_parameters: Vec<ASTTypeRef> = parameters
                            .iter()
                            .map(|it| match substitute(it, &resolved_param_types) {
                                None => it.clone(),
                                Some(p) => {
                                    something_converted = true;
                                    p
                                }
                            })
                            .collect();

                        something_to_convert = update(
                            ASTType::Builtin(BuiltinTypeKind::Lambda {
                                return_type: new_return_type.map(Box::new),
                                parameters: new_parameters,
                            }),
                            ASTExpression::Lambda(effective_lambda),
                            par,
                            &mut resolved_param_types,
                            &mut converted_parameters,
                            &mut expressions,
                        )? || something_to_convert;
                    } else {
                        dedent!();
                        return Err(format!(
                            "Expected Lambda but found {}",
                            &par.type_ref.ast_type
                        )
                        .into());
                    }
                }
            }
            //}
        }
        call_parameters = expressions.clone();
        function_parameters = converted_parameters.clone();
        something_converted |= something_to_convert;
        count += 1;
    }

    let mut remaining_generic_types = Vec::new();

    let mut parameters = Vec::new();

    for par in converted_parameters {
        if !get_generic_types(&par.type_ref.ast_type).is_empty() {
            if let Some(new_ref) = substitute(&par.type_ref, &resolved_param_types.clone()) {
                remaining_generic_types.append(&mut get_generic_types(&new_ref.ast_type));
                parameters.push(ASTParameterDef {
                    name: par.name.clone(),
                    type_ref: new_ref,
                });
            } else {
                remaining_generic_types.append(&mut get_generic_types(&par.type_ref.ast_type));
                parameters.push(par);
            }
        } else {
            parameters.push(par);
        }
    }

    let new_return_type = if let Some(er) = expected_return_type {
        something_converted = true;
        er
    } else {
        function_def.return_type.clone().map(|it| {
            let t = if let Some(new_t) = substitute(&it, &resolved_param_types) {
                something_converted = true;
                new_t
            } else {
                it.clone()
            };

            remaining_generic_types.append(&mut get_generic_types(&t.ast_type));
            t
        })
    };

    remaining_generic_types.sort();
    remaining_generic_types.dedup();

    if !remaining_generic_types.is_empty() {
        debug_i!(
            "remaining parametric types for {} {:?}",
            call.function_name,
            remaining_generic_types
        );
    }

    if !something_converted {
        let parameters_to_convert = parameters
            .iter()
            .any(|par| !get_generic_types(&par.type_ref.ast_type).is_empty());

        debug_i!("nothing converted");

        if function_def.param_types.is_empty() {
            debug_i!(
                "TODO check for not parameterized function {}",
                call.function_name
            );

            dedent!();

            return if function_def_from_module {
                if typed_context.add_untyped(function_def) {
                    //dedent!();
                    debug_i!(", but added to untyped");
                    Ok(Some(call.clone()))
                    //return Ok(None);
                } else {
                    if parameters_to_convert {
                        panic!();
                    }
                    //dedent!();
                    Ok(None)
                }
            } else {
                //dedent!();
                if parameters_to_convert {
                    panic!();
                }
                Ok(None)
            };
        }
        if parameters_to_convert {
            //panic!();
        }

        dedent!();
        return Ok(None);
    }

    let new_function_name = format!(
        "{}_{}",
        call.function_name.replace("::", "_"),
        typed_context.len()
    );

    let new_function_def = ASTFunctionDef {
        name: new_function_name,
        parameters,
        return_type: new_return_type,
        body: function_def.body.clone(),
        param_types: remaining_generic_types,
        inline: function_def.inline,
    };

    let effective_function = if let Some(f) =
        typed_context.try_add_new(&call.original_function_name, &new_function_def)
    {
        f
    } else {
        new_function_def
    };

    debug_i!("effective function {}", effective_function);

    dedent!();

    Ok(Some(ASTFunctionCall {
        original_function_name: call.original_function_name.clone(),
        function_name: effective_function.name,
        parameters: expressions,
    }))
}

fn get_type_of_statement(
    module: &EnhancedASTModule,
    context: &ValContext,
    statement: &ASTStatement,
    typed_context: &mut TypeConversionContext,
) -> Option<ASTType> {
    match statement {
        ASTStatement::Expression(e) => get_type_of_expression(module, context, e, typed_context),
        ASTStatement::LetStatement(_, e) => {
            get_type_of_expression(module, context, e, typed_context)
        }
    }
}

fn get_type_of_expression(
    module: &EnhancedASTModule,
    context: &ValContext,
    expr: &ASTExpression,
    typed_context: &mut TypeConversionContext,
) -> Option<ASTType> {
    debug_i!("get_type_of_expression {expr}");
    indent!();

    let result = match expr {
        ASTExpression::StringLiteral(_) => Some(ASTType::Builtin(BuiltinTypeKind::ASTString)),
        ASTFunctionCallExpression(call) => {
            if let Some(function_def) = module
                .functions_by_name
                .get(&call.function_name)
                .or_else(|| typed_context.get(&call.function_name))
            {
                function_def.return_type.clone().map(|it| it.ast_type)
            } else if let Some(ValKind::ParameterRef(_i, par)) = context.get(&call.function_name) {
                if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                    return_type,
                    parameters: _,
                }) = &par.type_ref.ast_type
                {
                    return_type.clone().map(|it| it.ast_type)
                } else {
                    panic!("Expected a lambda");
                }
            } else {
                panic!();
            }
        }
        ASTExpression::Val(v) => {
            if let Some(ValKind::ParameterRef(_i, par)) = context.get(v) {
                Some(par.type_ref.ast_type.clone())
            } else {
                panic!("Unknown val {v}");
            }
        }
        ASTExpression::Number(_) => Some(ASTType::Builtin(BuiltinTypeKind::ASTI32)),
        ASTExpression::Lambda(_) => {
            todo!()
        }
    };

    debug_i!("result {:?}", result);

    dedent!();

    result
}

fn extract_generic_types_from_effective_type(
    parametric_type: &ASTType,
    effective_type: &ASTType,
) -> Result<LinkedHashMap<String, ASTType>, TypeCheckError> {
    debug_i!("extract_generic_types_from_effective_type:");
    indent!();

    debug_i!("parametric_type {}", parametric_type);
    debug_i!("effective_type  {}", effective_type);

    let mut result: LinkedHashMap<String, ASTType> = LinkedHashMap::new();

    if parametric_type == effective_type {
        dedent!();
        return Ok(result);
    }

    match parametric_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::ASTString => {}
            BuiltinTypeKind::ASTI32 => {}
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
                        let inner_result = extract_generic_types_from_effective_type(
                            &p_p.ast_type,
                            &e_p.ast_type,
                        )?;

                        result.extend(inner_result.into_iter());
                    }

                    for p_t in p_return_type {
                        if let Some(e_t) = e_return_type {
                            let inner_result = extract_generic_types_from_effective_type(
                                &p_t.ast_type,
                                &e_t.ast_type,
                            )?;

                            result.extend(inner_result.into_iter());
                        } else {
                            dedent!();
                            if let ASTType::Parametric(p) = &p_t.ast_type {
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
        ASTType::Parametric(p) => {
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
                    let inner_result =
                        extract_generic_types_from_effective_type(&p_p.ast_type, &e_p.ast_type)?;

                    result.extend(inner_result.into_iter());
                }
            }
            ASTType::Parametric(_) => {}
            _ => {
                dedent!();
                return Err(format!("unmatched types {:?}", effective_type).into());
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
) -> Result<ValContext, TypeCheckError> {
    let mut context = ValContext::new(Some(context));

    for (inner_i, name) in lambda.parameter_names.iter().enumerate() {
        match lambda_type {
            ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters,
                return_type: _, // I cannot convert the return type at this stage
            }) => {
                let pp = parameters.get(inner_i).unwrap();

                context.insert_par(
                    name.clone(),
                    ASTParameterDef {
                        name: name.clone(),
                        type_ref: pp.clone(),
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
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
) -> Result<Option<ASTLambdaDef>, TypeCheckError> {
    debug_i!("converting lambda_type {lambda_type}, lambda {lambda}");
    indent!();

    let mut something_converted = false;

    let mut context = ValContext::new(Some(context));

    for (inner_i, name) in lambda.parameter_names.iter().enumerate() {
        match &lambda_type {
            ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters,
                return_type: _, // I cannot convert the return type at this stage
            }) => {
                let pp = parameters.get(inner_i).unwrap();

                if let Some(new_t) = substitute(pp, resolved_param_types) {
                    something_converted = true;

                    context.insert_par(
                        name.clone(),
                        ASTParameterDef {
                            name: name.clone(),
                            type_ref: new_t,
                        },
                    );
                } else {
                    context.insert_par(
                        name.clone(),
                        ASTParameterDef {
                            name: name.clone(),
                            type_ref: pp.clone(),
                        },
                    );
                }
            }
            _ => {
                dedent!();
                return Err(format!("expected lambda but got {lambda_type}").into());
            }
        }
    }

    let new_body: Result<Vec<ASTStatement>, TypeCheckError> = lambda
        .body
        .iter()
        .map(|it| {
            let converted_expr = convert_statement_in_body(
                module,
                it,
                &mut context,
                typed_context,
                &LinkedHashMap::new(),
            );

            let new_statement = match converted_expr {
                Ok(Some(new_expr)) => {
                    something_converted = true;
                    new_expr
                }
                Ok(None) => it.clone(),
                Err(e) => panic!("error converting {it}: {e}"),
            };

            match &new_statement {
                ASTStatement::Expression(_) => {}
                ASTStatement::LetStatement(name, expr) => match expr {
                    ASTFunctionCallExpression(call) => {
                        let type_ref = typed_context
                            .get(&call.function_name)
                            .unwrap()
                            .return_type
                            .clone()
                            .unwrap();
                        context.insert_let(name.clone(), type_ref);
                    }
                    _ => {
                        panic!("unsupported let value {expr}")
                    }
                },
            }

            Ok(new_statement)
        })
        .collect();

    let result = if something_converted {
        debug_i!("something converted");
        Some(ASTLambdaDef {
            body: new_body?,
            parameter_names: lambda.parameter_names.clone(),
        })
    } else {
        debug_i!("nothing converted");
        None
    };

    dedent!();

    Ok(result)
}

fn substitute(
    ast_type: &ASTTypeRef,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
) -> Option<ASTTypeRef> {
    let new_ast_type = substitute_type(&ast_type.ast_type, resolved_param_types);

    new_ast_type.map(|it| ASTTypeRef {
        ast_type: it,
        ast_ref: ast_type.ast_ref,
    })
}

fn substitute_type(
    ast_type: &ASTType,
    resolved_param_types: &LinkedHashMap<String, ASTType>,
) -> Option<ASTType> {
    debug_i!("substitute {ast_type} {:?}", resolved_param_types);
    indent!();

    let result = match &ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => {
                let mut something_substituted = false;
                let new_parameters = match substitute_type_refs(parameters, resolved_param_types) {
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
        ASTType::Parametric(p) => {
            if resolved_param_types.contains_key(p) {
                resolved_param_types.get(p).cloned()
            } else {
                None
            }
        }
        ASTType::Custom { name, param_types } => {
            substitute_type_refs(param_types, resolved_param_types).map(|new_param_types| {
                ASTType::Custom {
                    name: name.clone(),
                    param_types: new_param_types,
                }
            })
        }
    };

    if let Some(r) = &result {
        debug_i!("result {r}");
    } else {
        debug_i!("no result found");
    }
    dedent!();
    result
}

fn substitute_type_refs(
    types: &[ASTTypeRef],
    resolved_param_types: &LinkedHashMap<String, ASTType>,
) -> Option<Vec<ASTTypeRef>> {
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
    result_type: ASTType,
    expr: ASTExpression,
    par: &ASTParameterDef,
    resolved_param_types: &mut LinkedHashMap<String, ASTType>,
    parameters: &mut Vec<ASTParameterDef>,
    expressions: &mut Vec<ASTExpression>,
) -> Result<bool, TypeCheckError> {
    debug_i!("update:");
    indent!();

    debug_i!("par.type {}", par.type_ref.ast_type);
    debug_i!("result_type: {}", result_type);

    let generic_types_from_effective_type =
        extract_generic_types_from_effective_type(&par.type_ref.ast_type, &result_type)?;

    let result = if generic_types_from_effective_type.is_empty() {
        expressions.push(expr);
        parameters.push(par.clone());
        false
    } else {
        resolved_param_types.extend(generic_types_from_effective_type);

        if let Some(t) = substitute_type(&par.type_ref.ast_type, resolved_param_types) {
            expressions.push(expr);

            debug_i!("converted type {}", t);

            parameters.push(ASTParameterDef {
                name: par.name.clone(),
                type_ref: ASTTypeRef {
                    ast_type: t,
                    ast_ref: par.type_ref.ast_ref,
                },
            });
            true
        } else {
            expressions.push(expr);
            parameters.push(par.clone());
            false
        }
    };

    dedent!();
    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::codegen::backend::BackendAsm386;
    use linked_hash_map::LinkedHashMap;
    use std::collections::HashSet;

    use crate::codegen::EnhancedASTModule;

    use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule,
        ASTParameterDef, ASTStatement, ASTType, ASTTypeRef, BuiltinTypeKind,
    };
    use crate::type_check::{convert, extract_generic_types_from_effective_type, TypeCheckError};

    #[test]
    fn test_extract_generic_types_from_effective_type_simple() -> Result<(), TypeCheckError> {
        let parametric_type = parametric("T");
        let effective_type = i32();
        let result = extract_generic_types_from_effective_type(&parametric_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_custom() -> Result<(), TypeCheckError> {
        let parametric_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![parametric_ref("T")],
        };
        let effective_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![i32_ref()],
        };

        let result = extract_generic_types_from_effective_type(&parametric_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda() -> Result<(), TypeCheckError> {
        let parametric_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![parametric_ref("T")],
            return_type: Some(Box::new(parametric_ref("T"))),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![parametric_ref("T")],
            return_type: Some(Box::new(i32_ref())),
        });

        let result = extract_generic_types_from_effective_type(&parametric_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);

        Ok(())
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda1() -> Result<(), TypeCheckError> {
        let parametric_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![parametric_ref("T")],
            return_type: Some(Box::new(parametric_ref("T"))),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![i32_ref()],
            return_type: Some(Box::new(parametric_ref("T"))),
        });

        let result = extract_generic_types_from_effective_type(&parametric_type, &effective_type)?;

        let mut expected_result = LinkedHashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);
        Ok(())
    }

    fn parametric_ref(name: &str) -> ASTTypeRef {
        ASTTypeRef {
            ast_ref: false,
            ast_type: parametric(name),
        }
    }

    fn parametric(name: &str) -> ASTType {
        ASTType::Parametric(name.into())
    }

    fn i32_ref() -> ASTTypeRef {
        ASTTypeRef {
            ast_ref: false,
            ast_type: i32(),
        }
    }

    fn i32() -> ASTType {
        ASTType::Builtin(BuiltinTypeKind::ASTI32)
    }

    #[test]
    fn test() {
        let parameter = ASTExpression::Number(10);

        let call = ASTStatement::Expression(ASTFunctionCallExpression(ASTFunctionCall {
            original_function_name: "consume".into(),
            function_name: "consume".into(),
            parameters: vec![parameter],
        }));

        let function_def = ASTFunctionDef {
            name: "consume".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: vec![ASTParameterDef {
                name: "v".into(),
                type_ref: ASTTypeRef::parametric("T", false),
            }],
            inline: false,
            return_type: None,
            param_types: vec!["T".into()],
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

        let new_module = convert(
            &BackendAsm386::new(HashSet::new(), HashSet::new()),
            &EnhancedASTModule::new(&module),
            false,
            false,
            false,
        );

        let par = if let Some(ASTStatement::Expression(ASTFunctionCallExpression(e))) =
            module.body.get(0)
        {
            Some(e)
        } else {
            None
        };

        assert_eq!(par.unwrap().function_name, "consume");
        assert!(new_module.functions_by_name.get("consume_0").is_some());
    }
}
