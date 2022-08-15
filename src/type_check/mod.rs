use crate::codegen::{EnhancedASTModule, ValContext, VarKind};
use crate::parser::ast::MyToString;
use crate::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTParameterDef,
    ASTType, ASTTypeRef, BuiltinTypeKind,
};
use crate::type_check::typed_ast::{convert_to_typed_module, print_typed_module, ASTTypedModule};
use crate::type_check::typed_context::TypeConversionContext;
use log::debug;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub mod typed_ast;
pub mod typed_context;

static mut ENABLE_INDENT: bool = true;

thread_local! {
    // Could add pub to make it public to whatever Foo already is public to.
    //static mut INDENT: usize = 0;
    static INDENT : RefCell<usize> = RefCell::new(0);
}

macro_rules! debug_i {
    ($ ( $ a: expr), *) => {
        unsafe {
        INDENT.with(|indent| {
            let s = if !ENABLE_INDENT || *indent.borrow() == 0 {
                "".into()
            } else {
                "|  ".repeat(*indent.borrow())
            };
            debug ! ("{}{}", s, & format ! ( $( $ a), * ));
        });
    }
    };
}

macro_rules! indent {
    () => {
        unsafe {
            if ENABLE_INDENT {
                INDENT.with(|indent| {
                    *indent.borrow_mut() += 1;
                });
            }
        }
    };
}

macro_rules! dedent {
    () => {
        unsafe {
            if ENABLE_INDENT {
                INDENT.with(|indent| {
                    *indent.borrow_mut() -= 1;
                });
            }
        }
    };
}

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
        TypeCheckError { message: s.clone() }
    }
}

pub fn convert(module: &EnhancedASTModule, debug_asm: bool, print_allocation: bool) -> ASTTypedModule {
    //unsafe {
    INDENT.with(|indent| {
        *indent.borrow_mut() = 0;
    });
    //}

    let mut body = module.body.clone();

    let context = ValContext::new(None);

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

        for call in body.iter() {
            let mut resolved_param_types = HashMap::new();

            let converted_call = convert_call(
                module,
                &context,
                call,
                &mut type_conversion_context,
                &mut resolved_param_types,
            );

            match converted_call {
                Ok(Some(new_call)) => {
                    something_to_convert = true;
                    debug_i!(
                        "converted call {} in {}",
                        call.function_name,
                        new_call.function_name
                    );
                    new_body.push(new_call)
                }
                Ok(None) => new_body.push(call.clone()),
                Err(e) => {
                    panic!("{e}");
                }
            }
        }

        body = new_body.clone();

        let len_before = type_conversion_context.len();

        for function_def in type_conversion_context.clone().iter() {
            debug_i!("converting function {}", function_def);
            indent!();

            let mut resolved_param_types = HashMap::new();

            let mut context = ValContext::new(None);

            for (i, par) in function_def.parameters.iter().enumerate() {
                //debug_i!("inserting par {} in context", par.name);
                context.insert(par.name.clone(), VarKind::ParameterRef(i, par.clone()));
            }

            match &function_def.body {
                ASTFunctionBody::RASMBody(body) => {
                    let mut new_function_def = function_def.clone();

                    new_function_def.body = ASTFunctionBody::RASMBody(
                        body.iter()
                            .map(|it| {
                                let converted_expr = convert_expr_in_body(
                                    module,
                                    it,
                                    &context,
                                    &mut type_conversion_context,
                                    &mut resolved_param_types,
                                );

                                match converted_expr {
                                    Ok(Some(new_expr)) => {
                                        debug_i!("converted expr {}", new_expr);
                                        something_to_convert = true;
                                        new_expr
                                    }
                                    Ok(None) => it.clone(),
                                    Err(e) => {
                                        panic!("Error converting {it} in {function_def} : {e}");
                                    }
                                }
                            })
                            .collect(),
                    );

                    type_conversion_context.replace_body(&new_function_def);
                }
                ASTFunctionBody::ASMBody(_) => {}
            }

            /*
            for (name, v) in context.clone().iter() {
                match v {
                    VarKind::ParameterRef(i, par_def) => {
                        if let Some(_) = substitute(&par_def.type_ref, &resolved_param_types) {
                            something_to_convert = true;
                            //let new_parameter_def = ASTParameterDef { name: par_def.name.clone(), type_ref: new_type };
                            //context.insert(name.clone(), VarKind::ParameterRef(*i, new_parameter_def));
                            //debug_i!("SUBSTITUTED CONTEXT PAR");
                        }
                    }
                }
            }

             */

            /*
            if let Some(return_type) = &function_def.return_type {
                //let mut new_function_def = function_def.clone();

                if !get_parametric_types(&return_type.ast_type).is_empty() {
                    panic!()
                }
            }

             */
            dedent!();
        }
        dedent!();

        if len_before != type_conversion_context.len() && !something_to_convert {
            panic!();
        }

        /*
        {
            let mut copy_for_print = module.clone();
            copy_for_print.body = body.clone();

            let mut functions_by_name = LinkedHashMap::new();

            for new_function_def in type_conversion_context.iter() {
                functions_by_name.insert(new_function_def.name.clone(), new_function_def);
            }

            copy_for_print.functions_by_name = functions_by_name;

            Parser::print_enhanced(&copy_for_print);
        }

         */

        /*
        something_to_convert = something_to_convert ||
            body.iter().any(|it| type_conversion_context.get(&it.function_name).is_none()) ||
            type_conversion_context.iter().any(|it| {
                !it.param_types.is_empty() || match it.body {
                    ASTFunctionBody::RASMBody(body) => {
                        body.iter().any(|expr| {
                            unknown_function_in_expr(&type_conversion_context, expr)
                        })
                    }
                    ASTFunctionBody::ASMBody(_) => {
                        false
                    }
                }
            });

         */
    }

    /*
    let mut copy_for_print = module.clone();

    let mut functions_by_name = LinkedHashMap::new();

    for new_function_def in type_conversion_context.iter() {
        functions_by_name.insert(new_function_def.name.clone(), new_function_def);
    }

    copy_for_print.functions_by_name = functions_by_name;

    Parser::print_enhanced(&copy_for_print);

     */

    let new_module = convert_to_typed_module(module, body, &mut type_conversion_context,
                                             debug_asm,
                                             print_allocation);

    //print_typed_module(&new_module);

    new_module
}

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

fn convert_expr_in_body(
    module: &EnhancedASTModule,
    expr: &ASTExpression,
    context: &ValContext,
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &mut HashMap<String, ASTType>,
) -> Result<Option<ASTExpression>, TypeCheckError> {
    debug_i!("converting expr {expr}");

    indent!();

    let result = match expr {
        ASTExpression::ASTFunctionCallExpression(call) => {
            convert_call(module, context, call, typed_context, resolved_param_types)?
                .map(ASTExpression::ASTFunctionCallExpression)
        }
        ASTExpression::StringLiteral(_) => None,
        ASTExpression::Val(p) => {
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
    resolved_param_types: &mut HashMap<String, ASTType>,
) -> Result<Option<ASTFunctionCall>, TypeCheckError> {
    debug_i!("converting call {}", call);

    indent!();

    if let Some(p) = context.get(&call.function_name) {
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

    // TODO I don't like count, probably something seems to be converted,but is not...
    while something_to_convert && count < 100 {
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
                        resolved_param_types,
                        &mut converted_parameters,
                        &mut expressions,
                    )? || something_to_convert;
                }
                ASTExpression::ASTFunctionCallExpression(call) => {
                    if let Some(ast_function_call) =
                    convert_call(module, context, call, typed_context, resolved_param_types)?
                    {
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
                                ASTExpression::ASTFunctionCallExpression(ast_function_call),
                                par,
                                resolved_param_types,
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
                                    ASTExpression::ASTFunctionCallExpression(ast_function_call),
                                    par,
                                    resolved_param_types,
                                    &mut converted_parameters,
                                    &mut expressions,
                                )?;
                            } else {
                                converted_parameters.push(par.clone());
                                expressions.push(ASTExpression::ASTFunctionCallExpression(ast_function_call));
                            }
                        }
                    } else if !get_generic_types(&par.type_ref.ast_type).is_empty() {
                        if let Some(inner_function_def) = typed_context.get(&call.function_name) {
                            if let Some(rt) = &inner_function_def.return_type {
                                // the generic types of the inner function are not the same of the this function
                                let result_type = if get_generic_types(&rt.ast_type).is_empty() {
                                    something_to_convert = true;
                                    inner_function_def.return_type.clone().unwrap()
                                } else {
                                    par.clone().type_ref
                                };
                                debug_i!("calling update for ASTFunctionCallExpression");
                                debug_i!(
                                    "expression {}",
                                    ASTExpression::ASTFunctionCallExpression(call.clone())
                                );
                                update(
                                    result_type.ast_type,
                                    ASTExpression::ASTFunctionCallExpression(call.clone()),
                                    par,
                                    resolved_param_types,
                                    &mut converted_parameters,
                                    &mut expressions,
                                )?;
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
                        if let Some(te) = get_type_of_expression(module, context, expr, typed_context) {
                            if !get_generic_types(&te).is_empty() {
                                let mut map = extract_generic_types_from_effective_type(&te, &par.type_ref.ast_type)?;
                                if !map.is_empty() {
                                    /*
                                    let inner_function_def = typed_context
                                        .get(&call.function_name)
                                        .unwrap_or_else(|| {
                                            module.functions_by_name.get(&call.original_function_name)
                                                .unwrap_or_else(|| panic!("Cannot find function {}", call.original_function_name))
                                        });

                                     */

                                    if let Some(new_call) = convert_call(module, context, call, typed_context, &mut map)? {
                                        debug_i!("new_call {new_call}");
                                        something_to_convert = true;
                                        converted_parameters.push(par.clone());
                                        expressions.push(ASTExpression::ASTFunctionCallExpression(new_call));
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
                        VarKind::ParameterRef(_, referenced_parameter_def) => {
                            // TODO the generic types are not the same of those in this function
                            //   so if there's some generic type, I cannot "resolve" the ref
                            let gen_types =
                                get_generic_types(&referenced_parameter_def.type_ref.ast_type);

                            if gen_types.is_empty() {
                                Some(referenced_parameter_def.type_ref.clone())
                            } else {
                                None
                                /*
                                let mut generics_map = HashMap::new();

                                for (i, gt) in gen_types.iter().enumerate() {
                                    let ast_type = ASTType::Parametric(format!("{gt}_{}", i));
                                    generics_map.insert(gt.into(), ast_type);
                                }

                                substitute(&referenced_parameter_def.type_ref, &generics_map).unwrap()

                                 */
                            }
                        }
                    };

                    if let Some(t) = result_type {
                        debug_i!("calling update for Val {v}");
                        something_to_convert = update(
                            t.ast_type,
                            expr.clone(),
                            par,
                            resolved_param_types,
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
                        resolved_param_types,
                        &mut converted_parameters,
                        &mut expressions,
                    )? || something_to_convert;
                }
                ASTExpression::Lambda(lambda) => {
                    let effective_lambda = if let Some(new_lambda) = convert_lambda(
                        module,
                        &par.type_ref.ast_type,
                        lambda,
                        context,
                        typed_context,
                        resolved_param_types,
                    )? {
                        something_to_convert = true;
                        new_lambda
                    } else {
                        lambda.clone()
                    };

                    if let Some(last) = effective_lambda.body.last() {
                        let result_type =
                            get_type_of_expression(module, context, last, typed_context);

                        {
                            // the generic types of the expression do not belong to this
                            if result_type
                                .clone()
                                .map(|it| get_generic_types(&it).is_empty())
                                .unwrap_or(true)
                            {
                                debug_i!("got result_type from lambda body {:?}", &result_type);

                                if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                                                            parameters,
                                                            return_type,
                                                        }) = &par.type_ref.ast_type
                                {
                                    if result_type != return_type.clone().map(|it| it.ast_type) {
                                        if let Some(rt) = return_type {
                                            something_to_convert = update(
                                                ASTType::Builtin(BuiltinTypeKind::Lambda {
                                                    return_type: result_type.map(|it| {
                                                        Box::new(ASTTypeRef {
                                                            ast_ref: rt
                                                                .clone()
                                                                .ast_ref,
                                                            ast_type: it,
                                                        })
                                                    }),
                                                    parameters: parameters.clone(),
                                                }),
                                                ASTExpression::Lambda(effective_lambda),
                                                par,
                                                resolved_param_types,
                                                &mut converted_parameters,
                                                &mut expressions,
                                            )? || something_to_convert;
                                        } else {
                                            converted_parameters.push(par.clone());
                                            expressions.push(ASTExpression::Lambda(effective_lambda));
                                        }
                                    } else {
                                        converted_parameters.push(par.clone());
                                        expressions.push(ASTExpression::Lambda(effective_lambda));
                                    }
                                } else {
                                    return Err(format!(
                                        "Expected Lambda but found {}",
                                        &par.type_ref.ast_type
                                    )
                                        .into());
                                }
                            } else {
                                converted_parameters.push(par.clone());
                                expressions.push(ASTExpression::Lambda(effective_lambda));
                            }
                        }
                    } else {
                        converted_parameters.push(par.clone());
                        expressions.push(expr.clone());
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

    /*
    if function_def.param_types.is_empty() {
        debug_i!(
            "TODO check for not parameterized function {}",
            call.function_name
        );

        if function_def_from_module {
            if typed_context.add_untyped(function_def) {
                dedent!();
                return Ok(Some(call.clone()));
            } else {
                dedent!();
                return Ok(None);
            }
        } else {
            dedent!();
            return Ok(None);
        }
    }
     */
    /*
       if let Some(rt) = &function_def.return_type {
           if let Some(new_return_type) = substitute(rt, resolved_param_types) {
               something_converted = true;
           }
       }


    */

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

    let new_return_type = function_def.return_type.clone().map(|it| {
        let t = if let Some(new_t) = substitute(&it, &resolved_param_types) {
            something_converted = true;
            new_t
        } else {
            it.clone()
        };

        remaining_generic_types.append(&mut get_generic_types(&t.ast_type));
        t
    });

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
        dedent!();
        if function_def.param_types.is_empty() {
            debug_i!(
                "TODO check for not parameterized function {}",
                call.function_name
            );

            if function_def_from_module {
                if typed_context.add_untyped(function_def) {
                    //dedent!();
                    debug_i!(", but added to untyped");
                    return Ok(Some(call.clone()));
                    //return Ok(None);
                } else {
                    if parameters_to_convert {
                        panic!();
                    }
                    //dedent!();
                    return Ok(None);
                }
            } else {
                //dedent!();
                if parameters_to_convert {
                    panic!();
                }
                return Ok(None);
            }
        }
        if parameters_to_convert {
            panic!();
        }

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

fn get_type_of_expression(
    module: &EnhancedASTModule,
    context: &ValContext,
    expr: &ASTExpression,
    typed_context: &mut TypeConversionContext,
) -> Option<ASTType> {
    match expr {
        ASTExpression::StringLiteral(_) => Some(ASTType::Builtin(BuiltinTypeKind::ASTString)),
        ASTExpression::ASTFunctionCallExpression(call) => {
            if let Some(function_def) = module
                .functions_by_name
                .get(&call.function_name)
                .or_else(||
                    typed_context
                        .get(&call.function_name)
                ) {
                function_def.return_type.clone().map(|it| it.ast_type)
            } else if let Some(VarKind::ParameterRef(i, par)) = context.get(&call.function_name) {
                Some(par.type_ref.ast_type.clone())
            } else {
                panic!();
            }
        }
        ASTExpression::Val(v) => {
            if let Some(VarKind::ParameterRef(i, par)) = context.get(v) {
                Some(par.type_ref.ast_type.clone())
            } else {
                panic!("Unknown val {v}");
            }
        }
        ASTExpression::Number(_) => Some(ASTType::Builtin(BuiltinTypeKind::ASTI32)),
        ASTExpression::Lambda(_) => {
            todo!()
        }
    }
}

fn extract_generic_types_from_effective_type(
    parametric_type: &ASTType,
    effective_type: &ASTType,
) -> Result<HashMap<String, ASTType>, TypeCheckError> {
    debug_i!("extract_generic_types_from_effective_type:");
    indent!();

    debug_i!("parametric_type {}", parametric_type);
    debug_i!("effective_type  {}", effective_type);

    let mut result: HashMap<String, ASTType> = HashMap::new();

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
                            if let ASTType::Parametric(p) = &p_t.ast_type {
                                return Err(format!("Found parametric type {p} that is (). For now we cannot handle it").into());
                            }
                            return Err("Expected some type but got None".into());
                        }
                    }
                }
                _ => {
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
                    return Err(format!("unmatched custom type name {p_name} {e_name}").into());
                }

                for (i, p_p) in p_param_types.iter().enumerate() {
                    let e_p = e_param_types.get(i).unwrap();
                    let inner_result =
                        extract_generic_types_from_effective_type(&p_p.ast_type, &e_p.ast_type)?;

                    result.extend(inner_result.into_iter());
                }
            }
            _ => {
                return Err(format!("unmatched types {:?}", effective_type).into());
            }
        },
    }

    debug_i!("result {}", result.my_to_string());
    dedent!();
    Ok(result)
}

fn convert_lambda(
    module: &EnhancedASTModule,
    lambda_type: &ASTType,
    lambda: &ASTLambdaDef,
    context: &ValContext,
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &mut HashMap<String, ASTType>,
) -> Result<Option<ASTLambdaDef>, TypeCheckError> {
    debug_i!("converting lambda_type {lambda_type}, lambda {lambda}");
    indent!();

    let mut something_converted = false;

    let mut context = ValContext::new(Some(context));

    for (inner_i, name) in lambda.parameter_names.iter().enumerate() {
        match &lambda_type {
            ASTType::Builtin(BuiltinTypeKind::Lambda {
                                 parameters,
                                 return_type, // TODO I cannot convert the return type at this stage
                             }) => {
                let pp = parameters.get(inner_i).unwrap();

                if let Some(new_t) = substitute(pp, resolved_param_types) {
                    something_converted = true;

                    context.insert(
                        name.clone(),
                        VarKind::ParameterRef(
                            inner_i,
                            ASTParameterDef {
                                name: name.clone(),
                                type_ref: new_t,
                            },
                        ),
                    );
                } else {
                    context.insert(
                        name.clone(),
                        VarKind::ParameterRef(
                            inner_i,
                            ASTParameterDef {
                                name: name.clone(),
                                type_ref: pp.clone(),
                            },
                        ),
                    );
                }
            }
            _ => {
                return Err(format!("expected lambda but got {lambda_type}").into());
            }
        }
    }

    let new_body: Result<Vec<ASTExpression>, TypeCheckError> = lambda
        .body
        .iter()
        .map(|it| {
            let converted_expr =
                convert_expr_in_body(module, it, &context, typed_context, resolved_param_types);

            match converted_expr {
                Ok(Some(new_expr)) => {
                    something_converted = true;
                    Ok(new_expr)
                }
                Ok(None) => Ok(it.clone()),
                Err(e) => Err(e),
            }
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
    resolved_param_types: &HashMap<String, ASTType>,
) -> Option<ASTTypeRef> {
    let new_ast_type = substitute_type(&ast_type.ast_type, resolved_param_types);

    new_ast_type.map(|it| ASTTypeRef {
        ast_type: it,
        ast_ref: ast_type.ast_ref,
    })
}

fn substitute_type(
    ast_type: &ASTType,
    resolved_param_types: &HashMap<String, ASTType>,
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
                let new_parameters = parameters
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
            let mut something_substituted = false;

            let new_param_types = param_types
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
                Some(ASTType::Custom {
                    name: name.clone(),
                    param_types: new_param_types,
                })
            } else {
                None
            }
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

fn update(
    result_type: ASTType,
    expr: ASTExpression,
    par: &ASTParameterDef,
    resolved_param_types: &mut HashMap<String, ASTType>,
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
    use crate::codegen::EnhancedASTModule;
    use crate::lexer::Lexer;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule,
        ASTParameterDef, ASTType, ASTTypeRef, BuiltinTypeKind,
    };
    use crate::parser::Parser;
    use crate::transformations::enum_functions_creator::enum_functions_creator;
    use crate::transformations::struct_functions_creator::struct_functions_creator;
    use crate::type_check::typed_ast::ASTTypedModule;
    use crate::type_check::{convert, extract_generic_types_from_effective_type, TypeCheckError};
    use std::collections::HashMap;
    use std::path::Path;
    use test_env_log::test;

    #[test]
    fn test_extract_generic_types_from_effective_type_simple() -> Result<(), TypeCheckError> {
        let parametric_type = parametric("T");
        let effective_type = i32();
        let result = extract_generic_types_from_effective_type(&parametric_type, &effective_type)?;

        let mut expected_result = HashMap::new();
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

        let mut expected_result = HashMap::new();
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

        let mut expected_result = HashMap::new();
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

        let mut expected_result = HashMap::new();
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

        let call = ASTFunctionCall {
            original_function_name: "consume".into(),
            function_name: "consume".into(),
            parameters: vec![parameter],
        };

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
        };

        let new_module = convert(&EnhancedASTModule::new(&module), false, false);

        assert_eq!(new_module.body.get(0).unwrap().function_name, "consume_0");
        assert!(new_module.functions_by_name.get("consume_0").is_some());

        print(new_module);
    }

    #[test]
    fn test_list() {
        test_file("list.rasm");
    }

    #[test]
    fn test_list_fmap() {
        test_file("list_fmap.rasm");
    }

    #[test]
    fn test_gameoflife() {
        test_file("gameoflife.rasm");
    }

    #[test]
    fn test_structs() {
        test_file("structs.rasm");
    }

    fn test_file(file_name: &str) {
        println!("file_name {file_name}");

        let resource = format!("resources/test/{}", file_name);
        let path = Path::new(&resource);
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

        let backend = BackendAsm386::new();
        let new_module = convert(&struct_functions_creator(
            &backend,
            &enum_functions_creator(&backend, &EnhancedASTModule::new(&module)),
        ), false, false);

        print(new_module);
    }

    fn print(new_module: ASTTypedModule) {
        println!(
            "functions {:?}",
            new_module
                .functions_by_name
                .values()
                .map(|it| &it.name)
                .collect::<Vec<&String>>()
        );

        println!(
            "enums {:?}",
            new_module
                .enums
                .iter()
                .map(|it| &it.name)
                .collect::<Vec<&String>>()
        );

        println!(
            "structs {:?}",
            new_module
                .structs
                .iter()
                .map(|it| &it.name)
                .collect::<Vec<&String>>()
        );
    }
}
