use crate::codegen::{EnhancedASTModule, VarContext, VarKind};
use crate::parser::ast::MyToString;
use crate::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTParameterDef,
    ASTType, ASTTypeRef, BuiltinTypeKind,
};
use crate::parser::Parser;
use crate::type_check2::typed_ast::{convert_to_typed_module, ASTTypedModule};
use crate::type_check2::typed_context::TypeConversionContext;
use linked_hash_map::LinkedHashMap;
use log::{debug, info};
use std::collections::HashMap;

pub mod typed_ast;
pub mod typed_context;

// indent works only if you run one single test (you may even find a capacity overflowthread error)
static mut ENABLE_INDENT: bool = false;
static mut INDENT: usize = 0;

macro_rules! debug_i {
    ($ ( $ a: expr), *) => {
        unsafe {
            let s = if !ENABLE_INDENT || INDENT == 0 {
                "".into()
            } else {
                "|  ".repeat(INDENT)
            };
            debug ! ("{}{}", s, & format ! ( $( $ a), * ));
        }
    };
}

macro_rules! indent {
    () => {
        unsafe {
            if ENABLE_INDENT {
                INDENT += 1;
            }
        }
    };
}

macro_rules! dedent {
    () => {
        unsafe {
            if ENABLE_INDENT {
                INDENT -= 1;
            }
        }
    };
}

pub fn convert(module: &EnhancedASTModule) -> ASTTypedModule {
    unsafe {
        INDENT = 0;
    }

    let mut body = module.body.clone();

    let context = VarContext::new(None);

    let mut type_conversion_context = TypeConversionContext::new();

    let mut something_to_convert = true;

    let mut count = 0;

    while something_to_convert && count < 100 {
        count += 1;

        debug_i!("convert loop {count}");
        debug_i!("------------------------");

        indent!();

        something_to_convert = false;

        let mut new_body = Vec::new();

        for call in body.iter() {
            let mut resolved_param_types = HashMap::new();

            if let Some(new_call) = convert_call(
                module,
                &context,
                call,
                &mut type_conversion_context,
                &mut resolved_param_types,
            ) {
                something_to_convert = true;
                debug_i!(
                    "converted call {} in {}",
                    call.function_name,
                    new_call.function_name
                );

                new_body.push(new_call)
            } else {
                new_body.push(call.clone());
            }
        }

        body = new_body;

        for function_def in type_conversion_context.clone().iter() {
            debug_i!("converting function {}", function_def.name);

            let mut resolved_param_types = HashMap::new();

            let mut context = VarContext::new(None);

            for (i, par) in function_def.parameters.iter().enumerate() {
                debug_i!("inserting par {} in context", par.name);
                context.insert(par.name.clone(), VarKind::ParameterRef(i, par.clone()));
            }

            match &function_def.body {
                ASTFunctionBody::RASMBody(body) => {
                    let mut new_function_def = function_def.clone();

                    new_function_def.body = ASTFunctionBody::RASMBody(
                        body.iter()
                            .map(|it| {
                                if let Some(new_expr) = convert_expr(
                                    module,
                                    it,
                                    &context,
                                    &mut type_conversion_context,
                                    &mut resolved_param_types,
                                ) {
                                    debug_i!("converted expr {}", new_expr);
                                    something_to_convert = true;
                                    new_expr
                                } else {
                                    it.clone()
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
        }
        dedent!();
    }

    let mut copy_for_print = module.clone();

    let mut functions_by_name = LinkedHashMap::new();

    for new_function_def in type_conversion_context.iter() {
        functions_by_name.insert(new_function_def.name.clone(), new_function_def);
    }

    copy_for_print.functions_by_name = functions_by_name;

    Parser::print_enhanced(&copy_for_print);

    convert_to_typed_module(module, body, &mut type_conversion_context)
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

fn convert_expr(
    module: &EnhancedASTModule,
    expr: &ASTExpression,
    context: &VarContext,
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &mut HashMap<String, ASTType>,
) -> Option<ASTExpression> {
    debug_i!("converting expr {expr}");

    indent!();

    let result = match expr {
        ASTExpression::StringLiteral(_) => None,
        ASTExpression::ASTFunctionCallExpression(call) => {
            convert_call(module, context, call, typed_context, resolved_param_types)
                .map(ASTExpression::ASTFunctionCallExpression)
        }
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
    result
}

fn convert_call(
    module: &EnhancedASTModule,
    context: &VarContext,
    call: &ASTFunctionCall,
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &mut HashMap<String, ASTType>,
) -> Option<ASTFunctionCall> {
    debug_i!("converting call {}", call);

    indent!();

    if let Some(p) = context.get(&call.function_name) {
        dedent!();
        return None;
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

    if function_def.param_types.is_empty() {
        debug_i!(
            "TODO check for not parameterized function {}",
            call.function_name
        );

        if function_def_from_module {
            if typed_context.add_untyped(function_def) {
                dedent!();
                return Some(call.clone());
            } else {
                dedent!();
                return None;
            }
        } else {
            dedent!();
            return None;
        }
    }

    debug_i!("function to convert {}", function_def);

    //let mut resolved_param_types = HashMap::new();

    let mut expressions = Vec::new();
    let mut converted_parameters = Vec::new();

    let mut something_converted = false;

    for (i, expr) in call.parameters.iter().enumerate() {
        let par = function_def.parameters.get(i).unwrap_or_else(|| {
            panic!(
                "Cannot find parameter {i} when calling function {}",
                call.function_name
            )
        });

        if get_generic_types(&par.type_ref.ast_type).is_empty() {
            converted_parameters.push(par.clone());
            expressions.push(expr.clone());
            continue;
        }

        match expr {
            ASTExpression::StringLiteral(_) => {
                debug_i!("calling update for StringLiteral");

                something_converted = update(
                    ASTType::Builtin(BuiltinTypeKind::ASTString),
                    expr.clone(),
                    par,
                    resolved_param_types,
                    &mut converted_parameters,
                    &mut expressions,
                ) || something_converted;
            }
            ASTExpression::ASTFunctionCallExpression(call) => {
                if let Some(ast_function_call) =
                    convert_call(module, context, call, typed_context, resolved_param_types)
                {
                    something_converted = true;
                    //info!("new_function_defs {:?} used_untyped_function_defs {:?}", new_function_defs, used_untyped_function_defs);

                    let inner_function_def = typed_context
                        .get(&ast_function_call.function_name)
                        .unwrap_or_else(|| {
                            info!(
                                "{:?}",
                                typed_context
                                    .iter()
                                    .map(|it| it.name)
                                    .collect::<Vec<String>>()
                            );
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
                        );
                    } else {
                        debug_i!("calling update for ASTFunctionCallExpression");
                        debug_i!(
                            "expression {}",
                            ASTExpression::ASTFunctionCallExpression(ast_function_call.clone())
                        );
                        update(
                            inner_function_def.return_type.clone().unwrap().ast_type,
                            ASTExpression::ASTFunctionCallExpression(ast_function_call),
                            par,
                            resolved_param_types,
                            &mut converted_parameters,
                            &mut expressions,
                        );
                    }
                } else {
                    converted_parameters.push(par.clone());
                    expressions.push(expr.clone());
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
                    something_converted = update(
                        t.ast_type,
                        expr.clone(),
                        par,
                        resolved_param_types,
                        &mut converted_parameters,
                        &mut expressions,
                    ) || something_converted;
                } else {
                    converted_parameters.push(par.clone());
                    expressions.push(expr.clone());
                }
            }
            ASTExpression::Number(_) => {
                debug_i!("calling update for Number");
                something_converted = update(
                    ASTType::Builtin(BuiltinTypeKind::ASTI32),
                    expr.clone(),
                    par,
                    resolved_param_types,
                    &mut converted_parameters,
                    &mut expressions,
                ) || something_converted;
            }
            ASTExpression::Lambda(lambda) => {
                let effective_lambda = if let Some(new_lambda) = convert_lambda(
                    module,
                    &par.type_ref.ast_type,
                    lambda,
                    context,
                    typed_context,
                    resolved_param_types,
                ) {
                    new_lambda
                } else {
                    lambda.clone()
                };

                if let Some(last) = effective_lambda.body.last() {
                    /*if let Some(new_expr) =
                    convert_expr(module, last, &context, typed_context, resolved_param_types)
                    {
                        debug_i!("NEW_EXPR {new_expr}");
                    }

                     */

                    if let Some(result_type) =
                        get_type_of_expression(module, context, last, typed_context)
                    {
                        // the generic types of the expression do not belong to this
                        if get_generic_types(&result_type).is_empty() {
                            something_converted = true;

                            debug_i!("got result_type from lambda body {result_type}");

                            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                                parameters,
                                return_type,
                            }) = &par.type_ref.ast_type
                            {
                                update(
                                    ASTType::Builtin(BuiltinTypeKind::Lambda {
                                        return_type: Some(Box::new(ASTTypeRef {
                                            ast_ref: return_type.clone().unwrap().ast_ref,
                                            ast_type: result_type,
                                        })),
                                        parameters: parameters.clone(),
                                    }),
                                    ASTExpression::Lambda(effective_lambda),
                                    par,
                                    resolved_param_types,
                                    &mut converted_parameters,
                                    &mut expressions,
                                );
                            }
                        } else {
                            converted_parameters.push(par.clone());
                            expressions.push(expr.clone());
                        }
                    } else {
                        converted_parameters.push(par.clone());
                        expressions.push(expr.clone());
                    }
                }
            }
        }
        //}
    }

    if !something_converted {
        debug_i!("nothing converted");
        dedent!();
        return None;
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

    let new_function_name = format!("{}_{}", call.function_name, typed_context.len());

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

    Some(ASTFunctionCall {
        original_function_name: call.original_function_name.clone(),
        function_name: effective_function.name,
        parameters: expressions,
    })
}

fn get_type_of_expression(
    module: &EnhancedASTModule,
    context: &VarContext,
    expr: &ASTExpression,
    typed_context: &mut TypeConversionContext,
) -> Option<ASTType> {
    match expr {
        ASTExpression::StringLiteral(_) => Some(ASTType::Builtin(BuiltinTypeKind::ASTString)),
        ASTExpression::ASTFunctionCallExpression(call) => {
            let function_def = module
                .functions_by_name
                .get(&call.function_name)
                .unwrap_or_else(|| {
                    typed_context
                        .get(&call.function_name)
                        .unwrap_or_else(|| panic!("function {}", call.function_name))
                });

            function_def.return_type.clone().map(|it| it.ast_type)
        }
        ASTExpression::Val(v) => {
            if let Some(VarKind::ParameterRef(i, par)) = context.get(v) {
                Some(par.type_ref.ast_type.clone())
            } else {
                panic!();
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
) -> HashMap<String, ASTType> {
    debug_i!("extract_generic_types_from_effective_type:");
    indent!();

    debug_i!("parametric_type {}", parametric_type);
    debug_i!("effective_type  {}", effective_type);

    let mut result: HashMap<String, ASTType> = HashMap::new();

    if parametric_type == effective_type {
        dedent!();
        return result;
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
                        let inner_result =
                            extract_generic_types_from_effective_type(&p_p.ast_type, &e_p.ast_type);

                        result.extend(inner_result.into_iter());
                    }

                    for p_t in p_return_type {
                        let e_t = e_return_type.clone().unwrap();

                        let inner_result =
                            extract_generic_types_from_effective_type(&p_t.ast_type, &e_t.ast_type);

                        result.extend(inner_result.into_iter());
                    }
                }
                _ => {
                    panic!("unmatched types")
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
                    panic!("unmatched custom type name")
                }

                for (i, p_p) in p_param_types.iter().enumerate() {
                    let e_p = e_param_types.get(i).unwrap();
                    let inner_result =
                        extract_generic_types_from_effective_type(&p_p.ast_type, &e_p.ast_type);

                    result.extend(inner_result.into_iter());
                }
            }
            _ => {
                panic!("unmatched types {:?}", effective_type);
            }
        },
    }

    debug_i!("result {}", result.my_to_string());
    dedent!();
    result
}

fn convert_lambda(
    module: &EnhancedASTModule,
    lambda_type: &ASTType,
    lambda: &ASTLambdaDef,
    context: &VarContext,
    typed_context: &mut TypeConversionContext,
    resolved_param_types: &mut HashMap<String, ASTType>,
) -> Option<ASTLambdaDef> {
    debug_i!("converting lambda_type {lambda_type}, lambda {lambda}");
    indent!();

    let mut something_converted = false;

    let mut context = VarContext::new(Some(context));

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
                panic!()
            }
        }
    }

    let new_body: Vec<ASTExpression> = lambda
        .body
        .iter()
        .map(|it| {
            if let Some(new_expr) =
                convert_expr(module, it, &context, typed_context, resolved_param_types)
            {
                something_converted = true;
                new_expr
            } else {
                it.clone()
            }
        })
        .collect();

    let result = if something_converted {
        debug_i!("something converted");
        Some(ASTLambdaDef {
            body: new_body,
            parameter_names: lambda.parameter_names.clone(),
        })
    } else {
        debug_i!("nothing converted");
        None
    };

    dedent!();

    result
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
) -> bool {
    debug_i!("update:");
    indent!();

    debug_i!("par.type {}", par.type_ref.ast_type);
    debug_i!("result_type: {}", result_type);

    let generic_types_from_effective_type =
        extract_generic_types_from_effective_type(&par.type_ref.ast_type, &result_type);

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
    result
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
    use crate::type_check2::typed_ast::ASTTypedModule;
    use crate::type_check2::{convert, extract_generic_types_from_effective_type};
    use std::collections::HashMap;
    use std::path::Path;
    use test_env_log::test;

    #[test]
    fn test_extract_generic_types_from_effective_type_simple() {
        let parametric_type = parametric("T");
        let effective_type = i32();
        let result = extract_generic_types_from_effective_type(&parametric_type, &effective_type);

        let mut expected_result = HashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_custom() {
        let parametric_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![parametric_ref("T")],
        };
        let effective_type = ASTType::Custom {
            name: "List".into(),
            param_types: vec![i32_ref()],
        };

        let result = extract_generic_types_from_effective_type(&parametric_type, &effective_type);

        let mut expected_result = HashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda() {
        let parametric_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![parametric_ref("T")],
            return_type: Some(Box::new(parametric_ref("T"))),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![parametric_ref("T")],
            return_type: Some(Box::new(i32_ref())),
        });

        let result = extract_generic_types_from_effective_type(&parametric_type, &effective_type);

        let mut expected_result = HashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_extract_generic_types_from_effective_type_lambda1() {
        let parametric_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![parametric_ref("T")],
            return_type: Some(Box::new(parametric_ref("T"))),
        });

        let effective_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![i32_ref()],
            return_type: Some(Box::new(parametric_ref("T"))),
        });

        let result = extract_generic_types_from_effective_type(&parametric_type, &effective_type);

        let mut expected_result = HashMap::new();
        expected_result.insert("T".into(), i32());

        assert_eq!(result, expected_result);
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

        let new_module = convert(&EnhancedASTModule::new(&module));

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
        ));

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
