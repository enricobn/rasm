use std::{collections::HashMap, iter::zip};

use crate::{
    codegen::{
        enhanced_module::EnhancedASTModule, statics::Statics, val_context::ValContext, ValKind,
    },
    parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTParameterDef,
        ASTStatement, ASTType, BuiltinTypeKind,
    },
    type_check::type_check_error::TypeCheckError,
    utils::SliceDisplay,
};

use super::{
    functions_container::TypeFilter, resolve_generic_types_from_effective_type,
    resolved_generic_types::ResolvedGenericTypes, substitute,
};

pub struct FunctionTypeChecker<'a> {
    enhanced_ast_module: &'a EnhancedASTModule,
}

impl<'a> FunctionTypeChecker<'a> {
    pub fn new(enhanced_ast_module: &'a EnhancedASTModule) -> Self {
        Self {
            enhanced_ast_module,
        }
    }

    pub fn get_type_map(
        &self,
        function: &ASTFunctionDef,
        statics: &mut Statics,
    ) -> (HashMap<ASTIndex, TypeFilter>, Vec<TypeCheckError>) {
        let mut result = HashMap::new();
        let mut errors = Vec::new();
        let mut val_context = ValContext::new(None);

        /*
        let (mut result, mut return_type, mut errors) = self.get_body_type_map(
            &mut val_context,
            statics,
            &self.enhanced_ast_module.body,
            None,
        );
        */

        for par in &function.parameters {
            // TODO check error
            val_context.insert_par(par.name.clone(), par.clone());
        }

        match &function.body {
            ASTFunctionBody::RASMBody(body) => {
                // TODO return_type
                (result, _, errors) = self.get_body_type_map(
                    &mut val_context,
                    statics,
                    body,
                    Some(&function.return_type),
                );
            }
            ASTFunctionBody::NativeBody(_body) => {}
        }

        (result, errors)
    }

    pub fn get_body_type_map(
        &self,
        val_context: &mut ValContext,
        statics: &mut Statics,
        body: &Vec<ASTStatement>,
        expected_last_statement_type: Option<&ASTType>,
    ) -> (
        HashMap<ASTIndex, TypeFilter>,
        Option<TypeFilter>,
        Vec<TypeCheckError>,
    ) {
        let mut errors = Vec::new();
        let mut result = HashMap::new();
        let mut return_type = None;
        /*
        println!(
            "get_body_type_map expected_last_statement_type {}",
            OptionDisplay(&expected_last_statement_type)
        );
        */

        for (i, statement) in body.iter().enumerate() {
            match statement {
                ASTStatement::Expression(e) => {
                    if i == body.len() - 1 {
                        if let Some(ref elst) = expected_last_statement_type {
                            let (e_result, e_errors) = if !elst.is_unit() {
                                self.get_expr_type_map(e, val_context, statics, Some(elst.clone()))
                            } else {
                                self.get_expr_type_map(e, val_context, statics, None)
                            };

                            result.extend(e_result);
                            errors.extend(e_errors);

                            return_type = result.get(&e.get_index()).cloned();
                        } else {
                            let (e_result, e_errors) =
                                self.get_expr_type_map(e, val_context, statics, None);
                            result.extend(e_result);
                            errors.extend(e_errors);
                        }
                    } else {
                        let (e_result, e_errors) =
                            self.get_expr_type_map(e, val_context, statics, None);
                        result.extend(e_result);
                        errors.extend(e_errors);
                    }
                }
                ASTStatement::LetStatement(key, e, is_const, index) => {
                    let (e_result, e_errors) =
                        self.get_expr_type_map(e, val_context, statics, None);
                    result.extend(e_result);
                    errors.extend(e_errors);

                    if let Some(filter) = result.get(&e.get_index()) {
                        if let TypeFilter::Exact(ast_type) = filter {
                            if *is_const {
                                statics.add_const(key.clone(), ast_type.clone());
                            } else {
                                // TODO error
                                val_context.insert_let(key.clone(), ast_type.clone(), index);
                            }
                        }

                        result.insert(index.clone(), filter.clone());
                    }
                }
            }
        }

        (result, return_type, errors)
    }

    fn get_expr_type_map(
        &self,
        expr: &ASTExpression,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_expression_type: Option<&ASTType>,
    ) -> (HashMap<ASTIndex, TypeFilter>, Vec<TypeCheckError>) {
        let mut errors = Vec::new();
        let mut result = HashMap::new();

        match expr {
            ASTExpression::StringLiteral(_, index) => {
                result.insert(
                    index.clone(),
                    TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::String)),
                );
            }
            ASTExpression::ASTFunctionCallExpression(call) => {
                let (call_result, call_errors) =
                    self.get_call_type_map(call, val_context, statics, expected_expression_type);
                result.extend(call_result);
                errors.extend(call_errors);
            }
            ASTExpression::ValueRef(name, index) => {
                if let Some(kind) = val_context.get(name) {
                    result.insert(index.clone(), TypeFilter::Exact(kind.ast_type()));
                } else if let Some(entry) = statics.get_const(name) {
                    result.insert(index.clone(), TypeFilter::Exact(entry.ast_type.clone()));
                }
            }
            ASTExpression::Value(value_type, index) => {
                result.insert(index.clone(), TypeFilter::Exact(value_type.to_type()));
            }
            ASTExpression::Lambda(lambda) => {
                if let Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                })) = expected_expression_type
                {
                    let mut val_context = ValContext::new(Some(&val_context));

                    for ((name, index), ast_type) in
                        lambda.parameter_names.iter().zip(parameters.iter())
                    {
                        // TODO check error
                        if let Err(e) = val_context.insert_par(
                            name.clone(),
                            ASTParameterDef {
                                name: name.clone(),
                                ast_type: ast_type.clone(),
                                ast_index: index.clone(),
                            },
                        ) {
                            errors.push(TypeCheckError::new(lambda.index.clone(), e, Vec::new()));
                        }
                    }

                    let (body_result, body_return_type, body_errors) = self.get_body_type_map(
                        &mut val_context,
                        statics,
                        &lambda.body,
                        Some(return_type.as_ref()),
                    );

                    result.extend(body_result);
                    errors.extend(body_errors);

                    if let Some(TypeFilter::Exact(brt)) = body_return_type {
                        result.insert(
                            lambda.index.clone(),
                            TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::Lambda {
                                parameters: parameters.clone(),
                                return_type: Box::new(brt),
                            })),
                        );
                    } else {
                        result.insert(
                            lambda.index.clone(),
                            TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::Lambda {
                                parameters: parameters.clone(),
                                return_type: return_type.clone(),
                            })),
                        );
                    }
                } else {
                    result.insert(
                        lambda.index.clone(),
                        TypeFilter::Lambda(lambda.parameter_names.len(), None),
                    );
                }
            }
            ASTExpression::Any(_) => todo!(),
        }

        (result, errors)
    }

    fn get_call_type_map(
        &self,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_expression_type: Option<&ASTType>,
    ) -> (HashMap<ASTIndex, TypeFilter>, Vec<TypeCheckError>) {
        /*
        println!(
            "get_call_type_map {call} expected_expression_type {}",
            OptionDisplay(&expected_expression_type)
        );
        */
        let mut errors = Vec::new();
        let mut result = HashMap::new();

        let lambda_from_context_result = if let Some((lambda_return_type, parameters_types)) =
            val_context.get_lambda(&call.function_name)
        {
            let lrt = lambda_return_type.as_ref().clone();
            let pts = parameters_types.iter().cloned().collect::<Vec<_>>();
            Some((lrt, pts))
        } else {
            None
        };

        if let Some((lambda_return_type, parameters_types)) = lambda_from_context_result {
            for (expr, parameter_type) in zip(call.parameters.iter(), parameters_types) {
                let (expr_result, expr_errors) =
                    self.get_expr_type_map(expr, val_context, statics, Some(&parameter_type));
                result.extend(expr_result);
                errors.extend(expr_errors);
            }

            result.insert(call.index.clone(), TypeFilter::Exact(lambda_return_type));

            return (result, errors);
        }

        let mut first_try_of_map = HashMap::new();

        for e in &call.parameters {
            let (e_result, e_errors) = self.get_expr_type_map(e, val_context, statics, None);
            first_try_of_map.extend(e_result);
        }

        let mut parameter_types_filters = Vec::new();

        for e in &call.parameters {
            if let Some(ast_type) = first_try_of_map.get(&e.get_index()) {
                parameter_types_filters.push(ast_type.clone());
            } else {
                parameter_types_filters.push(TypeFilter::Any);
            }
        }

        if let Ok(mut functions) = self.enhanced_ast_module.find_call_vec(
            call,
            &parameter_types_filters,
            expected_expression_type,
        ) {
            if functions.is_empty() {
                print!(
                    "no functions for {} : {} -> ",
                    call.function_name, call.index
                );
                println!("{}", SliceDisplay(&parameter_types_filters));
                errors.push(TypeCheckError::new(
                    call.index.clone(),
                    format!("no functions for {}", call.function_name),
                    Vec::new(),
                ));
            } else {
                let min_rank = functions
                    .iter()
                    .min_by(|f1, f2| f1.rank.cmp(&f2.rank))
                    .unwrap()
                    .rank;

                functions = functions
                    .into_iter()
                    .filter(|it| it.rank == min_rank)
                    .collect::<Vec<_>>();

                if functions.len() > 1 {
                    print!(
                        "found more than one function for {} : {} -> ",
                        call.function_name, call.index
                    );
                    println!("{}", SliceDisplay(&parameter_types_filters));
                    for fun in functions.iter() {
                        println!("  function {fun}");
                    }

                    errors.push(TypeCheckError::new(
                        call.index.clone(),
                        format!("found more than one function for {}", call.function_name),
                        Vec::new(),
                    ));

                    result.extend(first_try_of_map);
                } else {
                    let found_function = functions.remove(0);

                    let mut resolved_generic_types = ResolvedGenericTypes::new();

                    for (i, parameter) in found_function.parameters.iter().enumerate() {
                        if parameter.ast_type.is_generic() {
                            let calculated_type_filter = parameter_types_filters.get(i).unwrap();

                            let p_errors = self.resolve_type_filter(
                                call.index.clone(),
                                &parameter.ast_type,
                                calculated_type_filter,
                                &mut resolved_generic_types,
                            );

                            errors.extend(p_errors);
                        }
                    }

                    loop {
                        let resolved_generic_types_len = resolved_generic_types.len();

                        for (i, e) in call.parameters.iter().enumerate() {
                            let parameter_type =
                                found_function.parameters.get(i).unwrap().ast_type.clone();
                            let ast_type = substitute(&parameter_type, &resolved_generic_types)
                                .unwrap_or(parameter_type.clone());

                            let (e_result, e_errors) =
                                self.get_expr_type_map(e, val_context, statics, Some(&ast_type));

                            result.extend(e_result);
                            errors.extend(e_errors);

                            if let Some(calculated_type_filter) = result.get(&e.get_index()) {
                                let p_errors = self.resolve_type_filter(
                                    call.index.clone(),
                                    &parameter_type,
                                    calculated_type_filter,
                                    &mut resolved_generic_types,
                                );

                                if !p_errors.is_empty() {
                                    println!("found errors resoving {e} expected expression type {ast_type}:");
                                    for error in p_errors.iter() {
                                        println!("  {error}");
                                    }
                                }

                                errors.extend(p_errors);
                            }
                        }

                        if resolved_generic_types.len() == resolved_generic_types_len {
                            break;
                        }
                    }

                    let mut return_type = if found_function.return_type.is_generic()
                        && resolved_generic_types.len() > 0
                    {
                        if let Some(return_type) =
                            substitute(&found_function.return_type, &resolved_generic_types)
                        {
                            return_type
                        } else {
                            found_function.return_type.clone()
                        }
                    } else {
                        found_function.return_type.clone()
                    };

                    if let Some(ert) = expected_expression_type {
                        if !ert.is_generic() {
                            return_type = ert.clone();
                        }
                    }

                    result.insert(call.index.clone(), TypeFilter::Exact(return_type));
                }
            }
        }
        (result, errors)
    }

    fn resolve_type_filter(
        &self,
        index: ASTIndex,
        generic_type: &ASTType,
        effective_filter: &TypeFilter,
        resolved_generic_types: &mut ResolvedGenericTypes,
    ) -> Vec<TypeCheckError> {
        let mut errors = Vec::new();
        if let TypeFilter::Exact(calculated_type) = effective_filter {
            match resolve_generic_types_from_effective_type(generic_type, calculated_type) {
                Ok(rgt) => {
                    if let Err(e) = resolved_generic_types.extend(rgt) {
                        errors.push(TypeCheckError::new(index, e, Vec::new()));
                    }
                }
                Err(e) => errors.push(e),
            }
        }
        errors
    }
}

#[cfg(test)]
mod tests {
    use std::{
        env,
        path::{Path, PathBuf},
    };

    use crate::{
        codegen::{
            c::options::COptions, compile_target::CompileTarget,
            enhanced_module::EnhancedASTModule, statics::Statics,
        },
        commandline::CommandLineOptions,
        project::RasmProject,
        type_check::function_type_checker::FunctionTypeChecker,
    };

    #[test]
    fn test_breakout_check_functions() {
        let project = RasmProject::new(PathBuf::from("../rasm/resources/examples/breakout"));

        let mut statics = Statics::new();

        let target = CompileTarget::C(COptions::default());

        let (modules, _errors) = project.get_all_modules(
            &mut statics,
            false,
            &target,
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        );

        let (enhanced_ast_module, _errors) =
            EnhancedASTModule::new(modules, &project, &mut statics, &target, false);

        println!(
            "eh_module_functions: {}",
            enhanced_ast_module.functions().len()
        );
        /*
        let default_functions = target.get_default_functions(false);
        let mandatory_functions = target.get_mandatory_functions(&enhanced_ast_module);

        let type_checked_ast = TypeCheck::new(&enhanced_ast_module.body_namespace)
            .type_check(
                &enhanced_ast_module,
                &mut statics,
                default_functions,
                mandatory_functions,
                &target,
                false,
            )
            .unwrap();

        println!("type_checked_ast: {}", type_checked_ast.functions().len());
        */

        let function_type_checker = FunctionTypeChecker::new(&enhanced_ast_module);
        let mut statics = Statics::new();

        for function in project
            .get_module(
                Path::new("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm"),
                &target,
            )
            .unwrap()
            .0
            .functions
            .iter()
        //.filter(|it| it.name == "take")
        {
            //println!("function {}", function.name);
            function_type_checker.get_type_map(function, &mut statics);
        }
    }
}
