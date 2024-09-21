use std::collections::HashMap;

use log::warn;

use crate::{
    codegen::{enhanced_module::EnhancedASTModule, val_context::ValContext},
    parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTParameterDef,
        ASTStatement, ASTType, BuiltinTypeKind,
    },
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
    pub fn get_type_map(&self, function: &ASTFunctionDef) -> HashMap<ASTIndex, TypeFilter> {
        //let mut result = HashMap::new();
        let mut val_context = ValContext::new(None);

        let mut result = self.get_body_type_map(
            function,
            &mut val_context,
            &self.enhanced_ast_module.body,
            None,
        );

        for par in &function.parameters {
            // TODO check error
            val_context.insert_par(par.name.clone(), par.clone());
        }

        match &function.body {
            ASTFunctionBody::RASMBody(body) => {
                result = self.get_body_type_map(
                    function,
                    &mut val_context,
                    body,
                    Some(function.return_type.clone()),
                );
            }
            ASTFunctionBody::NativeBody(_body) => {}
        }

        result
    }

    pub fn get_body_type_map(
        &self,
        function: &ASTFunctionDef,
        val_context: &mut ValContext,
        body: &Vec<ASTStatement>,
        expected_last_statement_type: Option<ASTType>,
    ) -> HashMap<ASTIndex, TypeFilter> {
        let mut result = HashMap::new();

        for (i, statement) in body.iter().enumerate() {
            match statement {
                ASTStatement::Expression(e) => {
                    if i == body.len() - 1 {
                        if let Some(ref elst) = expected_last_statement_type {
                            if !elst.is_unit() {
                                result.extend(self.get_expr_type_map(
                                    function,
                                    e,
                                    val_context,
                                    Some(elst.clone()),
                                ));
                            } else {
                                result.extend(self.get_expr_type_map(
                                    function,
                                    e,
                                    val_context,
                                    None,
                                ));
                            }
                        } else {
                            result.extend(self.get_expr_type_map(function, e, val_context, None));
                        }
                    } else {
                        result.extend(self.get_expr_type_map(function, e, val_context, None));
                    }
                }
                ASTStatement::LetStatement(key, e, _, index) => {
                    result.extend(self.get_expr_type_map(function, e, val_context, None));
                    if let Some(filter) = result.get(&e.get_index()) {
                        if let TypeFilter::Exact(ast_type) = filter {
                            // TODO error
                            val_context.insert_let(key.clone(), ast_type.clone(), index);
                        }

                        result.insert(index.clone(), filter.clone());
                    }
                }
            }
        }

        result
    }

    fn get_expr_type_map(
        &self,
        function: &ASTFunctionDef,
        expr: &ASTExpression,
        val_context: &mut ValContext,
        expected_expression_type: Option<ASTType>,
    ) -> HashMap<ASTIndex, TypeFilter> {
        let mut result = HashMap::new();

        match expr {
            ASTExpression::StringLiteral(_, index) => {
                result.insert(
                    index.clone(),
                    TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::String)),
                );
            }
            ASTExpression::ASTFunctionCallExpression(call) => {
                result.extend(self.get_call_type_map(
                    function,
                    call,
                    val_context,
                    expected_expression_type,
                ));
            }
            ASTExpression::ValueRef(name, index) => {
                if let Some(kind) = val_context.get(name) {
                    result.insert(index.clone(), TypeFilter::Exact(kind.ast_type()));
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
                        val_context.insert_par(
                            name.clone(),
                            ASTParameterDef {
                                name: name.clone(),
                                ast_type: ast_type.clone(),
                                ast_index: index.clone(),
                            },
                        );
                    }

                    result.extend(self.get_body_type_map(
                        function,
                        &mut val_context,
                        &lambda.body,
                        Some(return_type.as_ref().clone()),
                    ));

                    result.insert(
                        lambda.index.clone(),
                        TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters,
                            return_type,
                        })),
                    );
                } else {
                    result.insert(
                        lambda.index.clone(),
                        TypeFilter::Lambda(lambda.parameter_names.len(), None),
                    );
                }
            }
            ASTExpression::Any(_) => todo!(),
        }

        result
    }

    fn get_call_type_map(
        &self,
        function: &ASTFunctionDef,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        expected_expression_type: Option<ASTType>,
    ) -> HashMap<ASTIndex, TypeFilter> {
        let mut result = HashMap::new();

        let mut first_try_of_map = HashMap::new();

        for e in &call.parameters {
            first_try_of_map.extend(self.get_expr_type_map(function, e, val_context, None));
        }

        let mut parameter_types_filters = Vec::new();

        for e in &call.parameters {
            if let Some(ast_type) = first_try_of_map.get(&e.get_index()) {
                parameter_types_filters.push(ast_type.clone());
            } else {
                parameter_types_filters.push(TypeFilter::Any);
            }
        }

        if let Ok(mut functions) =
            self.enhanced_ast_module
                .find_call_vec(call, &parameter_types_filters, None)
        {
            if functions.is_empty() {
                print!(
                    "no functions for {} : {} -> ",
                    call.function_name, call.index
                );
                println!("{}", SliceDisplay(&parameter_types_filters));
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
                        "more than one function for {} : {} -> ",
                        call.function_name, call.index
                    );
                    println!("{}", SliceDisplay(&parameter_types_filters));
                    for fun in functions.iter() {
                        println!("  function {fun}");
                    }
                } else {
                    let found_function = functions.remove(0);

                    let return_type = if found_function.return_type.is_generic() {
                        // TODO resolve
                        let mut resolved_generic_types = ResolvedGenericTypes::new();
                        for (i, parameter) in found_function.parameters.iter().enumerate() {
                            if parameter.ast_type.is_generic() {
                                let calculated_type_filter =
                                    parameter_types_filters.get(i).unwrap();

                                if let TypeFilter::Exact(calculated_type) = calculated_type_filter {
                                    if let Ok(rgt) = resolve_generic_types_from_effective_type(
                                        &parameter.ast_type,
                                        calculated_type,
                                    ) {
                                        // TODO error
                                        resolved_generic_types.extend(rgt);
                                    }
                                }
                            }
                        }

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

                    result.insert(call.index.clone(), TypeFilter::Exact(return_type));
                }
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use std::{env, path::PathBuf};

    use crate::{
        codegen::{
            c::options::COptions, compile_target::CompileTarget,
            enhanced_module::EnhancedASTModule, statics::Statics,
        },
        commandline::CommandLineOptions,
        new_type_check2::TypeCheck,
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

        let (enhanced_ast_module, errors) =
            EnhancedASTModule::new(modules, &project, &mut statics, &target, false);

        println!(
            "eh_module_functions: {}",
            enhanced_ast_module.functions().len()
        );

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

        let function_type_checker = FunctionTypeChecker {
            enhanced_ast_module: &enhanced_ast_module,
        };

        for function in enhanced_ast_module.functions().iter()
        //.filter(|it| it.name == "take")
        {
            //println!("function {}", function.name);
            function_type_checker.get_type_map(function);
        }
    }
}
