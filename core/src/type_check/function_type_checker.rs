use std::collections::HashMap;

use crate::{
    codegen::{
        enh_ast::{
            EnhASTExpression, EnhASTFunctionBody, EnhASTFunctionCall, EnhASTFunctionDef,
            EnhASTFunctionSignature, EnhASTIndex, EnhASTParameterDef, EnhASTStatement, EnhASTType,
            EnhBuiltinTypeKind,
        },
        enhanced_module::EnhancedASTModule,
        statics::Statics,
        enh_val_context::EnhValContext,
    },
    type_check::type_check_error::TypeCheckError,
    utils::{OptionDisplay, SliceDisplay},
};

use super::{
    functions_container::EnhTypeFilter, resolve_generic_types_from_effective_type,
    resolved_generic_types::ResolvedGenericTypes, substitute,
};

pub struct FunctionTypeCheckerResult {
    map: HashMap<EnhASTIndex, EnhTypeFilter>,
}

impl FunctionTypeCheckerResult {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, index: EnhASTIndex, filter: EnhTypeFilter) {
        self.map.insert(index, filter);
    }

    pub fn get(&self, index: &EnhASTIndex) -> Option<&EnhTypeFilter> {
        self.map.get(index)
    }

    pub fn extend(&mut self, other: FunctionTypeCheckerResult) {
        self.map.extend(other.map);
    }
}

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
        function: &EnhASTFunctionDef,
        statics: &mut Statics,
    ) -> (FunctionTypeCheckerResult, Vec<TypeCheckError>) {
        let mut result = FunctionTypeCheckerResult::new();
        let mut errors = Vec::new();
        let mut val_context = EnhValContext::new(None);

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
            EnhASTFunctionBody::RASMBody(body) => {
                // TODO return_type
                (result, _, errors) = self.get_body_type_map(
                    &mut val_context,
                    statics,
                    body,
                    Some(&function.return_type),
                );
            }
            EnhASTFunctionBody::NativeBody(_body) => {}
        }

        (result, errors)
    }

    pub fn get_body_type_map(
        &self,
        val_context: &mut EnhValContext,
        statics: &mut Statics,
        body: &Vec<EnhASTStatement>,
        expected_last_statement_type: Option<&EnhASTType>,
    ) -> (
        FunctionTypeCheckerResult,
        Option<EnhTypeFilter>,
        Vec<TypeCheckError>,
    ) {
        let mut errors = Vec::new();
        let mut result = FunctionTypeCheckerResult::new();
        let mut return_type = None;
        /*
        println!(
            "get_body_type_map expected_last_statement_type {}",
            OptionDisplay(&expected_last_statement_type)
        );
        */

        for (i, statement) in body.iter().enumerate() {
            match statement {
                EnhASTStatement::Expression(e) => {
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
                EnhASTStatement::LetStatement(key, e, is_const, index) => {
                    let (e_result, e_errors) =
                        self.get_expr_type_map(e, val_context, statics, None);
                    result.extend(e_result);
                    errors.extend(e_errors);

                    if let Some(filter) = result.get(&e.get_index()) {
                        if let EnhTypeFilter::Exact(ast_type) = filter {
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
        expr: &EnhASTExpression,
        val_context: &mut EnhValContext,
        statics: &mut Statics,
        expected_expression_type: Option<&EnhASTType>,
    ) -> (FunctionTypeCheckerResult, Vec<TypeCheckError>) {
        let mut errors = Vec::new();
        let mut result = FunctionTypeCheckerResult::new();

        match expr {
            EnhASTExpression::StringLiteral(_, index) => {
                result.insert(
                    index.clone(),
                    EnhTypeFilter::Exact(EnhASTType::Builtin(EnhBuiltinTypeKind::String)),
                );
            }
            EnhASTExpression::ASTFunctionCallExpression(call) => {
                let (call_result, call_errors) =
                    self.get_call_type_map(call, val_context, statics, expected_expression_type);
                result.extend(call_result);
                errors.extend(call_errors);
            }
            EnhASTExpression::ValueRef(name, index) => {
                if let Some(kind) = val_context.get(name) {
                    result.insert(index.clone(), EnhTypeFilter::Exact(kind.ast_type()));
                } else if let Some(entry) = statics.get_const(name) {
                    result.insert(index.clone(), EnhTypeFilter::Exact(entry.ast_type.clone()));
                }
            }
            EnhASTExpression::Value(value_type, index) => {
                result.insert(index.clone(), EnhTypeFilter::Exact(value_type.to_enh_type()));
            }
            EnhASTExpression::Lambda(lambda) => {
                if let Some(EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                })) = expected_expression_type
                {
                    let mut val_context = EnhValContext::new(Some(&val_context));

                    for ((name, index), ast_type) in
                        lambda.parameter_names.iter().zip(parameters.iter())
                    {
                        // TODO check error
                        if let Err(e) = val_context.insert_par(
                            name.clone(),
                            EnhASTParameterDef {
                                name: name.clone(),
                                ast_type: ast_type.clone(),
                                ast_index: index.clone(),
                            },
                        ) {
                            errors.push(TypeCheckError::new(lambda.index.clone(), e, Vec::new()));
                        } else {
                            result.insert(index.clone(), EnhTypeFilter::Exact(ast_type.clone()));
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

                    if let Some(EnhTypeFilter::Exact(brt)) = body_return_type {
                        result.insert(
                            lambda.index.clone(),
                            EnhTypeFilter::Exact(EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                                parameters: parameters.clone(),
                                return_type: Box::new(brt),
                            })),
                        );
                    } else {
                        result.insert(
                            lambda.index.clone(),
                            EnhTypeFilter::Exact(EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                                parameters: parameters.clone(),
                                return_type: return_type.clone(),
                            })),
                        );
                    }
                } else {
                    result.insert(
                        lambda.index.clone(),
                        EnhTypeFilter::Lambda(lambda.parameter_names.len(), None),
                    );
                }
            }
            EnhASTExpression::Any(_) => todo!(),
        }

        // the resolved type could be generic on a different generic type, we want to resolve it
        // with the generic type of the expected type

        if let Some(eet) = expected_expression_type {
            if eet.is_generic() {
                if let Some(EnhTypeFilter::Exact(et)) = result.get(&expr.get_index()) {
                    if et.is_generic() {
                        if let Ok(rgt) = resolve_generic_types_from_effective_type(et, eet) {
                            if let Some(rt) = substitute(et, &rgt) {
                                println!(
                                    "resolved generic type from expected: expected {eet}, real {et}, result {rt}\n: {}",
                                    expr.get_index()
                                );
                                result.insert(expr.get_index(), EnhTypeFilter::Exact(rt));
                            }
                        }
                    }
                }
            }
        }

        (result, errors)
    }

    fn get_call_type_map(
        &self,
        call: &EnhASTFunctionCall,
        val_context: &mut EnhValContext,
        statics: &mut Statics,
        expected_expression_type: Option<&EnhASTType>,
    ) -> (FunctionTypeCheckerResult, Vec<TypeCheckError>) {
        /*
        println!(
            "get_call_type_map {call} expected_expression_type {}",
            OptionDisplay(&expected_expression_type)
        );
        */
        let mut first_try_of_map = FunctionTypeCheckerResult::new();

        for e in &call.parameters {
            let (e_result, e_errors) = self.get_expr_type_map(e, val_context, statics, None);
            first_try_of_map.extend(e_result);
        }

        let mut parameter_types_filters = Vec::new();

        for e in &call.parameters {
            if let Some(ast_type) = first_try_of_map.get(&e.get_index()) {
                parameter_types_filters.push(ast_type.clone());
            } else {
                parameter_types_filters.push(EnhTypeFilter::Any);
            }
        }

        if let Some((lambda_return_type, parameters_types)) =
            val_context.get_lambda(&call.function_name)
        {
            let return_type = lambda_return_type.as_ref().clone();
            let parameters_types = parameters_types.clone();

            let lambda_signature = EnhASTFunctionSignature {
                name: String::new(),
                parameters_types,
                return_type,
            };

            let (result, errors) = self.process_function_signature(
                &lambda_signature,
                &parameter_types_filters,
                call,
                val_context,
                statics,
                expected_expression_type,
            );

            return (result, errors);
        }

        let mut errors = Vec::new();
        let mut result = FunctionTypeCheckerResult::new();

        if let Ok(mut functions) = self.enhanced_ast_module.find_call_vec(
            &call,
            &parameter_types_filters,
            expected_expression_type,
        ) {
            if functions.is_empty() {
                println!("no functions for {} : {}", call.function_name, call.index);
                println!("filters {}", SliceDisplay(&parameter_types_filters));
                println!(
                    "expected_expression_type {}",
                    OptionDisplay(&expected_expression_type)
                );
                errors.push(TypeCheckError::new(
                    call.index.clone(),
                    format!("no functions for {}", call.function_name),
                    Vec::new(),
                ));
            } else {
                /*
                if functions.len() != 1 {
                    let min_rank = functions
                        .iter()
                        .min_by(|f1, f2| f1.rank.cmp(&f2.rank))
                        .unwrap()
                        .rank;

                    functions = functions
                        .into_iter()
                        .filter(|it| it.rank == min_rank)
                        .collect::<Vec<_>>();
                }
                */

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

                    let function_signature = found_function.signature();

                    let (function_result, function_errors) = self.process_function_signature(
                        &function_signature,
                        &parameter_types_filters,
                        &call,
                        val_context,
                        statics,
                        expected_expression_type,
                    );

                    result.extend(function_result);
                    errors.extend(function_errors);
                }
            }
        }
        (result, errors)
    }

    fn process_function_signature(
        &self,
        function_signature: &EnhASTFunctionSignature,
        parameter_types_filters: &Vec<EnhTypeFilter>,
        call: &EnhASTFunctionCall,
        val_context: &mut EnhValContext,
        statics: &mut Statics,
        expected_expression_type: Option<&EnhASTType>,
    ) -> (FunctionTypeCheckerResult, Vec<TypeCheckError>) {
        let mut result = FunctionTypeCheckerResult::new();
        let mut errors = Vec::new();

        let mut resolved_generic_types = ResolvedGenericTypes::new();

        for (i, parameter) in function_signature.parameters_types.iter().enumerate() {
            if parameter.is_generic() {
                let calculated_type_filter = parameter_types_filters.get(i).unwrap();

                let p_errors = self.resolve_type_filter(
                    call.index.clone(),
                    parameter,
                    calculated_type_filter,
                    &mut resolved_generic_types,
                );

                errors.extend(p_errors);
            }
        }

        loop {
            let resolved_generic_types_len = resolved_generic_types.len();

            for (i, e) in call.parameters.iter().enumerate() {
                let parameter_type = function_signature.parameters_types.get(i).unwrap();
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

        let return_type =
            if function_signature.return_type.is_generic() && resolved_generic_types.len() > 0 {
                if let Some(return_type) =
                    substitute(&function_signature.return_type, &resolved_generic_types)
                {
                    return_type
                } else {
                    function_signature.return_type.clone()
                }
            } else {
                function_signature.return_type.clone()
            };

        // the resolved type could be generic on a different generic type, we want to resolve it
        // with the generic type of the expected type
        /*
                if let Some(eet) = expected_expression_type {
                    if eet.is_generic() {
                        if return_type.is_generic() {
                            if let Ok(rgt) = resolve_generic_types_from_effective_type(&return_type, eet) {
                                if let Some(rt) = substitute(&return_type, &rgt) {
                                    println!("resolved generic type from expected: {return_type} -> {rt}");
                                    return_type = rt;
                                }
                            }
                        }
                    }
                }
        */

        result.insert(call.index.clone(), EnhTypeFilter::Exact(return_type));

        (result, errors)
    }

    fn resolve_type_filter(
        &self,
        index: EnhASTIndex,
        generic_type: &EnhASTType,
        effective_filter: &EnhTypeFilter,
        resolved_generic_types: &mut ResolvedGenericTypes,
    ) -> Vec<TypeCheckError> {
        let mut errors = Vec::new();
        if let EnhTypeFilter::Exact(effective_type) = effective_filter {
            //if !effective_type.is_generic() {
            match resolve_generic_types_from_effective_type(generic_type, effective_type) {
                Ok(rgt) => {
                    if let Err(e) = resolved_generic_types.extend(rgt) {
                        errors.push(TypeCheckError::new(index, e, Vec::new()));
                    }
                }
                Err(e) => errors.push(e),
            }
            //} else {
            //    println!("resolve_type_filter effective_type is generic, generic_type {generic_type}, effective_type {effective_type}");
            //}
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
            c::options::COptions,
            compile_target::CompileTarget,
            enh_ast::{EnhASTFunctionDef, EnhASTIndex, EnhASTModule, EnhASTNameSpace},
            enhanced_module::EnhancedASTModule,
            statics::Statics,
            enh_val_context::EnhValContext,
        },
        commandline::CommandLineOptions,
        project::RasmProject,
        type_check::function_type_checker::FunctionTypeChecker,
        utils::OptionDisplay,
    };

    use super::FunctionTypeCheckerResult;

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
            EnhancedASTModule::from_ast(modules, &project, &mut statics, &target, false);

        let function_type_checker = FunctionTypeChecker::new(&enhanced_ast_module);
        let mut statics = Statics::new();

        let path = Path::new("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm");
        for function in project
            .get_module(path, &target)
            .unwrap()
            .0
            .functions
            .into_iter()
        {
            let eh_function_def = EnhASTFunctionDef::from_ast(
                Some(path.to_path_buf()),
                EnhASTNameSpace::new("breakout".to_owned(), "breakout".to_owned()),
                function,
            );
            function_type_checker.get_type_map(&eh_function_def, &mut statics);
        }
    }

    #[test]
    fn test_svg_check_functions() {
        let project = RasmProject::new(PathBuf::from("/home/enrico/development/rasm/svglib"));

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
            EnhancedASTModule::from_ast(modules, &project, &mut statics, &target, false);

        let function_type_checker = FunctionTypeChecker::new(&enhanced_ast_module);
        let mut statics = Statics::new();

        let path = Path::new("/home/enrico/development/rasm/svglib/src/main/rasm/svg.rasm");
        for function in project
            .get_module(path, &target)
            .unwrap()
            .0
            .functions
            .into_iter()
        {
            let eh_function_def = EnhASTFunctionDef::from_ast(
                Some(path.to_path_buf()),
                EnhASTNameSpace::new("svglib".to_owned(), "svg".to_owned()),
                function,
            );
            function_type_checker.get_type_map(
                &&eh_function_def.fix_namespaces(&enhanced_ast_module),
                &mut statics,
            );
        }
    }

    #[test]
    fn test_functions_checker1() {
        let file = "resources/test/functions_checker1.rasm";

        let types_map = check_body(file);

        let r_value = types_map.get(&EnhASTIndex::new(
            Some(PathBuf::from(file).canonicalize().unwrap()),
            1,
            6,
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!("{}", OptionDisplay(&r_value),)
        );
    }

    #[test]
    fn test_functions_checker2() {
        let file = "resources/test/functions_checker2.rasm";

        let types_map = check_body(file);

        let r_value = types_map.get(&EnhASTIndex::new(
            Some(PathBuf::from(file).canonicalize().unwrap()),
            1,
            6,
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!("{}", OptionDisplay(&r_value),)
        );

        /*
        for (index, type_filter) in result {
            println!("{index} {type_filter}");
        }
        */
    }

    #[test]
    fn test_functions_checker3() {
        let file = "resources/test/functions_checker3.rasm";

        let types_map = check_body(file);

        let r_value = types_map.get(&EnhASTIndex::new(
            Some(PathBuf::from(file).canonicalize().unwrap()),
            1,
            6,
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!("{}", OptionDisplay(&r_value),)
        );

        /*
        for (index, type_filter) in result {
            println!("{index} {type_filter}");
        }
        */
    }

    #[test]
    fn test_functions_checker4() {
        let file = "resources/test/functions_checker4.rasm";

        let types_map = check_body(file);

        let r_value = types_map.get(&EnhASTIndex::new(
            Some(PathBuf::from(file).canonicalize().unwrap()),
            1,
            6,
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!("{}", OptionDisplay(&r_value),)
        );

        /*
        for (index, type_filter) in result {
            println!("{index} {type_filter}");
        }
        */
    }

    #[test]
    fn test_functions_checker5() {
        let file = "resources/test/functions_checker5.rasm";

        let types_map = check_body(file);

        let r_value = types_map.get(&EnhASTIndex::new(
            Some(PathBuf::from(file).canonicalize().unwrap()),
            1,
            6,
        ));

        assert_eq!(
            "Some(Exact(List<Option<i32>>))",
            format!("{}", OptionDisplay(&r_value),)
        );
    }

    #[test]
    fn test_functions_checker6() {
        let file = "resources/test/functions_checker6.rasm";

        let types_map = check_function(file);

        let r_value = types_map.get(&EnhASTIndex::new(
            Some(PathBuf::from(file).canonicalize().unwrap()),
            3,
            10,
        ));

        assert_eq!(
            "Some(Exact(Option<functions_checker6:functions_checker6_endsWith:T>))",
            format!("{}", OptionDisplay(&r_value),)
        );
    }

    #[test]
    fn test_functions_checker7() {
        let file = "resources/test/functions_checker7.rasm";

        let types_map = check_function(file);

        let r_value = types_map.get(&EnhASTIndex::new(
            Some(PathBuf::from(file).canonicalize().unwrap()),
            4,
            23,
        ));

        assert_eq!(
            "Some(Exact(functions_checker7:functions_checker7_endsWith:T))",
            format!("{}", OptionDisplay(&r_value),)
        );
    }

    fn check_body(file: &str) -> FunctionTypeCheckerResult {
        apply_to_functions_checker(file, file, |_enhanced_module, statics, ftc, module| {
            let mut val_context = EnhValContext::new(None);
            ftc.get_body_type_map(&mut val_context, statics, &module.body, None)
                .0
        })
    }

    fn check_function(file: &str) -> FunctionTypeCheckerResult {
        apply_to_functions_checker(file, file, |enhanced_module, statics, ftc, module| {
            let function = module.functions.first().unwrap().clone();
            ftc.get_type_map(
                &function.fix_namespaces(enhanced_module).fix_generics(),
                statics,
            )
            .0
        })
    }

    fn apply_to_functions_checker<F>(
        project_path: &str,
        file: &str,
        f: F,
    ) -> FunctionTypeCheckerResult
    where
        F: Fn(
            &EnhancedASTModule,
            &mut Statics,
            FunctionTypeChecker,
            EnhASTModule,
        ) -> FunctionTypeCheckerResult,
    {
        env::set_var("RASM_STDLIB", "/home/enrico/development/rust/rasm/stdlib");

        let target = CompileTarget::C(COptions::default());
        let mut statics = Statics::new();
        let (project, enhanced_ast_module) =
            project_and_enhanced_module(&target, &mut statics, &project_path);
        let function_type_checker = FunctionTypeChecker::new(&enhanced_ast_module);

        let (module, _, info) = project.get_module(Path::new(file), &target).unwrap();

        f(
            &enhanced_ast_module,
            &mut statics,
            function_type_checker,
            EnhASTModule::from_ast(module, info),
        )
    }

    fn project_and_enhanced_module(
        target: &CompileTarget,
        statics: &mut Statics,
        project_path: &str,
    ) -> (RasmProject, EnhancedASTModule) {
        let project = RasmProject::new(PathBuf::from(project_path));

        let (modules, _errors) = project.get_all_modules(
            statics,
            false,
            target,
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        );

        let (enhanced_ast_module, _errors) =
            EnhancedASTModule::from_ast(modules, &project, statics, target, false);

        (project, enhanced_ast_module.fix_generics())
    }
}
