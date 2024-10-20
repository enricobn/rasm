use std::{collections::HashMap, fmt::Display};

use linked_hash_map::{Iter, LinkedHashMap};

use crate::{
    codegen::val_context::{ASTIndex, ValContext},
    debug_i, dedent, indent,
    parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTFunctionSignature,
        ASTModifiers, ASTParameterDef, ASTStatement, ASTType, BuiltinTypeKind,
    },
    utils::{OptionDisplay, SliceDisplay},
};

use super::ast_modules_container::{
    ASTModulesContainer, FunctionTypeFilter, ModuleId, ModuleSource,
};

#[derive(Debug, Clone, PartialEq)]
struct ResolvedGenericTypes {
    map: LinkedHashMap<String, ASTType>,
}

impl ResolvedGenericTypes {
    pub fn new() -> Self {
        Self {
            map: LinkedHashMap::new(),
        }
    }

    pub fn get(&self, key: &String) -> Option<&ASTType> {
        self.map.get(key)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn contains_key(&self, key: &String) -> bool {
        self.map.contains_key(key)
    }

    pub fn extend(&mut self, other: Self) -> Result<(), String> {
        for (key, new_type) in other.map.into_iter() {
            if let Some(prev_type) = self.get(&key) {
                if &new_type != prev_type && new_type.is_generic() {
                    return Err(format!(
                        "Already resolved generic {key}, prev {prev_type}, new {new_type}"
                    ));
                }
            }
            self.map.insert(key, new_type);
        }
        Ok(())
    }

    pub fn iter(&self) -> Iter<String, ASTType> {
        self.map.iter()
    }

    pub fn insert(&mut self, key: String, value: ASTType) -> Option<ASTType> {
        if let Some(t) = self.map.get(&key) {
            assert_eq!(t, &value);
        }
        self.map.insert(key, value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypeCheckError {
    index: ASTIndex,
    message: String,
    inner: Vec<ASTTypeCheckError>,
}

impl ASTTypeCheckError {
    fn new(index: ASTIndex, message: String) -> Self {
        Self {
            index,
            message,
            inner: Vec::new(),
        }
    }

    fn add(self, index: ASTIndex, message: String) -> Self {
        let mut result = self.clone();
        result.inner.push(ASTTypeCheckError::new(index, message));
        result
    }
}

impl Display for ASTTypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.message, self.index)
    }
}

pub struct ASTTypeCheckerResult {
    map: HashMap<ASTIndex, FunctionTypeFilter>,
}

impl ASTTypeCheckerResult {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, index: ASTIndex, filter: FunctionTypeFilter) {
        self.map.insert(index, filter);
    }

    pub fn get(&self, index: &ASTIndex) -> Option<&FunctionTypeFilter> {
        self.map.get(index)
    }

    pub fn extend(&mut self, other: ASTTypeCheckerResult) {
        self.map.extend(other.map);
    }
}

pub struct ASTTypeChecker<'a> {
    modules_container: &'a ASTModulesContainer,
}

impl<'a> ASTTypeChecker<'a> {
    pub fn new(enhanced_ast_module: &'a ASTModulesContainer) -> Self {
        Self {
            modules_container: enhanced_ast_module,
        }
    }

    pub fn get_type_map(
        &self,
        function: &ASTFunctionDef,
        statics: &mut ValContext,
        module_id: &ModuleId,
        module_source: &ModuleSource,
    ) -> (ASTTypeCheckerResult, Vec<ASTTypeCheckError>) {
        let mut result = ASTTypeCheckerResult::new();
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
            let par = par
                .clone()
                .fix_generics(&function.signature().generics_prefix(module_id));
            // TODO check error
            val_context.insert_par(par.name.clone(), par, module_id, module_source);
        }

        match &function.body {
            ASTFunctionBody::RASMBody(body) => {
                // TODO return_type
                (result, _, errors) = self.get_body_type_map(
                    &mut val_context,
                    statics,
                    body,
                    Some(&function.return_type),
                    module_id,
                    module_source,
                );
            }
            ASTFunctionBody::NativeBody(_body) => {}
        }

        (result, errors)
    }

    pub fn get_body_type_map(
        &self,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        body: &Vec<ASTStatement>,
        expected_last_statement_type: Option<&ASTType>,
        module_id: &ModuleId,
        module_source: &ModuleSource,
    ) -> (
        ASTTypeCheckerResult,
        Option<FunctionTypeFilter>,
        Vec<ASTTypeCheckError>,
    ) {
        let mut errors = Vec::new();
        let mut result = ASTTypeCheckerResult::new();
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
                                self.get_expr_type_map(
                                    e,
                                    val_context,
                                    statics,
                                    Some(*elst),
                                    module_id,
                                    module_source,
                                )
                            } else {
                                self.get_expr_type_map(
                                    e,
                                    val_context,
                                    statics,
                                    None,
                                    module_id,
                                    module_source,
                                )
                            };

                            result.extend(e_result);
                            errors.extend(e_errors);

                            let index = ASTIndex::new(
                                module_id.clone(),
                                module_source.clone(),
                                e.position(),
                            );
                            return_type = result.get(&index).cloned();
                        } else {
                            let (e_result, e_errors) = self.get_expr_type_map(
                                e,
                                val_context,
                                statics,
                                None,
                                module_id,
                                module_source,
                            );
                            result.extend(e_result);
                            errors.extend(e_errors);
                        }
                    } else {
                        let (e_result, e_errors) = self.get_expr_type_map(
                            e,
                            val_context,
                            statics,
                            None,
                            module_id,
                            module_source,
                        );
                        result.extend(e_result);
                        errors.extend(e_errors);
                    }
                }
                ASTStatement::LetStatement(key, e, is_const, index) => {
                    let (e_result, e_errors) = self.get_expr_type_map(
                        e,
                        val_context,
                        statics,
                        None,
                        module_id,
                        module_source,
                    );
                    result.extend(e_result);
                    errors.extend(e_errors);

                    let e_index =
                        ASTIndex::new(module_id.clone(), module_source.clone(), e.position());

                    let index =
                        ASTIndex::new(module_id.clone(), module_source.clone(), index.clone());
                    if let Some(filter) = result.get(&e_index) {
                        if let FunctionTypeFilter::Exact(ast_type, module_id) = filter {
                            if *is_const {
                                statics.insert_let(key.clone(), ast_type.clone(), &index);
                            } else {
                                // TODO error
                                val_context.insert_let(key.clone(), ast_type.clone(), &index);
                            }
                        }
                        result.insert(index, filter.clone());
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
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        module_id: &ModuleId,
        module_source: &ModuleSource,
    ) -> (ASTTypeCheckerResult, Vec<ASTTypeCheckError>) {
        let mut errors = Vec::new();
        let mut result = ASTTypeCheckerResult::new();

        let index = ASTIndex::new(module_id.clone(), module_source.clone(), expr.position());
        match expr {
            ASTExpression::StringLiteral(_, _) => {
                result.insert(
                    index.clone(),
                    FunctionTypeFilter::Exact(
                        ASTType::Builtin(BuiltinTypeKind::String),
                        module_id.clone(),
                    ),
                );
            }
            ASTExpression::ASTFunctionCallExpression(call) => {
                let (call_result, call_errors) = self.get_call_type_map(
                    call,
                    val_context,
                    statics,
                    expected_expression_type,
                    module_id,
                    module_source,
                );
                result.extend(call_result);
                errors.extend(call_errors);
            }
            ASTExpression::ValueRef(name, _) => {
                if let Some(kind) = val_context.get(name) {
                    result.insert(
                        index.clone(),
                        FunctionTypeFilter::Exact(kind.ast_type(), module_id.clone()),
                    );
                } else if let Some(entry) = statics.get(name) {
                    result.insert(
                        index.clone(),
                        FunctionTypeFilter::Exact(entry.ast_type(), module_id.clone()),
                    );
                }
            }
            ASTExpression::Value(value_type, _) => {
                result.insert(
                    index.clone(),
                    FunctionTypeFilter::Exact(value_type.to_type(), module_id.clone()),
                );
            }
            ASTExpression::Lambda(lambda) => {
                if let Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                })) = expected_expression_type
                {
                    let mut val_context = ValContext::new(Some(&val_context));

                    for ((name, par_position), ast_type) in
                        lambda.parameter_names.iter().zip(parameters.iter())
                    {
                        let par_index = ASTIndex::new(
                            module_id.clone(),
                            module_source.clone(),
                            par_position.clone(),
                        );
                        // TODO check error
                        if let Err(e) = val_context.insert_par(
                            name.clone(),
                            ASTParameterDef {
                                name: name.clone(),
                                ast_type: ast_type.clone(),
                                position: par_position.clone(),
                            },
                            module_id,
                            module_source,
                        ) {
                            errors.push(ASTTypeCheckError::new(par_index, e));
                        } else {
                            result.insert(
                                par_index.clone(),
                                FunctionTypeFilter::Exact(ast_type.clone(), module_id.clone()),
                            );
                        }
                    }

                    let (body_result, body_return_type, body_errors) = self.get_body_type_map(
                        &mut val_context,
                        statics,
                        &lambda.body,
                        Some(return_type.as_ref()),
                        module_id,
                        module_source,
                    );

                    result.extend(body_result);
                    errors.extend(body_errors);

                    if let Some(FunctionTypeFilter::Exact(brt, position)) = body_return_type {
                        result.insert(
                            index.clone(),
                            FunctionTypeFilter::Exact(
                                ASTType::Builtin(BuiltinTypeKind::Lambda {
                                    parameters: parameters.clone(),
                                    return_type: Box::new(brt),
                                }),
                                module_id.clone(),
                            ),
                        );
                    } else {
                        result.insert(
                            index.clone(),
                            FunctionTypeFilter::Exact(
                                ASTType::Builtin(BuiltinTypeKind::Lambda {
                                    parameters: parameters.clone(),
                                    return_type: return_type.clone(),
                                }),
                                module_id.clone(),
                            ),
                        );
                    }
                } else {
                    result.insert(
                        index.clone(),
                        FunctionTypeFilter::Lambda(lambda.parameter_names.len(), None),
                    );
                }
            }
            ASTExpression::Any(_) => todo!(),
        }

        // the resolved type could be generic on a different generic type, we want to resolve it
        // with the generic type of the expected type

        if let Some(eet) = expected_expression_type {
            if eet.is_generic() {
                if let Some(FunctionTypeFilter::Exact(et, e_module_id)) = result.get(&index) {
                    if et.is_generic() {
                        if let Ok(rgt) = Self::resolve_generic_types_from_effective_type(et, eet) {
                            if let Some(rt) = Self::substitute(et, &rgt) {
                                println!(
                                    "resolved generic type from expected: expected {eet}, real {et}, result {rt}\n: {}",
                                    expr.position()
                                );
                                result.insert(
                                    index,
                                    FunctionTypeFilter::Exact(rt, module_id.clone()),
                                );
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
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        module_id: &ModuleId,
        module_source: &ModuleSource,
    ) -> (ASTTypeCheckerResult, Vec<ASTTypeCheckError>) {
        let index = ASTIndex::new(
            module_id.clone(),
            module_source.clone(),
            call.position.clone(),
        );
        /*
        println!(
            "get_call_type_map {call} expected_expression_type {}",
            OptionDisplay(&expected_expression_type)
        );
        */
        let mut first_try_of_map = ASTTypeCheckerResult::new();

        for e in &call.parameters {
            let (e_result, e_errors) =
                self.get_expr_type_map(e, val_context, statics, None, module_id, module_source);
            first_try_of_map.extend(e_result);
        }

        let mut parameter_types_filters = Vec::new();

        for e in &call.parameters {
            let e_index = ASTIndex::new(module_id.clone(), module_source.clone(), e.position());
            if let Some(ast_type) = first_try_of_map.get(&e_index) {
                parameter_types_filters.push(ast_type.clone());
            } else {
                parameter_types_filters.push(FunctionTypeFilter::Any);
            }
        }

        if let Some((lambda_return_type, parameters_types)) =
            val_context.get_lambda(&call.function_name)
        {
            let return_type = lambda_return_type.as_ref().clone();
            let parameters_types = parameters_types.clone();
            let generics = parameters_types
                .iter()
                .filter_map(|p| {
                    if let ASTType::Generic(g_p, g_name) = p {
                        Some(g_name.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            let lambda_signature = ASTFunctionSignature {
                name: String::new(),
                parameters_types,
                return_type,
                generics,
                modifiers: ASTModifiers { public: true },
            };

            let (result, errors) = self.process_function_signature(
                &lambda_signature,
                &parameter_types_filters,
                call,
                val_context,
                statics,
                expected_expression_type,
                module_id,
                module_source,
            );

            return (result, errors);
        }

        let mut errors = Vec::new();
        let mut result = ASTTypeCheckerResult::new();

        let mut functions = self.modules_container.find_call_vec(
            &call.function_name,
            &parameter_types_filters,
            expected_expression_type,
            module_id,
        );

        if functions.is_empty() {
            println!("no functions for {} : {}", call.function_name, index);
            println!("filters {}", SliceDisplay(&parameter_types_filters));
            println!(
                "expected_expression_type {}",
                OptionDisplay(&expected_expression_type)
            );
            errors.push(ASTTypeCheckError::new(
                index.clone(),
                format!("no functions for {}", call.function_name),
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
                    call.function_name, index
                );
                println!("{}", SliceDisplay(&parameter_types_filters));
                for fun in functions.iter() {
                    println!("  function {}", fun.signature);
                }

                errors.push(ASTTypeCheckError::new(
                    index.clone(),
                    format!("found more than one function for {}", call.function_name),
                ));

                result.extend(first_try_of_map);
            } else {
                let found_function = functions.remove(0);

                let (function_result, function_errors) = self.process_function_signature(
                    &found_function.signature,
                    &parameter_types_filters,
                    &call,
                    val_context,
                    statics,
                    expected_expression_type,
                    module_id,
                    module_source,
                );

                result.extend(function_result);
                errors.extend(function_errors);
            }
        }
        (result, errors)
    }

    fn process_function_signature(
        &self,
        function_signature: &ASTFunctionSignature,
        parameter_types_filters: &Vec<FunctionTypeFilter>,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        call_module_id: &ModuleId,
        call_source: &ModuleSource,
    ) -> (ASTTypeCheckerResult, Vec<ASTTypeCheckError>) {
        let index = ASTIndex::new(
            call_module_id.clone(),
            call_source.clone(),
            call.position.clone(),
        );
        let mut result = ASTTypeCheckerResult::new();
        let mut errors = Vec::new();

        let mut resolved_generic_types = ResolvedGenericTypes::new();

        for (i, parameter) in function_signature.parameters_types.iter().enumerate() {
            if parameter.is_generic() {
                let calculated_type_filter = parameter_types_filters.get(i).unwrap();

                let p_errors = self.resolve_type_filter(
                    &index,
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
                let e_index =
                    ASTIndex::new(call_module_id.clone(), call_source.clone(), e.position());
                let parameter_type = function_signature.parameters_types.get(i).unwrap();
                let ast_type = Self::substitute(&parameter_type, &resolved_generic_types)
                    .unwrap_or(parameter_type.clone());

                let (e_result, e_errors) = self.get_expr_type_map(
                    e,
                    val_context,
                    statics,
                    Some(&ast_type),
                    call_module_id,
                    call_source,
                );

                result.extend(e_result);
                errors.extend(e_errors);

                if let Some(calculated_type_filter) = result.get(&e_index) {
                    let p_errors = self.resolve_type_filter(
                        &index,
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
                    Self::substitute(&function_signature.return_type, &resolved_generic_types)
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

        result.insert(
            index.clone(),
            FunctionTypeFilter::Exact(return_type, call_module_id.clone()),
        );

        (result, errors)
    }

    fn resolve_type_filter(
        &self,
        index: &ASTIndex,
        generic_type: &ASTType,
        effective_filter: &FunctionTypeFilter,
        resolved_generic_types: &mut ResolvedGenericTypes,
    ) -> Vec<ASTTypeCheckError> {
        let mut errors = Vec::new();
        if let FunctionTypeFilter::Exact(effective_type, t_module_id) = effective_filter {
            //if !effective_type.is_generic() {
            match Self::resolve_generic_types_from_effective_type(generic_type, effective_type) {
                Ok(rgt) => {
                    if let Err(e) = resolved_generic_types.extend(rgt) {
                        errors.push(ASTTypeCheckError::new(index.clone(), e));
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

    pub fn resolve_generic_types_from_effective_type(
        generic_type: &ASTType,
        effective_type: &ASTType,
    ) -> Result<ResolvedGenericTypes, ASTTypeCheckError> {
        let mut result = ResolvedGenericTypes::new();
        if generic_type == effective_type || !generic_type.is_generic() {
            return Ok(result);
        }

        debug_i!("resolve_generic_types_from_effective_type: generic_type {generic_type} effective_type  {effective_type}");
        //println!("resolve_generic_types_from_effective_type: generic_type {generic_type} effective_type  {effective_type}");
        indent!();

        match generic_type {
            ASTType::Builtin(kind) => {
                match kind {
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
                            if e_parameters.len() != p_parameters.len() {
                                return Err(Self::type_check_error(
                                    "Invalid parameters count.".to_string(),
                                ));
                            }
                            for (i, p_p) in p_parameters.iter().enumerate() {
                                let e_p = e_parameters.get(i).unwrap();

                                let inner_result = Self::resolve_generic_types_from_effective_type(p_p, e_p)
                                .map_err(|e| e.add(ASTIndex::none(), format!("lambda param gen type {generic_type}, eff. type {effective_type}")))?;

                                result
                                    .extend(inner_result)
                                    .map_err(|it| Self::type_check_error(it.clone()))?;
                            }

                            /*
                            for p_t in p_return_type {
                                if let Some(e_t) = e_return_type {
                                    let inner_result = resolve_generic_types_from_effective_type(p_t, e_t)
                                        .map_err(|e| format!("{} in return type gen type {generic_type}eff. type {effective_type}", e))?;

                                    result.extend(inner_result.into_iter());
                                } else {
                                    dedent!();
                                    if let ASTType::Generic(p) = p_t.as_ref() {
                                        return Err(format!("Found generic type {p} that is (). For now, we cannot handle it").into());
                                    }
                                    return Err("Expected some type but got None".into());
                                }
                            }

                             */
                            let inner_result = Self::resolve_generic_types_from_effective_type(p_return_type, e_return_type)
                            .map_err(|e| e.add(ASTIndex::none(), format!("in return type gen type {generic_type}, eff. type {effective_type}")))?;

                            result
                                .extend(inner_result)
                                .map_err(|it| Self::type_check_error(it.clone()))?;
                        }
                        _ => {
                            dedent!();
                            return Err(Self::type_check_error(format!("unmatched types, generic type is {generic_type}, real type is {effective_type}")));
                        }
                    },
                }
            }
            ASTType::Generic(_, p) => {
                let ignore = if let ASTType::Generic(_, p1) = effective_type {
                    p == p1
                } else {
                    false
                };
                if !ignore {
                    debug_i!("resolved generic type {p} to {effective_type}");
                    result.insert(p.clone(), effective_type.clone());
                }
            }
            ASTType::Custom {
                name: g_name,
                param_types: g_param_types,
                position: _,
            } => match effective_type {
                ASTType::Custom {
                    name: e_name,
                    param_types: e_param_types,
                    position: _,
                } => {
                    if g_name != e_name {
                        dedent!();
                        return Err(Self::type_check_error(format!(
                            "unmatched custom type name {g_name} != {e_name}"
                        )));
                    }

                    for (i, p_p) in g_param_types.iter().enumerate() {
                        let e_p = if let Some(p) = e_param_types.get(i) {
                            p
                        } else {
                            return Err(ASTTypeCheckError::new(
                                ASTIndex::none(),
                                format!("Cannot find parameter {i}"),
                            ));
                        };
                        let inner_result = Self::resolve_generic_types_from_effective_type(p_p, e_p)
                            .map_err(|e| e.add(ASTIndex::none(), format!("in custom type gen type {generic_type} eff type {effective_type}")))?;

                        result.extend(inner_result).map_err(|it| {
                            ASTTypeCheckError::new(
                                ASTIndex::none(),
                                format!(
                                    "{it}: in custom type gen type {generic_type} eff type {effective_type}"
                                ),
                            )
                        })?;
                    }
                }
                ASTType::Generic(_, _) => {}
                _ => {
                    dedent!();
                    return Err(Self::type_check_error(format!(
                        "unmatched types, generic type is {generic_type}, real type is {effective_type}")));
                }
            },
            ASTType::Unit => {}
        }

        //debug_i!("result {result}");
        dedent!();
        Ok(result)
    }

    fn type_check_error(message: String) -> ASTTypeCheckError {
        ASTTypeCheckError::new(ASTIndex::none(), message)
    }

    fn substitute(
        ast_type: &ASTType,
        resolved_param_types: &ResolvedGenericTypes,
    ) -> Option<ASTType> {
        if !ast_type.is_generic() {
            return None;
        }

        let result = match &ast_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let mut something_substituted = false;
                    let new_parameters =
                        match Self::substitute_types(parameters, resolved_param_types) {
                            None => parameters.clone(),
                            Some(new_parameters) => {
                                something_substituted = true;
                                new_parameters
                            }
                        };

                    let new_return_type =
                        if let Some(new_t) = Self::substitute(return_type, resolved_param_types) {
                            something_substituted = true;
                            Box::new(new_t)
                        } else {
                            return_type.clone()
                        };

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
            ASTType::Generic(_, p) => {
                if resolved_param_types.contains_key(p) {
                    resolved_param_types.get(p).cloned()
                } else {
                    None
                }
            }
            ASTType::Custom {
                name,
                param_types,
                position: index,
            } => {
                Self::substitute_types(param_types, resolved_param_types).map(|new_param_types| {
                    // TODO it's a bit heuristic
                    let new_index = if new_param_types.is_empty() {
                        index.clone()
                    } else if let Some(ASTType::Custom {
                        name: _,
                        param_types: _,
                        position: ast_index,
                    }) = new_param_types.last()
                    {
                        ast_index.mv_right(1)
                    } else {
                        index.clone()
                    };
                    ASTType::Custom {
                        name: name.clone(),
                        param_types: new_param_types,
                        position: new_index,
                    }
                })
            }
            ASTType::Unit => None,
        };

        if let Some(r) = &result {
            debug_i!("something substituted {ast_type} -> {r}");
        }
        result
    }

    fn substitute_types(
        types: &[ASTType],
        resolved_param_types: &ResolvedGenericTypes,
    ) -> Option<Vec<ASTType>> {
        let mut something_substituted = false;
        let new_types = types
            .iter()
            .map(|it| {
                if let Some(new_t) = Self::substitute(it, resolved_param_types) {
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
            enh_ast::EhModuleInfo,
            statics::Statics,
            val_context::{ASTIndex, ValContext},
        },
        commandline::CommandLineOptions,
        parser::ast::{ASTModule, ASTPosition},
        project::RasmProject,
        type_check::ast_modules_container::ASTModulesContainer,
        utils::OptionDisplay,
    };

    use super::{ASTTypeChecker, ASTTypeCheckerResult};

    #[test]
    fn test_breakout_check_functions() {
        let project = RasmProject::new(PathBuf::from("../rasm/resources/examples/breakout"));

        let target = CompileTarget::C(COptions::default());

        let container = container_from_project(&project, &target);

        let function_type_checker = ASTTypeChecker::new(&container);

        let mut statics = ValContext::new(None);

        let path = Path::new("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm");

        let (module, _errors, info) = project.get_module(path, &target).unwrap();

        for function in module.functions.into_iter() {
            function_type_checker.get_type_map(
                &function,
                &mut statics,
                &info.module_id(),
                &info.module_source(),
            );
        }
    }

    #[test]
    fn test_svg_check_functions() {
        let project = RasmProject::new(PathBuf::from("/home/enrico/development/rasm/svglib"));

        let target = CompileTarget::C(COptions::default());

        let container = container_from_project(&project, &target);

        let function_type_checker = ASTTypeChecker::new(&container);

        let mut statics = ValContext::new(None);

        let path = Path::new("/home/enrico/development/rasm/svglib/src/main/rasm/svg.rasm");

        let (module, _errors, info) = project.get_module(path, &target).unwrap();

        for function in module.functions.into_iter() {
            function_type_checker.get_type_map(
                &function,
                &mut statics,
                &info.module_id(),
                &info.module_source(),
            );
        }
    }

    #[test]
    fn test_functions_checker1() {
        let file = "resources/test/functions_checker1.rasm";

        let (types_map, info) = check_body(file);

        for (key, value) in types_map.map.iter() {
            println!("types_map {key} = {value}");
        }

        let r_value = types_map.get(&ASTIndex::new(
            info.module_id(),
            info.module_source(),
            ASTPosition::new(1, 6),
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!("{}", OptionDisplay(&r_value),)
        );
    }

    #[test]
    fn test_functions_checker2() {
        let file = "resources/test/functions_checker2.rasm";

        let (types_map, info) = check_body(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_id(),
            info.module_source(),
            ASTPosition::new(1, 6),
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

        let (types_map, info) = check_body(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_id(),
            info.module_source(),
            ASTPosition::new(1, 6),
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

        let (types_map, info) = check_body(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_id(),
            info.module_source(),
            ASTPosition::new(1, 6),
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

        let (types_map, info) = check_body(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_id(),
            info.module_source(),
            ASTPosition::new(1, 6),
        ));

        assert_eq!(
            "Some(Exact(List<Option<i32>>))",
            format!("{}", OptionDisplay(&r_value),)
        );
    }

    #[test]
    fn test_functions_checker6() {
        let file = "resources/test/functions_checker6.rasm";

        let (types_map, info) = check_function(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_id(),
            info.module_source(),
            ASTPosition::new(3, 10),
        ));

        assert_eq!(
            "Some(Exact(Option<functions_checker6_functions_checker6_endsWith:T>))",
            format!("{}", OptionDisplay(&r_value),)
        );
    }

    #[test]
    fn test_functions_checker7() {
        let file = "resources/test/functions_checker7.rasm";

        let (types_map, info) = check_function(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_id(),
            info.module_source(),
            ASTPosition::new(4, 23),
        ));

        assert_eq!(
            "Some(Exact(functions_checker7_functions_checker7_endsWith:T))",
            format!("{}", OptionDisplay(&r_value),)
        );
    }

    fn container_from_project(
        project: &RasmProject,
        target: &CompileTarget,
    ) -> ASTModulesContainer {
        let mut statics = Statics::new();
        let (modules, _errors) = project.get_all_modules(
            &mut statics,
            false,
            &target,
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        );

        let mut container = ASTModulesContainer::new();

        for (module, info) in modules {
            container.add(module, info.module_id(), info.module_source(), false);
        }

        container
    }

    fn check_body(file: &str) -> (ASTTypeCheckerResult, EhModuleInfo) {
        apply_to_functions_checker(file, file, |module, statics, ftc, info| {
            let mut val_context = ValContext::new(None);
            let mut static_val_context = ValContext::new(None);
            ftc.get_body_type_map(
                &mut val_context,
                &mut static_val_context,
                &module.body,
                None,
                &info.module_id(),
                &info.module_source(),
            )
            .0
        })
    }

    fn check_function(file: &str) -> (ASTTypeCheckerResult, EhModuleInfo) {
        apply_to_functions_checker(file, file, |module, statics, ftc, info| {
            let function = module.functions.first().unwrap().clone();
            let mut static_val_context = ValContext::new(None);
            ftc.get_type_map(
                &function,
                &mut static_val_context,
                &info.module_id(),
                &info.module_source(),
            )
            .0
        })
    }

    fn apply_to_functions_checker<F>(
        project_path: &str,
        file: &str,
        f: F,
    ) -> (ASTTypeCheckerResult, EhModuleInfo)
    where
        F: Fn(&ASTModule, &mut Statics, ASTTypeChecker, EhModuleInfo) -> ASTTypeCheckerResult,
    {
        env::set_var("RASM_STDLIB", "/home/enrico/development/rust/rasm/stdlib");

        let target = CompileTarget::C(COptions::default());
        let mut statics = Statics::new();
        let (project, enhanced_ast_module) =
            project_and_container(&target, &mut statics, &project_path);
        let function_type_checker = ASTTypeChecker::new(&enhanced_ast_module);

        let (module, _, info) = project.get_module(Path::new(file), &target).unwrap();

        println!("module:");
        module.print();

        (
            f(&module, &mut statics, function_type_checker, info.clone()),
            info,
        )
    }

    fn project_and_container(
        target: &CompileTarget,
        statics: &mut Statics,
        project_path: &str,
    ) -> (RasmProject, ASTModulesContainer) {
        let project = RasmProject::new(PathBuf::from(project_path));

        let container = container_from_project(&project, target);

        (project, container)
    }
}
