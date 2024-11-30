use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use rasm_parser::{
    catalog::{ASTIndex, ModuleId, ModuleNamespace},
    parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTParameterDef,
        ASTStatement, ASTType,
    },
};
use rasm_utils::{debug_i, dedent, indent, HashMapDisplay, OptionDisplay, SliceDisplay};

use crate::type_check::ast_modules_container::ASTTypeFilter;

use super::{
    ast_modules_container::ASTFunctionType,
    ast_type_checker::{ASTTypeCheckInfo, ASTTypeChecker},
};

#[derive(Clone)]
pub enum ASTParameterDependencies {
    Any,
    None,
    Precise(HashSet<ASTType>),
}

impl ASTParameterDependencies {
    pub fn precise(types: Vec<ASTType>) -> Self {
        ASTParameterDependencies::Precise(types.into_iter().collect())
    }
}

impl Display for ASTParameterDependencies {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTParameterDependencies::Any => f.write_str("Any"),
            ASTParameterDependencies::None => f.write_str("None"),
            ASTParameterDependencies::Precise(hash_set) => {
                let mut v = hash_set
                    .iter()
                    .map(|it| format!("{it}"))
                    .collect::<Vec<_>>();
                v.sort();
                write!(f, "{}", SliceDisplay(&v))
            }
        }
    }
}

impl ASTParameterDependencies {
    fn and(&self, other: &Self) -> Self {
        match self {
            ASTParameterDependencies::Any => other.clone(),
            ASTParameterDependencies::None => ASTParameterDependencies::None,
            ASTParameterDependencies::Precise(self_types) => match other {
                ASTParameterDependencies::Any => self.clone(),
                ASTParameterDependencies::None => ASTParameterDependencies::None,
                ASTParameterDependencies::Precise(other_types) => {
                    ASTParameterDependencies::Precise(
                        self_types
                            .intersection(other_types)
                            .cloned()
                            .collect::<HashSet<_>>(),
                    )
                }
            },
        }
    }

    fn or(&self, other: &Self) -> Self {
        match self {
            ASTParameterDependencies::Any => ASTParameterDependencies::Any,
            ASTParameterDependencies::None => other.clone(),
            ASTParameterDependencies::Precise(self_types) => match other {
                ASTParameterDependencies::Any => ASTParameterDependencies::Any,
                ASTParameterDependencies::None => self.clone(),
                ASTParameterDependencies::Precise(other_types) => {
                    ASTParameterDependencies::Precise(
                        self_types
                            .union(other_types)
                            .cloned()
                            .collect::<HashSet<_>>(),
                    )
                }
            },
        }
    }
}

#[derive(Clone)]
pub struct ASTFunctionsDependencies {
    parameters: HashMap<String, ASTParameterDependencies>,
}

impl Display for ASTFunctionsDependencies {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", HashMapDisplay(&self.parameters))
    }
}

impl ASTFunctionsDependencies {
    fn new() -> Self {
        Self {
            parameters: HashMap::new(),
        }
    }

    fn and(&mut self, parameter: &ASTParameterDef, types: ASTParameterDependencies) {
        //debug_i!("before and {}", HashMapDisplay(&self.parameters));
        //indent!();
        //debug_i!("and {types}");
        let pd = self
            .parameters
            .entry(parameter.name.clone())
            .or_insert(ASTParameterDependencies::Any)
            .clone();
        self.parameters
            .insert(parameter.name.clone(), pd.and(&types));
        //dedent!();
        //debug_i!("after and {}", HashMapDisplay(&self.parameters));
    }

    fn or(&mut self, parameter: &ASTParameterDef, types: ASTParameterDependencies) {
        //debug_i!("before or {}", HashMapDisplay(&self.parameters));
        //indent!();
        //debug_i!("or {types}");
        let pd = self
            .parameters
            .entry(parameter.name.clone())
            .or_insert(ASTParameterDependencies::None)
            .clone();
        self.parameters
            .insert(parameter.name.clone(), pd.or(&types));
        //dedent!();
        //debug_i!("after or {}", HashMapDisplay(&self.parameters));
    }

    fn get(&self, name: &str) -> Option<&ASTParameterDependencies> {
        self.parameters.get(name)
    }
}

pub fn function_dependencies(
    function: &ASTFunctionDef,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    ast_type_check: &ASTTypeChecker,
) -> ASTFunctionsDependencies {
    let mut already_checked = HashMap::new();
    function_dependencies_inner(
        function,
        module_namespace,
        module_id,
        ast_type_check,
        &mut already_checked,
    )
}

fn function_dependencies_inner(
    function: &ASTFunctionDef,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    ast_type_check: &ASTTypeChecker,
    already_checked: &mut HashMap<ASTIndex, ASTFunctionsDependencies>,
) -> ASTFunctionsDependencies {
    debug_i!("function_dependencies {function}");
    indent!();
    let mut result = ASTFunctionsDependencies::new();

    for parameter in function.parameters.iter() {
        if parameter.ast_type.is_generic() {
            result.or(parameter, ASTParameterDependencies::Any);
        } else {
            result.and(
                parameter,
                ASTParameterDependencies::precise(vec![parameter.ast_type.clone()]),
            );
        }
    }

    if function.generic_types.is_empty() {
        debug_i!("non generic function {result}");
        dedent!();
        return result;
    }

    let index = ASTIndex::new(
        module_namespace.clone(),
        module_id.clone(),
        function.position.clone(),
    );

    if let Some(r) = already_checked.get(&index) {
        debug_i!("Already resolved to {r}");
        dedent!();
        return r.clone();
    }

    already_checked.insert(index.clone(), result.clone());

    let mut calls = Vec::new();

    function_calls(
        &mut calls,
        function,
        module_namespace,
        module_id,
        ast_type_check,
    );

    for call in calls.iter() {
        debug_i!("call {call}");
        indent!();
        let mut call_result = ASTFunctionsDependencies::new();
        if let Some(call_type_check_entry) = ast_type_check.result.get(&ASTIndex::new(
            module_namespace.clone(),
            module_id.clone(),
            call.position.clone(),
        )) {
            for (call_expr_i, call_expr) in call.parameters.iter().enumerate() {
                debug_i!("evaluating expr {call_expr}");
                indent!();
                let call_expr_index = ASTIndex::new(
                    module_namespace.clone(),
                    module_id.clone(),
                    call_expr.position().clone(),
                );
                if let Some(call_expr_entry) = ast_type_check.result.get(&call_expr_index) {
                    if let Some(ASTTypeFilter::Exact(call_expr_type, call_expr_info)) =
                        call_expr_entry.filter()
                    {
                        match call_type_check_entry.info() {
                            ASTTypeCheckInfo::Call(_, vec) => {
                                for (signature, index) in vec.iter() {
                                    debug_i!("call to function {signature} : {index}");
                                    indent!();

                                    match ast_type_check.container().function(index).unwrap() {
                                        ASTFunctionType::Standard(inner_function) => {
                                            let deps = function_dependencies_inner(
                                                inner_function,
                                                index.module_namespace(),
                                                index.module_id(),
                                                ast_type_check,
                                                already_checked,
                                            );

                                            let inner_function_par =
                                                inner_function.parameters.get(call_expr_i).unwrap();
                                            if let Some(par_dependencies) =
                                                deps.get(&inner_function_par.name)
                                            {
                                                match par_dependencies {
                                                    ASTParameterDependencies::Any => {
                                                        /*call_result.or(
                                                            &inner_function_par,
                                                            ASTParameterDependencies::Any,
                                                        );
                                                        */
                                                    }
                                                    ASTParameterDependencies::None => {}
                                                    ASTParameterDependencies::Precise(
                                                        ref found_types,
                                                    ) => {
                                                        for ft in found_types.iter() {
                                                            debug_i!("found_type {ft}");
                                                            match ASTTypeChecker::resolve_generic_types_from_effective_type(&call_expr_type, ft) {
                                                                Ok(rgt) => {
                                                                    for par in function.parameters.iter() {
                                                                        let par_type = par.ast_type.fix_generics(&format!("{}_{}", module_namespace.0, function.name));
                                                                        if par_type.is_generic() {
                                                                            debug_i!("resolved generic types {rgt}");
                                                                            if let Some(t) = ASTTypeChecker::substitute(&par_type, &rgt) {
                                                                                call_result.or(par, ASTParameterDependencies::precise(vec![t]));
                                                                            } else {
                                                                                debug_i!("cannot substitute {}", par.ast_type);
                                                                                //call_result.or(par, ASTParameterDependencies::precise(vec![par.ast_type.clone()]));    
                                                                            }
                                                                        } else {
                                                                            call_result.or(par, ASTParameterDependencies::precise(vec![par_type.clone()]));
                                                                        }
                                                                    }
                                                                }
                                                                Err(e) => {
                                                                    debug_i!("Error resolving generic type from effective type: {e}");
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            } else {
                                                // TODO
                                            }
                                        }
                                        _ => {}
                                    }

                                    dedent!();
                                }
                            }
                            _ => {}
                        }
                    }
                } else {
                    debug_i!("cannot find entry in type check");
                }
                dedent!();
            }
        } else {
            debug_i!("cannot find entry in type check");
        }

        debug_i!("call result {call_result}");
        for par in function.parameters.iter() {
            if let Some(r) = call_result.get(&par.name) {
                result.and(par, r.clone());
            }
        }
        dedent!();
    }

    debug_i!("function result {result}");
    dedent!();
    result
}

fn function_dependencies_inner_2(
    function: &ASTFunctionDef,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    ast_type_check: &ASTTypeChecker,
    already_checked: &mut HashMap<ASTIndex, ASTFunctionsDependencies>,
) -> ASTFunctionsDependencies {
    let mut result = ASTFunctionsDependencies::new();

    debug_i!("function_dependencies {function}");
    indent!();

    let parameters_with_generic_type = function
        .parameters
        .iter()
        .filter(|it| it.ast_type.is_generic())
        .collect::<Vec<_>>();

    let index = ASTIndex::new(
        module_namespace.clone(),
        module_id.clone(),
        function.position.clone(),
    );

    if let Some(r) = already_checked.get(&index) {
        debug_i!("Already resolved to {r}");
        dedent!();
        return r.clone();
    }

    for parameter in parameters_with_generic_type.iter() {
        result.or(parameter, ASTParameterDependencies::Any);
    }

    already_checked.insert(index.clone(), result.clone());

    result = ASTFunctionsDependencies::new();

    let mut calls = Vec::new();

    function_calls(
        &mut calls,
        function,
        module_namespace,
        module_id,
        ast_type_check,
    );

    for parameter in parameters_with_generic_type.iter() {
        let mut parameter_dependencies = ASTParameterDependencies::Any;
        debug_i!("analyzing parameter {parameter}");
        indent!();

        for call in calls.iter() {
            if let Some(call_type_check_entry) = ast_type_check.result.get(&ASTIndex::new(
                module_namespace.clone(),
                module_id.clone(),
                call.position.clone(),
            )) {
                match call_type_check_entry.info() {
                    ASTTypeCheckInfo::Call(_, vec) => {
                        for (signature, index) in vec.iter() {
                            debug_i!("call to function {signature}");
                            indent!();

                            for (i, e) in call.parameters.iter().enumerate() {
                                let parameter_type = signature.parameters_types.get(i).unwrap();
                                /*parameter
                                    .ast_type
                                    .fix_generics(&format!("{}_{}", module_namespace.0, function.name));
                                */

                                let e_index = ASTIndex::new(
                                    module_namespace.clone(),
                                    module_id.clone(),
                                    e.position().clone(),
                                );
                                if let Some(e_entry) = ast_type_check.result.get(&e_index) {
                                    if let Some(filter) = e_entry.filter() {
                                        match filter {
                                            ASTTypeFilter::Exact(asttype, module_info) => {
                                                if asttype.is_generic() {
                                                    debug_i!("asttype generic {asttype}");
                                                    indent!();

                                                    if let Ok(rgt) = ASTTypeChecker::resolve_generic_types_from_effective_type(&parameter_type, asttype) {
                                                        if rgt.len() > 0 {
                                                        debug_i!("resolved generic types {rgt}");
                                                        }
                                                    }

                                                    match ast_type_check
                                                        .container()
                                                        .function(index)
                                                        .unwrap()
                                                    {
                                                        ASTFunctionType::Standard(
                                                            inner_function,
                                                        ) => {
                                                            let deps = function_dependencies_inner(
                                                                inner_function,
                                                                index.module_namespace(),
                                                                index.module_id(),
                                                                ast_type_check,
                                                                already_checked,
                                                            );

                                                            if let Some(par_dependencies) = deps
                                                                .get(
                                                                    &inner_function
                                                                        .parameters
                                                                        .get(i)
                                                                        .unwrap()
                                                                        .name,
                                                                )
                                                            {
                                                                if let ASTParameterDependencies::Precise(ref found_types) = par_dependencies {
                                                                    for ft in found_types.iter() {
                                                                        debug_i!("found_type {ft}");
                                                                    }
                                                                }
                                                            } else {
                                                                // TODO
                                                            }
                                                        }
                                                        _ => {}
                                                    }
                                                    dedent!();
                                                } else {
                                                }
                                            }
                                            ASTTypeFilter::Any => {}
                                            ASTTypeFilter::Lambda(_, asttype_filter) => {}
                                        }
                                        let signature_parameter_type =
                                            signature.parameters_types.get(i).unwrap();
                                        if signature_parameter_type.is_generic() {}
                                    }
                                }
                            }

                            dedent!();
                        }
                    }
                    _ => {}
                }
            }

            /*
            if let Some((i, _)) = call.parameters.iter().enumerate().find(|(_, it)| {
                if let ASTExpression::ValueRef(name, _) = it {
                    name == &parameter.name
                } else {
                    false
                }
            }) {
                let mut parameter_dependencies_for_call = ASTParameterDependencies::None;

                debug_i!("call to '{}' for generic parameter {} as parameter {i}", call.function_name, parameter);
                indent!();
                if let Some(call_type_check_entry) = ast_type_check.result.get(&ASTIndex::new(
                    module_namespace.clone(),
                    module_id.clone(),
                    call.position.clone(),
                )) {
                    match call_type_check_entry.info() {
                        ASTTypeCheckInfo::Call(_, vec) => {
                            for (signature, index) in vec.iter() {
                                debug_i!("call to function {signature}");
                                indent!();
                                let signature_parameter_type =
                                    signature.parameters_types.get(i).unwrap();
                                if signature_parameter_type.is_generic() {
                                    match ast_type_check.container().function(index).unwrap() {
                                        ASTFunctionType::Standard(inner_function) => {
                                            let deps = function_dependencies_inner(
                                                inner_function,
                                                index.module_namespace(),
                                                index.module_id(),
                                                ast_type_check,
                                                already_checked,
                                            );

                                            if let Some(inner_types) = deps.get(
                                                &inner_function.parameters.get(i).unwrap().name,
                                            ) {
                                                match inner_types {
                                                    ASTParameterDependencies::Any => {
                                                        if !signature_parameter_type
                                                            .is_strictly_generic()
                                                        {
                                                            parameter_dependencies_for_call = parameter_dependencies_for_call.or(
                                                                &ASTParameterDependencies::precise(
                                                                    vec![signature_parameter_type
                                                                        .clone()],
                                                                ),
                                                            );
                                                        }
                                                    }
                                                    ASTParameterDependencies::None => {

                                                    },
                                                    ASTParameterDependencies::Precise(hash_set) => {
                                                        let mut resolved_types = Vec::new();

                                                        if let Ok(resolved_generic_types) =
                                                            ASTTypeChecker::resolve_generic_types_from_effective_type(&parameter.ast_type,
                                                            signature_parameter_type) {

                                                            let resolved_type = ASTTypeChecker::substitute(
                                                                &parameter.ast_type,
                                                                &resolved_generic_types,
                                                            ).unwrap_or(parameter.ast_type.clone());

                                                            for precise_type in hash_set.iter() {
                                                                if let Ok(resolved_generic_types) =
                                                    ASTTypeChecker::resolve_generic_types_from_effective_type(&resolved_type,
                                                    precise_type) {
                                                                    if let Some(t) = ASTTypeChecker::substitute(&resolved_type, &resolved_generic_types) {
                                                                        resolved_types.push(t);
                                                                    }
                                                                }
                                                            }
                                                        } else {
                                                            debug_i!("cannot resolve_generic_types_from_effective_type {} {}", parameter.ast_type, signature_parameter_type);
                                                        }

                                                        parameter_dependencies_for_call = parameter_dependencies_for_call.or(&ASTParameterDependencies::precise(resolved_types));

                                                    }
                                                }
                                            } else if !signature_parameter_type
                                                .is_strictly_generic()
                                            {
                                                parameter_dependencies_for_call =
                                                parameter_dependencies_for_call.or(&ASTParameterDependencies::precise(
                                                        vec![signature_parameter_type.clone()],
                                                    ));
                                            }
                                        }
                                        ASTFunctionType::Builtin => {} // TODO
                                    }
                                } else {
                                    parameter_dependencies_for_call = parameter_dependencies_for_call.or(&ASTParameterDependencies::precise(vec![
                                        signature_parameter_type.clone(),
                                    ]));
                                }
                                dedent!();
                            }
                        }
                        _ => {}
                    }
                }
                debug_i!("parameter dependencies for call {parameter_dependencies_for_call}");
                parameter_dependencies = parameter_dependencies.and(&parameter_dependencies_for_call);
                dedent!();
            }
            */
        }
        debug_i!("parameter dependencies {parameter_dependencies}");
        dedent!();
        result.or(&parameter, parameter_dependencies);
    }
    debug_i!("result {result}");

    dedent!();

    already_checked.insert(index, result.clone());

    result
}

fn function_calls<'a>(
    calls: &mut Vec<&'a ASTFunctionCall>,
    function: &'a ASTFunctionDef,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    ast_type_check: &ASTTypeChecker,
) {
    match &function.body {
        ASTFunctionBody::RASMBody(body) => {
            statements(calls, body, module_namespace, module_id, ast_type_check);
        }
        ASTFunctionBody::NativeBody(_) => {} // TODO
    }
}

fn statements<'a>(
    calls: &mut Vec<&'a ASTFunctionCall>,
    statements: &'a Vec<ASTStatement>,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    ast_type_check: &ASTTypeChecker,
) {
    for statement in statements.iter() {
        match statement {
            ASTStatement::Expression(expr) => {
                expr_calls(calls, expr, module_namespace, module_id, ast_type_check)
            }
            ASTStatement::LetStatement(_, expr, _, astposition) => {
                expr_calls(calls, expr, module_namespace, module_id, ast_type_check)
            }
        }
    }
}

fn expr_calls<'a>(
    calls: &mut Vec<&'a ASTFunctionCall>,
    expr: &'a ASTExpression,
    module_namespace: &ModuleNamespace,
    module_id: &ModuleId,
    ast_type_check: &ASTTypeChecker,
) {
    match &expr {
        ASTExpression::ASTFunctionCallExpression(call) => {
            calls.push(call);

            for expr in call.parameters.iter() {
                expr_calls(calls, expr, module_namespace, module_id, ast_type_check);
            }
        }
        ASTExpression::Lambda(lambda_def) => {
            // TODO
            let t = ast_type_check.result.get(&ASTIndex::new(
                module_namespace.clone(),
                module_id.clone(),
                lambda_def.position.clone(),
            ));

            debug_i!("type of lambda {}", OptionDisplay(&t));

            statements(
                calls,
                &lambda_def.body,
                module_namespace,
                module_id,
                ast_type_check,
            );
        } // TODO
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use rasm_utils::OptionDisplay;

    use crate::{
        codegen::{c::options::COptions, compile_target::CompileTarget, val_context::ValContext},
        project::RasmProjectRunType,
        type_check::{
            ast_type_checker_from_project, functions_dependencies::function_dependencies,
            test_utils::project_and_container, tests::init_log,
        },
    };

    use super::ASTFunctionsDependencies;

    #[test]
    pub fn test1() {
        let deps = get_deps("functions_dependencies.rasm", "v");

        assert_eq!("i32", &deps);
    }

    #[test]
    pub fn test2() {
        let deps = get_deps("functions_dependencies2.rasm", "v");

        assert_eq!(
            "Iter<stdlib_iter_add:T>, Vec<stdlib_vec_add:T>, f32, i32, str",
            &deps
        );
    }

    #[test]
    pub fn test3() {
        let deps = get_deps("functions_dependencies3.rasm", "v");

        assert_eq!(
            "Iter<stdlib_iter_add:T>, Vec<stdlib_vec_add:T>, f32, i32, str",
            &deps
        );
    }

    #[test]
    pub fn test4() {
        let deps = get_deps("functions_dependencies4.rasm", "v");

        assert_eq!(
            "Iter<stdlib_iter_add:T>, Vec<stdlib_vec_add:T>, f32, i32, str",
            &deps
        );
    }

    fn get_deps(file: &str, param_name: &str) -> String {
        init_log();
        let base_path = "resources/test/functions_dependencies";
        let deps = get_first_function_dependencies(&format!("{base_path}/{file}"));

        format!("{}", OptionDisplay(&deps.get(param_name)))
    }

    fn get_first_function_dependencies(file_path: &str) -> ASTFunctionsDependencies {
        let target = CompileTarget::C(COptions::default());
        let (project, container) = project_and_container(&target, file_path);

        let (module, _errors, info) = project.get_module(Path::new(file_path), &target).unwrap();

        let function = module.functions.first().unwrap();

        let type_checker =
            ast_type_checker_from_project(&project, RasmProjectRunType::Main, &target, &container);

        function_dependencies(
            function,
            &info.module_namespace(),
            &info.module_id(),
            &type_checker,
        )
    }
}
