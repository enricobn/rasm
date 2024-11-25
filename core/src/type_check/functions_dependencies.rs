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
use rasm_utils::{debug_i, dedent, indent, HashMapDisplay, SliceDisplay};

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

pub struct ASTFunctionDependencies {
    parameters: HashMap<String, ASTParameterDependencies>,
}

impl ASTFunctionDependencies {
    fn new() -> Self {
        Self {
            parameters: HashMap::new(),
        }
    }

    fn and(&mut self, parameter: &ASTParameterDef, types: ASTParameterDependencies) {
        debug_i!("before and {}", HashMapDisplay(&self.parameters));
        indent!();
        debug_i!("and {types}");
        let pd = self
            .parameters
            .entry(parameter.name.clone())
            .or_insert(ASTParameterDependencies::Any)
            .clone();
        self.parameters
            .insert(parameter.name.clone(), pd.and(&types));
        dedent!();
        debug_i!("after and {}", HashMapDisplay(&self.parameters));
    }

    fn or(&mut self, parameter: &ASTParameterDef, types: ASTParameterDependencies) {
        debug_i!("before or {}", HashMapDisplay(&self.parameters));
        indent!();
        debug_i!("or {types}");
        let pd = self
            .parameters
            .entry(parameter.name.clone())
            .or_insert(ASTParameterDependencies::None)
            .clone();
        self.parameters
            .insert(parameter.name.clone(), pd.or(&types));
        dedent!();
        debug_i!("after or {}", HashMapDisplay(&self.parameters));
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
) -> ASTFunctionDependencies {
    debug_i!("function_dependencies {function}");
    indent!();

    let mut result = ASTFunctionDependencies::new();

    let function_calls = function_calls(function);

    let parameters_with_generic_type = function
        .parameters
        .iter()
        .filter(|it| it.ast_type.is_generic())
        .collect::<Vec<_>>();

    for parameter in parameters_with_generic_type.iter() {
        for call in function_calls.iter() {
            if let Some((i, _)) = call.parameters.iter().enumerate().find(|(_, it)| {
                if let ASTExpression::ValueRef(name, _) = it {
                    name == &parameter.name
                } else {
                    false
                }
            }) {
                debug_i!("parameter {}", parameter.name);
                indent!();
                if let Some(r) = ast_type_check.result.get(&ASTIndex::new(
                    module_namespace.clone(),
                    module_id.clone(),
                    call.position.clone(),
                )) {
                    let mut types = ASTParameterDependencies::None;
                    match r.info() {
                        ASTTypeCheckInfo::Call(_, vec) => {
                            for (signature, index) in vec.iter() {
                                debug_i!("call to {signature}");
                                indent!();
                                let t = signature.parameters_types.get(i).unwrap();
                                if t.is_generic() {
                                    match ast_type_check.container().function(index).unwrap() {
                                        ASTFunctionType::Standard(function) => {
                                            let deps = function_dependencies(
                                                function,
                                                index.module_namespace(),
                                                index.module_id(),
                                                ast_type_check,
                                            );

                                            if let Some(inner_types) =
                                                deps.get(&function.parameters.get(i).unwrap().name)
                                            {
                                                types = types.or(inner_types);
                                            }
                                        }
                                        ASTFunctionType::Builtin => {} // TODO
                                    }
                                } else {
                                    let mut hs = HashSet::new();
                                    hs.insert(t.clone());
                                    types = types.or(&ASTParameterDependencies::Precise(hs))
                                }
                                dedent!();
                            }
                        }
                        _ => {}
                    }
                    result.and(parameter, types);
                }
                dedent!();
            }
        }
    }

    dedent!();

    result
}

fn function_calls(function: &ASTFunctionDef) -> Vec<&ASTFunctionCall> {
    let mut result = Vec::new();

    match &function.body {
        ASTFunctionBody::RASMBody(body) => {
            result.append(&mut statements(body));
        }
        ASTFunctionBody::NativeBody(_) => {} // TODO
    }

    result
}

fn statements(statements: &Vec<ASTStatement>) -> Vec<&ASTFunctionCall> {
    let mut result = Vec::new();

    for statement in statements.iter() {
        match statement {
            ASTStatement::Expression(expr) => result.append(&mut expr_calls(expr)),
            ASTStatement::LetStatement(_, expr, _, astposition) => {
                result.append(&mut expr_calls(expr))
            }
        }
    }

    result
}

fn expr_calls(expr: &ASTExpression) -> Vec<&ASTFunctionCall> {
    match &expr {
        ASTExpression::ASTFunctionCallExpression(call) => {
            let mut result = vec![call];

            for expr in call.parameters.iter() {
                result.append(&mut expr_calls(expr));
            }

            result
        }
        ASTExpression::Lambda(lambda_def) => Vec::new(), // TODO
        _ => Vec::new(),
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

    use super::ASTFunctionDependencies;

    #[test]
    pub fn test() {
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

    fn get_first_function_dependencies(file_path: &str) -> ASTFunctionDependencies {
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
