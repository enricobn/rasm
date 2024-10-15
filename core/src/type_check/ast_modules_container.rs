use std::{collections::HashMap, iter::zip};

use crate::parser::ast::{ASTFunctionDef, ASTModule, ASTType};

pub type ModuleId = String;

struct ASTModuleEntry {
    module: ASTModule,
    source: String,
}

impl ASTModuleEntry {
    fn new(module: ASTModule, source: String) -> Self {
        Self { module, source }
    }
}

pub struct ASTModulesContainer {
    modules: HashMap<ModuleId, Vec<ASTModuleEntry>>,
}

impl ASTModulesContainer {
    fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    fn add(&mut self, module: ASTModule, id: ModuleId, source: String) -> Result<(), String> {
        let modules = self.modules.entry(id).or_insert(Vec::new());
        modules.push(ASTModuleEntry::new(module, source));
        Ok(())
    }

    fn get_all<'a, T>(
        &'a self,
        mapper: &'a dyn Fn(&'a ASTModule) -> &'a Vec<T>,
    ) -> impl Iterator<Item = (&'a T, ModuleId)>
    where
        T: 'a,
    {
        self.modules.iter().flat_map(|(id, entries)| {
            entries
                .iter()
                .flat_map(|entry| mapper(&entry.module).iter().map(|it| (it, id.clone())))
        })
    }

    fn find_call_vec(
        &self,
        function_to_call: &str,
        parameter_types_filter: &Vec<FunctionTypeFilter>,
        return_type_filter: Option<&ASTType>,
    ) -> Result<Vec<&ASTFunctionDef>, String> {
        let mut result = Vec::new();
        let possible_functions = self
            .get_all(&|module| &module.functions)
            .filter(|(function, id)| {
                function.parameters.len() == parameter_types_filter.len()
                    && function.name == function_to_call
            })
            .filter(|(function, id)| {
                zip(parameter_types_filter, &function.parameters)
                    .all(|(filter, parameter)| filter.is_compatible(&parameter.ast_type, id, self))
            })
            .collect::<Vec<_>>();

        result.extend(possible_functions.iter().map(|(f, id)| f));

        Ok(result)
    }

    fn is_equals(
        &self,
        a_type: &ASTType,
        an_id: &ModuleId,
        with_type: &ASTType,
        with_id: &ModuleId,
    ) -> bool {
        match a_type {
            ASTType::Builtin(a_kind) => {
                if let ASTType::Builtin(with_kind) = with_type {
                    a_kind == with_kind // TODO lambda
                } else if let ASTType::Generic(_, _) = with_type {
                    true
                } else {
                    false
                }
            }
            ASTType::Generic(_, name) => true,
            ASTType::Custom {
                name: a_name,
                param_types: a_param_types,
                index: _,
            } => {
                if let ASTType::Custom {
                    name: with_name,
                    param_types: with_param_types,
                    index: _,
                } = with_type
                {
                    todo!()
                } else if let ASTType::Generic(_, _) = with_type {
                    true
                } else {
                    false
                }
            }
            ASTType::Unit => matches!(with_type, ASTType::Unit),
        }
    }
}

enum FunctionTypeFilter {
    Exact(ASTType, ModuleId),
    Any,
    Lambda(usize, Option<Box<FunctionTypeFilter>>),
}

impl FunctionTypeFilter {
    pub fn is_compatible(
        &self,
        ast_type: &ASTType,
        module_id: &ModuleId,
        container: &ASTModulesContainer,
    ) -> bool {
        match self {
            FunctionTypeFilter::Exact(f_ast_type, f_module_id) => {
                container.is_equals(ast_type, module_id, f_ast_type, f_module_id)
            }
            FunctionTypeFilter::Any => true,
            FunctionTypeFilter::Lambda(par_len, return_type_filter) => match ast_type {
                ASTType::Builtin(builtin_type_kind) => match builtin_type_kind {
                    crate::parser::ast::BuiltinTypeKind::Lambda {
                        parameters,
                        return_type,
                    } => {
                        &parameters.len() == par_len
                            && return_type_filter
                                .as_ref()
                                .map(|it| it.is_compatible(&return_type, module_id, container))
                                .unwrap_or(true)
                    }
                    _ => false,
                },
                ASTType::Generic(_, _) => true,
                ASTType::Custom {
                    name: _,
                    param_types: _,
                    index: _,
                } => false,
                ASTType::Unit => false,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{env, path::PathBuf};

    use crate::{
        codegen::{c::options::COptions, compile_target::CompileTarget, statics::Statics},
        commandline::CommandLineOptions,
        parser::ast::{ASTType, BuiltinTypeKind},
        project::RasmProject,
        utils::OptionDisplay,
    };

    use super::{ASTModulesContainer, FunctionTypeFilter};

    #[test]
    pub fn test_add() {
        let container = sut_from_project("../rasm/resources/examples/breakout");

        let functions = container
            .find_call_vec(
                "add",
                &vec![
                    exact_builtin(BuiltinTypeKind::I32),
                    exact_builtin(BuiltinTypeKind::I32),
                ],
                None,
            )
            .unwrap();
        assert_eq!(2, functions.len());
    }

    fn sut_from_project(project_path: &str) -> ASTModulesContainer {
        let project = RasmProject::new(PathBuf::from(project_path));
        let target = CompileTarget::C(COptions::default());
        let mut statics = Statics::new();
        let (modules, _) = project.get_all_modules(
            &mut statics,
            false,
            &target,
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        );

        let mut container = ASTModulesContainer::new();
        for (module, info) in modules {
            match container.add(
                module,
                info.namespace.safe_name(),
                info.path
                    .as_ref()
                    .map(|it| it.to_string_lossy().to_string())
                    .unwrap_or(String::new())
                    .to_string(),
            ) {
                Ok(_) => {}
                Err(msg) => panic!(
                    "Error '{msg}' adding module {} : {}",
                    info.namespace,
                    OptionDisplay(&info.path.map(|it| it.to_string_lossy().to_string()))
                ),
            }
        }

        container
    }

    fn exact_builtin(kind: BuiltinTypeKind) -> FunctionTypeFilter {
        FunctionTypeFilter::Exact(ASTType::Builtin(kind), String::new())
    }
}
