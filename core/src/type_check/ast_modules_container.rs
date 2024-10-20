use std::{collections::HashMap, fmt::Display, iter::zip};

use itertools::Itertools;

use crate::{
    parser::{
        ast::{ASTFunctionSignature, ASTModule, ASTStatement, ASTType, BuiltinTypeKind},
        builtin_functions::BuiltinFunctions,
    },
    utils::OptionDisplay,
};

pub type ModuleId = String;
pub type ModuleSource = String;

struct ASTModuleEntry {
    module: ASTModule,
    source: ModuleSource,
}

pub struct ASTFunctionSignatureEntry {
    pub signature: ASTFunctionSignature,
    pub id: ModuleId,
    pub source: ModuleSource,
}

impl ASTFunctionSignatureEntry {
    pub fn new(signature: ASTFunctionSignature, id: ModuleId, source: ModuleSource) -> Self {
        Self {
            signature,
            id,
            source,
        }
    }
}

impl Display for ASTFunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let generics = if self.generics.is_empty() {
            ""
        } else {
            &format!("<{}>", self.generics.iter().join(", "))
        };

        write!(
            f,
            "{}{}({})",
            self.name,
            generics,
            self.parameters_types
                .iter()
                .map(|it| format!("{it}"))
                .join(", ")
        )?;
        if !self.return_type.is_unit() {
            write!(f, " -> {}", self.return_type)?;
        }
        Ok(())
    }
}

impl ASTModuleEntry {
    fn new(module: ASTModule, source: ModuleSource) -> Self {
        Self { module, source }
    }
}

pub struct ASTModulesContainer {
    signatures: HashMap<String, Vec<ASTFunctionSignatureEntry>>,
}

impl ASTModulesContainer {
    pub fn new() -> Self {
        Self {
            signatures: HashMap::new(),
        }
    }

    pub fn add(
        &mut self,
        module: ASTModule,
        id: ModuleId,
        source: ModuleSource,
        add_builtin: bool,
    ) {
        if add_builtin {
            if !module.enums.is_empty() {
                for enum_def in module.enums.iter() {
                    for signature in BuiltinFunctions::enum_signatures(enum_def) {
                        let signatures = self
                            .signatures
                            .entry(signature.name.clone())
                            .or_insert(Vec::new());
                        signatures.push(ASTFunctionSignatureEntry::new(
                            signature.fix_generics(&id),
                            id.clone(),
                            source.clone(),
                        ));
                    }
                }
            }

            if !module.structs.is_empty() {
                for struct_def in module.structs.iter() {
                    for signature in BuiltinFunctions::struct_signatures(struct_def) {
                        let signatures = self
                            .signatures
                            .entry(signature.name.clone())
                            .or_insert(Vec::new());
                        signatures.push(ASTFunctionSignatureEntry::new(
                            signature.fix_generics(&id),
                            id.clone(),
                            source.clone(),
                        ));
                    }
                }
            }
        }

        for signature in module.functions.iter().map(|it| it.signature()) {
            let signatures = self
                .signatures
                .entry(signature.name.clone())
                .or_insert(Vec::new());
            signatures.push(ASTFunctionSignatureEntry::new(
                signature.fix_generics(&id),
                id.clone(),
                source.clone(),
            ));
        }
    }

    /*
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
    */

    pub fn find_call_vec(
        &self,
        function_to_call: &str,
        parameter_types_filter: &Vec<FunctionTypeFilter>,
        return_type_filter: Option<&ASTType>,
        function_call_module_id: &ModuleId,
    ) -> Vec<&ASTFunctionSignatureEntry> {
        if let Some(signatures) = self.signatures.get(function_to_call) {
            let result = signatures
                .iter()
                .filter(|entry| {
                    entry.signature.parameters_types.len() == parameter_types_filter.len()
                })
                .filter(|entry| {
                    entry.signature.modifiers.public || &entry.id == function_call_module_id
                })
                .filter(|entry| {
                    zip(parameter_types_filter, &entry.signature.parameters_types).all(
                        |(filter, parameter)| filter.is_compatible(&parameter, &entry.id, self),
                    )
                })
                .collect::<Vec<_>>();

            if result.len() > 1 {
                // TODO return type filter
                let functions_with_all_non_generic = result
                    .iter()
                    .cloned()
                    .filter(|it| {
                        it.signature
                            .parameters_types
                            .iter()
                            .all(|f| !f.is_strictly_generic())
                    })
                    .collect::<Vec<_>>();
                let all_filters_are_not_generic =
                    parameter_types_filter.iter().all(|it| match it {
                        FunctionTypeFilter::Exact(ast_type, _) => {
                            !matches!(ast_type, ASTType::Generic(..))
                        }
                        FunctionTypeFilter::Any => false,
                        FunctionTypeFilter::Lambda(..) => true,
                    });
                if functions_with_all_non_generic.len() == 1 && all_filters_are_not_generic {
                    functions_with_all_non_generic
                } else {
                    result
                }
            } else {
                result
            }
        } else {
            Vec::new()
        }
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
                    if let BuiltinTypeKind::Lambda {
                        parameters: a_p,
                        return_type: a_rt,
                    } = a_kind
                    {
                        if let BuiltinTypeKind::Lambda {
                            parameters: w_p,
                            return_type: wrt,
                        } = with_kind
                        {
                            // TODO
                            a_p.len() == w_p.len()
                        } else {
                            false
                        }
                    } else {
                        a_kind == with_kind // TODO lambda
                    }
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
                position: _,
            } => {
                if let ASTType::Custom {
                    name: with_name,
                    param_types: with_param_types,
                    position: _,
                } = with_type
                {
                    a_name == with_name // TODO namespace
                        && a_param_types.len() == with_param_types.len()
                        && zip(a_param_types, with_param_types)
                            .all(|(a_pt, w_pt)| self.is_equals(a_pt, an_id, w_pt, with_id))
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

#[derive(Clone)]
pub enum FunctionTypeFilter {
    Exact(ASTType, ModuleId),
    Any,
    Lambda(usize, Option<Box<FunctionTypeFilter>>),
}

impl Display for FunctionTypeFilter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionTypeFilter::Exact(asttype, _) => write!(f, "Exact({asttype})"),
            FunctionTypeFilter::Any => f.write_str("Any"),
            FunctionTypeFilter::Lambda(n, function_type_filter) => {
                write!(f, "Lambda({n}, {})", OptionDisplay(&function_type_filter))
            }
        }
    }
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
                    position: _,
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
        parser::ast::{ASTPosition, ASTType, BuiltinTypeKind},
        project::RasmProject,
    };

    use super::{ASTModulesContainer, FunctionTypeFilter};

    #[test]
    pub fn test_add() {
        let container = sut_from_project("../rasm/resources/examples/breakout");

        let functions = container.find_call_vec(
            "add",
            &vec![
                exact_builtin(BuiltinTypeKind::I32),
                exact_builtin(BuiltinTypeKind::I32),
            ],
            None,
            &String::new(),
        );
        assert_eq!(1, functions.len());
    }

    #[test]
    pub fn test_match() {
        let container = sut_from_project("../rasm/resources/examples/breakout");

        let functions = container.find_call_vec(
            "match",
            &vec![
                exact_custom("Option", vec![ASTType::Builtin(BuiltinTypeKind::I32)]),
                exact_builtin(BuiltinTypeKind::Lambda {
                    parameters: vec![ASTType::Builtin(BuiltinTypeKind::I32)],
                    return_type: Box::new(ASTType::Generic(ASTPosition::none(), "T".to_owned())),
                }),
                exact_builtin(BuiltinTypeKind::Lambda {
                    parameters: vec![],
                    return_type: Box::new(ASTType::Generic(ASTPosition::none(), "T".to_owned())),
                }),
            ],
            None,
            &String::new(),
        );
        assert_eq!(1, functions.len());
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
            /*
            println!(
                "adding module {}",
                OptionDisplay(&info.path.clone().map(|it| it.to_string_lossy().to_string()))
            );
            */
            container.add(
                module,
                info.namespace.safe_name(),
                info.path
                    .as_ref()
                    .map(|it| it.to_string_lossy().to_string())
                    .unwrap_or(String::new())
                    .to_string(),
                false, // modules fromRasmProject contains already builtin functions
            );
        }

        container
    }

    fn exact_builtin(kind: BuiltinTypeKind) -> FunctionTypeFilter {
        FunctionTypeFilter::Exact(ASTType::Builtin(kind), String::new())
    }

    fn exact_custom(name: &str, param_types: Vec<ASTType>) -> FunctionTypeFilter {
        FunctionTypeFilter::Exact(
            ASTType::Custom {
                name: name.to_owned(),
                param_types,
                position: ASTPosition::none(),
            },
            String::new(),
        )
    }
}
