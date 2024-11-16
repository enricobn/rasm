use std::{collections::HashMap, fmt::Display, iter::zip};

use rasm_utils::OptionDisplay;

use rasm_parser::{
    catalog::{ModuleId, ModuleInfo, ModuleNamespace},
    parser::{
        ast::{
            ASTEnumDef, ASTFunctionSignature, ASTModule, ASTPosition, ASTStructDef, ASTType,
            ASTTypeDef, BuiltinTypeKind,
        },
        builtin_functions::BuiltinFunctions,
    },
};

pub struct ASTFunctionSignatureEntry {
    pub signature: ASTFunctionSignature,
    pub namespace: ModuleNamespace,
    pub module_id: ModuleId,
    pub position: ASTPosition,
}

impl ASTFunctionSignatureEntry {
    pub fn new(
        signature: ASTFunctionSignature,
        namespace: ModuleNamespace,
        module_id: ModuleId,
        position: ASTPosition,
    ) -> Self {
        Self {
            signature,
            namespace,
            module_id,
            position,
        }
    }
}

pub struct ASTModulesContainer {
    struct_defs: HashMap<String, Vec<(ModuleInfo, ASTStructDef)>>,
    enum_defs: HashMap<String, Vec<(ModuleInfo, ASTEnumDef)>>,
    type_defs: HashMap<String, Vec<(ModuleInfo, ASTTypeDef)>>,
    signatures: HashMap<String, Vec<ASTFunctionSignatureEntry>>,
}

impl ASTModulesContainer {
    pub fn new() -> Self {
        Self {
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            type_defs: HashMap::new(),
            signatures: HashMap::new(),
        }
    }

    pub fn add(
        &mut self,
        module: &ASTModule,
        namespace: ModuleNamespace,
        module_id: ModuleId,
        add_builtin: bool,
    ) {
        if add_builtin {
            for enum_def in module.enums.iter() {
                for signature in BuiltinFunctions::enum_signatures(enum_def) {
                    let signatures = self
                        .signatures
                        .entry(signature.name.clone())
                        .or_insert(Vec::new());
                    signatures.push(ASTFunctionSignatureEntry::new(
                        signature.fix_generics(&namespace.0),
                        namespace.clone(),
                        module_id.clone(),
                        ASTPosition::none(), // TODO I don't have the position of the signature
                    ));
                }
            }

            for struct_def in module.structs.iter() {
                for signature in BuiltinFunctions::struct_signatures(struct_def) {
                    let signatures = self
                        .signatures
                        .entry(signature.name.clone())
                        .or_insert(Vec::new());
                    signatures.push(ASTFunctionSignatureEntry::new(
                        signature.fix_generics(&namespace.0),
                        namespace.clone(),
                        module_id.clone(),
                        ASTPosition::none(), // TODO I don't have the position of the signature
                    ));
                }
            }
        }

        for enum_def in module.enums.iter() {
            let enum_defs = self
                .enum_defs
                .entry(enum_def.name.clone())
                .or_insert(Vec::new());
            enum_defs.push((
                ModuleInfo::new(namespace.clone(), module_id.clone()),
                enum_def.clone(),
            ));
        }

        for struct_def in module.structs.iter() {
            let struct_defs = self
                .struct_defs
                .entry(struct_def.name.clone())
                .or_insert(Vec::new());
            struct_defs.push((
                ModuleInfo::new(namespace.clone(), module_id.clone()),
                struct_def.clone(),
            ));
        }

        for type_def in module.types.iter() {
            let type_defs = self
                .type_defs
                .entry(type_def.name.clone())
                .or_insert(Vec::new());
            type_defs.push((
                ModuleInfo::new(namespace.clone(), module_id.clone()),
                type_def.clone(),
            ));
        }

        for function in module.functions.iter() {
            let signature = function.signature();
            let signatures = self
                .signatures
                .entry(signature.name.clone())
                .or_insert(Vec::new());
            signatures.push(ASTFunctionSignatureEntry::new(
                signature.fix_generics(&namespace.0),
                namespace.clone(),
                module_id.clone(),
                function.position.clone(),
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
        parameter_types_filter: &Vec<ASTTypeFilter>,
        return_type_filter: Option<&ASTType>,
        function_call_module_namespace: &ModuleNamespace,
    ) -> Vec<&ASTFunctionSignatureEntry> {
        if let Some(signatures) = self.signatures.get(function_to_call) {
            let result = signatures
                .iter()
                .filter(|entry| {
                    entry.signature.parameters_types.len() == parameter_types_filter.len()
                })
                .filter(|entry| {
                    entry.signature.modifiers.public
                        || &entry.namespace == function_call_module_namespace
                })
                .filter(|entry| {
                    zip(parameter_types_filter, &entry.signature.parameters_types).all(
                        |(filter, parameter)| {
                            filter.is_compatible(&parameter, &entry.namespace, self)
                        },
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
                        ASTTypeFilter::Exact(ast_type, _) => !ast_type.is_strictly_generic(),
                        ASTTypeFilter::Any => false,
                        ASTTypeFilter::Lambda(..) => true,
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

    pub fn signatures(&self) -> Vec<&ASTFunctionSignatureEntry> {
        self.signatures.values().flatten().collect::<Vec<_>>()
    }

    pub fn get_enum_def(
        &self,
        from_module_id: &ModuleNamespace,
        name: &str,
    ) -> Option<&(ModuleInfo, ASTEnumDef)> {
        self.enum_defs.get(name).and_then(|it| {
            it.iter()
                .find(|(info, e)| e.modifiers.public || info.namespace() == from_module_id)
        })
    }

    pub fn get_struct_def(
        &self,
        from_module_id: &ModuleNamespace,
        name: &str,
    ) -> Option<&(ModuleInfo, ASTStructDef)> {
        self.struct_defs.get(name).and_then(|it| {
            it.iter()
                .find(|(info, e)| e.modifiers.public || info.namespace() == from_module_id)
        })
    }

    pub fn get_type_def(
        &self,
        from_module_id: &ModuleNamespace,
        name: &str,
    ) -> Option<&(ModuleInfo, ASTTypeDef)> {
        self.type_defs.get(name).and_then(|it| {
            it.iter()
                .find(|(info, e)| e.modifiers.public || info.namespace() == from_module_id)
        })
    }

    fn is_equals(
        &self,
        a_type: &ASTType,
        an_id: &ModuleNamespace,
        with_type: &ASTType,
        with_id: &ModuleNamespace,
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

#[derive(Debug, Clone)]
pub enum ASTTypeFilter {
    Exact(ASTType, ModuleInfo),
    Any,
    Lambda(usize, Option<Box<ASTTypeFilter>>),
}

impl Display for ASTTypeFilter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypeFilter::Exact(asttype, _) => write!(f, "Exact({asttype})"),
            ASTTypeFilter::Any => f.write_str("Any"),
            ASTTypeFilter::Lambda(n, function_type_filter) => {
                write!(f, "Lambda({n}, {})", OptionDisplay(&function_type_filter))
            }
        }
    }
}

impl ASTTypeFilter {
    pub fn is_compatible(
        &self,
        ast_type: &ASTType,
        module_id: &ModuleNamespace,
        container: &ASTModulesContainer,
    ) -> bool {
        match self {
            ASTTypeFilter::Exact(f_ast_type, f_module_info) => {
                container.is_equals(ast_type, module_id, f_ast_type, f_module_info.namespace())
            }
            ASTTypeFilter::Any => true,
            ASTTypeFilter::Lambda(par_len, return_type_filter) => match ast_type {
                ASTType::Builtin(builtin_type_kind) => match builtin_type_kind {
                    BuiltinTypeKind::Lambda {
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

    use rasm_parser::parser::ast::{ASTPosition, ASTType, BuiltinTypeKind};

    use crate::{
        codegen::{c::options::COptions, compile_target::CompileTarget, statics::Statics},
        commandline::CommandLineOptions,
        project::RasmProject,
        type_check::ast_modules_container::ModuleNamespace,
    };

    use super::{ASTModulesContainer, ASTTypeFilter, ModuleId, ModuleInfo};

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
            &ModuleNamespace(String::new()),
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
            &ModuleNamespace(String::new()),
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
                &module,
                ModuleNamespace(info.namespace.safe_name()),
                ModuleId(
                    info.path
                        .as_ref()
                        .map(|it| it.to_string_lossy().to_string())
                        .unwrap_or(String::new())
                        .to_string(),
                ),
                false, // modules fromRasmProject contains already builtin functions
            );
        }

        container
    }

    fn exact_builtin(kind: BuiltinTypeKind) -> ASTTypeFilter {
        ASTTypeFilter::Exact(ASTType::Builtin(kind), ModuleInfo::global())
    }

    fn exact_custom(name: &str, param_types: Vec<ASTType>) -> ASTTypeFilter {
        ASTTypeFilter::Exact(
            ASTType::Custom {
                name: name.to_owned(),
                param_types,
                position: ASTPosition::none(),
            },
            ModuleInfo::global(),
        )
    }
}
