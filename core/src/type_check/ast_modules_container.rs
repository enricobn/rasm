use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    iter::zip,
};

use rasm_utils::{debug_i, find_one, OptionDisplay, SliceDisplay};

use rasm_parser::{
    catalog::{ASTIndex, ModuleId, ModuleInfo, ModuleNamespace},
    parser::{
        ast::{
            ASTEnumDef, ASTFunctionDef, ASTFunctionSignature, ASTModule, ASTPosition, ASTStructDef,
            ASTType, ASTTypeDef, BuiltinTypeKind, CustomTypeDef,
        },
        builtin_functions::BuiltinFunctions,
    },
};

use crate::type_check::{
    ast_generic_types_resolver::ASTResolvedGenericTypes, ast_type_checker::ASTTypeChecker,
};

#[derive(Clone)]
pub struct ASTFunctionSignatureEntry {
    pub signature: ASTFunctionSignature,
    pub namespace: ModuleNamespace,
    pub module_id: ModuleId,
    pub position: ASTPosition,
    pub rank: usize,
    pub target: Option<String>,
}

impl ASTFunctionSignatureEntry {
    pub fn new(
        signature: ASTFunctionSignature,
        namespace: ModuleNamespace,
        module_id: ModuleId,
        position: ASTPosition,
        target: Option<String>,
    ) -> Self {
        let rank = Self::signature_precedence_coeff(&signature);
        Self {
            signature,
            namespace,
            module_id,
            position,
            rank,
            target,
        }
    }

    ///
    /// lower means a better precedence
    ///
    fn signature_precedence_coeff(function: &ASTFunctionSignature) -> usize {
        let generic_coeff: usize = function
            .parameters_types
            .iter()
            .map(|it| Self::generic_type_coeff(&it))
            .sum();

        generic_coeff
        /*            + if matches!(function.body, EnhASTFunctionBody::NativeBody(_)) {
            0usize
        } else {
            1usize
        }
        */
    }

    ///
    /// return a coefficient that is higher for how the type is generic
    ///
    pub fn generic_type_coeff(ast_type: &ASTType) -> usize {
        Self::generic_type_coeff_internal(ast_type, usize::MAX / 100)
    }

    fn generic_type_coeff_internal(ast_type: &ASTType, coeff: usize) -> usize {
        if ast_type.is_generic() {
            match ast_type {
                ASTType::Builtin(builtin) => {
                    if let BuiltinTypeKind::Lambda {
                        parameters: _,
                        return_type,
                    } = builtin
                    {
                        Self::generic_type_coeff(&return_type)
                    } else {
                        0
                    }
                }
                ASTType::Generic(_, _, _) => coeff,
                ASTType::Custom {
                    name: _,
                    param_types,
                    position: _,
                } => param_types
                    .iter()
                    .map(|it| Self::generic_type_coeff_internal(it, coeff / 100))
                    .sum(),
                ASTType::Unit => 0,
            }
        } else {
            0
        }
    }

    pub fn index(&self) -> ASTIndex {
        ASTIndex::new(
            self.namespace.clone(),
            self.module_id.clone(),
            self.position.clone(),
        )
    }

    pub fn module_info(&self) -> ModuleInfo {
        ModuleInfo::new(self.namespace.clone(), self.module_id.clone())
    }
}

impl Display for ASTFunctionSignatureEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.signature)
    }
}

#[derive(Clone)]
pub struct ASTModulesContainer {
    struct_defs: HashMap<String, Vec<(ModuleInfo, ASTStructDef)>>,
    enum_defs: HashMap<String, Vec<(ModuleInfo, ASTEnumDef)>>,
    type_defs: HashMap<String, Vec<(ModuleInfo, ASTTypeDef)>>,
    signatures: HashMap<String, Vec<ASTFunctionSignatureEntry>>,
    functions_by_index: HashMap<ASTIndex, ASTFunctionDef>,
    readonly_modules: HashSet<ModuleId>,
    modules: HashMap<ModuleId, (ASTModule, ModuleNamespace)>,
}

impl ASTModulesContainer {
    pub fn new() -> Self {
        Self {
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            type_defs: HashMap::new(),
            signatures: HashMap::new(),
            functions_by_index: HashMap::new(),
            readonly_modules: HashSet::new(),
            modules: HashMap::new(),
        }
    }

    pub fn add(
        &mut self,
        module: ASTModule,
        namespace: ModuleNamespace,
        module_id: ModuleId,
        add_builtin: bool,
        readonly: bool,
    ) {
        if add_builtin {
            for enum_def in module.enums.iter() {
                for (signature, position, ft) in BuiltinFunctions::enum_signatures(enum_def) {
                    let signatures = self
                        .signatures
                        .entry(signature.name.clone())
                        .or_insert(Vec::new());

                    signatures.push(ASTFunctionSignatureEntry::new(
                        signature.add_generic_prefix(&namespace.0),
                        namespace.clone(),
                        module_id.clone(),
                        ASTPosition::builtin(&position, ft),
                        Some(enum_def.name.clone()),
                    ));
                }
            }

            for struct_def in module.structs.iter() {
                for (signature, position, tf) in BuiltinFunctions::struct_signatures(struct_def) {
                    let signatures = self
                        .signatures
                        .entry(signature.name.clone())
                        .or_insert(Vec::new());

                    signatures.push(ASTFunctionSignatureEntry::new(
                        signature.add_generic_prefix(&namespace.0),
                        namespace.clone(),
                        module_id.clone(),
                        ASTPosition::builtin(&position, tf),
                        Some(struct_def.name.clone()),
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
                signature.add_generic_prefix(&namespace.0),
                namespace.clone(),
                module_id.clone(),
                function.position.clone(),
                function.target.clone(),
            ));
            let index = ASTIndex::new(
                namespace.clone(),
                module_id.clone(),
                function.position.clone(),
            );

            self.functions_by_index
                .insert(index, function.clone())
                .iter()
                .for_each(|f| {
                    panic!(
                        "already added function {f} : {} {}\n{function} : {} {}",
                        f.position,
                        OptionDisplay(&f.position.builtin),
                        function.position,
                        OptionDisplay(&function.position.builtin)
                    )
                });
        }
        if readonly {
            self.readonly_modules.insert(module_id.clone());
        }
        self.modules
            .insert(module_id.clone(), (module, namespace.clone()));
    }

    pub fn function(&self, index: &ASTIndex) -> Option<&ASTFunctionDef> {
        self.functions_by_index.get(index)
    }

    pub fn module(&self, id: &ModuleId) -> Option<&ASTModule> {
        self.modules.get(id).map(|it| &it.0)
    }

    pub fn module_namespace(&self, id: &ModuleId) -> Option<&ModuleNamespace> {
        self.modules.get(id).map(|it| &it.1)
    }

    pub fn find_call_vec(
        &self,
        function_to_call: &str,
        call_target: &Option<String>,
        parameter_types_filter: &Vec<ASTTypeFilter>,
        return_type_filter: Option<&ASTType>,
        function_call_module_namespace: &ModuleNamespace,
    ) -> Vec<&ASTFunctionSignatureEntry> {
        debug_i!(
            "find_call_vec {function_to_call} {}",
            SliceDisplay(parameter_types_filter)
        );

        let signatures = if let Some(signatures) = self.signatures.get(function_to_call) {
            signatures
        } else {
            return Vec::new();
        };

        let mut result = signatures
            .iter()
            .filter(|entry| {
                entry.signature.parameters_types.len() == parameter_types_filter.len()
                    && (entry.signature.modifiers.public
                        || &entry.namespace == function_call_module_namespace)
            })
            .filter(|entry| {
                zip(parameter_types_filter, &entry.signature.parameters_types).all(
                    |(filter, parameter)| filter.is_compatible(&parameter, &entry.namespace, self),
                )
            })
            .filter(|it| {
                call_target
                    .as_ref()
                    .map(|t| match &it.target {
                        Some(target) => t == target,
                        _ => false,
                    })
                    .unwrap_or(true)
            })
            .collect::<Vec<_>>();

        if result.len() > 1 {
            if let Some(rt) = return_type_filter {
                result = result
                    .into_iter()
                    .filter(|it| {
                        self.is_compatible(
                            rt,
                            function_call_module_namespace,
                            &it.signature.return_type,
                            &it.namespace,
                        )
                    })
                    .collect::<Vec<_>>();
            }
        }

        if result.len() > 1 {
            result = result
                .into_iter()
                .filter(|it| {
                    if it.signature.is_generic() {
                        let mut resolver = ASTResolvedGenericTypes::new();
                        let mut compatible = true;
                        for (filter, t) in
                            zip(parameter_types_filter, &it.signature.parameters_types)
                        {
                            let t = if let Some(new_t) = resolver.substitute(t) {
                                new_t
                            } else {
                                t.clone()
                            };
                            match ASTTypeChecker::resolve_type_filter(&t, filter) {
                                Ok(t_resolver) => {
                                    if resolver.extend(t_resolver).is_err() {
                                        compatible = false;
                                        break;
                                    }
                                }
                                Err(_e) => {
                                    compatible = false;
                                    break;
                                }
                            }
                        }

                        if compatible {
                            if let Some(rt) = return_type_filter {
                                if let Ok(t_resolver) =
                                ASTResolvedGenericTypes::resolve_generic_types_from_effective_type(
                                        rt, rt,
                                    )
                                {
                                    if resolver.extend(t_resolver).is_err() {
                                        compatible = false;
                                    }
                                } else {
                                    compatible = false;
                                }
                            }
                        }
                        compatible
                    } else {
                        true
                    }
                })
                .collect::<Vec<_>>();
        }

        if result.len() > 1 {
            let mut functions_by_coeff = HashMap::new();
            let mut max_coeff = 0;
            for entry in result.into_iter() {
                let mut coeff = 0;
                for (filter, t) in zip(parameter_types_filter, &entry.signature.parameters_types) {
                    let filter_coeff = filter.compatibility_coeff(&t, &entry.namespace, self);
                    if filter_coeff == 0 {
                        coeff = 0;
                        break;
                    } else {
                        coeff += filter_coeff;
                    }
                }

                if coeff != 0 {
                    if coeff > max_coeff {
                        max_coeff = coeff;
                    }
                    functions_by_coeff
                        .entry(coeff)
                        .or_insert(Vec::new())
                        .push(entry);
                }
            }

            if functions_by_coeff.len() == 0 {
                result = Vec::new();
            } else {
                result = functions_by_coeff.remove(&max_coeff).unwrap();
            }
        }

        result
    }

    pub fn signatures(&self) -> Vec<&ASTFunctionSignatureEntry> {
        self.signatures.values().flatten().collect::<Vec<_>>()
    }

    pub fn get_enum_def(
        &self,
        from_namespace: &ModuleNamespace,
        name: &str,
    ) -> Option<&(ModuleInfo, ASTEnumDef)> {
        self.enum_defs.get(name).and_then(|it| {
            find_one(it.iter(), |(info, e)| {
                e.modifiers.public || info.namespace() == from_namespace
            })
        })
    }

    pub fn enum_defs(&self) -> Vec<&(ModuleInfo, ASTEnumDef)> {
        self.enum_defs.values().flatten().collect()
    }

    pub fn get_struct_def(
        &self,
        from_module_id: &ModuleNamespace,
        name: &str,
    ) -> Option<&(ModuleInfo, ASTStructDef)> {
        self.struct_defs.get(name).and_then(|it| {
            find_one(it.iter(), |(info, e)| {
                e.modifiers.public || info.namespace() == from_module_id
            })
        })
    }

    pub fn struct_defs(&self) -> Vec<&(ModuleInfo, ASTStructDef)> {
        self.struct_defs.values().flatten().collect()
    }

    pub fn get_type_def(
        &self,
        from_module_id: &ModuleNamespace,
        name: &str,
    ) -> Option<&(ModuleInfo, ASTTypeDef)> {
        self.type_defs.get(name).and_then(|it| {
            find_one(it.iter(), |(info, e)| {
                e.modifiers.public || info.namespace() == from_module_id
            })
        })
    }

    pub fn type_defs(&self) -> Vec<&(ModuleInfo, ASTTypeDef)> {
        self.type_defs.values().flatten().collect()
    }

    pub fn is_readonly_module(&self, module_id: &ModuleId) -> bool {
        self.readonly_modules.contains(module_id)
    }

    pub fn modules(&self) -> Vec<(&ModuleId, &ModuleNamespace, &ASTModule)> {
        self.modules
            .iter()
            .map(|(id, (module, namespace))| (id, namespace, module))
            .collect()
    }

    fn is_compatible(
        &self,
        a_type: &ASTType,
        a_namespace: &ModuleNamespace,
        with_type: &ASTType,
        with_namespace: &ModuleNamespace,
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
                            a_p.len() == w_p.len()
                                && zip(a_p, w_p).all(|(a, w)| {
                                    self.is_compatible(a, a_namespace, w, with_namespace)
                                })
                                && self.is_compatible(a_rt, a_namespace, wrt, with_namespace)
                        } else {
                            false
                        }
                    } else {
                        a_kind == with_kind
                    }
                } else if let ASTType::Generic(_, _, _) = with_type {
                    true
                } else {
                    false
                }
            }
            ASTType::Generic(_, _, _) => true,
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
                    if a_name != with_name || a_param_types.len() != with_param_types.len() {
                        return false;
                    }

                    let a_real_ns =
                        if let Some(i) = self.custom_type_module_namespace(a_namespace, &a_name) {
                            i
                        } else {
                            return false;
                        };
                    let with_real_nf = if let Some(i) =
                        self.custom_type_module_namespace(with_namespace, &with_name)
                    {
                        i
                    } else {
                        return false;
                    };

                    if a_real_ns != with_real_nf {
                        return false;
                    }

                    zip(a_param_types, with_param_types).all(|(a_pt, w_pt)| {
                        self.is_compatible(a_pt, a_namespace, w_pt, with_namespace)
                    })
                } else if let ASTType::Generic(_, _, _) = with_type {
                    true
                } else {
                    false
                }
            }
            ASTType::Unit => {
                matches!(with_type, ASTType::Unit) || matches!(with_type, ASTType::Generic(_, _, _))
            }
        }
    }

    pub fn custom_type_index(
        &self,
        from_namespace: &ModuleNamespace,
        name: &str,
    ) -> Option<ASTIndex> {
        if let Some((info, def)) = self.custom_type_def(from_namespace, name) {
            Some(ASTIndex::new(
                info.namespace().clone(),
                info.id().clone(),
                def.position().clone(),
            ))
        } else {
            None
        }
    }

    pub fn custom_type_module_id(
        &self,
        from_namespace: &ModuleNamespace,
        name: &str,
    ) -> Option<&ModuleId> {
        if let Some((info, _def)) = self.get_enum_def(from_namespace, name) {
            return Some(info.id());
        } else if let Some((info, _def)) = self.get_struct_def(from_namespace, name) {
            return Some(info.id());
        } else if let Some((info, _def)) = self.get_type_def(from_namespace, name) {
            return Some(info.id());
        } else {
            return None;
        }
    }

    pub fn custom_type_module_namespace(
        &self,
        from_namespace: &ModuleNamespace,
        name: &str,
    ) -> Option<&ModuleNamespace> {
        if let Some((info, _def)) = self.get_enum_def(from_namespace, name) {
            return Some(info.namespace());
        } else if let Some((info, _def)) = self.get_struct_def(from_namespace, name) {
            return Some(info.namespace());
        } else if let Some((info, _def)) = self.get_type_def(from_namespace, name) {
            return Some(info.namespace());
        } else {
            return None;
        }
    }

    pub fn custom_type_def(
        &self,
        from_namespace: &ModuleNamespace,
        name: &str,
    ) -> Option<(&ModuleInfo, &dyn CustomTypeDef)> {
        if let Some((info, def)) = self.get_enum_def(from_namespace, name) {
            return Some((info, def));
        } else if let Some((info, def)) = self.get_struct_def(from_namespace, name) {
            return Some((info, def));
        } else if let Some((info, def)) = self.get_type_def(from_namespace, name) {
            return Some((info, def));
        } else {
            return None;
        }
    }

    pub fn remove_body(&mut self) {
        for (module, _) in self.modules.values_mut() {
            module.body.clear();
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
        module_namespace: &ModuleNamespace,
        container: &ASTModulesContainer,
    ) -> bool {
        match self {
            ASTTypeFilter::Exact(f_ast_type, f_module_info) => container.is_compatible(
                ast_type,
                module_namespace,
                f_ast_type,
                f_module_info.namespace(),
            ),
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
                                // TODO I don't like it:
                                // a lambda that returns some type can be passed to a function that takes a lambda that returns Unit.
                                // For example in a forEach (that takes a lambda that returns Unit) we can pass a lambda that returns something...
                                .map(|it| {
                                    return_type.is_unit()
                                        || it.is_compatible(
                                            &return_type,
                                            module_namespace,
                                            container,
                                        )
                                })
                                .unwrap_or(true)
                    }
                    _ => false,
                },
                ASTType::Generic(_, _, _) => true,
                ASTType::Custom {
                    name: _,
                    param_types: _,
                    position: _,
                } => false,
                ASTType::Unit => false,
            },
        }
    }

    pub fn is_generic(&self) -> bool {
        match &self {
            ASTTypeFilter::Exact(asttype, _) => asttype.is_generic(),
            ASTTypeFilter::Any => todo!(),
            ASTTypeFilter::Lambda(_, asttype_filter) => asttype_filter
                .as_ref()
                .map(|it| it.is_generic())
                .unwrap_or(false),
        }
    }

    pub fn exact(
        ast_type: ASTType,
        module_namespace: &ModuleNamespace,
        module_id: &ModuleId,
    ) -> Self {
        ASTTypeFilter::Exact(
            ast_type,
            ModuleInfo::new(module_namespace.clone(), module_id.clone()),
        )
    }

    pub fn generic_type_coeff(&self) -> Option<usize> {
        match self {
            ASTTypeFilter::Exact(t, _) => Some(ASTFunctionSignatureEntry::generic_type_coeff(t)),
            ASTTypeFilter::Lambda(_, rt) => rt.as_ref().and_then(|it| it.generic_type_coeff()),
            _ => None,
        }
    }

    pub fn compatibility_coeff(
        &self,
        ast_type: &ASTType,
        module_id: &ModuleNamespace,
        container: &ASTModulesContainer,
    ) -> u32 {
        match self {
            ASTTypeFilter::Exact(f_ast_type, f_module_info) => {
                if self.is_compatible(f_ast_type, f_module_info.namespace(), container) {
                    if f_ast_type.is_generic() {
                        500
                    } else if ast_type.is_generic() {
                        500
                    } else {
                        1000
                    }
                } else {
                    0
                }
            }
            ASTTypeFilter::Any => 1000,
            ASTTypeFilter::Lambda(par_len, return_type_filter) => match ast_type {
                ASTType::Builtin(builtin_type_kind) => match builtin_type_kind {
                    BuiltinTypeKind::Lambda {
                        parameters,
                        return_type,
                    } => {
                        if &parameters.len() == par_len
                            && return_type_filter
                                .as_ref()
                                // TODO I don't like it:
                                // a lambda that returns some type can be passed to a function that takes a lambda that returns Unit.
                                // For example in a forEach (that takes a lambda that returns Unit) we can pass a lambda that returns something...
                                .map(|it| {
                                    return_type.is_unit()
                                        || it.is_compatible(&return_type, module_id, container)
                                })
                                .unwrap_or(true)
                        {
                            1000
                        } else {
                            0
                        }
                    }
                    _ => 0,
                },
                ASTType::Generic(_, _, _) => 500,
                ASTType::Custom {
                    name: _,
                    param_types: _,
                    position: _,
                } => 0,
                ASTType::Unit => 0,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use rasm_parser::parser::ast::{ASTPosition, ASTType, BuiltinTypeKind};

    use crate::{
        codegen::{c::options::COptions, compile_target::CompileTarget, statics::Statics},
        project::{RasmProject, RasmProjectRunType},
        type_check::ast_modules_container::ModuleNamespace,
    };

    use super::{ASTModulesContainer, ASTTypeFilter, ModuleInfo};

    #[test]
    pub fn test_add() {
        let container = sut_from_project("../rasm/resources/examples/breakout");

        let functions = container.find_call_vec(
            "add",
            &None,
            &vec![
                exact_builtin(BuiltinTypeKind::I32),
                exact_builtin(BuiltinTypeKind::I32),
            ],
            None,
            &ModuleNamespace::global(),
        );
        assert_eq!(1, functions.len());
    }

    #[test]
    pub fn test_match() {
        let container = sut_from_project("../rasm/resources/examples/breakout");

        let functions = container.find_call_vec(
            "match",
            &None,
            &vec![
                exact_custom("Option", vec![ASTType::Builtin(BuiltinTypeKind::I32)]),
                exact_builtin(BuiltinTypeKind::Lambda {
                    parameters: vec![ASTType::Builtin(BuiltinTypeKind::I32)],
                    return_type: Box::new(ASTType::Generic(
                        ASTPosition::none(),
                        "T".to_owned(),
                        Vec::new(),
                    )),
                }),
                exact_builtin(BuiltinTypeKind::Lambda {
                    parameters: vec![],
                    return_type: Box::new(ASTType::Generic(
                        ASTPosition::none(),
                        "T".to_owned(),
                        Vec::new(),
                    )),
                }),
            ],
            None,
            &ModuleNamespace::global(),
        );
        assert_eq!(1, functions.len());
    }

    fn sut_from_project(project_path: &str) -> ASTModulesContainer {
        let project = RasmProject::new(PathBuf::from(project_path));
        let target = CompileTarget::C(COptions::default());
        let mut statics = Statics::new();

        let (container, _catalog, _errors) =
            project.container_and_catalog(&mut statics, &RasmProjectRunType::Main, &target, false);

        container
    }

    fn exact_builtin(kind: BuiltinTypeKind) -> ASTTypeFilter {
        ASTTypeFilter::Exact(ASTType::Builtin(kind), ModuleInfo::global())
    }

    fn exact_custom(name: &str, param_types: Vec<ASTType>) -> ASTTypeFilter {
        if name == "T" {
            panic!();
        }
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
