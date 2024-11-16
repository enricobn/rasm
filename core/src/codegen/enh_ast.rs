use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::iter::zip;
use std::ops::Deref;
use std::path::PathBuf;

use derivative::Derivative;
use rasm_parser::catalog::{ASTIndex, ModuleId, ModuleInfo, ModuleNamespace};

use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::new_type_check2::TypeCheck;
use crate::project::RasmProject;

use crate::type_check::resolved_generic_types::ResolvedGenericTypes;

use rasm_parser::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
    ASTLambdaDef, ASTModifiers, ASTModule, ASTParameterDef, ASTPosition, ASTStatement,
    ASTStructDef, ASTStructPropertyDef, ASTType, ASTTypeDef, ASTValueType, BuiltinTypeKind,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnhModuleId {
    Path(PathBuf),
    Other(String),
}

impl Display for EnhModuleId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EnhModuleId::Path(path_buf) => write!(f, "{}", path_buf.to_string_lossy()),
            EnhModuleId::Other(s) => f.write_str(s),
        }
    }
}

impl EnhModuleId {
    pub fn none() -> Self {
        EnhModuleId::Other(String::new())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnhModuleInfo {
    pub id: EnhModuleId,
    pub namespace: EnhASTNameSpace,
}

impl EnhModuleInfo {
    pub fn new(id: EnhModuleId, namespace: EnhASTNameSpace) -> Self {
        Self { id, namespace }
    }

    pub fn module_namespace(&self) -> ModuleNamespace {
        ModuleNamespace(self.namespace.safe_name())
    }

    pub fn module_id(&self) -> ModuleId {
        match &self.id {
            EnhModuleId::Path(path_buf) => ModuleId(path_buf.clone().to_string_lossy().to_string()),
            EnhModuleId::Other(s) => ModuleId(s.clone()),
        }
    }

    pub fn index(&self, position: ASTPosition) -> ASTIndex {
        ASTIndex::new(self.module_namespace(), self.module_id(), position)
    }

    pub fn module_info(&self) -> ModuleInfo {
        ModuleInfo::new(self.module_namespace(), self.module_id())
    }

    pub fn path(&self) -> Option<PathBuf> {
        match &self.id {
            EnhModuleId::Path(path_buf) => Some(path_buf.clone()),
            EnhModuleId::Other(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnhASTNameSpace {
    lib: String,
    path: String,
}

const GLOBAL_NAMESPACE: EnhASTNameSpace = EnhASTNameSpace {
    lib: String::new(),
    path: String::new(),
};

const GLOBAL_NAMESPACE_REF: &'static EnhASTNameSpace = &GLOBAL_NAMESPACE;

impl EnhASTNameSpace {
    pub fn new(lib: String, path: String) -> Self {
        if lib.is_empty() {
            panic!("lib cannot be empty");
        }
        if path.ends_with(".rasm") {
            panic!("path should not end with .rasm {path}")
        }
        if lib.ends_with(".rasm") {
            panic!("lib should not end with .rasm {lib}")
        }
        Self { lib, path }
    }

    pub fn root_namespace(project: &RasmProject) -> Self {
        let namespace_path = if let Some(p) = &project.config.package.main {
            p.strip_suffix(".rasm").unwrap().to_string()
        } else {
            String::new()
        };
        Self::new(project.config.package.name.clone(), namespace_path)
    }

    pub const fn global() -> Self {
        GLOBAL_NAMESPACE
    }

    pub const fn global_ref() -> &'static Self {
        GLOBAL_NAMESPACE_REF
    }

    pub fn safe_name(&self) -> String {
        format!("{self}").replace('/', "_").replace(':', "_")
    }

    pub fn is_core(&self) -> bool {
        self.lib == "::core"
    }
}

impl Display for EnhASTNameSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}", self.lib, self.path))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnhASTFunctionDef {
    pub original_name: String,
    pub name: String,
    pub parameters: Vec<EnhASTParameterDef>,
    pub return_type: EnhASTType,
    pub body: EnhASTFunctionBody,
    pub inline: bool,
    pub generic_types: Vec<String>,
    pub resolved_generic_types: ResolvedGenericTypes,
    pub index: EnhASTIndex,
    pub modifiers: ASTModifiers,
    pub namespace: EnhASTNameSpace,
    ///
    /// The rank is the sum of the parameter's type generic coefficient, the coefficient
    /// of <T> is higher than Option<T> that is higher than Option<List<T>>
    /// So less is the rank, best suited is the function
    pub rank: usize,
}

pub struct EnhASTFunctionSignature {
    pub name: String,
    //pub generics: Vec<String>,
    pub parameters_types: Vec<EnhASTType>,
    pub return_type: EnhASTType,
}

impl Display for EnhASTFunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let generic_types = if self.generic_types.is_empty() {
            "".into()
        } else {
            format!("<{}>", self.generic_types.join(","))
        };

        let rt = if self.return_type != EnhASTType::Unit {
            format!("{}", self.return_type)
        } else {
            "()".into()
        };

        let modifiers = if self.modifiers.public { "pub " } else { "" };

        let fun_or_asm = if let EnhASTFunctionBody::RASMBody(_) = self.body {
            "fn"
        } else {
            "native"
        };

        let args = self
            .parameters
            .iter()
            .map(|it| format!("{}", it))
            .collect::<Vec<String>>()
            .join(",");
        f.write_str(&format!(
            "{}{} {}{generic_types}({args}) -> {rt}",
            modifiers, fun_or_asm, self.name
        ))
    }
}

impl EnhASTFunctionDef {
    pub fn update_calculated_properties(&mut self) {
        self.rank = TypeCheck::function_precedence_coeff(self);
    }

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        let mut result = self;
        if let EnhASTFunctionBody::RASMBody(statements) = result.body {
            result.body = EnhASTFunctionBody::RASMBody(
                statements
                    .into_iter()
                    .map(|it| it.fix_namespaces(enhanced_module))
                    .collect(),
            )
        }
        result.parameters = result
            .parameters
            .into_iter()
            .map(|it| it.fix_namespaces(enhanced_module))
            .collect();
        result.resolved_generic_types = result
            .resolved_generic_types
            .fix_namespaces(enhanced_module);
        result.return_type = result.return_type.fix_namespaces(enhanced_module);

        result
    }

    pub fn fix_generics(self) -> Self {
        let generics_prefix = format!("{}_{}", self.namespace, self.name);
        let mut result = self;
        result.parameters = result
            .parameters
            .into_iter()
            .map(|it| it.fix_generics(&generics_prefix))
            .collect();
        result.resolved_generic_types =
            result.resolved_generic_types.fix_generics(&generics_prefix);
        result.return_type = result.return_type.fix_generics(&generics_prefix);
        result.generic_types = result
            .generic_types
            .into_iter()
            .map(|it| format!("{generics_prefix}:{it}"))
            .collect();

        result
    }

    pub fn signature(&self) -> EnhASTFunctionSignature {
        EnhASTFunctionSignature {
            name: self.name.clone(),
            parameters_types: self
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect(),
            return_type: self.return_type.clone(),
        }
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        function: ASTFunctionDef,
    ) -> Self {
        Self {
            original_name: function.name.clone(),
            name: function.name,
            parameters: EnhASTParameterDef::from_asts(
                path.clone(),
                namespace.clone(),
                function.parameters,
            ),
            return_type: EnhASTType::from_ast(
                path.clone(),
                namespace.clone(),
                function.return_type,
            ),
            body: {
                match function.body {
                    ASTFunctionBody::RASMBody(statements) => EnhASTFunctionBody::RASMBody(
                        statements
                            .into_iter()
                            .map(|it| {
                                EnhASTStatement::from_ast(path.clone(), namespace.clone(), it)
                            })
                            .collect(),
                    ),
                    ASTFunctionBody::NativeBody(value) => EnhASTFunctionBody::NativeBody(value),
                }
            },
            inline: function.inline,
            generic_types: function.generic_types,
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: EnhASTIndex::from_position(path.clone(), &function.position),
            modifiers: function.modifiers,
            namespace,
            rank: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnhASTLambdaDef {
    pub parameter_names: Vec<(String, EnhASTIndex)>,
    pub body: Vec<EnhASTStatement>,
    pub index: EnhASTIndex,
}

impl EnhASTLambdaDef {
    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        let mut result = self;
        result.body = result
            .body
            .into_iter()
            .map(|it| it.fix_namespaces(enhanced_module))
            .collect();
        result
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        lambda: ASTLambdaDef,
    ) -> Self {
        Self {
            parameter_names: lambda
                .parameter_names
                .into_iter()
                .map(|(name, position)| (name, EnhASTIndex::from_position(path.clone(), &position)))
                .collect(),
            body: lambda
                .body
                .into_iter()
                .map(|it| EnhASTStatement::from_ast(path.clone(), namespace.clone(), it))
                .collect(),
            index: EnhASTIndex::from_position(path.clone(), &lambda.position),
        }
    }
}

impl Display for EnhASTLambdaDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .parameter_names
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>()
            .join(",");
        let body = self
            .body
            .iter()
            .map(|it| format!("{it};"))
            .collect::<Vec<String>>()
            .join("");

        if pars.is_empty() {
            f.write_str(&format!("{{ {body} }}"))
        } else {
            f.write_str(&format!("fn({pars}) {{ {body} }}"))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnhASTFunctionBody {
    RASMBody(Vec<EnhASTStatement>),
    NativeBody(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnhBuiltinTypeKind {
    Bool,
    Char,
    I32,
    F32,
    String,
    Lambda {
        parameters: Vec<EnhASTType>,
        return_type: Box<EnhASTType>,
    },
}

impl EnhBuiltinTypeKind {
    pub fn to_ast(&self) -> BuiltinTypeKind {
        match self {
            EnhBuiltinTypeKind::Bool => BuiltinTypeKind::Bool,
            EnhBuiltinTypeKind::Char => BuiltinTypeKind::Char,
            EnhBuiltinTypeKind::I32 => BuiltinTypeKind::I32,
            EnhBuiltinTypeKind::F32 => BuiltinTypeKind::F32,
            EnhBuiltinTypeKind::String => BuiltinTypeKind::String,
            EnhBuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => BuiltinTypeKind::Lambda {
                parameters: parameters.iter().map(|it| it.to_ast()).collect(),
                return_type: Box::new(return_type.to_ast()),
            },
        }
    }
}

#[derive(Derivative)]
#[derivative(PartialEq, Hash)]
#[derive(Debug, Clone, Eq)]
pub enum EnhASTType {
    Builtin(EnhBuiltinTypeKind),
    Generic(EnhASTIndex, String),
    Custom {
        namespace: EnhASTNameSpace,
        name: String,
        param_types: Vec<EnhASTType>,
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        index: EnhASTIndex,
    },
    Unit,
}

impl EnhASTType {
    pub fn is_unit(&self) -> bool {
        self == &EnhASTType::Unit
    }

    pub fn namespace(&self) -> &EnhASTNameSpace {
        match self {
            EnhASTType::Custom {
                namespace,
                name: _,
                param_types: _,
                index: _,
            } => namespace,
            _ => EnhASTNameSpace::global_ref(),
        }
    }

    #[deprecated(
        note = "probably there's no need of this function, sice namespaces are fixed in EnhancedAstModule::new"
    )]
    pub fn equals_excluding_namespace(&self, other: &Self) -> bool {
        match self {
            EnhASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => match other {
                EnhASTType::Custom {
                    namespace: _,
                    name: other_name,
                    param_types: other_param_types,
                    index: _,
                } => {
                    name == other_name
                        && param_types.len() == other_param_types.len()
                        && zip(param_types.iter(), other_param_types.iter())
                            .all(|(first, second)| first.equals_excluding_namespace(second))
                }
                _ => false,
            },
            EnhASTType::Builtin(builtin_type_kind) => {
                if let EnhASTType::Builtin(other_builtin) = other {
                    match builtin_type_kind {
                        EnhBuiltinTypeKind::Bool => {
                            matches!(other_builtin, EnhBuiltinTypeKind::Bool)
                        }
                        EnhBuiltinTypeKind::Char => {
                            matches!(other_builtin, EnhBuiltinTypeKind::Char)
                        }
                        EnhBuiltinTypeKind::I32 => matches!(other_builtin, EnhBuiltinTypeKind::I32),
                        EnhBuiltinTypeKind::F32 => matches!(other_builtin, EnhBuiltinTypeKind::F32),
                        EnhBuiltinTypeKind::String => {
                            matches!(other_builtin, EnhBuiltinTypeKind::String)
                        }
                        EnhBuiltinTypeKind::Lambda {
                            parameters,
                            return_type,
                        } => {
                            if let EnhBuiltinTypeKind::Lambda {
                                parameters: o_parameters,
                                return_type: o_return_type,
                            } = other_builtin
                            {
                                parameters.len() == o_parameters.len()
                                    && zip(parameters.iter(), o_parameters.iter())
                                        .all(|(p, o_p)| p.equals_excluding_namespace(o_p))
                                    && return_type.equals_excluding_namespace(&o_return_type)
                            } else {
                                false
                            }
                        }
                    }
                } else {
                    false
                }
            }
            EnhASTType::Generic(_, g) => {
                if let EnhASTType::Generic(_, og) = other {
                    g == og
                } else {
                    false
                }
            }
            EnhASTType::Unit => other.is_unit(),
        }
    }

    pub fn is_generic(&self) -> bool {
        return match self {
            EnhASTType::Builtin(kind) => match kind {
                EnhBuiltinTypeKind::String => false,
                EnhBuiltinTypeKind::I32 => false,
                EnhBuiltinTypeKind::Bool => false,
                EnhBuiltinTypeKind::Char => false,
                EnhBuiltinTypeKind::F32 => false,
                EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let mut par_types: bool = parameters.iter().any(Self::is_generic);
                    if !return_type.is_unit() {
                        par_types = par_types || Self::is_generic(return_type.deref());
                    }
                    par_types
                }
            },
            EnhASTType::Generic(_, _) => true,
            EnhASTType::Custom {
                namespace: _,
                name: _,
                param_types: pt,
                index: _,
            } => pt.iter().any(|it| match it {
                EnhASTType::Generic(_, _) => true,
                _ => Self::is_generic(it),
            }),
            EnhASTType::Unit => false,
        };
    }

    pub fn is_reference(&self, type_def_provider: &dyn TypeDefProvider) -> bool {
        if let EnhASTType::Builtin(EnhBuiltinTypeKind::String) = self {
            true
        } else if let EnhASTType::Custom {
            namespace: _,
            name,
            param_types: _,
            index: _,
        } = self
        {
            if let Some(t) = type_def_provider.get_typed_type_def_from_type_name(name) {
                t.is_ref
            } else {
                true
            }
        } else {
            false
        }
    }

    pub fn is_reference_by_module(&self, module: &EnhASTModule) -> bool {
        if let EnhASTType::Builtin(EnhBuiltinTypeKind::String) = self {
            true
        } else if let EnhASTType::Custom {
            namespace: _,
            name,
            param_types: _,
            index: _,
        } = self
        {
            if let Some(t) = module.types.iter().find(|it| &it.name == name) {
                t.is_ref
            } else {
                true
            }
        } else {
            false
        }
    }

    pub fn fix_namespaces(&self, enhanced_module: &EnhancedASTModule) -> Self {
        match self {
            EnhASTType::Builtin(builtin_type_kind) => match builtin_type_kind {
                EnhBuiltinTypeKind::Bool => self.clone(),
                EnhBuiltinTypeKind::Char => self.clone(),
                EnhBuiltinTypeKind::I32 => self.clone(),
                EnhBuiltinTypeKind::F32 => self.clone(),
                EnhBuiltinTypeKind::String => self.clone(),
                EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                    parameters: parameters
                        .iter()
                        .map(|it| it.fix_namespaces(enhanced_module))
                        .collect(),
                    return_type: Box::new(return_type.fix_namespaces(enhanced_module)),
                }),
            },
            EnhASTType::Generic(_, _) => self.clone(),
            EnhASTType::Custom {
                namespace: _,
                name,
                param_types,
                index,
            } => {
                if let Some(type_def) = enhanced_module.get_type_def(self) {
                    EnhASTType::Custom {
                        namespace: type_def.namespace().clone(),
                        name: name.clone(),
                        param_types: param_types
                            .iter()
                            .map(|it| it.fix_namespaces(enhanced_module))
                            .collect(),
                        index: index.clone(),
                    }
                } else {
                    panic!("Cannot find custom type declaration for {self}");
                }
            }
            EnhASTType::Unit => self.clone(),
        }
    }

    pub fn fix_generics(&self, prefix: &dyn Display) -> Self {
        match self {
            EnhASTType::Builtin(builtin_type_kind) => match builtin_type_kind {
                EnhBuiltinTypeKind::Bool => self.clone(),
                EnhBuiltinTypeKind::Char => self.clone(),
                EnhBuiltinTypeKind::I32 => self.clone(),
                EnhBuiltinTypeKind::F32 => self.clone(),
                EnhBuiltinTypeKind::String => self.clone(),
                EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                    parameters: parameters
                        .iter()
                        .map(|it| it.fix_generics(prefix))
                        .collect(),
                    return_type: Box::new(return_type.fix_generics(prefix)),
                }),
            },
            EnhASTType::Generic(index, name) => {
                EnhASTType::Generic(index.clone(), format!("{prefix}:{name}"))
            }
            EnhASTType::Custom {
                namespace,
                name,
                param_types,
                index,
            } => EnhASTType::Custom {
                namespace: namespace.clone(),
                name: name.clone(),
                param_types: param_types
                    .iter()
                    .map(|it| it.fix_generics(prefix))
                    .collect(),
                index: index.clone(),
            },
            EnhASTType::Unit => self.clone(),
        }
    }

    pub fn to_ast(&self) -> ASTType {
        match self {
            EnhASTType::Builtin(enh_builtin_type_kind) => {
                ASTType::Builtin(enh_builtin_type_kind.to_ast())
            }
            EnhASTType::Generic(index, name) => {
                ASTType::Generic(ASTPosition::new(index.row, index.column), name.clone())
            }
            EnhASTType::Custom {
                namespace,
                name,
                param_types,
                index,
            } => ASTType::Custom {
                name: name.clone(),
                param_types: param_types.iter().map(|it| it.to_ast()).collect(),
                position: ASTPosition::new(index.row, index.column),
            },
            EnhASTType::Unit => ASTType::Unit,
        }
    }

    pub fn from_ast(path: Option<PathBuf>, namespace: EnhASTNameSpace, ast_type: ASTType) -> Self {
        match ast_type {
            ASTType::Builtin(kind) => {
                let builtin = match kind {
                    BuiltinTypeKind::Bool => EnhBuiltinTypeKind::Bool,
                    BuiltinTypeKind::Char => EnhBuiltinTypeKind::Char,
                    BuiltinTypeKind::I32 => EnhBuiltinTypeKind::I32,
                    BuiltinTypeKind::F32 => EnhBuiltinTypeKind::F32,
                    BuiltinTypeKind::String => EnhBuiltinTypeKind::String,
                    BuiltinTypeKind::Lambda {
                        parameters,
                        return_type,
                    } => EnhBuiltinTypeKind::Lambda {
                        parameters: EnhASTType::from_asts(
                            path.clone(),
                            namespace.clone(),
                            parameters,
                        ),
                        return_type: Box::new(EnhASTType::from_ast(
                            path.clone(),
                            namespace.clone(),
                            return_type.as_ref().clone(),
                        )),
                    },
                };
                EnhASTType::Builtin(builtin)
            }
            ASTType::Generic(astposition, name) => {
                EnhASTType::Generic(EnhASTIndex::from_position(path.clone(), &astposition), name)
            }
            ASTType::Custom {
                name,
                param_types,
                position: index,
            } => EnhASTType::Custom {
                namespace: namespace.clone(),
                name,
                param_types: EnhASTType::from_asts(path.clone(), namespace.clone(), param_types),
                index: EnhASTIndex::from_position(path, &index),
            },
            ASTType::Unit => EnhASTType::Unit,
        }
    }

    pub fn from_asts(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        asts: Vec<ASTType>,
    ) -> Vec<EnhASTType> {
        asts.into_iter()
            .map(|it| EnhASTType::from_ast(path.clone(), namespace.clone(), it))
            .collect()
    }
}

impl Display for EnhASTType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EnhASTType::Builtin(kind) => match kind {
                EnhBuiltinTypeKind::String => f.write_str("str"),
                EnhBuiltinTypeKind::I32 => f.write_str("i32"),
                EnhBuiltinTypeKind::Bool => f.write_str("bool"),
                EnhBuiltinTypeKind::Char => f.write_str("char"),
                EnhBuiltinTypeKind::F32 => f.write_str("f32"),
                EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let pars: Vec<String> = parameters.iter().map(|it| format!("{it}")).collect();

                    f.write_str(&format!(
                        "fn ({}) -> {}",
                        pars.join(","),
                        return_type.deref()
                    ))
                }
            },
            EnhASTType::Generic(_, name) => f.write_str(name),
            EnhASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => {
                let pars: Vec<String> = param_types.iter().map(|it| format!("{it}")).collect();

                if pars.is_empty() {
                    f.write_str(name)
                } else {
                    f.write_str(&format!("{name}<{}>", pars.join(",")))
                }
            }
            EnhASTType::Unit => f.write_str("()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnhASTParameterDef {
    pub name: String,
    pub ast_type: EnhASTType,
    pub ast_index: EnhASTIndex,
}

impl Display for EnhASTParameterDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}: {}", self.name, self.ast_type))
    }
}

impl EnhASTParameterDef {
    pub fn new(name: &str, ast_type: EnhASTType, ast_index: EnhASTIndex) -> EnhASTParameterDef {
        EnhASTParameterDef {
            name: name.into(),
            ast_type,
            ast_index,
        }
    }

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        let mut result = self;
        result.ast_type = result.ast_type.fix_namespaces(enhanced_module);
        result
    }

    fn fix_generics(self, generics_prefix: &dyn Display) -> Self {
        let mut result = self;
        result.ast_type = result.ast_type.fix_generics(generics_prefix);
        result
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        parameter: ASTParameterDef,
    ) -> Self {
        Self {
            name: parameter.name,
            ast_type: EnhASTType::from_ast(path.clone(), namespace, parameter.ast_type),
            ast_index: EnhASTIndex::from_position(path.clone(), &parameter.position),
        }
    }

    pub fn from_asts(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        parameters: Vec<ASTParameterDef>,
    ) -> Vec<EnhASTParameterDef> {
        parameters
            .into_iter()
            .map(|it| EnhASTParameterDef::from_ast(path.clone(), namespace.clone(), it))
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnhASTStructPropertyDef {
    pub name: String,
    pub ast_type: EnhASTType,
    pub index: EnhASTIndex,
}

impl EnhASTStructPropertyDef {
    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        let mut result = self;
        result.ast_type = result.ast_type.fix_namespaces(enhanced_module);
        result
    }

    fn fix_generics(self, generics_prefix: &dyn Display) -> Self {
        let mut result = self;
        result.ast_type = result.ast_type.fix_generics(generics_prefix);
        result
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        property: ASTStructPropertyDef,
    ) -> Self {
        Self {
            name: property.name,
            ast_type: EnhASTType::from_ast(path.clone(), namespace.clone(), property.ast_type),
            index: EnhASTIndex::from_position(path.clone(), &property.position),
        }
    }
}

impl Display for EnhASTStructPropertyDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}: {}", self.name, self.ast_type))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnhASTFunctionCall {
    pub namespace: EnhASTNameSpace,
    pub original_function_name: String,
    pub function_name: String,
    pub parameters: Vec<EnhASTExpression>,
    pub index: EnhASTIndex,
    pub generics: Vec<EnhASTType>,
}

impl EnhASTFunctionCall {
    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        let mut result = self;
        result.parameters = result
            .parameters
            .into_iter()
            .map(|it| it.fix_namespaces(enhanced_module))
            .collect();
        result.generics = result
            .generics
            .into_iter()
            .map(|it| it.fix_namespaces(enhanced_module))
            .collect();
        result
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        call: ASTFunctionCall,
    ) -> Self {
        Self {
            namespace: namespace.clone(),
            original_function_name: call.function_name.clone(),
            function_name: call.function_name,
            parameters: call
                .parameters
                .into_iter()
                .map(|it| EnhASTExpression::from_ast(path.clone(), namespace.clone(), it))
                .collect(),
            index: EnhASTIndex::from_position(path.clone(), &call.position),
            generics: EnhASTType::from_asts(path.clone(), namespace.clone(), call.generics),
        }
    }
}

impl Display for EnhASTFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars: Vec<String> = self.parameters.iter().map(|it| format!("{}", it)).collect();

        f.write_str(&format!(
            "{}::{}({})",
            self.namespace,
            self.function_name,
            pars.join(",")
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnhASTIndex {
    pub file_name: Option<PathBuf>,
    pub row: usize,
    pub column: usize,
}

impl EnhASTIndex {
    pub fn none() -> Self {
        Self {
            file_name: None,
            row: 0,
            column: 0,
        }
    }

    pub fn new(file_name: Option<PathBuf>, row: usize, column: usize) -> Self {
        Self {
            file_name,
            row,
            column,
        }
    }

    pub fn mv_right(&self, offset: usize) -> Self {
        Self {
            file_name: self.file_name.clone(),
            row: self.row,
            column: self.column + offset,
        }
    }

    pub fn mv_left(&self, offset: usize) -> Self {
        Self {
            file_name: self.file_name.clone(),
            row: self.row,
            column: (self.column as i32 - (offset as i32)) as usize,
        }
    }

    pub fn mv_down(&self, offset: usize) -> Self {
        Self {
            file_name: self.file_name.clone(),
            row: self.row + offset,
            column: self.column,
        }
    }

    pub fn from_position(path: Option<PathBuf>, position: &ASTPosition) -> Self {
        Self {
            file_name: path,
            row: position.row,
            column: position.column,
        }
    }
}

impl Display for EnhASTIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "{}:{}:{}",
            &self
                .file_name
                .as_ref()
                .map(|it| {
                    if it.exists() {
                        format!("file:///{}", it.canonicalize().unwrap().to_str().unwrap())
                    } else {
                        format!("{}", it.to_string_lossy())
                    }
                })
                .unwrap_or_else(|| "".to_owned()),
            &self.row,
            &self.column
        ))
    }
}

/*
impl ASTValueType {
    pub fn value_type_to_enh_type(&self) -> EnhASTType {
        match self {
            ASTValueType::String(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::String),
            ASTValueType::Boolean(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::Bool),
            ASTValueType::I32(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::I32),
            ASTValueType::Char(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::Char),
            ASTValueType::F32(_) => EnhASTType::Builtin(EnhBuiltinTypeKind::F32),
        }
    }
    pub fn value_type_to_typed_type(&self) -> ASTTypedType {
        match self {
            ASTValueType::String(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
            ASTValueType::Boolean(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Bool),
            ASTValueType::I32(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::I32),
            ASTValueType::Char(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
            ASTValueType::F32(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::F32),
        }
    }
}
    */

// TODO can we do partialeq? It depends on ASTIndex
#[derive(Debug, Clone, PartialEq)]
pub enum EnhASTExpression {
    ASTFunctionCallExpression(EnhASTFunctionCall),
    ValueRef(String, EnhASTIndex),
    Value(ASTValueType, EnhASTIndex),
    Lambda(EnhASTLambdaDef),
    Any(EnhASTType), //EnumConstructor { name: String, variant: String, parameters: Vec<ASTExpression> },
}

impl EnhASTExpression {
    pub fn get_index(&self) -> EnhASTIndex {
        match self {
            EnhASTExpression::ASTFunctionCallExpression(call) => call.index.clone(),
            EnhASTExpression::ValueRef(_, index) => index.clone(),
            EnhASTExpression::Value(_, index) => index.clone(),
            EnhASTExpression::Lambda(def) => def.index.clone(),
            EnhASTExpression::Any(_) => EnhASTIndex::none(),
        }
    }

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        match self {
            EnhASTExpression::ASTFunctionCallExpression(astfunction_call) => {
                EnhASTExpression::ASTFunctionCallExpression(
                    astfunction_call.fix_namespaces(enhanced_module),
                )
            }
            EnhASTExpression::Lambda(astlambda_def) => {
                EnhASTExpression::Lambda(astlambda_def.fix_namespaces(enhanced_module))
            }
            EnhASTExpression::Any(asttype) => {
                EnhASTExpression::Any(asttype.fix_namespaces(enhanced_module))
            }
            _ => self.clone(),
        }
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        expr: ASTExpression,
    ) -> Self {
        match expr {
            ASTExpression::ASTFunctionCallExpression(call) => {
                EnhASTExpression::ASTFunctionCallExpression(EnhASTFunctionCall::from_ast(
                    path.clone(),
                    namespace.clone(),
                    call,
                ))
            }
            ASTExpression::ValueRef(name, position) => EnhASTExpression::ValueRef(
                name,
                EnhASTIndex::from_position(path.clone(), &position),
            ),
            ASTExpression::Value(value_type, position) => EnhASTExpression::Value(
                value_type,
                EnhASTIndex::from_position(path.clone(), &position),
            ),
            ASTExpression::Lambda(lambda) => EnhASTExpression::Lambda(EnhASTLambdaDef::from_ast(
                path.clone(),
                namespace.clone(),
                lambda,
            )),
        }
    }
}

impl Display for EnhASTExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EnhASTExpression::ASTFunctionCallExpression(call) => {
                /*let pars: Vec<String> =
                    call.parameters.iter().map(|it| format!("{}", it)).collect();


                f.write_str(&format!("{}({})", call.function_name, pars.join(",")))

                 */
                f.write_str(&format!("{call}"))
            }
            EnhASTExpression::ValueRef(name, _index) => f.write_str(name),
            EnhASTExpression::Value(val_type, _) => write!(f, "{val_type}"),
            EnhASTExpression::Lambda(lambda) => f.write_str(&format!("{lambda}")),
            EnhASTExpression::Any(ast_type) => f.write_str(&format!("Any({ast_type})")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnhASTStatement {
    Expression(EnhASTExpression),
    LetStatement(String, EnhASTExpression, bool, EnhASTIndex),
}

impl EnhASTStatement {
    pub fn get_index(&self) -> EnhASTIndex {
        match self {
            EnhASTStatement::Expression(expr) => expr.get_index(),
            EnhASTStatement::LetStatement(_, _, _, index) => index.clone(),
        }
    }

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        match self {
            EnhASTStatement::Expression(exp) => {
                EnhASTStatement::Expression(exp.fix_namespaces(enhanced_module))
            }
            EnhASTStatement::LetStatement(name, exp, is_const, astindex) => {
                EnhASTStatement::LetStatement(
                    name,
                    exp.fix_namespaces(enhanced_module),
                    is_const,
                    astindex,
                )
            }
        }
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        statement: ASTStatement,
    ) -> Self {
        match statement {
            ASTStatement::Expression(expr) => EnhASTStatement::Expression(
                EnhASTExpression::from_ast(path.clone(), namespace.clone(), expr),
            ),
            ASTStatement::LetStatement(name, astexpression, is_const, position) => {
                let expr =
                    EnhASTExpression::from_ast(path.clone(), namespace.clone(), astexpression);
                EnhASTStatement::LetStatement(
                    name,
                    expr,
                    is_const,
                    EnhASTIndex::from_position(path.clone(), &position),
                )
            }
        }
    }
}

impl Display for EnhASTStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EnhASTStatement::Expression(e) => f.write_str(&format!("{e};\n")),
            EnhASTStatement::LetStatement(name, e, is_const, _index) => {
                let keyword = if *is_const { "const" } else { "let" };
                f.write_str(&format!("{keyword} {name} = {e};\n"))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnhASTModule {
    pub path: PathBuf,
    pub body: Vec<EnhASTStatement>,
    pub functions: Vec<EnhASTFunctionDef>,
    pub enums: Vec<EnhASTEnumDef>,
    pub structs: Vec<EnhASTStructDef>,
    pub types: Vec<EnhASTTypeDef>,
    pub namespace: EnhASTNameSpace,
}

impl EnhASTModule {
    pub fn empty(namespace: EnhASTNameSpace) -> Self {
        EnhASTModule {
            path: Default::default(),
            body: vec![],
            functions: vec![],
            enums: vec![],
            structs: vec![],
            types: vec![],
            namespace,
        }
    }

    pub fn add_function(&mut self, function_def: EnhASTFunctionDef) {
        self.functions.push(function_def);
    }

    pub fn add(&mut self, mut module: EnhASTModule) {
        self.body.append(&mut module.body);
        self.functions.append(&mut module.functions);
        self.enums.append(&mut module.enums);
        self.structs.append(&mut module.structs);
        self.types.extend(module.types);
    }

    pub fn from_ast(module: ASTModule, info: EnhModuleInfo) -> Self {
        Self {
            path: info.path().unwrap(),
            body: module
                .body
                .into_iter()
                .map(|it| EnhASTStatement::from_ast(info.path(), info.namespace.clone(), it))
                .collect(),
            functions: module
                .functions
                .into_iter()
                .map(|it| EnhASTFunctionDef::from_ast(info.path(), info.namespace.clone(), it))
                .collect(),
            enums: module
                .enums
                .into_iter()
                .map(|it| EnhASTEnumDef::from_ast(info.path(), info.namespace.clone(), it))
                .collect(),
            structs: module
                .structs
                .into_iter()
                .map(|it| EnhASTStructDef::from_ast(info.path(), info.namespace.clone(), it))
                .collect(),
            types: module
                .types
                .into_iter()
                .map(|it| EnhASTTypeDef::from_ast(info.path(), info.namespace.clone(), it))
                .collect(),
            namespace: info.namespace.clone(),
        }
    }
}

pub trait CustomTypeDef: Display {
    fn name(&self) -> &str;
    fn modifiers(&self) -> &ASTModifiers;
    fn namespace(&self) -> &EnhASTNameSpace;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnhASTEnumDef {
    pub namespace: EnhASTNameSpace,
    pub name: String,
    pub type_parameters: Vec<String>,
    pub variants: Vec<EnhASTEnumVariantDef>,
    pub index: EnhASTIndex,
    pub modifiers: ASTModifiers,
}

impl CustomTypeDef for EnhASTEnumDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &EnhASTNameSpace {
        &self.namespace
    }
}

impl Display for EnhASTEnumDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let variants = self
            .variants
            .iter()
            .map(|it| format!("  {it}"))
            .collect::<Vec<_>>()
            .join("\n");
        f.write_str(&format!("enum {} {{\n{variants}\n}}", self.name))
    }
}

impl EnhASTEnumDef {
    pub fn variant_function_name(&self, variant: &EnhASTEnumVariantDef) -> String {
        let mut result = String::new();
        result.push_str(&self.name);
        result.push_str("::");
        result.push_str(&variant.name);
        result
    }

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        let mut result = self.clone();
        result.variants = self
            .variants
            .into_iter()
            .map(|it| it.fix_namespaces(enhanced_module))
            .collect();
        result
    }

    pub fn fix_generics(self) -> Self {
        let generics_prefix = format!("{}_{}", self.namespace, self.name);
        let mut result = self.clone();

        result.type_parameters = result
            .type_parameters
            .into_iter()
            .map(|it| format!("{generics_prefix}:{it}"))
            .collect();

        result.variants = self
            .variants
            .into_iter()
            .map(|it| it.fix_generics(&generics_prefix))
            .collect();
        result
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        enum_def: ASTEnumDef,
    ) -> Self {
        Self {
            namespace: namespace.clone(),
            name: enum_def.name,
            type_parameters: enum_def.type_parameters,
            variants: enum_def
                .variants
                .into_iter()
                .map(|it| EnhASTEnumVariantDef::from_ast(path.clone(), namespace.clone(), it))
                .collect(),
            index: EnhASTIndex::from_position(path, &enum_def.position),
            modifiers: enum_def.modifiers,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnhASTEnumVariantDef {
    pub name: String,
    pub parameters: Vec<EnhASTParameterDef>,
    pub index: EnhASTIndex,
}

impl EnhASTEnumVariantDef {
    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        Self {
            name: self.name.clone(),
            parameters: self
                .parameters
                .into_iter()
                .map(|it| it.fix_namespaces(enhanced_module))
                .collect(),
            index: self.index.clone(),
        }
    }

    pub fn fix_generics(self, generics_prefix: &dyn Display) -> Self {
        Self {
            name: self.name.clone(),
            parameters: self
                .parameters
                .into_iter()
                .map(|it| it.fix_generics(generics_prefix))
                .collect(),
            index: self.index.clone(),
        }
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        variant_def: ASTEnumVariantDef,
    ) -> Self {
        Self {
            name: variant_def.name,
            parameters: EnhASTParameterDef::from_asts(
                path.clone(),
                namespace.clone(),
                variant_def.parameters,
            ),
            index: EnhASTIndex::from_position(path.clone(), &variant_def.position),
        }
    }
}

impl Display for EnhASTEnumVariantDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .parameters
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<String>>()
            .join(",");
        f.write_str(&format!("{}({})", self.name, pars))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnhASTStructDef {
    pub namespace: EnhASTNameSpace,
    pub name: String,
    pub type_parameters: Vec<String>,
    pub properties: Vec<EnhASTStructPropertyDef>,
    pub index: EnhASTIndex,
    pub modifiers: ASTModifiers,
}

impl EnhASTStructDef {
    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        let mut result = self;
        result.properties = result
            .properties
            .into_iter()
            .map(|it| it.fix_namespaces(enhanced_module))
            .collect();
        return result;
    }

    pub fn fix_generics(self) -> Self {
        let generics_prefix = format!("{}_{}", self.namespace, self.name);
        let mut result = self;
        result.type_parameters = result
            .type_parameters
            .into_iter()
            .map(|it| format!("{generics_prefix}:{it}"))
            .collect();
        result.properties = result
            .properties
            .into_iter()
            .map(|it| it.fix_generics(&generics_prefix))
            .collect();
        return result;
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        struct_def: ASTStructDef,
    ) -> Self {
        Self {
            namespace: namespace.clone(),
            name: struct_def.name,
            type_parameters: struct_def.type_parameters,
            properties: struct_def
                .properties
                .into_iter()
                .map(|it| EnhASTStructPropertyDef::from_ast(path.clone(), namespace.clone(), it))
                .collect(),
            index: EnhASTIndex::from_position(path.clone(), &struct_def.position),
            modifiers: struct_def.modifiers,
        }
    }
}

impl CustomTypeDef for EnhASTStructDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &EnhASTNameSpace {
        &self.namespace
    }
}

impl Display for EnhASTStructDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .properties
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<_>>()
            .join(",");
        f.write_str(&format!("struct {}:{}({pars})", self.namespace, self.name))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnhASTTypeDef {
    pub namespace: EnhASTNameSpace,
    pub name: String,
    pub type_parameters: Vec<String>,
    pub is_ref: bool,
    pub index: EnhASTIndex,
    pub modifiers: ASTModifiers,
    pub native_type: Option<String>,
}
impl EnhASTTypeDef {
    pub fn fix_generics(self) -> Self {
        let generics_prefix = format!("{}_{}", self.namespace, self.name);

        let mut result = self;

        result.type_parameters = result
            .type_parameters
            .into_iter()
            .map(|it| format!("{generics_prefix}:{it}"))
            .collect();
        result
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: EnhASTNameSpace,
        type_def: ASTTypeDef,
    ) -> Self {
        Self {
            namespace: namespace.clone(),
            name: type_def.name,
            type_parameters: type_def.type_parameters,
            is_ref: type_def.is_ref,
            index: EnhASTIndex::from_position(path.clone(), &type_def.position),
            modifiers: type_def.modifiers,
            native_type: type_def.native_type,
        }
    }
}

impl CustomTypeDef for EnhASTTypeDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &EnhASTNameSpace {
        &self.namespace
    }
}

impl Display for EnhASTTypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("type {}", self.name))
    }
}

pub fn lambda(return_type: EnhASTType) -> EnhASTType {
    EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
        parameters: Vec::new(),
        return_type: Box::new(return_type),
    })
}

pub fn lambda_unit() -> EnhASTType {
    lambda(EnhASTType::Unit)
}

#[cfg(test)]
mod tests {
    use crate::codegen::enh_ast::{
        EnhASTFunctionBody, EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTParameterDef,
        EnhASTType, EnhBuiltinTypeKind,
    };
    use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
    use rasm_parser::parser::ast::ASTModifiers;

    #[test]
    fn display_custom_type() {
        let inner_type = EnhASTType::Custom {
            namespace: EnhASTNameSpace::global(),
            name: "Option".to_owned(),
            param_types: vec![EnhASTType::Builtin(EnhBuiltinTypeKind::String)],
            index: EnhASTIndex::none(),
        };

        let ast_type = EnhASTType::Custom {
            namespace: EnhASTNameSpace::global(),
            name: "List".to_owned(),
            param_types: vec![inner_type],
            index: EnhASTIndex::none(),
        };
        assert_eq!(format!("{ast_type}"), "List<Option<str>>");
    }

    #[test]
    fn display_function_def() {
        let inner_type = EnhASTType::Custom {
            namespace: EnhASTNameSpace::global(),
            name: "Option".to_owned(),
            param_types: vec![EnhASTType::Generic(EnhASTIndex::none(), "T".to_string())],
            index: EnhASTIndex::none(),
        };

        let ast_type = EnhASTType::Custom {
            namespace: EnhASTNameSpace::global(),
            name: "List".to_owned(),
            param_types: vec![inner_type],
            index: EnhASTIndex::none(),
        };

        let def = EnhASTFunctionDef {
            original_name: "aFun".to_string(),
            name: "aFun".to_string(),
            parameters: vec![EnhASTParameterDef {
                name: "aPar".to_string(),
                ast_type,
                ast_index: EnhASTIndex::none(),
            }],
            return_type: EnhASTType::Generic(EnhASTIndex::none(), "T".to_string()),
            body: EnhASTFunctionBody::RASMBody(vec![]),
            inline: false,
            generic_types: vec!["T".to_string()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: EnhASTIndex::none(),
            modifiers: ASTModifiers::private(),
            namespace: EnhASTNameSpace {
                lib: "".to_string(),
                path: "".to_string(),
            },
            rank: 0,
        };

        assert_eq!(format!("{def}"), "fn aFun<T>(aPar: List<Option<T>>) -> T");
    }
}
