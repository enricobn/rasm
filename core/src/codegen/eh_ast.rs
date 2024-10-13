use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::iter::zip;
use std::ops::Deref;
use std::path::PathBuf;

use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::new_type_check2::TypeCheck;
use crate::project::RasmProject;

use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::typed_ast::{ASTTypedType, BuiltinTypedTypeKind};

use crate::parser::ast::{self, ASTPosition};

#[derive(Debug, Clone)]
pub struct EhModuleInfo {
    pub path: Option<PathBuf>,
    pub namespace: ASTNameSpace,
}

impl EhModuleInfo {
    pub fn new(path: Option<PathBuf>, namespace: ASTNameSpace) -> Self {
        Self { path, namespace }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ASTNameSpace {
    lib: String,
    path: String,
}

const GLOBAL_NAMESPACE: ASTNameSpace = ASTNameSpace {
    lib: String::new(),
    path: String::new(),
};

const GLOBAL_NAMESPACE_REF: &'static ASTNameSpace = &GLOBAL_NAMESPACE;

impl ASTNameSpace {
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

impl Display for ASTNameSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}", self.lib, self.path))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionDef {
    pub original_name: String,
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub return_type: ASTType,
    pub body: ASTFunctionBody,
    pub inline: bool,
    pub generic_types: Vec<String>,
    pub resolved_generic_types: ResolvedGenericTypes,
    pub index: ASTIndex,
    pub modifiers: ASTModifiers,
    pub namespace: ASTNameSpace,
    ///
    /// The rank is the sum of the parameter's type generic coefficient, the coefficient
    /// of <T> is higher than Option<T> that is higher than Option<List<T>>
    /// So less is the rank, best suited is the function
    pub rank: usize,
}

impl ASTFunctionDef {
    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: ASTNameSpace,
        function: ast::ASTFunctionDef,
    ) -> Self {
        Self {
            original_name: function.name.clone(),
            name: function.name,
            parameters: ASTParameterDef::from_asts(
                path.clone(),
                namespace.clone(),
                function.parameters,
            ),
            return_type: ASTType::from_ast(path.clone(), namespace.clone(), function.return_type),
            body: {
                match function.body {
                    ast::ASTFunctionBody::RASMBody(statements) => ASTFunctionBody::RASMBody(
                        statements
                            .into_iter()
                            .map(|it| ASTStatement::from_ast(path.clone(), namespace.clone(), it))
                            .collect(),
                    ),
                    ast::ASTFunctionBody::NativeBody(value) => ASTFunctionBody::NativeBody(value),
                }
            },
            inline: function.inline,
            generic_types: function.generic_types,
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::from_position(path.clone(), function.index),
            modifiers: ASTModifiers {
                public: function.modifiers.public,
            },
            namespace,
            rank: 0,
        }
    }
}

pub struct ASTFunctionSignature {
    pub name: String,
    //pub generics: Vec<String>,
    pub parameters_types: Vec<ASTType>,
    pub return_type: ASTType,
}

impl Display for ASTFunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let generic_types = if self.generic_types.is_empty() {
            "".into()
        } else {
            format!("<{}>", self.generic_types.join(","))
        };

        let rt = if self.return_type != ASTType::Unit {
            format!("{}", self.return_type)
        } else {
            "()".into()
        };

        let modifiers = if self.modifiers.public { "pub " } else { "" };

        let fun_or_asm = if let ASTFunctionBody::RASMBody(_) = self.body {
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

impl ASTFunctionDef {
    pub fn update_calculated_properties(&mut self) {
        self.rank = TypeCheck::function_precedence_coeff(self);
    }

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        let mut result = self;
        if let ASTFunctionBody::RASMBody(statements) = result.body {
            result.body = ASTFunctionBody::RASMBody(
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

    pub fn signature(&self) -> ASTFunctionSignature {
        ASTFunctionSignature {
            name: self.name.clone(),
            parameters_types: self
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect(),
            return_type: self.return_type.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTLambdaDef {
    pub parameter_names: Vec<(String, ASTIndex)>,
    pub body: Vec<ASTStatement>,
    pub index: ASTIndex,
}

impl ASTLambdaDef {
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
        namespace: ASTNameSpace,
        lambda: ast::ASTLambdaDef,
    ) -> Self {
        Self {
            parameter_names: lambda
                .parameter_names
                .into_iter()
                .map(|(name, position)| (name, ASTIndex::from_position(path.clone(), position)))
                .collect(),
            body: lambda
                .body
                .into_iter()
                .map(|it| ASTStatement::from_ast(path.clone(), namespace.clone(), it))
                .collect(),
            index: ASTIndex::from_position(path.clone(), lambda.index),
        }
    }
}

impl Display for ASTLambdaDef {
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
pub enum ASTFunctionBody {
    RASMBody(Vec<ASTStatement>),
    NativeBody(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinTypeKind {
    Bool,
    Char,
    I32,
    F32,
    String,
    Lambda {
        parameters: Vec<ASTType>,
        return_type: Box<ASTType>,
    },
}

#[derive(Derivative)]
#[derivative(PartialEq, Hash)]
#[derive(Debug, Clone, Eq)]
pub enum ASTType {
    Builtin(BuiltinTypeKind),
    Generic(ASTIndex, String),
    Custom {
        namespace: ASTNameSpace,
        name: String,
        param_types: Vec<ASTType>,
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        index: ASTIndex,
    },
    Unit,
}

impl ASTType {
    pub fn is_unit(&self) -> bool {
        self == &ASTType::Unit
    }

    pub fn namespace(&self) -> &ASTNameSpace {
        match self {
            ASTType::Custom {
                namespace,
                name: _,
                param_types: _,
                index: _,
            } => namespace,
            _ => ASTNameSpace::global_ref(),
        }
    }

    #[deprecated(
        note = "probably there's no need of this function, sice namespaces are fixed in EnhancedAstModule::new"
    )]
    pub fn equals_excluding_namespace(&self, other: &Self) -> bool {
        match self {
            ASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => match other {
                ASTType::Custom {
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
            ASTType::Builtin(builtin_type_kind) => {
                if let ASTType::Builtin(other_builtin) = other {
                    match builtin_type_kind {
                        BuiltinTypeKind::Bool => matches!(other_builtin, BuiltinTypeKind::Bool),
                        BuiltinTypeKind::Char => matches!(other_builtin, BuiltinTypeKind::Char),
                        BuiltinTypeKind::I32 => matches!(other_builtin, BuiltinTypeKind::I32),
                        BuiltinTypeKind::F32 => matches!(other_builtin, BuiltinTypeKind::F32),
                        BuiltinTypeKind::String => matches!(other_builtin, BuiltinTypeKind::String),
                        BuiltinTypeKind::Lambda {
                            parameters,
                            return_type,
                        } => {
                            if let BuiltinTypeKind::Lambda {
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
            ASTType::Generic(_, g) => {
                if let ASTType::Generic(_, og) = other {
                    g == og
                } else {
                    false
                }
            }
            ASTType::Unit => other.is_unit(),
        }
    }

    pub fn is_generic(&self) -> bool {
        return match self {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::String => false,
                BuiltinTypeKind::I32 => false,
                BuiltinTypeKind::Bool => false,
                BuiltinTypeKind::Char => false,
                BuiltinTypeKind::F32 => false,
                BuiltinTypeKind::Lambda {
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
            ASTType::Generic(_, _) => true,
            ASTType::Custom {
                namespace: _,
                name: _,
                param_types: pt,
                index: _,
            } => pt.iter().any(|it| match it {
                ASTType::Generic(_, _) => true,
                _ => Self::is_generic(it),
            }),
            ASTType::Unit => false,
        };
    }

    pub fn is_reference(&self, type_def_provider: &dyn TypeDefProvider) -> bool {
        if let ASTType::Builtin(BuiltinTypeKind::String) = self {
            true
        } else if let ASTType::Custom {
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

    pub fn is_reference_by_module(&self, module: &ASTModule) -> bool {
        if let ASTType::Builtin(BuiltinTypeKind::String) = self {
            true
        } else if let ASTType::Custom {
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
            ASTType::Builtin(builtin_type_kind) => match builtin_type_kind {
                BuiltinTypeKind::Bool => self.clone(),
                BuiltinTypeKind::Char => self.clone(),
                BuiltinTypeKind::I32 => self.clone(),
                BuiltinTypeKind::F32 => self.clone(),
                BuiltinTypeKind::String => self.clone(),
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters: parameters
                        .iter()
                        .map(|it| it.fix_namespaces(enhanced_module))
                        .collect(),
                    return_type: Box::new(return_type.fix_namespaces(enhanced_module)),
                }),
            },
            ASTType::Generic(_, _) => self.clone(),
            ASTType::Custom {
                namespace: _,
                name,
                param_types,
                index,
            } => {
                if let Some(type_def) = enhanced_module.get_type_def(self) {
                    ASTType::Custom {
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
            ASTType::Unit => self.clone(),
        }
    }

    pub fn fix_generics(&self, prefix: &dyn Display) -> Self {
        match self {
            ASTType::Builtin(builtin_type_kind) => match builtin_type_kind {
                BuiltinTypeKind::Bool => self.clone(),
                BuiltinTypeKind::Char => self.clone(),
                BuiltinTypeKind::I32 => self.clone(),
                BuiltinTypeKind::F32 => self.clone(),
                BuiltinTypeKind::String => self.clone(),
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters: parameters
                        .iter()
                        .map(|it| it.fix_generics(prefix))
                        .collect(),
                    return_type: Box::new(return_type.fix_generics(prefix)),
                }),
            },
            ASTType::Generic(index, name) => {
                ASTType::Generic(index.clone(), format!("{prefix}:{name}"))
            }
            ASTType::Custom {
                namespace,
                name,
                param_types,
                index,
            } => ASTType::Custom {
                namespace: namespace.clone(),
                name: name.clone(),
                param_types: param_types
                    .iter()
                    .map(|it| it.fix_generics(prefix))
                    .collect(),
                index: index.clone(),
            },
            ASTType::Unit => self.clone(),
        }
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: ASTNameSpace,
        ast_type: ast::ASTType,
    ) -> Self {
        match ast_type {
            ast::ASTType::Builtin(kind) => {
                let builtin = match kind {
                    ast::BuiltinTypeKind::Bool => BuiltinTypeKind::Bool,
                    ast::BuiltinTypeKind::Char => BuiltinTypeKind::Char,
                    ast::BuiltinTypeKind::I32 => BuiltinTypeKind::I32,
                    ast::BuiltinTypeKind::F32 => BuiltinTypeKind::F32,
                    ast::BuiltinTypeKind::String => BuiltinTypeKind::String,
                    ast::BuiltinTypeKind::Lambda {
                        parameters,
                        return_type,
                    } => BuiltinTypeKind::Lambda {
                        parameters: ASTType::from_asts(path.clone(), namespace.clone(), parameters),
                        return_type: Box::new(ASTType::from_ast(
                            path.clone(),
                            namespace.clone(),
                            return_type.as_ref().clone(),
                        )),
                    },
                };
                ASTType::Builtin(builtin)
            }
            ast::ASTType::Generic(astposition, name) => {
                ASTType::Generic(ASTIndex::from_position(path.clone(), astposition), name)
            }
            ast::ASTType::Custom {
                name,
                param_types,
                index,
            } => ASTType::Custom {
                namespace: namespace.clone(),
                name,
                param_types: ASTType::from_asts(path.clone(), namespace.clone(), param_types),
                index: ASTIndex::from_position(path, index),
            },
            ast::ASTType::Unit => ASTType::Unit,
        }
    }

    pub fn from_asts(
        path: Option<PathBuf>,
        namespace: ASTNameSpace,
        asts: Vec<ast::ASTType>,
    ) -> Vec<ASTType> {
        asts.into_iter()
            .map(|it| ASTType::from_ast(path.clone(), namespace.clone(), it))
            .collect()
    }
}

impl Display for ASTType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::String => f.write_str("str"),
                BuiltinTypeKind::I32 => f.write_str("i32"),
                BuiltinTypeKind::Bool => f.write_str("bool"),
                BuiltinTypeKind::Char => f.write_str("char"),
                BuiltinTypeKind::F32 => f.write_str("f32"),
                BuiltinTypeKind::Lambda {
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
            ASTType::Generic(_, name) => f.write_str(name),
            ASTType::Custom {
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
            ASTType::Unit => f.write_str("()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTParameterDef {
    pub name: String,
    pub ast_type: ASTType,
    pub ast_index: ASTIndex,
}

impl Display for ASTParameterDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}: {}", self.name, self.ast_type))
    }
}

impl ASTParameterDef {
    pub fn new(name: &str, ast_type: ASTType, ast_index: ASTIndex) -> ASTParameterDef {
        ASTParameterDef {
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
        namespace: ASTNameSpace,
        parameter: ast::ASTParameterDef,
    ) -> Self {
        Self {
            name: parameter.name,
            ast_type: ASTType::from_ast(path.clone(), namespace, parameter.ast_type),
            ast_index: ASTIndex::from_position(path.clone(), parameter.index),
        }
    }

    pub fn from_asts(
        path: Option<PathBuf>,
        namespace: ASTNameSpace,
        parameters: Vec<ast::ASTParameterDef>,
    ) -> Vec<ASTParameterDef> {
        parameters
            .into_iter()
            .map(|it| ASTParameterDef::from_ast(path.clone(), namespace.clone(), it))
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTStructPropertyDef {
    pub name: String,
    pub ast_type: ASTType,
    pub index: ASTIndex,
}

impl ASTStructPropertyDef {
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
        namespace: ASTNameSpace,
        property: ast::ASTStructPropertyDef,
    ) -> Self {
        Self {
            name: property.name,
            ast_type: ASTType::from_ast(path.clone(), namespace.clone(), property.ast_type),
            index: ASTIndex::from_position(path.clone(), property.index),
        }
    }
}

impl Display for ASTStructPropertyDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}: {}", self.name, self.ast_type))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionCall {
    pub namespace: ASTNameSpace,
    pub original_function_name: String,
    pub function_name: String,
    pub parameters: Vec<ASTExpression>,
    pub index: ASTIndex,
    pub generics: Vec<ASTType>,
}

impl ASTFunctionCall {
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
        namespace: ASTNameSpace,
        call: ast::ASTFunctionCall,
    ) -> Self {
        Self {
            namespace: namespace.clone(),
            original_function_name: call.function_name.clone(),
            function_name: call.function_name,
            parameters: call
                .parameters
                .into_iter()
                .map(|it| ASTExpression::from_ast(path.clone(), namespace.clone(), it))
                .collect(),
            index: ASTIndex::from_position(path.clone(), call.index),
            generics: ASTType::from_asts(path.clone(), namespace.clone(), call.generics),
        }
    }
}

impl Display for ASTFunctionCall {
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
pub struct ASTIndex {
    pub file_name: Option<PathBuf>,
    pub row: usize,
    pub column: usize,
}

impl ASTIndex {
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

    pub fn from_position(path: Option<PathBuf>, position: ASTPosition) -> Self {
        Self {
            file_name: path,
            row: position.row,
            column: position.column,
        }
    }
}

impl Display for ASTIndex {
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

#[derive(Clone, Debug, PartialEq)]
pub enum ValueType {
    Boolean(bool),
    I32(i32),
    Char(String),
    F32(f32),
}

impl ValueType {
    pub fn to_type(&self) -> ASTType {
        match self {
            ValueType::Boolean(_) => ASTType::Builtin(BuiltinTypeKind::Bool),
            ValueType::I32(_) => ASTType::Builtin(BuiltinTypeKind::I32),
            ValueType::Char(_) => ASTType::Builtin(BuiltinTypeKind::Char),
            ValueType::F32(_) => ASTType::Builtin(BuiltinTypeKind::F32),
        }
    }

    pub fn to_typed_type(&self) -> ASTTypedType {
        match self {
            ValueType::Boolean(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Bool),
            ValueType::I32(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::I32),
            ValueType::Char(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
            ValueType::F32(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::F32),
        }
    }

    pub fn from_ast(path: Option<PathBuf>, namespace: ASTNameSpace, value: ast::ValueType) -> Self {
        match value {
            ast::ValueType::Boolean(value) => ValueType::Boolean(value),
            ast::ValueType::I32(value) => ValueType::I32(value),
            ast::ValueType::Char(value) => ValueType::Char(value),
            ast::ValueType::F32(value) => ValueType::F32(value),
        }
    }
}

// TODO can we do partialeq? It depends on ASTIndex
#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpression {
    StringLiteral(String, ASTIndex),
    ASTFunctionCallExpression(ASTFunctionCall),
    ValueRef(String, ASTIndex),
    Value(ValueType, ASTIndex),
    Lambda(ASTLambdaDef),
    Any(ASTType), //EnumConstructor { name: String, variant: String, parameters: Vec<ASTExpression> },
}

impl ASTExpression {
    pub fn get_index(&self) -> ASTIndex {
        match self {
            ASTExpression::StringLiteral(_, index) => index.clone(),
            ASTExpression::ASTFunctionCallExpression(call) => call.index.clone(),
            ASTExpression::ValueRef(_, index) => index.clone(),
            ASTExpression::Value(_, index) => index.clone(),
            ASTExpression::Lambda(def) => def.index.clone(),
            ASTExpression::Any(_) => ASTIndex::none(),
        }
    }

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        match self {
            ASTExpression::ASTFunctionCallExpression(astfunction_call) => {
                ASTExpression::ASTFunctionCallExpression(
                    astfunction_call.fix_namespaces(enhanced_module),
                )
            }
            ASTExpression::Lambda(astlambda_def) => {
                ASTExpression::Lambda(astlambda_def.fix_namespaces(enhanced_module))
            }
            ASTExpression::Any(asttype) => {
                ASTExpression::Any(asttype.fix_namespaces(enhanced_module))
            }
            _ => self.clone(),
        }
    }

    pub fn from_ast(
        path: Option<PathBuf>,
        namespace: ASTNameSpace,
        expr: ast::ASTExpression,
    ) -> Self {
        match expr {
            ast::ASTExpression::StringLiteral(value, position) => {
                ASTExpression::StringLiteral(value, ASTIndex::from_position(path.clone(), position))
            }
            ast::ASTExpression::ASTFunctionCallExpression(call) => {
                ASTExpression::ASTFunctionCallExpression(ASTFunctionCall::from_ast(
                    path.clone(),
                    namespace.clone(),
                    call,
                ))
            }
            ast::ASTExpression::ValueRef(name, position) => {
                ASTExpression::ValueRef(name, ASTIndex::from_position(path.clone(), position))
            }
            ast::ASTExpression::Value(value_type, position) => ASTExpression::Value(
                ValueType::from_ast(path.clone(), namespace.clone(), value_type),
                ASTIndex::from_position(path.clone(), position),
            ),
            ast::ASTExpression::Lambda(lambda) => ASTExpression::Lambda(ASTLambdaDef::from_ast(
                path.clone(),
                namespace.clone(),
                lambda,
            )),
            ast::ASTExpression::Any(ast_type) => {
                ASTExpression::Any(ASTType::from_ast(path.clone(), namespace.clone(), ast_type))
            }
        }
    }
}

impl Display for ASTExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTExpression::StringLiteral(s, _) => f.write_str(&format!("\"{s}\"")),
            ASTExpression::ASTFunctionCallExpression(call) => {
                /*let pars: Vec<String> =
                    call.parameters.iter().map(|it| format!("{}", it)).collect();


                f.write_str(&format!("{}({})", call.function_name, pars.join(",")))

                 */
                f.write_str(&format!("{call}"))
            }
            ASTExpression::ValueRef(name, _index) => f.write_str(name),
            ASTExpression::Value(val_type, _) => match val_type {
                ValueType::Boolean(b) => f.write_str(&format!("{b}")),
                ValueType::I32(n) => f.write_str(&format!("{n}")),
                ValueType::F32(n) => f.write_str(&format!("{n}")),
                ValueType::Char(c) => f.write_str(&format!("'{c}'")),
            },
            ASTExpression::Lambda(lambda) => f.write_str(&format!("{lambda}")),
            ASTExpression::Any(ast_type) => f.write_str(&format!("Any({ast_type})")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTStatement {
    Expression(ASTExpression),
    LetStatement(String, ASTExpression, bool, ASTIndex),
}

impl ASTStatement {
    pub fn get_index(&self) -> ASTIndex {
        match self {
            ASTStatement::Expression(expr) => expr.get_index(),
            ASTStatement::LetStatement(_, _, _, index) => index.clone(),
        }
    }

    pub fn fix_namespaces(self, enhanced_module: &EnhancedASTModule) -> Self {
        match self {
            ASTStatement::Expression(exp) => {
                ASTStatement::Expression(exp.fix_namespaces(enhanced_module))
            }
            ASTStatement::LetStatement(name, exp, is_const, astindex) => {
                ASTStatement::LetStatement(
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
        namespace: ASTNameSpace,
        statement: ast::ASTStatement,
    ) -> Self {
        match statement {
            ast::ASTStatement::Expression(expr) => ASTStatement::Expression(
                ASTExpression::from_ast(path.clone(), namespace.clone(), expr),
            ),
            ast::ASTStatement::LetStatement(name, astexpression, is_const, position) => {
                let expr = ASTExpression::from_ast(path.clone(), namespace.clone(), astexpression);
                ASTStatement::LetStatement(
                    name,
                    expr,
                    is_const,
                    ASTIndex::from_position(path.clone(), position),
                )
            }
        }
    }
}

impl Display for ASTStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTStatement::Expression(e) => f.write_str(&format!("{e};\n")),
            ASTStatement::LetStatement(name, e, is_const, _index) => {
                let keyword = if *is_const { "const" } else { "let" };
                f.write_str(&format!("{keyword} {name} = {e};\n"))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTModifiers {
    pub public: bool,
}

impl ASTModifiers {
    pub fn public() -> Self {
        Self { public: true }
    }

    pub fn private() -> Self {
        Self { public: false }
    }
}

#[derive(Debug, Clone)]
pub struct ASTModule {
    pub path: PathBuf,
    pub body: Vec<ASTStatement>,
    pub functions: Vec<ASTFunctionDef>,
    pub enums: Vec<ASTEnumDef>,
    pub structs: Vec<ASTStructDef>,
    pub types: Vec<ASTTypeDef>,
    pub namespace: ASTNameSpace,
}

impl ASTModule {
    pub fn empty(namespace: ASTNameSpace) -> Self {
        ASTModule {
            path: Default::default(),
            body: vec![],
            functions: vec![],
            enums: vec![],
            structs: vec![],
            types: vec![],
            namespace,
        }
    }

    pub fn add_function(&mut self, function_def: ASTFunctionDef) {
        self.functions.push(function_def);
    }

    pub fn add(&mut self, mut module: ASTModule) {
        self.body.append(&mut module.body);
        self.functions.append(&mut module.functions);
        self.enums.append(&mut module.enums);
        self.structs.append(&mut module.structs);
        self.types.extend(module.types);
    }

    pub fn from_ast(module: ast::ASTModule, info: EhModuleInfo) -> Self {
        Self {
            path: info.path.clone().unwrap(),
            body: module
                .body
                .into_iter()
                .map(|it| ASTStatement::from_ast(info.path.clone(), info.namespace.clone(), it))
                .collect(),
            functions: module
                .functions
                .into_iter()
                .map(|it| ASTFunctionDef::from_ast(info.path.clone(), info.namespace.clone(), it))
                .collect(),
            enums: module
                .enums
                .into_iter()
                .map(|it| ASTEnumDef::from_ast(info.path.clone(), info.namespace.clone(), it))
                .collect(),
            structs: module
                .structs
                .into_iter()
                .map(|it| ASTStructDef::from_ast(info.path.clone(), info.namespace.clone(), it))
                .collect(),
            types: module
                .types
                .into_iter()
                .map(|it| ASTTypeDef::from_ast(info.path.clone(), info.namespace.clone(), it))
                .collect(),
            namespace: info.namespace.clone(),
        }
    }
}

pub trait CustomTypeDef: Display {
    fn name(&self) -> &str;
    fn modifiers(&self) -> &ASTModifiers;
    fn namespace(&self) -> &ASTNameSpace;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTEnumDef {
    pub namespace: ASTNameSpace,
    pub name: String,
    pub type_parameters: Vec<String>,
    pub variants: Vec<ASTEnumVariantDef>,
    pub index: ASTIndex,
    pub modifiers: ASTModifiers,
}

impl CustomTypeDef for ASTEnumDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &ASTNameSpace {
        &self.namespace
    }
}

impl Display for ASTEnumDef {
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

impl ASTEnumDef {
    pub fn variant_function_name(&self, variant: &ASTEnumVariantDef) -> String {
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
        namespace: ASTNameSpace,
        enum_def: ast::ASTEnumDef,
    ) -> Self {
        Self {
            namespace: namespace.clone(),
            name: enum_def.name,
            type_parameters: enum_def.type_parameters,
            variants: enum_def
                .variants
                .into_iter()
                .map(|it| ASTEnumVariantDef::from_ast(path.clone(), namespace.clone(), it))
                .collect(),
            index: ASTIndex::from_position(path, enum_def.index),
            modifiers: ASTModifiers {
                public: enum_def.modifiers.public,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTEnumVariantDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub index: ASTIndex,
}

impl ASTEnumVariantDef {
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
        namespace: ASTNameSpace,
        variant_def: ast::ASTEnumVariantDef,
    ) -> Self {
        Self {
            name: variant_def.name,
            parameters: ASTParameterDef::from_asts(
                path.clone(),
                namespace.clone(),
                variant_def.parameters,
            ),
            index: ASTIndex::from_position(path.clone(), variant_def.index),
        }
    }
}

impl Display for ASTEnumVariantDef {
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
pub struct ASTStructDef {
    pub namespace: ASTNameSpace,
    pub name: String,
    pub type_parameters: Vec<String>,
    pub properties: Vec<ASTStructPropertyDef>,
    pub index: ASTIndex,
    pub modifiers: ASTModifiers,
}

impl ASTStructDef {
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
        namespace: ASTNameSpace,
        struct_def: ast::ASTStructDef,
    ) -> Self {
        Self {
            namespace: namespace.clone(),
            name: struct_def.name,
            type_parameters: struct_def.type_parameters,
            properties: struct_def
                .properties
                .into_iter()
                .map(|it| ASTStructPropertyDef::from_ast(path.clone(), namespace.clone(), it))
                .collect(),
            index: ASTIndex::from_position(path.clone(), struct_def.index),
            modifiers: ASTModifiers {
                public: struct_def.modifiers.public,
            },
        }
    }
}

impl CustomTypeDef for ASTStructDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &ASTNameSpace {
        &self.namespace
    }
}

impl Display for ASTStructDef {
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
pub struct ASTTypeDef {
    pub namespace: ASTNameSpace,
    pub name: String,
    pub type_parameters: Vec<String>,
    pub is_ref: bool,
    pub index: ASTIndex,
    pub modifiers: ASTModifiers,
    pub native_type: Option<String>,
}
impl ASTTypeDef {
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
        namespace: ASTNameSpace,
        type_def: ast::ASTTypeDef,
    ) -> Self {
        Self {
            namespace: namespace.clone(),
            name: type_def.name,
            type_parameters: type_def.type_parameters,
            is_ref: type_def.is_ref,
            index: ASTIndex::from_position(path.clone(), type_def.index),
            modifiers: ASTModifiers {
                public: type_def.modifiers.public,
            },
            native_type: type_def.native_type,
        }
    }
}

impl CustomTypeDef for ASTTypeDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &ASTNameSpace {
        &self.namespace
    }
}

impl Display for ASTTypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("type {}", self.name))
    }
}

pub fn lambda(return_type: ASTType) -> ASTType {
    ASTType::Builtin(BuiltinTypeKind::Lambda {
        parameters: Vec::new(),
        return_type: Box::new(return_type),
    })
}

pub fn lambda_unit() -> ASTType {
    lambda(ASTType::Unit)
}

#[cfg(test)]
mod tests {
    use crate::codegen::eh_ast::{
        ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModifiers, ASTNameSpace, ASTParameterDef,
        ASTType, BuiltinTypeKind,
    };
    use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
    use crate::utils::tests::test_namespace;

    #[test]
    fn display_custom_type() {
        let inner_type = ASTType::Custom {
            namespace: test_namespace(),
            name: "Option".to_owned(),
            param_types: vec![ASTType::Builtin(BuiltinTypeKind::String)],
            index: ASTIndex::none(),
        };

        let ast_type = ASTType::Custom {
            namespace: test_namespace(),
            name: "List".to_owned(),
            param_types: vec![inner_type],
            index: ASTIndex::none(),
        };
        assert_eq!(format!("{ast_type}"), "List<Option<str>>");
    }

    #[test]
    fn display_function_def() {
        let inner_type = ASTType::Custom {
            namespace: test_namespace(),
            name: "Option".to_owned(),
            param_types: vec![ASTType::Generic(ASTIndex::none(), "T".to_string())],
            index: ASTIndex::none(),
        };

        let ast_type = ASTType::Custom {
            namespace: test_namespace(),
            name: "List".to_owned(),
            param_types: vec![inner_type],
            index: ASTIndex::none(),
        };

        let def = ASTFunctionDef {
            original_name: "aFun".to_string(),
            name: "aFun".to_string(),
            parameters: vec![ASTParameterDef {
                name: "aPar".to_string(),
                ast_type,
                ast_index: ASTIndex::none(),
            }],
            return_type: ASTType::Generic(ASTIndex::none(), "T".to_string()),
            body: ASTFunctionBody::RASMBody(vec![]),
            inline: false,
            generic_types: vec!["T".to_string()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
            modifiers: ASTModifiers { public: false },
            namespace: ASTNameSpace {
                lib: "".to_string(),
                path: "".to_string(),
            },
            rank: 0,
        };

        assert_eq!(format!("{def}"), "fn aFun<T>(aPar: List<Option<T>>) -> T");
    }
}
