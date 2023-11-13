use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::path::PathBuf;

use crate::project::RasmProject;
use linked_hash_map::LinkedHashMap;

use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::typed_ast::{ASTTypedType, BuiltinTypedTypeKind};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTNameSpace {
    lib: String,
    path: String,
}

impl ASTNameSpace {
    pub fn new(lib: String, path: String) -> Self {
        Self { lib, path }
    }

    pub fn global() -> Self {
        Self {
            lib: "".to_string(),
            path: "".to_string(),
        }
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

        let args = self
            .parameters
            .iter()
            .map(|it| format!("{}", it))
            .collect::<Vec<String>>()
            .join(",");
        f.write_str(&format!("{}{generic_types}({args}) -> {rt}", self.name))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTLambdaDef {
    pub parameter_names: Vec<(String, ASTIndex)>,
    pub body: Vec<ASTStatement>,
    pub index: ASTIndex,
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

        f.write_str(&format!("{{ {pars} -> {body} }}"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTFunctionBody {
    RASMBody(Vec<ASTStatement>),
    ASMBody(String),
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
    Generic(String),
    Custom {
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
            ASTType::Generic(name) => f.write_str(name),
            ASTType::Custom {
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
        f.write_str(&format!("{}:{}", self.name, self.ast_type))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTStructPropertyDef {
    pub name: String,
    pub ast_type: ASTType,
    pub index: ASTIndex,
}

impl Display for ASTStructPropertyDef {
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionCall {
    pub original_function_name: String,
    pub function_name: String,
    pub parameters: Vec<ASTExpression>,
    pub index: ASTIndex,
    pub generics: Vec<ASTType>,
}

impl Display for ASTFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars: Vec<String> = self.parameters.iter().map(|it| format!("{}", it)).collect();

        f.write_str(&format!("{}({})", self.function_name, pars.join(",")))
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

    pub fn mv(&self, offset: i32) -> Self {
        Self {
            file_name: self.file_name.clone(),
            row: self.row,
            column: (self.column as i32 + offset) as usize,
        }
    }

    pub fn mv_left(&self, offset: usize) -> Self {
        Self {
            file_name: self.file_name.clone(),
            row: self.row,
            column: (self.column as i32 - (offset as i32)) as usize,
        }
    }
}

impl Display for ASTIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "{}:{}:{}",
            &self
                .file_name
                .clone()
                .map(|it| format!("file:///{}", it.canonicalize().unwrap().to_str().unwrap()))
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
    Char(char),
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
}

// TODO can we do partialeq? It depends on ASTIndex
#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpression {
    StringLiteral(String),
    ASTFunctionCallExpression(ASTFunctionCall),
    ValueRef(String, ASTIndex),
    Value(ValueType, ASTIndex),
    Lambda(ASTLambdaDef),
    Any(ASTType), //EnumConstructor { name: String, variant: String, parameters: Vec<ASTExpression> },
}

impl ASTExpression {
    pub fn get_index(&self) -> ASTIndex {
        match self {
            ASTExpression::StringLiteral(_) => ASTIndex::none(),
            ASTExpression::ASTFunctionCallExpression(call) => call.index.clone(),
            ASTExpression::ValueRef(_, index) => index.clone(),
            ASTExpression::Value(_, index) => index.clone(),
            ASTExpression::Lambda(def) => def.index.clone(),
            ASTExpression::Any(_) => ASTIndex::none(),
        }
    }
}

impl Display for ASTExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTExpression::StringLiteral(s) => f.write_str(&format!("\"{s}\"")),
            ASTExpression::ASTFunctionCallExpression(call) => {
                let pars: Vec<String> =
                    call.parameters.iter().map(|it| format!("{}", it)).collect();
                f.write_str(&format!("{}({})", call.function_name, pars.join(",")))
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
    public: bool,
}

impl ASTModifiers {
    pub fn public() -> Self {
        Self { public: true }
    }

    pub fn private() -> Self {
        Self { public: false }
    }
}

pub trait MyToString {
    fn my_to_string(&self) -> String;
}

impl MyToString for HashMap<String, ASTType> {
    fn my_to_string(&self) -> String {
        let pars: Vec<String> = self
            .iter()
            .map(|(name, it)| format!("{name}={it}"))
            .collect();
        pars.join(",")
    }
}

impl MyToString for LinkedHashMap<String, ASTType> {
    fn my_to_string(&self) -> String {
        let pars: Vec<String> = self
            .iter()
            .map(|(name, it)| format!("{name}={it}"))
            .collect();
        pars.join(",")
    }
}

impl Display for dyn MyToString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.my_to_string())
    }
}

#[derive(Debug, Clone)]
pub struct ASTModule {
    pub path: PathBuf,
    pub body: Vec<ASTStatement>,
    pub functions: Vec<ASTFunctionDef>,
    pub enums: Vec<ASTEnumDef>,
    pub structs: Vec<ASTStructDef>,
    pub requires: HashSet<String>,
    pub externals: HashSet<String>,
    pub types: Vec<ASTTypeDef>,
    pub namespace: ASTNameSpace,
}

impl ASTModule {
    pub fn add_function(&mut self, function_def: ASTFunctionDef) {
        self.functions.push(function_def);
    }

    pub fn add(&mut self, mut module: ASTModule) {
        self.body.append(&mut module.body);
        self.functions.append(&mut module.functions);
        self.enums.append(&mut module.enums);
        self.structs.append(&mut module.structs);
        self.requires.extend(module.requires);
        self.externals.extend(module.externals);
        self.types.extend(module.types);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTEnumDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub variants: Vec<ASTEnumVariantDef>,
    pub index: ASTIndex,
    pub modifiers: ASTModifiers,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTEnumVariantDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub index: ASTIndex,
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
    pub name: String,
    pub type_parameters: Vec<String>,
    pub properties: Vec<ASTStructPropertyDef>,
    pub index: ASTIndex,
    pub modifiers: ASTModifiers,
}

impl Display for ASTStructDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .properties
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<_>>()
            .join(",");
        f.write_str(&format!("struct {}({pars})", self.name))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTTypeDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub is_ref: bool,
    pub index: ASTIndex,
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
    use crate::parser::ast::{ASTIndex, ASTType, BuiltinTypeKind};

    #[test]
    fn display() {
        let inner_type = ASTType::Custom {
            name: "Option".to_owned(),
            param_types: vec![ASTType::Builtin(BuiltinTypeKind::String)],
            index: ASTIndex::none(),
        };

        let ast_type = ASTType::Custom {
            name: "List".to_owned(),
            param_types: vec![inner_type],
            index: ASTIndex::none(),
        };
        assert_eq!(format!("{ast_type}"), "List<Option<str>>");
    }
}
