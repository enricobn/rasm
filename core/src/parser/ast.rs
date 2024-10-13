use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::path::PathBuf;

use linked_hash_map::LinkedHashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ASTPosition {
    pub row: usize,
    pub column: usize,
}

impl ASTPosition {
    pub fn new(row: usize, column: usize) -> Self {
        Self { row, column }
    }

    pub fn none() -> Self {
        Self { row: 0, column: 0 }
    }

    pub fn mv_left(&mut self, len: usize) -> Self {
        Self {
            row: self.row,
            column: self.column - len,
        }
    }
}

impl Display for ASTPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.row, self.column)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub return_type: ASTType,
    pub body: ASTFunctionBody,
    pub inline: bool,
    pub generic_types: Vec<String>,
    pub index: ASTPosition,
    pub modifiers: ASTModifiers,
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
    pub parameter_names: Vec<(String, ASTPosition)>,
    pub body: Vec<ASTStatement>,
    pub index: ASTPosition,
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
    Generic(ASTPosition, String),
    Custom {
        name: String,
        param_types: Vec<ASTType>,
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        index: ASTPosition,
    },
    Unit,
}

impl ASTType {
    pub fn is_unit(&self) -> bool {
        self == &ASTType::Unit
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

    pub fn is_reference_by_module(&self, module: &ASTModule) -> bool {
        if let ASTType::Builtin(BuiltinTypeKind::String) = self {
            true
        } else if let ASTType::Custom {
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
    pub index: ASTPosition,
}

impl Display for ASTParameterDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}: {}", self.name, self.ast_type))
    }
}

impl ASTParameterDef {
    pub fn new(name: &str, ast_type: ASTType, index: ASTPosition) -> ASTParameterDef {
        ASTParameterDef {
            name: name.into(),
            ast_type,
            index,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTStructPropertyDef {
    pub name: String,
    pub ast_type: ASTType,
    pub index: ASTPosition,
}

impl Display for ASTStructPropertyDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}: {}", self.name, self.ast_type))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionCall {
    pub function_name: String,
    pub parameters: Vec<ASTExpression>,
    pub index: ASTPosition,
    pub generics: Vec<ASTType>,
}

impl Display for ASTFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars: Vec<String> = self.parameters.iter().map(|it| format!("{}", it)).collect();

        f.write_str(&format!("{}({})", self.function_name, pars.join(",")))
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
}

// TODO can we do partialeq? It depends on ASTIndex
#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpression {
    StringLiteral(String, ASTPosition),
    ASTFunctionCallExpression(ASTFunctionCall),
    ValueRef(String, ASTPosition),
    Value(ValueType, ASTPosition),
    Lambda(ASTLambdaDef),
    Any(ASTType), //EnumConstructor { name: String, variant: String, parameters: Vec<ASTExpression> },
}

impl ASTExpression {
    pub fn get_index(&self) -> ASTPosition {
        match self {
            ASTExpression::StringLiteral(_, index) => index.clone(),
            ASTExpression::ASTFunctionCallExpression(call) => call.index.clone(),
            ASTExpression::ValueRef(_, index) => index.clone(),
            ASTExpression::Value(_, index) => index.clone(),
            ASTExpression::Lambda(def) => def.index.clone(),
            ASTExpression::Any(_) => ASTPosition::none(),
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
    LetStatement(String, ASTExpression, bool, ASTPosition),
}

impl ASTStatement {
    pub fn get_index(&self) -> ASTPosition {
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

pub trait MyToString {
    fn my_to_string(&self) -> String;
}

/*
impl MyToString for HashMap<String, ASTType> {
    fn my_to_string(&self) -> String {
        let pars: Vec<String> = self
            .iter()
            .map(|(name, it)| format!("{name}={it}"))
            .collect();
        pars.join(",")
    }
}
*/

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
    pub body: Vec<ASTStatement>,
    pub functions: Vec<ASTFunctionDef>,
    pub enums: Vec<ASTEnumDef>,
    pub structs: Vec<ASTStructDef>,
    pub types: Vec<ASTTypeDef>,
}

impl ASTModule {
    pub fn empty() -> Self {
        ASTModule {
            body: vec![],
            functions: vec![],
            enums: vec![],
            structs: vec![],
            types: vec![],
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
}

pub trait CustomTypeDef: Display {
    fn name(&self) -> &str;
    fn modifiers(&self) -> &ASTModifiers;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTEnumDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub variants: Vec<ASTEnumVariantDef>,
    pub index: ASTPosition,
    pub modifiers: ASTModifiers,
}

impl CustomTypeDef for ASTEnumDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTEnumVariantDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub index: ASTPosition,
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
    pub index: ASTPosition,
    pub modifiers: ASTModifiers,
}

impl CustomTypeDef for ASTStructDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
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
        f.write_str(&format!("struct {}({pars})", self.name))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTTypeDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub is_ref: bool,
    pub index: ASTPosition,
    pub modifiers: ASTModifiers,
    pub native_type: Option<String>,
}

impl CustomTypeDef for ASTTypeDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
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
    use crate::parser::ast::{
        ASTFunctionBody, ASTFunctionDef, ASTModifiers, ASTParameterDef, ASTPosition, ASTType,
        BuiltinTypeKind,
    };

    #[test]
    fn display_custom_type() {
        let inner_type = ASTType::Custom {
            name: "Option".to_owned(),
            param_types: vec![ASTType::Builtin(BuiltinTypeKind::String)],
            index: ASTPosition::none(),
        };

        let ast_type = ASTType::Custom {
            name: "List".to_owned(),
            param_types: vec![inner_type],
            index: ASTPosition::none(),
        };
        assert_eq!(format!("{ast_type}"), "List<Option<str>>");
    }

    #[test]
    fn display_function_def() {
        let inner_type = ASTType::Custom {
            name: "Option".to_owned(),
            param_types: vec![ASTType::Generic(ASTPosition::none(), "T".to_string())],
            index: ASTPosition::none(),
        };

        let ast_type = ASTType::Custom {
            name: "List".to_owned(),
            param_types: vec![inner_type],
            index: ASTPosition::none(),
        };

        let def = ASTFunctionDef {
            name: "aFun".to_string(),
            parameters: vec![ASTParameterDef {
                name: "aPar".to_string(),
                ast_type,
                index: ASTPosition::none(),
            }],
            return_type: ASTType::Generic(ASTPosition::none(), "T".to_string()),
            body: ASTFunctionBody::RASMBody(vec![]),
            inline: false,
            generic_types: vec!["T".to_string()],
            index: ASTPosition::none(),
            modifiers: ASTModifiers { public: false },
        };

        assert_eq!(format!("{def}"), "fn aFun<T>(aPar: List<Option<T>>) -> T");
    }
}
