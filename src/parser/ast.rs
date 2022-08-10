use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::string::ToString;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub return_type: Option<ASTTypeRef>,
    pub body: ASTFunctionBody,
    pub inline: bool,
    pub param_types: Vec<String>,
}

impl Display for ASTFunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pt = if self.param_types.is_empty() {
            "".into()
        } else {
            format!("<{}>", self.param_types.join(","))
        };

        let rt = if let Some(rt) = &self.return_type {
            format!("{}", rt)
        } else {
            "()".into()
        };

        let args = self
            .parameters
            .iter()
            .map(|it| format!("{}", it))
            .collect::<Vec<String>>()
            .join(",");
        f.write_str(&format!("{}{pt}({args}) -> {rt}", self.name))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTLambdaDef {
    pub parameter_names: Vec<String>,
    pub body: Vec<ASTExpression>,
}

impl Display for ASTLambdaDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self.parameter_names.join(",");
        let body = self.body.iter().map(|it| format!("{it};")).collect::<Vec<String>>().join("");

        f.write_str(&format!("{{ {pars} -> {body} }}"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTFunctionBody {
    RASMBody(Vec<ASTExpression>),
    ASMBody(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinTypeKind {
    ASTString,
    ASTI32,
    Lambda {
        parameters: Vec<ASTTypeRef>,
        return_type: Option<Box<ASTTypeRef>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ASTType {
    Builtin(BuiltinTypeKind),
    Parametric(String),
    Custom {
        name: String,
        param_types: Vec<ASTTypeRef>,
    },
}

impl Display for ASTType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::ASTString => f.write_str("str"),
                BuiltinTypeKind::ASTI32 => f.write_str("i32"),
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let pars: Vec<String> = parameters.iter().map(|it| format!("{it}")).collect();

                    let formatted_return_type = if let Some(rt) = return_type {
                        format!("{}", *rt)
                    } else {
                        "()".into()
                    };

                    f.write_str(&format!(
                        "fn ({}) -> {}",
                        pars.join(","),
                        formatted_return_type
                    ))
                }
            },
            ASTType::Parametric(name) => f.write_str(name),
            ASTType::Custom { name, param_types } => {
                let pars: Vec<String> = param_types.iter().map(|it| format!("{it}")).collect();

                if pars.is_empty() {
                    f.write_str(name)
                } else {
                    f.write_str(&format!("{name}<{}>", pars.join(",")))
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTParameterDef {
    pub name: String,
    pub type_ref: ASTTypeRef,
}

impl Display for ASTParameterDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}", self.name, self.type_ref))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTStructPropertyDef {
    pub name: String,
    pub type_ref: ASTTypeRef,
}

impl ASTParameterDef {
    pub fn new(name: &str, type_ref: ASTTypeRef) -> ASTParameterDef {
        ASTParameterDef {
            name: name.into(),
            type_ref,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ASTTypeRef {
    pub ast_type: ASTType,
    pub ast_ref: bool,
}

impl Display for ASTTypeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.ast_ref {
            f.write_str("&")?
        }
        f.write_str(&format!("{}", self.ast_type))
    }
}

impl ASTTypeRef {
    pub fn parametric(name: &str, ast_ref: bool) -> ASTTypeRef {
        ASTTypeRef {
            ast_type: ASTType::Parametric(name.into()),
            ast_ref,
        }
    }

    pub fn custom(name: &str, ast_ref: bool, param_types: Vec<ASTTypeRef>) -> ASTTypeRef {
        ASTTypeRef {
            ast_type: ASTType::Custom {
                name: name.into(),
                param_types,
            },
            ast_ref,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionCall {
    pub original_function_name: String,
    pub function_name: String,
    pub parameters: Vec<ASTExpression>,
}

impl Display for ASTFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars: Vec<String> = self.parameters.iter().map(|it| format!("{}", it)).collect();

        f.write_str(&format!("{}({})", self.function_name, pars.join(",")))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpression {
    StringLiteral(String),
    ASTFunctionCallExpression(ASTFunctionCall),
    Val(String),
    Number(i32),
    Lambda(ASTLambdaDef),
    //EnumConstructor { name: String, variant: String, parameters: Vec<ASTExpression> },
}

impl Display for ASTExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTExpression::StringLiteral(s) => f.write_str(&"\"s\"".to_string()),
            ASTExpression::ASTFunctionCallExpression(call) => {
                let pars: Vec<String> =
                    call.parameters.iter().map(|it| format!("{}", it)).collect();
                f.write_str(&format!("{}({})", call.function_name, pars.join(",")))
            }
            ASTExpression::Val(p) => f.write_str(p),
            ASTExpression::Number(b) => f.write_str(&format!("{b}")),
            ASTExpression::Lambda(lambda) => f.write_str(&format!("{lambda}")),
        }
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

impl Display for dyn MyToString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.my_to_string())
    }
}

#[derive(Debug, Clone)]
pub struct ASTModule {
    pub body: Vec<ASTFunctionCall>,
    pub functions: Vec<ASTFunctionDef>,
    pub enums: Vec<ASTEnumDef>,
    pub structs: Vec<ASTStructDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTEnumDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub variants: Vec<ASTEnumVariantDef>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct ASTEnumVariantDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct ASTStructDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub properties: Vec<ASTStructPropertyDef>,
}

pub fn lambda(return_type: Option<Box<ASTTypeRef>>) -> ASTType {
    ASTType::Builtin(BuiltinTypeKind::Lambda {
        parameters: Vec::new(),
        return_type,
    })
}

pub fn lambda_unit() -> ASTType {
    lambda(None)
}
