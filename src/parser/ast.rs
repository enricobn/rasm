#[derive(Debug, Clone)]
pub struct ASTFunctionDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub return_type: Option<ASTTypeRef>,
    pub body: ASTFunctionBody,
    pub inline: bool,
    pub param_types: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ASTLambdaDef {
    pub parameter_names: Vec<String>,
    pub body: ASTFunctionBody,
}

#[derive(Debug, Clone)]
pub enum ASTFunctionBody {
    RASMBody(Vec<ASTFunctionCall>),
    ASMBody(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinTypeKind {
    ASTString,
    ASTI32,
    Lambda { parameters: Vec<ASTTypeRef>, return_type: Option<Box<ASTTypeRef>> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTType {
    Builtin(BuiltinTypeKind),
    Parametric(String),
    Custom { name: String, param_types: Vec<ASTTypeRef> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTParameterDef {
    pub name: String,
    pub type_ref: ASTTypeRef,
    pub from_context: bool,
}

impl ASTParameterDef {
    pub fn new(name: &str, type_ref: ASTTypeRef, from_context: bool) -> ASTParameterDef {
        ASTParameterDef { name: name.into(), type_ref, from_context }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypeRef {
    pub ast_type: ASTType,
    pub ast_ref: bool,
}

impl ASTTypeRef {
    pub fn parametric(name: &str, ast_ref: bool) -> ASTTypeRef {
        ASTTypeRef { ast_type: ASTType::Parametric(name.into()), ast_ref }
    }

    pub fn custom(name: &str, ast_ref: bool, param_types: Vec<ASTTypeRef>) -> ASTTypeRef {
        ASTTypeRef { ast_type: ASTType::Custom { name: name.into(), param_types}, ast_ref }
    }
}

#[derive(Debug, Clone)]
pub struct ASTFunctionCall {
    pub function_name: String,
    pub parameters: Vec<ASTExpression>,
}

#[derive(Debug, Clone)]
pub enum ASTExpression {
    StringLiteral(String),
    ASTFunctionCallExpression(ASTFunctionCall),
    Var(String),
    Number(i32),
    Lambda(ASTLambdaDef),
    //EnumConstructor { name: String, variant: String, parameters: Vec<ASTExpression> },
}

#[derive(Debug, Clone)]
pub struct ASTModule {
    pub body: Vec<ASTFunctionCall>,
    pub functions: Vec<ASTFunctionDef>,
    pub enums: Vec<ASTEnumDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTEnumDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub variants: Vec<ASTEnumVariantDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTEnumVariantDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
}

pub fn lambda(return_type: Option<Box<ASTTypeRef>>) -> ASTType {
    ASTType::Builtin(BuiltinTypeKind::Lambda { parameters: Vec::new(), return_type })
}

pub fn lambda_unit() -> ASTType {
    lambda(None)
}