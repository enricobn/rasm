use linked_hash_map::LinkedHashMap;
use crate::codegen::MemoryValue;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedFunctionDef {
    pub name: String,
    pub parameters: Vec<ASTTypedParameterDef>,
    pub return_type: Option<ASTTypedTypeRef>,
    pub body: ASTTypedFunctionBody,
    pub inline: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedLambdaDef {
    //pub parameter_names: Vec<String>,
    pub body: Vec<ASTTypedExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedFunctionBody {
    RASMBody(Vec<ASTTypedExpression>),
    ASMBody(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinTypedTypeKind {
    ASTString,
    ASTI32,
    Lambda { parameters: Vec<ASTTypedTypeRef>, return_type: Option<Box<ASTTypedTypeRef>> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedType {
    Builtin(BuiltinTypedTypeKind),
    Enum { name: String, param_types: Vec<ASTTypedTypeRef> },
    Struct { name: String, param_types: Vec<ASTTypedTypeRef> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedParameterDef {
    pub name: String,
    pub type_ref: ASTTypedTypeRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedStructPropertyDef {
    pub name: String,
    pub type_ref: ASTTypedTypeRef,
}

impl ASTTypedParameterDef {
    pub fn new(name: &str, type_ref: ASTTypedTypeRef) -> ASTTypedParameterDef {
        Self { name: name.into(), type_ref }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedTypeRef {
    pub ast_type: ASTTypedType,
    pub ast_ref: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedFunctionCall {
    pub function_name: String,
    pub parameters: Vec<ASTTypedExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedExpression {
    StringLiteral(String),
    ASTFunctionCallExpression(ASTTypedFunctionCall),
    Val(String),
    Number(i32),
    Lambda(ASTTypedLambdaDef),
    //EnumConstructor { name: String, variant: String, parameters: Vec<ASTExpression> },
}

#[derive(Debug, Clone)]
pub struct ASTTypedModule {
    pub body: Vec<ASTTypedFunctionCall>,
    pub functions_by_name: LinkedHashMap<String, ASTTypedFunctionDef>,
    pub enums: Vec<ASTTypedEnumDef>,
    pub structs: Vec<ASTTypedStructDef>,
    pub native_body: String,
    pub statics: LinkedHashMap<String, MemoryValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedEnumDef {
    pub name: String,
    pub variants: Vec<ASTTypedEnumVariantDef>,
}

impl ASTTypedEnumDef {

    pub fn variant_function_name(&self, variant: &ASTTypedEnumVariantDef) -> String {
        let mut result = String::new();
        result.push_str(&self.name);
        result.push_str("::");
        result.push_str(&variant.name);
        result
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedEnumVariantDef {
    pub name: String,
    pub parameters: Vec<ASTTypedParameterDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedStructDef {
    pub name: String,
    pub properties: Vec<ASTTypedStructPropertyDef>,
}