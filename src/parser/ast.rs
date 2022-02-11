#[derive(Debug, Clone)]
pub struct ASTFunctionDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub return_type: Option<ASTReturnType>,
    pub body: ASTFunctionBody,
    pub inline: bool,
}

#[derive(Debug, Clone)]
pub struct ASTReturnType {
    pub type_ref: ASTTypeRef,
    pub register: String,
}

#[derive(Debug, Clone)]
pub enum ASTFunctionBody {
    RASMBody(Vec<ASTFunctionCall>),
    ASMBody(String)
}

#[derive(Debug, Clone)]
pub enum BuiltinTypeKind {
    ASTString,
    ASTI32,
    Lambda,
}

#[derive(Debug, Clone)]
pub enum ASTType {
    BuiltinType(BuiltinTypeKind)
}

#[derive(Debug, Clone)]
pub struct ASTParameterDef {
    pub name: String,
    pub type_ref: ASTTypeRef,
}

#[derive(Debug, Clone)]
pub struct ASTTypeRef {
    pub ast_ref: bool,
    pub ast_type: ASTType,
}

#[derive(Debug,Clone)]
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
    Lambda(ASTFunctionDef)
}

#[derive(Debug, Clone)]
pub struct ASTModule {
    pub body: Vec<ASTFunctionCall>,
    pub functions: Vec<ASTFunctionDef>
}