use std::collections::HashMap;
use crate::codegen::{EnhancedASTModule, MemoryValue};
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
    ASTLambdaDef, ASTModule, ASTParameterDef, ASTStructDef, ASTType, ASTTypeRef, BuiltinTypeKind,
};
use linked_hash_map::LinkedHashMap;

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
    Lambda {
        parameters: Vec<ASTTypedTypeRef>,
        return_type: Option<Box<ASTTypedTypeRef>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedType {
    Builtin(BuiltinTypedTypeKind),
    Enum {
        name: String,
        param_types: Vec<ASTTypedTypeRef>,
    },
    Struct {
        name: String,
        param_types: Vec<ASTTypedTypeRef>,
    },
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
        Self {
            name: name.into(),
            type_ref,
        }
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

pub fn convert_to_typed_module(
    module: &EnhancedASTModule,
    new_body: Vec<ASTFunctionCall>,
    new_function_defs: HashMap<String, ASTFunctionDef>,
    used_untyped_function_defs: HashMap<String, ASTFunctionDef>
) -> ASTTypedModule {

    let mut functions_by_name = LinkedHashMap::new();

    for (name, new_function_def) in new_function_defs.iter() {
        functions_by_name.insert(name.clone(), function_def(new_function_def));
    }

    for (name, new_function_def) in used_untyped_function_defs.iter() {
        functions_by_name.insert(name.clone(), function_def(new_function_def));
    }

    ASTTypedModule {
        body: new_body.iter().map(function_call).collect(),
        structs: Vec::new(),
        enums: Vec::new(),
        functions_by_name,
        statics: module.statics.clone(),
        native_body: module.native_body.clone(),
    }
}

fn function_def(def: &ASTFunctionDef) -> ASTTypedFunctionDef {
    ASTTypedFunctionDef {
        name: def.name.clone(),
        body: body(&def.body),
        return_type: def
            .return_type
            .clone()
            .map(|it| type_ref(&it, &format!("function {} return type", def.name))),
        inline: def.inline,
        parameters: def
            .parameters
            .iter()
            .map(|it| parameter_def(it, &format!("function {}", def.name)))
            .collect(),
    }
}

fn expression(expression: &ASTExpression) -> ASTTypedExpression {
    match expression {
        ASTExpression::StringLiteral(s) => ASTTypedExpression::StringLiteral(s.to_string()),
        ASTExpression::ASTFunctionCallExpression(fc) => {
            ASTTypedExpression::ASTFunctionCallExpression(function_call(fc))
        }
        ASTExpression::Val(v) => ASTTypedExpression::Val(v.clone()),
        ASTExpression::Number(n) => ASTTypedExpression::Number(*n),
        ASTExpression::Lambda(l) => ASTTypedExpression::Lambda(lambda_def(l)),
    }
}

fn lambda_def(lambda_def: &ASTLambdaDef) -> ASTTypedLambdaDef {
    ASTTypedLambdaDef {
        body: lambda_def.body.iter().map(expression).collect(),
    }
}

fn body(body: &ASTFunctionBody) -> ASTTypedFunctionBody {
    match body {
        ASTFunctionBody::RASMBody(body) => {
            ASTTypedFunctionBody::RASMBody(body.iter().map(|it| expression(it)).collect())
        }
        ASTFunctionBody::ASMBody(body) => ASTTypedFunctionBody::ASMBody(body.clone()),
    }
}

fn struct_def(struct_def: &ASTStructDef) -> ASTTypedStructDef {
    todo!()
}

fn enum_def(enum_def: &ASTEnumDef) -> ASTTypedEnumDef {
    ASTTypedEnumDef {
        name: enum_def.name.clone(),
        variants: enum_def
            .variants
            .iter()
            .map(|it| enum_variant(it, &format!("enum {}", enum_def.name)))
            .collect(),
    }
}

fn enum_variant(variant: &ASTEnumVariantDef, message: &str) -> ASTTypedEnumVariantDef {
    ASTTypedEnumVariantDef {
        name: variant.name.clone(),
        parameters: variant
            .parameters
            .iter()
            .map(|it| parameter_def(it, &format!("{message}, variant {}", variant.name)))
            .collect(),
    }
}

fn function_call(function_call: &ASTFunctionCall) -> ASTTypedFunctionCall {
    ASTTypedFunctionCall {
        function_name: function_call.function_name.clone(),
        parameters: function_call.parameters.iter().map(expression).collect(),
    }
}

fn parameter_def(parameter_def: &ASTParameterDef, message: &str) -> ASTTypedParameterDef {
    ASTTypedParameterDef {
        name: parameter_def.name.clone(),
        type_ref: type_ref(
            &parameter_def.type_ref,
            &format!("{message}: parameter {}", parameter_def.name),
        ),
    }
}

fn type_ref(t_ref: &ASTTypeRef, message: &str) -> ASTTypedTypeRef {
    let ast_type = match &t_ref.ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::ASTString => ASTTypedType::Builtin(BuiltinTypedTypeKind::ASTString),
            BuiltinTypeKind::ASTI32 => ASTTypedType::Builtin(BuiltinTypedTypeKind::ASTI32),
            BuiltinTypeKind::Lambda {
                return_type,
                parameters,
            } => ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: parameters
                    .iter()
                    .map(|it| type_ref(it, &(message.to_owned() + ", lambda parameter")))
                    .collect(),
                return_type: return_type.clone().map(|it| {
                    Box::new(type_ref(
                        &it,
                        &(message.to_owned() + ", lambda return type"),
                    ))
                }),
            }),
        },
        ASTType::Parametric(p) => {
            panic!("Unresolved parametric type '{p}': {message}");
        }
        ASTType::Custom { name, param_types } => {
            /*
            if let Some(enum_def) = self.module.enums.iter().find(|it| &it.name == name) {
                ASTTypedType::Enum {
                    name: name.clone(),
                    param_types: param_types
                        .iter()
                        .map(|it| type_ref(it, &format!("{message}, enum {name}")))
                        .collect(),
                }
            } else if let Some(struct_def) =
            self.module.structs.iter().find(|it| &it.name == name)
            {
                ASTTypedType::Struct {
                    name: name.clone(),
                    param_types: param_types
                        .iter()
                        .map(|it| type_ref(it, &format!("{message}, struct {name}")))
                        .collect(),
                }
            } else if let Some(resolved_enum_def) = self
                .enums
                .iter()
                .flat_map(|it| it.1.iter())
                .find(|it| &it.enum_def.name == name)
            {
                ASTTypedType::Enum {
                    name: name.clone(),
                    param_types: resolved_enum_def
                        .parameter_types
                        .iter()
                        .map(|it| self.type_ref(it, &format!("{message}, enum {name}")))
                        .collect(),
                }
            } else {
                panic!("Cannot find Custom type '{name}'");
            }

             */
            ASTTypedType::Enum {
                name: name.clone(),
                param_types: param_types.iter().map(|it| type_ref(it, message)).collect(),
            }
        }
    };

    ASTTypedTypeRef {
        ast_type,
        ast_ref: t_ref.ast_ref,
    }
}
