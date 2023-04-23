use std::collections::HashMap;

use crate::parser::ast::{ASTFunctionDef, ASTIndex, ASTType, BuiltinTypeKind, ValueType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnhancedType {
    Builtin(EnhancedBuiltinTypeKind),
    Generic(String, usize),
    Custom {
        name: String,
        param_types: Vec<EnhancedType>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnhancedBuiltinTypeKind {
    Bool,
    Char,
    I32,
    F32,
    String,
    Lambda {
        parameters: Vec<EnhancedType>,
        return_type: Option<Box<EnhancedType>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnhancedASTParameterDef {
    pub name: String,
    pub ast_type: EnhancedType,
    pub ast_index: ASTIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnhancedFunctionCall {
    pub original_function_name: String,
    pub function_name: String,
    pub parameters: Vec<EnhancedExpression>,
    pub index: ASTIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnhancedLambdaDef {
    pub parameter_names: Vec<(String, ASTIndex)>,
    pub body: Vec<EnhancedStatement>,
}

// TODO can we do partialeq? It depends on ASTIndex
#[derive(Debug, Clone, PartialEq)]
pub enum EnhancedExpression {
    StringLiteral(String),
    FunctionCallExpression(EnhancedFunctionCall),
    ValueRef(String, ASTIndex),
    Value(ValueType, ASTIndex),
    Lambda(EnhancedLambdaDef),
    Any(EnhancedType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnhancedStatement {
    Expression(EnhancedExpression),
    LetStatement(String, EnhancedExpression, bool, ASTIndex),
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnhancedFunctionBody {
    RASMBody(Vec<EnhancedStatement>),
    ASMBody(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnhancedFunctionDef {
    pub original_name: String,
    pub name: String,
    pub parameters: Vec<EnhancedASTParameterDef>,
    pub return_type: Option<EnhancedType>,
    pub body: EnhancedFunctionBody,
    pub inline: bool,
}

pub struct EnhancedFunctionDefTransformer {
    generic_type_id: usize,
}

impl EnhancedFunctionDefTransformer {
    fn transform(&mut self, function_def: &ASTFunctionDef) -> EnhancedFunctionDef {
        let generic_types_map = self.new_generic_types_map(
            &function_def.generic_types,
            &format!(" function def {}", function_def.name),
        );

        let parameters = function_def
            .parameters
            .iter()
            .map(|param| EnhancedASTParameterDef {
                name: param.name.clone(),
                ast_type: self.transform_type(&param.ast_type, &generic_types_map),
                ast_index: param.ast_index.clone(),
            })
            .collect();

        EnhancedFunctionDef {
            name: function_def.name.clone(),
            original_name: function_def.original_name.clone(),
            parameters,
            // TODO
            body: EnhancedFunctionBody::RASMBody(Vec::new()),
            return_type: function_def
                .return_type
                .clone()
                .map(|it| self.transform_type(&it, &generic_types_map)),
            inline: function_def.inline,
        }
    }

    fn transform_type(
        &self,
        ast_type: &ASTType,
        generic_types_map: &HashMap<String, usize>,
    ) -> EnhancedType {
        match ast_type {
            ASTType::Builtin(builtin) => {
                EnhancedType::Builtin(self.transform_builtin(builtin, generic_types_map))
            }
            ASTType::Generic(name) => EnhancedType::Generic(
                name.clone(),
                *generic_types_map
                    .get(name)
                    .expect(&format!("Cannot find generic type {}", name)),
            ),
            ASTType::Custom { name, param_types } => EnhancedType::Custom {
                name: name.clone(),
                param_types: param_types
                    .iter()
                    .map(|it| self.transform_type(it, generic_types_map))
                    .collect(),
            },
        }
    }

    fn transform_builtin(
        &self,
        builtin_type_kind: &BuiltinTypeKind,
        generic_types_map: &HashMap<String, usize>,
    ) -> EnhancedBuiltinTypeKind {
        match builtin_type_kind {
            BuiltinTypeKind::String => EnhancedBuiltinTypeKind::String,
            BuiltinTypeKind::I32 => EnhancedBuiltinTypeKind::F32,
            BuiltinTypeKind::Bool => EnhancedBuiltinTypeKind::Bool,
            BuiltinTypeKind::Char => EnhancedBuiltinTypeKind::Char,
            BuiltinTypeKind::F32 => EnhancedBuiltinTypeKind::F32,
            BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            } => EnhancedBuiltinTypeKind::Lambda {
                parameters: parameters
                    .iter()
                    .map(|it| self.transform_type(it, generic_types_map))
                    .collect(),
                return_type: return_type
                    .clone()
                    .map(|it| Box::new(self.transform_type(&it, generic_types_map))),
            },
        }
    }

    fn new_generic_types_map(
        &mut self,
        names: &Vec<String>,
        source_for_error_msg: &str,
    ) -> HashMap<String, usize> {
        let mut generic_types = HashMap::new();
        names.iter().for_each(|name| {
            self.generic_type_id += 1;
            if generic_types
                .insert(name.clone(), self.generic_type_id)
                .is_some()
            {
                panic!("duplicated generic type {name} in {}", source_for_error_msg);
            }
        });
        generic_types
    }
}
