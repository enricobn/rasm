use crate::codegen::statics::Statics;
use crate::parser::ast::{
    ASTEnumDef, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTStatement, ASTStructDef, ASTType,
    ASTTypeDef, BuiltinTypeKind,
};
use linked_hash_map::LinkedHashMap;
use std::collections::HashSet;
use std::iter::zip;

#[derive(Clone)]
pub struct EnhancedASTModule {
    pub body: Vec<ASTStatement>,
    /// key: logical name
    functions_by_name: LinkedHashMap<String, Vec<ASTFunctionDef>>,
    pub enums: Vec<ASTEnumDef>,
    pub structs: Vec<ASTStructDef>,
    pub native_body: String,
    pub statics: Statics,
    pub requires: HashSet<String>,
    pub externals: HashSet<String>,
    pub types: Vec<ASTTypeDef>,
}

impl EnhancedASTModule {
    pub fn new(module: &ASTModule) -> Self {
        let mut functions_by_name: LinkedHashMap<String, Vec<ASTFunctionDef>> =
            LinkedHashMap::new();

        module.functions.iter().for_each(|it| {
            if let Some(functions) = functions_by_name.get_mut(&it.name) {
                functions.push(it.clone());
            } else {
                functions_by_name.insert(it.name.clone(), vec![it.clone()]);
            }
        });

        Self {
            body: module.body.clone(),
            functions_by_name,
            enums: module.enums.clone(),
            structs: module.structs.clone(),
            native_body: String::new(),
            statics: Statics::new(),
            requires: module.requires.clone(),
            externals: module.externals.clone(),
            types: module.types.clone(),
        }
    }

    pub fn add_function(&mut self, key: String, function_def: ASTFunctionDef) {
        if let Some(functions) = self.functions_by_name.get_mut(&key) {
            functions.push(function_def);
        } else {
            self.functions_by_name.insert(key, vec![function_def]);
        }
    }

    pub fn find_function(&self, name: &str) -> Option<&ASTFunctionDef> {
        if let Some(functions) = self.functions_by_name.get(name) {
            if functions.len() != 1 {
                panic!("{} functions with name {name}", functions.len());
            } else {
                functions.first()
            }
        } else {
            None
        }
    }

    pub fn find_call(
        &self,
        call: &ASTFunctionCall,
        parameter_types_filter: Option<Vec<Option<ASTType>>>,
    ) -> Option<&ASTFunctionDef> {
        let name = call.function_name.clone();
        if let Some(functions) = self.functions_by_name.get(&name) {
            if functions.is_empty() {
                panic!("cannot find functions {name}");
            } else if functions.len() != 1 {
                let lambda = |it: &&ASTFunctionDef| {
                    if it.name != name {
                        false
                    } else if let Some(parameter_types) = parameter_types_filter.clone() {
                        Self::almost_same_parameters_types(
                            &it.parameters
                                .iter()
                                .map(|it| it.ast_type.clone())
                                .collect::<Vec<ASTType>>(),
                            &parameter_types,
                        )
                    } else {
                        true
                    }
                };
                let count = functions.iter().filter(lambda).count();
                if count != 1 {
                    panic!(
                        "found more than one function for call {call} filter {:?}: {}",
                        parameter_types_filter, call.index
                    );
                } else {
                    functions.iter().find(lambda)
                }
            } else {
                functions.first()
            }
        } else {
            None
        }
    }

    fn almost_same_parameters_types(
        parameters1: &Vec<ASTType>,
        parameters2: &Vec<Option<ASTType>>,
    ) -> bool {
        let result = if parameters1.len() != parameters2.len() {
            false
        } else {
            zip(parameters1.iter(), parameters2.iter()).all(|(p1, p2)| match p1 {
                ASTType::Builtin(kind) => {
                    match kind {
                        BuiltinTypeKind::Lambda { .. } => true, // TODO
                        _ => p2.iter().map(|p| p == p1).next().unwrap_or(false),
                    }
                }
                ASTType::Parametric(_) => true, // TODO
                ASTType::Custom {
                    name: p1_name,
                    param_types: _,
                } => match p2 {
                    Some(ASTType::Custom {
                        name,
                        param_types: _,
                    }) => name == p1_name,
                    None => true,
                    _ => false,
                },
            })
        };
        result
    }

    pub fn functions(&self) -> Vec<&ASTFunctionDef> {
        self.functions_by_name
            .values()
            .flat_map(|it| it.iter())
            .collect()
    }
}
