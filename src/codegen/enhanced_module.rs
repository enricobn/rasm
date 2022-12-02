use std::collections::HashSet;

use crate::codegen::statics::Statics;
use crate::parser::ast::{
    ASTEnumDef, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTStatement, ASTStructDef, ASTType,
    ASTTypeDef,
};
use crate::type_check::functions_container::FunctionsContainer;

#[derive(Clone)]
pub struct EnhancedASTModule {
    pub body: Vec<ASTStatement>,
    /// key: logical name
    functions_by_name: FunctionsContainer,
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
        let mut container = FunctionsContainer::new();

        module.functions.iter().for_each(|it| {
            container.add_function(it.name.clone(), it.clone());
        });

        Self {
            body: module.body.clone(),
            functions_by_name: container,
            enums: module.enums.clone(),
            structs: module.structs.clone(),
            native_body: String::new(),
            statics: Statics::new(),
            requires: module.requires.clone(),
            externals: module.externals.clone(),
            types: module.types.clone(),
        }
    }

    pub fn add_function(&mut self, original_name: String, function_def: ASTFunctionDef) {
        self.functions_by_name
            .add_function(original_name, function_def);
    }

    pub fn find_function(&self, name: &str) -> Option<&ASTFunctionDef> {
        self.functions_by_name.find_function(name)
    }

    pub fn find_call(
        &self,
        call: &ASTFunctionCall,
        parameter_types_filter: Option<Vec<Option<ASTType>>>,
    ) -> Option<&ASTFunctionDef> {
        self.functions_by_name
            .find_call(call, parameter_types_filter)
    }

    pub fn functions(&self) -> Vec<&ASTFunctionDef> {
        self.functions_by_name.functions()
    }
}
