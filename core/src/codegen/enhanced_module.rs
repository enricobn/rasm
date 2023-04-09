use std::collections::HashSet;

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
    pub requires: HashSet<String>,
    pub externals: HashSet<String>,
    pub types: Vec<ASTTypeDef>,
}

impl EnhancedASTModule {
    pub fn new(module: ASTModule) -> Self {
        let mut container = FunctionsContainer::new();

        module.functions.into_iter().for_each(|it| {
            container.add_function(it.original_name.clone(), it);
        });

        Self {
            body: module.body,
            functions_by_name: container,
            enums: module.enums,
            structs: module.structs,
            native_body: String::new(),
            requires: module.requires,
            externals: module.externals,
            types: module.types,
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
        function_name: &str,
        original_function_name: &str,
        parameter_types_filter: Option<Vec<Option<ASTType>>>,
        return_type_filter: Option<Option<ASTType>>,
    ) -> Option<&ASTFunctionDef> {
        self.functions_by_name.find_call(
            function_name,
            original_function_name,
            parameter_types_filter,
            return_type_filter,
            false,
        )
    }

    pub fn find_call_vec(
        &self,
        call: &ASTFunctionCall,
        parameter_types_filter: Option<Vec<Option<ASTType>>>,
        return_type_filter: Option<Option<ASTType>>,
    ) -> Vec<ASTFunctionDef> {
        self.functions_by_name.find_call_vec(
            call,
            parameter_types_filter,
            return_type_filter,
            false,
        )
    }

    pub fn find_default_call(
        &self,
        name: String,
        parameter_types_filter: Vec<ASTType>,
    ) -> Option<&ASTFunctionDef> {
        self.functions_by_name
            .find_default_call(name, parameter_types_filter)
    }

    pub fn functions(&self) -> Vec<&ASTFunctionDef> {
        self.functions_by_name.functions()
    }

    pub fn funcion_desc(&self) -> Vec<String> {
        self.functions_by_name.functions_desc()
    }

    pub fn debug_i(&self) {
        self.functions_by_name.debug_i("module");
    }
}
