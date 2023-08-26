use std::collections::HashSet;

use log::debug;

use crate::debug_i;
use crate::parser::ast::{
    ASTEnumDef, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTModule, ASTStatement, ASTStructDef,
    ASTType, ASTTypeDef,
};
use crate::type_check::functions_container::{FunctionsContainer, TypeFilter};
use crate::type_check::type_check_error::TypeCheckError;

#[derive(Clone)]
pub struct EnhancedASTModule {
    pub body: Vec<ASTStatement>,
    functions_by_name: FunctionsContainer,
    pub enums: Vec<ASTEnumDef>,
    pub structs: Vec<ASTStructDef>,
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
    pub fn find_function_by_original_name(&self, name: &str) -> Option<&ASTFunctionDef> {
        self.functions_by_name.find_function_by_original_name(name)
    }

    pub fn find_call(
        &self,
        function_name: &str,
        original_function_name: &str,
        parameter_types_filter: Vec<TypeFilter>,
        return_type_filter: Option<ASTType>,
        index: &ASTIndex,
    ) -> Result<Option<ASTFunctionDef>, TypeCheckError> {
        self.functions_by_name.find_call(
            function_name,
            original_function_name,
            parameter_types_filter,
            return_type_filter,
            false,
            index,
        )
    }

    pub fn find_call_vec(
        &self,
        call: &ASTFunctionCall,
        parameter_types_filter: Vec<TypeFilter>,
        return_type_filter: Option<ASTType>,
    ) -> Result<Vec<ASTFunctionDef>, TypeCheckError> {
        debug_i!("find call vec for module");
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
    ) -> Result<Option<ASTFunctionDef>, TypeCheckError> {
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

    pub fn check_duplicate_functions(&self) {
        self.functions_by_name.check_duplicate_functions();
    }
}
