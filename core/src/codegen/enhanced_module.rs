use std::collections::HashSet;

use log::debug;

use crate::debug_i;
use crate::parser::ast::{
    ASTEnumDef, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTModule,
    ASTStatement, ASTStructDef, ASTType, ASTTypeDef,
};
use crate::type_check::functions_container::{FunctionsContainer, TypeFilter};
use crate::type_check::type_check_error::TypeCheckError;

#[derive(Clone, Debug)]
pub struct EnhancedASTModule {
    pub body: Vec<ASTStatement>,
    pub functions_by_name: FunctionsContainer,
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

    pub fn find_functions_by_original_name(&self, name: &str) -> &[ASTFunctionDef] {
        self.functions_by_name.find_functions_by_original_name(name)
    }

    pub fn find_precise_function(
        &self,
        original_name: &str,
        name: &str,
    ) -> Option<&ASTFunctionDef> {
        let found = self
            .functions_by_name
            .find_functions_by_original_name(original_name)
            .iter()
            .filter(|it| it.name == name)
            .collect::<Vec<_>>();

        if found.len() == 1 {
            found.first().cloned()
        } else {
            None
        }
    }

    pub fn find_call(
        &self,
        function_name: &str,
        original_function_name: &str,
        parameter_types_filter: Vec<TypeFilter>,
        return_type_filter: Option<ASTType>,
        index: &ASTIndex,
    ) -> Result<Option<&ASTFunctionDef>, TypeCheckError> {
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
        parameter_types_filter: &Vec<TypeFilter>,
        return_type_filter: Option<ASTType>,
    ) -> Result<Vec<&ASTFunctionDef>, TypeCheckError> {
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

    pub fn functions_mut(&mut self) -> Vec<&mut ASTFunctionDef> {
        self.functions_by_name.functions_mut()
    }

    pub fn funcion_desc(&self) -> Vec<String> {
        self.functions_by_name.functions_desc()
    }

    pub fn functions_owned(self) -> Vec<ASTFunctionDef> {
        self.functions_by_name.functions_owned()
    }

    pub fn debug_i(&self) {
        self.functions_by_name.debug_i("module");
    }

    pub fn check_duplicate_functions(&self) {
        self.functions_by_name.check_duplicate_functions();
    }

    pub fn print(&self) {
        for s in self.structs.iter() {
            println!("{s}");
        }

        for e in self.enums.iter() {
            println!("{e}");
        }

        for t in self.types.iter() {
            println!("{t}");
        }

        for statement in self.body.iter() {
            println!("{statement}");
        }

        for function_def in self.functions_by_name.functions() {
            println!("{function_def}");
            match &function_def.body {
                ASTFunctionBody::RASMBody(b) => {
                    for s in b.iter() {
                        println!("  {s}");
                    }
                }
                ASTFunctionBody::ASMBody(_) => {
                    println!();
                }
            }
        }
    }
}
