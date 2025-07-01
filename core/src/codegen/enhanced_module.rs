use std::iter::zip;

use rasm_utils::debug_i;

use crate::codegen::compile_target::CompileTarget;
use crate::codegen::statics::Statics;
use crate::errors::{self, CompilationError};

use crate::codegen::enh_ast::{
    CustomTypeDef, EnhASTEnumDef, EnhASTFunctionBody, EnhASTFunctionCall, EnhASTFunctionDef,
    EnhASTIndex, EnhASTModule, EnhASTNameSpace, EnhASTStatement, EnhASTStructDef, EnhASTType,
    EnhASTTypeDef,
};
use crate::enh_type_check::enh_functions_container::{EnhFunctionsContainer, EnhTypeFilter};
use crate::enh_type_check::enh_type_check_error::EnhTypeCheckError;
use crate::project::RasmProject;
use rasm_parser::parser::ast;

use super::enh_ast::EnhModuleInfo;

#[derive(Clone, Debug)]
pub struct EnhancedASTModule {
    pub body: Vec<EnhASTStatement>,
    pub functions_by_name: EnhFunctionsContainer,
    pub enums: Vec<EnhASTEnumDef>,
    pub structs: Vec<EnhASTStructDef>,
    pub types: Vec<EnhASTTypeDef>,
    pub body_namespace: EnhASTNameSpace,
}

impl EnhancedASTModule {
    pub fn from_ast(
        modules: Vec<(ast::ASTModule, EnhModuleInfo)>,
        project: &RasmProject,
        statics: &mut Statics,
        target: &CompileTarget,
        debug: bool,
        prefix_generics: bool,
    ) -> (Self, Vec<CompilationError>) {
        let result = Self::new(
            modules
                .into_iter()
                .map(|(module, info)| EnhASTModule::from_ast(module, info, prefix_generics))
                .collect(),
            project,
            statics,
            target,
            debug,
        );

        // result.0.print();

        result
    }

    pub fn new(
        modules: Vec<EnhASTModule>,
        project: &RasmProject,
        statics: &mut Statics,
        target: &CompileTarget,
        debug: bool,
    ) -> (Self, Vec<CompilationError>) {
        let mut body = Vec::new();
        let mut enums = Vec::new();
        let mut structs = Vec::new();
        let mut types = Vec::new();

        let mut container = EnhFunctionsContainer::new();

        for module in modules {
            module.functions.into_iter().for_each(|mut it| {
                it.update_calculated_properties();
                container.add_function(it.original_name.clone(), it);
            });
            body.extend(module.body);
            enums.extend(module.enums);
            structs.extend(module.structs);
            types.extend(module.types);
        }

        let body_namespace = EnhASTNameSpace::root_namespace(project);

        let mut enhanced_module = Self {
            body,
            functions_by_name: container,
            enums,
            structs,
            types,
            body_namespace,
        };

        target
            .functions_creator(debug)
            .create_globals(&mut enhanced_module, statics);

        enhanced_module = enhanced_module.fix_namespaces();

        let errors = enhanced_module.check_for_duplicates();

        (enhanced_module, errors)
    }

    pub fn empty() -> Self {
        Self {
            body: Vec::new(),
            functions_by_name: EnhFunctionsContainer::new(),
            enums: Vec::new(),
            structs: Vec::new(),
            types: Vec::new(),
            body_namespace: EnhASTNameSpace::global(),
        }
    }

    pub fn add_function(&mut self, original_name: String, function_def: EnhASTFunctionDef) {
        self.functions_by_name
            .add_function(original_name, function_def);
    }

    pub fn find_function(&self, name: &str) -> Option<&EnhASTFunctionDef> {
        self.functions_by_name.find_function(name)
    }
    pub fn find_function_by_original_name(&self, name: &str) -> Option<&EnhASTFunctionDef> {
        self.functions_by_name.find_function_by_original_name(name)
    }

    pub fn find_functions_by_original_name(&self, name: &str) -> &[EnhASTFunctionDef] {
        self.functions_by_name.find_functions_by_original_name(name)
    }

    pub fn find_precise_function(
        &self,
        original_name: &str,
        name: &str,
    ) -> Option<&EnhASTFunctionDef> {
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

    pub fn find_precise_or_original_function(
        &self,
        original_name: &str,
        name: &str,
    ) -> Option<&EnhASTFunctionDef> {
        let found = self
            .functions_by_name
            .find_functions_by_original_name(original_name)
            .iter()
            .filter(|it| it.name == name || it.name == original_name)
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
        parameter_types_filter: &Vec<EnhTypeFilter>,
        return_type_filter: Option<&EnhASTType>,
        index: &EnhASTIndex,
    ) -> Result<Option<&EnhASTFunctionDef>, EnhTypeCheckError> {
        self.functions_by_name.find_call(
            function_name,
            original_function_name,
            parameter_types_filter,
            return_type_filter,
            false,
            index,
            self,
        )
    }

    pub fn find_call_vec(
        &self,
        call: &EnhASTFunctionCall,
        parameter_types_filter: &Vec<EnhTypeFilter>,
        return_type_filter: Option<&EnhASTType>,
    ) -> Result<Vec<&EnhASTFunctionDef>, EnhTypeCheckError> {
        debug_i!("find call vec for module");
        self.functions_by_name.find_call_vec(
            call,
            parameter_types_filter,
            return_type_filter,
            false,
            self,
        )
    }

    pub fn find_default_call(
        &self,
        name: String,
        parameter_types_filter: Vec<EnhASTType>,
    ) -> Result<Option<EnhASTFunctionDef>, EnhTypeCheckError> {
        self.functions_by_name
            .find_default_call(name, parameter_types_filter, self)
    }

    pub fn functions(&self) -> Vec<&EnhASTFunctionDef> {
        self.functions_by_name.functions()
    }

    pub fn functions_mut(&mut self) -> Vec<&mut EnhASTFunctionDef> {
        self.functions_by_name.functions_mut()
    }

    pub fn funcion_desc(&self) -> Vec<String> {
        self.functions_by_name.functions_desc()
    }

    pub fn functions_owned(self) -> Vec<EnhASTFunctionDef> {
        self.functions_by_name.functions_owned()
    }

    pub fn debug_i(&self) {
        self.functions_by_name.debug_i("module");
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
                EnhASTFunctionBody::RASMBody(b) => {
                    for s in b.iter() {
                        println!("  {s}");
                    }
                }
                EnhASTFunctionBody::NativeBody(_) => {
                    println!();
                }
            }
        }
    }

    pub fn get_type_def(&self, ast_type: &EnhASTType) -> Option<&dyn CustomTypeDef> {
        //println!("get_type_def {ast_type}");
        if let EnhASTType::Custom {
            namespace,
            name,
            param_types,
            index: _,
        } = ast_type
        {
            if !param_types.iter().all(|param_type| {
                if let EnhASTType::Custom { .. } = param_type {
                    if let Some(p_type_def) = self.get_type_def(param_type) {
                        p_type_def.modifiers().public
                            || p_type_def.namespace() == param_type.namespace()
                    } else {
                        // TODO we have not found the type definition, should we return an error?
                        false
                    }
                } else {
                    true
                }
            }) {
                return None;
            }

            if let Some(enum_def) = self
                .enums
                .iter()
                .find(|it| &it.name == name && (it.modifiers.public || &it.namespace == namespace))
            {
                Some(enum_def)
            } else if let Some(struct_def) = self
                .structs
                .iter()
                .find(|it| &it.name == name && (it.modifiers.public || &it.namespace == namespace))
            {
                Some(struct_def)
            } else if let Some(t) = self
                .types
                .iter()
                .find(|it| &it.name == name && (it.modifiers.public || &it.namespace == namespace))
            {
                Some(t)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn check_for_duplicates(&self) -> Vec<CompilationError> {
        let mut errors = Vec::new();

        for it in self.functions() {
            let similar_functions = self
                .find_functions_by_original_name(&it.original_name)
                .iter()
                .filter(|function| {
                    it.index != function.index
                        && it.parameters.len() == function.parameters.len()
                        && zip(it.parameters.iter(), function.parameters.iter()).all(|(p1, p2)| {
                            EnhTypeFilter::Exact(p1.ast_type.clone())
                                .almost_equal(&p2.ast_type, self)
                                .unwrap_or(false)
                        })
                        && EnhTypeFilter::Exact(it.return_type.clone())
                            .almost_equal(&function.return_type, self)
                            .unwrap_or(false)
                        && it.rank == function.rank
                        && (it.modifiers.public
                            || function.modifiers.public
                            || (it.namespace == function.namespace))
                })
                .collect::<Vec<_>>();

            if !similar_functions.is_empty() {
                let message = format!(
                "function {it} has the same signature of other functions : {}\nsimilar functions:\n{}",
                it.index,
                similar_functions
                    .iter()
                    .map(|it| format!("{it} : {}", it.index))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
                errors.push(CompilationError {
                    index: it.index.clone(),
                    error_kind: errors::CompilationErrorKind::Generic(message),
                })
            }
        }
        errors
    }

    fn fix_namespaces(self) -> Self {
        let mut result = self;
        result.enums = result
            .enums
            .iter()
            .map(|e| e.clone().fix_namespaces(&result))
            .collect();
        result.structs = result
            .structs
            .iter()
            .map(|e| e.clone().fix_namespaces(&result))
            .collect();
        result.body = result
            .body
            .iter()
            .map(|it| it.clone().fix_namespaces(&result))
            .collect();
        result.functions_by_name = result.functions_by_name.fix_namespaces(&result);

        result
    }

    pub fn fix_generics(self) -> Self {
        let mut result = self;
        result.enums = result
            .enums
            .iter()
            .map(|e| e.clone().fix_generics())
            .collect();
        result.structs = result
            .structs
            .iter()
            .map(|e| e.clone().fix_generics())
            .collect();
        result.types = result
            .types
            .iter()
            .map(|e| e.clone().fix_generics())
            .collect();

        result.functions_by_name = result.functions_by_name.fix_generics();

        result
    }
}
