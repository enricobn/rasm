use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use log::debug;
use std::collections::HashSet;

use crate::debug_i;
use crate::parser::ast::{
    ASTEnumDef, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTModule,
    ASTNameSpace, ASTStatement, ASTStructDef, ASTType, ASTTypeDef, CustomTypeDef,
};
use crate::project::RasmProject;
use crate::transformations::globals_creator::add_folder;
use crate::transformations::str_functions_creator::str_functions_creator;
use crate::type_check::functions_container::{FunctionsContainer, TypeFilter};
use crate::type_check::type_check_error::TypeCheckError;

#[derive(Clone, Debug)]
pub struct EnhancedASTModule {
    pub body: Vec<ASTStatement>,
    pub functions_by_name: FunctionsContainer,
    pub enums: Vec<ASTEnumDef>,
    pub structs: Vec<ASTStructDef>,
    pub types: Vec<ASTTypeDef>,
    pub body_namespace: ASTNameSpace,
    pub externals: HashSet<String>,
}

impl EnhancedASTModule {
    pub fn new(
        modules: Vec<ASTModule>,
        project: &RasmProject,
        backend: &dyn Backend,
        statics: &mut Statics,
    ) -> Self {
        let mut body = Vec::new();
        let mut enums = Vec::new();
        let mut structs = Vec::new();
        let mut types = Vec::new();
        let mut externals = HashSet::new();

        let mut container = FunctionsContainer::new();

        for module in modules {
            module.functions.into_iter().for_each(|mut it| {
                /*
                   let similar_functions = container
                       .find_functions_by_original_name(&it.original_name)
                       .iter()
                       .filter(|function| {
                           it.parameters.len() == function.parameters.len()
                               && zip(it.parameters.iter(), function.parameters.iter()).all(|(p1, p2)| {
                               TypeFilter::Exact(p1.ast_type.clone())
                                   .almost_equal(&p2.ast_type)
                                   .unwrap()
                           })
                       })
                       .collect::<Vec<_>>();
                   if !similar_functions.is_empty() {
                       let coeff : usize = TypeCheck::function_generic_coeff(&it);
                       if similar_functions.iter().filter(|function| TypeCheck::function_generic_coeff(function) == coeff).count() > 0 {
                           panic!(
                               "function {it} has the same signature of other generic functions : {}\nsimilar functions:\n{}",
                               it.index,
                               similar_functions
                                   .iter()
                                   .map(|it| format!("{it} : {}", it.index))
                                   .collect::<Vec<_>>()
                                   .join("\n")
                           );
                       }
                   }

                */
                it.update_calculated_properties();
                container.add_function(it.original_name.clone(), it);
            });
            body.extend(module.body);
            enums.extend(module.enums);
            structs.extend(module.structs);
            types.extend(module.types);
            externals.extend(module.externals);
        }

        let mut module = Self {
            body,
            functions_by_name: container,
            enums,
            structs,
            types,
            body_namespace: ASTNameSpace::root_namespace(project),
            externals,
        };

        add_folder(
            &mut module,
            "RASMRESOURCEFOLDER",
            project.main_resource_folder(),
        );
        add_folder(
            &mut module,
            "RASMTESTRESOURCEFOLDER",
            project.test_resource_folder(),
        );
        str_functions_creator(&mut module, backend, statics);

        module
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
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<Option<&ASTFunctionDef>, TypeCheckError> {
        self.functions_by_name.find_call(
            function_name,
            original_function_name,
            parameter_types_filter,
            return_type_filter,
            false,
            index,
            enhanced_astmodule,
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
            self,
        )
    }

    pub fn find_default_call(
        &self,
        name: String,
        parameter_types_filter: Vec<ASTType>,
    ) -> Result<Option<ASTFunctionDef>, TypeCheckError> {
        self.functions_by_name
            .find_default_call(name, parameter_types_filter, self)
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
                ASTFunctionBody::NativeBody(_) => {
                    println!();
                }
            }
        }
    }

    pub fn get_type_def(&self, ast_type: &ASTType) -> Option<&dyn CustomTypeDef> {
        //println!("get_type_def {ast_type}");
        if let ASTType::Custom {
            namespace,
            name,
            param_types,
            index: _,
        } = ast_type
        {
            if !param_types.iter().all(|param_type| {
                if let ASTType::Custom { .. } = param_type {
                    if let Some(p_type_def) = self.get_type_def(param_type) {
                        p_type_def.modifiers().public
                            || p_type_def.namespace() == &param_type.namespace()
                    } else {
                        panic!()
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
}
