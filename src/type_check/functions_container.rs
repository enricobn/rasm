use crate::parser::ast::{ASTFunctionCall, ASTFunctionDef, ASTType, BuiltinTypeKind};
use linked_hash_map::LinkedHashMap;
use log::debug;
use std::iter::zip;

#[derive(Clone, Debug)]
pub struct FunctionsContainer {
    functions_by_name: LinkedHashMap<String, Vec<ASTFunctionDef>>,
}

impl FunctionsContainer {
    pub fn new() -> Self {
        Self {
            functions_by_name: LinkedHashMap::new(),
        }
    }

    pub fn add_function(&mut self, otiginal_name: String, function_def: ASTFunctionDef) {
        if let Some(functions) = self.functions_by_name.get_mut(&otiginal_name) {
            functions.push(function_def);
        } else {
            self.functions_by_name
                .insert(otiginal_name, vec![function_def]);
        }
    }

    pub fn try_add_new(
        &mut self,
        original_name: &String,
        function_def: &ASTFunctionDef,
    ) -> Option<ASTFunctionDef> {
        debug!("trying to add new function {function_def}");

        if let Some(same_name_functions) = self.functions_by_name.get_mut(original_name) {
            if let Some(already_present) = same_name_functions.iter().find(|it| {
                it.parameters == function_def.parameters
                    && it.return_type == function_def.return_type
            }) {
                debug!("already added as {already_present}");
                Some(already_present.clone())
            } else if same_name_functions
                .iter()
                .any(|it| it.name == function_def.name)
            {
                let mut def = function_def.clone();
                def.name = format!("{}_{}", def.name, same_name_functions.len());
                same_name_functions.push(def.clone());
                Some(def)
            } else {
                same_name_functions.push(function_def.clone());
                None
            }
        } else {
            let same_name_functions = vec![function_def.clone()];
            self.functions_by_name
                .insert(original_name.clone(), same_name_functions);
            None
        }
    }

    pub fn replace_body(&mut self, function_def: &ASTFunctionDef) {
        for (_, f_defs) in self.functions_by_name.iter_mut() {
            for mut f_def in f_defs.iter_mut() {
                if f_def.name == function_def.name
                    && f_def.parameters == function_def.parameters
                    && f_def.return_type == function_def.return_type
                {
                    f_def.body = function_def.body.clone();
                    return;
                }
            }
        }

        panic!("cannot find function {}", function_def.name)
    }

    pub fn find_function(&self, name: &str) -> Option<&ASTFunctionDef> {
        let found = self
            .functions()
            .iter()
            .filter(|it| it.name == name || it.name == name.replace("::", "_"))
            .copied()
            .collect::<Vec<_>>();

        if found.len() > 1 {
            panic!("found more functions with name {name}");
        } else {
            found.first().cloned()
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
            } else if functions.len() == 1 {
                functions.first()
            } else {
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

    pub fn len(&self) -> usize {
        self.functions_by_name.iter().map(|it| it.1.len()).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.functions_by_name.is_empty()
    }
}
