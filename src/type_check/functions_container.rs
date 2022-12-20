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
        original_name: &str,
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
                .insert(original_name.into(), same_name_functions);
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
            panic!(
                "found more functions with name {name}: {:?}",
                found.iter().map(|it| format!("{it}")).collect::<Vec<_>>()
            );
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
                panic!(
                    "cannot find functions for call {call} filter {:?}",
                    parameter_types_filter
                );
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
                if count == 0 {
                    None
                } else if count > 1 {
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

    pub fn find_default_call(
        &self,
        name: String,
        parameter_types_filter: Vec<ASTType>,
    ) -> Option<&ASTFunctionDef> {
        if let Some(functions) = self.functions_by_name.get(&name) {
            if functions.is_empty() {
                panic!(
                    "cannot find function {name} filter {:?}",
                    parameter_types_filter
                );
            } else if functions.len() == 1 {
                functions.first()
            } else {
                let lambda = |it: &&ASTFunctionDef| {
                    if it.name != name {
                        false
                    } else {
                        Self::almost_same_parameters_types(
                            &it.parameters
                                .iter()
                                .map(|it| it.ast_type.clone())
                                .collect::<Vec<ASTType>>(),
                            &parameter_types_filter
                                .iter()
                                .map(|it| Some(it.clone()))
                                .collect(),
                        )
                    }
                };
                let count = functions.iter().filter(lambda).count();
                if count == 0 {
                    None
                } else if count > 1 {
                    panic!(
                        "found more than one function for {name} filter {:?}",
                        parameter_types_filter
                    );
                } else {
                    functions.iter().find(lambda)
                }
            }
        } else {
            None
        }
    }

    fn almost_same_parameters_types_(
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

    fn almost_same_parameters_types(
        parameter_types: &Vec<ASTType>,
        parameter_types_filter: &Vec<Option<ASTType>>,
    ) -> bool {
        let result = if parameter_types.len() != parameter_types_filter.len() {
            false
        } else {
            zip(parameter_types.iter(), parameter_types_filter.iter()).all(
                |(parameter_type, parameter_type_filter)| {
                    match parameter_type_filter {
                        None => true,
                        Some(expected_type) => {
                            match expected_type {
                                ASTType::Builtin(expected_kind) => match expected_kind {
                                    BuiltinTypeKind::Lambda { .. } => matches!(
                                        parameter_type,
                                        ASTType::Builtin(BuiltinTypeKind::Lambda { .. }) // TODO we don't check the lambda
                                    ),
                                    _ => match parameter_type {
                                        ASTType::Builtin(_) => expected_type == parameter_type,
                                        ASTType::Parametric(_) => true,
                                        ASTType::Custom { .. } => false,
                                    },
                                },
                                ASTType::Parametric(_) => true,
                                ASTType::Custom {
                                    param_types: _,
                                    name: expected_type_name,
                                } => {
                                    match parameter_type {
                                        ASTType::Builtin(_) => false,
                                        ASTType::Parametric(_) => true, // TODO
                                        ASTType::Custom {
                                            param_types: _,
                                            name: type_name,
                                        } => type_name == expected_type_name,
                                    }
                                }
                            }
                        }
                    }
                    /*
                    match parameter_type {
                        ASTType::Builtin(kind) => {
                            match kind {
                                BuiltinTypeKind::Lambda { .. } => true, // TODO
                                _ => parameter_type_filter
                                    .iter()
                                    .map(|p| p == parameter_type)
                                    .next()
                                    .unwrap_or(false),
                            }
                        }
                        ASTType::Parametric(_) => true, // TODO
                        ASTType::Custom {
                            name: p1_name,
                            param_types: _,
                        } => match parameter_type_filter {
                            Some(ASTType::Custom {
                                     name,
                                     param_types: _,
                                 }) => name == p1_name,
                            None => true,
                            _ => false,
                        },
                    }

                     */
                },
            )
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

    pub fn functions_desc(&self) -> Vec<String> {
        let mut vec: Vec<String> = self.functions().iter().map(|it| format!("{it}")).collect();
        vec.sort();
        vec
    }
}

#[cfg(test)]
mod tests {
    use linked_hash_map::LinkedHashMap;

    use crate::parser::ast::ASTFunctionBody::ASMBody;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTParameterDef, ASTType,
        BuiltinTypeKind,
    };
    use crate::parser::ValueType;
    use crate::type_check::functions_container::FunctionsContainer;

    #[test]
    fn test() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_function("toString", "n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_none());

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());
    }

    #[test]
    fn test_1() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_function("toString", "n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_none());

        let function_def = create_function("toString", "b", BuiltinTypeKind::Bool);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());
    }

    #[test]
    fn test_2() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_function("AModule::toString", "n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_none());

        let function_def = create_function("AModule::toString", "b", BuiltinTypeKind::Bool);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());

        assert!(sut.find_function("AModule::toString").is_some());
        assert!(sut.find_function("AModule::toString_1").is_some());
    }

    /*
    #[test]
    fn test_3() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_function("AModule::toString", "n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("AModule::toString", &function_def);

        assert!(result.is_none());

        assert!(sut.find_function("AModule::toString").is_some());

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_none());

        assert!(sut.find_function("AModule::toString").is_some());
    }

     */

    #[test]
    fn test_3() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_function("toString", "n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_none());

        assert!(sut.find_function("toString").is_some());

        let mut function_def = create_function("toString", "n", BuiltinTypeKind::I32);
        function_def.parameters = vec![];

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());

        assert!(sut.find_function("toString").is_some());
        assert!(sut.find_function("toString_1").is_some());
    }

    #[test]
    fn test_4() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_add_function("n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("add", &function_def);

        assert!(result.is_none());

        let function_def = create_add_function("s", BuiltinTypeKind::String);

        let result = sut.try_add_new("add", &function_def);

        assert!(result.is_some());

        let call = ASTFunctionCall {
            original_function_name: "add".into(),
            function_name: "add".into(),
            parameters: vec![
                ASTExpression::Value(
                    ValueType::Number(10),
                    ASTIndex {
                        file_name: None,
                        row: 0,
                        column: 0,
                    },
                ),
                ASTExpression::Value(
                    ValueType::Number(20),
                    ASTIndex {
                        file_name: None,
                        row: 0,
                        column: 0,
                    },
                ),
            ],
            index: ASTIndex {
                file_name: None,
                row: 0,
                column: 0,
            },
        };

        let result = sut.find_call(
            &call,
            Some(vec![
                Some(ASTType::Builtin(BuiltinTypeKind::I32)),
                Some(ASTType::Parametric("T".into())),
            ]),
        );

        println!(
            "{:?}",
            sut.functions()
                .iter()
                .map(|it| format!("{it}"))
                .collect::<Vec<_>>()
        );

        assert!(result.is_some());
    }

    fn create_function(
        name: &str,
        param_name: &str,
        param_kind: BuiltinTypeKind,
    ) -> ASTFunctionDef {
        ASTFunctionDef {
            name: name.into(),
            body: ASMBody("".into()),
            param_types: vec![],
            parameters: vec![ASTParameterDef {
                name: param_name.into(),
                ast_type: ASTType::Builtin(param_kind),
            }],
            inline: false,
            resolved_generic_types: LinkedHashMap::new(),
            return_type: Some(ASTType::Builtin(BuiltinTypeKind::String)),
        }
    }

    fn create_add_function(param_name: &str, param_kind: BuiltinTypeKind) -> ASTFunctionDef {
        ASTFunctionDef {
            name: "add".into(),
            body: ASMBody("".into()),
            param_types: vec![],
            parameters: vec![
                ASTParameterDef {
                    name: param_name.into(),
                    ast_type: ASTType::Builtin(param_kind.clone()),
                },
                ASTParameterDef {
                    name: format!("{}_1", param_name),
                    ast_type: ASTType::Builtin(param_kind.clone()),
                },
            ],
            inline: false,
            resolved_generic_types: LinkedHashMap::new(),
            return_type: Some(ASTType::Builtin(param_kind)),
        }
    }
}
