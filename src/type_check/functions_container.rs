use crate::parser::ast::{ASTFunctionCall, ASTFunctionDef, ASTType, BuiltinTypeKind};
use crate::{debug_i, dedent, indent};
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

    pub fn add_function(&mut self, original_name: String, function_def: ASTFunctionDef) {
        // let original_name = function_def.original_name.clone();
        if let Some(functions) = self.functions_by_name.get_mut(&original_name) {
            functions.push(function_def);
        } else {
            self.functions_by_name
                .insert(original_name, vec![function_def]);
        }
    }

    pub fn try_add_new(
        &mut self,
        original_name: &str,
        function_def: &ASTFunctionDef,
    ) -> Option<ASTFunctionDef> {
        if !function_def.param_types.is_empty() {
            panic!("addin eneric function {function_def}");
        }
        // let original_name = &function_def.original_name;
        debug!("trying to add new function {function_def}");
        // println!("trying to add new function {function_def} {original_name}");

        if let Some(same_name_functions) = self.functions_by_name.get_mut(original_name) {
            if let Some(already_present) = same_name_functions.iter().find(|it| {
                it.parameters == function_def.parameters
                    && it.return_type == function_def.return_type
            }) {
                debug!("already added as {already_present}");
                // Some(already_present.clone())
                None
            } else {
                let mut def = function_def.clone();
                def.name = format!("{}_{}", def.name, same_name_functions.len());
                /*                if def.name == "Option_None_37_0" {
                    panic!()
                }*/
                debug_i!("added {def} to function context");
                same_name_functions.push(def.clone());
                Some(def)
            }
        } else {
            let mut def = function_def.clone();
            def.name = format!("{}_{}", def.name, 0);

            /*            if def.name == "Option_None_37_0" {
                panic!()
            }*/

            debug_i!("added {def} to function context");
            let same_name_functions = vec![def.clone()];
            self.functions_by_name
                .insert(original_name.into(), same_name_functions);
            Some(def)
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

        panic!(
            "cannot find function {} {:?}",
            function_def,
            self.functions_desc()
        )
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
        return_type_filter: Option<Option<ASTType>>,
        filter_on_name: bool,
    ) -> Option<&ASTFunctionDef> {
        let name = call.function_name.clone().replace("::", "_");
        if let Some(functions) = self.functions_by_name.get(&call.original_function_name) {
            if functions.is_empty() {
                panic!(
                    "cannot find functions for call {call} filter {:?}",
                    parameter_types_filter
                );
            } else {
                let lambda = |it: &&ASTFunctionDef| {
                    if filter_on_name && it.name == call.function_name {
                        return true;
                    }
                    let verify_params =
                        if let Some(parameter_types) = parameter_types_filter.clone() {
                            Self::almost_same_parameters_types(
                                &it.parameters
                                    .iter()
                                    .map(|it| it.ast_type.clone())
                                    .collect::<Vec<ASTType>>(),
                                &parameter_types,
                            )
                        } else {
                            it.name == name
                        };

                    verify_params
                        && match return_type_filter {
                            None => true,
                            Some(None) => it.return_type.is_none(),
                            Some(ref rt) => match rt {
                                None => it.return_type.is_none(),
                                Some(t) => Self::almost_same_type(
                                    &it.return_type.clone().unwrap(),
                                    &Some(t.clone()),
                                ),
                            },
                        }
                };
                let matching_functions = functions.iter().filter(lambda).collect::<Vec<_>>();

                let count = matching_functions.len();
                if count == 0 {
                    None
                } else if count > 1 {
                    let f_descs = matching_functions
                        .iter()
                        .map(|it| format!("{it}"))
                        .collect::<Vec<String>>()
                        .join(",");
                    panic!(
                        "Found more than one function for call {call}\nfilter {:?}\nfunctions {f_descs}\n: {}",
                        parameter_types_filter, call.index
                    );
                } else {
                    matching_functions.first().cloned()
                }
            }
        } else {
            None
        }
    }

    pub fn find_call_vec(
        &self,
        call: &ASTFunctionCall,
        parameter_types_filter: Option<Vec<Option<ASTType>>>,
        return_type_filter: Option<Option<ASTType>>,
        filter_on_name: bool,
    ) -> Vec<&ASTFunctionDef> {
        let name = call.function_name.clone().replace("::", "_");
        if let Some(functions) = self.functions_by_name.get(&call.original_function_name) {
            if functions.is_empty() {
                panic!(
                    "cannot find functions for call {call} filter {:?}",
                    parameter_types_filter
                );
            } else {
                let lambda = |it: &&ASTFunctionDef| {
                    if filter_on_name && it.name == call.function_name {
                        return true;
                    }
                    let verify_params =
                        if let Some(parameter_types) = parameter_types_filter.clone() {
                            Self::almost_same_parameters_types(
                                &it.parameters
                                    .iter()
                                    .map(|it| it.ast_type.clone())
                                    .collect::<Vec<ASTType>>(),
                                &parameter_types,
                            )
                        } else {
                            it.name == name
                        };

                    verify_params
                        && match return_type_filter {
                            None => true,
                            Some(None) => it.return_type.is_none(),
                            Some(ref rt) => match rt {
                                None => it.return_type.is_none(),
                                Some(t) => Self::almost_same_type(
                                    &it.return_type.clone().unwrap(),
                                    &Some(t.clone()),
                                ),
                            },
                        }
                };
                functions.iter().filter(lambda).collect::<Vec<_>>()
            }
        } else {
            vec![]
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
                    Self::almost_same_type(parameter_type, parameter_type_filter)
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

    fn almost_same_type(parameter_type: &ASTType, parameter_type_filter: &Option<ASTType>) -> bool {
        match parameter_type_filter {
            None => true,
            Some(filter_type) => {
                match filter_type {
                    ASTType::Builtin(filter_kind) => match filter_kind {
                        BuiltinTypeKind::Lambda {
                            parameters: filter_ps,
                            return_type: filter_rt,
                        } => match parameter_type {
                            ASTType::Builtin(BuiltinTypeKind::Lambda {
                                parameters: a_p,
                                return_type: a_rt,
                            }) => {
                                let parameter_same = filter_ps.len() == a_p.len()
                                    && zip(filter_ps, a_p).all(|(filter_type, a)| {
                                        Self::almost_same_type(a, &Some(filter_type.clone()))
                                    });

                                let return_type_same = match filter_rt {
                                    None => a_rt.is_none(),
                                    Some(filter_rt1) => match a_rt {
                                        None => false,
                                        Some(a_rt1) => Self::almost_same_type(
                                            a_rt1,
                                            &Some(*filter_rt1.clone()),
                                        ),
                                    },
                                };

                                parameter_same && return_type_same
                            }
                            _ => false,
                        },
                        _ => match parameter_type {
                            ASTType::Builtin(_) => filter_type == parameter_type,
                            ASTType::Parametric(_) => true,
                            ASTType::Custom { .. } => false,
                        },
                    },
                    ASTType::Parametric(_) => true,
                    ASTType::Custom {
                        param_types: expected_param_types,
                        name: expected_type_name,
                    } => {
                        match parameter_type {
                            ASTType::Builtin(_) => false,
                            ASTType::Parametric(_) => true, // TODO
                            ASTType::Custom {
                                param_types,
                                name: type_name,
                            } => {
                                type_name == expected_type_name
                                    && param_types.len() == expected_param_types.len()
                                    && param_types.iter().enumerate().all(|(i, pt)| {
                                        Self::almost_same_type(
                                            pt,
                                            &expected_param_types.get(i).cloned(),
                                        )
                                    })
                            }
                        }
                    }
                }
            }
        }
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
        let mut vec: Vec<String> = self
            .functions_by_name
            .iter()
            .flat_map(|(k, v)| v.iter().map(|it| format!("{}/{it}", k.clone())))
            .collect();
        vec.sort();
        vec
    }

    pub fn debug_i(&self, descr: &str) {
        debug_i!("{descr}:");
        indent!();
        for x in self.functions_desc() {
            debug_i!("{x}");
        }
        dedent!();
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

        assert!(result.is_some());

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_none());
    }

    #[test]
    fn test_1() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_function("toString", "n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());

        let function_def = create_function("toString", "b", BuiltinTypeKind::Bool);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());
    }

    #[test]
    fn test_2() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_function("AModule::toString", "n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());

        let function_def = create_function("AModule::toString", "b", BuiltinTypeKind::Bool);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());

        assert!(sut.find_function("AModule::toString_0").is_some());
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

        assert!(result.is_some());

        assert!(sut.find_function("toString_0").is_some());

        let mut function_def = create_function("toString", "n", BuiltinTypeKind::I32);
        function_def.parameters = vec![];

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());

        assert!(sut.find_function("toString_0").is_some());
        assert!(sut.find_function("toString_1").is_some());
    }

    #[test]
    fn test_4() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_add_function("n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("add", &function_def);

        assert!(result.is_some());

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
            None,
            false,
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
            original_name: name.into(),
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
            original_name: "add".into(),
        }
    }
}
