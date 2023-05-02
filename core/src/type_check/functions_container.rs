use std::fmt::{Debug, Display, Formatter};
use std::iter::zip;

use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::parser::ast::{ASTFunctionCall, ASTFunctionDef, ASTType, BuiltinTypeKind};
use crate::utils::{format_option_option, SliceDisplay};
use crate::{debug_i, dedent, indent};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionsContainer {
    functions_by_name: LinkedHashMap<String, Vec<ASTFunctionDef>>,
}

// TODO
#[derive(Clone)]
pub enum TypeFilter {
    Exact(ASTType),
    Any,
    Lambda,
}

impl Display for TypeFilter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeFilter::Exact(ast_type) => write!(f, "Exact({ast_type})",),
            TypeFilter::Any => write!(f, "Any"),
            TypeFilter::Lambda => write!(f, "Lambda"),
        }
    }
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
        if !function_def.generic_types.is_empty() {
            panic!("adding generic function {function_def}");
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
                if already_present.name == function_def.name {
                    None
                } else {
                    Some(already_present.clone())
                }
            } else {
                let mut def = function_def.clone();
                def.name = format!("{}_{}", def.name, same_name_functions.len());
                debug_i!("added {def} to function context");
                same_name_functions.push(def.clone());
                Some(def)
            }
        } else {
            let mut def = function_def.clone();
            def.name = format!("{}_{}", def.name, 0);
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

        panic!("clone() {} {:?}", function_def, self.functions_desc())
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
        function_name: &str,
        original_function_name: &str,
        parameter_types_filter: Vec<TypeFilter>,
        return_type_filter: Option<Option<ASTType>>,
        filter_on_name: bool,
    ) -> Option<&ASTFunctionDef> {
        if let Some(functions) = self.functions_by_name.get(original_function_name) {
            if functions.is_empty() {
                panic!(
                    "cannot find functions for {function_name} filter {}",
                    SliceDisplay(&parameter_types_filter)
                );
            } else {
                let mut resolved_generic_types = LinkedHashMap::new();
                let lambda = |it: &&ASTFunctionDef| {
                    if filter_on_name && it.name == function_name {
                        return true;
                    }
                    let verify_params = Self::almost_same_parameters_types(
                        &it.parameters
                            .iter()
                            .map(|it| it.ast_type.clone())
                            .collect::<Vec<ASTType>>(),
                        &parameter_types_filter,
                        &mut resolved_generic_types,
                    );

                    verify_params
                        && match return_type_filter {
                            None => true,
                            Some(None) => it.return_type.is_none(),
                            Some(ref rt) => match rt {
                                None => it.return_type.is_none(),
                                Some(t) => Self::almost_same_type(
                                    &it.return_type.clone().unwrap(),
                                    &TypeFilter::Exact(t.clone()),
                                    &mut resolved_generic_types,
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
                        "Found more than one function for {function_name}\nfilter {}\nfunctions {f_descs}",
                        SliceDisplay(&parameter_types_filter)
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
        parameter_types_filter: Vec<TypeFilter>,
        return_type_filter: Option<Option<ASTType>>,
        filter_on_name: bool,
    ) -> Vec<ASTFunctionDef> {
        debug_i!(
            "find_call_vec {call} return type {} filter {}",
            format_option_option(&return_type_filter),
            SliceDisplay(&parameter_types_filter)
        );
        let result =
            if let Some(functions) = self.functions_by_name.get(&call.original_function_name) {
                if functions.is_empty() {
                    panic!(
                        "cannot find functions for call {call} filter {}",
                        SliceDisplay(&parameter_types_filter)
                    );
                } else {
                    let mut resolved_generic_types = LinkedHashMap::new();
                    let lambda = |it: &&ASTFunctionDef| {
                        if filter_on_name && it.name == call.function_name {
                            return true;
                        }
                        let verify_params = Self::almost_same_parameters_types(
                            &it.parameters
                                .iter()
                                .map(|it| it.ast_type.clone())
                                .collect::<Vec<ASTType>>(),
                            &parameter_types_filter,
                            &mut resolved_generic_types,
                        );

                        verify_params
                            && match return_type_filter {
                                None => true,
                                Some(None) => it.return_type.is_none(),
                                Some(ref rt) => match rt {
                                    None => it.return_type.is_none(),
                                    Some(t) => Self::almost_same_type(
                                        &it.return_type.clone().unwrap(),
                                        &TypeFilter::Exact(t.clone()),
                                        &mut resolved_generic_types,
                                    ),
                                },
                            }
                    };
                    functions.iter().filter(lambda).cloned().collect::<Vec<_>>()
                }
            } else {
                vec![]
            };

        debug_i!("Found functions {}", SliceDisplay(&result));

        result
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
                let mut resolved_generic_types = LinkedHashMap::new();

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
                                .map(|it| TypeFilter::Exact(it.clone()))
                                .collect(),
                            &mut resolved_generic_types,
                        )
                    }
                };

                let result = functions.iter().filter(lambda).collect::<Vec<_>>();

                let count = result.len();
                if count == 0 {
                    None
                } else if count > 1 {
                    panic!(
                        "found more than one function for {name} filter {:?}",
                        parameter_types_filter
                    );
                } else {
                    result.first().cloned()
                }
            }
        } else {
            None
        }
    }

    fn almost_same_parameters_types(
        parameter_types: &Vec<ASTType>,
        parameter_types_filter: &Vec<TypeFilter>,
        resolved_generic_types: &mut LinkedHashMap<String, ASTType>,
    ) -> bool {
        let result = if parameter_types.len() != parameter_types_filter.len() {
            false
        } else {
            zip(parameter_types.iter(), parameter_types_filter.iter()).all(
                |(parameter_type, parameter_type_filter)| {
                    Self::almost_same_type(
                        parameter_type,
                        parameter_type_filter,
                        resolved_generic_types,
                    )
                },
            )
        };
        result
    }

    pub fn almost_same_return_type(
        actual_return_type: &Option<ASTType>,
        expected_return_type: &Option<ASTType>,
        resolved_generic_types: &mut LinkedHashMap<String, ASTType>,
    ) -> bool {
        if let Some(art) = actual_return_type {
            if let Some(ert) = expected_return_type {
                Self::almost_same_type(art, &TypeFilter::Exact(ert.clone()), resolved_generic_types)
            } else {
                false
            }
        } else {
            expected_return_type.is_none()
        }
    }

    pub fn almost_same_type(
        parameter_type: &ASTType,
        parameter_type_filter: &TypeFilter,
        resolved_generic_types: &mut LinkedHashMap<String, ASTType>,
    ) -> bool {
        debug_i!(
            "almost_same_type {parameter_type} filter {}",
            &parameter_type_filter
        );
        match parameter_type_filter {
            TypeFilter::Any => true,
            TypeFilter::Exact(filter_type) => {
                if filter_type == parameter_type {
                    return true;
                }
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
                                        Self::almost_same_type(
                                            a,
                                            &TypeFilter::Exact(filter_type.clone()),
                                            resolved_generic_types,
                                        )
                                    });

                                let return_type_same = match filter_rt {
                                    None => a_rt.is_none(),
                                    Some(filter_rt1) => match a_rt {
                                        None => false,
                                        Some(a_rt1) => Self::almost_same_type(
                                            a_rt1,
                                            &TypeFilter::Exact(*filter_rt1.clone()),
                                            resolved_generic_types,
                                        ),
                                    },
                                };

                                parameter_same && return_type_same
                            }
                            _ => false,
                        },
                        _ => match parameter_type {
                            ASTType::Builtin(_) => filter_type == parameter_type,
                            ASTType::Generic(_) => true,
                            ASTType::Custom { .. } => false,
                        },
                    },
                    ASTType::Generic(filter_generic_type) => {
                        let already_resolved_o = resolved_generic_types.get(filter_generic_type);
                        match parameter_type {
                            ASTType::Generic(_parameter_generic_type) => {
                                // TODO we don't know if the two generic types belong to the same context (Enum, Struct or function),
                                //   to know it we need another attribute in ASTType::Builtin::Parametric : the context
                                true
                            }
                            _ => {
                                if let Some(already_resolved) = already_resolved_o {
                                    already_resolved == parameter_type
                                } else {
                                    resolved_generic_types.insert(
                                        filter_generic_type.clone(),
                                        parameter_type.clone(),
                                    );
                                    true
                                }
                            }
                        }
                    }
                    ASTType::Custom {
                        param_types: expected_param_types,
                        name: expected_type_name,
                        index: _,
                    } => {
                        match parameter_type {
                            ASTType::Builtin(_) => false,
                            ASTType::Generic(_) => true, // TODO
                            ASTType::Custom {
                                param_types,
                                name: type_name,
                                index: _,
                            } => {
                                type_name == expected_type_name
                                    && param_types.len() == expected_param_types.len()
                                    && param_types.iter().enumerate().all(|(i, pt)| {
                                        Self::almost_same_type(
                                            pt,
                                            &TypeFilter::Exact(
                                                expected_param_types.get(i).unwrap().clone(),
                                            ),
                                            resolved_generic_types,
                                        )
                                    })
                            }
                        }
                    }
                }
            }
            TypeFilter::Lambda => {
                matches!(
                    parameter_type,
                    ASTType::Builtin(BuiltinTypeKind::Lambda { .. })
                )
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
        BuiltinTypeKind, ValueType,
    };
    use crate::type_check::functions_container::FunctionsContainer;
    use crate::type_check::functions_container::TypeFilter::Exact;

    #[test]
    fn test() {
        let mut sut = FunctionsContainer::new();

        let function_def = create_function("toString", "n", BuiltinTypeKind::I32);

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());

        let result = sut.try_add_new("toString", &function_def);

        assert!(result.is_some());

        assert_eq!(result.unwrap().name, "toString_0");
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
                    ValueType::I32(10),
                    ASTIndex {
                        file_name: None,
                        row: 0,
                        column: 0,
                    },
                ),
                ASTExpression::Value(
                    ValueType::I32(20),
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
            &call.function_name,
            &call.original_function_name,
            vec![
                Exact(ASTType::Builtin(BuiltinTypeKind::I32)),
                Exact(ASTType::Generic("T".into())),
            ],
            None,
            false,
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
            generic_types: vec![],
            parameters: vec![ASTParameterDef {
                name: param_name.into(),
                ast_type: ASTType::Builtin(param_kind),
                ast_index: ASTIndex::none(),
            }],
            inline: false,
            resolved_generic_types: LinkedHashMap::new(),
            return_type: Some(ASTType::Builtin(BuiltinTypeKind::String)),
            original_name: name.into(),
            index: ASTIndex::none(),
        }
    }

    fn create_add_function(param_name: &str, param_kind: BuiltinTypeKind) -> ASTFunctionDef {
        ASTFunctionDef {
            name: "add".into(),
            body: ASMBody("".into()),
            generic_types: vec![],
            parameters: vec![
                ASTParameterDef {
                    name: param_name.into(),
                    ast_type: ASTType::Builtin(param_kind.clone()),
                    ast_index: ASTIndex::none(),
                },
                ASTParameterDef {
                    name: format!("{}_1", param_name),
                    ast_type: ASTType::Builtin(param_kind.clone()),
                    ast_index: ASTIndex::none(),
                },
            ],
            inline: false,
            resolved_generic_types: LinkedHashMap::new(),
            return_type: Some(ASTType::Builtin(param_kind)),
            original_name: "add".into(),
            index: ASTIndex::none(),
        }
    }
}
