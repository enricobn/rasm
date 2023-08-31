use std::fmt::{Debug, Display, Formatter};
use std::iter::zip;
use std::ops::Deref;

use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::parser::ast::{
    ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTType, BuiltinTypeKind,
};
use crate::type_check::type_check_error::TypeCheckError;
use crate::utils::{OptionDisplay, SliceDisplay};
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
    Lambda(usize),
    NotALambda,
}

impl TypeFilter {
    pub fn almost_equal(&self, ast_type: &ASTType) -> bool {
        match self {
            TypeFilter::Exact(f_type) => f_type == ast_type,
            TypeFilter::Any => true,
            TypeFilter::Lambda(n) => match ast_type {
                ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                }) => &parameters.len() == n,
                _ => false,
            },
            TypeFilter::NotALambda => {
                !matches!(ast_type, ASTType::Builtin(BuiltinTypeKind::Lambda { .. }))
            }
        }
    }
}

impl Display for TypeFilter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeFilter::Exact(ast_type) => write!(f, "Exact({ast_type})",),
            TypeFilter::Any => write!(f, "Any"),
            TypeFilter::Lambda(size) => write!(f, "Lambda({size})"),
            TypeFilter::NotALambda => write!(f, "Not a lambda"),
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

        for par in function_def.parameters.iter() {
            if matches!(par.ast_type, ASTType::Unit) {
                panic!("Parameters cannot have unit type.");
            }
        }

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

    pub fn replace_body(
        &mut self,
        function_def: &ASTFunctionDef,
        body: ASTFunctionBody,
    ) -> ASTFunctionDef {
        let functions = self
            .functions_by_name
            .get_mut(&function_def.original_name)
            .unwrap_or_else(|| panic!("Cannot find {}", function_def.name));
        for mut f_def in functions.iter_mut() {
            if f_def.name == function_def.name {
                f_def.body = body;
                return f_def.clone();
            }
        }
        panic!("Cannot find {}", function_def.name)
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

    pub fn has_function(&self, original_name: &str, name: &str) -> bool {
        self.functions_by_name
            .get(original_name)
            .map(|it| it.iter().any(|function_def| &function_def.name == name))
            .unwrap_or(false)
    }

    pub fn find_function_by_original_name(&self, name: &str) -> Option<&ASTFunctionDef> {
        let found = self.functions_by_name.get(name);

        if let Some(f) = found {
            if f.len() > 1 {
                None
            } else {
                f.first()
            }
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
        filter_on_name: bool,
        index: &ASTIndex,
    ) -> Result<Option<ASTFunctionDef>, TypeCheckError> {
        if let Some(functions) = self.functions_by_name.get(original_function_name) {
            if functions.is_empty() {
                panic!(
                    "cannot find functions for {function_name} filter {}",
                    SliceDisplay(&parameter_types_filter)
                );
            } else {
                let matching_functions = Self::find_call_vec_1(
                    function_name,
                    &parameter_types_filter,
                    return_type_filter,
                    filter_on_name,
                    index,
                    functions,
                )?;

                let count = matching_functions.len();
                if count == 0 {
                    Ok(None)
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
                    Ok(matching_functions.first().cloned())
                }
            }
        } else {
            Ok(None)
        }
    }

    fn find_call_vec_1(
        function_name: &str,
        parameter_types_filter: &Vec<TypeFilter>,
        return_type_filter: Option<ASTType>,
        filter_on_name: bool,
        index: &ASTIndex,
        functions: &Vec<ASTFunctionDef>,
    ) -> Result<Vec<ASTFunctionDef>, TypeCheckError> {
        let mut resolved_generic_types = LinkedHashMap::new();
        let lambda = |it: &ASTFunctionDef| {
            debug_i!("testing function {it}");
            indent!();
            if filter_on_name && it.name == function_name {
                dedent!();
                return Ok((it.clone(), true));
            }
            let result = Self::almost_same_parameters_types(
                &it.parameters
                    .iter()
                    .map(|it| it.ast_type.clone())
                    .collect::<Vec<ASTType>>(),
                parameter_types_filter,
                &mut resolved_generic_types,
                &it.index,
            )
            .and_then(|verify_params| {
                if !verify_params {
                    Ok((it.clone(), false))
                } else {
                    match return_type_filter {
                        None => Ok(true),
                        Some(ref rt) => {
                            if matches!(rt, ASTType::Generic(_)) {
                                Ok(true)
                            } else {
                                Self::almost_same_return_type(
                                    &it.return_type,
                                    rt,
                                    &mut resolved_generic_types,
                                    index,
                                )
                            }
                        }
                    }
                    .map(|b| (it.clone(), b))
                }
            });
            dedent!();
            result
        };
        let matching_functions = functions
            .iter()
            .map(lambda)
            .collect::<Result<Vec<_>, TypeCheckError>>()?
            .into_iter()
            .filter(|(_, v)| *v)
            .map(|it| it.0.clone())
            .collect::<Vec<_>>();
        Ok(matching_functions)
    }

    pub fn find_call_vec(
        &self,
        call: &ASTFunctionCall,
        parameter_types_filter: &Vec<TypeFilter>,
        return_type_filter: Option<ASTType>,
        filter_only_on_name: bool,
    ) -> Result<Vec<ASTFunctionDef>, TypeCheckError> {
        debug_i!(
            "find_call_vec {call} return type {} filter {}",
            OptionDisplay(&return_type_filter),
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
                    Self::find_call_vec_1(
                        &call.function_name,
                        &parameter_types_filter,
                        return_type_filter,
                        filter_only_on_name,
                        &ASTIndex::none(),
                        functions,
                    )
                    /*
                    let mut resolved_generic_types = LinkedHashMap::new();
                    let lambda = |it: &&ASTFunctionDef| {
                        debug_i!("verifying function {it}");
                        //println!("verifying function {it} in {call}");
                        if filter_only_on_name && it.name == call.function_name {
                            return true;
                        }
                        let verify_params = Self::almost_same_parameters_types(
                            &it.parameters
                                .iter()
                                .map(|it| it.ast_type.clone())
                                .collect::<Vec<ASTType>>(),
                            &parameter_types_filter,
                            &mut resolved_generic_types,
                            &call.index,
                        );

                        verify_params
                            && match return_type_filter {
                                None => true,
                                Some(ref rt) => {
                                    if matches!(rt, ASTType::Generic(_)) {
                                        true
                                    } else {
                                        Self::almost_same_return_type(
                                            &it.return_type,
                                            &rt.clone(),
                                            &mut resolved_generic_types,
                                            &call.index,
                                        )
                                    }
                                }
                            }
                    };
                    functions.iter().filter(lambda).cloned().collect::<Vec<_>>()

                     */
                }
            } else {
                debug_i!(
                    "cannot find a function with name {}",
                    call.original_function_name
                );
                Ok(vec![])
            };

        if let Ok(r) = &result {
            debug_i!("Found functions: {}", SliceDisplay(r));
        }

        result
    }

    pub fn find_default_call(
        &self,
        name: String,
        parameter_types_filter: Vec<ASTType>,
    ) -> Result<Option<ASTFunctionDef>, TypeCheckError> {
        if let Some(functions) = self.functions_by_name.get(&name) {
            if functions.is_empty() {
                panic!(
                    "cannot find function {name} filter {:?}",
                    parameter_types_filter
                );
            } else {
                let mut resolved_generic_types = LinkedHashMap::new();

                let lambda = |it: &ASTFunctionDef| {
                    if it.name != name {
                        Ok((it.clone(), false))
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
                            &it.index,
                        )
                        .map(|b| (it.clone(), b))
                    }
                };

                let result = functions
                    .iter()
                    .map(lambda)
                    .collect::<Result<Vec<_>, TypeCheckError>>()?
                    .into_iter()
                    .filter(|(f, b)| *b)
                    .map(|it| it.0)
                    .collect::<Vec<_>>();

                let count = result.len();
                if count == 0 {
                    Ok(None)
                } else if count > 1 {
                    panic!(
                        "found more than one function for {name} filter {:?}",
                        parameter_types_filter
                    );
                } else {
                    Ok(result.first().cloned())
                }
            }
        } else {
            Ok(None)
        }
    }

    fn almost_same_parameters_types(
        parameter_types: &Vec<ASTType>,
        parameter_types_filter: &Vec<TypeFilter>,
        resolved_generic_types: &mut LinkedHashMap<String, ASTType>,
        index: &ASTIndex,
    ) -> Result<bool, TypeCheckError> {
        let result = if parameter_types.len() != parameter_types_filter.len() {
            debug_i!("not matching parameter length");
            false
        } else {
            Self::match_parameters(
                parameter_types,
                parameter_types_filter,
                resolved_generic_types,
                index,
            )?
        };
        Ok(result)
    }

    fn match_parameters(
        parameter_types: &[ASTType],
        parameter_types_filter: &[TypeFilter],
        resolved_generic_types: &mut LinkedHashMap<String, ASTType>,
        index: &ASTIndex,
    ) -> Result<bool, TypeCheckError> {
        let result = zip(parameter_types.iter(), parameter_types_filter.iter())
            .map(|(parameter_type, parameter_type_filter)| {
                Self::almost_same_type(
                    parameter_type,
                    parameter_type_filter,
                    resolved_generic_types,
                    index,
                )
            })
            .collect::<Result<Vec<bool>, TypeCheckError>>()?
            .iter()
            .all(|it| *it);
        debug_i!("match_parameters: {result}");
        Ok(result)
    }

    pub fn almost_same_return_type(
        actual_return_type: &ASTType,
        expected_return_type: &ASTType,
        resolved_generic_types: &mut LinkedHashMap<String, ASTType>,
        index: &ASTIndex,
    ) -> Result<bool, TypeCheckError> {
        Self::almost_same_type_internal(
            actual_return_type,
            &TypeFilter::Exact(expected_return_type.clone()),
            resolved_generic_types,
            index,
            true,
        )
    }

    pub fn almost_same_type(
        parameter_type: &ASTType,
        parameter_type_filter: &TypeFilter,
        resolved_generic_types: &mut LinkedHashMap<String, ASTType>,
        index: &ASTIndex,
    ) -> Result<bool, TypeCheckError> {
        Self::almost_same_type_internal(
            parameter_type,
            parameter_type_filter,
            resolved_generic_types,
            index,
            false,
        )
    }
    fn almost_same_type_internal(
        parameter_type: &ASTType,
        parameter_type_filter: &TypeFilter,
        resolved_generic_types: &mut LinkedHashMap<String, ASTType>,
        index: &ASTIndex,
        return_type: bool,
    ) -> Result<bool, TypeCheckError> {
        debug_i!("almost_same_type {parameter_type} filter {parameter_type_filter} return_type: {return_type}");
        indent!();
        let result: bool = match parameter_type_filter {
            TypeFilter::Any => Ok::<bool, TypeCheckError>(true),
            TypeFilter::Exact(filter_type) => {
                if filter_type == parameter_type {
                    dedent!();
                    return Ok(true);
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
                                debug_i!("filter_ps {}", SliceDisplay(filter_ps));
                                debug_i!("a_p {}", SliceDisplay(a_p));

                                let parameter_same = filter_ps.len() == a_p.len()
                                    && zip(filter_ps, a_p)
                                        .map(|(filter_type, a)| {
                                            Self::almost_same_type_internal(
                                                a,
                                                &TypeFilter::Exact(filter_type.clone()),
                                                resolved_generic_types,
                                                index,
                                                return_type,
                                            )
                                        })
                                        .collect::<Result<Vec<_>, TypeCheckError>>()?
                                        .into_iter()
                                        .all(|it| it);

                                let return_type_same = Self::almost_same_return_type(
                                    a_rt,
                                    &filter_rt.deref().clone(),
                                    resolved_generic_types,
                                    index,
                                )?;

                                Ok(parameter_same && return_type_same)
                            }
                            _ => Ok(false),
                        },
                        _ => {
                            debug_i!("parameter type {parameter_type}");
                            let r = match parameter_type {
                                ASTType::Builtin(_) => filter_type == parameter_type,
                                ASTType::Generic(_) => true,
                                ASTType::Custom { .. } => false,
                                ASTType::Unit => {
                                    if return_type {
                                        false
                                    } else {
                                        return Self::unit_type_is_not_allowed_here(index);
                                    }
                                }
                            };
                            Ok(r)
                        }
                    },
                    ASTType::Generic(filter_generic_type) => {
                        let already_resolved_o = resolved_generic_types.get(filter_generic_type);
                        debug_i!("already_resolved {}", OptionDisplay(&already_resolved_o));
                        match parameter_type {
                            ASTType::Generic(_parameter_generic_type) => {
                                // TODO we don't know if the two generic types belong to the same context (Enum, Struct or function),
                                //   to know it we need another attribute in ASTType::Builtin::Generic : the context
                                Ok(true)
                            }
                            _ => {
                                if let Some(already_resolved) = already_resolved_o {
                                    Ok(already_resolved == parameter_type)
                                } else {
                                    resolved_generic_types.insert(
                                        filter_generic_type.clone(),
                                        parameter_type.clone(),
                                    );
                                    Ok(true)
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
                            ASTType::Builtin(_) => Ok(false),
                            ASTType::Generic(_) => Ok(true), // TODO
                            ASTType::Custom {
                                param_types,
                                name: type_name,
                                index: _,
                            } => Ok(type_name == expected_type_name
                                && param_types.len() == expected_param_types.len()
                                && param_types
                                    .iter()
                                    .enumerate()
                                    .map(|(i, pt)| {
                                        Self::almost_same_type_internal(
                                            pt,
                                            &TypeFilter::Exact(
                                                expected_param_types.get(i).unwrap().clone(),
                                            ),
                                            resolved_generic_types,
                                            index,
                                            false,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, TypeCheckError>>()?
                                    .into_iter()
                                    .all(|it| it)),
                            ASTType::Unit => {
                                if !return_type {
                                    return Self::unit_type_is_not_allowed_here(index);
                                } else {
                                    Ok(false)
                                }
                            }
                        }
                    }
                    ASTType::Unit => match parameter_type {
                        ASTType::Builtin(_) => Ok(false),
                        ASTType::Generic(name) => {
                            if let Some(gt) = resolved_generic_types.get(name) {
                                Ok(gt.is_unit())
                            } else {
                                resolved_generic_types.insert(name.to_owned(), ASTType::Unit);
                                Ok(true)
                            }
                        }
                        ASTType::Custom { .. } => Ok(false),
                        ASTType::Unit => {
                            if return_type {
                                Ok(false)
                            } else {
                                return Self::unit_type_is_not_allowed_here(index);
                            }
                        }
                    },
                }
            }
            TypeFilter::Lambda(len) => {
                if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters,
                    return_type: _,
                }) = parameter_type
                {
                    Ok(len == &parameters.len())
                } else {
                    Ok(false)
                }
            }
            TypeFilter::NotALambda => Ok(!matches!(
                parameter_type,
                ASTType::Builtin(BuiltinTypeKind::Lambda { .. })
            )),
        }?;
        debug_i!("almost same: {result}");
        dedent!();
        Ok(result)
    }

    fn unit_type_is_not_allowed_here(index: &ASTIndex) -> Result<bool, TypeCheckError> {
        //Err(format!("Unit type is not allowed here: {index}").into())
        Ok(false)
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

    pub fn check_duplicate_functions(&self) {
        for (_name, functions) in self.functions_by_name.iter() {
            if functions.len() > 1 {
                for function in functions.iter() {
                    for other_function in functions.iter() {
                        if function.name != other_function.name {
                            if function.parameters.len() == other_function.parameters.len() {
                                if zip(&function.parameters, &other_function.parameters).all(
                                    |(a, b)| {
                                        a.ast_type == b.ast_type
                                            || matches!(a.ast_type, ASTType::Generic(_))
                                                && matches!(b.ast_type, ASTType::Generic(_))
                                    },
                                ) {
                                    panic!(
                                        "Duplicate function signature {} {}",
                                        function.index, other_function.index
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::ASTFunctionBody::ASMBody;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTParameterDef, ASTType,
        BuiltinTypeKind, ValueType,
    };
    use crate::type_check::functions_container::FunctionsContainer;
    use crate::type_check::functions_container::TypeFilter::Exact;
    use crate::type_check::resolved_generic_types::ResolvedGenericTypes;

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
            &ASTIndex::none(),
        );

        assert!(result.unwrap().is_some());
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
            resolved_generic_types: ResolvedGenericTypes::new(),
            return_type: ASTType::Builtin(BuiltinTypeKind::String),
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
            resolved_generic_types: ResolvedGenericTypes::new(),
            return_type: ASTType::Builtin(param_kind),
            original_name: "add".into(),
            index: ASTIndex::none(),
        }
    }
}
