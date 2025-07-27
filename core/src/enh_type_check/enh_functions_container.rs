use std::fmt::{Debug, Display, Formatter};
use std::iter::zip;
use std::ops::Deref;

use linked_hash_map::LinkedHashMap;
use rasm_utils::{debug_i, dedent, indent, OptionDisplay, SliceDisplay};

use crate::codegen::enh_ast::{
    EnhASTFunctionBody, EnhASTFunctionCall, EnhASTFunctionDef, EnhASTIndex, EnhASTType,
    EnhBuiltinTypeKind,
};
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::enh_type_check::enh_type_check_error::EnhTypeCheckError;

use super::enh_resolved_generic_types::EnhResolvedGenericTypes;

#[derive(Clone, Debug, PartialEq)]
pub struct EnhFunctionsContainer {
    functions_by_name: LinkedHashMap<String, Vec<EnhASTFunctionDef>>,
}

#[derive(Clone, Debug)]
pub enum EnhTypeFilter {
    Exact(EnhASTType),
    Any,
    Lambda(usize, Option<Box<EnhTypeFilter>>),
    //    NotALambda,
}

impl EnhTypeFilter {
    pub fn almost_equal(
        &self,
        ast_type: &EnhASTType,
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<bool, EnhTypeCheckError> {
        EnhFunctionsContainer::almost_same_type(
            ast_type,
            self,
            &mut EnhResolvedGenericTypes::new(),
            &EnhASTIndex::none(),
            enhanced_astmodule,
        )
    }
}

impl Display for EnhTypeFilter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EnhTypeFilter::Exact(ast_type) => write!(f, "Exact({ast_type})",),
            EnhTypeFilter::Any => write!(f, "Any"),
            EnhTypeFilter::Lambda(size, type_filter) => {
                write!(f, "Lambda({size}, {})", OptionDisplay(type_filter))
            } //EnhTypeFilter::NotALambda => write!(f, "Not a lambda"),
        }
    }
}

impl EnhFunctionsContainer {
    pub fn new() -> Self {
        Self {
            functions_by_name: LinkedHashMap::new(),
        }
    }

    pub fn add_function(&mut self, original_name: String, function_def: EnhASTFunctionDef) {
        // let original_name = function_def.original_name.clone();
        if let Some(functions) = self.functions_by_name.get_mut(&original_name) {
            functions.push(function_def);
        } else {
            self.functions_by_name
                .insert(original_name, vec![function_def]);
        }
    }

    pub fn replace_body(&mut self, function_def: &EnhASTFunctionDef, body: EnhASTFunctionBody) {
        let function = self.get_function(&function_def.name);
        function.body = body;
    }

    pub fn remove(&mut self, function_def: &EnhASTFunctionDef) {
        let function = self
            .functions_by_name
            .get_mut(&function_def.original_name)
            .unwrap();
        function.remove(
            function
                .iter()
                .enumerate()
                .find(|(_, it)| it.name == function_def.name)
                .unwrap()
                .0,
        );
    }

    pub fn find_function(&self, name: &str) -> Option<&EnhASTFunctionDef> {
        let functions = self.functions();
        let found = functions
            .into_iter()
            .filter(|it| it.name == name)
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

    pub fn get_function(&mut self, name: &str) -> &mut EnhASTFunctionDef {
        self.functions_by_name
            .iter_mut()
            .flat_map(|it| it.1.iter_mut())
            .find(|it| it.name == name)
            .unwrap()
    }

    pub fn has_function(&self, original_name: &str, name: &str) -> bool {
        self.functions_by_name
            .get(original_name)
            .map(|it| it.iter().any(|function_def| function_def.name == name))
            .unwrap_or(false)
    }

    pub fn find_function_by_original_name(&self, name: &str) -> Option<&EnhASTFunctionDef> {
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

    pub fn count_by_original_name(&self, name: &str) -> usize {
        let found = self.functions_by_name.get(name);

        if let Some(f) = found {
            f.len()
        } else {
            0
        }
    }

    pub fn find_functions_by_original_name(&self, name: &str) -> &[EnhASTFunctionDef] {
        if let Some(functions) = self.functions_by_name.get(name) {
            functions
        } else {
            &[]
        }
    }

    pub fn find_call(
        &self,
        function_name: &str,
        original_function_name: &str,
        parameter_types_filter: &Vec<EnhTypeFilter>,
        return_type_filter: Option<&EnhASTType>,
        filter_on_name: bool,
        index: &EnhASTIndex,
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<Option<&EnhASTFunctionDef>, EnhTypeCheckError> {
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
                    enhanced_astmodule,
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

    fn find_call_vec_1<'a>(
        function_name: &str,
        parameter_types_filter: &Vec<EnhTypeFilter>,
        return_type_filter: Option<&EnhASTType>,
        filter_on_name: bool,
        index: &EnhASTIndex,
        functions: &'a [EnhASTFunctionDef],
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<Vec<&'a EnhASTFunctionDef>, EnhTypeCheckError> {
        let lambda = |it: &&EnhASTFunctionDef| {
            debug_i!(
                "testing function {it}, filters {}, return type {}",
                SliceDisplay(parameter_types_filter),
                OptionDisplay(&return_type_filter)
            );
            if filter_on_name && it.name == function_name {
                return true;
            }
            if parameter_types_filter.len() != it.parameters.len() {
                return false;
            }
            indent!();
            let mut resolved_generic_types = EnhResolvedGenericTypes::new();
            let result = Self::almost_same_parameters_types(
                it.parameters
                    .iter()
                    .map(|it| &it.ast_type)
                    .collect::<Vec<_>>(),
                parameter_types_filter,
                &mut resolved_generic_types,
                &it.index,
                enhanced_astmodule,
            )
            .and_then(|verify_params| {
                if !verify_params {
                    Ok(false)
                } else {
                    match return_type_filter {
                        None => Ok(true),
                        Some(ref rt) => {
                            if matches!(rt, EnhASTType::Generic(_, _, _)) {
                                Ok(true)
                            } else {
                                Self::almost_same_return_type(
                                    &it.return_type,
                                    rt,
                                    &mut resolved_generic_types,
                                    index,
                                    enhanced_astmodule,
                                )
                            }
                        }
                    }
                }
            })
            .unwrap_or(false);
            dedent!();
            result
        };
        let matching_functions = functions.iter().filter(lambda).collect::<Vec<_>>();
        Ok(matching_functions)
    }

    pub fn find_call_vec(
        &self,
        call: &EnhASTFunctionCall,
        parameter_types_filter: &Vec<EnhTypeFilter>,
        return_type_filter: Option<&EnhASTType>,
        filter_only_on_name: bool,
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<Vec<&EnhASTFunctionDef>, EnhTypeCheckError> {
        debug_i!(
            "find_call_vec {call} return type {} filter {}",
            OptionDisplay(&return_type_filter),
            SliceDisplay(parameter_types_filter)
        );
        let result =
            if let Some(functions) = self.functions_by_name.get(&call.original_function_name) {
                if functions.is_empty() {
                    panic!(
                        "cannot find functions for call {call} filter {}",
                        SliceDisplay(parameter_types_filter)
                    );
                } else {
                    Self::find_call_vec_1(
                        &call.function_name,
                        parameter_types_filter,
                        return_type_filter,
                        filter_only_on_name,
                        &EnhASTIndex::none(),
                        functions,
                        enhanced_astmodule,
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
        parameter_types_filter: Vec<EnhASTType>,
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<Option<EnhASTFunctionDef>, EnhTypeCheckError> {
        if let Some(functions) = self.functions_by_name.get(&name) {
            if functions.is_empty() {
                panic!(
                    "cannot find function {name} filter {:?}",
                    parameter_types_filter
                );
            } else {
                let mut resolved_generic_types = EnhResolvedGenericTypes::new();

                let lambda = |it: &EnhASTFunctionDef| {
                    if it.name != name {
                        Ok((it.clone(), false))
                    } else {
                        Self::almost_same_parameters_types(
                            it.parameters
                                .iter()
                                .map(|it| &it.ast_type)
                                .collect::<Vec<_>>(),
                            &parameter_types_filter
                                .iter()
                                .map(|it| EnhTypeFilter::Exact(it.clone()))
                                .collect(),
                            &mut resolved_generic_types,
                            &it.index,
                            enhanced_astmodule,
                        )
                        .map(|b| (it.clone(), b))
                    }
                };

                let result = functions
                    .iter()
                    .map(lambda)
                    .collect::<Result<Vec<_>, EnhTypeCheckError>>()?
                    .into_iter()
                    .filter(|(_f, b)| *b)
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
        parameter_types: Vec<&EnhASTType>,
        parameter_types_filter: &Vec<EnhTypeFilter>,
        resolved_generic_types: &mut EnhResolvedGenericTypes,
        index: &EnhASTIndex,
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<bool, EnhTypeCheckError> {
        let result = if parameter_types.len() != parameter_types_filter.len() {
            debug_i!("not matching parameter length");
            false
        } else {
            Self::match_parameters(
                parameter_types,
                parameter_types_filter,
                resolved_generic_types,
                index,
                enhanced_astmodule,
            )?
        };
        Ok(result)
    }

    fn match_parameters(
        parameter_types: Vec<&EnhASTType>,
        parameter_types_filter: &[EnhTypeFilter],
        resolved_generic_types: &mut EnhResolvedGenericTypes,
        index: &EnhASTIndex,
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<bool, EnhTypeCheckError> {
        let result = zip(parameter_types.iter(), parameter_types_filter.iter())
            .map(|(parameter_type, parameter_type_filter)| {
                Self::almost_same_type(
                    parameter_type,
                    parameter_type_filter,
                    resolved_generic_types,
                    index,
                    enhanced_astmodule,
                )
            })
            .collect::<Result<Vec<bool>, EnhTypeCheckError>>()?
            .iter()
            .all(|it| *it);
        debug_i!("match_parameters: {result}");
        Ok(result)
    }

    pub fn almost_same_return_type(
        actual_return_type: &EnhASTType,
        expected_return_type: &EnhASTType,
        resolved_generic_types: &mut EnhResolvedGenericTypes,
        index: &EnhASTIndex,
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<bool, EnhTypeCheckError> {
        Self::almost_same_type_internal(
            actual_return_type,
            &EnhTypeFilter::Exact(expected_return_type.clone()),
            resolved_generic_types,
            index,
            true,
            enhanced_astmodule,
        )
    }

    pub fn almost_same_type(
        parameter_type: &EnhASTType,
        parameter_type_filter: &EnhTypeFilter,
        resolved_generic_types: &mut EnhResolvedGenericTypes,
        index: &EnhASTIndex,
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<bool, EnhTypeCheckError> {
        Self::almost_same_type_internal(
            parameter_type,
            parameter_type_filter,
            resolved_generic_types,
            index,
            false,
            enhanced_astmodule,
        )
    }
    fn almost_same_type_internal(
        parameter_type: &EnhASTType,
        parameter_type_filter: &EnhTypeFilter,
        resolved_generic_types: &mut EnhResolvedGenericTypes,
        index: &EnhASTIndex,
        return_type: bool,
        enhanced_astmodule: &EnhancedASTModule,
    ) -> Result<bool, EnhTypeCheckError> {
        debug_i!("almost_same_type {parameter_type} filter {parameter_type_filter} return_type: {return_type}");
        indent!();
        let result: bool = match parameter_type_filter {
            EnhTypeFilter::Any => Ok::<bool, EnhTypeCheckError>(true),
            EnhTypeFilter::Exact(filter_type) => {
                if filter_type == parameter_type {
                    dedent!();
                    return Ok(true);
                }
                match filter_type {
                    EnhASTType::Builtin(filter_kind) => match filter_kind {
                        EnhBuiltinTypeKind::Lambda {
                            parameters: filter_ps,
                            return_type: filter_rt,
                        } => match parameter_type {
                            EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
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
                                                &EnhTypeFilter::Exact(filter_type.clone()),
                                                resolved_generic_types,
                                                index,
                                                return_type,
                                                enhanced_astmodule,
                                            )
                                        })
                                        .collect::<Result<Vec<_>, EnhTypeCheckError>>()?
                                        .into_iter()
                                        .all(|it| it);

                                let return_type_same = Self::almost_same_return_type(
                                    a_rt,
                                    &filter_rt.deref().clone(),
                                    resolved_generic_types,
                                    index,
                                    enhanced_astmodule,
                                )?;

                                Ok(parameter_same && return_type_same)
                            }
                            EnhASTType::Generic(_, name, var_types) => {
                                resolved_generic_types.insert(
                                    name.to_owned(),
                                    var_types.clone(),
                                    filter_type.clone(),
                                );
                                Ok(true)
                            }
                            _ => Ok(false),
                        },
                        _ => {
                            debug_i!("parameter type {parameter_type}");
                            let r = match parameter_type {
                                EnhASTType::Builtin(_) => filter_type == parameter_type,
                                EnhASTType::Generic(_, _, _) => true,
                                EnhASTType::Custom { .. } => false,
                                EnhASTType::Unit => {
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
                    EnhASTType::Generic(_, filter_generic_name, filter_var_types) => {
                        let already_resolved_o =
                            resolved_generic_types.get(filter_generic_name, filter_var_types);
                        debug_i!("already_resolved {}", OptionDisplay(&already_resolved_o));
                        match parameter_type {
                            EnhASTType::Generic(_, _gen_name, _var_types) => {
                                // TODO we don't know if the two generic types belong to the same context (Enum, Struct or function),
                                //   to know it we need another attribute in ASTType::Builtin::Generic : the context

                                Ok(true)
                            }
                            _ => {
                                if let Some(already_resolved) = already_resolved_o {
                                    debug_i!("already resolved {already_resolved}");
                                    Ok(already_resolved == parameter_type)
                                } else {
                                    /*
                                    if filter_generic_name == "M" {
                                        println!(
                                            "almost_same_type_internal {} {parameter_type}",
                                            SliceDisplay(filter_var_types)
                                        );
                                    }
                                    */

                                    resolved_generic_types.insert(
                                        filter_generic_name.clone(),
                                        filter_var_types.clone(),
                                        parameter_type.clone(),
                                    );
                                    Ok(true)
                                }
                            }
                        }
                    }
                    EnhASTType::Custom {
                        namespace: expected_namespace,
                        param_types: expected_param_types,
                        name: expected_type_name,
                        index: _,
                    } => {
                        match parameter_type {
                            EnhASTType::Builtin(_) => Ok(false),
                            EnhASTType::Generic(_, _gen_name, var_types) => {
                                /*
                                if gen_name == "M" {
                                    println!(
                                        "almost_same_type_internal {} {parameter_type_filter}",
                                        SliceDisplay(var_types)
                                    );
                                }
                                */

                                Ok(var_types.is_empty()
                                    || expected_param_types.len() == var_types.len()
                                        && zip(expected_param_types.iter(), var_types.iter()).all(
                                            |(et, gt)| {
                                                let r = EnhTypeFilter::Exact(et.clone())
                                                    .almost_equal(gt, enhanced_astmodule)
                                                    .unwrap();

                                                if !r {
                                                    //println!("almost_same_type_internal {et} {gt}");
                                                }
                                                r
                                            },
                                        ))
                            } // TODO
                            EnhASTType::Custom {
                                namespace: _,
                                param_types,
                                name: type_name,
                                index: _,
                            } => {
                                if let Some(type_def) =
                                    enhanced_astmodule.get_type_def(parameter_type)
                                {
                                    Ok(type_name == expected_type_name
                                        && (type_def.modifiers().public
                                            || (type_def.namespace() == expected_namespace))
                                        && param_types.len() == expected_param_types.len()
                                        && param_types
                                            .iter()
                                            .enumerate()
                                            .map(|(i, pt)| {
                                                Self::almost_same_type_internal(
                                                    pt,
                                                    &EnhTypeFilter::Exact(
                                                        expected_param_types
                                                            .get(i)
                                                            .unwrap()
                                                            .clone(),
                                                    ),
                                                    resolved_generic_types,
                                                    index,
                                                    false,
                                                    enhanced_astmodule,
                                                )
                                            })
                                            .collect::<Result<Vec<_>, EnhTypeCheckError>>()?
                                            .into_iter()
                                            .all(|it| it))
                                } else {
                                    Err(EnhTypeCheckError::new(
                                        index.clone(),
                                        format!(
                                            "Cannot find custom type definition for {type_name}"
                                        ),
                                        vec![],
                                    )) // TODO stack?
                                }
                            }
                            EnhASTType::Unit => {
                                if !return_type {
                                    return Self::unit_type_is_not_allowed_here(index);
                                } else {
                                    Ok(false)
                                }
                            }
                        }
                    }
                    EnhASTType::Unit => match parameter_type {
                        EnhASTType::Builtin(_) => Ok(false),
                        EnhASTType::Generic(_, name, var_types) => {
                            if let Some(gt) = resolved_generic_types.get(name, var_types) {
                                Ok(gt.is_unit())
                            } else {
                                resolved_generic_types.insert(
                                    name.to_owned(),
                                    var_types.clone(),
                                    EnhASTType::Unit,
                                );
                                Ok(true)
                            }
                        }
                        EnhASTType::Custom { .. } => Ok(false),
                        EnhASTType::Unit => {
                            if return_type {
                                Ok(false)
                            } else {
                                return Self::unit_type_is_not_allowed_here(index);
                            }
                        }
                    },
                }
            }
            EnhTypeFilter::Lambda(len, lambda_return_type_filter) => {
                // TODO return type
                if let EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type: _,
                }) = parameter_type
                {
                    Ok(len == &parameters.len())
                } else if let EnhASTType::Generic(_, name, var_types) = parameter_type {
                    if let Some(EnhTypeFilter::Exact(lrt)) = lambda_return_type_filter.as_deref() {
                        if *len == 0 {
                            let lambda = EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                                parameters: Vec::new(),
                                return_type: Box::new(lrt.clone()),
                            });

                            resolved_generic_types.insert(
                                name.to_owned(),
                                var_types.clone(),
                                lambda,
                            );
                            Ok(true)
                        } else {
                            Ok(false)
                        }
                    } else {
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            } /*
              EnhTypeFilter::NotALambda => Ok(!matches!(
                  parameter_type,
                  EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda { .. })
              )),
              */
        }?;
        debug_i!("almost same: {result}");
        dedent!();
        Ok(result)
    }

    fn unit_type_is_not_allowed_here(_index: &EnhASTIndex) -> Result<bool, EnhTypeCheckError> {
        //Err(format!("Unit type is not allowed here: {index}").into())
        Ok(false)
    }

    pub fn functions(&self) -> Vec<&EnhASTFunctionDef> {
        self.functions_by_name
            .values()
            .flat_map(|it| it.iter())
            .collect()
    }

    pub fn functions_mut(&mut self) -> Vec<&mut EnhASTFunctionDef> {
        self.functions_by_name
            .iter_mut()
            .flat_map(|it| it.1.iter_mut())
            .collect()
    }

    pub fn functions_owned(self) -> Vec<EnhASTFunctionDef> {
        self.functions_by_name
            .into_iter()
            .flat_map(|it| it.1.into_iter())
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

    pub fn fix_namespaces(&self, enhanced_module: &EnhancedASTModule) -> Self {
        Self {
            functions_by_name: self
                .functions_by_name
                .iter()
                .map(|(key, f)| {
                    (
                        key.clone(),
                        f.into_iter()
                            .map(|it| it.clone().fix_namespaces(enhanced_module))
                            .collect(),
                    )
                })
                .collect(),
        }
    }

    pub fn fix_generics(&self) -> Self {
        Self {
            functions_by_name: self
                .functions_by_name
                .iter()
                .map(|(key, f)| {
                    (
                        key.clone(),
                        f.into_iter().map(|it| it.clone().fix_generics()).collect(),
                    )
                })
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use linked_hash_map::LinkedHashMap;

    use crate::codegen::asm::code_gen_asm::AsmOptions;
    use crate::codegen::compile_target::CompileTarget;
    use crate::codegen::enh_ast::{
        EnhASTExpression, EnhASTFunctionBody, EnhASTFunctionCall, EnhASTFunctionDef, EnhASTIndex,
        EnhASTNameSpace, EnhASTParameterDef, EnhASTType, EnhBuiltinTypeKind,
    };
    use crate::codegen::enhanced_module::EnhancedASTModule;
    use crate::codegen::statics::Statics;
    use crate::enh_type_check::enh_functions_container::EnhFunctionsContainer;
    use crate::enh_type_check::enh_functions_container::EnhTypeFilter::Exact;
    use crate::enh_type_check::enh_resolved_generic_types::EnhResolvedGenericTypes;
    use crate::project::{RasmConfig, RasmPackage, RasmProject};
    use rasm_parser::parser::ast::{ASTModifiers, ASTValueType};
    use std::path::PathBuf;

    #[test]
    fn test_2() {
        let mut sut = EnhFunctionsContainer::new();

        let function_def = create_function("AModule::toString_0", "n", EnhBuiltinTypeKind::Integer);

        sut.add_function("toString".to_string(), function_def);

        let function_def = create_function("AModule::toString_1", "b", EnhBuiltinTypeKind::Boolean);

        sut.add_function("toString".to_string(), function_def);

        assert!(sut.find_function("AModule::toString_0").is_some());
        assert!(sut.find_function("AModule::toString_1").is_some());
    }

    #[test]
    fn test_3() {
        let mut sut = EnhFunctionsContainer::new();

        let function_def = create_function("toString_0", "n", EnhBuiltinTypeKind::Integer);

        sut.add_function("toString".to_string(), function_def);

        assert!(sut.find_function("toString_0").is_some());

        let mut function_def = create_function("toString_1", "n", EnhBuiltinTypeKind::Integer);
        function_def.parameters = vec![];

        sut.add_function("toString".to_string(), function_def);

        assert!(sut.find_function("toString_0").is_some());
        assert!(sut.find_function("toString_1").is_some());
    }

    #[test]
    fn test_4() {
        let mut sut = EnhFunctionsContainer::new();

        let function_def = create_add_function("n", EnhBuiltinTypeKind::Integer);

        sut.add_function("add".to_string(), function_def);

        let function_def = create_add_function("s", EnhBuiltinTypeKind::String);

        sut.add_function("add".to_string(), function_def);

        let call = EnhASTFunctionCall {
            namespace: EnhASTNameSpace::global(),
            original_function_name: "add".into(),
            function_name: "add".into(),
            parameters: vec![
                EnhASTExpression::Value(ASTValueType::Integer(10), EnhASTIndex::none()),
                EnhASTExpression::Value(ASTValueType::Integer(20), EnhASTIndex::none()),
            ],
            index: EnhASTIndex::none(),
            generics: Vec::new(),
            target: None,
        };

        let project = RasmProject {
            root: PathBuf::new(),
            config: RasmConfig {
                package: RasmPackage {
                    name: "test".to_string(),
                    version: "".to_string(),
                    main: None,
                    source_folder: Some("".to_string()),
                },
                dependencies: None,
                natives: None,
            },
            from_file: false,
            in_memory_files: LinkedHashMap::new(),
        };

        let mut statics = Statics::new();

        let (module, _) = EnhancedASTModule::new(vec![], &project, &mut statics, &target(), false);

        let result = sut.find_call(
            &call.function_name,
            &call.original_function_name,
            &vec![
                Exact(EnhASTType::Builtin(EnhBuiltinTypeKind::Integer)),
                Exact(EnhASTType::Generic(
                    EnhASTIndex::none(),
                    "T".into(),
                    Vec::new(),
                )),
            ],
            None,
            false,
            &EnhASTIndex::none(),
            &module,
        );

        assert!(result.unwrap().is_some());
    }

    #[test]
    fn test_5() {
        let mut functions_container = EnhFunctionsContainer::new();

        functions_container.add_function("aFun".to_string(), simple_function_def("aFun_0"));

        functions_container.add_function("f".to_string(), simple_function_def("newFun_0"));
        functions_container.add_function("ff".to_string(), simple_function_def("anotherNewFun_0"));

        assert!(functions_container.find_function("aFun_0").is_some());
        assert!(functions_container.find_function("newFun_0").is_some());
        assert!(functions_container
            .find_function("anotherNewFun_0")
            .is_some());

        assert_eq!(
            functions_container
                .functions()
                .iter()
                .map(|it| it.name.clone())
                .collect::<Vec<String>>(),
            vec!["aFun_0", "newFun_0", "anotherNewFun_0"]
        );

        assert_eq!(functions_container.len(), 3);
    }

    fn target() -> CompileTarget {
        CompileTarget::Nasmi386(AsmOptions::default())
    }

    fn simple_function_def(name: &str) -> EnhASTFunctionDef {
        EnhASTFunctionDef {
            name: name.into(),
            body: EnhASTFunctionBody::NativeBody("".into()),
            parameters: Vec::new(),
            return_type: EnhASTType::Unit,
            generic_types: Vec::new(),
            resolved_generic_types: EnhResolvedGenericTypes::new(),
            original_name: name.into(),
            index: EnhASTIndex::none(),
            modifiers: ASTModifiers::private(),
            namespace: EnhASTNameSpace::global(),
            rank: 0,
            target: None,
        }
    }

    fn create_function(
        name: &str,
        param_name: &str,
        param_kind: EnhBuiltinTypeKind,
    ) -> EnhASTFunctionDef {
        EnhASTFunctionDef {
            name: name.into(),
            body: EnhASTFunctionBody::NativeBody("".into()),
            generic_types: vec![],
            parameters: vec![EnhASTParameterDef {
                name: param_name.into(),
                ast_type: EnhASTType::Builtin(param_kind),
                ast_index: EnhASTIndex::none(),
            }],
            resolved_generic_types: EnhResolvedGenericTypes::new(),
            return_type: EnhASTType::Builtin(EnhBuiltinTypeKind::String),
            original_name: name.into(),
            index: EnhASTIndex::none(),
            modifiers: ASTModifiers::private(),
            namespace: EnhASTNameSpace::global(),
            rank: 0,
            target: None,
        }
    }

    fn create_add_function(param_name: &str, param_kind: EnhBuiltinTypeKind) -> EnhASTFunctionDef {
        EnhASTFunctionDef {
            name: "add".into(),
            body: EnhASTFunctionBody::NativeBody("".into()),
            generic_types: vec![],
            parameters: vec![
                EnhASTParameterDef {
                    name: param_name.into(),
                    ast_type: EnhASTType::Builtin(param_kind.clone()),
                    ast_index: EnhASTIndex::none(),
                },
                EnhASTParameterDef {
                    name: format!("{}_1", param_name),
                    ast_type: EnhASTType::Builtin(param_kind.clone()),
                    ast_index: EnhASTIndex::none(),
                },
            ],
            resolved_generic_types: EnhResolvedGenericTypes::new(),
            return_type: EnhASTType::Builtin(param_kind),
            original_name: "add".into(),
            index: EnhASTIndex::none(),
            modifiers: ASTModifiers::private(),
            namespace: EnhASTNameSpace::global(),
            rank: 0,
            target: None,
        }
    }
}
