use std::{collections::HashMap, iter::zip, ops::Deref};

use itertools::Itertools;
use linked_hash_map::LinkedHashMap;
use linked_hash_set::LinkedHashSet;
use log::info;
use rasm_parser::{
    catalog::{modules_catalog::ModulesCatalog, ASTIndex, ModuleInfo},
    parser::ast::{ASTPosition, ASTType, BuiltinTypeKind},
};
use rasm_utils::{debug_i, dedent, indent, OptionDisplay, SliceDisplay};

use crate::{
    codegen::{
        c::code_gen_c::value_type_to_enh_type,
        compile_target::CompileTarget,
        enh_ast::{
            EnhASTExpression, EnhASTFunctionBody, EnhASTFunctionCall, EnhASTFunctionDef,
            EnhASTIndex, EnhASTLambdaDef, EnhASTNameSpace, EnhASTParameterDef, EnhASTStatement,
            EnhASTType, EnhBuiltinTypeKind, EnhModuleId, EnhModuleInfo,
        },
        enh_val_context::EnhValContext,
        enhanced_module::EnhancedASTModule,
        statics::Statics,
        typedef_provider::DummyTypeDefProvider,
        EnhValKind,
    },
    enh_type_check::enh_functions_container::EnhTypeFilter,
    errors::{CompilationError, CompilationErrorKind},
    type_check::{
        ast_modules_container::{ASTModulesContainer, ASTTypeFilter},
        ast_type_checker::{ASTTypeCheckEntry, ASTTypeCheckInfo, ASTTypeChecker},
        resolve_generic_types_from_effective_type, substitute,
    },
};

use super::{
    enh_functions_container::EnhFunctionsContainer,
    enh_resolved_generic_types::EnhResolvedGenericTypes,
    enh_type_check_error::{EnhTypeCheckError, EnhTypeCheckErrorKind},
    typed_ast::DefaultFunction,
};

pub struct EnhTypeCheck<'a> {
    target: CompileTarget,
    debug: bool,
    stack: Vec<EnhASTIndex>,
    functions_stack: LinkedHashMap<String, Vec<EnhASTIndex>>,
    new_functions: HashMap<String, EnhASTFunctionDef>,
    type_checker: ASTTypeChecker,
    modules_catalog: &'a dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    modules_container: &'a ASTModulesContainer,
}

type InputModule = EnhancedASTModule;
type OutputModule = EnhancedASTModule;

impl<'a> EnhTypeCheck<'a> {
    pub fn new(
        target: CompileTarget,
        debug: bool,
        type_checker: ASTTypeChecker,
        modules_catalog: &'a dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        modules_container: &'a ASTModulesContainer,
    ) -> Self {
        Self {
            target,
            debug,
            stack: vec![],
            functions_stack: Default::default(),
            new_functions: HashMap::new(),
            type_checker,
            modules_catalog,
            modules_container,
        }
    }

    pub fn type_check(
        mut self,
        module: EnhancedASTModule,
        statics: &mut Statics,
        default_functions: Vec<DefaultFunction>,
        mandatory_functions: Vec<DefaultFunction>,
    ) -> Result<OutputModule, CompilationError> {
        let mut val_context = EnhValContext::new(None);

        let mut default_functions = default_functions; // TODO print_allocation

        default_functions.extend(mandatory_functions);

        for default_function in default_functions {
            if let Some(f) =
                module.find_precise_function(&default_function.name, &default_function.name)
            {
                // TODO check error
                self.new_functions
                    .insert(default_function.name.clone(), f.clone());
                self.functions_stack
                    .insert(default_function.name.clone(), vec![]);
            } else {
                return Err(Self::compilation_error(format!(
                    "Cannot find default function {}",
                    default_function.name
                )));
            }
        }

        let mut new_functions = Vec::new();

        let new_body = self
            .transform_statements(
                &module,
                &module.body,
                &mut val_context,
                statics,
                None,
                &module.body_namespace,
                None,
                &mut new_functions,
                true,
            )
            .map_err(|it| CompilationError {
                index: EnhASTIndex::none(),
                error_kind: CompilationErrorKind::TypeCheck(
                    format!(
                        "Error transforming statements for module body, body namespace {}",
                        module.body_namespace
                    ),
                    vec![it],
                ),
            })?;

        for (f, s) in new_functions {
            let new_function_name = f.name.clone();
            if !self.functions_stack.contains_key(&new_function_name) {
                self.new_functions.insert(new_function_name.clone(), f);
                self.functions_stack.insert(new_function_name, s);
            }
        }

        loop {
            let functions_len = self.new_functions.len();
            info!("Type check loop {}", functions_len);

            let new_function_names = self.functions_stack.keys().cloned().collect::<Vec<_>>();

            for function_name in new_function_names {
                if let Some(stack) = self.functions_stack.remove(&function_name) {
                    self.stack = stack;
                } else {
                    continue;
                }

                let mut function = self.new_functions.get(&function_name).unwrap().clone();

                let mut new_functions = Vec::new();
                match self
                    .transform_function(&module, statics, &function, &mut new_functions)
                    .map_err(|it| CompilationError {
                        index: function.index.clone(),
                        error_kind: CompilationErrorKind::TypeCheck(
                            format!(
                                "Error in function {} : {}",
                                function.original_name, function.index
                            ),
                            vec![it],
                        ),
                    })? {
                    Some(new_body) => {
                        function.body = new_body;
                        self.new_functions.insert(function.name.clone(), function);
                    }
                    None => {}
                }
                for (f, s) in new_functions {
                    let new_function_name = f.name.clone();
                    if !self.functions_stack.contains_key(&new_function_name) {
                        self.new_functions.insert(f.name.clone(), f);
                        self.functions_stack.insert(new_function_name, s);
                    }
                }
            }

            if self.new_functions.len() == functions_len {
                break;
            }
        }

        let mut typed_module = EnhancedASTModule {
            body: new_body,
            functions_by_name: EnhFunctionsContainer::new(),
            enums: module.enums,
            structs: module.structs,
            types: module.types,
            body_namespace: module.body_namespace,
        };

        for (_, function) in self.new_functions {
            typed_module.add_function(function.original_name.clone(), function);
        }

        Ok(typed_module)
    }

    fn compilation_error(message: String) -> CompilationError {
        CompilationError {
            index: EnhASTIndex::none(),
            error_kind: CompilationErrorKind::TypeCheck(message, Vec::new()),
        }
    }

    fn transform_statement(
        &mut self,
        module: &InputModule,
        statement: &EnhASTStatement,
        val_context: &mut EnhValContext,
        statics: &mut Statics,
        expected_return_type: Option<&EnhASTType>,
        namespace: &EnhASTNameSpace,
        inside_function: Option<&EnhASTFunctionDef>,
        new_functions: &mut Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
        strict: bool,
    ) -> Result<EnhASTStatement, EnhTypeCheckError> {
        match statement {
            EnhASTStatement::Expression(e) => self
                .transform_expression(
                    module,
                    e,
                    val_context,
                    statics,
                    expected_return_type,
                    namespace,
                    inside_function,
                    new_functions,
                    strict,
                )
                .map(EnhASTStatement::Expression),
            EnhASTStatement::LetStatement(name, e, index) => self
                .transform_expression(
                    module,
                    e,
                    val_context,
                    statics,
                    None,
                    namespace,
                    inside_function,
                    new_functions,
                    strict,
                )
                .map(|it| EnhASTStatement::LetStatement(name.clone(), it, index.clone())),
            EnhASTStatement::ConstStatement(name, e, index, namespace, modifiers) => self
                .transform_expression(
                    module,
                    e,
                    val_context,
                    statics,
                    None,
                    namespace,
                    inside_function,
                    new_functions,
                    strict,
                )
                .map(|it| {
                    EnhASTStatement::ConstStatement(
                        name.clone(),
                        it,
                        index.clone(),
                        namespace.clone(),
                        modifiers.clone(),
                    )
                }),
        }
    }

    fn transform_expression(
        &mut self,
        module: &InputModule,
        expression: &EnhASTExpression,
        val_context: &EnhValContext,
        statics: &mut Statics,
        expected_type: Option<&EnhASTType>,
        namespace: &EnhASTNameSpace,
        inside_function: Option<&EnhASTFunctionDef>,
        new_functions: &mut Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
        strict: bool,
    ) -> Result<EnhASTExpression, EnhTypeCheckError> {
        match expression {
            EnhASTExpression::ASTFunctionCallExpression(call) => self
                .transform_call(
                    module,
                    call,
                    val_context,
                    statics,
                    expected_type,
                    namespace,
                    inside_function,
                    new_functions,
                    strict,
                )
                .map(EnhASTExpression::ASTFunctionCallExpression),
            EnhASTExpression::Lambda(lambda_def) => self
                .transform_lambda_def(
                    module,
                    lambda_def,
                    val_context,
                    statics,
                    expected_type,
                    namespace,
                    inside_function,
                    new_functions,
                    strict,
                )
                .map(EnhASTExpression::Lambda),
            EnhASTExpression::ValueRef(name, index, const_namespace) => {
                if val_context.get(&name).is_some()
                    || statics.get_const(&name, const_namespace).is_some()
                {
                    return Ok(expression.clone());
                } else if name.contains("_") {
                    return Ok(expression.clone());
                }
                let mut function_references = self.functions_referenced_by_name(
                    module,
                    expected_type,
                    name,
                    &new_functions,
                    namespace,
                    index,
                );

                if function_references.len() == 1 {
                    let new_function_def = function_references.remove(0);
                    let mut function_parameters = new_function_def.parameters.clone();
                    let mut function_return_type = new_function_def.return_type.clone();
                    let mut function_generics = new_function_def.generic_types.clone();

                    if let Some(et) = expected_type {
                        if let EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                            parameters,
                            return_type,
                        }) = et
                        {
                            let mut unresolved_generic_types = false;
                            let new_parameters = zip(&new_function_def.parameters, parameters)
                                .map(|(p, t)| {
                                    if p.ast_type.is_generic() && !t.is_generic() {
                                        let mut new_p = p.clone();
                                        new_p.ast_type = t.clone();
                                        new_p
                                    } else {
                                        if p.ast_type.is_generic() {
                                            unresolved_generic_types = true;
                                        }
                                        p.clone()
                                    }
                                })
                                .collect::<Vec<_>>();
                            function_parameters = new_parameters;

                            if new_function_def.return_type.is_generic()
                                && !return_type.is_generic()
                            {
                                function_return_type = return_type.as_ref().clone();
                            } else if new_function_def.return_type.is_generic() {
                                unresolved_generic_types = true;
                            }
                            if unresolved_generic_types {
                                return Err(EnhTypeCheckError::new_with_kind(
                                    index.clone(),
                                    format!("Unresolved generic types in reference to {name}"),
                                    self.stack.clone(),
                                    EnhTypeCheckErrorKind::Important,
                                ));
                            }
                            function_generics = Vec::new(); // TODO check if there are remaining generic types
                        } else {
                            return Err(EnhTypeCheckError::new_with_kind(
                                index.clone(),
                                format!("Expected lambda but found {et} in reference to {name}"),
                                self.stack.clone(),
                                EnhTypeCheckErrorKind::Important,
                            ));
                        }
                    }

                    let new_function_name = Self::unique_function_name(
                        &new_function_def.namespace,
                        &new_function_def.original_name,
                        &function_parameters,
                        &function_return_type,
                        module,
                    );

                    if !new_functions
                        .iter()
                        .any(|it| it.0.name == new_function_name)
                        && !self.new_functions.contains_key(&new_function_name)
                    {
                        let mut new_function_def = new_function_def.clone();
                        new_function_def.name = new_function_name.clone();
                        new_function_def.parameters = function_parameters;
                        new_function_def.return_type = function_return_type;
                        new_function_def.generic_types = function_generics;

                        new_functions.push((new_function_def, self.stack.clone()));
                    }

                    Ok(EnhASTExpression::ValueRef(
                        new_function_name,
                        index.clone(),
                        namespace.clone(), // TOD const_namespace?
                    ))
                } else {
                    let message = if function_references.is_empty() {
                        format!("Cannot find reference to {name} transforming {expression}")
                    } else {
                        format!("More than one reference to {name} transforming {expression}")
                    };
                    return Err(EnhTypeCheckError::new_with_kind(
                        index.clone(),
                        message,
                        self.stack.clone(),
                        EnhTypeCheckErrorKind::Important,
                    ));
                }
            }
            _ => Ok(expression.clone()),
        }
    }

    fn transform_call(
        &mut self,
        module: &InputModule,
        call: &EnhASTFunctionCall,
        val_context: &EnhValContext,
        statics: &mut Statics,
        expected_return_type: Option<&EnhASTType>,
        namespace: &EnhASTNameSpace,
        inside_function: Option<&EnhASTFunctionDef>,
        new_functions: &mut Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
        strict: bool,
    ) -> Result<EnhASTFunctionCall, EnhTypeCheckError> {
        debug_i!(
            "transform_call {call} expected_return_type {}",
            OptionDisplay(&expected_return_type)
        );
        indent!();

        if call.function_name.contains('_') {
            debug_i!("already converted");
            dedent!();
            return Ok(call.clone());
        }

        self.stack.push(call.index.clone());

        if let Some((_return_type, parameters_types)) = val_context.get_lambda(&call.function_name)
        {
            let new_expressions: Vec<EnhASTExpression> =
                zip(call.parameters.iter(), parameters_types.iter())
                    .map(|(it, ast_type)| {
                        self.transform_expression(
                            module,
                            it,
                            val_context,
                            statics,
                            Some(ast_type),
                            namespace,
                            inside_function,
                            new_functions,
                            strict,
                        )
                    })
                    .collect::<Result<Vec<_>, EnhTypeCheckError>>()
                    .map_err(|it| {
                        dedent!();
                        self.stack.pop();
                        it.add(
                            call.index.clone(),
                            format!("calling {}", call.original_function_name),
                            self.stack.clone(),
                        )
                    })?;

            let mut new_call = call.clone();
            new_call.parameters = new_expressions;
            dedent!();
            self.stack.pop();
            return Ok(new_call);
        }

        let (mut new_function_def, resolved_generic_types, new_expressions) = self
            .get_valid_function(
                module,
                call,
                val_context,
                statics,
                expected_return_type,
                namespace,
                inside_function,
                new_functions,
                strict,
            )
            .map_err(|it| {
                self.stack.pop();
                dedent!();
                it.add(
                    call.index.clone(),
                    format!("calling {}", call.original_function_name),
                    self.stack.clone(),
                )
            })?;

        debug_i!("found valid function {new_function_def}");

        if !new_function_def.generic_types.is_empty() {
            for p in new_function_def.parameters.iter_mut() {
                if let Some(new_t) = substitute(&p.ast_type, &resolved_generic_types) {
                    p.ast_type = new_t;
                }
                if p.ast_type.is_generic() {
                    if strict {
                        self.stack.pop();
                        dedent!();
                        let result = Err(EnhTypeCheckError::new(
                            p.ast_index.clone(),
                            format!(
                                "Unresolved generic type {} : {resolved_generic_types}",
                                p.ast_type,
                            ),
                            self.stack.clone(),
                        )
                        .add(
                            call.index.clone(),
                            format!("calling {}", call.original_function_name),
                            self.stack.clone(),
                        ));
                        return result;
                    }
                }
            }

            if let Some(new_t) = substitute(&new_function_def.return_type, &resolved_generic_types)
            {
                new_function_def.return_type = new_t;
            }

            if new_function_def.return_type.is_generic() {
                if strict {
                    dedent!();
                    self.stack.pop();

                    let result = Err(EnhTypeCheckError::new(
                        new_function_def.index.clone(),
                        format!(
                            "Unresolved generic return type {}, expected return type {} : {resolved_generic_types}",
                            new_function_def.return_type,
                            OptionDisplay(&expected_return_type)
                        ),
                        self.stack.clone(),
                    )
                    .add(
                        call.index.clone(),
                        format!("calling {}", call.original_function_name),
                        self.stack.clone(),
                    ));
                    return result;
                }
            }

            new_function_def.resolved_generic_types = resolved_generic_types;
            new_function_def.generic_types = Vec::new();
        }

        let new_function_name = Self::unique_function_name(
            &new_function_def.namespace,
            &new_function_def.original_name,
            &new_function_def.parameters,
            &new_function_def.return_type,
            module,
        );

        new_function_def.name = new_function_name.clone();

        let mut new_call = call.clone();
        new_call.parameters = new_expressions;

        // we check for an already converted function, but if it is a default function it has the original name,
        // so we check even with the original name
        let converted_functions = self
            .new_functions
            .get(&new_function_name)
            .or(self.new_functions.get(&call.original_function_name));

        if let Some(converted_function) = converted_functions {
            new_call.function_name = converted_function.name.clone();
            debug_i!("already added function {}", new_call.function_name);
        } else {
            new_call.function_name = new_function_name.clone();

            debug_i!("adding new function {}", new_function_def);

            new_functions.push((new_function_def, self.stack.clone()));
            // TODO check error

            /*
            self.module
                .functions_by_name
                .add_function(new_function_def.original_name.clone(), new_function_def);
            self.functions_stack
                .insert(new_function_name, self.stack.clone());
            */
        }
        dedent!();
        self.stack.pop();
        Ok(new_call)
    }

    fn unique_function_name(
        namespace: &EnhASTNameSpace,
        original_name: &str,
        parameters: &Vec<EnhASTParameterDef>,
        return_type: &EnhASTType,
        module: &EnhancedASTModule,
    ) -> String {
        let namespace = namespace.safe_name();
        let name = original_name.replace("::", "_");

        format!(
            "{namespace}_{name}_{}_{}",
            parameters
                .iter()
                .map(|it| Self::unique_type_name(&it.ast_type, module))
                .collect::<Vec<_>>()
                .join("_"),
            Self::unique_type_name(return_type, module)
        )
    }

    fn unique_type_name(ast_type: &EnhASTType, module: &EnhancedASTModule) -> String {
        match ast_type {
            EnhASTType::Builtin(kind) => match kind {
                EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    format!(
                        "fn_{}_{}",
                        parameters
                            .iter()
                            .map(|it| Self::unique_type_name(it, module))
                            .collect::<Vec<_>>()
                            .join("_"),
                        Self::unique_type_name(return_type, module)
                    )
                }
                _ => format!("{ast_type}"),
            },
            EnhASTType::Generic(_, name, var_types) => {
                if var_types.is_empty() {
                    name.clone()
                } else {
                    format!(
                        "{name}_{}",
                        var_types
                            .iter()
                            .map(|it| EnhTypeCheck::unique_type_name(it, module))
                            .join("_")
                    )
                }
            } // TODO it should not happen when strict = true
            EnhASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => {
                if let Some(type_def) = module.get_type_def(ast_type) {
                    format!(
                        "{}_{name}_{}",
                        type_def.namespace().safe_name(),
                        param_types
                            .iter()
                            .map(|it| Self::unique_type_name(it, module))
                            .collect::<Vec<_>>()
                            .join("_")
                    )
                } else {
                    panic!("Cannot find type {ast_type} definition");
                }
            }
            EnhASTType::Unit => "Unit".to_string(),
        }
    }

    pub fn get_valid_function(
        &mut self,
        module: &InputModule,
        call: &EnhASTFunctionCall,
        val_context: &EnhValContext,
        statics: &mut Statics,
        expected_return_type: Option<&EnhASTType>,
        namespace: &EnhASTNameSpace,
        inside_function: Option<&EnhASTFunctionDef>,
        new_functions: &mut Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
        strict: bool,
    ) -> Result<
        (
            EnhASTFunctionDef,
            EnhResolvedGenericTypes,
            Vec<EnhASTExpression>,
        ),
        EnhTypeCheckError,
    > {
        debug_i!(
            "get_valid_function call {call} expected_return_type {}: {}",
            OptionDisplay(&expected_return_type),
            call.index
        );
        indent!();

        let mut valid_functions: Vec<(
            EnhASTFunctionDef,
            usize,
            EnhResolvedGenericTypes,
            Vec<EnhASTExpression>,
        )> = Vec::new();
        let mut errors = Vec::new();

        let mut original_functions = Vec::new();

        if let Some(e) = self.get_type_check_entry(&call.index, namespace) {
            if let ASTTypeCheckInfo::Call(_, vec) = e.info() {
                if vec.len() == 1 {
                    let (f, index) = vec.first().unwrap();

                    if let Some((eh_id, _eh_ns)) =
                        self.modules_catalog.catalog_info(index.module_id())
                    {
                        if let Some(f) = module
                            .find_functions_by_original_name(&f.name)
                            .iter()
                            .find(|it| {
                                &it.index.id() == eh_id
                                    && it.index.row == index.position().row
                                    && it.index.builtin == index.position().builtin
                            })
                        {
                            if f.generic_types.is_empty() {
                                // println!("optimized call {call}:{} {f}", call.index);
                                original_functions = vec![f];
                            }
                        }
                    }
                }
            }
        }

        let first_type = Self::get_first_type(call, val_context);

        if original_functions.is_empty() {
            original_functions = module
                .find_functions_by_original_name(&call.original_function_name)
                .iter()
                //.chain(same_in_new_functions.iter())
                .filter(|it| {
                    (it.modifiers.public || &it.namespace == namespace)
                        && it.parameters.len() == call.parameters.len()
                })
                .filter(|it| {
                    if let Some(ref ft) = first_type {
                        match EnhTypeFilter::Exact(ft.clone())
                            .almost_equal(&it.parameters.get(0).unwrap().ast_type, module)
                        {
                            Ok(b) => b,
                            Err(_) => false,
                        }
                    } else {
                        true
                    }
                })
                .filter(|it| {
                    call.target
                        .as_ref()
                        .map(|t| {
                            if let Some(target) = &it.target {
                                t == target
                            } else {
                                false
                            }
                        })
                        .unwrap_or(true)
                })
                .sorted_by(|fn1, fn2| fn1.rank.cmp(&fn2.rank))
                .collect::<Vec<_>>();
        }

        if original_functions.is_empty() {
            dedent!();

            return Err(EnhTypeCheckError::new_with_kind(
                call.index.clone(),
                Self::invalid_function_message(namespace, first_type, call, expected_return_type),
                self.stack.clone(),
                EnhTypeCheckErrorKind::Standard,
            ));
        }

        let mut ok_inner_new_functions = Vec::new();

        for function in original_functions {
            let mut inner_new_functions = Vec::new();

            debug_i!("verifying function {function}");

            if let Some((old_function, _, _, _)) = valid_functions.get(0) {
                // if we have already found a valid function, check another function only if it has the same rank,
                // because if two functions are valid, but with the same rank, there's a problem: we cannot identify
                // the right function to call
                if old_function.rank != function.rank {
                    debug_i!("A valid function has already been found and the current one has a different rank.");
                    break;
                }
            }

            indent!();

            let mut resolved_generic_types = EnhResolvedGenericTypes::new();

            if !call.generics.is_empty() {
                if call.generics.len() != function.generic_types.len() {
                    errors.push(EnhTypeCheckError::new(
                        function.index.clone(),
                        format!(
                            "Not matching generics expected {} but got {}",
                            call.generics.len(),
                            function.generic_types.len()
                        ),
                        self.stack.clone(),
                    ));
                    dedent!();
                    continue;
                }
                zip(function.generic_types.iter(), call.generics.iter()).for_each(|(g, t)| {
                    //println!("function.generic_type {g} call.generic {t}");

                    let ast_type = if let Some(ifun) = inside_function {
                        if let Some(st) = substitute(t, &ifun.resolved_generic_types) {
                            st
                        } else {
                            t.clone()
                        }
                    } else {
                        t.clone()
                    };

                    // TODO type classes, I don't know if it works
                    if let EnhASTType::Generic(name, _, var_types) = t {
                        resolved_generic_types.insert(g.to_string(), var_types.clone(), ast_type);
                    } else {
                        resolved_generic_types.insert(g.to_string(), Vec::new(), ast_type);
                    }
                });
            }

            if let Some(rt) = expected_return_type {
                if !EnhTypeFilter::Exact(function.return_type.clone())
                    .almost_equal(rt, module)
                    .map_err(|it| {
                        dedent!();
                        dedent!();
                        it.clone()
                    })?
                {
                    errors.push(EnhTypeCheckError::new(
                        function.index.clone(),
                        format!(
                            "Function {function}, unmatching return type expected {rt} but got {} : {}",
                            function.return_type,
                            function.index
                        ),
                        self.stack.clone(),
                    ));
                    dedent!();
                    continue;
                }

                if !rt.is_generic() && function.return_type.is_generic() {
                    if let Ok(result) =
                        resolve_generic_types_from_effective_type(&function.return_type, rt, module)
                    {
                        if let Err(e) = resolved_generic_types.extend(result) {
                            errors.push(EnhTypeCheckError::new(
                                function.index.clone(),
                                format!(
                                    "{e} resolving generic type {} with {rt}",
                                    function.return_type
                                ),
                                self.stack.clone(),
                            ));
                            dedent!();
                            continue;
                        }
                    }
                }
            }

            let mut something_resolved = true;

            let mut count = 0;
            let mut valid = true;
            let mut result = Vec::new();
            while something_resolved & valid {
                debug_i!("calculating filters loop {count}");
                indent!();
                count += 1;
                something_resolved = false;

                result = zip(call.parameters.iter(), function.parameters.iter())
                    .map(|(expr, param)| {
                        if !valid {
                            return Err(EnhTypeCheckError::dummy());
                        }
                        let resolved_count = resolved_generic_types.len();
                        debug_i!("expr {expr}");

                        let substituted_type = substitute(&param.ast_type, &resolved_generic_types);

                        let param_type = if let Some(ast_type) = &substituted_type {
                            ast_type
                        } else {
                            &param.ast_type
                        };
                        debug_i!("real expression : {expr}");

                        debug_i!("real type of expression : {param_type}");

                        let (t, e) = self
                            .get_filter(
                                module,
                                val_context,
                                statics,
                                &mut resolved_generic_types,
                                expr,
                                Some(param_type),
                                namespace,
                                inside_function,
                                &mut inner_new_functions,
                                strict,
                            )?/*
                            .map_err(|it| {
                                it.add(
                                    expr.get_index(),
                                    format!(
                                        "getting filter from expression {expr} for {param_type}"
                                    ),
                                    self.stack.clone(),
                                )
                            })?*/;
                        if resolved_count != resolved_generic_types.len() {
                            something_resolved = true;
                        }
                        debug_i!("filter {t}");
                        if !t.almost_equal(param_type, module)? {
                            valid = false;
                            return Err(EnhTypeCheckError::new_with_kind(
                                expr.get_index().unwrap_or(&EnhASTIndex::none()).clone(),
                                format!("not matching type expected {t} got {param_type}"),
                                self.stack.clone(),
                                EnhTypeCheckErrorKind::Ignorable,
                            ));
                        }
                        Ok(e)
                    })
                    .filter(|it| !it.as_ref().is_err_and(|e| e.is_dummy()))
                    .collect();
                dedent!();
            }

            let result = result
                .into_iter()
                .collect::<Result<Vec<_>, EnhTypeCheckError>>();

            if let Err(e) = result {
                debug_i!("ignored function due to {e}");

                let error = if matches!(e.kind, EnhTypeCheckErrorKind::Ignorable) {
                    EnhTypeCheckError::new_with_kind(
                        call.index.clone(),
                        format!(
                            "ignoring function {function} {} : {}",
                            resolved_generic_types, function.index
                        ),
                        self.stack.clone(),
                        EnhTypeCheckErrorKind::Ignorable,
                    )
                    .add_errors(vec![e])
                } else {
                    EnhTypeCheckError::new(
                        call.index.clone(),
                        format!("ignoring function {function} : {}", function.index),
                        self.stack.clone(),
                    )
                    .add_errors(vec![e])
                };

                errors.push(error);
                dedent!();
                continue;
            }

            if valid {
                if let Some(rt) = expected_return_type {
                    //let rt = substitute(rt, &resolved_generic_types).unwrap_or(rt.clone());
                    valid = valid
                        && EnhTypeFilter::Exact(rt.clone())
                            .almost_equal(&function.return_type, module)
                            .unwrap_or(false);
                }
            }

            if valid {
                if let Some((old_function, _, _, _)) = valid_functions.get(0) {
                    if old_function.rank == function.rank {
                        dedent!();
                        let error = EnhTypeCheckError::new(
                            call.index.clone(),
                            format!("ignoring function {function} : {} because it has the same ranking of {old_function} : {}", function.index, old_function.index),
                            self.stack.clone(),
                        );
                        //self.stack.pop();
                        return Err(error);
                    }

                    // since function are already sorted by rank, we don't need to look at the other functions
                    dedent!();
                    break;
                } else {
                    ok_inner_new_functions = inner_new_functions;

                    valid_functions.push((
                        function.clone(),
                        function.rank,
                        resolved_generic_types,
                        result.unwrap(),
                    ));
                    debug_i!("it's valid with rank {}", function.rank);
                }
            }
            dedent!();
        }

        if valid_functions.is_empty() {
            dedent!();
            let result = Err(EnhTypeCheckError::new(
                call.index.clone(),
                Self::invalid_function_message(namespace, first_type, call, expected_return_type),
                self.stack.clone(),
            )
            .add_errors(errors));
            //self.stack.pop();
            result
        } else if valid_functions.len() > 1 {
            // we must disambiguate, but it should not happen because we break when we found a valid function.
            // TODO we don't consider when two functions have the same coefficient...
            // We get the function that has the minimal rank, if there's only one with that coefficient.
            panic!("There are more functions for call {call} : {}", call.index);
            let min = valid_functions.iter().map(|it| it.1).min().unwrap();

            let mut dis_valid_functions = valid_functions
                .into_iter()
                .filter(|it| it.1 == min)
                .collect::<Vec<_>>();

            if dis_valid_functions.is_empty() {
                //self.stack.pop();
                // I think it should not happen
                dedent!();
                return Err(EnhTypeCheckError::new(
                    call.index.clone(),
                    format!("call {call} cannot find a valid function",),
                    self.stack.clone(),
                ));
            } else if dis_valid_functions.len() > 1 {
                //self.stack.pop();
                dedent!();
                return Err(EnhTypeCheckError::new(
                    call.index.clone(),
                    format!(
                        "call {call}\nfound more than one valid function {}",
                        SliceDisplay(
                            &dis_valid_functions
                                .iter()
                                .map(|it| &it.0)
                                .collect::<Vec<_>>()
                        )
                    ),
                    self.stack.clone(),
                ));
            }
            //self.stack.pop();
            dedent!();
            let (valid_function, _x, resolved_generic_types, expressions) =
                dis_valid_functions.remove(0);
            Ok((valid_function, resolved_generic_types, expressions))
        } else {
            new_functions.append(&mut ok_inner_new_functions);
            //self.stack.pop();
            dedent!();
            let (valid_function, _x, resolved_generic_types, expressions) =
                valid_functions.remove(0);
            Ok((valid_function, resolved_generic_types, expressions))
        }
    }

    fn invalid_function_message(
        namespace: &EnhASTNameSpace,
        first_type: Option<EnhASTType>,
        call: &EnhASTFunctionCall,
        expected_return_type: Option<&EnhASTType>,
    ) -> String {
        let first_type = first_type
            .map(|it| format!("{it}"))
            .unwrap_or(String::new());

        let mut message = format!(
            "cannot find a valid function from namespace {namespace} for call {}({first_type} ...)",
            call.original_function_name
        );

        if let Some(er) = expected_return_type {
            message.push_str(&format!(" -> {er}"));
        }
        message
    }

    fn get_first_type(
        call: &EnhASTFunctionCall,
        val_context: &EnhValContext,
    ) -> Option<EnhASTType> {
        if call.parameters.len() > 0 {
            if let Some(p) = call.parameters.get(0) {
                match p {
                    EnhASTExpression::ValueRef(name, _, _) => {
                        if let Some(t) = val_context.get(name) {
                            match t {
                                EnhValKind::ParameterRef(_, par) => Some(par.ast_type.clone()),
                                EnhValKind::LetRef(_, t, _) => Some(t.clone()),
                            }
                        } else {
                            None
                        }
                    }
                    EnhASTExpression::Value(vt, _) => Some(value_type_to_enh_type(vt)),
                    EnhASTExpression::Any(t) => Some(t.clone()),
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    ///
    /// lower means a better precedence
    ///
    pub fn function_precedence_coeff(function: &EnhASTFunctionDef) -> usize {
        let generic_coeff: usize = function
            .parameters
            .iter()
            .map(|it| Self::generic_type_coeff(&it.ast_type))
            .sum();

        generic_coeff
            + if matches!(function.body, EnhASTFunctionBody::NativeBody(_)) {
                0usize
            } else {
                1usize
            }
    }

    fn get_filter(
        &mut self,
        module: &InputModule,
        val_context: &EnhValContext,
        statics: &mut Statics,
        resolved_generic_types: &mut EnhResolvedGenericTypes,
        expr: &EnhASTExpression,
        param_type: Option<&EnhASTType>,
        namespace: &EnhASTNameSpace,
        inside_function: Option<&EnhASTFunctionDef>,
        new_functions: &mut Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
        strict: bool,
    ) -> Result<(EnhTypeFilter, EnhASTExpression), EnhTypeCheckError> {
        let e = self.transform_expression(
            module,
            expr,
            val_context,
            statics,
            param_type,
            namespace,
            inside_function,
            new_functions,
            strict,
        )?;

        let t = self.type_of_expression(
            module,
            &e,
            val_context,
            statics,
            param_type,
            namespace,
            new_functions,
            strict,
        )?;
        if let EnhTypeFilter::Exact(et) = &t {
            if !et.is_generic() {
                if let Some(pt) = param_type {
                    resolved_generic_types
                        .extend(resolve_generic_types_from_effective_type(pt, et, module)?)
                        .map_err(|it| {
                            EnhTypeCheckError::new(
                                expr.get_index().unwrap_or(&EnhASTIndex::none()).clone(),
                                format!("cannot resolve {pt} with {et}, {it}"),
                                self.stack.clone(),
                            )
                        })?;
                }
            }
        } else if let EnhTypeFilter::Lambda(_, Some(lrt)) = &t {
            if let EnhTypeFilter::Exact(ref et) = lrt.deref() {
                if !et.is_generic() {
                    if let Some(EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                        parameters: _,
                        return_type,
                    })) = param_type
                    {
                        resolved_generic_types
                            .extend(resolve_generic_types_from_effective_type(
                                return_type.deref(),
                                et,
                                module,
                            )?)
                            .map_err(|it| {
                                EnhTypeCheckError::new(
                                    expr.get_index().unwrap_or(&EnhASTIndex::none()).clone(),
                                    format!("cannot resolve {return_type} with {et}, {it}"),
                                    self.stack.clone(),
                                )
                            })?;
                    }
                }
            }
        }
        Ok((t, e))
    }

    ///
    /// return a coefficient that is higher for how the type is generic
    ///
    pub fn generic_type_coeff(ast_type: &EnhASTType) -> usize {
        Self::generic_type_coeff_internal(ast_type, usize::MAX / 100)
    }

    fn generic_type_coeff_internal(ast_type: &EnhASTType, coeff: usize) -> usize {
        if ast_type.is_generic() {
            match ast_type {
                EnhASTType::Builtin(_) => 0,
                EnhASTType::Generic(_, _, _) => coeff,
                EnhASTType::Custom {
                    namespace: _,
                    name: _,
                    param_types,
                    index: _,
                } => param_types
                    .iter()
                    .map(|it| Self::generic_type_coeff_internal(it, coeff / 100))
                    .sum(),
                EnhASTType::Unit => 0,
            }
        } else {
            0
        }
    }

    fn transform_function(
        &mut self,
        module: &InputModule,
        statics: &mut Statics,
        new_function_def: &EnhASTFunctionDef,
        new_functions: &mut Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
    ) -> Result<Option<EnhASTFunctionBody>, EnhTypeCheckError> {
        debug_i!("transform_function {new_function_def}");
        debug_i!(
            "generic_types {}",
            SliceDisplay(&new_function_def.generic_types)
        );
        debug_i!(
            "resolved generic_types {}",
            new_function_def.resolved_generic_types
        );
        indent!();
        let mut val_context = EnhValContext::new(None);

        for parameter in new_function_def.parameters.iter() {
            val_context
                .insert_par(parameter.name.clone(), parameter.clone())
                .map_err(|e| {
                    EnhTypeCheckError::new(
                        parameter.ast_index.clone(),
                        e.clone(),
                        self.stack.clone(),
                    )
                })?;
        }

        let new_body = match &new_function_def.body {
            EnhASTFunctionBody::RASMBody(statements) => {
                let new_statements = self.transform_statements(
                    module,
                    statements,
                    &mut val_context,
                    statics,
                    Some(&new_function_def.return_type),
                    &new_function_def.namespace,
                    Some(&new_function_def),
                    new_functions,
                    true,
                )?;
                Some(EnhASTFunctionBody::RASMBody(new_statements))
            }
            EnhASTFunctionBody::NativeBody(native_body) => {
                let type_def_provider = DummyTypeDefProvider::new();

                let evaluator = self.target.get_evaluator(self.debug);
                let text_macro_names = evaluator
                    .get_macros(
                        None,
                        Some(new_function_def),
                        native_body,
                        &type_def_provider,
                    )
                    .map_err(|it| {
                        dedent!();
                        EnhTypeCheckError::new(
                            new_function_def.index.clone(),
                            format!("Error getting macros for {new_function_def}, {it}"),
                            self.stack.clone(),
                        )
                    })?
                    .iter()
                    .map(|(m, _i)| m.name.clone())
                    .collect::<LinkedHashSet<_>>();

                for text_macro_name in text_macro_names {
                    let default_function_calls = evaluator
                        .default_function_calls(&text_macro_name)
                        .map_err(|it| {
                            dedent!();
                            EnhTypeCheckError::new(
                                new_function_def.index.clone(),
                                format!("Error getting macros for {new_function_def}, {it}"),
                                self.stack.clone(),
                            )
                        })?;
                    for f in default_function_calls {
                        let call = f.to_call(new_function_def);
                        let _new_call = self
                            .transform_call(
                                module,
                                &call,
                                &mut val_context,
                                statics,
                                None,
                                &new_function_def.namespace,
                                Some(new_function_def),
                                new_functions,
                                true,
                            )
                            .map_err(|it| {
                                dedent!();
                                it.clone()
                            })?;
                    }
                }

                let called_functions = self
                    .target
                    .called_functions(
                        None,
                        Some(new_function_def),
                        native_body,
                        &val_context,
                        &type_def_provider,
                        statics,
                        self.debug,
                    )
                    .map_err(|it| {
                        dedent!();
                        EnhTypeCheckError::new(
                            new_function_def.index.clone(),
                            format!(
                                "Error determining function calls for {new_function_def}, {it}"
                            ),
                            self.stack.clone(),
                        )
                    })?;

                if called_functions.is_empty() {
                    None
                } else {
                    let mut lines: Vec<String> = native_body
                        .lines()
                        .map(|it| it.to_owned())
                        .collect::<Vec<_>>();

                    for (_m, f) in called_functions.iter() {
                        let call = f.to_call(new_function_def);
                        let new_call = self
                            .transform_call(
                                module,
                                &call,
                                &mut val_context,
                                statics,
                                None,
                                &new_function_def.namespace,
                                Some(new_function_def),
                                new_functions,
                                true,
                            )
                            .map_err(|it| {
                                dedent!();
                                it.clone()
                            })?;
                        debug_i!("old line {}", lines[f.i]);

                        let new_line =
                            lines[f.i].replace(&call.function_name, &new_call.function_name);
                        debug_i!("new line {}", new_line);
                        lines[f.i] = new_line;
                    }

                    Some(EnhASTFunctionBody::NativeBody(lines.join("\n")))
                }
            }
        };
        dedent!();
        Ok(new_body)
    }

    fn transform_statements(
        &mut self,
        module: &InputModule,
        statements: &[EnhASTStatement],
        val_context: &mut EnhValContext,
        statics: &mut Statics,
        expected_return_type: Option<&EnhASTType>,
        namespace: &EnhASTNameSpace,
        inside_function: Option<&EnhASTFunctionDef>,
        new_functions: &mut Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
        strict: bool,
    ) -> Result<Vec<EnhASTStatement>, EnhTypeCheckError> {
        debug_i!(
            "transform_statements expected_return_type {}",
            OptionDisplay(&expected_return_type)
        );
        indent!();
        let result = statements
            .iter()
            .enumerate()
            .map(|(i, it)| {
                let er = if i == statements.len() - 1 {
                    expected_return_type
                } else {
                    None
                };
                let new_statement = self.transform_statement(
                    module,
                    it,
                    val_context,
                    statics,
                    er,
                    namespace,
                    inside_function,
                    new_functions,
                    strict,
                );

                if let Ok(EnhASTStatement::LetStatement(name, expr, index)) = &new_statement {
                    let type_of_expr = self.type_of_expression(
                        module,
                        expr,
                        val_context,
                        statics,
                        None,
                        namespace,
                        new_functions,
                        strict,
                    )?;

                    if let EnhTypeFilter::Exact(ast_type) = type_of_expr {
                        val_context
                            .insert_let(name.clone(), ast_type, index)
                            .map_err(|it| {
                                EnhTypeCheckError::new(index.clone(), it, self.stack.clone())
                            })?;
                    } else {
                        return Err(EnhTypeCheckError::new(
                            index.clone(),
                            format!("Cannot determine type of {expr}, type_of_epr {type_of_expr}"),
                            self.stack.clone(),
                        ));
                    }
                } else if let Ok(EnhASTStatement::ConstStatement(
                    name,
                    expr,
                    index,
                    namespace,
                    modifiers,
                )) = &new_statement
                {
                    let type_of_expr = self.type_of_expression(
                        module,
                        expr,
                        val_context,
                        statics,
                        None,
                        namespace,
                        new_functions,
                        strict,
                    )?;

                    if let EnhTypeFilter::Exact(ast_type) = type_of_expr {
                        statics.add_const(name.clone(), ast_type, namespace, modifiers);
                    } else {
                        return Err(EnhTypeCheckError::new(
                            index.clone(),
                            format!("Cannot determine type of {expr}, type_of_epr {type_of_expr}"),
                            self.stack.clone(),
                        ));
                    }
                }

                new_statement
            })
            .collect::<Result<Vec<_>, EnhTypeCheckError>>();
        dedent!();
        result.map_err(|it| {
            it.add(
                EnhASTIndex::none(),
                format!(
                    "transforming expressions, expected_return_type {}",
                    OptionDisplay(&expected_return_type)
                ),
                self.stack.clone(),
            )
        })
    }

    fn enh_filter_from_ast(
        &self,
        filter: &ASTTypeFilter,
        namespace: &EnhASTNameSpace,
    ) -> EnhTypeFilter {
        match filter {
            ASTTypeFilter::Exact(ast_type, module_info) => EnhTypeFilter::Exact(self.enh_ast_type(
                ast_type.clone(),
                module_info,
                namespace,
                None,
            )),
            ASTTypeFilter::Any => EnhTypeFilter::Any,
            ASTTypeFilter::Lambda(s, asttype_filter) => EnhTypeFilter::Lambda(
                *s,
                asttype_filter
                    .clone()
                    .map(|it| Box::new(self.enh_filter_from_ast(&it, namespace))),
            ),
        }
    }

    fn enh_ast_type(
        &self,
        ast_type: ASTType,
        module_info: &ModuleInfo,
        namespace: &EnhASTNameSpace,
        function_name_for_fix_generics: Option<&str>,
    ) -> EnhASTType {
        /*let ast_type = if ast_type.is_generic() {
            ast_type.remove_generic_prefix()
        } else {
            ast_type
        };
        */

        let (filter_module_id, filter_module_namespace) = {
            if let ASTType::Custom {
                name,
                param_types,
                position,
            } = ast_type
            {
                let (filter_module_id, filter_module_namespace) =
                    if let Some(ast_namespace) = self.modules_catalog.namespace(namespace) {
                        if let Some(ct_module_id) = self
                            .modules_container
                            .custom_type_module_id(ast_namespace, &name)
                        {
                            if let Some((id, namespace)) =
                                self.modules_catalog.catalog_info(ct_module_id)
                            {
                                (id, namespace)
                            } else {
                                self.modules_catalog.catalog_info(module_info.id()).unwrap()
                            }
                        } else {
                            self.modules_catalog.catalog_info(module_info.id()).unwrap()
                        }
                    } else {
                        self.modules_catalog.catalog_info(module_info.id()).unwrap()
                    };

                let param_types = param_types
                    .into_iter()
                    .map(|it| {
                        self.enh_ast_type(
                            it,
                            module_info,
                            namespace,
                            function_name_for_fix_generics,
                        )
                    })
                    .collect();

                /*
                if name == "ASTType" && &filter_module_namespace.safe_name() == "stdlib_vec" {
                    println!("ASTType error namespace {}", module_info.namespace());
                }
                */

                return EnhASTType::Custom {
                    namespace: filter_module_namespace.clone(),
                    name: name.clone(),
                    param_types,
                    index: EnhASTIndex {
                        file_name: filter_module_id.path(),
                        row: position.row,
                        column: position.column,
                        builtin: position.clone().builtin,
                    },
                };
            } else if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            }) = ast_type
            {
                let parameters = parameters
                    .into_iter()
                    .map(|it| {
                        self.enh_ast_type(
                            it,
                            module_info,
                            namespace,
                            function_name_for_fix_generics,
                        )
                    })
                    .collect();

                let return_type = self.enh_ast_type(
                    *return_type,
                    module_info,
                    namespace,
                    function_name_for_fix_generics,
                );

                return EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type: Box::new(return_type),
                });
            } else {
                self.modules_catalog.catalog_info(module_info.id()).unwrap()
            }
        };

        /*
        if format!("{ast_type}").contains("Option<IOError>") {
            println!("ast_type {ast_type}");
            println!("    module_info {module_info}");
            println!("    filter_module_namespace {filter_module_namespace}");
        }
        */

        EnhASTType::from_ast(
            filter_module_namespace,
            filter_module_id,
            ast_type,
            function_name_for_fix_generics,
        )
    }

    fn get_type_check_entry(
        &self,
        enh_index: &EnhASTIndex,
        namespace: &EnhASTNameSpace,
    ) -> Option<&ASTTypeCheckEntry> {
        if let Some(ref path) = enh_index.file_name {
            let info = EnhModuleInfo::new(EnhModuleId::Path(path.clone()), namespace.clone());
            let index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(enh_index.row, enh_index.column),
            );
            return self.type_checker.result.get(&index);
        }
        None
    }

    pub fn type_of_expression(
        &mut self,
        module: &InputModule,
        typed_expression: &EnhASTExpression,
        val_context: &EnhValContext,
        statics: &mut Statics,
        expected_type: Option<&EnhASTType>,
        namespace: &EnhASTNameSpace,
        new_functions: &mut Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
        strict: bool,
    ) -> Result<EnhTypeFilter, EnhTypeCheckError> {
        debug_i!(
            "type_of_expression {typed_expression} expected type {}",
            OptionDisplay(&expected_type)
        );
        indent!();

        if namespace.safe_name().contains("reference") {
            if let EnhASTExpression::ValueRef(name, _, _) = typed_expression {
                if name == "add" {
                    println!(
                        "type_of_expression {typed_expression} expected type {}",
                        OptionDisplay(&expected_type)
                    );
                }
            }
        }

        if let Some(enh_index) = typed_expression.get_index() {
            if let Some(t) = self.get_type_check_entry(enh_index, namespace) {
                if let Some(f) = t.filter() {
                    if !f.is_generic() {
                        let filter = self.enh_filter_from_ast(f, namespace);
                        dedent!();
                        return Ok(filter);
                    }
                }
            }
        }

        let result = match typed_expression {
            EnhASTExpression::ASTFunctionCallExpression(call) => {
                if val_context.is_lambda(&call.function_name) {
                    if let Some(v) = val_context.get(&call.function_name) {
                        let lambda = match v {
                            EnhValKind::ParameterRef(_, p) => p.ast_type.clone(),
                            EnhValKind::LetRef(_, t, _index) => t.clone(),
                        };

                        if let EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                            parameters: _,
                            return_type,
                        }) = lambda
                        {
                            dedent!();
                            return Ok(EnhTypeFilter::Exact(return_type.deref().clone()));
                        } else {
                            dedent!();
                            return Err(EnhTypeCheckError::new(
                                call.index.clone(),
                                "It should not happen!!!".to_string(),
                                self.stack.clone(),
                            ));
                        }
                    };
                }

                if let Some(f) = self
                    .new_functions
                    .get(&call.function_name)
                    .or(self.new_functions.get(&call.original_function_name))
                {
                    dedent!();
                    return Ok(EnhTypeFilter::Exact(f.return_type.clone()));
                } else {
                    if let Some((f, _)) = new_functions
                        .iter()
                        .find(|(f, _)| f.name == call.function_name)
                    {
                        dedent!();
                        return Ok(EnhTypeFilter::Exact(f.return_type.clone()));
                    }
                }

                match self.get_valid_function(
                    module,
                    call,
                    val_context,
                    statics,
                    expected_type,
                    namespace,
                    None,
                    new_functions,
                    strict,
                ) {
                    Ok((found_function, resolved_generic_types, _)) => {
                        let return_ast_type = if let Some(new_t) =
                            substitute(&found_function.return_type, &resolved_generic_types)
                        {
                            new_t
                        } else {
                            found_function.return_type
                        };

                        EnhTypeFilter::Exact(return_ast_type)
                    }
                    Err(e) => {
                        debug_i!("{e}");
                        EnhTypeFilter::Any
                    }
                }
            }
            EnhASTExpression::ValueRef(name, index, const_namespace) => match val_context.get(name)
            {
                None => {
                    if let Some(c) = statics.get_const(name, const_namespace) {
                        EnhTypeFilter::Exact(c.ast_type.clone())
                    } else {
                        //if name == "stdlib_i32_eq_i32_i32_bool" || name == "breakout_breakout_update_stdlib_option_Option_sdl_sdl_KeyEvent__breakout_breakout_State__breakout_breakout_State" {
                        // println!("stdlib_i32_eq_i32_i32_bool");
                        //for (f, _) in new_functions.iter() {
                        //    println!("function name {} {}", f.name, f.original_name);
                        //}
                        //}
                        let mut function_references = self.functions_referenced_by_name(
                            module,
                            expected_type,
                            name,
                            &new_functions,
                            namespace,
                            index,
                        );

                        if function_references.len() == 1 {
                            let new_function_def = function_references.remove(0);

                            let lambda = EnhBuiltinTypeKind::Lambda {
                                parameters: new_function_def
                                    .parameters
                                    .iter()
                                    .map(|it| it.ast_type.clone())
                                    .collect(),
                                return_type: Box::new(new_function_def.return_type.clone()),
                            };

                            //new_functions.push((new_function_def, self.stack.clone()));

                            EnhTypeFilter::Exact(EnhASTType::Builtin(lambda))
                        } else {
                            dedent!();
                            let message = if function_references.is_empty() {
                                format!("Cannot find reference to {name} typing expression {typed_expression}")
                            } else {
                                format!("More than one reference to {name} typing expression {typed_expression}")
                            };
                            return Err(EnhTypeCheckError::new_with_kind(
                                index.clone(),
                                message,
                                self.stack.clone(),
                                EnhTypeCheckErrorKind::Important,
                            ));
                        }
                    }
                }
                Some(EnhValKind::LetRef(_, t, _index)) => EnhTypeFilter::Exact(t.clone()),
                Some(EnhValKind::ParameterRef(_, par)) => {
                    // TODO I must convert the type
                    EnhTypeFilter::Exact(par.ast_type.clone())
                }
            },
            EnhASTExpression::Value(value_type, _) => {
                EnhTypeFilter::Exact(value_type_to_enh_type(value_type))
            }
            EnhASTExpression::Lambda(def) => {
                if def.body.is_empty() {
                    dedent!();
                    return Ok(EnhTypeFilter::Lambda(
                        def.parameter_names.len(),
                        Some(Box::new(EnhTypeFilter::Exact(EnhASTType::Unit))),
                    ));
                }
                // I cannot go deep in determining the type
                if expected_type.is_none() {
                    dedent!();
                    return Ok(EnhTypeFilter::Lambda(def.parameter_names.len(), None));
                }

                let mut return_type = Some(Box::new(EnhTypeFilter::Exact(EnhASTType::Unit)));
                let mut lambda_val_context = EnhValContext::new(Some(val_context));

                self.add_lambda_parameters_to_val_context(
                    def,
                    &expected_type,
                    &mut lambda_val_context,
                )?;

                for (i, statement) in def.body.iter().enumerate() {
                    if let EnhASTStatement::LetStatement(name, expr, index) = statement {
                        let type_of_expr = self.type_of_expression(
                            module,
                            expr,
                            &mut lambda_val_context,
                            statics,
                            None,
                            namespace,
                            new_functions,
                            strict,
                        )?;

                        if let EnhTypeFilter::Exact(ast_type) = type_of_expr {
                            lambda_val_context
                                .insert_let(name.clone(), ast_type, index)
                                .map_err(|it| {
                                    EnhTypeCheckError::new(index.clone(), it, self.stack.clone())
                                })?;
                        } else if let EnhASTStatement::ConstStatement(_, _, _, _, _) = statement {
                            return Err(EnhTypeCheckError::new(
                                index.clone(),
                                format!("Const not allowed here {expr}"),
                                self.stack.clone(),
                            ));
                        } else {
                            dedent!();
                            return Ok(EnhTypeFilter::Lambda(def.parameter_names.len(), None));
                        }
                    }

                    if i == def.body.len() - 1 {
                        if let EnhASTStatement::Expression(last) = statement {
                            return_type = Some(Box::new(self.type_of_expression(
                                module,
                                last,
                                &mut lambda_val_context,
                                statics,
                                None,
                                namespace,
                                new_functions,
                                strict,
                            )?));
                        }
                    }
                }
                debug_i!("lambda return type {}", OptionDisplay(&return_type));
                if def.parameter_names.is_empty() {
                    if let Some(EnhTypeFilter::Exact(exact_return_type)) = return_type.as_deref() {
                        EnhTypeFilter::Exact(EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                            parameters: Vec::new(),
                            return_type: Box::new(exact_return_type.clone()),
                        }))
                    } else {
                        EnhTypeFilter::Lambda(def.parameter_names.len(), return_type)
                    }
                } else {
                    EnhTypeFilter::Lambda(def.parameter_names.len(), return_type)
                }
            }
            EnhASTExpression::Any(t) => EnhTypeFilter::Exact(t.clone()),
        };

        debug_i!("found type {result}");
        dedent!();

        /*
        if let Some(ftc) = found_in_type_check {
            if let EnhTypeFilter::Exact(ft) = &ftc {
                if let EnhTypeFilter::Exact(t) = &result {
                    if ft != t {
                        println!("different exact {ft:?} {t:?}");
                    }
                } else {
                    // println!("not found in result {ft}");
                }
            }
        }
        */

        Ok(result)
    }

    fn transform_lambda_def(
        &mut self,
        module: &InputModule,
        lambda_def: &EnhASTLambdaDef,
        val_context: &EnhValContext,
        statics: &mut Statics,
        expected_type: Option<&EnhASTType>,
        namespace: &EnhASTNameSpace,
        inside_function: Option<&EnhASTFunctionDef>,
        new_functions: &mut Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
        strict: bool,
    ) -> Result<EnhASTLambdaDef, EnhTypeCheckError> {
        let mut new_lambda = lambda_def.clone();

        let mut val_context = EnhValContext::new(Some(val_context));

        self.add_lambda_parameters_to_val_context(lambda_def, &expected_type, &mut val_context)?;

        let ert = if let Some(et) = expected_type {
            if let EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                parameters: _,
                return_type,
            }) = et
            {
                Ok(Some(return_type.deref()))
            } else if let EnhASTType::Generic(_, _, _) = et {
                Ok(None)
            } else {
                Err(EnhTypeCheckError::new(
                    lambda_def.index.clone(),
                    format!("Expected lambda but got {et}"),
                    self.stack.clone(),
                ))
            }
        } else {
            Ok(None)
        };

        new_lambda.body = self.transform_statements(
            module,
            &lambda_def.body,
            &mut val_context,
            statics,
            ert?,
            namespace,
            inside_function,
            new_functions,
            strict,
        )?;

        Ok(new_lambda)
    }

    fn add_lambda_parameters_to_val_context(
        &self,
        lambda_def: &EnhASTLambdaDef,
        expected_type: &Option<&EnhASTType>,
        val_context: &mut EnhValContext,
    ) -> Result<(), EnhTypeCheckError> {
        if !lambda_def.parameter_names.is_empty() {
            if let Some(EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                parameters,
                return_type: _,
            })) = expected_type
            {
                for ((name, index), t) in zip(lambda_def.parameter_names.iter(), parameters.iter())
                {
                    val_context
                        .insert_par(
                            name.to_owned(),
                            EnhASTParameterDef {
                                name: name.to_owned(),
                                ast_type: t.clone(),
                                ast_index: index.clone(),
                            },
                        )
                        .map_err(|e| {
                            EnhTypeCheckError::new(index.clone(), e.clone(), self.stack.clone())
                        })?;
                }
            } else {
                for (i, (name, index)) in lambda_def.parameter_names.iter().enumerate() {
                    val_context
                        .insert_par(
                            name.to_owned(),
                            EnhASTParameterDef {
                                name: name.to_owned(),
                                ast_type: EnhASTType::Generic(
                                    EnhASTIndex::none(),
                                    format!("L_{i}"),
                                    Vec::new(), // TODO type classes
                                ),
                                ast_index: index.clone(),
                            },
                        )
                        .map_err(|e| {
                            EnhTypeCheckError::new(index.clone(), e.clone(), self.stack.clone())
                        })?;
                }
                /*
                return Err(TypeCheckError::new(
                    lambda_def.index.clone(),
                    format!("Expecting lambda but got {}", OptionDisplay(expected_type)),
                    self.stack.clone(),
                ));
                */
            }
        }
        Ok(())
    }

    fn functions_referenced_by_name(
        &self,
        module: &'a EnhancedASTModule,
        expected_type: Option<&EnhASTType>,
        name: &str,
        new_functions: &'a Vec<(EnhASTFunctionDef, Vec<EnhASTIndex>)>,
        namespace: &EnhASTNameSpace,
        index: &EnhASTIndex,
    ) -> Vec<&'a EnhASTFunctionDef> {
        let mut function_references = module
            .functions()
            .iter()
            .cloned()
            .filter(|it| &it.name == name && (&it.namespace == namespace || it.modifiers.public))
            .collect::<Vec<_>>();

        function_references.extend(
            new_functions
                .iter()
                .map(|it| &it.0)
                .filter(|it| &it.name == name)
                .collect::<Vec<_>>(),
        );

        if let Some(t) = self.get_type_check_entry(index, namespace) {
            if let ASTTypeCheckInfo::Ref(_, ref_index) = t.info() {
                let (eh_id, _) = self
                    .modules_catalog
                    .catalog_info(ref_index.module_id())
                    .unwrap();

                function_references = function_references
                    .into_iter()
                    .filter(|it| {
                        &it.index.id() == eh_id
                            && it.index.row == ref_index.position().row
                            && it.index.column == ref_index.position().column
                    })
                    .collect::<Vec<_>>();

                if function_references.len() == 1 {
                    return function_references;
                }
            }
        }

        if let Some(EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
            parameters,
            return_type,
        })) = expected_type
        {
            function_references = function_references
                .iter()
                .cloned()
                .filter(|it| it.parameters.len() == parameters.len())
                .filter(|it| {
                    zip(it.parameters.iter(), parameters).all(|(a, b)| {
                        return EnhTypeFilter::Exact(b.clone())
                            .almost_equal(&a.ast_type, module)
                            .unwrap_or(false);
                    })
                })
                .collect::<Vec<_>>();
        }
        function_references
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Write;
    use std::path::PathBuf;

    use env_logger::Builder;

    use crate::codegen::asm::code_gen_asm::AsmOptions;
    use crate::codegen::c::options::COptions;
    use crate::codegen::compile_target::CompileTarget;
    use crate::codegen::enh_ast::{
        EnhASTFunctionBody, EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTParameterDef,
        EnhASTType, EnhBuiltinTypeKind,
    };
    use crate::codegen::statics::Statics;

    use crate::enh_type_check::enh_resolved_generic_types::EnhResolvedGenericTypes;
    use crate::enh_type_check::enh_type_check::EnhTypeCheck;
    use crate::enh_type_check::typed_ast::ASTTypedModule;
    use crate::errors::CompilationError;
    use crate::project::RasmProject;
    use crate::test_utils::project_to_ast_typed_module;
    use rasm_parser::parser::ast::ASTModifiers;

    #[test]
    pub fn fibonacci() {
        let project = file_to_project("fibonacci.rasm");
        test_project(project).unwrap();
    }

    #[test]
    pub fn breakout() {
        let project = dir_to_project("../rasm/resources/examples/breakout");
        test_project(project).unwrap();
    }

    #[test]
    pub fn let1() {
        let project = dir_to_project("../rasm/resources/test/let1.rasm");
        test_project(project).unwrap();
    }

    #[test]
    pub fn function_reference() {
        let project = dir_to_project("resources/test/ast_type_checker/function_reference.rasm");
        test_project(project).unwrap();
    }

    #[test]
    pub fn gameoflife_tc() {
        let project = dir_to_project("../rasm/resources/examples/gameoflife_tc");
        test_project(project).unwrap();
    }

    #[test]
    fn test_type_check_vec() {
        let project = file_to_project("vec.rasm");
        test_project(project).unwrap();
    }

    #[test]
    pub fn test_generic_type_coeff() {
        assert_eq!(
            usize::MAX / 100,
            EnhTypeCheck::generic_type_coeff(&EnhASTType::Generic(
                EnhASTIndex::none(),
                "".to_owned(),
                Vec::new()
            ))
        );
    }

    #[test]
    pub fn test_generic_type_coeff_1() {
        assert_eq!(
            0,
            EnhTypeCheck::generic_type_coeff(&EnhASTType::Builtin(EnhBuiltinTypeKind::I32))
        );
    }

    #[test]
    pub fn test_generic_type_coeff_2() {
        assert_eq!(
            0,
            EnhTypeCheck::generic_type_coeff(&EnhASTType::Custom {
                namespace: EnhASTNameSpace::global(),
                param_types: vec![EnhASTType::Builtin(EnhBuiltinTypeKind::I32)],
                name: "".to_owned(),
                index: EnhASTIndex::none()
            },)
        );
    }

    #[test]
    pub fn test_generic_type_coeff_3() {
        assert_eq!(
            usize::MAX / 100 / 100,
            EnhTypeCheck::generic_type_coeff(&EnhASTType::Custom {
                namespace: EnhASTNameSpace::global(),
                param_types: vec![EnhASTType::Generic(
                    EnhASTIndex::none(),
                    "".to_owned(),
                    Vec::new()
                )],
                name: "".to_owned(),
                index: EnhASTIndex::none()
            },)
        );
    }

    #[test]
    fn test_generic_function_coeff() {
        // this is "more" generic
        let function1 = simple_function(
            vec![EnhASTType::Generic(
                EnhASTIndex::none(),
                "T".to_string(),
                Vec::new(),
            )],
            false,
        );

        let function2 = simple_function(
            vec![EnhASTType::Custom {
                namespace: EnhASTNameSpace::global(),
                name: "".to_string(),
                param_types: vec![EnhASTType::Generic(
                    EnhASTIndex::none(),
                    "T".to_string(),
                    Vec::new(),
                )],
                index: EnhASTIndex::none(),
            }],
            false,
        );

        let coeff1 = EnhTypeCheck::function_precedence_coeff(&function1);
        let coeff2 = EnhTypeCheck::function_precedence_coeff(&function2);

        assert!(coeff1 > coeff2)
    }

    #[test]
    fn test_generic_native_function_coeff() {
        // not native function have lower priority (higher coeff)
        let function1 = simple_function(
            vec![EnhASTType::Generic(
                EnhASTIndex::none(),
                "T".to_string(),
                Vec::new(),
            )],
            false,
        );
        let function2 = simple_function(
            vec![EnhASTType::Generic(
                EnhASTIndex::none(),
                "T".to_string(),
                Vec::new(),
            )],
            true,
        );

        let coeff1 = EnhTypeCheck::function_precedence_coeff(&function1);
        let coeff2 = EnhTypeCheck::function_precedence_coeff(&function2);

        assert!(coeff1 > coeff2)
    }
    fn simple_function(parameters: Vec<EnhASTType>, native: bool) -> EnhASTFunctionDef {
        EnhASTFunctionDef {
            original_name: "".to_string(),
            name: "".to_string(),
            parameters: parameters
                .iter()
                .map(|it| EnhASTParameterDef::new("", it.clone(), EnhASTIndex::none()))
                .collect(),
            return_type: EnhASTType::Unit,
            body: if native {
                EnhASTFunctionBody::NativeBody(String::new())
            } else {
                EnhASTFunctionBody::RASMBody(Vec::new())
            },
            inline: false,
            generic_types: vec![],
            resolved_generic_types: EnhResolvedGenericTypes::new(),
            index: EnhASTIndex::none(),
            modifiers: ASTModifiers::private(),
            namespace: EnhASTNameSpace::global(),
            rank: 0,
            target: None,
        }
    }

    /*
    TODO cannot work without a source file or folder
    #[test]
    pub fn test_add() {
        init();
        let mut check = TypeCheck::new();

        let mut module = EnhancedASTModule::new(
            vec![ASTModule::new(PathBuf::new())],
            PathBuf::from("resources"),
        );
        let function_def = ASTFunctionDef {
            original_name: "add".to_string(),
            name: "add".to_string(),
            parameters: vec![
                ASTParameterDef {
                    name: "v1".to_string(),
                    ast_type: ASTType::Builtin(BuiltinTypeKind::I32),
                    ast_index: ASTIndex::none(),
                },
                ASTParameterDef {
                    name: "v2".to_string(),
                    ast_type: ASTType::Builtin(BuiltinTypeKind::I32),
                    ast_index: ASTIndex::none(),
                },
            ],
            return_type: ASTType::Builtin(BuiltinTypeKind::I32),
            body: ASTFunctionBody::NativeBody("".to_owned()),
            inline: false,
            generic_types: vec![],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
        };
        module.add_function("add".to_owned(), function_def);

        let inner_call = ASTFunctionCall {
            original_function_name: "add".to_string(),
            function_name: "add".to_string(),
            parameters: vec![
                ASTExpression::Value(ValueType::I32(1), ASTIndex::none()),
                ASTExpression::Value(ValueType::I32(2), ASTIndex::none()),
            ],
            index: ASTIndex::none(),
        };

        let call = ASTFunctionCall {
            original_function_name: "add".to_string(),
            function_name: "add".to_string(),
            parameters: vec![
                ASTExpression::ASTFunctionCallExpression(inner_call.clone()),
                ASTExpression::ASTFunctionCallExpression(inner_call.clone()),
            ],
            index: ASTIndex::none(),
        };

        let mut val_context = ValContext::new(None);
        let mut statics = Statics::new();

        let transformed_call = check
            .transform_call(&module, &call, &mut val_context, &mut statics, None)
            .unwrap();

        assert_eq!(
            format!("{transformed_call}"),
            "add_0(add_0(1,2),add_0(1,2))"
        );

        assert_eq!(
            format!("{}", check.module.functions().get(0).unwrap()),
            "add_0(v1:i32,v2:i32) -> i32"
        );
    }
     */

    /*
    TODO cannot work without a source file or folder
    #[test]
    pub fn test_option_none() {
        init();

        let mut check = TypeCheck::new();

        let mut module = EnhancedASTModule::new(
            vec![ASTModule::new(PathBuf::new())],
            PathBuf::from("resources"),
        );
        let option_t = ASTType::Custom {
            name: "Option".to_owned(),
            param_types: vec![ASTType::Generic("T".to_owned())],
            index: ASTIndex::none(),
        };

        let function_def = ASTFunctionDef {
            original_name: "orElse".to_string(),
            name: "orElse".to_string(),
            parameters: vec![
                ASTParameterDef {
                    name: "v1".to_string(),
                    ast_type: option_t.clone(),
                    ast_index: ASTIndex::none(),
                },
                ASTParameterDef {
                    name: "v2".to_string(),
                    ast_type: ASTType::Builtin(BuiltinTypeKind::Lambda {
                        return_type: Box::new(option_t.clone()),
                        parameters: vec![],
                    }),
                    ast_index: ASTIndex::none(),
                },
            ],
            return_type: option_t.clone(),
            body: ASTFunctionBody::NativeBody("".to_owned()),
            inline: false,
            generic_types: vec!["T".to_owned()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
        };
        module.add_function("orElse".to_owned(), function_def);

        let function_def = ASTFunctionDef {
            original_name: "Option::None".to_string(),
            name: "Option::None".to_string(),
            parameters: vec![],
            return_type: option_t.clone(),
            body: ASTFunctionBody::NativeBody("".to_owned()),
            inline: false,
            generic_types: vec!["T".to_owned()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
        };
        module.add_function("Option::None".to_owned(), function_def);

        let function_def = ASTFunctionDef {
            original_name: "Option::Some".to_string(),
            name: "Option::Some".to_string(),
            parameters: vec![ASTParameterDef {
                name: "v1".to_string(),
                ast_type: ASTType::Generic("T".to_owned()),
                ast_index: ASTIndex::none(),
            }],
            return_type: option_t.clone(),
            body: ASTFunctionBody::NativeBody("".to_owned()),
            inline: false,
            generic_types: vec!["T".to_owned()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
        };
        module.add_function("Option::Some".to_owned(), function_def);

        let call_to_none = ASTFunctionCall {
            original_function_name: "Option::None".to_string(),
            function_name: "Option::None".to_string(),
            parameters: vec![],
            index: ASTIndex::none(),
        };

        let call_to_some = ASTFunctionCall {
            original_function_name: "Option::Some".to_string(),
            function_name: "Option::Some".to_string(),
            parameters: vec![ASTExpression::Value(ValueType::I32(10), ASTIndex::none())],
            index: ASTIndex::none(),
        };

        let call = ASTFunctionCall {
            original_function_name: "orElse".to_string(),
            function_name: "orElse".to_string(),
            parameters: vec![
                ASTExpression::ASTFunctionCallExpression(call_to_none.clone()),
                ASTExpression::Lambda(ASTLambdaDef {
                    body: vec![ASTStatement::Expression(
                        ASTExpression::ASTFunctionCallExpression(call_to_some),
                    )],
                    parameter_names: vec![],
                    index: ASTIndex::none(),
                }),
            ],
            index: ASTIndex::none(),
        };

        let mut val_context = ValContext::new(None);
        let mut statics = Statics::new();

        let transformed_call = check
            .transform_call(&module, &call, &mut val_context, &mut statics, None)
            .unwrap();

        assert_eq!(
            format!("{transformed_call}"),
            "orElse_0(Option_None_0(),{  -> Option_Some_0(10);\n; })"
        );
        assert_eq!(
            format!("{}", check.module.functions().get(0).unwrap()),
            "Option_Some_0(v1:i32) -> Option<i32>"
        );
    }

     */

    fn test_project(
        project: RasmProject,
    ) -> Result<(ASTTypedModule, Statics), Vec<CompilationError>> {
        project_to_ast_typed_module(&project, &CompileTarget::Nasmi386(AsmOptions::default()))?;
        project_to_ast_typed_module(&project, &CompileTarget::C(COptions::default()))
    }

    fn file_to_project(test_file: &str) -> RasmProject {
        init_log();
        env::set_var("RASM_STDLIB", "../stdlib");

        let current_path = env::current_dir().unwrap();

        let file_name = if current_path.ends_with("rasm/core") {
            PathBuf::from(&format!("../rasm/resources/test/{test_file}"))
        // for debugging in Codium
        } else if current_path.ends_with("rasm") {
            PathBuf::from(&format!("rasm/resources/test/{test_file}"))
        } else {
            panic!(
                "cannot handle current path: {}",
                current_path.to_str().unwrap()
            );
        };

        RasmProject::new(file_name)
    }

    fn dir_to_project(test_folder: &str) -> RasmProject {
        env::set_var("RASM_STDLIB", "../stdlib");

        init_log();
        let file_name = PathBuf::from(test_folder);

        RasmProject::new(file_name)
    }

    pub fn init_log() {
        Builder::from_default_env()
            .format(|buf, record| {
                writeln!(
                    buf,
                    "{} [{}] - {}",
                    chrono::Local::now().format("%Y-%m-%d %H:%M:%S.%3f"),
                    record.level(),
                    record.args()
                )
            })
            .try_init()
            .unwrap_or(());
    }
}
