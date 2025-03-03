use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    time::Instant,
};

use itertools::Itertools;
use linked_hash_map::LinkedHashMap;
use log::info;
use rasm_utils::{debug_i, dedent, indent, LinkedHashMapDisplay, OptionDisplay, SliceDisplay};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::codegen::val_context::ValContext;

use rasm_parser::{
    catalog::{ASTIndex, ModuleId, ModuleInfo, ModuleNamespace},
    parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTFunctionSignature,
        ASTModifiers, ASTParameterDef, ASTStatement, ASTType, BuiltinTypeKind,
    },
};

use super::ast_modules_container::{ASTFunctionSignatureEntry, ASTModulesContainer, ASTTypeFilter};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTResolvedGenericTypes {
    map: LinkedHashMap<String, ASTType>,
}

impl Display for ASTResolvedGenericTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", LinkedHashMapDisplay(&self.map))
    }
}

impl ASTResolvedGenericTypes {
    pub fn new() -> Self {
        Self {
            map: LinkedHashMap::new(),
        }
    }

    pub fn get(&self, key: &String) -> Option<&ASTType> {
        self.map.get(key)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn contains_key(&self, key: &String) -> bool {
        self.map.contains_key(key)
    }

    pub fn extend(&mut self, other: Self) -> Result<(), String> {
        for (key, new_type) in other.map.into_iter() {
            if let Some(prev_type) = self.get(&key) {
                if &new_type != prev_type {
                    if !prev_type.is_generic() {
                        debug_i!(
                            "Already resolved generic {key}, prev {prev_type}, new {new_type}"
                        );
                        return Err(format!(
                            "Already resolved generic {key}, prev {prev_type}, new {new_type}"
                        ));
                    }
                }
            }
            self.map.insert(key, new_type);
        }
        Ok(())
    }

    pub fn insert(&mut self, key: String, value: ASTType) -> Option<ASTType> {
        if let Some(t) = self.map.get(&key) {
            assert_eq!(t, &value);
        }
        self.map.insert(key, value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypeCheckErroKind {
    Error,
    Warning,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypeCheckError {
    kind: ASTTypeCheckErroKind,
    index: ASTIndex,
    message: String,
    inner: Vec<ASTTypeCheckError>,
}

impl ASTTypeCheckError {
    fn new(kind: ASTTypeCheckErroKind, index: ASTIndex, message: String) -> Self {
        Self {
            kind,
            index,
            message,
            inner: Vec::new(),
        }
    }

    fn add(self, kind: ASTTypeCheckErroKind, index: ASTIndex, message: String) -> Self {
        let mut result = self.clone();
        result
            .inner
            .push(ASTTypeCheckError::new(kind, index, message));
        result
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn index(&self) -> &ASTIndex {
        &self.index
    }

    pub fn kind(&self) -> &ASTTypeCheckErroKind {
        &self.kind
    }
}

impl Display for ASTTypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.message, self.index)
    }
}

#[derive(Debug, Clone)]
pub enum ASTTypeCheckInfo {
    Call(String, Vec<(ASTFunctionSignature, ASTIndex)>),
    LambdaCall(ASTFunctionSignature, ASTIndex),
    Ref(String, ASTIndex),
    Let(String, bool),
    Param(String),
    Value(usize), // the length of the token of the Value, for example gfor a string "s" it's 3
    Lambda,
}

impl Display for ASTTypeCheckInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypeCheckInfo::Call(name, vec) => {
                if vec.len() == 1 {
                    write!(f, "call to {}", vec.first().unwrap().0)?;
                } else {
                    write!(f, "call to {name} which can be one of \n")?;
                    for function_signature in vec {
                        write!(f, "{}\n", function_signature.0)?;
                    }
                }
                Result::Ok(())
            }
            ASTTypeCheckInfo::LambdaCall(function_signature, _) => {
                write!(f, "lambda call to {function_signature}")
            }
            ASTTypeCheckInfo::Ref(_, _) => f.write_str("ref"),
            ASTTypeCheckInfo::Value(_) => f.write_str("value"),
            ASTTypeCheckInfo::Let(_, _) => f.write_str("let"),
            ASTTypeCheckInfo::Param(_) => f.write_str("param"),
            ASTTypeCheckInfo::Lambda => f.write_str("lambda"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTTypeCheckEntry {
    filter: Option<ASTTypeFilter>,
    info: ASTTypeCheckInfo,
}

impl ASTTypeCheckEntry {
    fn new(filter: Option<ASTTypeFilter>, info: ASTTypeCheckInfo) -> Self {
        Self { filter, info }
    }

    pub fn filter(&self) -> &Option<ASTTypeFilter> {
        &self.filter
    }

    pub fn info(&self) -> &ASTTypeCheckInfo {
        &self.info
    }

    fn primitive(filter: ASTTypeFilter, len: usize) -> Self {
        Self::new(Some(filter), ASTTypeCheckInfo::Value(len))
    }

    fn reference(filter: ASTTypeFilter, name: String, index: ASTIndex) -> Self {
        Self::new(Some(filter), ASTTypeCheckInfo::Ref(name, index))
    }

    fn param(filter: ASTTypeFilter, name: String) -> Self {
        Self::new(Some(filter), ASTTypeCheckInfo::Param(name))
    }

    fn lambda(filter: ASTTypeFilter) -> Self {
        Self::new(Some(filter), ASTTypeCheckInfo::Lambda)
    }

    pub fn exact(&self) -> Option<(&ASTType, &ModuleInfo)> {
        if let Some(ASTTypeFilter::Exact(ref e, ref info)) = self.filter {
            Some((e, info))
        } else {
            None
        }
    }

    pub fn is_generic(&self) -> bool {
        match &self.filter {
            Some(ASTTypeFilter::Exact(t, i)) => t.is_generic(),
            Some(ASTTypeFilter::Lambda(_, rt)) => {
                rt.as_ref().map(|it| it.is_generic()).unwrap_or(false)
            }
            _ => false,
        }
    }

    pub fn generic_type_coeff(&self) -> Option<usize> {
        self.filter.as_ref().and_then(|it| it.generic_type_coeff())
    }

    /*
    fn any() -> Self {
        Self::new(Some(ASTTypeFilter::Any), ASTTypeCheckInfo::Any)
    }
    */
}

impl Display for ASTTypeCheckEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref filter) = self.filter {
            write!(f, "{filter}")?;
        } else {
            f.write_str("no type determined")?;
        }
        write!(f, " {}", self.info)
    }
}

pub struct ASTTypeCheckerResult {
    pub map: HashMap<ASTIndex, ASTTypeCheckEntry>,
}

impl ASTTypeCheckerResult {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, index: ASTIndex, entry: ASTTypeCheckEntry) {
        self.map.insert(index, entry);
    }

    pub fn get(&self, index: &ASTIndex) -> Option<&ASTTypeCheckEntry> {
        self.map.get(index)
    }

    pub fn extend(&mut self, other: ASTTypeCheckerResult) {
        self.map.extend(other.map);
    }

    pub fn remove(&mut self, index: &ASTIndex) -> Option<ASTTypeCheckEntry> {
        self.map.remove(index)
    }
}

pub struct ASTTypeChecker {
    pub result: ASTTypeCheckerResult,
    pub errors: Vec<ASTTypeCheckError>,
}

impl ASTTypeChecker {
    pub fn new() -> Self {
        Self {
            result: ASTTypeCheckerResult::new(),
            errors: Vec::new(),
        }
    }

    pub fn from_modules_container(modules_container: &ASTModulesContainer) -> (Self, ValContext) {
        let mut type_checker = ASTTypeChecker::new();
        let mut static_val_context = ValContext::new(None);

        for (id, namespace, module) in modules_container.modules() {
            let mut val_context = ValContext::new(None);

            type_checker.add_body(
                &mut val_context,
                &mut static_val_context,
                &module.body,
                None,
                &namespace,
                &id,
                &modules_container,
                None,
            );
        }

        let functions_ast_type_checkers = modules_container
            .modules()
            .par_iter()
            .flat_map(|(id, namespace, module)| {
                module.functions.par_iter().map(|function| {
                    let mut ftc = ASTTypeChecker::new();
                    let start = Instant::now();
                    ftc.add_function(
                        &function,
                        &static_val_context,
                        namespace,
                        id,
                        &modules_container,
                    );

                    let elapsed = start.elapsed();
                    if function.name == "processNone" {
                        info!("type check of {} in {}", function, elapsed.as_millis());
                    }

                    ftc
                })
            })
            .collect::<Vec<_>>();

        for fatc in functions_ast_type_checkers {
            type_checker.result.extend(fatc.result);
            type_checker.errors.extend(fatc.errors);
        }

        (type_checker, static_val_context)
    }

    pub fn add_function(
        &mut self,
        function: &ASTFunctionDef,
        static_val_context: &ValContext,
        module_namespace: &ModuleNamespace,
        module_id: &ModuleId,
        modules_container: &ASTModulesContainer,
    ) {
        let mut val_context = ValContext::new(None);

        /*
        let (mut result, mut return_type, mut errors) = self.get_body_type_map(
            &mut val_context,
            statics,
            &self.enhanced_ast_module.body,
            None,
        );
        */

        let generics_prefix = function.signature().generics_prefix(&module_namespace.0);

        for par in &function.parameters {
            let position = par.position.clone();
            let par = par.clone().fix_generics(&generics_prefix);
            if let Err(e) =
                val_context.insert_par(par.name.clone(), par, module_namespace, module_id)
            {
                self.errors.push(ASTTypeCheckError::new(
                    ASTTypeCheckErroKind::Error,
                    ASTIndex::new(module_namespace.clone(), module_id.clone(), position),
                    e,
                ));
            }
        }

        // in function body cannot be consts, but we need already defined ones...
        let mut tmp_static_val_context = ValContext::new(Some(static_val_context));

        match &function.body {
            ASTFunctionBody::RASMBody(body) => {
                let rt = if function.return_type.is_generic() {
                    &function
                        .return_type
                        .clone()
                        .add_generic_prefix(&generics_prefix)
                } else {
                    &function.return_type
                };
                // TODO return_type
                self.add_body(
                    &mut val_context,
                    &mut tmp_static_val_context,
                    body,
                    Some(rt),
                    module_namespace,
                    module_id,
                    modules_container,
                    Some(function),
                );
            }
            ASTFunctionBody::NativeBody(_body) => {}
        }
    }

    pub fn add_body(
        &mut self,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        body: &Vec<ASTStatement>,
        expected_last_statement_type: Option<&ASTType>,
        module_namespace: &ModuleNamespace,
        module_id: &ModuleId,
        modules_container: &ASTModulesContainer,
        function: Option<&ASTFunctionDef>,
    ) -> Option<ASTTypeCheckEntry> {
        let mut return_type = None;
        let inner_val_context = &mut ValContext::new(Some(val_context));
        /*
        println!(
            "get_body_type_map expected_last_statement_type {}",
            OptionDisplay(&expected_last_statement_type)
        );
        */

        for (i, statement) in body.iter().enumerate() {
            match statement {
                ASTStatement::Expression(e) => {
                    if i == body.len() - 1 {
                        let index = ASTIndex::new(
                            module_namespace.clone(),
                            module_id.clone(),
                            e.position(),
                        );
                        if let Some(ref elst) = expected_last_statement_type {
                            if !elst.is_unit() {
                                self.add_expr(
                                    e,
                                    inner_val_context,
                                    statics,
                                    Some(*elst),
                                    module_namespace,
                                    module_id,
                                    modules_container,
                                    function,
                                )
                            } else {
                                self.add_expr(
                                    e,
                                    inner_val_context,
                                    statics,
                                    None,
                                    module_namespace,
                                    module_id,
                                    modules_container,
                                    function,
                                )
                            };
                        } else {
                            self.add_expr(
                                e,
                                inner_val_context,
                                statics,
                                None,
                                module_namespace,
                                module_id,
                                modules_container,
                                function,
                            );
                        }
                        if let Some(rt) = self.result.get(&index).cloned() {
                            // can I do something when is generic? Take in account that it can be generic on something different
                            if !rt.is_generic() {
                                return_type = Some(rt);
                            }
                        }
                    } else {
                        self.add_expr(
                            e,
                            inner_val_context,
                            statics,
                            None,
                            module_namespace,
                            module_id,
                            modules_container,
                            function,
                        );
                    }
                }
                ASTStatement::LetStatement(key, e, is_const, index) => {
                    self.add_expr(
                        e,
                        inner_val_context,
                        statics,
                        None,
                        module_namespace,
                        module_id,
                        modules_container,
                        function,
                    );

                    let e_index =
                        ASTIndex::new(module_namespace.clone(), module_id.clone(), e.position());

                    let index =
                        ASTIndex::new(module_namespace.clone(), module_id.clone(), index.clone());
                    if let Some(entry) = self.result.get(&e_index) {
                        if let Some(filter) = &entry.filter {
                            if let ASTTypeFilter::Exact(ast_type, _module_info) = filter {
                                let insert_result = if *is_const {
                                    statics.insert_let(key.clone(), ast_type.clone(), &index)
                                } else {
                                    // TODO error
                                    inner_val_context.insert_let(
                                        key.clone(),
                                        ast_type.clone(),
                                        &index,
                                    )
                                };

                                if let Err(e) = insert_result {
                                    self.errors.push(ASTTypeCheckError::new(
                                        ASTTypeCheckErroKind::Error,
                                        index.clone(),
                                        e,
                                    ));
                                }
                            }
                            self.result.insert(
                                index,
                                ASTTypeCheckEntry::new(
                                    entry.filter.clone(),
                                    ASTTypeCheckInfo::Let(key.clone(), *is_const),
                                ),
                            );
                        }
                    }
                }
            }
        }

        //println!("return_type {}", OptionDisplay(&return_type));

        return_type
    }

    fn add_expr(
        &mut self,
        expr: &ASTExpression,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        module_namespace: &ModuleNamespace,
        module_id: &ModuleId,
        modules_container: &ASTModulesContainer,
        function: Option<&ASTFunctionDef>,
    ) {
        debug_i!(
            "add_expr {expr} expected {}",
            OptionDisplay(&expected_expression_type)
        );
        indent!();
        let index = ASTIndex::new(module_namespace.clone(), module_id.clone(), expr.position());

        if let Some(r) = self.result.get(&index) {
            if !r.is_generic() && r.exact().is_some() {
                debug_i!("Cached {r}");
                dedent!();

                return;
            }
        }

        match expr {
            ASTExpression::ASTFunctionCallExpression(call) => {
                self.add_call(
                    call,
                    val_context,
                    statics,
                    expected_expression_type,
                    module_namespace,
                    module_id,
                    modules_container,
                    function,
                );
            }
            ASTExpression::ValueRef(name, _) => {
                if let Some(kind) = val_context.get(name) {
                    self.result.insert(
                        index.clone(),
                        ASTTypeCheckEntry::reference(
                            ASTTypeFilter::exact(
                                kind.ast_type(),
                                module_namespace,
                                module_id,
                                modules_container,
                            ),
                            name.to_owned(),
                            kind.index(module_namespace, module_id),
                        ),
                    );
                } else if let Some(kind) = statics.get(name) {
                    self.result.insert(
                        index.clone(),
                        ASTTypeCheckEntry::reference(
                            ASTTypeFilter::exact(
                                kind.ast_type(),
                                module_namespace,
                                module_id,
                                modules_container,
                            ),
                            name.to_owned(),
                            kind.index(module_namespace, module_id),
                        ),
                    );
                } else {
                    self.errors.push(ASTTypeCheckError::new(
                        ASTTypeCheckErroKind::Error,
                        index.clone(),
                        format!("Cannot find value referencing {name}"),
                    ));
                }
            }
            ASTExpression::Value(value_type, _position) => {
                self.result.insert(
                    index.clone(),
                    ASTTypeCheckEntry::primitive(
                        ASTTypeFilter::Exact(
                            value_type.to_type(),
                            ModuleInfo::new(module_namespace.clone(), module_id.clone()),
                        ),
                        value_type.token_len(),
                    ),
                );
            }
            ASTExpression::Lambda(lambda) => {
                let mut lambda_val_context = ValContext::new(Some(&val_context));

                let expected_last_statement_type_and_parameters =
                    if let Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters,
                        return_type,
                    })) = expected_expression_type
                    {
                        for ((name, par_position), ast_type) in
                            lambda.parameter_names.iter().zip(parameters.iter())
                        {
                            let par_index = ASTIndex::new(
                                module_namespace.clone(),
                                module_id.clone(),
                                par_position.clone(),
                            );
                            if let Err(e) = lambda_val_context.insert_par(
                                name.clone(),
                                ASTParameterDef {
                                    name: name.clone(),
                                    ast_type: ast_type.clone(),
                                    position: par_position.clone(),
                                },
                                module_namespace,
                                module_id,
                            ) {
                                self.errors.push(ASTTypeCheckError::new(
                                    ASTTypeCheckErroKind::Error,
                                    par_index,
                                    e,
                                ));
                            } else {
                                self.result.insert(
                                    par_index.clone(),
                                    ASTTypeCheckEntry::param(
                                        ASTTypeFilter::exact(
                                            ast_type.clone(),
                                            module_namespace,
                                            module_id,
                                            modules_container,
                                        ),
                                        name.to_owned(),
                                    ),
                                );
                            }
                        }

                        Some((return_type.as_ref(), parameters))
                    } else {
                        None
                    };

                let body_return_type = self.add_body(
                    &mut lambda_val_context,
                    statics,
                    &lambda.body,
                    expected_last_statement_type_and_parameters.map(|it| it.0),
                    module_namespace,
                    module_id,
                    modules_container,
                    function,
                );

                if let Some(ASTTypeFilter::Exact(brt, _position)) =
                    body_return_type.and_then(|it| it.filter)
                {
                    let type_filter = if let Some((_return_type, parameters)) =
                        expected_last_statement_type_and_parameters
                    {
                        ASTTypeFilter::Exact(
                            ASTType::Builtin(BuiltinTypeKind::Lambda {
                                parameters: parameters.clone(),
                                return_type: Box::new(brt),
                            }),
                            ModuleInfo::new(module_namespace.clone(), module_id.clone()),
                        )
                    } else {
                        ASTTypeFilter::Lambda(lambda.parameter_names.len(), None)
                    };
                    self.result
                        .insert(index.clone(), ASTTypeCheckEntry::lambda(type_filter));
                } else {
                    let type_filter = if let Some((return_type, parameters)) =
                        expected_last_statement_type_and_parameters
                    {
                        ASTTypeFilter::Exact(
                            ASTType::Builtin(BuiltinTypeKind::Lambda {
                                parameters: parameters.clone(),
                                return_type: Box::new(return_type.clone()),
                            }),
                            ModuleInfo::new(module_namespace.clone(), module_id.clone()),
                        )
                    } else {
                        ASTTypeFilter::Lambda(lambda.parameter_names.len(), None)
                    };
                    self.result
                        .insert(index.clone(), ASTTypeCheckEntry::lambda(type_filter));
                }
            }
        }

        // the resolved type could be generic on a different generic type, we want to resolve it
        // with the generic type of the expected type

        if let Some(eet) = expected_expression_type {
            if eet.is_generic() {
                if let Some(entry) = self.result.get(&index) {
                    if let Some(ASTTypeFilter::Exact(et, e_module_id)) = &entry.filter {
                        if et.is_generic() {
                            if let Ok(rgt) =
                                Self::resolve_generic_types_from_effective_type(et, eet)
                            {
                                if let Some(rt) = Self::substitute(et, &rgt) {
                                    self.result.insert(
                                        index,
                                        ASTTypeCheckEntry::new(
                                            Some(ASTTypeFilter::Exact(rt, e_module_id.clone())),
                                            entry.info.clone(),
                                        ),
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        dedent!();
    }

    fn add_call(
        &mut self,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        module_namespace: &ModuleNamespace,
        module_id: &ModuleId,
        modules_container: &ASTModulesContainer,
        function: Option<&ASTFunctionDef>,
    ) {
        let index = ASTIndex::new(
            module_namespace.clone(),
            module_id.clone(),
            call.position.clone(),
        );

        debug_i!(
            "add_call {call} expected_expression_type {} : {index}",
            OptionDisplay(&expected_expression_type)
        );

        indent!();

        if let Some(t) = self.result.get(&index) {
            if !t.is_generic() && t.exact().is_some() {
                debug_i!("Cached {t}");
                dedent!();
                return;
            }
        }
        /*
        if let Some(kind) = val_context.get(&call.function_name) {
            self.result.insert(
                index,
                ASTTypeCheckEntry::reference(
                    ASTTypeFilter::exact(
                        kind.ast_type(),
                        module_namespace,
                        module_id,
                        modules_container,
                    ),
                    call.function_name.clone(),
                    kind.index(module_namespace, module_id),
                ),
            );
            dedent!();
            return;
        }
        */

        let mut first_try_of_map = HashMap::new();

        let mut tmp = ASTTypeChecker::new();

        for e in &call.parameters {
            let e_index = ASTIndex::new(module_namespace.clone(), module_id.clone(), e.position());
            if self.result.get(&e_index).is_none() {
                // it's almost impossible to determine the right type of lambda without knowing the expected type,
                // here we are calculating only the types for filtering the functions,
                // we hope that knowing only that it's a lambda, eventually the return type and the number of parameters is sufficient

                if let ASTExpression::Lambda(def) = e {
                    let ret_type = if let Some(body_ret_type) = tmp.add_body(
                        val_context,
                        statics,
                        &def.body,
                        None,
                        module_namespace,
                        module_id,
                        modules_container,
                        function,
                    ) {
                        body_ret_type.filter().clone()
                    } else {
                        None
                    };
                    first_try_of_map.insert(
                        e_index,
                        ASTTypeFilter::Lambda(
                            def.parameter_names.len(),
                            ret_type.map(|it| Box::new(it)),
                        ),
                    );
                } else {
                    self.add_expr(
                        e,
                        val_context,
                        statics,
                        None,
                        module_namespace,
                        module_id,
                        modules_container,
                        function,
                    );
                }
            }
        }

        let mut parameter_types_filters = Vec::new();

        for e in &call.parameters {
            let e_index = ASTIndex::new(module_namespace.clone(), module_id.clone(), e.position());
            if let Some(ast_type) = self.result.get(&e_index).and_then(|it| it.filter.clone()) {
                parameter_types_filters.push(ast_type.clone());
            } else {
                if let Some(entry) = first_try_of_map.get(&e_index) {
                    parameter_types_filters.push(entry.clone());
                } else {
                    parameter_types_filters.push(ASTTypeFilter::Any);
                }
            }
        }

        if let Some((lambda_return_type, parameters_types)) =
            val_context.get_lambda(&call.function_name)
        {
            let return_type = lambda_return_type.as_ref().clone();
            let parameters_types = parameters_types.clone();
            let generics = parameters_types
                .iter()
                .filter_map(|p| {
                    if let ASTType::Generic(_, g_name) = p {
                        Some(g_name.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            let lambda_signature = ASTFunctionSignature {
                name: call.function_name.clone(),
                parameters_types,
                return_type,
                generics,
                modifiers: ASTModifiers { public: true },
            };

            let entry = ASTFunctionSignatureEntry::new(
                lambda_signature,
                module_namespace.clone(),
                module_id.clone(),
                call.position.clone(),
            );

            self.process_function_signature(
                &entry,
                &parameter_types_filters,
                call,
                val_context,
                statics,
                expected_expression_type,
                module_namespace,
                module_id,
                true,
                modules_container,
                function,
            );

            /*
            let kind = val_context.get(&call.function_name).unwrap();

            self.result.insert(
                index,
                ASTTypeCheckEntry::reference(
                    ASTTypeFilter::exact(
                        kind.ast_type(),
                        module_namespace,
                        module_id,
                        modules_container,
                    ),
                    call.function_name.clone(),
                    kind.index(module_namespace, module_id),
                ),
            );
            */

            dedent!();

            return;
        }

        let mut functions = modules_container.find_call_vec(
            &call.function_name,
            &call.position,
            &parameter_types_filters,
            expected_expression_type,
            module_namespace,
            module_id,
        );

        if functions.is_empty() {
            // println!("no functions for {} : {}", call.function_name, index);
            // println!("filters {}", SliceDisplay(&parameter_types_filters));
            /*println!(
                "expected_expression_type {}",
                OptionDisplay(&expected_expression_type)
            );*/
            self.errors.push(ASTTypeCheckError::new(
                ASTTypeCheckErroKind::Error,
                index.clone(),
                format!(
                    "no functions for {}({})",
                    call.function_name,
                    SliceDisplay(&parameter_types_filters)
                ),
            ));
        } else {
            /*
            if functions.len() > 1 {
                let min = functions.iter().map(|it| it.rank).min().unwrap();

                functions = functions
                    .into_iter()
                    .filter(|it| it.rank == min)
                    .collect::<Vec<_>>();
            }
            */

            if functions.len() > 1 {
                /*
                print!(
                    "found more than one function for {} : {} -> ",
                    call.function_name, index
                );
                println!("{}", SliceDisplay(&parameter_types_filters));
                for fun in functions.iter() {
                    println!("  function {}", fun.signature);
                }
                */

                let functions_msg = functions
                    .iter()
                    .map(|it| format!("  function {}", it.signature))
                    .join("\n");

                let is_generic = function.map(ASTFunctionDef::is_generic).unwrap_or(false);
                self.errors.push(ASTTypeCheckError::new(
                    if is_generic {
                        ASTTypeCheckErroKind::Warning
                    } else {
                        ASTTypeCheckErroKind::Error
                    },
                    index.clone(),
                    format!(
                        "found more than one function for {}\n{functions_msg}",
                        call.function_name
                    ),
                ));

                let return_types = functions
                    .iter()
                    .map(|it| &it.signature.return_type)
                    .collect::<HashSet<_>>();

                let filter = if return_types.len() == 1 {
                    Some(ASTTypeFilter::exact(
                        (*return_types.iter().exactly_one().unwrap()).clone(),
                        module_namespace,
                        module_id,
                        modules_container,
                    ))
                } else {
                    None
                };

                self.result.insert(
                    index,
                    ASTTypeCheckEntry::new(
                        filter,
                        ASTTypeCheckInfo::Call(
                            call.function_name.clone(),
                            functions
                                .into_iter()
                                .map(|it| {
                                    (
                                        it.signature.clone(),
                                        ASTIndex::new(
                                            it.namespace.clone(),
                                            it.module_id.clone(),
                                            it.position.clone(),
                                        ),
                                    )
                                })
                                .collect(),
                        ),
                    ),
                );

                // self.result.extend(first_try_of_map);
            } else {
                let found_function = functions.remove(0);

                /*
                for (e, t) in zip(&call.parameters, &found_function.signature.parameters_types) {
                    let e_index =
                        ASTIndex::new(module_namespace.clone(), module_id.clone(), e.position());

                    self.result.remove(&e_index);

                    self.get_expr_type_map(
                        e,
                        val_context,
                        statics,
                        Some(t),
                        module_namespace,
                        module_id,
                    );
                }
                */

                self.process_function_signature(
                    &found_function,
                    &parameter_types_filters,
                    &call,
                    val_context,
                    statics,
                    expected_expression_type,
                    module_namespace,
                    module_id,
                    false,
                    modules_container,
                    function,
                );
            }
        }
        dedent!();
    }

    fn process_function_signature(
        &mut self,
        function_signature_entry: &ASTFunctionSignatureEntry,
        parameter_types_filters: &Vec<ASTTypeFilter>,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        call_module_namespace: &ModuleNamespace,
        call_module_id: &ModuleId,
        is_lambda: bool,
        modules_container: &ASTModulesContainer,
        function: Option<&ASTFunctionDef>,
    ) {
        debug_i!(
            "process_function_signature {} with {} expected {}",
            function_signature_entry.signature,
            SliceDisplay(parameter_types_filters),
            OptionDisplay(&expected_expression_type)
        );
        indent!();

        let function_signature = &function_signature_entry.signature;
        let index = ASTIndex::new(
            call_module_namespace.clone(),
            call_module_id.clone(),
            call.position.clone(),
        );

        let mut resolved_generic_types = ASTResolvedGenericTypes::new();

        for (i, parameter) in function_signature.parameters_types.iter().enumerate() {
            if parameter.is_generic() {
                let calculated_type_filter = parameter_types_filters.get(i).unwrap();

                let p_errors = Self::add_resolve_type_filter(
                    &index,
                    parameter,
                    calculated_type_filter,
                    &mut resolved_generic_types,
                );

                //self.errors.extend(p_errors);
            }
        }

        loop {
            if call.parameters.is_empty() {
                break;
            }
            debug_i!("loop  {}", resolved_generic_types);
            indent!();
            let resolved_generic_types_len = resolved_generic_types.len();
            let mut loop_errors = Vec::new();

            for (i, e) in call.parameters.iter().enumerate() {
                let e_index = ASTIndex::new(
                    call_module_namespace.clone(),
                    call_module_id.clone(),
                    e.position(),
                );
                let parameter_type = function_signature.parameters_types.get(i).unwrap();
                let ps = Self::substitute(&parameter_type, &resolved_generic_types);
                let ast_type = if let Some(ref a) = ps {
                    a
                } else {
                    parameter_type
                };

                self.add_expr(
                    e,
                    val_context,
                    statics,
                    Some(&ast_type),
                    call_module_namespace,
                    call_module_id,
                    modules_container,
                    function,
                );

                if let Some(entry) = self.result.get(&e_index) {
                    if let Some(ref calculated_type_filter) = entry.filter {
                        let p_errors = Self::add_resolve_type_filter(
                            &index,
                            &parameter_type,
                            calculated_type_filter,
                            &mut resolved_generic_types,
                        );

                        loop_errors.extend(p_errors);
                    }
                }
            }

            dedent!();

            if resolved_generic_types.len() == resolved_generic_types_len {
                self.errors.extend(loop_errors);
                break;
            }
        }

        let mut return_type =
            if function_signature.return_type.is_generic() && resolved_generic_types.len() > 0 {
                if let Some(return_type) =
                    Self::substitute(&function_signature.return_type, &resolved_generic_types)
                {
                    return_type
                } else {
                    function_signature.return_type.clone()
                }
            } else {
                function_signature.return_type.clone()
            };

        if let Some(eet) = expected_expression_type {
            if return_type.is_generic() {
                if let Ok(rgt) = Self::resolve_generic_types_from_effective_type(&return_type, eet)
                {
                    if let Some(rt) = Self::substitute(&return_type, &rgt) {
                        return_type = rt;
                    }
                }
            }
        }

        if is_lambda {
            self.result.insert(
                index.clone(),
                ASTTypeCheckEntry::new(
                    Some(ASTTypeFilter::exact(
                        return_type,
                        call_module_namespace,
                        call_module_id,
                        modules_container,
                    )),
                    ASTTypeCheckInfo::LambdaCall(
                        function_signature.clone(),
                        ASTIndex::new(
                            function_signature_entry.namespace.clone(),
                            function_signature_entry.module_id.clone(),
                            function_signature_entry.position.clone(),
                        ),
                    ),
                ),
            );
        } else {
            self.result.insert(
                index.clone(),
                ASTTypeCheckEntry::new(
                    Some(ASTTypeFilter::exact(
                        return_type,
                        call_module_namespace,
                        call_module_id,
                        modules_container,
                    )),
                    ASTTypeCheckInfo::Call(
                        call.function_name.clone(),
                        vec![(
                            function_signature.clone(),
                            ASTIndex::new(
                                function_signature_entry.namespace.clone(),
                                function_signature_entry.module_id.clone(),
                                function_signature_entry.position.clone(),
                            ),
                        )],
                    ),
                ),
            );
        }
        dedent!();
    }

    fn add_resolve_type_filter(
        index: &ASTIndex,
        generic_type: &ASTType,
        effective_filter: &ASTTypeFilter,
        resolved_generic_types: &mut ASTResolvedGenericTypes,
    ) -> Vec<ASTTypeCheckError> {
        let mut errors = Vec::new();

        match Self::resolve_type_filter(generic_type, effective_filter) {
            Ok(rgt) => {
                if let Err(e) = resolved_generic_types.extend(rgt) {
                    errors.push(ASTTypeCheckError::new(
                        ASTTypeCheckErroKind::Error,
                        index.clone(),
                        e,
                    ));
                }
            }
            Err(e) => errors.push(e),
        }

        errors
    }

    pub fn resolve_type_filter(
        generic_type: &ASTType,
        effective_filter: &ASTTypeFilter,
    ) -> Result<ASTResolvedGenericTypes, ASTTypeCheckError> {
        if let ASTTypeFilter::Exact(effective_type, _) = effective_filter {
            return Self::resolve_generic_types_from_effective_type(generic_type, effective_type);
        } else if let ASTTypeFilter::Lambda(n, ret_type) = effective_filter {
            if let Some(rt_filter) = ret_type {
                match generic_type {
                    ASTType::Builtin(BuiltinTypeKind::Lambda {
                        parameters: _,
                        return_type,
                    }) => {
                        return Self::resolve_type_filter(&return_type, &rt_filter);
                    }
                    ASTType::Generic(_, _) => {
                        if *n == 0 {
                            if let ASTTypeFilter::Exact(effective_type, _) = rt_filter.as_ref() {
                                let ef = ASTType::Builtin(BuiltinTypeKind::Lambda {
                                    parameters: Vec::new(),
                                    return_type: Box::new(effective_type.clone()),
                                });
                                return Self::resolve_generic_types_from_effective_type(
                                    generic_type,
                                    &ef,
                                );
                            }
                        }
                    }
                    _ => {
                        return Err(ASTTypeCheckError::new(
                            ASTTypeCheckErroKind::Error,
                            ASTIndex::none(),
                            format!("Expected lambda or generic. Got {generic_type}"),
                        ));
                    }
                }
            }
        }
        Ok(ASTResolvedGenericTypes::new())
    }

    pub fn resolve_generic_types_from_effective_type(
        generic_type: &ASTType,
        effective_type: &ASTType,
    ) -> Result<ASTResolvedGenericTypes, ASTTypeCheckError> {
        let mut result = ASTResolvedGenericTypes::new();
        if generic_type == effective_type {
            return Ok(result);
        }

        debug_i!("resolve_generic_types_from_effective_type {generic_type} ==> {effective_type}");
        //println!("resolve_generic_types_from_effective_type: generic_type {generic_type} effective_type  {effective_type}");
        indent!();

        match generic_type {
            ASTType::Builtin(kind) => {
                match kind {
                    BuiltinTypeKind::String => {}
                    BuiltinTypeKind::I32 => {}
                    BuiltinTypeKind::Bool => {}
                    BuiltinTypeKind::Char => {}
                    BuiltinTypeKind::F32 => {}
                    BuiltinTypeKind::Lambda {
                        parameters: p_parameters,
                        return_type: p_return_type,
                    } => match effective_type {
                        ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters: e_parameters,
                            return_type: e_return_type,
                        }) => {
                            if e_parameters.len() != p_parameters.len() {
                                dedent!();
                                return Err(Self::type_check_error(
                                    ASTTypeCheckErroKind::Error,
                                    "Invalid parameters count.".to_string(),
                                ));
                            }
                            for (i, p_p) in p_parameters.iter().enumerate() {
                                let e_p = e_parameters.get(i).unwrap();

                                let inner_result = Self::resolve_generic_types_from_effective_type(p_p, e_p)
                                .map_err(|e| {
                                    dedent!();
                                    e.add(ASTTypeCheckErroKind::Error, ASTIndex::none(), format!("lambda param gen type {generic_type}, eff. type {effective_type}"))})?;

                                result.extend(inner_result).map_err(|it| {
                                    dedent!();
                                    Self::type_check_error(ASTTypeCheckErroKind::Error, it.clone())
                                })?;
                            }

                            /*
                            for p_t in p_return_type {
                                if let Some(e_t) = e_return_type {
                                    let inner_result = resolve_generic_types_from_effective_type(p_t, e_t)
                                        .map_err(|e| format!("{} in return type gen type {generic_type}eff. type {effective_type}", e))?;

                                    result.extend(inner_result.into_iter());
                                } else {
                                    dedent!();
                                    if let ASTType::Generic(p) = p_t.as_ref() {
                                        return Err(format!("Found generic type {p} that is (). For now, we cannot handle it").into());
                                    }
                                    return Err("Expected some type but got None".into());
                                }
                            }

                             */
                            let inner_result = Self::resolve_generic_types_from_effective_type(p_return_type, e_return_type)
                            .map_err(|e| {
                                dedent!();
                                e.add(ASTTypeCheckErroKind::Error, ASTIndex::none(), format!("in return type gen type {generic_type}, eff. type {effective_type}"))})?;

                            result.extend(inner_result).map_err(|it| {
                                dedent!();
                                Self::type_check_error(ASTTypeCheckErroKind::Error, it.clone())
                            })?;
                        }
                        _ => {
                            dedent!();
                            return Err(Self::type_check_error(ASTTypeCheckErroKind::Error, format!("unmatched types, generic type is {generic_type}, real type is {effective_type}")));
                        }
                    },
                }
            }
            ASTType::Generic(_, p) => {
                let ignore = if let ASTType::Generic(_, p1) = effective_type {
                    p == p1
                } else {
                    false
                };
                //if !ignore {
                debug_i!("resolved generic type {p} to {effective_type}");
                result.insert(p.clone(), effective_type.clone());
                //}
            }
            ASTType::Custom {
                name: g_name,
                param_types: g_param_types,
                position: _,
            } => match effective_type {
                ASTType::Custom {
                    name: e_name,
                    param_types: e_param_types,
                    position: _,
                } => {
                    if g_name != e_name {
                        dedent!();
                        return Err(Self::type_check_error(
                            ASTTypeCheckErroKind::Error,
                            format!("unmatched custom type name {g_name} != {e_name}"),
                        ));
                    }

                    for (i, p_p) in g_param_types.iter().enumerate() {
                        let e_p = if let Some(p) = e_param_types.get(i) {
                            p
                        } else {
                            return Err(ASTTypeCheckError::new(
                                ASTTypeCheckErroKind::Error,
                                ASTIndex::none(),
                                format!("Cannot find parameter {i}"),
                            ));
                        };
                        let inner_result = Self::resolve_generic_types_from_effective_type(p_p, e_p)
                            .map_err(|e| { dedent!();
                        e.add(ASTTypeCheckErroKind::Error, ASTIndex::none(), format!("in custom type gen type {generic_type} eff type {effective_type}"))})?;

                        result.extend(inner_result).map_err(|it| {
                            dedent!();
                            ASTTypeCheckError::new(
                                ASTTypeCheckErroKind::Error,
                                ASTIndex::none(),
                                format!(
                                    "{it}: in custom type gen type {generic_type} eff type {effective_type}"
                                ),
                            )
                        })?;
                    }
                }
                ASTType::Generic(_, _) => {}
                _ => {
                    dedent!();
                    return Err(Self::type_check_error(ASTTypeCheckErroKind::Error, format!(
                        "unmatched types, generic type is {generic_type}, real type is {effective_type}")));
                }
            },
            ASTType::Unit => {}
        }

        //debug_i!("result {result}");
        dedent!();
        Ok(result)
    }

    fn type_check_error(kind: ASTTypeCheckErroKind, message: String) -> ASTTypeCheckError {
        ASTTypeCheckError::new(kind, ASTIndex::none(), message)
    }

    pub fn substitute(
        ast_type: &ASTType,
        resolved_param_types: &ASTResolvedGenericTypes,
    ) -> Option<ASTType> {
        if !ast_type.is_generic() {
            return None;
        }

        let result = match &ast_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let mut something_substituted = false;
                    let new_parameters =
                        match Self::substitute_types(parameters, resolved_param_types) {
                            None => parameters.clone(),
                            Some(new_parameters) => {
                                something_substituted = true;
                                new_parameters
                            }
                        };

                    let new_return_type =
                        if let Some(new_t) = Self::substitute(return_type, resolved_param_types) {
                            something_substituted = true;
                            Box::new(new_t)
                        } else {
                            return_type.clone()
                        };

                    if something_substituted {
                        Some(ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters: new_parameters,
                            return_type: new_return_type,
                        }))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            ASTType::Generic(_, p) => {
                if resolved_param_types.contains_key(p) {
                    resolved_param_types.get(p).cloned()
                } else {
                    None
                }
            }
            ASTType::Custom {
                name,
                param_types,
                position,
            } => {
                Self::substitute_types(param_types, resolved_param_types).map(|new_param_types| {
                    // TODO calculating the new position it's a bit heuristic, and probably it's not needed
                    /*let new_index = if new_param_types.is_empty() {
                        position.clone()
                    } else if let Some(ASTType::Custom {
                        name: _,
                        param_types: _,
                        position: ast_index,
                    }) = new_param_types.last()
                    {
                        ast_index.mv_right(1)
                    } else {
                        position.clone()
                    };
                    */
                    ASTType::Custom {
                        name: name.clone(),
                        param_types: new_param_types,
                        position: position.clone(),
                    }
                })
            }
            ASTType::Unit => None,
        };

        if let Some(r) = &result {
            debug_i!("something substituted {ast_type} -> {r}");
        }
        result
    }

    fn substitute_types(
        types: &[ASTType],
        resolved_param_types: &ASTResolvedGenericTypes,
    ) -> Option<Vec<ASTType>> {
        let mut something_substituted = false;
        let new_types = types
            .iter()
            .map(|it| {
                if let Some(new_t) = Self::substitute(it, resolved_param_types) {
                    something_substituted = true;
                    new_t
                } else {
                    it.clone()
                }
            })
            .collect();

        if something_substituted {
            Some(new_types)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        env,
        path::{Path, PathBuf},
        str::FromStr,
    };

    use rasm_utils::{
        test_utils::{init_log, init_minimal_log},
        OptionDisplay, SliceDisplay,
    };

    use crate::{
        codegen::{
            c::options::COptions,
            compile_target::CompileTarget,
            enh_ast::{EnhASTNameSpace, EnhModuleId, EnhModuleInfo},
            statics::Statics,
            val_context::ValContext,
        },
        commandline::CommandLineOptions,
        project::{RasmProject, RasmProjectRunType},
        type_check::{
            ast_modules_container::ASTModulesContainer, ast_type_checker::ASTTypeCheckInfo,
            test_utils::project_and_container,
        },
    };
    use rasm_parser::{
        catalog::{modules_catalog::ModulesCatalog, ASTIndex},
        parser::ast::{
            ASTExpression, ASTFunctionCall, ASTLambdaDef, ASTModule, ASTPosition, ASTStatement,
            ASTValueType,
        },
    };

    use super::{ASTTypeChecker, ASTTypeCheckerResult};

    #[test]
    fn test_breakout_check_functions() {
        let project = RasmProject::new(PathBuf::from("../rasm/resources/examples/breakout"));

        let target = CompileTarget::C(COptions::default());

        let mut statics = Statics::new();
        let (container, _catalog, _errors) = project.container_and_catalog(
            &mut statics,
            &RasmProjectRunType::Main,
            &target,
            false,
            &CommandLineOptions::default(),
        );

        let mut statics = ValContext::new(None);

        let path = Path::new("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm");

        let (module, _errors, info) = project.get_module(path, &target).unwrap();

        let mut function_type_checker = ASTTypeChecker::new();

        for function in module.functions.into_iter() {
            function_type_checker.add_function(
                &function,
                &mut statics,
                &info.module_namespace(),
                &info.module_id(),
                &container,
            );
        }
    }

    #[test]
    fn test_functions_checker1() {
        let file = "resources/test/ast_type_checker/ast_type_checker1.rasm";

        let (types_map, info) = check_body(file);

        /*
        for (key, value) in types_map.map.iter() {
            println!("types_map {key} = {value:?}");
        }
        */

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(1, 5),
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker2() {
        let file = "resources/test/ast_type_checker/ast_type_checker2.rasm";

        let (types_map, info) = check_body(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(1, 5),
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );

        /*
        for (index, type_filter) in result {
            println!("{index} {type_filter}");
        }
        */
    }

    #[test]
    fn test_functions_checker3() {
        let file = "resources/test/ast_type_checker/ast_type_checker3.rasm";

        let (types_map, info) = check_body(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(1, 5),
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );

        /*
        for (index, type_filter) in result {
            println!("{index} {type_filter}");
        }
        */
    }

    #[test]
    fn test_functions_checker9() {
        init_minimal_log();

        let file = "resources/test/ast_type_checker/ast_type_checker9.rasm";

        let (types_map, info) = check_body(file);

        let none_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(1, 36),
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!(
                "{}",
                OptionDisplay(&none_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker10() {
        init_minimal_log();

        let file = "resources/test/ast_type_checker/ast_type_checker10.rasm";

        let (types_map, info) = check_body(file);

        let none_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(1, 18),
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!(
                "{}",
                OptionDisplay(&none_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker11() {
        init_minimal_log();

        let file = "resources/test/ast_type_checker/ast_type_checker11.rasm";

        let (types_map, info) = check_body(file);

        let none_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(1, 20),
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!(
                "{}",
                OptionDisplay(&none_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker4() {
        let file = "resources/test/ast_type_checker/ast_type_checker4.rasm";

        let (types_map, info) = check_body(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(1, 5),
        ));

        assert_eq!(
            "Some(Exact(Option<i32>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );

        /*
        for (index, type_filter) in result {
            println!("{index} {type_filter}");
        }
        */
    }

    #[test]
    fn test_functions_checker5() {
        let file = "resources/test/ast_type_checker/ast_type_checker5.rasm";

        let (types_map, info) = check_body(file);

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(1, 5),
        ));

        assert_eq!(
            "Some(Exact(List<Option<i32>>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker6() {
        init_minimal_log();

        let file = "resources/test/ast_type_checker/ast_type_checker6.rasm";

        let (types_map, info) = check_function(file, "endsWith");

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(3, 9),
        ));

        assert_eq!(
            "Some(Exact(Option<ast_type_checker6_ast_type_checker6_endsWith:T>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker6_1() {
        init_minimal_log();

        let file = "resources/test/ast_type_checker/ast_type_checker6.rasm";

        let (types_map, info) = check_function(file, "endsWith1");

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(13, 9),
        ));

        assert_eq!(
            "Some(Exact(Option<ast_type_checker6_ast_type_checker6_endsWith1:T>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker7() {
        let file = "resources/test/ast_type_checker/ast_type_checker7.rasm";

        let (types_map, info) = check_function(file, "endsWith");

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(4, 22),
        ));

        assert_eq!(
            "Some(Exact(ast_type_checker7_ast_type_checker7_endsWith:T))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(3, 19),
        ));

        assert_eq!(
            "Some(Exact(ast_type_checker7_ast_type_checker7_endsWith:T))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker8() {
        let file: &str = "resources/test/ast_type_checker/ast_type_checker8.rasm";

        let (types_map, info) = check_function(file, "generic");

        let r_value = types_map.get(&ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition::new(9, 13),
        ));

        assert_eq!(
            "Some(Exact(ast_type_checker8_ast_type_checker8_generic:T))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_breakout() {
        check_project("../rasm/resources/examples/breakout");
    }

    #[test]
    fn test_let1() {
        check_project("../rasm/resources/test/let1.rasm");
    }

    #[test]
    fn test_type_check_lambda2() {
        init_log();

        let (type_checker, catalog, _) = check_project("../rasm/resources/test/lambda2.rasm");

        if let Some(index) = index(&catalog, "../rasm/resources/test/lambda2.rasm", 4, 6) {
            if let Some(entry) = type_checker.result.get(&index) {
                if let Some((t, _)) = entry.exact() {
                    assert_eq!("Option<i32>", &format!("{t}"));
                    return;
                }
            }
        }
        panic!();
    }

    #[test]
    fn test_lambda2_if() {
        init_minimal_log();
        env::set_var("RASM_STDLIB", "../stdlib");

        let path = PathBuf::from("../rasm/resources/test/lambda2.rasm");
        let project = RasmProject::new(path.clone());

        let target = CompileTarget::C(COptions::default());

        let mut statics = Statics::new();
        let (container, catalog, _errors) = project.container_and_catalog(
            &mut statics,
            &RasmProjectRunType::Main,
            &target,
            false,
            &CommandLineOptions::default(),
        );

        let mut type_checker = ASTTypeChecker::new();
        let mut val_context = ValContext::new(None);
        let mut statics = ValContext::new(None);

        let info = catalog
            .info(&EnhModuleId::Path(path.canonicalize().unwrap()))
            .unwrap();

        let i = ASTExpression::Value(ASTValueType::I32(10), ASTPosition::new(2, 29));

        let some = ASTFunctionCall {
            function_name: "Option::Some".to_owned(),
            parameters: vec![i],
            position: ASTPosition::new(2, 16),
            generics: Vec::new(),
        };
        let o = ASTExpression::ASTFunctionCallExpression(some);

        let l = ASTExpression::Lambda(ASTLambdaDef {
            parameter_names: Vec::new(),
            body: vec![ASTStatement::Expression(o)],
            position: ASTPosition::new(2, 14),
        });

        let t = ASTExpression::Value(ASTValueType::Boolean(true), ASTPosition::new(2, 8));

        let call = ASTFunctionCall {
            function_name: "if".to_owned(),
            parameters: vec![t, l],
            position: ASTPosition::new(2, 5),
            generics: Vec::new(),
        };

        type_checker.add_call(
            &call,
            &mut val_context,
            &mut statics,
            None,
            info.namespace(),
            info.id(),
            &container,
            None,
        );

        if let Some(index) = index(&catalog, "../rasm/resources/test/lambda2.rasm", 2, 5) {
            if let Some(entry) = type_checker.result.get(&index) {
                if let Some((t, _)) = entry.exact() {
                    assert_eq!("If<fn () -> Option<i32>>", &format!("{t}"));
                    return;
                }
            }
        }

        panic!();
    }

    #[test]
    fn test_lambda2_if_body() {
        init_minimal_log();
        env::set_var("RASM_STDLIB", "../stdlib");

        let path = PathBuf::from("../rasm/resources/test/lambda2.rasm");
        let project = RasmProject::new(path.clone());

        let target = CompileTarget::C(COptions::default());

        let mut statics = Statics::new();
        let (container, catalog, _errors) = project.container_and_catalog(
            &mut statics,
            &RasmProjectRunType::Main,
            &target,
            false,
            &CommandLineOptions::default(),
        );

        let mut type_checker = ASTTypeChecker::new();
        let mut val_context = ValContext::new(None);
        let mut statics = ValContext::new(None);

        let info = catalog
            .info(&EnhModuleId::Path(path.canonicalize().unwrap()))
            .unwrap();

        let i = ASTExpression::Value(ASTValueType::I32(10), ASTPosition::new(2, 29));

        let some = ASTFunctionCall {
            function_name: "Option::Some".to_owned(),
            parameters: vec![i],
            position: ASTPosition::new(2, 16),
            generics: Vec::new(),
        };
        let o = ASTExpression::ASTFunctionCallExpression(some);

        let body = vec![ASTStatement::Expression(o)];

        let res = type_checker.add_body(
            &mut val_context,
            &mut statics,
            &body,
            None,
            info.namespace(),
            info.id(),
            &container,
            None,
        );

        if let Some(entry) = res {
            if let Some((t, _)) = entry.exact() {
                assert_eq!("Option<i32>", &format!("{t}"));
                return;
            }
        }

        panic!();
    }

    #[test]
    fn test_print() {
        init_minimal_log();

        let (tc, catalog, _) = check_project("../stdlib");
        if let Some(entry) = tc
            .result
            .get(&index(&catalog, "../stdlib/src/main/rasm/print.rasm", 13, 5).unwrap())
        {
            if let ASTTypeCheckInfo::Call(_, vec) = &entry.info {
                let mut v = vec.iter().map(|it| format!("{}", it.0)).collect::<Vec<_>>();
                v.sort();
                assert_eq!(
                    "print(Compare), print<stdlib_iter_print:T>(Iter<stdlib_iter_print:T>), print<stdlib_list_print:T>(List<stdlib_list_print:T>), print<stdlib_print_print:T>(stdlib_print_print:T)",
                    format!(
                        "{}",
                        SliceDisplay(&v)
                    )
                );
                return;
            }
        }
        panic!()
    }

    fn index(
        catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        path: &str,
        row: usize,
        col: usize,
    ) -> Option<ASTIndex> {
        if let Some(info) = catalog.info(&EnhModuleId::Path(
            PathBuf::from_str(path).unwrap().canonicalize().unwrap(),
        )) {
            let index = ASTIndex::new(
                info.namespace().clone(),
                info.id().clone(),
                ASTPosition::new(row, col),
            );
            Some(index)
        } else {
            None
        }
    }

    fn check_project(
        path: &str,
    ) -> (
        ASTTypeChecker,
        impl ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        ValContext,
    ) {
        env::set_var("RASM_STDLIB", "../stdlib");
        let project = RasmProject::new(PathBuf::from(path));

        let target = CompileTarget::C(COptions::default());

        let mut statics = Statics::new();
        let (container, catalog, _errors) = project.container_and_catalog(
            &mut statics,
            &RasmProjectRunType::Main,
            &target,
            false,
            &CommandLineOptions::default(),
        );

        let result = ASTTypeChecker::from_modules_container(&container);

        (result.0, catalog, result.1)
    }

    fn check_body(file: &str) -> (ASTTypeCheckerResult, EnhModuleInfo) {
        apply_to_functions_checker(file, file, |module, mut ftc, info, cont| {
            for e in ftc.errors.iter() {
                println!("type checker error {e}");
            }
            let mut val_context = ValContext::new(None);
            let mut static_val_context = ValContext::new(None);
            ftc.add_body(
                &mut val_context,
                &mut static_val_context,
                &module.body,
                None,
                &info.module_namespace(),
                &info.module_id(),
                &cont,
                None,
            );
            ftc.result
        })
    }

    fn check_function(file: &str, function_name: &str) -> (ASTTypeCheckerResult, EnhModuleInfo) {
        apply_to_functions_checker(file, file, |module, mut ftc, info, cont| {
            let function = module
                .functions
                .iter()
                .find(|it| &it.name == function_name)
                .unwrap()
                .clone();

            let mut static_val_context = ValContext::new(None);
            ftc.add_function(
                &function,
                &mut static_val_context,
                &info.module_namespace(),
                &info.module_id(),
                &cont,
            );
            ftc.result
        })
    }

    fn apply_to_functions_checker<'a, F>(
        project_path: &str,
        file: &str,
        f: F,
    ) -> (ASTTypeCheckerResult, EnhModuleInfo)
    where
        F: Fn(
            &ASTModule,
            ASTTypeChecker,
            EnhModuleInfo,
            ASTModulesContainer,
        ) -> ASTTypeCheckerResult,
    {
        let target = CompileTarget::C(COptions::default());
        let (project, modules_container) = project_and_container(&target, &project_path);
        let function_type_checker = ASTTypeChecker::new();

        let (module, _, info) = project.get_module(Path::new(file), &target).unwrap();

        (
            f(
                &module,
                function_type_checker,
                info.clone(),
                modules_container,
            ),
            info,
        )
    }
}
