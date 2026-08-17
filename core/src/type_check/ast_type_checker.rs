use std::{collections::HashMap, fmt::Display, iter::zip, sync::Arc, time::Instant};

use itertools::Itertools;

use linked_hash_map::LinkedHashMap;
use rasm_utils::{
    OptionDisplay, SliceDisplay, chunk_size, debug_i,
    debug_indent::{enable_log, log_enabled},
    dedent, indent,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    ast::{
        ast_function_signature::ASTFunctionSignature,
        generic_prefix::{add_generic_prefix, fix_ast_par_generics},
    },
    codegen::val_context::ValContext,
    macros::macro_call_extractor::get_macro_result_type,
    type_check::ast_generic_types_resolver::ASTResolvedGenericTypes,
};

use rasm_parser::{
    catalog::{ASTIndex, ModuleId, ModuleInfo, ModuleNamespace},
    parser::ast::{
        ASTBuiltinTypeKind, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
        ASTLambdaDef, ASTModifiers, ASTParameterDef, ASTStatement, ASTType,
    },
};

use super::ast_modules_container::{ASTFunctionSignatureEntry, ASTModulesContainer, ASTTypeFilter};

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypeCheckErroKind {
    Fatal,
    Error,
    Warning,
}

impl Display for ASTTypeCheckErroKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypeCheckErroKind::Fatal => write!(f, "Fatal"),
            ASTTypeCheckErroKind::Error => write!(f, "Error"),
            ASTTypeCheckErroKind::Warning => write!(f, "Warning"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypeCheckError {
    kind: ASTTypeCheckErroKind,
    index: ASTIndex,
    message: String,
    inner: Vec<ASTTypeCheckError>,
}

impl ASTTypeCheckError {
    pub fn new(kind: ASTTypeCheckErroKind, index: ASTIndex, message: String) -> Self {
        Self {
            kind,
            index,
            message,
            inner: Vec::new(),
        }
    }

    pub fn new_with_inner(
        kind: ASTTypeCheckErroKind,
        index: ASTIndex,
        message: String,
        inner: Vec<ASTTypeCheckError>,
    ) -> Self {
        Self {
            kind,
            index,
            message,
            inner,
        }
    }

    pub fn add(self, kind: ASTTypeCheckErroKind, index: ASTIndex, message: String) -> Self {
        let mut result = self;
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

    fn write(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let spaces = " ".repeat(indent * 2);
        let m = self.message.replace("$indent", &spaces);
        write!(f, "{spaces}{} : {}", m, self.index)?;
        for error in &self.inner {
            write!(f, "\n")?;
            error.write(f, indent + 1)?;
        }
        Result::Ok(())
    }

    pub fn inner(&self) -> &Vec<ASTTypeCheckError> {
        &self.inner
    }
}

impl Display for ASTTypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypeCheckInfo {
    Call(String, Vec<(ASTFunctionSignature, ASTIndex)>, bool),
    LambdaCall(ASTFunctionSignature, ASTIndex),
    ReferenceToFunction(ASTFunctionSignature, ASTIndex),
    Ref(String, ASTIndex),
    Let(String),
    Const(String),
    Param(String),
    Value(usize), // the length of the token of the Value, for example for a string "s" it's 3
    Lambda,
}

impl Display for ASTTypeCheckInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypeCheckInfo::Call(name, vec, is_macro) => {
                let mc = if *is_macro { "macro " } else { "" };
                if vec.len() == 1 {
                    write!(f, "call to {mc}{}", vec.first().unwrap().0)?;
                } else {
                    write!(f, "call to {mc}{name} which can be one of \n")?;
                    for function_signature in vec {
                        write!(f, "  $indent{}\n", function_signature.0)?;
                    }
                }
                Result::Ok(())
            }
            ASTTypeCheckInfo::LambdaCall(function_signature, _) => {
                write!(f, "call to lambda {}", function_signature.name)
            }
            ASTTypeCheckInfo::Ref(name, _) => f.write_str(&format!("ref to {name}")),
            ASTTypeCheckInfo::Value(_) => f.write_str("value"),
            ASTTypeCheckInfo::Let(_) => f.write_str("let"),
            ASTTypeCheckInfo::Const(_) => f.write_str("const"),
            ASTTypeCheckInfo::Param(_) => f.write_str("param"),
            ASTTypeCheckInfo::Lambda => f.write_str("lambda"),
            ASTTypeCheckInfo::ReferenceToFunction(function_signature, _) => {
                write!(f, "reference to function {}", function_signature.name)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypeCheckEntry {
    index: ASTIndex,
    filter: Option<ASTTypeFilter>,
    info: ASTTypeCheckInfo,
}

impl ASTTypeCheckEntry {
    fn new(index: ASTIndex, filter: Option<ASTTypeFilter>, info: ASTTypeCheckInfo) -> Self {
        Self {
            index,
            filter,
            info,
        }
    }

    pub fn filter(&self) -> &Option<ASTTypeFilter> {
        &self.filter
    }

    pub fn info(&self) -> &ASTTypeCheckInfo {
        &self.info
    }

    pub fn index(&self) -> &ASTIndex {
        &self.index
    }

    fn primitive(index: ASTIndex, filter: ASTTypeFilter, len: usize) -> Self {
        Self::new(index, Some(filter), ASTTypeCheckInfo::Value(len))
    }

    fn reference(
        index: ASTIndex,
        filter: ASTTypeFilter,
        name: String,
        ref_index: ASTIndex,
    ) -> Self {
        Self::new(index, Some(filter), ASTTypeCheckInfo::Ref(name, ref_index))
    }

    fn param(index: ASTIndex, filter: ASTTypeFilter, name: String) -> Self {
        Self::new(index, Some(filter), ASTTypeCheckInfo::Param(name))
    }

    fn lambda(index: ASTIndex, filter: ASTTypeFilter) -> Self {
        Self::new(index, Some(filter), ASTTypeCheckInfo::Lambda)
    }

    pub fn exact_type(&self) -> Option<(&ASTType, &ModuleInfo)> {
        if let Some(ASTTypeFilter::Exact(ref e, ref info)) = self.filter {
            Some((e, info))
        } else {
            None
        }
    }

    pub fn exact_filter(&self) -> Option<&ASTTypeFilter> {
        if let Some(ASTTypeFilter::Exact(_, _)) = self.filter {
            self.filter.as_ref()
        } else {
            None
        }
    }

    pub fn exact_filter_not_generic(&self) -> Option<&ASTTypeFilter> {
        if let Some(ASTTypeFilter::Exact(ref t, _)) = self.filter {
            if t.is_generic() {
                return None;
            }
            self.filter.as_ref()
        } else {
            None
        }
    }

    pub fn is_exact_not_generic(&self) -> bool {
        if let Some(ASTTypeFilter::Exact(ref t, _)) = self.filter {
            !t.is_generic()
        } else {
            false
        }
    }

    pub fn is_exact(&self) -> bool {
        matches!(self.filter, Some(ASTTypeFilter::Exact(_, _)))
    }
}

impl Display for ASTTypeCheckEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref filter) = self.filter {
            write!(f, "{filter}")?;
        } else {
            f.write_str("no type determined for ")?;
        }
        write!(f, " {}", self.info)
    }
}

#[derive(Debug, Clone)]
pub struct ASTTypeCheckerResult {
    map: HashMap<usize, Arc<ASTTypeCheckEntry>>,
}

impl ASTTypeCheckerResult {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn get_by_index(&self, index: &ASTIndex) -> Option<&Arc<ASTTypeCheckEntry>> {
        self.get(index.position().id)
    }

    pub fn get(&self, id: usize) -> Option<&Arc<ASTTypeCheckEntry>> {
        self.map.get(&id)
    }

    pub fn extend(&mut self, other: ASTTypeCheckerResult) {
        self.map.extend(other.map);
    }

    pub fn remove(&mut self, id: &usize) -> Option<Arc<ASTTypeCheckEntry>> {
        self.map.remove(id)
    }

    pub fn find_by_index(&self, index: &ASTIndex) -> Vec<(&usize, &Arc<ASTTypeCheckEntry>)> {
        self.map.iter().filter(|it| it.1.index() == index).collect()
    }
}

pub struct ASTTypeChecker<'a> {
    parent: Option<&'a ASTTypeCheckerResult>,
    result: ASTTypeCheckerResult,
    errors: LinkedHashMap<usize, ASTTypeCheckError>,
}

impl<'a> ASTTypeChecker<'a> {
    pub fn new() -> Self {
        Self {
            parent: None,
            result: ASTTypeCheckerResult::new(),
            errors: LinkedHashMap::new(),
        }
    }

    pub fn with_parent(parent: &'a ASTTypeCheckerResult) -> Self {
        Self {
            parent: Some(parent),
            result: ASTTypeCheckerResult::new(),
            errors: LinkedHashMap::new(),
        }
    }

    pub fn get(&self, id: usize) -> Option<&Arc<ASTTypeCheckEntry>> {
        self.result
            .get(id)
            .or_else(|| self.parent.as_ref().and_then(|it| it.get(id)))
    }

    pub fn errors(&self) -> &LinkedHashMap<usize, ASTTypeCheckError> {
        &self.errors
    }

    pub fn values(&self) -> &HashMap<usize, Arc<ASTTypeCheckEntry>> {
        &self.result.map
    }

    pub fn result(&self) -> &ASTTypeCheckerResult {
        &self.result
    }

    pub fn get_exact_non_generics(self) -> Vec<(usize, Arc<ASTTypeCheckEntry>)> {
        self.result
            .map
            .into_iter()
            .filter(|it| it.1.is_exact_not_generic())
            .collect_vec()
    }

    pub fn get_exact(self) -> Vec<(usize, Arc<ASTTypeCheckEntry>)> {
        self.result
            .map
            .into_iter()
            .filter(|it| it.1.is_exact())
            .collect_vec()
    }

    fn insert(&mut self, index: ASTIndex, entry: ASTTypeCheckEntry) -> Arc<ASTTypeCheckEntry> {
        self.insert_by_id(index.position().id, entry)
    }

    fn insert_by_id(&mut self, id: usize, entry: ASTTypeCheckEntry) -> Arc<ASTTypeCheckEntry> {
        let entry = Arc::new(entry);
        self.insert_arc_by_id(id, entry.clone());
        entry
    }

    fn insert_arc_by_id(&mut self, id: usize, entry: Arc<ASTTypeCheckEntry>) {
        if let Some((ast_type, _)) = entry.exact_type() {
            if !ast_type.is_generic() {
                self.errors.remove(&id);
            }
        }

        debug_i!("ast_type_check added: {entry} : {}", entry.index());
        self.result.map.insert(id, entry);
    }

    fn add_error(&mut self, kind: ASTTypeCheckErroKind, index: ASTIndex, message: String) {
        debug_i!("ast_type_check error: {kind} : {index} : {message}");

        self.errors.insert(
            index.position().id,
            ASTTypeCheckError::new(kind, index, message),
        );
    }

    fn add_error_with_inner(
        &mut self,
        kind: ASTTypeCheckErroKind,
        index: ASTIndex,
        message: String,
        inner: Vec<ASTTypeCheckError>,
    ) {
        debug_i!("ast_type_check error: {kind} : {index} : {message}");

        self.errors.insert(
            index.position().id,
            ASTTypeCheckError::new_with_inner(kind, index, message, inner),
        );
    }

    pub fn from_modules_container(modules_container: &ASTModulesContainer) -> (Self, ValContext) {
        let mut type_checker = ASTTypeChecker::new();
        let mut static_val_context = ValContext::new(None);

        let modules = modules_container.modules();

        let mut functions_count = 0;
        for (id, namespace, module) in modules.iter() {
            let mut val_context = ValContext::new(None);

            if let Result::Ok(return_type) = type_checker.add_body(
                &mut val_context,
                &mut static_val_context,
                &module.body,
                None,
                &namespace,
                &id,
                &modules_container,
                None,
            ) {
                if let Some(filter) = return_type.exact_filter_not_generic() {
                    if let ASTTypeFilter::Exact(t, _) = filter {
                        if !t.is_generic() && !t.is_unit() {
                            let index = ASTIndex::new(
                                (*namespace).clone(),
                                (*id).clone(),
                                module.body.last().unwrap().position().clone(),
                            );
                            type_checker.add_error(
                                ASTTypeCheckErroKind::Fatal,
                                index,
                                "main body cannot return a value".to_owned(),
                            );
                        }
                    }
                }
            }

            functions_count += module.functions.len();
        }

        let functions_data = modules.into_iter().flat_map(|(id, namespace, module)| {
            module
                .functions
                .iter()
                //.filter(|f| !f.is_generic())
                .map(move |f| (id, namespace, f))
        });

        let chunks = functions_data
            .chunks(chunk_size(functions_count))
            .into_iter()
            .map(|chunk| chunk.into_iter().collect::<Vec<_>>())
            .collect_vec();

        let log_enabled = log_enabled();

        let chunk_results: Vec<(
            ASTTypeCheckerResult,
            LinkedHashMap<usize, ASTTypeCheckError>,
        )> = chunks
            // HENRY
            //.into_iter()
            .into_par_iter()
            .map(|chunk| {
                enable_log(log_enabled);

                let mut type_checker_chunk = ASTTypeChecker::new();
                for (id, namespace, function) in chunk {
                    let found = function.name == "next"
                        && format!("{}", function.return_type).contains("Internal");
                    if found {
                        //println!("found {} function", function.name);
                        enable_log(true);
                    }

                    //println!("adding function: {function}");
                    debug_i!("adding function: {function}");
                    indent!();
                    //let start = Instant::now();

                    type_checker_chunk.add_function(
                        function,
                        &static_val_context,
                        namespace,
                        id,
                        modules_container,
                    );

                    /*
                    let elapsed = start.elapsed();
                    if elapsed > Duration::from_millis(50) {
                        println!(
                            "function {function} took {} ms : {}",
                            elapsed.as_millis(),
                            ASTIndex::new(namespace.clone(), id.clone(), function.position.clone())
                        );
                    }
                    */

                    dedent!();

                    if found {
                        enable_log(false);
                    }
                }
                (type_checker_chunk.result, type_checker_chunk.errors)
            })
            .collect();

        for (results_map, errors_vec) in chunk_results {
            type_checker.result.extend(results_map);
            type_checker.errors.extend(errors_vec);
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
        // let start = Instant::now();
        let mut val_context = ValContext::new(None);

        let generics_prefix =
            ASTFunctionSignature::from_def(function).generics_prefix(&module_namespace.safe_name());

        for par in &function.parameters {
            self.check_valid_type(
                modules_container,
                module_namespace,
                module_id,
                &function.modifiers,
                &par.ast_type,
            );
            let position = par.position.clone();
            let par = fix_ast_par_generics(par.clone(), &generics_prefix);
            if let Err(e) =
                val_context.insert_par(par.name.clone(), par, module_namespace, module_id)
            {
                self.add_error(
                    ASTTypeCheckErroKind::Error,
                    ASTIndex::new(module_namespace.clone(), module_id.clone(), position),
                    e,
                );
            }
        }

        self.check_valid_type(
            modules_container,
            module_namespace,
            module_id,
            &function.modifiers,
            &function.return_type,
        );

        // in function body cannot be consts, but we need to know already defined ones...
        let mut tmp_static_val_context = ValContext::new(Some(static_val_context));

        match &function.body {
            ASTFunctionBody::RASMBody(body) => {
                let rt = if function.return_type.is_generic() {
                    &add_generic_prefix(function.return_type.clone(), &generics_prefix)
                } else {
                    &function.return_type
                };
                // TODO return_type
                if let Err(e) = self.add_body(
                    &mut val_context,
                    &mut tmp_static_val_context,
                    body,
                    Some(rt),
                    module_namespace,
                    module_id,
                    modules_container,
                    Some(function),
                ) {
                    debug_i!("errors adding function {function} : {}", SliceDisplay(&e));
                }
            }
            ASTFunctionBody::NativeBody(_body) => {}
        }

        /*
        let elapsed = start.elapsed().as_millis();
        if elapsed > 0 {
            println!(
                "ast_type_check function {} in {}micros : {}",
                function.name,
                start.elapsed().as_micros(),
                ASTIndex::new(
                    module_namespace.clone(),
                    module_id.clone(),
                    function.position.clone()
                )
            );
        }
        */
    }

    fn check_valid_type(
        &mut self,
        modules_container: &ASTModulesContainer,
        module_namespace: &ModuleNamespace,
        module_id: &ModuleId,
        function_modifiers: &ASTModifiers,
        ast_type: &ASTType,
    ) {
        if let ASTType::ASTCustomType {
            name,
            param_types: _,
            position,
        } = ast_type
        {
            let index = ASTIndex::new(
                module_namespace.clone(),
                module_id.clone(),
                position.clone(),
            );
            if let Some((info, def)) = modules_container.custom_type_def(module_namespace, name) {
                if !Self::is_valid_modifier(
                    function_modifiers,
                    module_namespace,
                    def.modifiers(),
                    info.namespace(),
                ) {
                    self.add_error(
                        ASTTypeCheckErroKind::Fatal,
                        index,
                        format!("{name} visibility is not compatible with the visibility of the function"),
                    );
                }
            } else {
                self.add_error(
                    ASTTypeCheckErroKind::Fatal,
                    index,
                    format!("{name} is not defined"),
                );
            }
        }
    }

    fn is_valid_modifier(
        outer: &ASTModifiers,
        outer_namespace: &ModuleNamespace,
        inner: &ASTModifiers,
        inner_namespace: &ModuleNamespace,
    ) -> bool {
        match outer {
            ASTModifiers::Public => inner == &ASTModifiers::Public,
            ASTModifiers::Private => true,
            ASTModifiers::Internal(outer_internal) => match inner {
                ASTModifiers::Public => true,
                ASTModifiers::Private => false,
                ASTModifiers::Internal(inner_internals) => {
                    let oii = outer_namespace.internal().to_owned();
                    let iii = inner_namespace.internal().to_owned();
                    let oi = outer_internal.as_ref().unwrap_or(&oii);
                    let ii = inner_internals.as_ref().unwrap_or(&iii);
                    oi == ii
                }
            },
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
    ) -> Result<Arc<ASTTypeCheckEntry>, Vec<ASTTypeCheckError>> {
        if body.is_empty() {
            return Ok(Arc::new(ASTTypeCheckEntry {
                index: ASTIndex::none(),
                filter: Some(ASTTypeFilter::Exact(
                    ASTType::ASTUnitType,
                    ModuleInfo::global(),
                )),
                info: ASTTypeCheckInfo::Lambda, // TODO
            }));
        }
        let last_statement_index = body.len() - 1;
        let mut return_type = None;
        let inner_val_context = &mut ValContext::new(Some(val_context));

        let mut errors = Vec::new();
        for (i, statement) in body.iter().enumerate() {
            match statement {
                ASTStatement::ASTExpressionStatement(e, _) => {
                    if i == last_statement_index {
                        let entry = if let Some(ref elst) = expected_last_statement_type {
                            /*
                            if format!("{elst}") == "fn () -> T" {
                                println!("found expected last statement type {}", elst);
                            }
                            */

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
                            )
                        };

                        if let Some(rt) = entry {
                            // can I do something when is generic? Take in account that it can be generic on something different
                            //if !rt.is_generic_or_any() {
                            return_type = Some(rt.clone());
                            //}
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

                    if let Some(error) = self.errors.get(&e.position().id) {
                        errors.push(error.clone());
                    }
                }
                ASTStatement::ASTLetStatement(name, e, position) => {
                    self.add_binding(
                        name,
                        e,
                        position,
                        false,
                        None,
                        inner_val_context,
                        statics,
                        module_namespace,
                        module_id,
                        modules_container,
                        function,
                    );
                    if let Some(error) = self.errors.get(&e.position().id) {
                        errors.push(error.clone());
                    }
                }
                ASTStatement::ASTConstStatement(name, e, position, astmodifiers) => {
                    self.add_binding(
                        name,
                        e,
                        position,
                        true,
                        Some(astmodifiers),
                        inner_val_context,
                        statics,
                        module_namespace,
                        module_id,
                        modules_container,
                        function,
                    );
                    if let Some(error) = self.errors.get(&e.position().id) {
                        errors.push(error.clone());
                    }
                }
            }
        }

        return_type.ok_or(errors)
    }

    fn add_binding(
        &mut self,
        name: &str,
        e: &ASTExpression,
        position: &rasm_parser::parser::ast::ASTPosition,
        is_const: bool,
        astmodifiers: Option<&ASTModifiers>,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        module_namespace: &ModuleNamespace,
        module_id: &ModuleId,
        modules_container: &ASTModulesContainer,
        function: Option<&ASTFunctionDef>,
    ) {
        let entry = self.add_expr(
            e,
            val_context,
            statics,
            None,
            module_namespace,
            module_id,
            modules_container,
            function,
        );

        let index = ASTIndex::new(
            module_namespace.clone(),
            module_id.clone(),
            position.clone(),
        );

        if let Some(entry) = entry {
            if let Some(filter) = &entry.filter {
                let error_msg = if let ASTTypeFilter::Exact(ast_type, _module_info) = filter {
                    let insert_result = if is_const {
                        statics.insert_const(
                            name.to_string(),
                            ast_type.clone(),
                            &index,
                            astmodifiers.unwrap(),
                        )
                    } else {
                        val_context.insert_let(name.to_string(), ast_type.clone(), &index)
                    };
                    insert_result.err()
                } else {
                    None
                };

                let filter_clone = entry.filter.clone();

                if let Some(e) = error_msg {
                    self.add_error(ASTTypeCheckErroKind::Error, index.clone(), e);
                }

                if is_const {
                    self.insert(
                        index.clone(),
                        ASTTypeCheckEntry::new(
                            index,
                            filter_clone,
                            ASTTypeCheckInfo::Const(name.to_string()),
                        ),
                    );
                } else {
                    self.insert(
                        index.clone(),
                        ASTTypeCheckEntry::new(
                            index,
                            filter_clone,
                            ASTTypeCheckInfo::Let(name.to_string()),
                        ),
                    );
                }
            }
        } else {
            self.add_error(
                ASTTypeCheckErroKind::Error,
                index.clone(),
                format!("Could not infer type for {name}"),
            );
        }
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
    ) -> Option<Arc<ASTTypeCheckEntry>> {
        debug_i!(
            "add_expr {expr} expected {}",
            OptionDisplay(&expected_expression_type)
        );
        indent!();

        if let Some(r) = self.get(expr.position().id) {
            // If the expression is already cached and it is not generic, we can use it. We can use it
            // even if it's generic, but we have not the expected type, since we don't know
            // how to resolve it further
            if r.is_exact_not_generic() || expected_expression_type.is_none() {
                let index = ASTIndex::new(
                    module_namespace.clone(),
                    module_id.clone(),
                    expr.position().clone(),
                );
                debug_i!("Cached {r} : {index}");
                dedent!();

                return Some(r.clone());
            }
        }

        let index = ASTIndex::new(
            module_namespace.clone(),
            module_id.clone(),
            expr.position().clone(),
        );

        let entry = match expr {
            ASTExpression::ASTFunctionCallExpression(call) => self.add_call(
                &index,
                call,
                val_context,
                statics,
                expected_expression_type,
                modules_container,
                function,
            ),
            ASTExpression::ASTValueRefExpression(name, _) => self.add_value_ref_expr(
                &index,
                name,
                val_context,
                statics,
                expected_expression_type,
                modules_container,
                function,
            ),
            ASTExpression::ASTValueExpression(value_type, _position) => Some(self.insert(
                index.clone(),
                ASTTypeCheckEntry::primitive(
                    index.clone(),
                    ASTTypeFilter::Exact(
                        value_type.to_type(),
                        ModuleInfo::new(module_namespace.clone(), module_id.clone()),
                    ),
                    value_type.token_len(),
                ),
            )),
            ASTExpression::ASTLambdaExpression(lambda) => self.add_lambda_expr(
                &index,
                lambda,
                val_context,
                statics,
                expected_expression_type,
                modules_container,
                function,
            ),
        };

        // the resolved type could be generic on a different generic type, we want to resolve it
        // with the generic type of the expected type

        if let Some(eet) = expected_expression_type {
            if eet.is_generic() {
                if let Some(entry) = &entry {
                    if let Some(ASTTypeFilter::Exact(et, e_module_id)) = &entry.filter {
                        if et.is_generic() {
                            if let Ok(rgt) =
                                ASTResolvedGenericTypes::resolve_generic_types_from_effective_type(
                                    et, eet, &index,
                                )
                            {
                                if let Some(rt) = rgt.substitute(et) {
                                    dedent!();
                                    return Some(self.insert(
                                        index.clone(),
                                        ASTTypeCheckEntry::new(
                                            index,
                                            Some(ASTTypeFilter::Exact(rt, e_module_id.clone())),
                                            entry.info.clone(),
                                        ),
                                    ));
                                }
                            }
                        }
                    }
                }
            }
        }

        dedent!();
        entry
    }

    fn add_value_ref_expr(
        &mut self,
        index: &ASTIndex,
        name: &str,
        val_context: &ValContext,
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        modules_container: &ASTModulesContainer,
        function: Option<&ASTFunctionDef>,
    ) -> Option<Arc<ASTTypeCheckEntry>> {
        debug_i!(
            "add_value_ref_expr {name} expected {}",
            OptionDisplay(&expected_expression_type)
        );
        let module_namespace = index.module_namespace();
        let module_id = index.module_id();
        if let Some(kind) = val_context.get(name, module_namespace) {
            Some(self.insert(
                index.clone(),
                ASTTypeCheckEntry::reference(
                    index.clone(),
                    ASTTypeFilter::exact(kind.ast_type().clone(), module_namespace, module_id),
                    name.to_owned(),
                    kind.index(module_namespace, module_id),
                ),
            ))
        } else if let Some(kind) = statics.get_const(name, module_namespace) {
            Some(self.insert(
                index.clone(),
                ASTTypeCheckEntry::reference(
                    index.clone(),
                    ASTTypeFilter::exact(kind.ast_type().clone(), module_namespace, module_id),
                    name.to_owned(),
                    kind.index(module_namespace, module_id),
                ),
            ))
        } else {
            indent!();
            let mut function_references =
                if let Some(ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                    parameters,
                    return_type,
                })) = expected_expression_type
                {
                    let module_info = ModuleInfo::new(module_namespace.clone(), module_id.clone());
                    let function_references = modules_container
                        .find_call_vec(
                            name,
                            &None, //TODO
                            parameters
                                .iter()
                                .map(move |it| {
                                    ASTTypeFilter::Exact(it.clone(), module_info.clone())
                                })
                                .collect_vec()
                                .as_ref(),
                            Some(&return_type.clone()),
                            module_namespace,
                            index,
                        )
                        .into_iter()
                        .filter(|it| {
                            let function_is_macro = function
                                .map_or(false, |f| get_macro_result_type(&f.return_type).is_some());

                            if function_is_macro {
                                true
                            } else {
                                let current_function_is_macro =
                                    get_macro_result_type(&it.signature.return_type).is_some();
                                if current_function_is_macro {
                                    // println!("main function ({}) is not macro, current function ({it}) is, we skip", OptionDisplay(&function) );
                                }
                                !current_function_is_macro
                            }
                        })
                        .collect_vec();

                    if function_references.len() > 1 {
                        if let Some(rt) = Self::get_return_type_from_signatures(
                            &function_references
                                .iter()
                                .map(|it| &it.signature)
                                .collect_vec(),
                        ) {
                            let module_info =
                                ModuleInfo::new(module_namespace.clone(), module_id.clone());
                            dedent!();
                            return Some(
                                self.insert(
                                    index.clone(),
                                    ASTTypeCheckEntry::new(
                                        index.clone(),
                                        Some(ASTTypeFilter::Lambda(
                                            parameters.len(),
                                            Some(Box::new(ASTTypeFilter::Exact(
                                                rt.clone(),
                                                module_info.clone(),
                                            ))),
                                        )),
                                        ASTTypeCheckInfo::Call(
                                            name.to_owned(),
                                            function_references
                                                .into_iter()
                                                .map(|it| {
                                                    (it.signature.clone(), it.index().clone())
                                                })
                                                .collect_vec(),
                                            false,
                                        ),
                                    ),
                                ),
                            );
                        }
                    }

                    function_references
                } else {
                    if let Some(signatures) = modules_container.get_signatures(name) {
                        signatures
                            .iter()
                            .filter(|it| {
                                if !it
                                    .namespace
                                    .visible_from(&it.signature.modifiers, &module_namespace)
                                {
                                    return false;
                                } else {
                                    let function_is_macro = function.map_or(false, |f| {
                                        get_macro_result_type(&f.return_type).is_some()
                                    });

                                    if function_is_macro {
                                        true
                                    } else {
                                        let current_function_is_macro =
                                            get_macro_result_type(&it.signature.return_type)
                                                .is_some();
                                        if current_function_is_macro {
                                            // println!("main function ({}) is not macro, current function ({it}) is, we skip", OptionDisplay(&function) );
                                        }
                                        !current_function_is_macro
                                    }
                                }
                            })
                            .collect_vec()
                    } else {
                        Vec::new()
                    }
                };

            if function_references.len() == 1 {
                let fun_entry = function_references.pop().unwrap();

                let lambda =
                    if let Some(ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                        parameters,
                        return_type,
                    })) = expected_expression_type
                    {
                        let new_parameters = zip(&fun_entry.signature.parameters_types, parameters)
                            .map(|(p, t)| {
                                if p.is_generic() && !t.is_generic() {
                                    t.clone()
                                } else {
                                    p.clone()
                                }
                            })
                            .collect::<Vec<_>>();

                        let new_return_type = if fun_entry.signature.return_type.is_generic()
                            && !return_type.is_generic()
                        {
                            return_type
                        } else {
                            &fun_entry.signature.return_type
                        };

                        ASTBuiltinTypeKind::ASTLambdaType {
                            parameters: new_parameters,
                            return_type: Box::new(new_return_type.clone()),
                        }
                    } else {
                        ASTBuiltinTypeKind::ASTLambdaType {
                            parameters: fun_entry.signature.parameters_types.clone(),
                            return_type: Box::new(fun_entry.signature.return_type.clone()),
                        }
                    };
                dedent!();
                Some(self.insert(
                    index.clone(),
                    ASTTypeCheckEntry::new(
                        index.clone(),
                        Some(ASTTypeFilter::exact(
                            ASTType::ASTBuiltinType(lambda),
                            module_namespace,
                            module_id,
                        )),
                        ASTTypeCheckInfo::ReferenceToFunction(
                            fun_entry.signature.clone(),
                            fun_entry.index().clone(),
                        ),
                    ),
                ))
            } else {
                if function_references.len() > 1 {
                    let is_generic = function.map(ASTFunctionDef::is_generic).unwrap_or(false);
                    self.add_error(
                        if is_generic {
                            ASTTypeCheckErroKind::Warning
                        } else {
                            ASTTypeCheckErroKind::Error
                        },
                        index.clone(),
                        format!(
                            "Cannot find unique function {name}: {}",
                            SliceDisplay(&function_references)
                        ),
                    );
                } else {
                    self.add_error(
                        ASTTypeCheckErroKind::Error,
                        index.clone(),
                        format!("Cannot find function {name}"),
                    );
                }
                dedent!();
                None
            }
        }
    }

    fn add_lambda_expr(
        &mut self,
        index: &ASTIndex,
        lambda: &ASTLambdaDef,
        val_context: &ValContext,
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        modules_container: &ASTModulesContainer,
        function: Option<&ASTFunctionDef>,
    ) -> Option<Arc<ASTTypeCheckEntry>> {
        let module_namespace = index.module_namespace();
        let module_id = index.module_id();
        let mut lambda_val_context = ValContext::new(Some(val_context));

        let expected_last_statement_type_and_parameters =
            if let Some(ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
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
                        self.add_error(ASTTypeCheckErroKind::Error, par_index, e);
                    } else {
                        self.insert(
                            par_index.clone(),
                            ASTTypeCheckEntry::param(
                                par_index,
                                ASTTypeFilter::exact(ast_type.clone(), module_namespace, module_id),
                                name.to_owned(),
                            ),
                        );
                    }
                }

                Some((return_type.as_ref(), parameters))
            } else {
                if let Some(eet) = expected_expression_type {
                    if !eet.is_strictly_generic() {
                        self.add_error(
                            ASTTypeCheckErroKind::Error,
                            index.clone(),
                            format!("Expected lambda but found {eet}"),
                        );
                        return None;
                    }
                }
                if !lambda.parameter_names.is_empty() {
                    return Some(self.insert(
                        index.clone(),
                        ASTTypeCheckEntry::lambda(
                            index.clone(),
                            ASTTypeFilter::Lambda(lambda.parameter_names.len(), None),
                        ),
                    ));
                } else {
                    None
                }
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

        match body_return_type {
            Ok(entry) => {
                if let Some(ASTTypeFilter::Exact(brt, _position)) = entry.filter.clone() {
                    let type_filter = if let Some((_return_type, parameters)) =
                        expected_last_statement_type_and_parameters
                    {
                        ASTTypeFilter::Exact(
                            ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                                parameters: parameters.clone(),
                                return_type: Box::new(brt),
                            }),
                            ModuleInfo::new(module_namespace.clone(), module_id.clone()),
                        )
                    } else {
                        if lambda.parameter_names.is_empty() {
                            ASTTypeFilter::Lambda(
                                lambda.parameter_names.len(),
                                Some(Box::new(ASTTypeFilter::Exact(
                                    brt,
                                    ModuleInfo::new(module_namespace.clone(), module_id.clone()),
                                ))),
                            )
                        } else {
                            ASTTypeFilter::Lambda(lambda.parameter_names.len(), None)
                        }
                    };
                    Some(self.insert(
                        index.clone(),
                        ASTTypeCheckEntry::lambda(index.clone(), type_filter),
                    ))
                } else {
                    /*
                    let type_filter = if let Some((return_type, parameters)) =
                        expected_last_statement_type_and_parameters
                    {
                        ASTTypeFilter::Exact(
                            ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                                parameters: parameters.clone(),
                                return_type: Box::new(return_type.clone()),
                            }),
                            ModuleInfo::new(module_namespace.clone(), module_id.clone()),
                        )
                    } else {
                        ASTTypeFilter::Lambda(lambda.parameter_names.len(), None)
                    };
                    Some(self.insert(
                        index.clone(),
                        ASTTypeCheckEntry::lambda(index.clone(), type_filter),
                    ))
                    */
                    None
                }
            }
            Err(error) => {
                self.add_error_with_inner(
                    ASTTypeCheckErroKind::Error,
                    index.clone(),
                    format!("Invalid lambda {lambda}"),
                    error,
                );
                None
            }
        }
    }

    fn add_call(
        &mut self,
        index: &ASTIndex,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        modules_container: &ASTModulesContainer,
        function: Option<&ASTFunctionDef>,
    ) -> Option<Arc<ASTTypeCheckEntry>> {
        let inside_a_generic_function = function.map_or(false, |f| f.is_generic());

        let found = false; //function.map(|it| it.name == "generic").unwrap_or(false);
        // call.function_name() == "append" && format!("{index}").contains("3:71");

        if found {
            // println!("found: {}", call.function_name());
            enable_log(true);
        }

        if let Some(r) = self.get(call.position().id) {
            // If the expression is already cached and it is not generic, we can use it. We can use it
            // even if it's generic, but we have not the expected type, since we don't know
            // how to resolve it further
            if r.is_exact_not_generic() || expected_expression_type.is_none() {
                debug_i!("Cached {r} : {index}");

                return Some(r.clone());
            }
        }

        let module_namespace = index.module_namespace();
        let module_id = index.module_id();

        debug_i!(
            "add_call {call} expected_expression_type {} : {index}",
            OptionDisplay(&expected_expression_type)
        );

        indent!();

        if let Some((lambda_return_type, parameters_types)) =
            val_context.get_lambda(call.function_name(), module_namespace)
        {
            let return_type = lambda_return_type.as_ref().clone();
            let parameters_types = parameters_types.clone();
            let generics = parameters_types
                .iter()
                .filter_map(|p| {
                    if let ASTType::ASTGenericType(_, g_name, _var_types) = p {
                        Some(g_name.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            let lambda_signature = ASTFunctionSignature {
                name: call.function_name().clone(),
                parameters_types,
                return_type,
                generics,
                modifiers: ASTModifiers::Public, // TODO is it right?
                associated_type: call.associated_type().clone(),
            };

            let entry = ASTFunctionSignatureEntry::new(
                lambda_signature,
                module_namespace.clone(),
                module_id.clone(),
                call.position().clone(),
                call.associated_type().clone(),
            );

            let result = self.process_function_signature(
                &entry,
                call,
                val_context,
                statics,
                expected_expression_type,
                true,
                modules_container,
                function,
                index,
            );

            dedent!();

            return result;
        }

        let start = Instant::now();

        let candidate_functions = modules_container
            .get_signatures(&call.function_name())
            .map(|it| {
                let functions_iter = it
                    .iter()
                    .filter(|entry| {
                        entry.signature.parameters_types.len() == call.parameters().len()
                            && entry
                                .namespace
                                .visible_from(&entry.signature.modifiers, module_namespace)
                    })
                    .filter(|it| {
                        call.associated_type()
                            .as_ref()
                            .map(|t| match &it.associated_type {
                                Some(associated_type) => {
                                    t == associated_type
                                        || function
                                            .map(|f| f.generic_types.contains(t))
                                            .unwrap_or(false)
                                }
                                _ => false,
                            })
                            .unwrap_or(true)
                    })
                    .filter(|it| {
                        if call.is_macro() {
                            get_macro_result_type(&it.signature.return_type).is_some()
                        } else {
                            // TODO a function that is a macro, can be called inside a non macro function,
                            // it is what does the macro compiler itself to resolve an expression macro, it calls it inside the
                            // auto generated function that resolves that call...
                            true
                        }
                    });

                let functions_iter = functions_iter.filter_map(move |it| {
                    if let Some(et) = &expected_expression_type {
                        if found {
                            println!("index {} -> {et} : {it}", it.signature.return_type);
                        }
                        if modules_container.is_compatible(
                            et,
                            module_namespace,
                            &it.signature.return_type,
                            &it.namespace,
                        ) {
                            Some(it)
                        } else {
                            None
                        }
                    } else {
                        Some(it)
                    }
                });
                functions_iter.collect_vec()
            })
            .unwrap_or(Vec::new());

        let mut inner_errors = Vec::new();

        if candidate_functions.is_empty() {
            self.add_error(
                ASTTypeCheckErroKind::Error,
                index.clone(),
                format!(
                    "no functions for {}, expected expression type: {}, call associated_type: {}",
                    call.function_name(),
                    OptionDisplay(&expected_expression_type),
                    OptionDisplay(call.associated_type())
                ),
            );
            dedent!();
            if found {
                enable_log(false);
            }
            return None;
        } else {
            let mut compatible_functions: Vec<(
                &ASTFunctionSignatureEntry,
                Vec<(usize, Arc<ASTTypeCheckEntry>)>,
                ASTTypeCheckerResult,
            )> = Vec::new();
            if call.parameters().is_empty() {
                for signature in candidate_functions.into_iter() {
                    compatible_functions.push((signature, Vec::new(), ASTTypeCheckerResult::new()));
                }
            } else {
                /*
                   Here we try to fill the cache with some simple expressions.
                   But it seems that it's not faster
                */

                if false && compatible_functions.len() > 1 {
                    let mut internal_type_checker = ASTTypeChecker::with_parent(&self.result);

                    for (i, e) in call.parameters().iter().enumerate() {
                        if matches!(e, ASTExpression::ASTLambdaExpression(_))
                            || matches!(e, ASTExpression::ASTFunctionCallExpression(_))
                        {
                            continue;
                        }

                        debug_i!("trying to resolve parameter {i}: {e}");
                        indent!();

                        internal_type_checker.add_expr(
                            e,
                            val_context,
                            statics,
                            None,
                            module_namespace,
                            module_id,
                            modules_container,
                            function,
                        );

                        dedent!();
                    }

                    for (id, entry) in internal_type_checker.get_exact() {
                        self.insert_arc_by_id(id, entry);
                    }
                }

                for signature in candidate_functions.into_iter() {
                    debug_i!("trying to resolve call with signature: {signature}");
                    indent!();
                    if found {
                        println!("  signature: {signature}");
                    }
                    let mut resolved_generic_types = ASTResolvedGenericTypes::new();

                    self.resolve_signature_generics_with_call_generics(
                        &signature.signature,
                        call,
                        &mut resolved_generic_types,
                        function,
                        index,
                    );

                    if let Some(et) = &expected_expression_type {
                        if signature.signature.return_type.is_generic() {
                            // TODO collect errors?
                            if let Ok(rgt) =
                                ASTResolvedGenericTypes::resolve_generic_types_from_effective_type(
                                    &signature.signature.return_type,
                                    *et,
                                    index,
                                )
                            {
                                if resolved_generic_types.extend(rgt).is_err() {
                                    dedent!();
                                    debug_i!("error resolving generic types");
                                    continue;
                                }
                            } else {
                                dedent!();
                                debug_i!("error resolving generic types");
                                continue;
                            }
                        }
                    }

                    let mut resolved = resolved_generic_types.len();
                    let mut parameter_types_filters: Vec<Option<(usize, Arc<ASTTypeCheckEntry>)>> =
                        vec![None; call.parameters().len()];
                    let mut resolved_signature_types: Vec<Option<ASTType>> =
                        vec![None; call.parameters().len()];

                    let mut internal_type_checker = ASTTypeChecker::with_parent(&self.result);
                    let mut invalid_function = false;

                    let mut function_errors = Vec::new();

                    loop {
                        // We only need to re-resolve a parameter if its type changed since the last
                        // iteration (a generic got bound), or if it was never successfully resolved.
                        // Parameters whose type is unchanged already hold an entry resolved with the
                        // final bindings, so re-resolving them would repeat the same work.
                        // By opencode.
                        let mut to_resolve = Vec::new();
                        for i in 0..call.parameters().len() {
                            let signature_type =
                                signature.signature.parameters_types.get(i).unwrap();

                            // A non-generic parameter type can never change between iterations and,
                            // once successfully resolved, there is nothing left to do for it here.
                            // By opencode.
                            if !signature_type.is_generic()
                                && resolved_signature_types[i].is_some()
                                && parameter_types_filters[i].is_some()
                            {
                                continue;
                            }

                            let resolved_signature_type = if signature_type.is_generic() {
                                if let Some(t) = resolved_generic_types.substitute(signature_type) {
                                    debug_i!("substituted {signature_type} to {t}");
                                    t
                                } else {
                                    signature_type.clone()
                                }
                            } else {
                                signature_type.clone()
                            };

                            if resolved_signature_types[i].as_ref()
                                != Some(&resolved_signature_type)
                                || parameter_types_filters[i].is_none()
                            {
                                resolved_signature_types[i] = Some(resolved_signature_type);
                                to_resolve.push(i);
                            }
                        }

                        if to_resolve.is_empty() {
                            break;
                        }

                        //internal_type_checker = ASTTypeChecker::with_parent(&self.result);
                        function_errors.clear();

                        debug_i!("loop resolved generic types: {resolved_generic_types}");
                        indent!();
                        //let mut val_context = val_context.clone();
                        for i in to_resolve {
                            let e = &call.parameters()[i];
                            debug_i!("trying to resolve parameter {i}: {e}");
                            indent!();
                            let signature_type =
                                signature.signature.parameters_types.get(i).unwrap();

                            let resolved_signature_type = if signature_type.is_generic() {
                                if let Some(t) = resolved_generic_types.substitute(signature_type) {
                                    debug_i!("substituted {signature_type} to {t}");
                                    t
                                } else {
                                    signature_type.clone()
                                }
                            } else {
                                signature_type.clone()
                            };
                            resolved_signature_types[i] = Some(resolved_signature_type.clone());

                            if let Some(entry) = internal_type_checker.add_expr(
                                e,
                                val_context,
                                statics,
                                Some(&resolved_signature_type),
                                module_namespace,
                                module_id,
                                modules_container,
                                function,
                            ) {
                                if let Some(filter) = entry.filter().as_ref() {
                                    if filter.is_compatible(
                                        &resolved_signature_type,
                                        module_namespace,
                                        modules_container,
                                    ) {
                                        if found {
                                            println!("    filter: {filter}");
                                        }
                                        if signature_type.is_generic() {
                                            if !Self::add_resolve_type_filter(
                                                entry.index(),
                                                signature_type,
                                                filter,
                                                &mut resolved_generic_types,
                                            )
                                            .is_empty()
                                            {
                                                debug_i!("cannot add resolve type filter");
                                                dedent!();
                                                continue;
                                            };
                                        }
                                        parameter_types_filters[i] = Some((e.position().id, entry));
                                        dedent!();
                                    } else {
                                        dedent!();
                                        debug_i!(
                                            "not compatible filter filter={filter}, type={resolved_signature_type}"
                                        );
                                        function_errors.push(ASTTypeCheckError::new(
                                            ASTTypeCheckErroKind::Error,
                                            ASTIndex::new(module_namespace.clone(), module_id.clone(), e.position().clone()),
                                            format!(
                                                "not compatible filter filter={filter}, type={resolved_signature_type}"
                                            ),
                                        ));
                                        // TODO we found that the expression type is not compatible with the function parameter type.
                                        //      Is it still possible that the function is the right one? Can the type of expression
                                        //      be misleading, due to the fact that the parameter type is generic, but we have not yet
                                        //      found the generic?
                                        //      For now we assume that it is not possible, so we can break, but also in the outer loop.
                                        invalid_function = true;
                                        break;
                                    }
                                } else {
                                    dedent!();
                                    debug_i!(
                                        "no filter for {e} : {}",
                                        ASTIndex::new(
                                            module_namespace.clone(),
                                            module_id.clone(),
                                            e.position().clone()
                                        )
                                    );
                                    function_errors.push(ASTTypeCheckError::new(
                                        ASTTypeCheckErroKind::Error,
                                        ASTIndex::new(
                                            module_namespace.clone(),
                                            module_id.clone(),
                                            e.position().clone(),
                                        ),
                                        format!("no filter for {e}, info={}", entry.info()),
                                    ));
                                    if !signature_type.is_generic() {
                                        invalid_function = true;
                                        break;
                                    }
                                    // if generic, we cannot break, because we need to check all parameters for eventually substitute generics
                                    // and try another time
                                }
                            } else if let Some(error) =
                                internal_type_checker.errors.get(&e.position().id)
                            {
                                function_errors.push(ASTTypeCheckError::new_with_inner(
                                    ASTTypeCheckErroKind::Error,
                                    ASTIndex::new(
                                        module_namespace.clone(),
                                        module_id.clone(),
                                        e.position().clone(),
                                    ),
                                    format!("error in expression {e}"),
                                    vec![error.clone()],
                                ));
                                dedent!();
                                debug_i!("error in expression {e}");

                                if error.kind() == &ASTTypeCheckErroKind::Fatal
                                    || !signature_type.is_generic()
                                {
                                    invalid_function = true;
                                    break;
                                }
                            } else {
                                dedent!();
                                debug_i!("not valid expression");

                                function_errors.push(ASTTypeCheckError::new(
                                    ASTTypeCheckErroKind::Error,
                                    ASTIndex::new(
                                        module_namespace.clone(),
                                        module_id.clone(),
                                        e.position().clone(),
                                    ),
                                    format!("not valid expression {e}",),
                                ));
                                if !signature_type.is_generic() {
                                    // println!("optimized error for {e}");
                                    invalid_function = true;
                                    break;
                                }
                            }
                        }

                        if invalid_function || resolved == resolved_generic_types.len() {
                            dedent!();
                            break;
                        }

                        debug_i!(
                            "repeating loop resolved_generic_types: {}",
                            resolved_generic_types
                        );

                        resolved = resolved_generic_types.len();
                        dedent!();
                    }

                    if found {
                        let rgt_format = format!("{}", resolved_generic_types);
                        println!("    resolved_generic_types: {rgt_format}");
                    }

                    if parameter_types_filters.iter().all(Option::is_some) {
                        let parameter_types_filters = parameter_types_filters
                            .into_iter()
                            .map(|it| it.unwrap())
                            .collect::<Vec<_>>();
                        compatible_functions.push((
                            signature,
                            parameter_types_filters,
                            internal_type_checker.result,
                        ));
                        dedent!();
                        debug_i!("function is compatible");
                    } else {
                        dedent!();
                        debug_i!(
                            "parameter_types_filters.iter().all(Option::is_some).len() != call.parameters().len(): {} != {}",
                            parameter_types_filters
                                .iter()
                                .filter(|it| it.is_some())
                                .count(),
                            call.parameters().len()
                        );

                        inner_errors.push(ASTTypeCheckError::new_with_inner(
                            ASTTypeCheckErroKind::Error,
                            index.clone(),
                            format!(
                                "function {signature} is not compatible : {}",
                                ASTIndex::new(
                                    module_namespace.clone(),
                                    module_id.clone(),
                                    call.position().clone(),
                                ),
                            ),
                            function_errors,
                        ));
                    }
                }
            }

            if found {
                let ct = &compatible_functions
                    .iter()
                    .map(|it| it.0.signature.clone())
                    .collect::<Vec<_>>();
                println!("compatible_functions: {}", SliceDisplay(&ct));
            }

            if compatible_functions.len() > 1
            //&& expected_expression_type.map_or(false, |e| !e.is_generic())
            {
                let len_before = compatible_functions.len();
                let mut min_rank = usize::MAX;
                for (signature, entries, _) in compatible_functions.iter() {
                    if found {
                        println!("signature: {}", signature);
                        println!(
                            "  entries: {}",
                            entries.iter().map(|it| format!("{}", it.1)).join(", ")
                        );
                    }

                    if !entries
                        .iter()
                        .all(|(_m, entry)| entry.is_exact_not_generic())
                    {
                        continue;
                    }

                    let rank: usize = signature.rank; // Self::signature_entries_rank(&signature.signature, entries);
                    min_rank = min_rank.min(rank);
                    if found {
                        println!("    rank: {}", rank);
                    }
                }

                if min_rank != usize::MAX {
                    compatible_functions.retain(|(signature, _, _)| signature.rank == min_rank);
                }

                if len_before != compatible_functions.len() {
                    debug_i!(
                        "filtered {} from {} to {}",
                        call.function_name(),
                        len_before,
                        compatible_functions.len()
                    );

                    if found {
                        let ct = &compatible_functions
                            .iter()
                            .map(|it| it.0.signature.clone())
                            .collect::<Vec<_>>();
                        println!("filtered compatible_functions: {}", SliceDisplay(&ct));
                    }
                }
            }

            let result = if compatible_functions.len() == 1 {
                let (signature, _, result) = compatible_functions.pop().unwrap();

                self.result.extend(result);

                let process_result = self.process_function_signature(
                    &signature,
                    call,
                    val_context,
                    statics,
                    expected_expression_type,
                    false,
                    modules_container,
                    function,
                    index,
                );

                dedent!();
                if found {
                    enable_log(false);
                }
                process_result
            } else if compatible_functions.len() > 1 {
                for i in 0..call.parameters().len() {
                    let mut entries = compatible_functions
                        .iter()
                        .map(|it| it.1[i].1.clone())
                        .collect_vec();
                    entries.dedup();

                    if entries.len() == 1 {
                        let entry = entries.pop().unwrap();
                        self.insert_arc_by_id(call.parameters()[i].position().id, entry);
                    }
                }

                self.add_error(
                    ASTTypeCheckErroKind::Warning,
                    index.clone(),
                    format!(
                        "found more than one valid function for {}\n{}",
                        call.function_name(),
                        compatible_functions
                            .iter()
                            .map(|it| format!(
                                "$indent  {} ->\n{}",
                                it.0.signature.clone().remove_generic_prefix(),
                                it.1.iter()
                                    .map(|it| format!("$indent    {}", it.1))
                                    .collect::<Vec<String>>()
                                    .join("\n")
                            ))
                            .join("\n")
                    ),
                );

                dedent!();

                if found {
                    enable_log(false);
                }

                let signatures = compatible_functions
                    .iter()
                    .map(|it| &it.0.signature)
                    .collect_vec();

                Some(
                    self.insert(
                        index.clone(),
                        ASTTypeCheckEntry::new(
                            index.clone(),
                            Self::get_filter_from_signatures(
                                module_namespace,
                                module_id,
                                &signatures,
                                expected_expression_type,
                            ),
                            ASTTypeCheckInfo::Call(
                                call.function_name().clone(),
                                compatible_functions
                                    .iter()
                                    .map(|it| {
                                        (
                                            it.0.signature.clone(),
                                            ASTIndex::new(
                                                it.0.namespace.clone(),
                                                it.0.module_id.clone(),
                                                it.0.position.clone(),
                                            ),
                                        )
                                    })
                                    .collect(),
                                call.is_macro(),
                            ),
                        ),
                    ),
                )
            } else {
                self.add_error_with_inner(
                    if inside_a_generic_function {
                        ASTTypeCheckErroKind::Warning
                    } else {
                        ASTTypeCheckErroKind::Error
                    },
                    index.clone(),
                    format!("cannot find a valid function for {}", call.function_name()),
                    inner_errors,
                );
                dedent!();
                if found {
                    enable_log(false);
                }

                None
            };

            let elapsed = start.elapsed();
            if elapsed.as_millis() > 1 {
                debug_i!("call {call } took {} ms : {}", elapsed.as_millis(), index);
            }

            result
        }
    }

    fn get_filter_from_signatures(
        module_namespace: &ModuleNamespace,
        module_id: &ModuleId,
        signatures: &Vec<&ASTFunctionSignature>,
        expected_expression_type: Option<&ASTType>,
    ) -> Option<ASTTypeFilter> {
        if signatures.is_empty() {
            None
        } else if let Some(rt) = Self::get_return_type_from_signatures(signatures) {
            return Some(ASTTypeFilter::exact(rt, module_namespace, module_id));
        } else if let Some(rt) = expected_expression_type {
            return Some(ASTTypeFilter::exact(
                rt.clone(),
                module_namespace,
                module_id,
            ));
        } else {
            None
        }
    }

    fn get_return_type_from_signatures(signatures: &Vec<&ASTFunctionSignature>) -> Option<ASTType> {
        if signatures.is_empty() {
            return None;
        }
        let mut return_types = signatures
            .into_iter()
            .map(|f| f.return_type.clone())
            .collect_vec();
        return_types.dedup();

        if return_types.len() == 1 {
            /*
            println!(
                "Multiple calls to {} with same return type: {}",
                signatures[0].name, return_types[0]
            );
            */
            Some(return_types[0].clone())
        } else {
            None
        }
    }

    fn process_function_signature(
        &mut self,
        function_signature_entry: &ASTFunctionSignatureEntry,
        call: &ASTFunctionCall,
        val_context: &mut ValContext,
        statics: &mut ValContext,
        expected_expression_type: Option<&ASTType>,
        is_lambda: bool,
        modules_container: &ASTModulesContainer,
        function: Option<&ASTFunctionDef>,
        index: &ASTIndex,
    ) -> Option<Arc<ASTTypeCheckEntry>> {
        debug_i!(
            "process_function_signature {} expected {}",
            function_signature_entry.signature,
            OptionDisplay(&expected_expression_type)
        );
        indent!();

        let call_module_namespace = index.module_namespace();
        let call_module_id = index.module_id();

        let function_signature = &function_signature_entry.signature;

        let mut resolved_generic_types = ASTResolvedGenericTypes::new();

        self.resolve_signature_generics_with_call_generics(
            function_signature,
            call,
            &mut resolved_generic_types,
            function,
            index,
        );

        if let Some(eet) = expected_expression_type {
            if function_signature.return_type.is_generic() {
                if let Err(e) = ASTResolvedGenericTypes::resolve_generic_types_from_effective_type(
                    &function_signature.return_type,
                    eet,
                    index,
                )
                .and_then(|rgt| {
                    resolved_generic_types.extend(rgt).map_err(|e| {
                        ASTTypeCheckError::new(ASTTypeCheckErroKind::Error, index.clone(), e)
                    })
                }) {
                    self.errors.insert(index.position().id, e);
                    dedent!();
                    return None;
                }
            }
        }

        let mut loop_errors = Vec::new();
        loop {
            if call.parameters().is_empty() {
                break;
            }
            debug_i!("process_function_signature loop {}", resolved_generic_types);
            indent!();
            let resolved_generic_types_len = resolved_generic_types.len();
            loop_errors.clear();

            for (i, e) in call.parameters().iter().enumerate() {
                let parameter_type = function_signature.parameters_types.get(i).unwrap();

                let ps = resolved_generic_types.substitute(&parameter_type);
                let ast_type = if let Some(ref a) = ps {
                    a
                } else {
                    parameter_type
                };

                if let Some(entry) = self.get(e.position().id).cloned().or_else(|| {
                    self.add_expr(
                        e,
                        val_context,
                        statics,
                        Some(ast_type),
                        call_module_namespace,
                        call_module_id,
                        modules_container,
                        function,
                    )
                }) {
                    if let Some(ref calculated_type_filter) = entry.filter {
                        if parameter_type.is_generic() {
                            loop_errors.extend(Self::add_resolve_type_filter(
                                entry.index(),
                                &parameter_type,
                                calculated_type_filter,
                                &mut resolved_generic_types,
                            ));
                        }
                    }
                }
            }

            dedent!();

            if resolved_generic_types.len() == resolved_generic_types_len {
                break;
            } else if !loop_errors.is_empty() {
                for e in loop_errors {
                    self.errors.insert(e.index.position().id, e);
                }
                dedent!();
                return None;
            }
        }

        let mut return_type =
            if function_signature.return_type.is_generic() && resolved_generic_types.len() > 0 {
                if let Some(return_type) =
                    resolved_generic_types.substitute(&function_signature.return_type)
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
                if let Ok(rgt) = ASTResolvedGenericTypes::resolve_generic_types_from_effective_type(
                    &return_type,
                    eet,
                    index,
                ) {
                    if let Some(rt) = rgt.substitute(&return_type) {
                        return_type = rt;
                    }
                }
            }
        }

        let call_index = ASTIndex::new(
            function_signature_entry.namespace.clone(),
            function_signature_entry.module_id.clone(),
            function_signature_entry.position.clone(),
        );

        let result = if is_lambda {
            Some(self.insert(
                index.clone(),
                ASTTypeCheckEntry::new(
                    index.clone(),
                    Some(ASTTypeFilter::exact(
                        return_type,
                        call_module_namespace,
                        call_module_id,
                    )),
                    ASTTypeCheckInfo::LambdaCall(function_signature.clone(), call_index),
                ),
            ))
        } else {
            Some(self.insert(
                index.clone(),
                ASTTypeCheckEntry::new(
                    index.clone(),
                    Some(ASTTypeFilter::exact(
                        return_type,
                        call_module_namespace,
                        call_module_id,
                    )),
                    ASTTypeCheckInfo::Call(
                        call.function_name().clone(),
                        vec![(function_signature.clone(), call_index)],
                        call.is_macro(),
                    ),
                ),
            ))
        };

        dedent!();
        result
    }

    /// Resolve the generics of a function signature based on the generics of a function call, and put that in `resolved_generic_types`.
    ///
    /// If the call has generics, it means that the call has been done specifying the types for the generics.
    /// If one of the types is generic, it means that we are inside a generic function definition where that type is a generic type of that function definition, so we add the prefix.
    /// If the call does not have generics, it means that the call has not been done specifying the types for the generics. So it's simpler...
    ///
    /// `inside_function` is `Some` if the function call is inside a function definition, and `None` otherwise.
    /// `index` is the index of the function call
    fn resolve_signature_generics_with_call_generics(
        &mut self,
        function_signature: &ASTFunctionSignature,
        call: &ASTFunctionCall,
        resolved_generic_types: &mut ASTResolvedGenericTypes,
        inside_function: Option<&ASTFunctionDef>,
        index: &ASTIndex,
    ) {
        if call.generics().is_empty() {
            return;
        }

        if call.generics().len() != function_signature.generics.len() {
            self.add_error(
                ASTTypeCheckErroKind::Fatal,
                index.clone(),
                format!(
                    "Generics in the call do not match the generics in the function signature, the call has {} generics and the function signature has {} generics",
                    call.generics().len(),
                    function_signature.generics.len()
                ),
            );
            return;
        }

        // The call has been done specifying the types for the generics.
        // If one of the types is generic, so for example in the call afunction<T>(arguments...),
        // it means that we are inside a generic function definition where T is a generic type of that function
        // definition, so we add the prefix.
        if call.generics().iter().any(|it| it.is_generic()) {
            match inside_function {
                Some(f) => {
                    let generics_prefix = ASTFunctionSignature::from_def(f)
                        .generics_prefix(&index.module_namespace().safe_name());

                    for (i, g) in call.generics().iter().enumerate() {
                        let t = function_signature.generics[i].clone();

                        // TODO var_types
                        resolved_generic_types.insert(
                            t,
                            Vec::new(),
                            add_generic_prefix(g.clone(), &generics_prefix),
                        );
                    }
                }
                None => {
                    self.add_error(
                        ASTTypeCheckErroKind::Fatal,
                        index.clone(),
                        "Used a generic function call outside of a function.".to_string(),
                    );
                }
            }
        // The call has not been done specifying the types for the generics. So it's simpler...
        } else {
            for (i, g) in call.generics().iter().enumerate() {
                let t = function_signature.generics[i].clone();
                // TODO var_types
                resolved_generic_types.insert(t, Vec::new(), g.clone());
            }
        }
    }

    fn add_resolve_type_filter(
        index: &ASTIndex,
        generic_type: &ASTType,
        effective_filter: &ASTTypeFilter,
        resolved_generic_types: &mut ASTResolvedGenericTypes,
    ) -> Vec<ASTTypeCheckError> {
        let mut errors = Vec::new();

        match Self::resolve_type_filter(generic_type, effective_filter, index) {
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
        index: &ASTIndex,
    ) -> Result<ASTResolvedGenericTypes, ASTTypeCheckError> {
        if let ASTTypeFilter::Exact(effective_type, _) = effective_filter {
            return ASTResolvedGenericTypes::resolve_generic_types_from_effective_type(
                generic_type,
                effective_type,
                index,
            );
        } else if let ASTTypeFilter::Lambda(n, ret_type) = effective_filter {
            if let Some(rt_filter) = ret_type {
                match generic_type {
                    ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                        parameters: _,
                        return_type,
                    }) => {
                        return Self::resolve_type_filter(&return_type, &rt_filter, index);
                    }
                    ASTType::ASTGenericType(_, _, _) => {
                        if *n == 0 {
                            if let ASTTypeFilter::Exact(effective_type, _) = rt_filter.as_ref() {
                                let ef =
                                    ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                                        parameters: Vec::new(),
                                        return_type: Box::new(effective_type.clone()),
                                    });
                                return ASTResolvedGenericTypes::resolve_generic_types_from_effective_type(
                                    generic_type,
                                    &ef,
                                    index
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
}

#[cfg(test)]
mod tests {
    use std::{
        path::{Path, PathBuf},
        sync::Arc,
    };

    use itertools::Itertools;
    use rasm_utils::{
        OptionDisplay, SliceDisplay, debug_indent::enable_log, test_utils::init_minimal_log,
    };

    use crate::{
        ast::ast_module_tree::ASTModuleTree,
        codegen::{
            c::options::COptions,
            compile_target::CompileTarget,
            enh_ast::{EnhASTNameSpace, EnhModuleId, EnhModuleInfo},
            statics::Statics,
            val_context::ValContext,
        },
        commandline::RasmProfile,
        project::RasmProject,
        project_catalog::RasmProjectCatalog,
        test_utils::{get_id, get_id_from_moduleid, project_and_container},
        transformations::enrich_container,
        type_check::{
            ast_modules_container::ASTModulesContainer,
            ast_type_checker::{ASTTypeCheckEntry, ASTTypeCheckErroKind, ASTTypeCheckInfo},
        },
    };
    use rasm_parser::{
        catalog::{ASTIndex, ModuleId, ModuleNamespace, modules_catalog::ModulesCatalog},
        lexer::Lexer,
        parser::{
            Parser,
            ast::{
                ASTExpression, ASTFunctionCall, ASTLambdaDef, ASTModule, ASTPosition, ASTStatement,
                ASTValue,
            },
        },
    };

    use super::{ASTTypeChecker, ASTTypeCheckerResult};

    #[test]
    fn test_breakout_check_functions() {
        let project = RasmProject::new(PathBuf::from("../rasm/resources/examples/breakout"));

        let target = CompileTarget::C(COptions::default());
        let profile = RasmProfile::Main;

        let (container, _catalog, _errors) = project.container_and_catalog(&profile, &target);

        let mut statics = ValContext::new(None);

        let path = Path::new("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm");

        let (module, _errors, info) = project
            .get_module(path, &target, &profile.principal_sub_project(), true)
            .unwrap();

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

        let (types_map, _info, module) = check_body(file);

        let r_value = get_type_check_entry(&module, &types_map, 1, 5);

        assert_eq!(
            "Some(Exact(Option<int>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker2() {
        let file = "resources/test/ast_type_checker/ast_type_checker2.rasm";

        let (types_map, _info, module) = check_body(file);

        let r_value = get_type_check_entry(&module, &types_map, 1, 5);

        assert_eq!(
            "Some(Exact(Option<int>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker3() {
        let file = "resources/test/ast_type_checker/ast_type_checker3.rasm";

        let (types_map, _, module) = check_body(file);

        let r_value = get_type_check_entry(&module, &types_map, 1, 5);

        assert_eq!(
            "Some(Exact(Option<int>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker9() {
        init_minimal_log();

        let file = "resources/test/ast_type_checker/ast_type_checker9.rasm";

        let (types_map, _info, module) = check_body(file);

        let none_value = get_type_check_entry(&module, &types_map, 1, 29);

        assert_eq!(
            "Some(Exact(Option<int>))",
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

        let (types_map, _info, module) = check_body(file);

        let none_value = get_type_check_entry(&module, &types_map, 1, 19);

        assert_eq!(
            "Some(Exact(Option<int>))",
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

        let (types_map, _info, module) = check_body(file);

        let none_value = get_type_check_entry(&module, &types_map, 1, 20);

        assert_eq!(
            "Some(Exact(Option<int>))",
            format!(
                "{}",
                OptionDisplay(&none_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker4() {
        let file = "resources/test/ast_type_checker/ast_type_checker4.rasm";

        let (types_map, _info, module) = check_body(file);

        let r_value = get_type_check_entry(&module, &types_map, 1, 5);

        assert_eq!(
            "Some(Exact(Option<int>))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );
    }

    #[test]
    fn test_functions_checker5() {
        let file = "resources/test/ast_type_checker/ast_type_checker5.rasm";

        let (types_map, _info, module) = check_body(file);

        let r_value = get_type_check_entry(&module, &types_map, 1, 5);

        assert_eq!(
            "Some(Exact(List<Option<int>>))",
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

        let (types_map, _info, module) = check_function(file, "endsWith");

        let r_value = get_type_check_entry(&module, &types_map, 3, 9);

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

        let (types_map, _info, module) = check_function(file, "endsWith1");

        let r_value = get_type_check_entry(&module, &types_map, 13, 9);

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

        let (types_map, _info, module) = check_function(file, "endsWith");

        let r_value = get_type_check_entry(&module, &types_map, 4, 14);

        assert_eq!(
            "Some(Exact(ast_type_checker7_ast_type_checker7_endsWith:T))",
            format!(
                "{}",
                OptionDisplay(&r_value.and_then(|it| it.filter.clone())),
            )
        );

        let r_value = get_type_check_entry(&module, &types_map, 3, 19);

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
        init_minimal_log();

        let file: &str = "resources/test/ast_type_checker/ast_type_checker8.rasm";

        let (types_map, _info, module) = check_function(file, "generic");

        let r_value = get_type_check_entry(&module, &types_map, 9, 13);

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
    fn ast_type_check_gameoflife_tc() {
        init_minimal_log();
        check_project("../rasm/resources/examples/gameoflife_tc");
    }

    #[test]
    fn test_function_reference() {
        init_minimal_log();

        test_single_file(
            "resources/test/ast_type_checker/function_reference.rasm",
            3,
            15,
            "fn (float,float) -> float",
        );
    }

    #[test]
    fn test_function_reference_nostdlib() {
        init_minimal_log();
        //enable_log(false);

        let (type_checker, catalog, _, container) =
            check_project("resources/test/ast_type_checker/function_reference_nostdlib");

        let id = get_id(
            "resources/test/ast_type_checker/function_reference_nostdlib/src/main/rasm/main.rasm",
            &catalog,
            &container,
            15,
            19,
        )
        .unwrap();

        let (t, _) = type_checker.result.get(id).unwrap().exact_type().unwrap();

        assert_eq!(format!("{t}"), "fn (float,float) -> float");

        let id = get_id(
            "resources/test/ast_type_checker/function_reference_nostdlib/src/main/rasm/main.rasm",
            &catalog,
            &container,
            18,
            20,
        )
        .unwrap();

        let (t, _) = type_checker.result.get(id).unwrap().exact_type().unwrap();

        assert_eq!(format!("{t}"), "fn (int,int) -> int");
    }

    #[test]
    fn test_function_reference1() {
        init_minimal_log();

        enable_log(false);

        test_single_file(
            "resources/test/ast_type_checker/function_reference.rasm",
            7,
            16,
            "fn (int,int) -> int",
        );
    }

    #[test]
    fn test_function_reference2() {
        init_minimal_log();

        test_single_file(
            "resources/test/ast_type_checker/function_reference.rasm",
            9,
            11,
            "fn (str,float) -> ()",
        );
    }

    #[test]
    fn test_type_check_lambda2() {
        init_minimal_log();

        test_single_file("../rasm/resources/test/lambda2.rasm", 4, 6, "Option<int>");
    }

    #[test]
    fn test_lambda2_if() {
        init_minimal_log();

        let path = PathBuf::from("../rasm/resources/test/lambda2.rasm");
        let project = RasmProject::new(path.clone());

        let target = CompileTarget::C(COptions::default());

        let (container, catalog, _errors) =
            project.container_and_catalog(&RasmProfile::Main, &target);

        let container = enrich_container(
            &target,
            &mut Statics::new(),
            container,
            &catalog,
            false,
            false,
        );

        let mut type_checker = ASTTypeChecker::new();
        let mut val_context = ValContext::new(None);
        let mut statics = ValContext::new(None);

        let info = catalog
            .info(&EnhModuleId::Path(path.canonicalize().unwrap()))
            .unwrap();

        let i = ASTExpression::ASTValueExpression(
            ASTValue::ASTIntegerValue(10),
            ASTPosition::new(2, 29),
        );

        let position = ASTPosition::new(2, 16);

        let some = ASTFunctionCall::new(
            "Some".to_owned(),
            vec![i],
            position.clone(),
            Vec::new(),
            None,
            false,
        );
        let o = ASTExpression::ASTFunctionCallExpression(some);

        let l = ASTExpression::ASTLambdaExpression(ASTLambdaDef {
            parameter_names: Vec::new(),
            body: vec![ASTStatement::ASTExpressionStatement(
                o,
                ASTPosition::new(2, 16),
            )],
            position: ASTPosition::new(2, 14),
        });

        let t = ASTExpression::ASTValueExpression(
            ASTValue::ASTBooleanValue(true),
            ASTPosition::new(2, 8),
        );

        let call_position = ASTPosition::new(2, 5);
        let id = call_position.id;

        let call = ASTFunctionCall::new(
            "if".to_owned(),
            vec![t, l],
            call_position.clone(),
            Vec::new(),
            None,
            false,
        );

        type_checker.add_call(
            &ASTIndex::new(info.namespace().clone(), info.id().clone(), call_position),
            &call,
            &mut val_context,
            &mut statics,
            None,
            &container,
            None,
        );

        if let Some(entry) = type_checker.get(id) {
            if let Some((t, _)) = entry.exact_type() {
                assert_eq!("If<fn () -> Option<int>>", &format!("{t}"));
                return;
            }
        }

        panic!();
    }

    #[test]
    fn test_lambda2_if_body() {
        init_minimal_log();

        let path = PathBuf::from("../rasm/resources/test/lambda2.rasm");
        let project = RasmProject::new(path.clone());

        let target = CompileTarget::C(COptions::default());

        let (container, catalog, _errors) =
            project.container_and_catalog(&RasmProfile::Main, &target);

        let container = enrich_container(
            &target,
            &mut Statics::new(),
            container,
            &catalog,
            false,
            false,
        );

        let mut type_checker = ASTTypeChecker::new();
        let mut val_context = ValContext::new(None);
        let mut statics = ValContext::new(None);

        let info = catalog
            .info(&EnhModuleId::Path(path.canonicalize().unwrap()))
            .unwrap();

        let i = ASTExpression::ASTValueExpression(
            ASTValue::ASTIntegerValue(10),
            ASTPosition::new(2, 29),
        );

        let some = ASTFunctionCall::new(
            "Some".to_owned(),
            vec![i],
            ASTPosition::new(2, 16),
            Vec::new(),
            None,
            false,
        );
        let o = ASTExpression::ASTFunctionCallExpression(some);

        let body = vec![ASTStatement::ASTExpressionStatement(
            o,
            ASTPosition::new(2, 16),
        )];

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

        if let Ok(entry) = res {
            if let Some((t, _)) = entry.exact_type() {
                assert_eq!("Option<int>", &format!("{t}"));
                return;
            }
        }

        panic!();
    }

    #[test]
    fn test_ast_checker_print() {
        init_minimal_log();

        enable_log(false);

        let (tc, catalog, _, container) = check_project("../stdlib");

        let id = get_id(
            "../stdlib/src/main/rasm/print.rasm",
            &catalog,
            &container,
            13,
            5,
        )
        .unwrap();

        /*
        for e in tc.errors().values() {
            println!("error: {e}");
        }
        */

        if let Some(entry) = tc.get(id) {
            if let ASTTypeCheckInfo::Call(_, vec, _) = &entry.info {
                let mut v = vec.iter().map(|it| format!("{}", it.0)).collect::<Vec<_>>();
                v.sort();
                assert_eq!(
                    "print(Compare), print<stdlib_iter_print:T>(Iter<stdlib_iter_print:T>), print<stdlib_list_print:T>(List<stdlib_list_print:T>), print<stdlib_print_print:T>(stdlib_print_print:T)",
                    format!("{}", SliceDisplay(&v))
                );
                return;
            }
        }
        panic!()
    }

    #[test]
    fn test_json_lexer() {
        init_minimal_log();
        enable_log(false);

        let (tc, catalog, _, container) = check_project("../stdlib");

        let id = get_id(
            "../stdlib/src/main/rasm/json/lexer.rasm",
            &catalog,
            &container,
            51,
            9,
        )
        .unwrap();
        /*
        for e in tc.errors().values() {
            println!("error: {e}");
        }
        */

        if let Some(entry) = tc.get(id) {
            if let ASTTypeCheckInfo::Call(_, vec, _) = &entry.info {
                let mut v = vec.iter().map(|it| format!("{}", it.0)).collect::<Vec<_>>();
                v.sort();
                assert_eq!(
                    "processChar(LexerState, char) -> Result<LexerState,str>",
                    format!("{}", SliceDisplay(&v))
                );
                return;
            }
        }
        panic!()
    }

    #[test]
    fn test_type_check_3() {
        init_minimal_log();

        let (tc, _, _, _) = check_project_with_profile(
            "../rasm/resources/test/type_check/type_check_3.rasm",
            &RasmProfile::Main,
        );

        let entries = tc
            .result
            .map
            .values()
            .filter(|it| {
                it.index()
                    .module_namespace()
                    .safe_name()
                    .contains("type_check_3")
                    && (it.index().position().row == 2 || it.index().position().row == 5)
            })
            .collect_vec();

        if entries.len() != 2 {
            panic!();
        }

        entries.into_iter().for_each(|entry| {
            if let ASTTypeCheckInfo::Call(_, vec, _) = &entry.info {
                if vec.len() != 1 {
                    panic!();
                }
            }
        });
    }

    #[test]
    fn test_type_check_generic_call() {
        type_check_functions(
            r#"
                pub fn aFunction() { 
                    let o = Ok<int,str>(10)
                }
            "#,
            3,
            false,
        );
    }

    #[test]
    fn test_type_check_let() {
        type_check_functions(
            r#"
                pub fn aFunction() { 
                    let o = 10
                    println(o)
                }
            "#,
            4,
            false,
        );
    }

    #[test]
    fn test_type_check_lambda_1() {
        type_check_functions(
            r#"
                pub fn aFunction() {
                    let o = Ok<int,str>(10)
                    o.fmap(fn(it) {Ok<str,str>("value=".append(it))}).println
                }
            "#,
            12,
            false,
        );
    }

    #[test]
    fn test_type_check_lambda_2() {
        type_check_functions(
            r#"
                pub fn aFunction() {
                    println(
                        if(true, { Some(10)})
                        .else({ None()})
                        .call()
                    )
                }
            "#,
            10,
            false,
        );
    }

    #[test]
    fn test_type_check_lambda_3() {
        init_minimal_log();

        type_check_functions(
            r#"
                pub fn aFunction() {
                    Some(10).map(fn(it) {"value=".append(it)}).println
                }
            "#,
            9,
            false,
        );
    }

    #[test]
    fn test_generic_function_in_lambda() {
        init_minimal_log();

        type_check_functions(
            r#"
                pub fn aFunction() -> Vec<int> {
                    if(true, {
                        vecOf(1)
                    }, Vec)
                }
            "#,
            6,
            false,
        );
    }

    #[test]
    fn test_generic_function_in_lambda_and_let() {
        init_minimal_log();

        type_check_functions(
            r#"
                pub fn aFunction() -> Vec<int> {
                    let a = if(true, {
                        vecOf(1)
                    }, Vec)
                    a
                }
            "#,
            8,
            false,
        );
    }

    #[test]
    fn test_type_check_function_ref_1() {
        type_check_functions(
            r#"
                pub fn aFunction<OK,ERROR,T>(result: Result<OK,ERROR>, mapFun: fn(OK) -> T) -> Result<T,ERROR> {
                    result.match(fn(value) { Ok(mapFun(value)) }, Error)
                }
            "#,
            8,
            true,
        );
    }

    #[test]
    fn test_type_check_function_ref_2() {
        type_check_functions(
            r#"
                pub fn aFunction<OK,ERROR>(r: Result<Result<OK,ERROR>,ERROR>) -> Result<OK,ERROR> {
                    r.match(identity, Error)
                }
            "#,
            4,
            true,
        );
    }

    #[test]
    fn test_type_check_function_ref_3() {
        type_check_functions(
            r#"
                pub fn aFunction<OK,ERROR,T>(l: Result<OK,ERROR>, f: fn(OK) -> Result<T,ERROR>) -> Result<T,ERROR> {
                    l.match(f, Error)
                }
            "#,
            4,
            true,
        );
    }

    #[test]
    fn test_type_check_if() {
        type_check_functions(
            r#"
                pub fn aFunction() -> str {
                    if(true, { "true"})
                    .elseIf(false, { "false"})
                    .elseIf(false, { "false"})
                    .elseIf(false, { "false"})
                    .elseIf(false, { "false"})
                    .elseIf(false, { "false"})
                    .elseIf(false, { "false"})
                    .else({ "false"})
                    .call()
                }
            "#,
            32,
            false,
        );
    }

    #[test]
    fn test_type_check_function_ref_4() {
        type_check_functions(
            r#"
                pub fn aFunction()-> str {
                    Some(0).match(substr, fn() -> {"None"})
                }

                fn substr(n: int) -> str {
                    "Hello"
                }
            "#,
            7,
            false,
        );
    }

    #[test]
    fn test_type_check_par_ref() {
        let (ast_type_checker, _, container) = type_check_functions(
            r#"
                pub fn aFunction(s: str) -> str { 
                    s
                }
            "#,
            1,
            false,
        );

        let id = get_id_from_moduleid(&ModuleId::global(), &container, 3, 21).unwrap();

        let entry = ast_type_checker.get(id).unwrap();

        match &entry.info {
            ASTTypeCheckInfo::Ref(name, _) => {
                assert_eq!(name, "s");
            }
            _ => panic!("not a ref: {entry}"),
        }
    }

    #[test]
    fn test_type_check_stdlib_vec() {
        init_minimal_log();
        enable_log(false);
        let (tc, catalog, _, container) =
            check_project_with_profile("../stdlib", &RasmProfile::Test);

        /*
        for error in tc.errors().values() {
            println!("{error}");
        }
        */

        let id = get_id(
            "../stdlib/src/test/rasm/vec/vec.rasm",
            &catalog,
            &container,
            4,
            17,
        )
        .unwrap();

        assert!(tc.get(id).is_some());

        let id = get_id(
            "../stdlib/src/test/rasm/vec/vec.rasm",
            &catalog,
            &container,
            4,
            6,
        )
        .unwrap();

        assert!(tc.get(id).is_some());
    }

    #[test]
    fn test_type_check_vec_suite() {
        type_check_functions(
            r#"
                pub fn vecSuite() -> TestSuite {
                    TestSuite("vec")
                    .add("sum", sum)
                }

                fn sum() -> Assertions {
                    Assertions()
                }
            "#,
            6,
            false,
        );
    }

    #[test]
    fn test_type_check_json() {
        type_check_functions(
            r#"
                pub fn json<T>(s : AStruct<T>)  -> str {
                    add(append(add(add(add(add(append(add(add(add(add(append(add(add(add("{", "\""), "x"), "\" : "), json(x(s))), ", "), "\""), "y"), "\" : "), json(y(s))), ", "), "\""), "v"), "\" : "), json(v(s))), "}") 
                }
            "#,
            35,
            true,
        );
    }

    fn type_check_functions<'a>(
        s: &'a str,
        expected_entries: usize,
        can_be_generic: bool,
    ) -> (ASTTypeChecker<'a>, RasmProjectCatalog, ASTModulesContainer) {
        let project = RasmProject::new(PathBuf::from("../stdlib"));

        let (modules_container, catalog, _) = project
            .container_and_catalog(&RasmProfile::Test, &CompileTarget::C(COptions::default()));

        let mut modules_container = enrich_container(
            &CompileTarget::C(COptions::default()),
            &mut Statics::new(),
            modules_container,
            &catalog,
            false,
            false,
        );

        let static_val_context = ValContext::new(None);

        let (module, _errors) = Parser::new(Lexer::new(s.to_owned()).collect_vec(), vec![]).parse();

        modules_container.add(
            module.clone(),
            ModuleNamespace::global(),
            ModuleId::global(),
            true,
        );

        init_minimal_log();

        let mut checker = ASTTypeChecker::new();
        for function in module.functions {
            enable_log(true);
            checker.add_function(
                &function,
                &static_val_context,
                &ModuleNamespace::global(),
                &ModuleId::global(),
                &modules_container,
            );
            enable_log(false);
        }

        for (i, e) in checker.errors.iter() {
            println!("Error: {e} : {i}");
        }

        if checker.result.map.len() != expected_entries {
            for entry in checker.result.map.values() {
                println!("entry {entry} : {}", entry.index());
            }
            panic!(
                "expected {expected_entries} entries, but got {}",
                checker.result.map.len()
            );
        }
        assert_eq!(checker.result.map.len(), expected_entries);

        for entry in checker.result.map.values() {
            if let Some((t, _)) = entry.exact_type() {
                if !can_be_generic {
                    assert!(
                        !t.is_generic(),
                        "{t} is generic in {entry} : {}",
                        entry.index()
                    );
                }
            } else {
                panic!("not exact in {entry} : {}", entry.index());
            }
        }

        if !checker.errors.is_empty() {
            panic!();
        }

        (checker, catalog, modules_container)
    }

    fn get_type_check_entry<'a>(
        module: &ASTModule,
        types_map: &'a ASTTypeCheckerResult,
        row: usize,
        column: usize,
    ) -> Option<&'a Arc<ASTTypeCheckEntry>> {
        let entries = get_type_check_entries(module, types_map, row, column);

        if entries.len() > 1 {
            for entry in entries.iter() {
                println!("entry {:?}", entry);
            }

            panic!();
        }

        entries.first().cloned()
    }

    fn get_type_check_entries<'a>(
        module: &ASTModule,
        types_map: &'a ASTTypeCheckerResult,
        row: usize,
        column: usize,
    ) -> Vec<&'a Arc<ASTTypeCheckEntry>> {
        let tree = ASTModuleTree::new(module);
        let elements = tree.get_elements_at(row, column);

        elements
            .into_iter()
            .flat_map(|element| {
                let id = element.element.position().id;
                if let Some(t) = types_map.get(id) {
                    vec![t]
                } else {
                    Vec::new()
                }
            })
            .collect::<Vec<_>>()
    }

    fn test_single_file(path: &str, row: usize, column: usize, expected: &str) {
        let (type_checker, catalog, _, container) = check_project(path);

        if let Some(id) = get_id(path, &catalog, &container, row, column) {
            if let Some(entry) = type_checker.result.get(id) {
                if let Some((t, _)) = entry.exact_type() {
                    assert_eq!(expected, &format!("{t}"));
                    return;
                }
                println!("not exact in {entry} : {}", entry.index());
            }
        }

        for (i, e) in type_checker.errors.iter() {
            if e.kind() == &ASTTypeCheckErroKind::Error {
                println!("Error: {e} : {i}");
            }
        }

        panic!();
    }

    fn check_project<'a>(
        path: &str,
    ) -> (
        ASTTypeChecker<'a>,
        impl ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        ValContext,
        ASTModulesContainer,
    ) {
        check_project_with_profile(path, &RasmProfile::Main)
    }

    fn check_project_with_profile<'a>(
        path: &str,
        profile: &RasmProfile,
    ) -> (
        ASTTypeChecker<'a>,
        impl ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        ValContext,
        ASTModulesContainer,
    ) {
        let project = RasmProject::new(PathBuf::from(path));

        let target = CompileTarget::C(COptions::default());

        let (container, catalog, _errors) = project.container_and_catalog(profile, &target);

        let mut statics = &mut Statics::new();

        let container = enrich_container(&target, &mut statics, container, &catalog, false, false);

        let result = ASTTypeChecker::from_modules_container(&container);

        let errors = result
            .0
            .errors
            .iter()
            .filter(|(_, error)| error.kind == ASTTypeCheckErroKind::Fatal)
            .collect::<Vec<_>>();

        /*
        for (_, error) in errors.iter() {
            if error.kind == ASTTypeCheckErroKind::Error {
                println!("error {error}");
            }
        }
        */

        if !errors.is_empty() {
            panic!();
        }

        (result.0, catalog, result.1, container)
    }

    fn check_body(file: &str) -> (ASTTypeCheckerResult, EnhModuleInfo, ASTModule) {
        apply_to_functions_checker(file, file, |module, mut ftc, info, cont| {
            /*
            for (_, e) in ftc.errors.iter() {
                println!("type checker error {e}");
            }
            */
            let mut val_context = ValContext::new(None);
            let mut static_val_context = ValContext::new(None);
            if let Err(errors) = ftc.add_body(
                &mut val_context,
                &mut static_val_context,
                &module.body,
                None,
                &info.module_namespace(),
                &info.module_id(),
                &cont,
                None,
            ) {
                errors
                    .iter()
                    .filter(|e| e.kind != ASTTypeCheckErroKind::Fatal)
                    .for_each(|e| {
                        println!("type checker error {e}");
                    });
            }
            ftc.result
        })
    }

    fn check_function(
        file: &str,
        function_name: &str,
    ) -> (ASTTypeCheckerResult, EnhModuleInfo, ASTModule) {
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
    ) -> (ASTTypeCheckerResult, EnhModuleInfo, ASTModule)
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

        let (module, _, info) = project
            .get_module(
                Path::new(file),
                &target,
                &RasmProfile::Main.principal_sub_project(),
                true,
            )
            .unwrap();

        (
            f(
                &module,
                function_type_checker,
                info.clone(),
                modules_container,
            ),
            info,
            module,
        )
    }
}
