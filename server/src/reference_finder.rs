use std::io;
use std::iter::zip;
use std::ops::Deref;
use std::path::PathBuf;

use log::warn;

use rasm_core::codegen::enhanced_module::EnhancedASTModule;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::val_context::ValContext;
use rasm_core::codegen::ValKind;
use rasm_core::new_type_check2::TypeCheck;
use rasm_core::parser::ast::{
    ASTEnumDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex,
    ASTLambdaDef, ASTModule, ASTNameSpace, ASTParameterDef, ASTStatement, ASTStructDef, ASTType,
    ASTTypeDef, BuiltinTypeKind,
};
use rasm_core::project::RasmProject;
use rasm_core::type_check::functions_container::TypeFilter;
use rasm_core::type_check::type_check_error::TypeCheckError;
use rasm_core::type_check::{self, substitute};

use crate::completion_service::{CompletionItem, CompletionResult, CompletionTrigger};
use crate::reference_context::ReferenceContext;
use crate::selectable_item::{SelectableItem, SelectableItemTarget};

pub struct ReferenceFinder {
    selectable_items: Vec<SelectableItem>,
    path: PathBuf,
    namespace: ASTNameSpace,
}

enum CompletionType {
    SelectableItem(ASTIndex, Option<String>),
    Identifier(String),
}

impl ReferenceFinder {
    pub fn new(module: &EnhancedASTModule, ast_module: &ASTModule) -> Result<Self, TypeCheckError> {
        let path = ast_module.path.clone();
        let selectable_items = Self::process_module(module, ast_module)?;

        Ok(Self {
            selectable_items,
            path,
            namespace: ast_module.namespace.clone(),
        })
    }

    pub fn find(&self, index: &ASTIndex) -> Result<Vec<SelectableItem>, io::Error> {
        let mut result = Vec::new();

        for selectable_item in self.selectable_items.iter() {
            if selectable_item.contains(index)? {
                result.push(selectable_item.clone());
            }
        }

        Ok(result)
    }

    pub fn get_completions(
        &self,
        project: &RasmProject,
        index: &ASTIndex,
        enhanched_module: &EnhancedASTModule,
        trigger: &CompletionTrigger,
    ) -> Result<CompletionResult, io::Error> {
        let module_content = project.content_from_file(&self.path)?;

        let lines = module_content.lines().collect::<Vec<_>>();

        let completion_type = match trigger {
            CompletionTrigger::Invoked => {
                let mut prefix = String::new();
                let mut index = index.clone();
                let mut completion_type = None;
                loop {
                    if let Some(c) = Self::char_at_index(&lines, &index) {
                        if c == '.' {
                            // it could be a number
                            completion_type = Self::dot_completion(&lines, &index, Some(prefix));
                            break;
                        } else if c.is_whitespace() {
                        } else if c == '{' || c == ';' || c == '=' {
                            // prefix could be a number
                            completion_type = Some(CompletionType::Identifier(prefix));
                            break;
                        } else if c.is_alphanumeric() {
                            prefix.insert(0, c);
                        } else {
                            break;
                        }
                    } else {
                        return Ok(CompletionResult::NotFound(
                            "Cannot find a completable expression.".to_string(),
                        ));
                    }
                    if let Some(i) = Self::move_left(&lines, &index) {
                        index = i;
                    } else {
                        return Ok(CompletionResult::NotFound(
                            "Cannot find a completable expression.".to_string(),
                        ));
                    }
                }

                if let Some(ct) = completion_type {
                    ct
                } else {
                    return Ok(CompletionResult::NotFound(
                        "Cannot determine a completion type.".to_string(),
                    ));
                }
            }
            CompletionTrigger::Character('.') => {
                if let Some(completion_type) = Self::dot_completion(&lines, index, None) {
                    completion_type
                } else {
                    return Ok(CompletionResult::NotFound(
                        "Cannot find last identifier.".to_string(),
                    ));
                }
            }
            CompletionTrigger::Character(c) => {
                return Ok(CompletionResult::NotFound(format!(
                    "Unsupported completion trigger char '{c}'."
                )));
            }
            CompletionTrigger::IncompleteCompletion => {
                return Ok(CompletionResult::NotFound(
                    "Incomplete completion is not supported.".to_string(),
                ))
            }
        };

        match completion_type {
            CompletionType::SelectableItem(index, prefix) => {
                for selectable_item in self.selectable_items.iter() {
                    if selectable_item.contains(&index)? {
                        if let Some(ref target) = selectable_item.target {
                            if let Some(ast_type) = target.completion_type() {
                                return Self::completion_for_type(
                                    &ast_type,
                                    enhanched_module,
                                    &selectable_item.namespace,
                                    &prefix,
                                );
                            }
                        }
                    }
                }
            }
            CompletionType::Identifier(prefix) => {
                return Self::completion_for_identifier(&prefix, enhanched_module, &self.namespace);
            }
        }

        Ok(CompletionResult::NotFound(
            "Cannot find completion".to_owned(),
        ))
    }

    fn dot_completion(
        lines: &Vec<&str>,
        index: &ASTIndex,
        prefix: Option<String>,
    ) -> Option<CompletionType> {
        let index = if let Some(i) = Self::move_left(lines, index) {
            i
        } else {
            return None;
        };
        if let Some(index) = Self::find_last_char_excluding(&lines, &index, &|c| !c.is_whitespace())
            .and_then(|it| {
                if Self::char_at_index(lines, &it) == Some('"') {
                    Self::move_left(lines, &it)
                } else {
                    Self::find_last_char_excluding(&lines, &it, &|c| c.is_alphabetic())
                }
            })
        {
            Some(CompletionType::SelectableItem(index, prefix))
        } else {
            None
        }
    }

    fn completion_for_type(
        ast_type: &ASTType,
        enhanched_module: &EnhancedASTModule,
        namespace: &ASTNameSpace,
        prefix: &Option<String>,
    ) -> Result<CompletionResult, io::Error> {
        let filter = TypeFilter::Exact(ast_type.clone());
        let mut items = Vec::new();
        for function in enhanched_module.functions() {
            if !function.modifiers.public && &function.namespace != namespace {
                continue;
            }
            if let Some(p) = prefix {
                if !function.name.starts_with(p) {
                    continue;
                }
            }
            if !function.parameters.is_empty() {
                let parameter_type = &function.parameters.get(0).unwrap().ast_type;
                if let Ok(value) = filter.almost_equal(parameter_type, enhanched_module) {
                    if value {
                        if let Some(item) = CompletionItem::for_function(function) {
                            items.push(item);
                        }
                    }
                }
            }
        }
        return Ok(CompletionResult::Found(items));
    }

    fn completion_for_identifier(
        prefix: &str,
        enhanched_module: &EnhancedASTModule,
        namespace: &ASTNameSpace,
    ) -> Result<CompletionResult, io::Error> {
        let mut items = Vec::new();
        for function in enhanched_module.functions() {
            if !function.modifiers.public && &function.namespace != namespace {
                continue;
            }
            if function.name.starts_with(prefix) {
                if let Some(item) = CompletionItem::for_function(function) {
                    items.push(item);
                }
            }
        }
        return Ok(CompletionResult::Found(items));
    }

    fn find_last_char_excluding(
        lines: &Vec<&str>,
        index: &ASTIndex,
        find: &dyn Fn(char) -> bool,
    ) -> Option<ASTIndex> {
        let mut result = index.clone();
        loop {
            let c = Self::char_at_index(&lines, &result)?;
            if find(c) {
                return Some(result);
            } else if c == ')' {
                result = Self::find_open_bracket(&lines, &Self::move_left(&lines, &result)?)?;
                return Self::move_left(&lines, &result);
            }
            result = Self::move_left(lines, &result)?;
        }
    }

    fn ignore_whitespaces(lines: &Vec<&str>, index: &ASTIndex) -> Option<ASTIndex> {
        let mut result = index.clone();
        while Self::char_at_index(&lines, &result)?.is_whitespace() {
            result = Self::move_left(&lines, &result)?;
        }
        Some(result)
    }

    fn ignore_until(lines: &Vec<&str>, index: &ASTIndex, c: char) -> Option<ASTIndex> {
        let mut result = index.clone();
        while Self::char_at_index(&lines, &result)? != c {
            result = Self::move_left(&lines, &result)?;
        }
        Some(result)
    }

    fn char_at_index(lines: &Vec<&str>, index: &ASTIndex) -> Option<char> {
        lines.get(index.row - 1).and_then(|line| {
            line.get((index.column - 1)..index.column)
                .and_then(|chars| chars.chars().next())
        })
    }

    fn move_left(lines: &Vec<&str>, index: &ASTIndex) -> Option<ASTIndex> {
        let mut row = index.row as i32;
        let mut column = index.column as i32 - 1;

        if column <= 0 {
            row -= 1;
            if row <= 0 {
                return None;
            }
            column = (lines.get((row - 1) as usize).unwrap().len()) as i32;
        }
        Some(ASTIndex::new(
            index.file_name.clone(),
            row as usize,
            column as usize,
        ))
    }

    fn find_open_bracket(lines: &Vec<&str>, index: &ASTIndex) -> Option<ASTIndex> {
        let mut result = index.clone();
        let mut count = 0;
        loop {
            let c = Self::char_at_index(lines, &result)?;
            if c == ')' {
                count += 1;
            } else if c == '(' {
                if count == 0 {
                    break;
                } else {
                    count -= 1;
                }
            }

            result = Self::move_left(lines, &result)?;
        }

        Some(result)
    }

    pub fn is_path(&self, path: &PathBuf) -> bool {
        &self.path == path
    }

    pub fn references(&self, index: &ASTIndex) -> Result<Vec<SelectableItem>, io::Error> {
        let mut items = self.find(index)?;

        if items.len() == 1 {
            let item = items.remove(0);

            let mut result = Vec::new();
            for se in self.selectable_items.iter() {
                if let Some(ref target) = se.target {
                    if let Some(i) = target.index() {
                        if i == item.file_token.start {
                            result.push(se.clone());
                        }
                    }
                }
            }

            return Ok(result);
        }

        Ok(Vec::new())
    }

    fn process_module(
        enhanced_module: &EnhancedASTModule,
        module: &ASTModule,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        let mut reference_context = ReferenceContext::new(None);
        let mut reference_static_context = ReferenceContext::new(None);
        let mut val_context = ValContext::new(None);
        let mut statics = Statics::new();

        let mut type_check = TypeCheck::new(&enhanced_module.body_namespace);

        let mut result = Vec::new();
        Self::process_statics(enhanced_module, &mut reference_static_context, &mut statics);

        let mut new_functions = Vec::new();

        let _ = Self::process_statements(
            enhanced_module,
            &module.body,
            &mut result,
            &mut reference_context,
            &mut reference_static_context,
            &module.namespace,
            &mut val_context,
            &mut statics,
            Some(&ASTType::Unit),
            None,
            &mut type_check,
            &mut new_functions,
        );

        result.append(
            &mut module
                .functions
                .iter()
                .flat_map(|it| {
                    let mut function_val_context = ValContext::new(None);
                    Self::process_function(
                        it,
                        enhanced_module,
                        &reference_static_context,
                        &mut function_val_context,
                        &mut statics,
                        &mut type_check,
                        &mut new_functions,
                    )
                    .unwrap_or_default()
                })
                .collect(),
        );

        Ok(result)
    }

    fn process_statics(
        enhanced_module: &EnhancedASTModule,
        reference_static_context: &mut ReferenceContext,
        statics: &mut Statics,
    ) -> Vec<SelectableItem> {
        let mut result = Vec::new();
        let mut reference_context = ReferenceContext::new(None);
        let mut val_context = ValContext::new(None);
        let mut type_check = TypeCheck::new(&enhanced_module.body_namespace);

        let mut new_functions = Vec::new();

        let _ = Self::process_statements(
            enhanced_module,
            &enhanced_module.body,
            &mut result,
            &mut reference_context,
            reference_static_context,
            &enhanced_module.body_namespace,
            &mut val_context,
            statics,
            None,
            None,
            &mut type_check,
            &mut new_functions,
        );
        result
    }

    fn process_function(
        function: &ASTFunctionDef,
        module: &EnhancedASTModule,
        reference_static_context: &ReferenceContext,
        val_context: &mut ValContext,
        statics: &mut Statics,
        type_check: &mut TypeCheck,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        let mut result = Vec::new();

        let mut reference_context = ReferenceContext::new(Some(reference_static_context));

        for par in function.parameters.iter() {
            reference_context.add(
                par.name.clone(),
                par.ast_index.clone(),
                TypeFilter::Exact(par.ast_type.clone()),
            );
            val_context
                .insert_par(par.name.clone(), par.clone())
                .map_err(|err| {
                    TypeCheckError::new(par.ast_index.clone(), err.clone(), Vec::new())
                })?;
            Self::process_type(&function.namespace, module, &par.ast_type, &mut result);
        }

        Self::process_type(
            &function.namespace,
            module,
            &function.return_type,
            &mut result,
        );

        if let ASTFunctionBody::RASMBody(statements) = &function.body {
            let _ = Self::process_statements(
                module,
                statements,
                &mut result,
                &mut reference_context,
                &mut ReferenceContext::new(None),
                &function.namespace,
                val_context,
                statics,
                Some(&function.return_type),
                Some(function),
                type_check,
                new_functions,
            );
        }

        Ok(result)
    }

    fn process_type(
        namespace: &ASTNameSpace,
        module: &EnhancedASTModule,
        ast_type: &ASTType,
        result: &mut Vec<SelectableItem>,
    ) {
        if let ASTType::Custom {
            namespace: _,
            name,
            param_types,
            index,
        } = ast_type
        {
            Self::process_custom_type(module, result, name, index, ast_type, namespace);
            param_types
                .iter()
                .for_each(|it| Self::process_type(namespace, module, it, result));
        }
    }

    fn process_custom_type(
        module: &EnhancedASTModule,
        result: &mut Vec<SelectableItem>,
        name: &String,
        index: &ASTIndex,
        ast_type: &ASTType,
        namespace: &ASTNameSpace,
    ) {
        let min = index.mv_left(name.len());

        if let ASTType::Custom { .. } = ast_type {
            let item = SelectableItem::new(
                min,
                name.len(),
                namespace.clone(),
                Some(SelectableItemTarget::Type(
                    Self::get_custom_type_index(module, name),
                    ast_type.clone(),
                )),
            );

            result.push(item);
        }
    }

    fn get_custom_type_index(module: &EnhancedASTModule, name: &str) -> Option<ASTIndex> {
        if let Some(def) = Self::get_enum(module, name) {
            Some(def.index)
        } else if let Some(def) = Self::get_struct(module, name) {
            Some(def.index)
        } else if let Some(def) = Self::get_type(module, name) {
            Some(def.index)
        } else {
            None
        }
    }

    fn process_statements(
        module: &EnhancedASTModule,
        statements: &Vec<ASTStatement>,
        result: &mut Vec<SelectableItem>,
        reference_context: &mut ReferenceContext,
        reference_static_context: &mut ReferenceContext,
        namespace: &ASTNameSpace,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_return_type: Option<&ASTType>,
        inside_function: Option<&ASTFunctionDef>,
        type_check: &mut TypeCheck,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
    ) -> Result<(), TypeCheckError> {
        for (i, stmt) in statements.iter().enumerate() {
            let expected_type = if i == statements.len() - 1 {
                expected_return_type
            } else {
                None
            };
            match Self::process_statement(
                stmt,
                reference_context,
                reference_static_context,
                module,
                namespace,
                val_context,
                statics,
                expected_type,
                inside_function,
                type_check,
                new_functions,
            ) {
                Ok(mut inner) => {
                    result.append(&mut inner);
                }
                Err(e) => {
                    eprintln!("{e}");
                }
            }
        }
        Ok(())
    }

    fn get_filter_of_expression(
        expr: &ASTExpression,
        enhanced_ast_module: &EnhancedASTModule,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
        namespace: &ASTNameSpace,
        type_check: &mut TypeCheck,
    ) -> Result<TypeFilter, TypeCheckError> {
        let mut new_functions = Vec::new();
        type_check.type_of_expression(
            enhanced_ast_module,
            expr,
            val_context,
            statics,
            expected_type,
            namespace,
            &mut new_functions,
            false,
        )
    }

    fn process_statement(
        stmt: &ASTStatement,
        reference_context: &mut ReferenceContext,
        reference_static_context: &mut ReferenceContext,
        module: &EnhancedASTModule,
        namespace: &ASTNameSpace,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
        inside_function: Option<&ASTFunctionDef>,
        type_check: &mut TypeCheck,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        match stmt {
            ASTStatement::Expression(expr) => Self::process_expression(
                expr,
                reference_context,
                reference_static_context,
                module,
                namespace,
                val_context,
                statics,
                expected_type,
                inside_function,
                type_check,
                new_functions,
            ),
            ASTStatement::LetStatement(name, expr, is_const, index) => {
                let filter = Self::get_filter_of_expression(
                    expr,
                    module,
                    val_context,
                    statics,
                    None,
                    namespace,
                    type_check,
                )?;

                reference_context.add(name.clone(), index.clone(), filter.clone());

                let index1 = index.mv_left(name.len());
                let mut result = vec![SelectableItem::new(
                    index1.clone(),
                    name.len(),
                    namespace.clone(),
                    Some(SelectableItemTarget::Ref(index1, None)),
                )];

                if *is_const {
                    if let TypeFilter::Exact(ref ast_type) = filter {
                        statics.add_const(name.clone(), ast_type.clone());
                    } else {
                        statics.add_const(name.clone(), ASTType::Generic("UNKNOWN".to_string()));
                    }
                    reference_static_context.add(name.clone(), index.clone(), filter);
                } else if let TypeFilter::Exact(ref ast_type) = filter {
                    val_context
                        .insert_let(name.clone(), ast_type.clone(), index)
                        .map_err(|err| {
                            TypeCheckError::new(index.clone(), err.clone(), Vec::new())
                        })?;
                } else {
                    val_context
                        .insert_let(name.clone(), ASTType::Generic("UNKNOWN".to_string()), index)
                        .map_err(|err| {
                            TypeCheckError::new(index.clone(), err.clone(), Vec::new())
                        })?;
                }

                result.extend(Self::process_expression(
                    expr,
                    reference_context,
                    reference_static_context,
                    module,
                    namespace,
                    val_context,
                    statics,
                    None,
                    inside_function,
                    type_check,
                    new_functions,
                )?);
                Ok(result)
            }
        }
    }

    fn process_expression(
        expr: &ASTExpression,
        reference_context: &mut ReferenceContext,
        reference_static_context: &mut ReferenceContext,
        module: &EnhancedASTModule,
        namespace: &ASTNameSpace,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
        inside_function: Option<&ASTFunctionDef>,
        type_check: &mut TypeCheck,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        let mut result = Vec::new();
        match expr {
            ASTExpression::StringLiteral(s, index) => result.push(SelectableItem::new(
                index.mv_left(s.len()),
                s.len(),
                namespace.clone(),
                Some(SelectableItemTarget::Type(
                    None,
                    ASTType::Builtin(BuiltinTypeKind::String),
                )),
            )),
            ASTExpression::ASTFunctionCallExpression(call) => {
                let _ = Self::process_function_call(
                    expr,
                    reference_context,
                    reference_static_context,
                    module,
                    namespace,
                    val_context,
                    statics,
                    &mut result,
                    call,
                    expected_type,
                    inside_function,
                    type_check,
                    new_functions,
                );
            }
            ASTExpression::ValueRef(name, index) => {
                Self::process_value_ref(
                    reference_context,
                    reference_static_context,
                    namespace,
                    &mut result,
                    name,
                    index,
                );
            }
            ASTExpression::Value(_value_type, _index) => {}
            ASTExpression::Lambda(def) => {
                let _ = Self::process_lambda(
                    reference_context,
                    module,
                    namespace,
                    val_context,
                    statics,
                    expected_type,
                    &mut result,
                    def,
                    type_check,
                    new_functions,
                );
            }
            ASTExpression::Any(_) => {}
        }
        Ok(result)
    }

    fn process_value_ref(
        reference_context: &mut ReferenceContext,
        reference_static_context: &mut ReferenceContext,
        namespace: &ASTNameSpace,
        result: &mut Vec<SelectableItem>,
        name: &String,
        index: &ASTIndex,
    ) {
        if let Some(v) = reference_context
            .get(name)
            .or_else(|| reference_static_context.get(name))
        {
            let ast_type = match &v.filter {
                TypeFilter::Exact(ast_type) => Some(ast_type.clone()),
                _ => None,
            };

            result.push(SelectableItem::new(
                index.mv_left(name.len()),
                name.len(),
                namespace.clone(),
                Some(SelectableItemTarget::Ref(
                    v.index.mv_left(name.len()).clone(),
                    ast_type,
                )),
            ));
        }
    }

    fn process_lambda(
        reference_context: &mut ReferenceContext,
        module: &EnhancedASTModule,
        namespace: &ASTNameSpace,
        val_context: &mut ValContext,
        statics: &mut Statics,
        expected_type: Option<&ASTType>,
        result: &mut Vec<SelectableItem>,
        def: &ASTLambdaDef,
        type_check: &mut TypeCheck,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
    ) -> Result<(), TypeCheckError> {
        let mut lambda_reference_context = ReferenceContext::new(Some(reference_context));
        let mut lambda_val_context = ValContext::new(Some(val_context));
        let mut lambda_result = Vec::new();

        let expected_lambda_return_type = if let Some(et) = expected_type {
            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters,
                return_type,
            }) = et
            {
                for ((par_name, par_index), par_type) in
                    zip(def.parameter_names.iter(), parameters.iter())
                {
                    let par = ASTParameterDef {
                        name: par_name.clone(),
                        ast_type: par_type.clone(),
                        ast_index: par_index.clone(),
                    };
                    let par_index = par.ast_index.clone();
                    lambda_val_context
                        .insert_par(par_name.clone(), par)
                        .map_err(|err| {
                            TypeCheckError::new(par_index.clone(), err.clone(), Vec::new())
                        })?;
                    lambda_reference_context.add(
                        par_name.clone(),
                        par_index.clone(),
                        TypeFilter::Exact(par_type.clone()),
                    );
                    lambda_result.push(SelectableItem::new(
                        par_index.mv_left(par_name.len()),
                        par_name.len(),
                        namespace.clone(),
                        None,
                    ))
                }
                Some(return_type.deref())
            } else {
                None
            }
        } else {
            for (name, index) in def.parameter_names.iter() {
                lambda_val_context
                    .insert_par(
                        name.clone(),
                        ASTParameterDef {
                            name: name.to_string(),
                            ast_index: index.clone(),
                            ast_type: ASTType::Generic("UNKNOWN".to_string()),
                        },
                    )
                    .map_err(|err| TypeCheckError::new(index.clone(), err.clone(), Vec::new()))?;
                lambda_reference_context.add(name.clone(), index.clone(), TypeFilter::Any);
            }
            None
        };

        let _ = Self::process_statements(
            module,
            &def.body,
            &mut lambda_result,
            &mut lambda_reference_context,
            &mut ReferenceContext::new(None),
            namespace,
            &mut lambda_val_context,
            statics,
            expected_lambda_return_type,
            None, // TODO
            type_check,
            new_functions,
        );
        result.append(&mut lambda_result);
        Ok(())
    }

    fn process_function_call(
        expr: &ASTExpression,
        reference_context: &mut ReferenceContext,
        reference_static_context: &mut ReferenceContext,
        module: &EnhancedASTModule,
        namespace: &ASTNameSpace,
        val_context: &mut ValContext,
        statics: &mut Statics,
        result: &mut Vec<SelectableItem>,
        call: &ASTFunctionCall,
        expected_return_type: Option<&ASTType>,
        inside_function: Option<&ASTFunctionDef>,
        type_check: &mut TypeCheck,
        new_functions: &mut Vec<(ASTFunctionDef, Vec<ASTIndex>)>,
    ) -> Result<(), TypeCheckError> {
        if let Some(val_kind) = val_context.get(&call.function_name) {
            let (index, ast_type) = match val_kind {
                ValKind::ParameterRef(_, par) => {
                    // TODO process parameters
                    (par.ast_index.clone(), par.ast_type.clone())
                }
                ValKind::LetRef(_, ast_type, index) => {
                    // TODO process parameters
                    (index.clone(), ast_type.clone())
                }
            };

            let cloned_type = ast_type.clone();

            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters,
                return_type: _,
            }) = ast_type
            {
                result.push(SelectableItem::new(
                    call.index.mv_left(call.original_function_name.len()),
                    call.original_function_name.len(),
                    namespace.clone(),
                    Some(SelectableItemTarget::Ref(index, Some(cloned_type))),
                ));

                let mut v = zip(call.parameters.iter(), &parameters)
                    .flat_map(|(it, par)| {
                        match Self::process_expression(
                            it,
                            reference_context,
                            reference_static_context,
                            module,
                            namespace,
                            val_context,
                            statics,
                            Some(par),
                            inside_function,
                            type_check,
                            new_functions,
                        ) {
                            Ok(inner) => inner,
                            Err(e) => {
                                eprintln!("Error evaluating expr {expr} : {}", expr.get_index());
                                eprintln!("{e}");
                                Vec::new()
                            }
                        }
                        .into_iter()
                    })
                    .collect::<Vec<_>>();
                result.append(&mut v);
            }

            return Ok(());
        }

        match type_check.get_valid_function(
            module,
            call,
            val_context,
            statics,
            expected_return_type,
            namespace,
            inside_function,
            new_functions,
            false,
        ) {
            Ok((function_def, resolved_generic_types, expressions)) => {
                // println!("expressions {}", SliceDisplay(&expressions));
                let mut fd = function_def.clone();
                fd.name = fd.original_name.clone();

                let descr = format!("{function_def}");
                result.push(SelectableItem::new(
                    call.index.mv_left(call.original_function_name.len()),
                    call.original_function_name.len(),
                    namespace.clone(),
                    Some(SelectableItemTarget::Function(
                        function_def.index,
                        function_def.return_type,
                        descr,
                    )),
                ));

                let mut v = zip(expressions.iter(), &function_def.parameters)
                    .flat_map(|(it, par)| {
                        let par_ast_type = if let Some(new_t) =
                            substitute(&par.ast_type, &resolved_generic_types)
                        {
                            new_t
                        } else {
                            par.ast_type.clone()
                        };
                        match Self::process_expression(
                            it,
                            reference_context,
                            reference_static_context,
                            module,
                            namespace,
                            val_context,
                            statics,
                            Some(&par_ast_type),
                            inside_function,
                            type_check,
                            new_functions,
                        ) {
                            Ok(inner) => inner,
                            Err(e) => {
                                eprintln!("Error evaluating expr {expr} : {}", expr.get_index());
                                eprintln!("{e}");
                                Vec::new()
                            }
                        }
                        .into_iter()
                    })
                    .collect::<Vec<_>>();
                result.append(&mut v);
            }
            Err(e) => {
                let mut v = call
                    .parameters
                    .iter()
                    .flat_map(|it| {
                        match Self::process_expression(
                            it,
                            reference_context,
                            reference_static_context,
                            module,
                            namespace,
                            val_context,
                            statics,
                            None,
                            inside_function,
                            type_check,
                            new_functions,
                        ) {
                            Ok(inner) => inner,
                            Err(e) => {
                                eprintln!("Error evaluating expr {expr} : {}", expr.get_index());
                                eprintln!("{e}");
                                Vec::new()
                            }
                        }
                        .into_iter()
                    })
                    .collect::<Vec<_>>();
                result.append(&mut v);
                warn!("Cannot find function for call {call}: {}\n{e}", call.index);
            }
        }
        Ok(())
    }

    fn get_if_custom_type_index(
        module: &EnhancedASTModule,
        ast_type: &ASTType,
    ) -> Option<ASTIndex> {
        if let ASTType::Custom {
            namespace: _,
            name,
            param_types: _,
            index: _,
        } = ast_type
        {
            Self::get_custom_type_index(module, name)
        } else {
            None
        }
    }

    fn get_enum(module: &EnhancedASTModule, name: &str) -> Option<ASTEnumDef> {
        module.enums.iter().find(|it| &it.name == name).cloned()
    }

    fn get_struct(module: &EnhancedASTModule, name: &str) -> Option<ASTStructDef> {
        module.structs.iter().find(|it| &it.name == name).cloned()
    }

    fn get_type(module: &EnhancedASTModule, name: &str) -> Option<ASTTypeDef> {
        module.types.iter().find(|it| &it.name == name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Write;
    use std::path::{Path, PathBuf};

    use env_logger::Builder;

    use rasm_core::codegen::compile_target::CompileTarget;
    use rasm_core::codegen::enhanced_module::EnhancedASTModule;
    use rasm_core::codegen::statics::Statics;
    use rasm_core::codegen::AsmOptions;
    use rasm_core::commandline::CommandLineOptions;
    use rasm_core::parser::ast::{ASTIndex, ASTModule, ASTType};
    use rasm_core::project::RasmProject;
    use rasm_core::utils::{OptionDisplay, SliceDisplay};

    use crate::completion_service::{CompletionItem, CompletionTrigger};
    use crate::reference_finder::{CompletionResult, ReferenceFinder};
    use crate::selectable_item::{SelectableItem, SelectableItemTarget};

    #[test]
    fn simple() {
        let (_project, eh_module, module) =
            get_reference_finder("resources/test/simple.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/simple.rasm");

        let project = RasmProject::new(file_name.to_path_buf());
        let stdlib_path = project
            .from_relative_to_root(Path::new("../../../stdlib"))
            .canonicalize()
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(file_name.to_path_buf()), 3, 15,))
                    .unwrap()
            ),
            vec![get_index(&project, "simple.rasm", 1, 5)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(file_name.to_path_buf()), 6, 13,))
                    .unwrap()
            ),
            vec![get_index(&project, "simple.rasm", 5, 16)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(file_name.to_path_buf()), 3, 2,))
                    .unwrap()
            ),
            vec![get_index(&project, "simple.rasm", 5, 4)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(file_name.to_path_buf()), 6, 9,))
                    .unwrap()
            ),
            vec![ASTIndex::new(
                Some(stdlib_path.join("src/main/rasm/print.rasm")),
                12,
                8
            )]
        );

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(file_name.to_path_buf()), 10, 15,))
                    .unwrap()
            ),
            vec![ASTIndex::new(
                Some(stdlib_path.join("src/main/rasm/option.rasm")),
                2,
                7
            )]
        );
    }

    #[test]
    fn types() {
        let (_project, eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/types.rasm");
        let project = RasmProject::new(file_name.to_path_buf());
        let source_file = Path::new(&file_name);
        let stdlib_path = stdlib_path(file_name);

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(source_file.to_path_buf()), 15, 23))
                    .unwrap()
            ),
            vec![ASTIndex::new(
                Some(stdlib_path.join("src/main/rasm/option.rasm")),
                1,
                10
            )],
        );

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(source_file.to_path_buf()), 19, 23,))
                    .unwrap()
            ),
            vec![get_index(&project, "types.rasm", 1, 8)],
        );

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(source_file.to_path_buf()), 23, 23,))
                    .unwrap()
            ),
            vec![ASTIndex::new(
                Some(stdlib_path.join("src/main/nasmi386/vec.rasm")),
                1,
                10
            )],
        );
    }

    #[test]
    fn complex_expression_completions() {
        let values = get_completion_values(
            None,
            "resources/test/complex_expression.rasm",
            6,
            9,
            CompletionTrigger::Character('.'),
        )
        .unwrap();

        // println!("values {}", SliceDisplay(&values));

        assert!(values.iter().find(|it| it.as_str() == "elseIf").is_some());
    }

    #[test]
    fn complex_expression_ref() {
        let values = get_find(None, "resources/test/complex_expression.rasm", 11, 13);

        println!("values {}", SliceDisplay(&values));

        assert_eq!(1, values.len());

        assert!(format!("{}", values.get(0).unwrap()).ends_with("if.rasm:9:8"));
    }

    #[test]
    fn struct_constructor() {
        let (project, eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/types.rasm");

        let source_file = Path::new(&file_name);

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(source_file.to_path_buf()), 6, 19,))
                    .unwrap()
            ),
            vec![get_index(&project, "types.rasm", 1, 8)],
        );
    }

    #[test]
    fn types_1() {
        let (project, eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = PathBuf::from("resources/test/types.rasm");
        let found = finder
            .find(&ASTIndex::new(Some(file_name.clone()), 27, 31))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(found),
            vec!(get_index(&project, "types.rasm", 1, 8,)),
        );
    }

    #[test]
    fn types_2() {
        let (project, eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = PathBuf::from("resources/test/types.rasm");
        let found = finder
            .find(&ASTIndex::new(Some(file_name.clone()), 9, 1))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(found),
            vec!(get_index(&project, "types.rasm", 15, 4,)),
        );
    }

    #[test]
    fn types_3() {
        let (project, eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = PathBuf::from("resources/test/types.rasm");
        let found = finder
            .find(&ASTIndex::new(Some(file_name.clone()), 9, 13))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(found),
            vec!(get_index(&project, "types.rasm", 5, 5,)),
        );
    }

    #[test]
    fn types_completion_struct_property() {
        let values = get_completion_values(
            None,
            "resources/test/types.rasm",
            12,
            16,
            CompletionTrigger::Character('.'),
        )
        .unwrap();

        assert_eq!(3, values.into_iter().filter(|it| it == "anI32").count());
    }

    #[test]
    fn types_completion_match() {
        let values = get_completion_values(
            None,
            "resources/test/types.rasm",
            13,
            7,
            CompletionTrigger::Character('.'),
        )
        .unwrap();

        assert_eq!(1, values.into_iter().filter(|it| it == "match").count());
    }

    #[test]
    fn types_completion_dot() {
        let values = get_completion_values(
            None,
            "resources/test/types.rasm",
            36,
            16,
            CompletionTrigger::Character('.'),
        )
        .unwrap();
        assert_eq!(3, values.into_iter().filter(|it| it == "anI32").count());
    }

    #[test]
    fn types_completion_invoked_function_call() {
        let values = get_completion_values(
            None,
            "resources/test/types.rasm",
            16,
            11,
            CompletionTrigger::Invoked,
        )
        .unwrap();

        assert!(!values.is_empty());
        assert!(values.iter().all(|it| it.starts_with("print")));
    }

    #[test]
    fn types_completion_invoked_function_call_on_object() {
        let values = get_completion_values(
            None,
            "resources/test/types.rasm",
            13,
            11,
            CompletionTrigger::Invoked,
        )
        .unwrap();

        assert_eq!(vec!["match", "matchNone", "matchSome"], values);
    }

    #[test]
    #[ignore = "not yet supported"]
    fn types_completion_invoked_parameter() {
        let values = get_completion_values(
            None,
            "resources/test/types.rasm",
            32,
            8,
            CompletionTrigger::Invoked,
        )
        .unwrap();

        assert_eq!(vec!["structVec"], values);
    }

    #[test]
    fn types_completion_dot_string() {
        let values = get_completion_values(
            None,
            "resources/test/types.rasm",
            40,
            14,
            CompletionTrigger::Character('.'),
        )
        .unwrap();

        assert_eq!(1, values.into_iter().filter(|it| it == "substr").count());
    }

    #[test]
    fn types_lambda_param_type() {
        let (_project, eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Some(PathBuf::from("resources/test/types.rasm"));

        match finder.find(&ASTIndex::new(file_name.clone(), 32, 28)) {
            Ok(mut selectable_items) => {
                if selectable_items.len() == 1 {
                    let selectable_item = selectable_items.remove(0);
                    if let Some(ref target) = selectable_item.target {
                        if let Some(ASTType::Custom {
                            namespace: _,
                            name,
                            param_types: _,
                            index: _,
                        }) = target.completion_type()
                        {
                            assert_eq!("AStruct", name);
                        } else {
                            panic!(
                                "Expected some Custom type but got {}",
                                OptionDisplay(&target.completion_type())
                            );
                        }
                    }
                } else {
                    panic!("Found {} elements", selectable_items.len());
                }
            }
            Err(e) => {
                panic!("{e}");
            }
        }
    }

    #[test]
    fn types_flatten() {
        let file_name = PathBuf::from("resources/test/types.rasm");
        let stdlib_path = stdlib_path(file_name.as_path());

        let result_rasm = stdlib_path.join("src/main/rasm/result.rasm");

        let (_project, eh_module, module) = get_reference_finder(
            "resources/test/types.rasm",
            Some(&result_rasm.to_string_lossy()),
        );
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        match finder.find(&ASTIndex::new(Some(result_rasm.clone()), 11, 9)) {
            Ok(mut selectable_items) => {
                if selectable_items.len() == 1 {
                    let selectable_item = selectable_items.remove(0);
                    let expected_index = ASTIndex::new(Some(result_rasm), 14, 8);
                    if let Some(SelectableItemTarget::Function(index, _, _)) =
                        selectable_item.target
                    {
                        assert_eq!(index, expected_index);
                    } else {
                        panic!("Found {:?}", selectable_item.target);
                    }
                } else {
                    panic!("Found {} elements", selectable_items.len());
                }
            }
            Err(e) => {
                panic!("{e}");
            }
        }
    }

    #[test]
    fn enums() {
        let (_project, eh_module, module) = get_reference_finder("resources/test/enums.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/enums.rasm");

        let mut items = finder
            .find(&ASTIndex::new(Some(file_name.to_path_buf()), 17, 13))
            .unwrap();

        assert_eq!(1, items.len());

        let item = items.remove(0);

        if let Some(SelectableItemTarget::Function(index, _, descr)) = item.target {
            assert_eq!(ASTIndex::none(), index);
            assert!(descr.starts_with("native match"));
        } else {
            panic!("Found {:?}", item.target);
        }
    }

    #[test]
    fn enums_1() {
        let (_project, eh_module, module) = get_reference_finder("resources/test/enums.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/enums.rasm");

        let mut items = finder
            .find(&ASTIndex::new(Some(file_name.to_path_buf()), 17, 40))
            .unwrap();

        assert_eq!(1, items.len());

        let item = items.remove(0);

        if let Some(SelectableItemTarget::Ref(index, _)) = item.target {
            assert_eq!(
                ASTIndex::new(
                    Some(file_name.canonicalize().unwrap().to_path_buf()),
                    17,
                    21
                ),
                index
            );
        } else {
            panic!("Found {:?}", item.target);
        }
    }

    #[test]
    fn references() {
        let (_project, eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/types.rasm");

        let mut items = finder
            .references(&ASTIndex::new(Some(file_name.to_path_buf()), 6, 7))
            .unwrap();

        assert_eq!(3, items.len());

        let item1 = items.remove(0);
        let item2 = items.remove(0);
        let item3 = items.remove(0);

        assert_eq!(
            ASTIndex::new(Some(file_name.canonicalize().unwrap().to_path_buf()), 6, 5),
            item1.file_token.start
        );

        assert_eq!(
            ASTIndex::new(
                Some(file_name.canonicalize().unwrap().to_path_buf()),
                10,
                13
            ),
            item2.file_token.start
        );

        assert_eq!(
            ASTIndex::new(Some(file_name.canonicalize().unwrap().to_path_buf()), 12, 9),
            item3.file_token.start
        );
    }

    #[test]
    fn references_breakout() {
        let _ = env_logger::builder()
            .is_test(true)
            .filter_level(log::LevelFilter::Info)
            .try_init();

        let (_project, eh_module, module) = get_reference_finder(
            "../rasm/resources/examples/breakout",
            Some("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm"),
        );

        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();
    }

    #[test]
    fn reference1() {
        let (_project, eh_module, module) =
            get_reference_finder("resources/test/references.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        for item in finder.selectable_items.iter() {
            println!("{item}");
        }

        let file_name = Path::new("resources/test/references.rasm");

        let references = finder
            .references(&ASTIndex::new(Some(file_name.to_path_buf()), 2, 14))
            .unwrap();

        assert_eq!(1, references.len());
    }

    fn stdlib_path(file_name: &Path) -> PathBuf {
        let project = RasmProject::new(file_name.to_path_buf());

        let stdlib_path = project
            .from_relative_to_root(Path::new("../../../stdlib"))
            .canonicalize()
            .unwrap();

        stdlib_path
    }

    fn get_index(project: &RasmProject, file_n: &str, row: usize, column: usize) -> ASTIndex {
        ASTIndex::new(
            Some(
                project
                    .from_relative_to_root(Path::new(file_n))
                    .as_path()
                    .canonicalize()
                    .unwrap(),
            ),
            row,
            column,
        )
    }

    fn format_collection_items(items: &[CompletionItem]) -> Vec<String> {
        items.iter().map(|it| it.descr.clone()).collect::<Vec<_>>()
    }

    fn vec_selectable_item_to_vec_index(vec: Vec<SelectableItem>) -> Vec<ASTIndex> {
        vec.iter()
            .flat_map(|it| it.target.clone().and_then(|item| item.index()))
            .collect::<Vec<_>>()
    }

    fn get_reference_finder(
        source: &str,
        module_path: Option<&str>,
    ) -> (RasmProject, EnhancedASTModule, ASTModule) {
        env::set_var("RASM_STDLIB", "../../../stdlib");

        let file_name = Path::new(source);
        let project = RasmProject::new(file_name.to_path_buf());
        let (enhanced_module, module) =
            get_reference_finder_for_project(&project, module_path.unwrap_or(source));
        (project, enhanced_module, module)
    }

    fn get_reference_finder_for_project(
        project: &RasmProject,
        module_path: &str,
    ) -> (EnhancedASTModule, ASTModule) {
        init_log();

        let mut statics = Statics::new();
        let target = CompileTarget::Nasmi386(AsmOptions::default());
        let (modules, _errors) = project.get_all_modules(
            &mut statics,
            false,
            &target,
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        );

        let (enhanced_ast_module, errors) =
            EnhancedASTModule::new(modules, &project, &mut statics, &target, false);

        if !errors.is_empty() {
            panic!("{}", SliceDisplay(&errors));
        }

        let (module, errors) = project.get_module(Path::new(module_path), &target).unwrap();

        if !errors.is_empty() {
            panic!("{}", SliceDisplay(&errors));
        }

        (enhanced_ast_module, module)
    }

    fn init_log() {
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

    fn get_completion_values(
        project: Option<RasmProject>,
        file_name: &str,
        row: usize,
        col: usize,
        trigger: CompletionTrigger,
    ) -> Result<Vec<String>, String> {
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let project = if let Some(project) = project {
            project
        } else {
            RasmProject::new(PathBuf::from(file_name))
        };
        let (eh_module, module) = get_reference_finder_for_project(&project, file_name);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Some(PathBuf::from(file_name));
        let index = ASTIndex::new(file_name.clone(), row, col);

        match finder.get_completions(&project, &index, &eh_module, &trigger) {
            Ok(CompletionResult::Found(items)) => {
                let mut sorted = items.clone();
                sorted.sort_by(|a, b| a.sort.cmp(&b.sort));

                //println!("{}", SliceDisplay(&sorted));

                Ok(sorted.iter().map(|it| it.value.clone()).collect::<Vec<_>>())
            }
            Ok(CompletionResult::NotFound(message)) => Err(message.to_string()),
            Err(error) => Err(error.to_string()),
        }
    }

    fn get_find(
        project: Option<RasmProject>,
        file_name: &str,
        row: usize,
        col: usize,
    ) -> Vec<ASTIndex> {
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let project = if let Some(project) = project {
            project
        } else {
            RasmProject::new(PathBuf::from(file_name))
        };
        let (eh_module, module) = get_reference_finder_for_project(&project, file_name);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Some(PathBuf::from(file_name));
        let index = ASTIndex::new(file_name.clone(), row, col);

        vec_selectable_item_to_vec_index(finder.find(&index).unwrap())
    }
}
