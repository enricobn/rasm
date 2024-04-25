use std::collections::HashMap;
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
use rasm_core::type_check::functions_container::TypeFilter;
use rasm_core::type_check::substitute;
use rasm_core::type_check::type_check_error::TypeCheckError;

use crate::completion_service::{CompletionItem, CompletionResult};
use crate::reference_context::ReferenceContext;
use crate::selectable_item::{SelectableItem, SelectableItemTarget};

pub struct ReferenceFinder {
    selectable_items: Vec<SelectableItem>,
    lookup_tables: ReferenceFinderLookupTables,
    path: PathBuf,
}

struct ReferenceFinderLookupTables {
    functions_by_index: HashMap<ASTIndex, String>,
    types_by_index: HashMap<ASTIndex, ASTType>,
    parameters_by_index: HashMap<ASTIndex, ASTParameterDef>,
    let_by_index: HashMap<ASTIndex, ASTType>,
}

impl ReferenceFinderLookupTables {
    fn new() -> Self {
        Self {
            functions_by_index: Default::default(),
            types_by_index: Default::default(),
            parameters_by_index: Default::default(),
            let_by_index: Default::default(),
        }
    }
}

impl ReferenceFinder {
    pub fn new(module: &EnhancedASTModule, ast_module: &ASTModule) -> Result<Self, TypeCheckError> {
        let mut lookup_tables = ReferenceFinderLookupTables::new();

        let path = ast_module.path.clone();
        let selectable_items = Self::process_module(module, ast_module, &mut lookup_tables)?;

        //println!("selectable_items {}", SliceDisplay(&selectable_items));

        Ok(Self {
            selectable_items,
            lookup_tables,
            path,
        })
    }

    pub fn find(&self, index: &ASTIndex) -> Result<Vec<SelectableItem>, io::Error> {
        let mut result = Vec::new();

        for selectable_item in self.selectable_items.iter() {
            /*
            if selectable_item.file_token.start.row == 31 {
                println!("{selectable_item}");
            }

             */
            if selectable_item.contains(index)? {
                //println!("found {:?}", selectable_item);
                result.push(selectable_item.clone());
            }
        }

        Ok(result)
    }

    pub fn get_completions(
        &self,
        index: &ASTIndex,
        enhanched_module: &EnhancedASTModule,
    ) -> Result<CompletionResult, io::Error> {
        let index = index.mv_left(2);
        for selectable_item in self.selectable_items.iter() {
            if selectable_item.contains(&index)? {
                if let Some(ref ast_type) = selectable_item.ast_type {
                    let filter = TypeFilter::Exact(ast_type.clone());
                    let mut items = Vec::new();
                    for function in enhanched_module.functions() {
                        if !function.modifiers.public
                            && function.namespace != selectable_item.namespace
                        {
                            continue;
                        }
                        if !function.parameters.is_empty() {
                            let parameter_type = &function.parameters.get(0).unwrap().ast_type;
                            if let Ok(value) = filter.almost_equal(parameter_type, enhanched_module)
                            {
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
            }
        }

        Ok(CompletionResult::NotFound(
            "Cannot find completion".to_owned(),
        ))
    }

    pub fn find_function(&self, index: &ASTIndex) -> Option<&String> {
        self.lookup_tables.functions_by_index.get(index)
    }

    pub fn is_path(&self, path: &PathBuf) -> bool {
        &self.path == path
    }

    fn process_module(
        enhanced_module: &EnhancedASTModule,
        module: &ASTModule,
        lookup_tables: &mut ReferenceFinderLookupTables,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        let mut reference_context = ReferenceContext::new(None);
        let mut reference_static_context = ReferenceContext::new(None);
        let mut val_context = ValContext::new(None);
        let mut statics = Statics::new();

        let mut result = Vec::new();

        let mut type_check = TypeCheck::new(&enhanced_module.body_namespace, false);

        Self::process_statics(enhanced_module, &mut reference_static_context, &mut statics);

        Self::process_statements(
            enhanced_module,
            &module.body,
            &mut result,
            &mut reference_context,
            &mut reference_static_context,
            &module.namespace,
            &mut val_context,
            &mut statics,
            lookup_tables,
            Some(&ASTType::Unit),
            None,
            &mut type_check,
        )?;

        module.functions.iter().for_each(|function| {
            lookup_tables
                .functions_by_index
                .insert(function.index.clone(), format!("{function}"));
        });

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
                        lookup_tables,
                        &mut type_check,
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
    ) {
        let mut result = Vec::new();
        let mut reference_context = ReferenceContext::new(None);
        let mut val_context = ValContext::new(None);
        let mut type_check = TypeCheck::new(&ASTNameSpace::global(), false);
        let mut lookup_tables = ReferenceFinderLookupTables::new();

        let _ = Self::process_statements(
            enhanced_module,
            &enhanced_module.body,
            &mut result,
            &mut reference_context,
            reference_static_context,
            &ASTNameSpace::global(),
            &mut val_context,
            statics,
            &mut lookup_tables,
            None,
            None,
            &mut type_check,
        );
    }

    fn process_function(
        function: &ASTFunctionDef,
        module: &EnhancedASTModule,
        reference_static_context: &ReferenceContext,
        val_context: &mut ValContext,
        statics: &mut Statics,
        lookup_tables: &mut ReferenceFinderLookupTables,
        type_check: &mut TypeCheck,
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
            Self::process_statements(
                module,
                statements,
                &mut result,
                &mut reference_context,
                &mut ReferenceContext::new(None),
                &function.namespace,
                val_context,
                statics,
                lookup_tables,
                Some(&function.return_type),
                Some(function),
                type_check,
            )?;
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

        if let Some(custom_type_index) = Self::get_custom_type_index(module, name) {
            let item = SelectableItem::new(
                min,
                name.len(),
                custom_type_index.clone(),
                None,
                Some(ast_type.clone()),
                Some(custom_type_index),
                namespace.clone(),
                SelectableItemTarget::Type,
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
        lookup_tables: &mut ReferenceFinderLookupTables,
        expected_return_type: Option<&ASTType>,
        inside_function: Option<&ASTFunctionDef>,
        type_check: &mut TypeCheck,
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
                lookup_tables,
                expected_type,
                inside_function,
                type_check,
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
        type_check.type_of_expression(
            enhanced_ast_module,
            expr,
            val_context,
            statics,
            expected_type,
            namespace,
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
        lookup_tables: &mut ReferenceFinderLookupTables,
        expected_type: Option<&ASTType>,
        inside_function: Option<&ASTFunctionDef>,
        type_check: &mut TypeCheck,
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
                lookup_tables,
                inside_function,
                type_check,
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

                Self::process_expression(
                    expr,
                    reference_context,
                    reference_static_context,
                    module,
                    namespace,
                    val_context,
                    statics,
                    None,
                    lookup_tables,
                    inside_function,
                    type_check,
                )
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
        lookup_tables: &mut ReferenceFinderLookupTables,
        inside_function: Option<&ASTFunctionDef>,
        type_check: &mut TypeCheck,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        let mut result = Vec::new();
        match expr {
            ASTExpression::StringLiteral(_) => {}
            ASTExpression::ASTFunctionCallExpression(call) => {
                Self::process_function_call(
                    expr,
                    reference_context,
                    reference_static_context,
                    module,
                    namespace,
                    val_context,
                    statics,
                    lookup_tables,
                    &mut result,
                    call,
                    expected_type,
                    inside_function,
                    type_check,
                )?;
            }
            ASTExpression::ValueRef(name, index) => {
                Self::process_value_ref(
                    expr,
                    reference_context,
                    reference_static_context,
                    module,
                    namespace,
                    &mut result,
                    name,
                    index,
                );
            }
            ASTExpression::Value(_value_type, _index) => {}
            ASTExpression::Lambda(def) => {
                Self::process_lambda(
                    reference_context,
                    module,
                    namespace,
                    val_context,
                    statics,
                    expected_type,
                    lookup_tables,
                    &mut result,
                    def,
                    type_check,
                )?;
            }
            ASTExpression::Any(_) => {}
        }
        Ok(result)
    }

    fn process_value_ref(
        expr: &ASTExpression,
        reference_context: &mut ReferenceContext,
        reference_static_context: &mut ReferenceContext,
        module: &EnhancedASTModule,
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

            let ast_type_index = ast_type
                .as_ref()
                .and_then(|t| Self::get_if_custom_type_index(module, t));

            result.push(SelectableItem::new(
                index.mv_left(name.len()),
                name.len(),
                v.index.mv_left(name.len()).clone(),
                Some(expr.clone()),
                ast_type,
                ast_type_index,
                namespace.clone(),
                SelectableItemTarget::Ref,
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
        lookup_tables: &mut ReferenceFinderLookupTables,
        result: &mut Vec<SelectableItem>,
        def: &ASTLambdaDef,
        type_check: &mut TypeCheck,
    ) -> Result<(), TypeCheckError> {
        let mut lambda_reference_context = ReferenceContext::new(Some(reference_context));
        let mut lambda_val_context = ValContext::new(Some(val_context));

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
                        par_index,
                        TypeFilter::Exact(par_type.clone()),
                    );
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
        let mut lambda_result = Vec::new();
        Self::process_statements(
            module,
            &def.body,
            &mut lambda_result,
            &mut lambda_reference_context,
            &mut ReferenceContext::new(None),
            namespace,
            &mut lambda_val_context,
            statics,
            lookup_tables,
            expected_lambda_return_type,
            None, // TODO
            type_check,
        )?;
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
        lookup_tables: &mut ReferenceFinderLookupTables,
        result: &mut Vec<SelectableItem>,
        call: &ASTFunctionCall,
        expected_return_type: Option<&ASTType>,
        inside_function: Option<&ASTFunctionDef>,
        type_check: &mut TypeCheck,
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

            let custom_type_index = Self::get_if_custom_type_index(module, &ast_type);

            result.push(SelectableItem::new(
                call.index.mv_left(call.original_function_name.len()),
                call.original_function_name.len(),
                index,
                Some(expr.clone()),
                Some(ast_type.clone()),
                custom_type_index,
                namespace.clone(),
                SelectableItemTarget::Ref,
            ));

            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters,
                return_type: _,
            }) = ast_type
            {
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
                            lookup_tables,
                            inside_function,
                            type_check,
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
        ) {
            Ok((function_def, resolved_generic_types, expressions)) => {
                // println!("expressions {}", SliceDisplay(&expressions));
                let custom_type_index =
                    Self::get_if_custom_type_index(module, &function_def.return_type);
                let mut fd = function_def.clone();
                fd.name = fd.original_name.clone();

                lookup_tables
                    .functions_by_index
                    .insert(function_def.index.clone(), format!("{fd}"));
                result.push(SelectableItem::new(
                    call.index.mv_left(call.original_function_name.len()),
                    call.original_function_name.len(),
                    function_def.index.clone(),
                    Some(expr.clone()),
                    Some(function_def.return_type.clone()),
                    custom_type_index,
                    namespace.clone(),
                    SelectableItemTarget::Function,
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
                            lookup_tables,
                            inside_function,
                            type_check,
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
                            lookup_tables,
                            inside_function,
                            type_check,
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
                warn!("Cannot find function for call {call}:\n{e}");
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
    use rasm_core::parser::ast::{ASTIndex, ASTModule, ASTType};
    use rasm_core::project::RasmProject;
    use rasm_core::utils::{OptionDisplay, SliceDisplay};

    use crate::completion_service::CompletionItem;
    use crate::reference_finder::{CompletionResult, ReferenceFinder};
    use crate::selectable_item::{SelectableItem, SelectableItemTarget};

    #[test]
    fn simple() {
        let (eh_module, module) = get_reference_finder("resources/test/simple.rasm", None);
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
                Some(stdlib_path.join("src/main/rasm/std.rasm")),
                10,
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
        let (eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/types.rasm");
        let project = RasmProject::new(file_name.to_path_buf());
        let source_file = Path::new(&file_name);
        let stdlib_path = stdlib_path(file_name);

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(source_file.to_path_buf()), 14, 23))
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
                    .find(&ASTIndex::new(Some(source_file.to_path_buf()), 18, 23,))
                    .unwrap()
            ),
            vec![get_index(&project, "types.rasm", 1, 8)],
        );

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(source_file.to_path_buf()), 22, 23,))
                    .unwrap()
            ),
            vec![ASTIndex::new(
                Some(stdlib_path.join("src/main/rasm/vec.rasm")),
                1,
                10
            )],
        );
    }

    #[test]
    fn complex_expression() {
        let (eh_module, module) =
            get_reference_finder("resources/test/complex_expression.rasm", None);
        let file_name = Path::new("resources/test/complex_expression.rasm");

        let project = RasmProject::new(file_name.to_path_buf());
        let _ = project
            .from_relative_to_root(Path::new("../../../stdlib"))
            .canonicalize()
            .unwrap();
    }

    #[test]
    fn struct_constructor() {
        let (eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/types.rasm");

        let project = RasmProject::new(file_name.to_path_buf());

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
        let (eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = PathBuf::from("resources/test/types.rasm");
        let project = RasmProject::new(file_name.to_path_buf());
        let found = finder
            .find(&ASTIndex::new(Some(file_name.clone()), 26, 31))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(found),
            vec!(get_index(&project, "types.rasm", 1, 8,)),
        );
    }

    #[test]
    fn types_2() {
        let (eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = PathBuf::from("resources/test/types.rasm");
        let project = RasmProject::new(file_name.to_path_buf());
        let found = finder
            .find(&ASTIndex::new(Some(file_name.clone()), 9, 1))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(found),
            vec!(get_index(&project, "types.rasm", 14, 4,)),
        );
    }

    #[test]
    fn types_3() {
        let (eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = PathBuf::from("resources/test/types.rasm");
        let project = RasmProject::new(file_name.to_path_buf());
        let found = finder
            .find(&ASTIndex::new(Some(file_name.clone()), 9, 13))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(found),
            vec!(get_index(&project, "types.rasm", 5, 5,)),
        );
    }

    #[test]
    fn types_completion() {
        let (eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Some(PathBuf::from("resources/test/types.rasm"));

        match finder.get_completions(&ASTIndex::new(file_name.clone(), 12, 17), &eh_module) {
            Ok(CompletionResult::Found(items)) => {
                assert!(format_collection_items(&items)
                    .contains(&"anI32(v:AStruct) -> i32".to_string()));
            }
            Ok(CompletionResult::NotFound(message)) => panic!("{message}"),
            Err(error) => {
                panic!("{error}")
            }
        }
    }

    #[test]
    fn types_lambda_param_type() {
        let (eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Some(PathBuf::from("resources/test/types.rasm"));

        match finder.find(&ASTIndex::new(file_name.clone(), 31, 22)) {
            Ok(mut selectable_items) => {
                if selectable_items.len() == 1 {
                    let selectable_item = selectable_items.remove(0);
                    if let Some(ASTType::Custom {
                        namespace: _,
                        name,
                        param_types: _,
                        index: _,
                    }) = selectable_item.ast_type
                    {
                        assert_eq!("AStruct", name);
                    } else {
                        panic!(
                            "Expected some Custom type but got {}",
                            OptionDisplay(&selectable_item.ast_type)
                        );
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

        let (eh_module, module) = get_reference_finder(
            "resources/test/types.rasm",
            Some(&result_rasm.to_string_lossy()),
        );
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        match finder.find(&ASTIndex::new(Some(result_rasm.clone()), 11, 9)) {
            Ok(mut selectable_items) => {
                if selectable_items.len() == 1 {
                    let selectable_item = selectable_items.remove(0);
                    assert_eq!(selectable_item.target, SelectableItemTarget::Function);
                    assert_eq!(
                        selectable_item.point_to,
                        ASTIndex::new(Some(result_rasm), 14, 8)
                    );
                } else {
                    panic!("Found {} elements", selectable_items.len());
                }
            }
            Err(e) => {
                panic!("{e}");
            }
        }
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
        vec.iter().map(|it| it.point_to.clone()).collect::<Vec<_>>()
    }

    fn get_reference_finder(
        source: &str,
        module_path: Option<&str>,
    ) -> (EnhancedASTModule, ASTModule) {
        init_log();
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let file_name = Path::new(source);

        let project = RasmProject::new(file_name.to_path_buf());

        let mut statics = Statics::new();
        let target = CompileTarget::Nasmi386(AsmOptions::default());
        let (modules, _errors) = project.get_all_modules(&mut statics, false, &target, false);
        // TODO errors
        let enhanced_ast_module =
            EnhancedASTModule::new(modules, &project, &mut statics, &target, false);

        let (module, errors) = if let Some(m) = module_path {
            project.get_module(Path::new(&m), &target)
        } else {
            project.get_module(file_name, &target)
        }
        .unwrap();

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
}
