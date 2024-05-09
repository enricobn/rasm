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
    path: PathBuf,
}

impl ReferenceFinder {
    pub fn new(module: &EnhancedASTModule, ast_module: &ASTModule) -> Result<Self, TypeCheckError> {
        let path = ast_module.path.clone();
        let selectable_items = Self::process_module(module, ast_module)?;

        Ok(Self {
            selectable_items,
            path,
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
        index: &ASTIndex,
        enhanched_module: &EnhancedASTModule,
    ) -> Result<CompletionResult, io::Error> {
        let index = index.mv_left(2);
        for selectable_item in self.selectable_items.iter() {
            if selectable_item.contains(&index)? {
                if let Some(ast_type) = selectable_item.target.completion_type() {
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

    pub fn is_path(&self, path: &PathBuf) -> bool {
        &self.path == path
    }

    pub fn references(&self, index: &ASTIndex) -> Result<Vec<SelectableItem>, io::Error> {
        let mut items = self.find(index)?;

        if items.len() == 1 {
            let item = items.remove(0);

            if let Some(target_index) = item.target.index() {
                let mut result = Vec::new();
                for se in self.selectable_items.iter() {
                    if let Some(i) = se.target.index() {
                        if i == target_index {
                            result.push(se.clone());
                        }
                    }
                }

                return Ok(result);
            }
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
            Some(&ASTType::Unit),
            None,
            &mut type_check,
        )?;

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
        let mut type_check = TypeCheck::new(&enhanced_module.body_namespace, false);

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
        );
    }

    fn process_function(
        function: &ASTFunctionDef,
        module: &EnhancedASTModule,
        reference_static_context: &ReferenceContext,
        val_context: &mut ValContext,
        statics: &mut Statics,
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

        if let ASTType::Custom { .. } = ast_type {
            let item = SelectableItem::new(
                min,
                name.len(),
                namespace.clone(),
                SelectableItemTarget::Type(
                    Self::get_custom_type_index(module, name),
                    ast_type.clone(),
                ),
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

                let index1 = index.mv_left(name.len());
                let mut result = vec![SelectableItem::new(
                    index1.clone(),
                    name.len(),
                    namespace.clone(),
                    SelectableItemTarget::Ref(index1, None),
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
                    &mut result,
                    call,
                    expected_type,
                    inside_function,
                    type_check,
                )?;
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
                Self::process_lambda(
                    reference_context,
                    module,
                    namespace,
                    val_context,
                    statics,
                    expected_type,
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
                SelectableItemTarget::Ref(v.index.mv_left(name.len()).clone(), ast_type),
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
                    SelectableItemTarget::Ref(index, Some(cloned_type)),
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
                let mut fd = function_def.clone();
                fd.name = fd.original_name.clone();

                let descr = format!("{function_def}");
                result.push(SelectableItem::new(
                    call.index.mv_left(call.original_function_name.len()),
                    call.original_function_name.len(),
                    namespace.clone(),
                    SelectableItemTarget::Function(
                        function_def.index,
                        function_def.return_type,
                        descr,
                    ),
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
                Some(stdlib_path.join("src/main/rasm/vec.rasm")),
                1,
                10
            )],
        );
    }

    #[test]
    fn complex_expression() {
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
            .find(&ASTIndex::new(Some(file_name.clone()), 27, 31))
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
            vec!(get_index(&project, "types.rasm", 15, 4,)),
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
                    .contains(&"anI32(v: AStruct) -> i32".to_string()));
            }
            Ok(CompletionResult::NotFound(message)) => panic!("{message}"),
            Err(error) => {
                panic!("{error}")
            }
        }
    }

    #[test]
    fn types_completion1() {
        let (eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Some(PathBuf::from("resources/test/types.rasm"));

        match finder.get_completions(&ASTIndex::new(file_name.clone(), 13, 8), &eh_module) {
            Ok(CompletionResult::Found(items)) => {
                let result = items
                    .iter()
                    .filter(|it| it.descr.contains("match<"))
                    .collect::<Vec<_>>();

                assert_eq!(1, result.len());

                assert_eq!(
                    Some("match(\n    fn(par0) {  }, \n    {  });".to_string()),
                    result.first().and_then(|it| it.insert.clone())
                );
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

        match finder.find(&ASTIndex::new(file_name.clone(), 32, 22)) {
            Ok(mut selectable_items) => {
                if selectable_items.len() == 1 {
                    let selectable_item = selectable_items.remove(0);
                    if let Some(ASTType::Custom {
                        namespace: _,
                        name,
                        param_types: _,
                        index: _,
                    }) = selectable_item.target.completion_type()
                    {
                        assert_eq!("AStruct", name);
                    } else {
                        panic!(
                            "Expected some Custom type but got {}",
                            OptionDisplay(&selectable_item.target.completion_type())
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
                    let expected_index = ASTIndex::new(Some(result_rasm), 14, 8);
                    if let SelectableItemTarget::Function(index, _, _) = selectable_item.target {
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
        let (eh_module, module) = get_reference_finder("resources/test/enums.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/enums.rasm");

        let mut items = finder
            .find(&ASTIndex::new(Some(file_name.to_path_buf()), 17, 13))
            .unwrap();

        assert_eq!(1, items.len());

        let item = items.remove(0);

        if let SelectableItemTarget::Function(index, _, descr) = item.target {
            assert_eq!(ASTIndex::none(), index);
            assert!(descr.starts_with("native match"));
        } else {
            panic!("Found {:?}", item.target);
        }
    }

    #[test]
    fn enums_1() {
        let (eh_module, module) = get_reference_finder("resources/test/enums.rasm", None);
        let finder = ReferenceFinder::new(&eh_module, &module).unwrap();

        let file_name = Path::new("resources/test/enums.rasm");

        let mut items = finder
            .find(&ASTIndex::new(Some(file_name.to_path_buf()), 17, 40))
            .unwrap();

        assert_eq!(1, items.len());

        let item = items.remove(0);

        if let SelectableItemTarget::Ref(index, _) = item.target {
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
        let (eh_module, module) = get_reference_finder("resources/test/types.rasm", None);
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
            .flat_map(|it| it.target.index())
            .collect::<Vec<_>>()
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
