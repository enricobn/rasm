use std::fmt::{Display, Formatter};
use std::io;
use std::path::PathBuf;

use log::warn;

use rasm_core::codegen::enhanced_module::EnhancedASTModule;
use rasm_core::parser::ast::{
    ASTEnumDef, ASTExpression, ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTNameSpace,
    ASTStatement, ASTStructDef, ASTType, ASTTypeDef, BuiltinTypeKind,
};
use rasm_core::type_check::functions_container::{FunctionsContainer, TypeFilter};
use rasm_core::type_check::type_check_error::TypeCheckError;
use rasm_core::utils::SliceDisplay;

use crate::completion_service::{CompletionItem, CompletionResult};
use crate::reference_context::ReferenceContext;

pub struct ReferenceFinder {
    selectable_items: Vec<SelectableItem>,
    module: EnhancedASTModule,
}

impl ReferenceFinder {
    pub fn new(module: EnhancedASTModule) -> Result<Self, TypeCheckError> {
        let selectable_items = Self::process_module(&module)?;

        //println!("selectable_items {}", SliceDisplay(&selectable_items));

        Ok(Self {
            selectable_items,
            module,
        })
    }

    pub fn find(&self, index: &ASTIndex) -> Result<Vec<SelectableItem>, io::Error> {
        let mut result = Vec::new();

        for selectable_item in self.selectable_items.iter() {
            if selectable_item.contains(index)? {
                //println!("found {:?}", selectable_item);
                result.push(selectable_item.clone());
            }
        }

        Ok(result)
    }

    pub fn get_completions(&self, index: &ASTIndex) -> Result<CompletionResult, io::Error> {
        let index = index.mv_left(2);
        for selectable_item in self.selectable_items.iter() {
            if selectable_item.contains(&index)? {
                if let Some(ref ast_type) = selectable_item.ast_type {
                    let filter = TypeFilter::Exact(ast_type.clone());
                    let mut items = Vec::new();
                    for function in self.module.functions() {
                        if !function.modifiers.public
                            && function.namespace != selectable_item.namespace
                        {
                            continue;
                        }
                        if !function.parameters.is_empty() {
                            let parameter_type = &function.parameters.get(0).unwrap().ast_type;
                            if let Ok(value) = filter.almost_equal(parameter_type) {
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

    fn process_module(module: &EnhancedASTModule) -> Result<Vec<SelectableItem>, TypeCheckError> {
        let mut reference_context = ReferenceContext::new(None);
        let mut reference_static_context = ReferenceContext::new(None);

        let mut result = Vec::new();

        Self::process_statements(
            module,
            &module.body,
            &mut result,
            &mut reference_context,
            &mut reference_static_context,
            &module.body_namespace,
        )?;

        result.append(
            &mut module
                .functions()
                .iter()
                .flat_map(|it| {
                    Self::process_function(it, module, &reference_static_context)
                        .unwrap_or_default()
                })
                .collect(),
        );

        Ok(result)
    }

    fn process_function(
        function: &ASTFunctionDef,
        module: &EnhancedASTModule,
        reference_static_context: &ReferenceContext,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        let mut result = Vec::new();

        let mut val_context = ReferenceContext::new(Some(reference_static_context));

        for par in function.parameters.iter() {
            val_context.add(
                par.name.clone(),
                par.ast_index.clone(),
                TypeFilter::Exact(par.ast_type.clone()),
            );
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
                &mut val_context,
                &mut ReferenceContext::new(None),
                &function.namespace,
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
    ) -> Result<(), TypeCheckError> {
        for stmt in statements {
            if let ASTStatement::LetStatement(name, expr, is_const, index) = stmt {
                let filter = Self::get_filter_of_expression(
                    expr,
                    reference_context,
                    reference_static_context,
                    &module.functions_by_name,
                )?;
                reference_context.add(name.clone(), index.clone(), filter.clone());
                if *is_const {
                    reference_static_context.add(name.clone(), index.clone(), filter);
                }
            }

            match Self::get_selectable_items_stmt(
                stmt,
                reference_context,
                reference_static_context,
                module,
                namespace,
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
        reference_context: &ReferenceContext,
        reference_static_context: &ReferenceContext,
        functions_container: &FunctionsContainer,
    ) -> Result<TypeFilter, TypeCheckError> {
        let result = match expr {
            ASTExpression::ASTFunctionCallExpression(call) => {
                let filters = &call
                    .parameters
                    .iter()
                    .map(|it| {
                        Self::get_filter_of_expression(
                            it,
                            reference_context,
                            reference_static_context,
                            functions_container,
                        )
                    })
                    .collect::<Result<Vec<_>, TypeCheckError>>()?;
                let functions = functions_container.find_call_vec(call, filters, None, false)?;
                if functions.len() == 1 {
                    TypeFilter::Exact(functions.first().unwrap().return_type.clone())
                } else {
                    TypeFilter::Any
                }
            }
            ASTExpression::Lambda(def) => TypeFilter::Lambda(def.parameter_names.len(), None),
            ASTExpression::StringLiteral(_) => {
                TypeFilter::Exact(ASTType::Builtin(BuiltinTypeKind::String))
            }
            ASTExpression::ValueRef(name, index) => reference_context
                .get(name)
                .or_else(|| reference_static_context.get(name))
                .ok_or_else(|| {
                    TypeCheckError::new(
                        index.clone(),
                        format!("cannot find ref to '{name}'"),
                        Vec::new(),
                    )
                })?
                .filter
                .clone(),
            ASTExpression::Value(value_type, _index) => TypeFilter::Exact(value_type.to_type()),
            ASTExpression::Any(ast_type) => TypeFilter::Exact(ast_type.clone()),
        };

        Ok(result)
    }

    fn get_selectable_items_stmt(
        stmt: &ASTStatement,
        reference_context: &mut ReferenceContext,
        reference_static_context: &mut ReferenceContext,
        module: &EnhancedASTModule,
        namespace: &ASTNameSpace,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        match stmt {
            ASTStatement::Expression(expr) => Self::get_selectable_items_expr(
                expr,
                reference_context,
                reference_static_context,
                module,
                namespace,
            ),
            ASTStatement::LetStatement(name, expr, _, index) => Self::get_selectable_items_expr(
                expr,
                reference_context,
                reference_static_context,
                module,
                namespace,
            ),
        }
    }

    fn get_selectable_items_expr(
        expr: &ASTExpression,
        reference_context: &mut ReferenceContext,
        reference_static_context: &mut ReferenceContext,
        module: &EnhancedASTModule,
        namespace: &ASTNameSpace,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        let mut result = Vec::new();
        match expr {
            ASTExpression::StringLiteral(_) => {}
            ASTExpression::ASTFunctionCallExpression(call) => {
                // TODO call
                let mut v = call
                    .parameters
                    .iter()
                    .flat_map(|it| {
                        match Self::get_selectable_items_expr(
                            it,
                            reference_context,
                            reference_static_context,
                            module,
                            namespace,
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

                let filters: Vec<TypeFilter> = call
                    .parameters
                    .iter()
                    .map(|it| {
                        Self::get_filter_of_expression(
                            it,
                            reference_context,
                            reference_static_context,
                            &module.functions_by_name,
                        )
                    })
                    .collect::<Result<Vec<_>, TypeCheckError>>()?;

                let mut functions = module
                    .functions_by_name
                    .find_call_vec(call, &filters, None, false)
                    .unwrap();

                if functions.len() == 1 {
                    let function = functions.remove(0);

                    let custom_type_index =
                        Self::get_if_custom_type_index(module, &function.return_type);

                    result.push(SelectableItem::new(
                        call.index.mv_left(call.original_function_name.len()),
                        call.original_function_name.len(),
                        function.index.clone(),
                        Some(expr.clone()),
                        Some(function.return_type.clone()),
                        custom_type_index,
                        namespace.clone(),
                    ));
                } else {
                    warn!(
                        "Cannot find function for call {call} with filters {} : {}",
                        SliceDisplay(&filters),
                        call.index
                    );
                    warn!("{}", SliceDisplay(&functions));
                }
            }
            ASTExpression::ValueRef(name, index) => {
                if let Some(v) = reference_context.get(name) {
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
                    ));
                }
            }
            ASTExpression::Value(value_type, index) => {}
            ASTExpression::Lambda(def) => {
                let mut lambda_context = ReferenceContext::new(Some(reference_context));

                for (name, index) in def.parameter_names.iter() {
                    lambda_context.add(name.clone(), index.clone(), TypeFilter::Any);
                }

                let mut lambda_result = Vec::new();
                Self::process_statements(
                    module,
                    &def.body,
                    &mut lambda_result,
                    &mut lambda_context,
                    &mut ReferenceContext::new(None),
                    namespace,
                )?;
                result.append(&mut lambda_result);
            }
            ASTExpression::Any(_) => {}
        }
        Ok(result)
    }

    fn get_if_custom_type_index(
        module: &EnhancedASTModule,
        ast_type: &ASTType,
    ) -> Option<ASTIndex> {
        if let ASTType::Custom {
            namespace: _,
            name,
            param_types,
            index,
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

#[derive(Debug, Clone)]
pub struct FileToken {
    start: ASTIndex,
    len: usize,
}

impl FileToken {
    pub fn new(start: ASTIndex, len: usize) -> Self {
        Self { start, len }
    }

    pub fn contains(&self, index: &ASTIndex) -> io::Result<bool> {
        Ok(index.row == self.start.row
            && index.column >= self.start.column
            && index.column <= (self.start.column + self.len - 1)
            && Self::path_matches(&index.file_name, &self.start.file_name)?)
    }

    fn path_matches(op1: &Option<PathBuf>, op2: &Option<PathBuf>) -> io::Result<bool> {
        if let Some(p1) = op1 {
            if let Some(p2) = op2 {
                if p1.file_name() != p2.file_name() {
                    return Ok(false);
                }
                let p1_canon = p1.canonicalize()?;

                let p2_canon = p2.canonicalize()?;

                return Ok(p1_canon == p2_canon);
            }
        }

        Ok(false)
    }
}

impl Display for FileToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let file_name = self
            .start
            .file_name
            .as_ref()
            .and_then(|it| it.to_str().map(|s| format!("file:///{s}")))
            .unwrap_or("".to_string());

        f.write_str(&format!(
            "{file_name} {}:{} len {}",
            self.start.row, self.start.column, self.len
        ))
    }
}

#[derive(Debug, Clone)]
pub struct SelectableItem {
    file_token: FileToken,
    pub point_to: ASTIndex,
    expr: Option<ASTExpression>,
    pub ast_type: Option<ASTType>,
    pub ast_type_index: Option<ASTIndex>,
    pub namespace: ASTNameSpace,
}

impl Display for SelectableItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{} -> {}", self.file_token, self.point_to))
    }
}

impl SelectableItem {
    pub fn new(
        start: ASTIndex,
        len: usize,
        point_to: ASTIndex,
        expr: Option<ASTExpression>,
        ast_type: Option<ASTType>,
        ast_type_index: Option<ASTIndex>,
        namespace: ASTNameSpace,
    ) -> Self {
        SelectableItem {
            file_token: FileToken::new(start, len),
            point_to,
            expr,
            ast_type,
            ast_type_index,
            namespace,
        }
    }

    fn contains(&self, index: &ASTIndex) -> io::Result<bool> {
        self.file_token.contains(index)
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Write;
    use std::path::{Path, PathBuf};

    use env_logger::Builder;

    use crate::completion_service::CompletionItem;
    use rasm_core::codegen::backend::BackendNasmi386;
    use rasm_core::codegen::enhanced_module::EnhancedASTModule;
    use rasm_core::codegen::statics::Statics;
    use rasm_core::codegen::CompileTarget;
    use rasm_core::parser::ast::ASTIndex;
    use rasm_core::project::RasmProject;

    use crate::reference_finder::{CompletionResult, ReferenceFinder, SelectableItem};

    #[test]
    fn simple() {
        let finder = get_reference_finder("resources/test/simple.rasm");

        let file_name = Path::new("resources/test/simple.rasm");

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(file_name.to_path_buf()), 3, 15,))
                    .unwrap()
            ),
            vec![ASTIndex::new(Some(file_name.to_path_buf()), 1, 5)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_index(
                finder
                    .find(&ASTIndex::new(Some(file_name.to_path_buf()), 6, 13,))
                    .unwrap()
            ),
            vec![ASTIndex::new(Some(file_name.to_path_buf()), 5, 16)]
        );
    }

    #[test]
    fn types() {
        let finder = get_reference_finder("resources/test/types.rasm");

        let file_name = Path::new("resources/test/types.rasm");
        let source_file = Path::new(&file_name);

        let project = RasmProject::new(file_name.to_path_buf());

        let stdlib_path = project
            .from_relative_to_root(Path::new("../../../stdlib"))
            .canonicalize()
            .unwrap();

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
            vec![ASTIndex::new(Some(source_file.to_path_buf()), 1, 8)],
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
    fn types_1() {
        let finder = get_reference_finder("resources/test/types.rasm");

        let file_name = Some(PathBuf::from("resources/test/types.rasm"));
        let found = finder
            .find(&ASTIndex::new(file_name.clone(), 26, 31))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(found),
            vec!(ASTIndex::new(file_name, 1, 8,)),
        );
    }

    #[test]
    fn types_2() {
        let finder = get_reference_finder("resources/test/types.rasm");

        let file_name = Some(PathBuf::from("resources/test/types.rasm"));
        let found = finder
            .find(&ASTIndex::new(file_name.clone(), 9, 1))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(found),
            vec!(ASTIndex::new(file_name, 14, 4,)),
        );
    }

    #[test]
    fn types_3() {
        let finder = get_reference_finder("resources/test/types.rasm");

        let file_name = Some(PathBuf::from("resources/test/types.rasm"));
        let found = finder
            .find(&ASTIndex::new(file_name.clone(), 9, 13))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_index(found),
            vec!(ASTIndex::new(file_name, 5, 5,)),
        );
    }

    #[test]
    fn types_completion() {
        let finder = get_reference_finder("resources/test/types.rasm");

        let file_name = Some(PathBuf::from("resources/test/types.rasm"));
        match finder.get_completions(&ASTIndex::new(file_name.clone(), 12, 17)) {
            Ok(CompletionResult::Found(items)) => {
                assert!(format_collection_items(&items)
                    .contains(&"anI32(v::types:AStruct) -> i32".to_string()));
            }
            Ok(CompletionResult::NotFound(message)) => panic!("{message}"),
            Err(error) => {
                panic!("{error}")
            }
        }
    }

    fn format_collection_items(items: &[CompletionItem]) -> Vec<String> {
        items.iter().map(|it| it.descr.clone()).collect::<Vec<_>>()
    }

    fn vec_selectable_item_to_vec_index(vec: Vec<SelectableItem>) -> Vec<ASTIndex> {
        vec.iter().map(|it| it.point_to.clone()).collect::<Vec<_>>()
    }

    fn get_reference_finder(source: &str) -> ReferenceFinder {
        init();
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let file_name = Path::new(source);

        let project = RasmProject::new(file_name.to_path_buf());

        let mut backend = BackendNasmi386::new(false);
        let mut statics = Statics::new();
        let (modules, errors) =
            project.get_all_modules(&mut backend, &mut statics, false, &CompileTarget::Nasmi36);
        let enhanced_astmodule = EnhancedASTModule::new(modules, &project, &backend, &mut statics);

        ReferenceFinder::new(enhanced_astmodule).unwrap()
    }

    fn init() {
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
