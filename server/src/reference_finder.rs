use std::fmt::{Display, Formatter};
use std::io;
use std::path::PathBuf;

use log::warn;

use rasm_core::parser::ast::{
    ASTEnumDef, ASTExpression, ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModule, ASTStatement,
    ASTStructDef, ASTType, ASTTypeDef, BuiltinTypeKind,
};
use rasm_core::type_check::functions_container::{FunctionsContainer, TypeFilter};
use rasm_core::type_check::type_check_error::TypeCheckError;
use rasm_core::utils::SliceDisplay;

use crate::reference_context::ReferenceContext;

pub struct ReferenceFinder {
    selectable_items: Vec<SelectableItem>,
    functions_container: FunctionsContainer,
}

impl ReferenceFinder {
    pub fn new(module: &ASTModule) -> Self {
        let mut functions_container = FunctionsContainer::new();

        module.functions.iter().for_each(|it| {
            functions_container.add_function(it.original_name.clone(), it.clone());
        });

        let selectable_items = Self::get_selectable_items(module, &functions_container);

        //println!("selectable_items {}", SliceDisplay(&selectable_items));

        Self {
            selectable_items,
            functions_container,
        }
    }

    pub fn find(&self, index: &ASTIndex) -> Result<Vec<ASTIndex>, io::Error> {
        let mut result = Vec::new();

        for selectable_item in self.selectable_items.iter() {
            if selectable_item.matches(index)? {
                //println!("found {:?}", selectable_item);
                result.push(selectable_item.point_to.clone());
            }

            if selectable_item.min.file_name == index.file_name {
                //println!("NOT found {:?}", selectable_item.min.row);
            }
        }

        Ok(result)
    }

    fn get_selectable_items(
        module: &ASTModule,
        functions_container: &FunctionsContainer,
    ) -> Vec<SelectableItem> {
        let mut reference_context = ReferenceContext::new(None);
        let mut reference_static_context = ReferenceContext::new(None);

        let mut result = Vec::new();

        Self::process_statements(
            module,
            &module.body,
            &mut result,
            &mut reference_context,
            &mut reference_static_context,
            functions_container,
        );

        result.append(
            &mut module
                .functions
                .iter()
                .flat_map(|it| {
                    Self::get_selectable_items_fn(
                        it,
                        module,
                        functions_container,
                        &reference_static_context,
                    )
                })
                .collect(),
        );

        result
    }

    fn get_selectable_items_fn(
        function: &ASTFunctionDef,
        module: &ASTModule,
        functions_container: &FunctionsContainer,
        reference_static_context: &ReferenceContext,
    ) -> Vec<SelectableItem> {
        let mut result = Vec::new();

        let mut val_context = ReferenceContext::new(Some(reference_static_context));

        for par in function.parameters.iter() {
            val_context.add(
                par.name.clone(),
                par.ast_index.clone(),
                TypeFilter::Exact(par.ast_type.clone()),
            );
            Self::process_type(module, &par.ast_type, &mut result);
        }

        Self::process_type(module, &function.return_type, &mut result);

        if let ASTFunctionBody::RASMBody(statements) = &function.body {
            Self::process_statements(
                module,
                statements,
                &mut result,
                &mut val_context,
                &mut ReferenceContext::new(None),
                functions_container,
            );
        }

        result
    }

    fn process_type(module: &ASTModule, ast_type: &ASTType, result: &mut Vec<SelectableItem>) {
        if let ASTType::Custom {
            name,
            param_types,
            index,
        } = ast_type
        {
            Self::process_custom_type(module, result, name, index);
            param_types
                .iter()
                .for_each(|it| Self::process_type(module, it, result));
        }
    }

    fn process_custom_type(
        module: &ASTModule,
        result: &mut Vec<SelectableItem>,
        name: &String,
        index: &ASTIndex,
    ) {
        let min = index.mv(-(name.len() as i32));
        if let Some(def) = Self::get_enum(module, name) {
            result.push(SelectableItem::new(min, index.clone(), def.index));
        } else if let Some(def) = Self::get_struct(module, name) {
            result.push(SelectableItem::new(min, index.clone(), def.index));
        } else if let Some(def) = Self::get_type(module, name) {
            result.push(SelectableItem::new(min, index.clone(), def.index));
        }
    }

    fn process_statements(
        module: &ASTModule,
        statements: &Vec<ASTStatement>,
        result: &mut Vec<SelectableItem>,
        reference_context: &mut ReferenceContext,
        reference_static_context: &mut ReferenceContext,
        functions_container: &FunctionsContainer,
    ) -> Result<(), TypeCheckError> {
        for stmt in statements {
            if let ASTStatement::LetStatement(name, expr, is_const, index) = stmt {
                let filter =
                    Self::get_filter_of_expression(expr, reference_context, functions_container)?;
                reference_context.add(name.clone(), index.clone(), filter.clone());
                if *is_const {
                    reference_static_context.add(name.clone(), index.clone(), filter);
                }
            }

            match Self::get_selectable_items_stmt(
                stmt,
                reference_context,
                module,
                functions_container,
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
        functions_container: &FunctionsContainer,
    ) -> Result<TypeFilter, TypeCheckError> {
        let result = match expr {
            ASTExpression::ASTFunctionCallExpression(call) => {
                let filters = &call
                    .parameters
                    .iter()
                    .map(|it| {
                        Self::get_filter_of_expression(it, reference_context, functions_container)
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
                .ok_or_else(|| {
                    TypeCheckError::from(format!("cannot find ref to '{name}' : {index}"))
                })?
                .filter
                .clone(),
            ASTExpression::Value(value_type, index) => TypeFilter::Exact(value_type.to_type()),
            ASTExpression::Any(ast_type) => TypeFilter::Exact(ast_type.clone()),
        };

        Ok(result)
    }

    fn get_selectable_items_stmt(
        stmt: &ASTStatement,
        reference_context: &mut ReferenceContext,
        module: &ASTModule,
        functions_container: &FunctionsContainer,
    ) -> Result<Vec<SelectableItem>, TypeCheckError> {
        match stmt {
            ASTStatement::Expression(expr) => Self::get_selectable_items_expr(
                expr,
                reference_context,
                module,
                functions_container,
            ),
            ASTStatement::LetStatement(name, expr, _, index) => Self::get_selectable_items_expr(
                expr,
                reference_context,
                module,
                functions_container,
            ),
        }
    }

    fn get_selectable_items_expr(
        expr: &ASTExpression,
        reference_context: &mut ReferenceContext,
        module: &ASTModule,
        functions_container: &FunctionsContainer,
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
                            module,
                            functions_container,
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
                        Self::get_filter_of_expression(it, reference_context, functions_container)
                    })
                    .collect::<Result<Vec<_>, TypeCheckError>>()?;

                let functions = functions_container
                    .find_call_vec(call, &filters, None, false)
                    .unwrap();

                if functions.len() == 1 {
                    result.push(SelectableItem::new(
                        call.index.mv(-(call.function_name.len() as i32)),
                        call.index.clone(),
                        functions.first().unwrap().index.clone(),
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
                    result.push(SelectableItem::new(
                        index.mv(-(name.len() as i32)),
                        index.clone(),
                        v.index.clone(),
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
                    functions_container,
                );
                result.append(&mut lambda_result);
            }
            ASTExpression::Any(_) => {}
        }
        Ok(result)
    }

    fn get_enum(module: &ASTModule, name: &String) -> Option<ASTEnumDef> {
        module.enums.iter().find(|it| &it.name == name).cloned()
    }

    fn get_struct(module: &ASTModule, name: &String) -> Option<ASTStructDef> {
        module.structs.iter().find(|it| &it.name == name).cloned()
    }

    fn get_type(module: &ASTModule, name: &String) -> Option<ASTTypeDef> {
        module.types.iter().find(|it| &it.name == name).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct SelectableItem {
    min: ASTIndex,
    max: ASTIndex,
    point_to: ASTIndex,
}

impl Display for SelectableItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let file_name = if self.min.file_name != self.max.file_name {
            String::new()
        } else {
            self.min
                .file_name
                .as_ref()
                .and_then(|it| it.to_str().map(|s| format!("file:///{s}")))
                .unwrap_or("".to_owned())
        };

        f.write_str(&format!(
            "{file_name} {}:{}-{}:{} {}",
            self.min.row, self.min.column, self.max.row, self.max.column, self.point_to
        ))
    }
}

impl SelectableItem {
    pub fn new(min: ASTIndex, max: ASTIndex, point_to: ASTIndex) -> Self {
        SelectableItem { min, max, point_to }
    }

    pub fn matches(&self, index: &ASTIndex) -> Result<bool, io::Error> {
        Ok(index.row == self.min.row
            && index.row == self.max.row
            && index.column >= self.min.column
            && index.column <= self.max.column
            && Self::path_matches(&index.file_name, &self.min.file_name)?
            && Self::path_matches(&index.file_name, &self.max.file_name)?)
    }

    fn path_matches(op1: &Option<PathBuf>, op2: &Option<PathBuf>) -> Result<bool, io::Error> {
        if let Some(p1) = op1 {
            if let Some(p2) = op2 {
                if p1.file_name() != p2.file_name() {
                    return Ok(false);
                }
                let p1_canon = p1.canonicalize().map_err(|it| {
                    io::Error::new(
                        it.kind(),
                        format!("Error canonilizing {}", p1.as_os_str().to_str().unwrap()),
                    )
                })?;

                let p2_canon = p2.canonicalize().map_err(|it| {
                    io::Error::new(
                        it.kind(),
                        format!("Error canonilizing {}", p2.as_os_str().to_str().unwrap()),
                    )
                })?;

                return Ok(p1_canon == p2_canon);
            }
        }

        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::env;
    use std::io::Write;
    use std::path::{Path, PathBuf};

    use env_logger::Builder;

    use rasm_core::codegen::backend::BackendNasm386;
    use rasm_core::codegen::statics::Statics;
    use rasm_core::parser::ast::ASTIndex;
    use rasm_core::project::project::RasmProject;
    use rasm_core::transformations::enrich_module;

    use crate::reference_finder::ReferenceFinder;

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

    #[test]
    fn simple() {
        let finder = get_reference_finder("resources/simple.rasm");

        let file_name = Path::new("resources/simple.rasm");
        finder.selectable_items.iter().for_each(|it| {
            if it.min.file_name == Some(file_name.to_path_buf()) {
                println!("{} {} -> {}", &it.min, &it.max, &it.point_to);
            }
        });

        assert_eq!(
            finder
                .find(&ASTIndex::new(Some(file_name.to_path_buf()), 3, 15,))
                .unwrap(),
            vec![ASTIndex::new(Some(file_name.to_path_buf()), 1, 10)]
        );

        assert_eq!(
            finder
                .find(&ASTIndex::new(Some(file_name.to_path_buf()), 6, 15,))
                .unwrap(),
            vec![ASTIndex::new(Some(file_name.to_path_buf()), 5, 21)]
        );
    }

    #[test]
    fn types() {
        let finder = get_reference_finder("resources/types.rasm");

        let file_name = Path::new("resources/types.rasm");
        let source_file = Path::new(&file_name);

        finder.selectable_items.iter().for_each(|it| {
            if it.min.file_name == Some(source_file.to_path_buf()) {
                println!("{} {} -> {}", &it.min, &it.max, &it.point_to);
            }
        });

        let project = RasmProject::new(file_name.to_path_buf());

        let stdlib_path = project
            .from_relative_to_root(Path::new("../../stdlib"))
            .canonicalize()
            .unwrap();

        assert_eq!(
            finder
                .find(&ASTIndex::new(Some(source_file.to_path_buf()), 13, 23,))
                .unwrap(),
            vec![ASTIndex::new(Some(stdlib_path.join("option.rasm")), 1, 5)]
        );

        assert_eq!(
            finder
                .find(&ASTIndex::new(Some(source_file.to_path_buf()), 17, 23,))
                .unwrap(),
            vec![ASTIndex::new(Some(source_file.to_path_buf()), 1, 7)]
        );

        assert_eq!(
            finder
                .find(&ASTIndex::new(Some(source_file.to_path_buf()), 21, 23,))
                .unwrap(),
            vec![ASTIndex::new(Some(stdlib_path.join("vec.rasm")), 1, 5)]
        );
    }

    #[test]
    fn breakout() {
        let reference_finder = get_reference_finder("../rasm/resources/examples/breakout");
        let found = reference_finder
            .find(&ASTIndex::new(
                Some(PathBuf::from(
                    "../rasm/resources/examples/breakout/breakout.rasm",
                )),
                128,
                34,
            ))
            .unwrap();

        assert_eq!(
            vec!(ASTIndex::new(
                Some(
                    PathBuf::from("../rasm/resources/examples/breakout/breakout.rasm",)
                        .canonicalize()
                        .unwrap()
                ),
                42,
                7,
            )),
            found
        );
    }

    fn get_reference_finder(source: &str) -> ReferenceFinder {
        init();
        env::set_var("RASM_STDLIB", "../../stdlib");
        let file_name = Path::new(source);

        let project = RasmProject::new(file_name.to_path_buf());

        let mut statics = Statics::new();
        let mut module = project.get_module();

        enrich_module(
            &BackendNasm386::new(HashSet::new(), HashSet::new(), false),
            project.resource_folder(),
            &mut statics,
            &mut module,
        );

        ReferenceFinder::new(&module)
    }
}
