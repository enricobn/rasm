/*
 *     RASM compiler.
 *     Copyright (C) 2022-2023  Enrico Benedetti
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

use std::io;
use std::ops::Deref;

use rasm_core::codegen::backend::BackendNasm386;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::typedef_provider::TypeDefProvider;
use rasm_core::codegen::val_context::TypedValContext;
use rasm_core::codegen::{CodeGen, TypedValKind};
use rasm_core::parser::ast::{ASTIndex, ASTModule};
use rasm_core::project::project::RasmProject;
use rasm_core::transformations::enrich_module;
use rasm_core::type_check::functions_container::TypeFilter;
use rasm_core::type_check::type_check_error::TypeCheckError;
use rasm_core::type_check::typed_ast::{
    get_type_of_typed_expression, ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef,
    ASTTypedModule, ASTTypedParameterDef, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};

pub struct CompletionItem {
    pub value: String,
    pub descr: String,
}

pub enum CompletionResult {
    Found(ASTTypedType, Vec<CompletionItem>),
    NotFound(String),
}

pub enum CompletableItemResult {
    Found(CompletableItem),
    NotFound(String),
}

#[derive(Clone)]
pub struct CompletableItem {
    min: ASTIndex,
    max: ASTIndex,
    ast_typed_type: ASTTypedType,
}

impl CompletableItem {
    fn new(min: ASTIndex, max: ASTIndex, ast_typed_type: ASTTypedType) -> Self {
        Self {
            min,
            max,
            ast_typed_type,
        }
    }
}

pub struct CompletionService {
    items: Vec<CompletableItem>,
    typed_module: ASTTypedModule,
    module: ASTModule,
}

impl CompletionService {
    pub fn new(project: &RasmProject) -> Result<Self, TypeCheckError> {
        let mut module = project.get_module();

        let mut statics = Statics::new();
        let backend = BackendNasm386::new(module.requires.clone(), module.externals.clone(), false);

        enrich_module(
            &backend,
            project.resource_folder(),
            &mut statics,
            &mut module,
        );

        let typed_module =
            CodeGen::get_typed_module(&backend, module.clone(), false, true, false, &mut statics)?;

        let mut completable_items = Vec::new();

        let mut val_context = TypedValContext::new(None);
        let mut statics = Statics::new();

        typed_module.body.iter().for_each(|it| {
            Self::process_statement(
                &typed_module,
                &mut completable_items,
                it,
                &mut val_context,
                &mut statics,
            );
        });

        typed_module.functions_by_name.values().for_each(|it| {
            Self::process_function(&typed_module, &mut completable_items, it, &mut statics);
        });

        Ok(Self {
            items: completable_items,
            module,
            typed_module,
        })
    }

    pub fn get_completions(&self, index: &ASTIndex) -> io::Result<CompletionResult> {
        match self.get_completable_item(index)? {
            CompletableItemResult::Found(completable_item) => {
                let completion_items = self
                    .module
                    .functions
                    .iter()
                    .filter(|it| {
                        !it.parameters.is_empty()
                            && TypeFilter::Exact(it.parameters.get(0).unwrap().ast_type.clone())
                                .almost_equal(
                                    &self
                                        .typed_module
                                        .get_type_from_typed_type(&completable_item.ast_typed_type)
                                        .expect(&format!(
                                            "cannot convert {}",
                                            completable_item.ast_typed_type
                                        )),
                                )
                                .unwrap()
                    })
                    .map(|it| (it.original_name.clone(), format!("{it}")))
                    .map(|(value, descr)| CompletionItem { value, descr })
                    .collect::<Vec<_>>();

                if completion_items.is_empty() {
                    return Ok(CompletionResult::NotFound(format!(
                        "cannot find items for {}",
                        completable_item.ast_typed_type
                    )));
                }

                Ok(CompletionResult::Found(
                    completable_item.ast_typed_type.clone(),
                    completion_items,
                ))
            }
            CompletableItemResult::NotFound(message) => Ok(CompletionResult::NotFound(message)),
        }
    }

    pub fn get_type(
        &self,
        index: &ASTIndex,
    ) -> io::Result<Option<(ASTTypedType, Option<ASTIndex>)>> {
        match self.get_completable_item(index)? {
            CompletableItemResult::Found(item) => {
                let index = match &item.ast_typed_type {
                    ASTTypedType::Builtin(_) => None,
                    ASTTypedType::Enum { name } => self
                        .typed_module
                        .enums
                        .iter()
                        .find(|it| &it.name == name)
                        .map(|it| it.index.clone()),
                    ASTTypedType::Struct { name } => self
                        .typed_module
                        .structs
                        .iter()
                        .find(|it| &it.name == name)
                        .map(|it| it.index.clone()),
                    ASTTypedType::Type { name } => self
                        .typed_module
                        .types
                        .iter()
                        .find(|it| &it.name == name)
                        .map(|it| it.index.clone()),
                    ASTTypedType::Unit => None,
                };
                Ok(Some((item.ast_typed_type, index)))
            }
            CompletableItemResult::NotFound(_) => Ok(None),
        }
    }

    fn get_completable_item(&self, index: &ASTIndex) -> io::Result<CompletableItemResult> {
        let items = self
            .items
            .iter()
            .map(|it| {
                index
                    .between(&it.min, &it.max)
                    .map(|valid| (it.clone(), valid))
            })
            .collect::<io::Result<Vec<_>>>()?;

        if items.is_empty() {
            return Ok(CompletableItemResult::NotFound(
                "cannot find a completable item".to_owned(),
            ));
        }

        let mut valid_items = items
            .into_iter()
            .filter(|it| it.1)
            .map(|it| it.0)
            .collect::<Vec<_>>();

        if valid_items.is_empty() {
            Ok(CompletableItemResult::NotFound(
                "cannot find a valid item".to_owned(),
            ))
        } else if valid_items.len() == 1 {
            Ok(CompletableItemResult::Found(valid_items.remove(0)))
        } else {
            Ok(CompletableItemResult::NotFound(
                "found more than a valid item".to_owned(),
            ))
        }
    }

    fn process_statements(
        module: &ASTTypedModule,
        completable_items: &mut Vec<CompletableItem>,
        statements: &[ASTTypedStatement],
        val_context: &mut TypedValContext,
        statics: &mut Statics,
    ) {
        statements.iter().for_each(|it| {
            Self::process_statement(module, completable_items, it, val_context, statics);
        });
    }

    fn process_statement(
        module: &ASTTypedModule,
        completable_items: &mut Vec<CompletableItem>,
        statement: &ASTTypedStatement,
        val_context: &mut TypedValContext,
        statics: &mut Statics,
    ) {
        match statement {
            ASTTypedStatement::Expression(expr) => {
                Self::process_expression(
                    module,
                    completable_items,
                    expr,
                    val_context,
                    None,
                    statics,
                );
            }
            ASTTypedStatement::LetStatement(name, expr, is_const, index) => {
                // TODO put name in context
                let typed_type =
                    get_type_of_typed_expression(module, val_context, expr, None, statics);
                val_context.insert(name.clone(), TypedValKind::LetRef(0, typed_type.clone()));
                Self::process_expression(
                    module,
                    completable_items,
                    expr,
                    val_context,
                    Some(typed_type),
                    statics,
                );
            }
        }
    }

    fn process_expression(
        module: &ASTTypedModule,
        completable_items: &mut Vec<CompletableItem>,
        expr: &ASTTypedExpression,
        val_context: &TypedValContext,
        expected_type: Option<ASTTypedType>,
        statics: &mut Statics,
    ) {
        match expr {
            ASTTypedExpression::StringLiteral(value) => {
                // TODO we need an index
            }
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                let (parameters, ast_typed_type) = if let Some(v) =
                    val_context.get(&call.function_name)
                {
                    let ast_typed_type = match v {
                        TypedValKind::ParameterRef(_, par) => par.ast_type.clone(),
                        TypedValKind::LetRef(_, ast_typed_type) => ast_typed_type.clone(),
                    };

                    if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                        parameters,
                        return_type,
                    }) = ast_typed_type
                    {
                        (parameters, return_type.deref().clone())
                    } else {
                        panic!("type of reference '{}' expected to be a lambda but is {ast_typed_type}", call.function_name);
                    }
                } else {
                    let function = module
                        .functions_by_name
                        .get(&call.function_name)
                        .unwrap_or_else(|| panic!("cannot find function {}", call.function_name));
                    (
                        function
                            .parameters
                            .iter()
                            .map(|it| it.ast_type.clone())
                            .collect(),
                        function.return_type.clone(),
                    )
                };
                completable_items.push(CompletableItem::new(
                    call.index.mv(-(call.original_function_name.len() as i32)),
                    call.index.clone(),
                    ast_typed_type,
                ));
                call.parameters.iter().enumerate().for_each(|(i, it)| {
                    Self::process_expression(
                        module,
                        completable_items,
                        it,
                        val_context,
                        parameters.get(i).cloned(),
                        statics,
                    )
                });
            }
            ASTTypedExpression::ValueRef(name, index) => {
                val_context.get(name).iter().for_each(|it| match it {
                    TypedValKind::ParameterRef(_, def) => {
                        completable_items.push(CompletableItem::new(
                            index.mv(-(name.len() as i32)),
                            index.clone(),
                            def.ast_type.clone(),
                        ));
                    }
                    TypedValKind::LetRef(_, ast_typed_type) => {
                        completable_items.push(CompletableItem::new(
                            index.mv(-(name.len() as i32)),
                            index.clone(),
                            ast_typed_type.clone(),
                        ));
                    }
                })
            }
            ASTTypedExpression::Value(_, _) => {}
            ASTTypedExpression::Lambda(lambda_def) => {
                if let Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                    parameters,
                    return_type,
                })) = expected_type
                {
                    let mut lambda_context = TypedValContext::new(Some(val_context));
                    for (i, (name, index)) in lambda_def.parameter_names.iter().enumerate() {
                        lambda_context.insert(
                            name.clone(),
                            TypedValKind::ParameterRef(
                                i,
                                ASTTypedParameterDef {
                                    name: name.clone(),
                                    ast_type: parameters.get(i).unwrap().clone(),
                                    ast_index: index.clone(),
                                },
                            ),
                        );
                    }

                    Self::process_statements(
                        module,
                        completable_items,
                        &lambda_def.body,
                        &mut lambda_context,
                        statics,
                    );
                }
            }
        }
    }

    fn process_function(
        module: &ASTTypedModule,
        completable_items: &mut Vec<CompletableItem>,
        function_def: &ASTTypedFunctionDef,
        statics: &mut Statics,
    ) {
        let mut val_context = TypedValContext::new(None);

        function_def
            .parameters
            .iter()
            .enumerate()
            .for_each(|(i, it)| {
                val_context.insert(it.name.clone(), TypedValKind::ParameterRef(i, it.clone()));
            });

        match &function_def.body {
            ASTTypedFunctionBody::RASMBody(body) => {
                Self::process_statements(
                    module,
                    completable_items,
                    &body,
                    &mut val_context,
                    statics,
                );
            }
            ASTTypedFunctionBody::ASMBody(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Write;
    use std::path::Path;

    use env_logger::Builder;

    use rasm_core::parser::ast::ASTIndex;
    use rasm_core::project::project::RasmProject;

    use crate::completion_service::{CompletionResult, CompletionService};

    #[test]
    fn test_simple() {
        let service = get_completion_service("resources/test/simple.rasm");
        let file = Path::new("resources/test/simple.rasm");

        let result = service
            .get_completions(&ASTIndex::new(Some(file.to_path_buf()), 6, 15))
            .unwrap();

        if let CompletionResult::Found(ast_type, items) = result {
            items.iter().for_each(|it| {
                println!("{},{}", it.value, it.descr);
            })
        } else {
            panic!();
        }
    }

    #[test]
    fn test_types() {
        let service = get_completion_service("resources/test/types.rasm");
        let file = Path::new("resources/test/types.rasm");

        let result = service
            .get_completions(&ASTIndex::new(Some(file.to_path_buf()), 11, 16))
            .unwrap();

        if let CompletionResult::Found(ast_type, items) = result {
            items.iter().for_each(|it| {
                println!("{},{}", it.value, it.descr);
            })
        } else {
            panic!();
        }
    }

    fn get_completion_service(source: &str) -> CompletionService {
        init();
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let file_name = Path::new(source);

        let project = RasmProject::new(file_name.to_path_buf());

        CompletionService::new(&project).unwrap()
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
