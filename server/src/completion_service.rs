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

use rasm_core::codegen::backend::Backend;
use rasm_core::codegen::enhanced_module::EnhancedASTModule;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::typedef_provider::TypeDefProvider;
use rasm_core::codegen::val_context::TypedValContext;
use rasm_core::codegen::{get_typed_module, CodeGenAsm, TypedValKind};
use rasm_core::errors::{CompilationError, CompilationErrorKind};
use rasm_core::new_type_check2;
use rasm_core::parser::ast::{ASTFunctionDef, ASTIndex, ASTType, BuiltinTypeKind};
use rasm_core::type_check::functions_container::TypeFilter;
use rasm_core::type_check::typed_ast::{
    get_type_of_typed_expression, ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef,
    ASTTypedModule, ASTTypedParameterDef, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};

use crate::reference_finder::FileToken;

#[derive(PartialEq, Debug)]
pub struct CompletionItem {
    pub value: String,
    pub descr: String,
    pub sort: Option<String>,
    pub insert: Option<String>,
}

impl CompletionItem {
    pub fn for_function(function: &ASTFunctionDef) -> Option<Self> {
        if function.parameters.is_empty() {
            return None;
        }
        let parameter_type = &function.parameters.get(0).unwrap().ast_type;
        let coeff = new_type_check2::TypeCheck::generic_type_coeff(parameter_type);
        let sort_value = format!("{:0>20}{}", coeff, function.original_name);

        Some(CompletionItem {
            value: function.original_name.clone(),
            descr: Self::function_descr(function),
            sort: Some(sort_value),
            insert: Some(Self::function_insert(function)),
        })
    }

    fn function_descr(function: &ASTFunctionDef) -> String {
        let generic_types = if function.generic_types.is_empty() {
            "".into()
        } else {
            format!("<{}>", function.generic_types.join(","))
        };

        let rt = if function.return_type != ASTType::Unit {
            format!("{}", function.return_type)
        } else {
            "()".into()
        };

        let args = function
            .parameters
            .iter()
            .map(|it| format!("{}", it))
            .collect::<Vec<String>>()
            .join(",");
        format!("{}{generic_types}({args}) -> {rt}", function.original_name)
    }

    fn function_insert(function: &ASTFunctionDef) -> String {
        let args = function
            .parameters
            .iter()
            .skip(1)
            .map(|it| match &it.ast_type {
                ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters,
                    return_type: _,
                }) => {
                    let par_names = parameters
                        .iter()
                        .enumerate()
                        .map(|(pos, _ast_type)| format!("par{pos}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("\n    {{ {par_names} -> }}")
                }
                _ => it.name.clone(),
            })
            .collect::<Vec<String>>()
            .join(", ");
        format!("{}({args});", function.original_name)
    }
}

#[derive(PartialEq, Debug)]
pub enum CompletionResult {
    Found(Vec<CompletionItem>),
    NotFound(String),
}

pub enum CompletableItemResult {
    Found(CompletableItem),
    NotFound(String),
}

#[derive(Clone)]
pub struct CompletableItem {
    file_token: FileToken,
    ast_typed_type: ASTTypedType,
}

impl CompletableItem {
    fn new(start: ASTIndex, len: usize, ast_typed_type: ASTTypedType) -> Self {
        Self {
            file_token: FileToken::new(start, len),
            ast_typed_type,
        }
    }

    fn contains(&self, index: &ASTIndex) -> io::Result<bool> {
        self.file_token.contains(index)
    }
}

pub struct CompletionService {
    items: Vec<CompletableItem>,
    typed_module: ASTTypedModule,
    module: EnhancedASTModule,
}

impl CompletionService {
    pub fn new(
        module: EnhancedASTModule,
        statics: &mut Statics,
        backend: &dyn Backend,
    ) -> Result<Self, CompilationError> {
        let typed_module = get_typed_module(backend, module.clone(), false, true, false, statics)?;

        let mut completable_items = Vec::new();

        let mut val_context = TypedValContext::new(None);

        for it in typed_module.body.iter() {
            Self::process_statement(
                &typed_module,
                &mut completable_items,
                it,
                &mut val_context,
                statics,
            )?;
        }

        for it in typed_module.functions_by_name.values() {
            Self::process_function(&typed_module, &mut completable_items, it, statics)?;
        }

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
                    .functions()
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
                    .filter_map(|it| CompletionItem::for_function(it))
                    .collect::<Vec<_>>();

                if completion_items.is_empty() {
                    return Ok(CompletionResult::NotFound(format!(
                        "cannot find items for {}",
                        completable_item.ast_typed_type
                    )));
                }

                Ok(CompletionResult::Found(completion_items))
            }
            CompletableItemResult::NotFound(message) => Ok(CompletionResult::NotFound(message)),
        }
    }

    pub fn get_type(
        &self,
        index: &ASTIndex,
    ) -> io::Result<Option<(ASTTypedType, Option<ASTType>, Option<ASTIndex>)>> {
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

                let ast_type = self
                    .typed_module
                    .get_type_from_typed_type(&item.ast_typed_type);

                Ok(Some((item.ast_typed_type.clone(), ast_type, index)))
            }
            CompletableItemResult::NotFound(_) => Ok(None),
        }
    }

    fn get_completable_item(&self, index: &ASTIndex) -> io::Result<CompletableItemResult> {
        let items = self
            .items
            .iter()
            .map(|it| it.contains(index).map(|valid| (it.clone(), valid)))
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
    ) -> Result<(), CompilationError> {
        for it in statements.iter() {
            Self::process_statement(module, completable_items, it, val_context, statics)?;
        }
        Ok(())
    }

    fn process_statement(
        module: &ASTTypedModule,
        completable_items: &mut Vec<CompletableItem>,
        statement: &ASTTypedStatement,
        val_context: &mut TypedValContext,
        statics: &mut Statics,
    ) -> Result<(), CompilationError> {
        match statement {
            ASTTypedStatement::Expression(expr) => Self::process_expression(
                module,
                completable_items,
                expr,
                val_context,
                None,
                statics,
            ),
            ASTTypedStatement::LetStatement(name, expr, _is_const, _index) => {
                let typed_type =
                    get_type_of_typed_expression(module, val_context, expr, None, statics)?;
                val_context.insert(name.clone(), TypedValKind::LetRef(0, typed_type.clone()));

                Self::process_expression(
                    module,
                    completable_items,
                    expr,
                    val_context,
                    Some(typed_type),
                    statics,
                )
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
    ) -> Result<(), CompilationError> {
        match expr {
            ASTTypedExpression::StringLiteral(_value) => {
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
                        return Err(CompilationError { index: call.index.clone(),
                                                       error_kind: CompilationErrorKind::Generic(format!("type of reference '{}' expected to be a lambda but is {ast_typed_type}", call.function_name))});
                    }
                } else {
                    match module.functions_by_name.get(&call.function_name) {
                        None => {
                            return Err(CompilationError {
                                index: call.index.clone(),
                                error_kind: CompilationErrorKind::Generic(format!(
                                    "cannot find function {}",
                                    call.function_name
                                )),
                            });
                        }
                        Some(function) => (
                            function
                                .parameters
                                .iter()
                                .map(|it| it.ast_type.clone())
                                .collect(),
                            function.return_type.clone(),
                        ),
                    }
                };
                completable_items.push(CompletableItem::new(
                    call.index.mv_left(call.original_function_name.len()),
                    call.original_function_name.len(),
                    ast_typed_type,
                ));
                for (i, it) in call.parameters.iter().enumerate() {
                    Self::process_expression(
                        module,
                        completable_items,
                        it,
                        val_context,
                        parameters.get(i).cloned(),
                        statics,
                    )?
                }
            }
            ASTTypedExpression::ValueRef(name, index) => {
                val_context.get(name).iter().for_each(|it| match it {
                    TypedValKind::ParameterRef(_, def) => {
                        completable_items.push(CompletableItem::new(
                            index.mv_left(name.len()),
                            name.len(),
                            def.ast_type.clone(),
                        ));
                    }
                    TypedValKind::LetRef(_, ast_typed_type) => {
                        completable_items.push(CompletableItem::new(
                            index.mv_left(name.len()),
                            name.len(),
                            ast_typed_type.clone(),
                        ));
                    }
                })
            }
            ASTTypedExpression::Value(_, _) => {}
            ASTTypedExpression::Lambda(lambda_def) => {
                if let Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                    parameters,
                    return_type: _,
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
                    )?;
                }
            }
        }
        Ok(())
    }

    fn process_function(
        module: &ASTTypedModule,
        completable_items: &mut Vec<CompletableItem>,
        function_def: &ASTTypedFunctionDef,
        statics: &mut Statics,
    ) -> Result<(), CompilationError> {
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
                )?;
            }
            ASTTypedFunctionBody::ASMBody(_) => {}
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Write;
    use std::path::Path;

    use env_logger::Builder;

    use rasm_core::codegen::backend::BackendNasm386;
    use rasm_core::codegen::enhanced_module::EnhancedASTModule;
    use rasm_core::codegen::statics::Statics;
    use rasm_core::parser::ast::{
        ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModifiers, ASTNameSpace, ASTParameterDef,
        ASTType, BuiltinTypeKind,
    };
    use rasm_core::project::RasmProject;
    use rasm_core::type_check::resolved_generic_types::ResolvedGenericTypes;

    use crate::completion_service::{CompletionItem, CompletionResult, CompletionService};

    #[test]
    fn test_simple() {
        let service = get_completion_service("resources/test/simple.rasm");
        let file = Path::new("resources/test/simple.rasm");

        let result = service
            .get_completions(&ASTIndex::new(Some(file.to_path_buf()), 6, 15))
            .unwrap();

        if let CompletionResult::Found(items) = result {
            // TODO
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

        if let CompletionResult::Found(items) = result {
            // TODO
        } else {
            panic!();
        }
    }

    #[test]
    fn test_completion_item_for_function() {
        let function1 = ASTFunctionDef {
            original_name: "add".to_string(),
            name: "add_0".to_string(),
            parameters: vec![ASTParameterDef {
                name: "par".to_string(),
                ast_type: ASTType::Builtin(BuiltinTypeKind::String),
                ast_index: ASTIndex::none(),
            }],
            return_type: ASTType::Unit,
            body: ASTFunctionBody::RASMBody(vec![]),
            inline: false,
            generic_types: vec![],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
            modifiers: ASTModifiers::private(),
            namespace: ASTNameSpace::global(),
        };

        let mut function2 = function1.clone();
        function2.parameters = vec![ASTParameterDef {
            name: "par".to_string(),
            ast_type: ASTType::Generic("T".to_string()),
            ast_index: ASTIndex::none(),
        }];

        CompletionItem::for_function(&function1).unwrap();
        CompletionItem::for_function(&function2).unwrap();
    }

    fn get_completion_service(source: &str) -> CompletionService {
        init();
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let file_name = Path::new(source);

        let project = RasmProject::new(file_name.to_path_buf());

        let mut backend = BackendNasm386::new(false);

        let mut statics = Statics::new();

        let (modules, errors) = project.get_all_modules(&mut backend, &mut statics, false);

        let module = EnhancedASTModule::new(modules, &project, &backend, &mut statics);

        assert!(errors.is_empty());

        CompletionService::new(module, &mut statics, &backend).unwrap()
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
