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

use std::fmt::Display;
use std::io;
use std::ops::Deref;

use rasm_core::codegen::compile_target::CompileTarget;
use rasm_core::codegen::enh_ast::{
    EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind, EnhModuleId,
};
use rasm_core::codegen::enh_val_context::TypedValContext;
use rasm_core::codegen::enhanced_module::EnhancedASTModule;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::typedef_provider::TypeDefProvider;
use rasm_core::codegen::{get_typed_module, TypedValKind};
use rasm_core::errors::{CompilationError, CompilationErrorKind};
use rasm_core::new_type_check2;
use rasm_core::type_check::ast_modules_container::ASTModulesContainer;
use rasm_core::type_check::ast_type_checker::ASTTypeChecker;
use rasm_core::type_check::functions_container::EnhTypeFilter;
use rasm_core::type_check::typed_ast::{
    get_type_of_typed_expression, ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef,
    ASTTypedModule, ASTTypedParameterDef, ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};
use rasm_parser::catalog::modules_catalog::ModulesCatalog;
use rasm_parser::parser::ast::{ASTFunctionSignature, ASTType, BuiltinTypeKind};
use rasm_utils::OptionDisplay;

use crate::file_token::FileToken;

#[derive(PartialEq, Debug, Clone)]
pub struct CompletionItem {
    pub value: String,
    pub descr: String,
    pub sort: Option<String>,
    pub insert: Option<String>,
}

impl Display for CompletionItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "CompletionItem({}, {}, {}, ...)",
            self.value,
            self.descr,
            OptionDisplay(&self.sort),
        ))
    }
}

impl CompletionItem {
    pub fn for_function(function: &EnhASTFunctionDef) -> Option<Self> {
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

    pub fn for_function_signature(function: &ASTFunctionSignature) -> Option<Self> {
        if function.parameters_types.is_empty() {
            return None;
        }
        let parameter_type = &function.parameters_types.get(0).unwrap();
        let coeff = Self::generic_type_coeff(parameter_type);
        let sort_value = format!("{:0>20}{}", coeff, function.name);

        Some(CompletionItem {
            value: function.name.clone(),
            descr: Self::signature_descr(function),
            sort: Some(sort_value),
            insert: Some(Self::signature_insert(function)),
        })
    }

    ///
    /// return a coefficient that is higher for how the type is generic
    ///
    fn generic_type_coeff(ast_type: &ASTType) -> usize {
        Self::generic_type_coeff_internal(ast_type, usize::MAX / 100)
    }

    fn generic_type_coeff_internal(ast_type: &ASTType, coeff: usize) -> usize {
        if ast_type.is_generic() {
            match ast_type {
                ASTType::Builtin(_) => 0,
                ASTType::Generic(_, _) => coeff,
                ASTType::Custom {
                    name: _,
                    param_types,
                    position: _,
                } => param_types
                    .iter()
                    .map(|it| Self::generic_type_coeff_internal(it, coeff / 100))
                    .sum(),
                ASTType::Unit => 0,
            }
        } else {
            0
        }
    }

    fn signature_descr(function: &ASTFunctionSignature) -> String {
        let generic_types = if function.generics.is_empty() {
            "".into()
        } else {
            format!("<{}>", function.generics.join(","))
        };

        let rt = if function.return_type != ASTType::Unit {
            format!("{}", function.return_type)
        } else {
            "()".into()
        };

        let args = function
            .parameters_types
            .iter()
            .map(|it| format!("{}", it))
            .collect::<Vec<String>>()
            .join(",");
        format!("{}{generic_types}({args}) -> {rt}", function.name)
    }

    fn function_descr(function: &EnhASTFunctionDef) -> String {
        let generic_types = if function.generic_types.is_empty() {
            "".into()
        } else {
            format!("<{}>", function.generic_types.join(","))
        };

        let rt = if function.return_type != EnhASTType::Unit {
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

    fn function_insert(function: &EnhASTFunctionDef) -> String {
        let args = function
            .parameters
            .iter()
            .skip(1)
            .map(|it| match &it.ast_type {
                EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type: _,
                }) => {
                    let par_names = parameters
                        .iter()
                        .enumerate()
                        .map(|(pos, _ast_type)| format!("par{pos}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    if par_names.is_empty() {
                        "\n    {  }".to_string()
                    } else {
                        format!("\n    fn({par_names}) {{  }}")
                    }
                }
                _ => it.name.clone(),
            })
            .collect::<Vec<String>>()
            .join(", ");
        format!("{}({args});", function.original_name)
    }

    fn signature_insert(function: &ASTFunctionSignature) -> String {
        let args = function
            .parameters_types
            .iter()
            .skip(1)
            .map(|it| match &it {
                ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters,
                    return_type: _,
                }) => {
                    let par_names = parameters
                        .iter()
                        .enumerate()
                        .map(|(_pos, ast_type)| format!("{ast_type}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    if par_names.is_empty() {
                        "\n    {  }".to_string()
                    } else {
                        format!("\n    fn({par_names}) {{  }}")
                    }
                }
                _ => format!("{it}"),
            })
            .collect::<Vec<String>>()
            .join(", ");
        format!("{}({args});", function.name)
    }
}

#[derive(PartialEq, Debug)]
pub enum CompletionResult {
    Found(Vec<CompletionItem>),
    NotFound(String),
}

#[derive(Debug)]
pub enum CompletionTrigger {
    Invoked,
    Character(char),
    IncompleteCompletion,
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
    fn new(start: EnhASTIndex, len: usize, ast_typed_type: ASTTypedType) -> Self {
        Self {
            file_token: FileToken::new(start, len),
            ast_typed_type,
        }
    }

    fn contains(&self, index: &EnhASTIndex) -> io::Result<bool> {
        self.file_token.contains(index)
    }
}

pub struct CompletionService {
    items: Vec<CompletableItem>,
    typed_module: ASTTypedModule,
}

impl CompletionService {
    pub fn new(
        module: EnhancedASTModule,
        statics: &mut Statics,
        target: &CompileTarget,
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        modules_container: &ASTModulesContainer,
    ) -> Result<Self, CompilationError> {
        let typed_module = get_typed_module(
            module,
            false,
            false,
            statics,
            target,
            false,
            ASTTypeChecker::new(), // TODO I think this class is not used
            modules_catalog,
            modules_container,
        )?;

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
            typed_module,
        })
    }

    pub fn get_completions(
        &self,
        index: &EnhASTIndex,
        enhanced_module: &EnhancedASTModule,
    ) -> io::Result<CompletionResult> {
        match self.get_completable_item(index)? {
            CompletableItemResult::Found(completable_item) => {
                let completion_items = enhanced_module
                    .functions()
                    .iter()
                    .filter(|it| {
                        !it.parameters.is_empty()
                            && EnhTypeFilter::Exact(it.parameters.get(0).unwrap().ast_type.clone())
                                .almost_equal(
                                    &self
                                        .typed_module
                                        .get_type_from_typed_type(&completable_item.ast_typed_type)
                                        .expect(&format!(
                                            "cannot convert {}",
                                            completable_item.ast_typed_type
                                        )),
                                    enhanced_module,
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
        index: &EnhASTIndex,
    ) -> io::Result<Option<(ASTTypedType, Option<EnhASTType>, Option<EnhASTIndex>)>> {
        match self.get_completable_item(index)? {
            CompletableItemResult::Found(item) => {
                let index = match &item.ast_typed_type {
                    ASTTypedType::Builtin(_) => None,
                    ASTTypedType::Enum { namespace, name } => self
                        .typed_module
                        .enums
                        .iter()
                        .find(|it| {
                            &it.name == name && (it.modifiers.public || &it.namespace == namespace)
                        })
                        .map(|it| it.index.clone()),
                    ASTTypedType::Struct { namespace, name } => self
                        .typed_module
                        .structs
                        .iter()
                        .find(|it| {
                            &it.name == name && (it.modifiers.public || &it.namespace == namespace)
                        })
                        .map(|it| it.index.clone()),
                    ASTTypedType::Type {
                        namespace,
                        name,
                        native_type: _,
                        is_ref: _,
                    } => self
                        .typed_module
                        .types
                        .iter()
                        .find(|it| {
                            &it.name == name && (it.modifiers.public || &it.namespace == namespace)
                        })
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

    fn get_completable_item(&self, index: &EnhASTIndex) -> io::Result<CompletableItemResult> {
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
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                let (parameters, ast_typed_type) = if let Some(v) =
                    val_context.get(&call.function_name)
                {
                    let ast_typed_type = v.typed_type().clone();

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
                    call.index.clone(),
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
                            index.clone(),
                            name.len(),
                            def.ast_type.clone(),
                        ));
                    }
                    TypedValKind::LetRef(_, ast_typed_type) => {
                        completable_items.push(CompletableItem::new(
                            index.clone(),
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
            ASTTypedFunctionBody::NativeBody(_) => {}
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

    use rasm_core::codegen::compile_target::CompileTarget;
    use rasm_core::codegen::enh_ast::{
        EnhASTFunctionBody, EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTParameterDef,
        EnhASTType, EnhBuiltinTypeKind,
    };
    use rasm_core::codegen::enhanced_module::EnhancedASTModule;
    use rasm_core::codegen::statics::Statics;
    use rasm_core::codegen::AsmOptions;
    use rasm_core::commandline::CommandLineOptions;
    use rasm_core::project::{RasmProject, RasmProjectRunType};
    use rasm_core::type_check::resolved_generic_types::ResolvedGenericTypes;
    use rasm_parser::parser::ast::ASTModifiers;

    use crate::completion_service::{CompletionItem, CompletionResult, CompletionService};

    #[test]
    fn test_simple() {
        let (service, enhanced_module) = get_completion_service("resources/test/simple.rasm");
        let file = Path::new("resources/test/simple.rasm");

        let result = service
            .get_completions(
                &EnhASTIndex::new(Some(file.to_path_buf()), 6, 15),
                &enhanced_module,
            )
            .unwrap();

        if let CompletionResult::Found(_items) = result {
            // TODO
        } else {
            panic!();
        }
    }

    #[test]
    fn test_types() {
        let (service, enhanced_module) = get_completion_service("resources/test/types.rasm");
        let file = Path::new("resources/test/types.rasm");

        let result = service
            .get_completions(
                &EnhASTIndex::new(Some(file.to_path_buf()), 11, 16),
                &enhanced_module,
            )
            .unwrap();

        if let CompletionResult::Found(_items) = result {
            // TODO
        } else {
            panic!();
        }
    }

    #[test]
    fn test_completion_item_for_function() {
        let function1 = EnhASTFunctionDef {
            original_name: "add".to_string(),
            name: "add_0".to_string(),
            parameters: vec![EnhASTParameterDef {
                name: "par".to_string(),
                ast_type: EnhASTType::Builtin(EnhBuiltinTypeKind::String),
                ast_index: EnhASTIndex::none(),
            }],
            return_type: EnhASTType::Unit,
            body: EnhASTFunctionBody::RASMBody(vec![]),
            inline: false,
            generic_types: vec![],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: EnhASTIndex::none(),
            modifiers: ASTModifiers::private(),
            namespace: test_namespace(), // HENRY
            rank: 0,
        };

        let mut function2 = function1.clone();
        function2.parameters = vec![EnhASTParameterDef {
            name: "par".to_string(),
            ast_type: EnhASTType::Generic(EnhASTIndex::none(), "T".to_string()),
            ast_index: EnhASTIndex::none(),
        }];

        CompletionItem::for_function(&function1).unwrap();
        CompletionItem::for_function(&function2).unwrap();
    }

    fn get_completion_service(source: &str) -> (CompletionService, EnhancedASTModule) {
        init();
        env::set_var("RASM_STDLIB", "../stdlib");
        let file_name = Path::new(source);

        let project = RasmProject::new(file_name.to_path_buf());

        let mut statics = Statics::new();

        let target = CompileTarget::Nasmi386(AsmOptions::default());
        let run_type = RasmProjectRunType::Main;

        let command_line_options = CommandLineOptions::default();

        let (container, catalog, _) = project.container_and_catalog(
            &mut statics,
            &run_type,
            &target,
            false,
            &command_line_options,
        );

        let mut statics = Statics::new();
        let (modules, errors) = project.get_all_modules(
            &mut statics,
            &run_type,
            &target,
            false,
            &command_line_options,
        );

        assert!(errors.is_empty());

        let (module, errors) =
            EnhancedASTModule::from_ast(modules, &project, &mut statics, &target, false);

        assert!(errors.is_empty());

        (
            CompletionService::new(module.clone(), &mut statics, &target, &catalog, &container)
                .unwrap(),
            module,
        )
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

    pub fn test_namespace() -> EnhASTNameSpace {
        EnhASTNameSpace::new("test".to_string(), "test".to_string())
    }
}
