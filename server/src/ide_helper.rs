use std::collections::HashMap;
use std::path::PathBuf;
use std::{env, io};

use rasm_core::codegen::c::options::COptions;
use rasm_core::codegen::compile_target::CompileTarget;
use rasm_core::codegen::enh_ast::{EnhASTIndex, EnhASTType, EnhBuiltinTypeKind, EnhModuleInfo};
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::val_context::{ASTIndex, ValContext};
use rasm_core::commandline::CommandLineOptions;
use rasm_core::parser::ast::{ASTModule, ASTPosition, ASTType};
use rasm_core::project::RasmProject;
use rasm_core::type_check::ast_modules_container::{
    ASTModulesContainer, ASTTypeFilter, ModuleId, ModuleInfo,
};
use rasm_core::type_check::ast_type_checker::{ASTTypeCheckInfo, ASTTypeChecker};

use crate::completion_service::{CompletionItem, CompletionResult, CompletionTrigger};
use crate::selectable_item::{SelectableItem, SelectableItemTarget};
use crate::{CompletionType, RasmTextEdit};

pub struct IDEHelperBuilder {
    entries: HashMap<ModuleInfo, (ASTModule, EnhModuleInfo, bool)>,
}

impl IDEHelperBuilder {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn add(mut self, module: ASTModule, info: EnhModuleInfo, add_builtin: bool) -> Self {
        self.entries
            .insert(info.module_info(), (module, info, add_builtin))
            .map(|it| panic!("Already added {it:?}"));

        self
    }

    pub fn build(self) -> IDEHelper {
        let mut modules_container = ASTModulesContainer::new();

        for (module, info, add_builtin) in self.entries.values() {
            modules_container.add(module, info.module_id(), info.module_source(), *add_builtin);
        }

        let mut static_val_context = ValContext::new(None);

        let mut function_type_checker = ASTTypeChecker::new(&modules_container);
        let mut selectable_items = Vec::new();
        let mut module_info_to_enh_info = HashMap::new();

        for (module, info, _) in self.entries.values() {
            module_info_to_enh_info.insert(info.module_info(), info.clone());
            let mut val_context = ValContext::new(None);

            function_type_checker.add_body(
                &mut val_context,
                &mut static_val_context,
                &module.body,
                None,
                &info.module_id(),
                &info.module_source(),
            );

            for function in module.functions.iter() {
                function_type_checker.add_function(
                    &function,
                    &static_val_context,
                    &info.module_id(),
                    &info.module_source(),
                );

                let start = enh_index_from_ast_position(&info, &function.position);

                selectable_items.push(SelectableItem::new(
                    start,
                    function.name.len(),
                    info.namespace.clone(),
                    None,
                ));

                for par in function.parameters.iter() {
                    let par_type = &par.ast_type;

                    self.add_par_selectable(
                        par_type,
                        info,
                        &modules_container,
                        &mut selectable_items,
                    );
                }
            }
        }

        for (index, entry) in function_type_checker.result.map {
            let info = &self.entries.get(&index.info()).unwrap().1;

            let start = enh_index_from_ast_index(&info, &index);

            let enh_type = entry.filter().clone().and_then(|it| {
                if let ASTTypeFilter::Exact(exact, id) = it {
                    let type_info = &self.entries.get(&id).unwrap().1;
                    Some(EnhASTType::from_ast(
                        type_info.path.clone(),
                        type_info.namespace.clone(),
                        exact,
                    ))
                } else {
                    None
                }
            });

            let selectable_item = match entry.info() {
                ASTTypeCheckInfo::Call(name, function_signatures) => {
                    let target = if function_signatures.len() == 1 {
                        let (function_signature, function_index) =
                            function_signatures.first().unwrap();

                        let function_info = &self.entries.get(&function_index.info()).unwrap().1;

                        Some(SelectableItemTarget::Function(
                            enh_index_from_ast_index(&function_info, &function_index),
                            EnhASTType::from_ast(
                                function_info.path.clone(),
                                function_info.namespace.clone(),
                                function_signature.return_type.clone(),
                            ),
                            format!("{function_signature}"),
                        ))
                    } else {
                        None
                    };

                    Some(SelectableItem::new(
                        start,
                        name.len(),
                        info.namespace.clone(),
                        target,
                    ))
                }
                ASTTypeCheckInfo::LambdaCall(function_signature, lambda_index) => {
                    let function_info = &self.entries.get(&lambda_index.info()).unwrap().1;

                    Some(SelectableItem::new(
                        start,
                        function_signature.name.len(),
                        info.namespace.clone(),
                        Some(SelectableItemTarget::Function(
                            enh_index_from_ast_index(&function_info, &lambda_index),
                            EnhASTType::from_ast(
                                function_info.path.clone(),
                                function_info.namespace.clone(),
                                function_signature.return_type.clone(),
                            ),
                            format!("{function_signature}"),
                        )),
                    ))
                }
                ASTTypeCheckInfo::Ref(name, ref_index) => {
                    let ref_info = &self.entries.get(&ref_index.info()).unwrap().1;

                    Some(SelectableItem::new(
                        start,
                        name.len(),
                        info.namespace.clone(),
                        Some(SelectableItemTarget::Ref(
                            enh_index_from_ast_index(&ref_info, ref_index),
                            enh_type.clone(),
                        )),
                    ))
                }
                ASTTypeCheckInfo::Let(name, _is_const) => Some(SelectableItem::new(
                    start,
                    name.len(),
                    info.namespace.clone(),
                    None,
                )),
                ASTTypeCheckInfo::Value(len) => {
                    if let Some(ref t) = enh_type {
                        if !matches!(
                            t,
                            EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                                parameters: _,
                                return_type: _
                            })
                        ) {
                            Some(SelectableItem::new(
                                start,
                                *len,
                                info.namespace.clone(),
                                Some(SelectableItemTarget::Type(None, t.clone())),
                            ))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                ASTTypeCheckInfo::Param(name) => Some(SelectableItem::new(
                    start,
                    name.len(),
                    info.namespace.clone(),
                    None,
                )),
                ASTTypeCheckInfo::Lambda => None,
            };

            if let Some(si) = selectable_item {
                selectable_items.push(si);
            }
        }

        IDEHelper::new(modules_container, selectable_items, module_info_to_enh_info)
    }

    fn add_par_selectable(
        &self,
        par_type: &ASTType,
        info: &EnhModuleInfo,
        modules_container: &ASTModulesContainer,
        selectable_items: &mut Vec<SelectableItem>,
    ) {
        if let ASTType::Custom {
            name,
            param_types,
            position,
        } = par_type
        {
            for parameter_type in param_types.iter() {
                self.add_par_selectable(parameter_type, info, modules_container, selectable_items);
            }

            let ct_start = enh_index_from_ast_position(&info, position);

            if let Some(ct_index) =
                IDEHelper::get_custom_type_index(modules_container, &info.module_id(), name)
            {
                let (_, ct_info, _) = self.entries.get(&ct_index.info()).unwrap();

                let def_info = EnhModuleInfo::new(ct_info.path.clone(), ct_info.namespace.clone());
                let def_index = enh_index_from_ast_position(&def_info, ct_index.position());

                selectable_items.push(SelectableItem::new(
                    ct_start,
                    name.len(),
                    info.namespace.clone(),
                    Some(SelectableItemTarget::Type(
                        Some(def_index),
                        EnhASTType::from_ast(
                            ct_info.path.clone(),
                            ct_info.namespace.clone(),
                            par_type.clone(),
                        ),
                    )),
                ));
            }
        }
    }
}

fn enh_index_from_ast_index(info: &EnhModuleInfo, index: &ASTIndex) -> EnhASTIndex {
    enh_index_from_ast_position(info, index.position())
}

fn enh_index_from_ast_position(info: &EnhModuleInfo, position: &ASTPosition) -> EnhASTIndex {
    EnhASTIndex::from_position(info.path.clone(), position)
}

pub fn get_ide_helper_from_project(project: &RasmProject) -> IDEHelper {
    let mut statics = Statics::new();

    let mut builder = IDEHelperBuilder::new();

    for (module, info) in project
        .get_all_modules(
            &mut statics,
            false,
            &CompileTarget::C(COptions::default()),
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        )
        .0
    {
        builder = builder.add(module, info, false);
    }

    builder.build()
}

pub struct IDEHelper {
    modules_container: ASTModulesContainer,
    selectable_items: Vec<SelectableItem>,
    module_info_to_enh_info: HashMap<ModuleInfo, EnhModuleInfo>,
}

impl IDEHelper {
    fn new(
        modules_container: ASTModulesContainer,
        selectable_items: Vec<SelectableItem>,
        module_info_to_enh_info: HashMap<ModuleInfo, EnhModuleInfo>,
    ) -> Self {
        Self {
            modules_container,
            selectable_items,
            module_info_to_enh_info,
        }
    }

    pub fn get_module_info_from_path(&self, path: &PathBuf) -> Option<ModuleInfo> {
        for (info, v) in self.module_info_to_enh_info.iter() {
            if let Some(ref p) = v.path {
                if p == path {
                    return Some(info.clone());
                }
            }
        }
        None
    }

    pub fn find(&self, index: &EnhASTIndex) -> Result<Vec<SelectableItem>, io::Error> {
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
        module_content: String,
        index: &ASTPosition,
        trigger: &CompletionTrigger,
        module_info: &ModuleInfo,
    ) -> Result<CompletionResult, io::Error> {
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
                            completion_type =
                                self.dot_completion(&lines, &index, Some(prefix), module_info);
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
                        index = i.clone();
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
                if let Some(completion_type) = self.dot_completion(&lines, index, None, module_info)
                {
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
                                    &ast_type.to_ast(),
                                    module_info,
                                    &self.modules_container,
                                    &prefix,
                                );
                            }
                        }
                    }
                }
            }
            CompletionType::Identifier(prefix) => {
                return Self::completion_for_identifier(
                    &prefix,
                    &self.modules_container,
                    module_info,
                );
            }
        }

        Ok(CompletionResult::NotFound(
            "Cannot find completion".to_owned(),
        ))
    }

    pub fn print(&self) {
        for item in self.selectable_items.iter() {
            println!("{item}");
        }
    }

    fn dot_completion(
        &self,
        lines: &Vec<&str>,
        index: &ASTPosition,
        prefix: Option<String>,
        module_info: &ModuleInfo,
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
            let enh_info = self.module_info_to_enh_info.get(module_info).unwrap();
            Some(CompletionType::SelectableItem(
                EnhASTIndex::from_position(enh_info.path.clone(), &index),
                prefix,
            ))
        } else {
            None
        }
    }

    fn completion_for_type(
        ast_type: &ASTType,
        module_info: &ModuleInfo,
        modules_container: &ASTModulesContainer,
        prefix: &Option<String>,
    ) -> Result<CompletionResult, io::Error> {
        let filter = ASTTypeFilter::Exact(ast_type.clone(), module_info.clone());
        let mut items = Vec::new();
        for function in modules_container.signatures().iter() {
            if !function.signature.modifiers.public && &function.id != module_info.id() {
                continue;
            }
            if let Some(p) = prefix {
                if !function.signature.name.starts_with(p) {
                    continue;
                }
            }
            if !function.signature.parameters_types.is_empty() {
                let parameter_type = function.signature.parameters_types.get(0).unwrap();
                if filter.is_compatible(parameter_type, module_info.id(), modules_container) {
                    if let Some(item) = CompletionItem::for_function_signature(&function.signature)
                    {
                        items.push(item);
                    }
                }
            }
        }
        return Ok(CompletionResult::Found(items));
    }

    fn completion_for_identifier(
        prefix: &str,
        modules_container: &ASTModulesContainer,
        module_info: &ModuleInfo,
    ) -> Result<CompletionResult, io::Error> {
        let mut items = Vec::new();
        for function in modules_container.signatures() {
            if !function.signature.modifiers.public && &function.id != module_info.id() {
                continue;
            }
            if function.signature.name.starts_with(prefix) {
                if let Some(item) = CompletionItem::for_function_signature(&function.signature) {
                    items.push(item);
                }
            }
        }
        return Ok(CompletionResult::Found(items));
    }

    fn find_last_char_excluding(
        lines: &Vec<&str>,
        index: &ASTPosition,
        find: &dyn Fn(char) -> bool,
    ) -> Option<ASTPosition> {
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

    fn char_at_index(lines: &Vec<&str>, index: &ASTPosition) -> Option<char> {
        lines.get(index.row - 1).and_then(|line| {
            line.get((index.column - 1)..index.column)
                .and_then(|chars| chars.chars().next())
        })
    }

    fn move_left(lines: &Vec<&str>, index: &ASTPosition) -> Option<ASTPosition> {
        let mut row = index.row as i32;
        let mut column = index.column as i32 - 1;

        if column <= 0 {
            row -= 1;
            if row <= 0 {
                return None;
            }
            column = (lines.get((row - 1) as usize).unwrap().len()) as i32;
        }
        Some(ASTPosition::new(row as usize, column as usize))
    }

    fn find_open_bracket(lines: &Vec<&str>, index: &ASTPosition) -> Option<ASTPosition> {
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

    pub fn references(&self, index: &EnhASTIndex) -> Result<Vec<SelectableItem>, io::Error> {
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

    pub fn rename(
        &self,
        index: &EnhASTIndex,
        new_name: String,
    ) -> Result<Vec<RasmTextEdit>, String> {
        let mut result = Vec::new();

        if let Some(ref path) = index.file_name {
            if let Ok(items) = self.find(&index) {
                if let Some(item) = items.first() {
                    let root_index_o = if let Some(ref target) = item.target {
                        if let Some(index) = target.index() {
                            result.push(RasmTextEdit::new(
                                index,
                                item.file_token.len,
                                new_name.to_owned(),
                            ));
                        }
                        target.index()
                    } else {
                        result.push(RasmTextEdit::new(
                            item.file_token.start.clone(),
                            item.file_token.len,
                            new_name.to_owned(),
                        ));
                        Some(item.file_token.start.clone())
                    };

                    if let Some(root_index) = root_index_o {
                        let references = self.references(&root_index).unwrap();

                        for reference in references.iter() {
                            result.push(RasmTextEdit {
                                from: reference.file_token.start.clone(),
                                len: item.file_token.len,
                                text: new_name.clone(),
                            });
                        }
                    }
                }
            }

            if result
                .iter()
                .filter_map(|it| it.from.clone().file_name)
                .all(|it| &it == path)
            {
                Ok(result)
            } else {
                // TODO we want to be able to rename symbols of the current lib
                Err("Rename of symbols outside current module is not yet supported.".to_owned())
            }
        } else {
            Err("Rename of default symbols is not supported.".to_owned())
        }
    }

    fn get_custom_type_index(
        modules_container: &ASTModulesContainer,
        module_id: &ModuleId,
        name: &str,
    ) -> Option<ASTIndex> {
        if let Some((id, def)) = modules_container.get_enum_def(module_id, name) {
            Some(ASTIndex::new(
                id.id().clone(),
                id.source().clone(),
                def.position.clone(),
            ))
        } else if let Some((id, def)) = modules_container.get_struct_def(module_id, name) {
            Some(ASTIndex::new(
                id.id().clone(),
                id.source().clone(),
                def.position.clone(),
            ))
        } else if let Some((id, def)) = modules_container.get_type_def(module_id, name) {
            Some(ASTIndex::new(
                id.id().clone(),
                id.source().clone(),
                def.position.clone(),
            ))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Write;
    use std::path::{Path, PathBuf};

    use env_logger::Builder;

    use rasm_core::codegen::c::options::COptions;
    use rasm_core::codegen::compile_target::CompileTarget;
    use rasm_core::codegen::enh_ast::{EnhASTIndex, EnhASTType};
    use rasm_core::codegen::statics::Statics;
    use rasm_core::commandline::CommandLineOptions;
    use rasm_core::parser::ast::ASTPosition;
    use rasm_core::project::RasmProject;
    use rasm_utils::{OptionDisplay, SliceDisplay};

    use crate::completion_service::{CompletionResult, CompletionTrigger};
    use crate::ide_helper::IDEHelperBuilder;
    use crate::selectable_item::{SelectableItem, SelectableItemTarget};

    use super::IDEHelper;

    #[test]
    fn simple() {
        let (project, helper) = get_helper("resources/test/simple.rasm");

        let file_name = Path::new("resources/test/simple.rasm");

        let stdlib_path = project
            .from_relative_to_root(Path::new("../../../stdlib"))
            .canonicalize()
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_target_index(
                helper
                    .find(&EnhASTIndex::new(Some(file_name.to_path_buf()), 3, 15,))
                    .unwrap()
            ),
            vec![get_index(&project, "simple.rasm", 1, 5)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(
                helper
                    .find(&EnhASTIndex::new(Some(file_name.to_path_buf()), 6, 13,))
                    .unwrap()
            ),
            vec![get_index(&project, "simple.rasm", 5, 16)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(
                helper
                    .find(&EnhASTIndex::new(Some(file_name.to_path_buf()), 3, 2,))
                    .unwrap()
            ),
            vec![get_index(&project, "simple.rasm", 5, 4)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(
                helper
                    .find(&EnhASTIndex::new(Some(file_name.to_path_buf()), 6, 9,))
                    .unwrap()
            ),
            vec![EnhASTIndex::new(
                Some(stdlib_path.join("src/main/rasm/print.rasm")),
                12,
                8
            )]
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(
                helper
                    .find(&EnhASTIndex::new(Some(file_name.to_path_buf()), 10, 15,))
                    .unwrap()
            ),
            vec![EnhASTIndex::new(
                Some(stdlib_path.join("src/main/rasm/option.rasm")),
                2,
                3
            )]
        );
    }

    #[test]
    fn types() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        let file_name = Path::new("resources/test/types.rasm");
        let source_file = Path::new(&file_name);
        let stdlib_path = stdlib_path();

        assert_eq!(
            vec_selectable_item_to_vec_target_index(
                helper
                    .find(&EnhASTIndex::new(Some(source_file.to_path_buf()), 15, 23))
                    .unwrap()
            ),
            vec![EnhASTIndex::new(
                Some(stdlib_path.join("src/main/rasm/option.rasm")),
                1,
                10
            )],
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(
                helper
                    .find(&EnhASTIndex::new(Some(source_file.to_path_buf()), 19, 23,))
                    .unwrap()
            ),
            vec![get_index(&project, "types.rasm", 1, 8)],
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(
                helper
                    .find(&EnhASTIndex::new(Some(source_file.to_path_buf()), 23, 23,))
                    .unwrap()
            ),
            vec![EnhASTIndex::new(
                Some(stdlib_path.join("src/main/c/vec.rasm")),
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
        let (project, helper) = get_helper("resources/test/types.rasm");

        let file_name = Path::new("resources/test/types.rasm");

        let source_file = Path::new(&file_name);

        assert_eq!(
            vec_selectable_item_to_vec_target_index(
                helper
                    .find(&EnhASTIndex::new(Some(source_file.to_path_buf()), 6, 19,))
                    .unwrap()
            ),
            vec![get_index(&project, "types.rasm", 1, 8)],
        );
    }

    #[test]
    fn types_1() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        let file_name = PathBuf::from("resources/test/types.rasm");
        let found = helper
            .find(&EnhASTIndex::new(Some(file_name.clone()), 27, 31))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_target_index(found),
            vec!(get_index(&project, "types.rasm", 1, 8,)),
        );
    }

    #[test]
    fn types_2() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        let file_name = PathBuf::from("resources/test/types.rasm");
        let found = helper
            .find(&EnhASTIndex::new(Some(file_name.clone()), 9, 1))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_target_index(found),
            vec!(get_index(&project, "types.rasm", 15, 4,)),
        );
    }

    #[test]
    fn types_3() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        let file_name = PathBuf::from("resources/test/types.rasm");
        let found = helper
            .find(&EnhASTIndex::new(Some(file_name.clone()), 9, 13))
            .unwrap();

        assert_eq!(
            vec_selectable_item_to_vec_target_index(found),
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
    fn types_completion_dot_char() {
        let values = get_completion_values(
            None,
            "resources/test/types.rasm",
            44,
            9,
            CompletionTrigger::Character('.'),
        )
        .unwrap();

        assert_eq!(1, values.into_iter().filter(|it| it == "isAlpha").count());
    }

    #[test]
    fn types_lambda_param_type() {
        let (_project, helper) = get_helper("resources/test/types.rasm");

        let file_name = Some(PathBuf::from("resources/test/types.rasm"));

        match helper.find(&EnhASTIndex::new(file_name.clone(), 32, 28)) {
            Ok(mut selectable_items) => {
                if selectable_items.len() == 1 {
                    let selectable_item = selectable_items.remove(0);
                    if let Some(ref target) = selectable_item.target {
                        if let Some(EnhASTType::Custom {
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
        let stdlib_path = stdlib_path();

        let result_rasm = stdlib_path.join("src/main/rasm/result.rasm");

        let (_project, helper) = get_helper("resources/test/types.rasm");

        match helper.find(&EnhASTIndex::new(Some(result_rasm.clone()), 11, 9)) {
            Ok(mut selectable_items) => {
                if selectable_items.len() == 1 {
                    let selectable_item = selectable_items.remove(0);
                    let expected_index = EnhASTIndex::new(Some(result_rasm), 14, 8);
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
        let (_project, helper) = get_helper("resources/test/enums.rasm");

        let file_name = Path::new("resources/test/enums.rasm");

        let mut items = helper
            .find(&EnhASTIndex::new(Some(file_name.to_path_buf()), 17, 13))
            .unwrap();

        assert_eq!(1, items.len());

        let item = items.remove(0);

        if let Some(SelectableItemTarget::Function(index, _, descr)) = item.target {
            assert_eq!(
                EnhASTIndex::new(Some(file_name.canonicalize().unwrap()), 0, 0),
                index
            );
            assert!(descr.starts_with("match"));
        } else {
            panic!("Found {:?}", item.target);
        }
    }

    #[test]
    fn enums_1() {
        let (_project, helper) = get_helper("resources/test/enums.rasm");

        let file_name = Path::new("resources/test/enums.rasm");

        let mut items = helper
            .find(&EnhASTIndex::new(Some(file_name.to_path_buf()), 17, 40))
            .unwrap();

        assert_eq!(1, items.len());

        let item = items.remove(0);

        if let Some(SelectableItemTarget::Ref(index, _)) = item.target {
            assert_eq!(
                EnhASTIndex::new(
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
    fn references_breakout() {
        let _ = env_logger::builder()
            .is_test(true)
            .filter_level(log::LevelFilter::Info)
            .try_init();

        let (_project, _helper) = get_helper("../rasm/resources/examples/breakout");
    }

    #[test]
    fn references_types() {
        test_references("resources/test/types.rasm", 6, 7, vec![(10, 13), (12, 9)]);
    }

    #[test]
    fn reference1() {
        test_references("resources/test/references.rasm", 2, 14, vec![(3, 13)]);
    }

    #[test]
    fn rename() {
        test_rename(
            "resources/test/references.rasm",
            "s",
            2,
            14,
            Ok(vec![(2, 13, 2), (3, 13, 2)]),
        );
    }

    #[test]
    fn rename_fn() {
        test_rename(
            "resources/test/simple.rasm",
            "aName",
            5,
            7,
            Ok(vec![(3, 1, 11), (5, 4, 11)]),
        );
    }

    #[test]
    fn rename_fn1() {
        test_rename(
            "resources/test/simple.rasm",
            "aName",
            3,
            6,
            Ok(vec![(3, 1, 11), (5, 4, 11)]),
        );
    }

    #[test]
    fn rename_fn_of_ext_lib() {
        test_rename(
            "resources/test/types.rasm",
            "aName",
            7,
            13,
            Err("Rename of symbols outside current module is not yet supported.".to_owned()),
        );
    }

    fn test_references(file: &str, row: usize, column: usize, expected: Vec<(usize, usize)>) {
        let (_project, helper) = get_helper(file);

        /*
        for item in finder.selectable_items.iter() {
            match &item.target {
                Some(target) => match target {
                    SelectableItemTarget::Ref(index, enh_asttype) => {}
                    SelectableItemTarget::Function(index, enh_asttype, _) => {
                        println!("function {}", OptionDisplay(&target.index()))
                    }
                    SelectableItemTarget::Type(index, enh_asttype) => {}
                },
                None => {}
            }
        }
        */

        let file_name = Path::new(file).canonicalize().unwrap();

        let items = helper
            .references(&EnhASTIndex::new(
                Some(file_name.to_path_buf()),
                row,
                column,
            ))
            .unwrap();

        let mut found = items
            .into_iter()
            .map(|it| (it.file_token.start.row, it.file_token.start.column))
            .collect::<Vec<_>>();

        found.sort_by(|a, b| a.0.cmp(&b.0));

        assert_eq!(expected, found);
    }

    fn test_rename(
        file: &str,
        new_name: &str,
        row: usize,
        column: usize,
        expected: Result<Vec<(usize, usize, usize)>, String>,
    ) {
        /*
        let (project, eh_module, module) = get_reference_finder(file, None);
        let finder = get_reference_finder2_for_project(&project);
        */

        let (_project, helper) = get_helper(file);

        /*
        for item in finder.selectable_items.iter() {
            match &item.target {
                Some(target) => match target {
                    SelectableItemTarget::Ref(index, enh_asttype) => {}
                    SelectableItemTarget::Function(index, enh_asttype, _) => {
                        println!("function {}", OptionDisplay(&target.index()))
                    }
                    SelectableItemTarget::Type(index, enh_asttype) => {}
                },
                None => {}
            }
        }
        */

        let file_name = Path::new(file).canonicalize().unwrap();

        let edits = helper.rename(
            &EnhASTIndex::new(Some(file_name.to_path_buf()), row, column),
            new_name.to_owned(),
        );

        let found = edits.map(|it| {
            let mut f = it
                .into_iter()
                .map(|it| (it.from.row, it.from.column, it.len))
                .collect::<Vec<_>>();
            f.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
            f
        });

        assert_eq!(expected, found);
    }

    fn stdlib_path() -> PathBuf {
        PathBuf::from("../stdlib").canonicalize().unwrap()
    }

    fn get_index(project: &RasmProject, file_n: &str, row: usize, column: usize) -> EnhASTIndex {
        EnhASTIndex::new(
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

    fn vec_selectable_item_to_vec_target_index(vec: Vec<SelectableItem>) -> Vec<EnhASTIndex> {
        vec.iter()
            .flat_map(|it| it.target.clone().and_then(|item| item.index()))
            .collect::<Vec<_>>()
    }

    fn get_helper(project_path: &str) -> (RasmProject, IDEHelper) {
        env::set_var("RASM_STDLIB", "../../../stdlib");

        let file_name = Path::new(project_path);
        let project = RasmProject::new(file_name.to_path_buf());

        let mut statics = Statics::new();

        let mut builder = IDEHelperBuilder::new();

        for (module, info) in project
            .get_all_modules(
                &mut statics,
                false,
                &CompileTarget::C(COptions::default()),
                false,
                &env::temp_dir().join("tmp"),
                &CommandLineOptions::default(),
            )
            .0
        {
            builder = builder.add(module, info, false);
        }

        (project, builder.build())
    }

    fn get_helper_for_project(project: &RasmProject) -> IDEHelper {
        env::set_var("RASM_STDLIB", "../../../stdlib");

        let mut statics = Statics::new();

        let mut builder = IDEHelperBuilder::new();

        for (module, info) in project
            .get_all_modules(
                &mut statics,
                false,
                &CompileTarget::C(COptions::default()),
                false,
                &env::temp_dir().join("tmp"),
                &CommandLineOptions::default(),
            )
            .0
        {
            builder = builder.add(module, info, false);
        }

        builder.build()
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
        let helper = get_helper_for_project(&project);

        let path = PathBuf::from(file_name).canonicalize().unwrap();
        let file_name = Some(path.clone());
        let index = EnhASTIndex::new(file_name.clone(), row, col);

        match helper.get_completions(
            project.content_from_file(&path).unwrap(),
            &ASTPosition::new(index.row, index.column),
            &trigger,
            &helper.get_module_info_from_path(&path).unwrap(),
        ) {
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
    ) -> Vec<EnhASTIndex> {
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let project = if let Some(project) = project {
            project
        } else {
            RasmProject::new(PathBuf::from(file_name))
        };
        let helper = get_helper_for_project(&project);

        let file_name = Some(PathBuf::from(file_name));
        let index = EnhASTIndex::new(file_name.clone(), row, col);

        vec_selectable_item_to_vec_target_index(helper.find(&index).unwrap())
    }
}
