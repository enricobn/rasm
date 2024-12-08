use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io;

use rasm_core::codegen::c::options::COptions;
use rasm_core::codegen::compile_target::CompileTarget;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::val_context::ValContext;
use rasm_core::commandline::CommandLineOptions;
use rasm_core::errors::CompilationError;
use rasm_core::project::{RasmProject, RasmProjectRunType};
use rasm_core::type_check::ast_modules_container::{ASTModulesContainer, ASTTypeFilter};
use rasm_core::type_check::ast_type_checker::{
    ASTTypeCheckError, ASTTypeCheckInfo, ASTTypeChecker,
};
use rasm_parser::catalog::modules_catalog::ModulesCatalog;
use rasm_parser::catalog::{ASTIndex, ModuleId, ModuleInfo, ModuleNamespace};
use rasm_parser::parser::ast::{ASTModule, ASTPosition, ASTType, BuiltinTypeKind};
use rasm_utils::OptionDisplay;

use crate::completion_service::{CompletionItem, CompletionResult, CompletionTrigger};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IDETextEdit {
    pub from: ASTIndex,
    pub len: usize,
    pub text: String,
}

impl IDETextEdit {
    pub fn new(from: ASTIndex, len: usize, text: String) -> Self {
        Self { from, len, text }
    }
}

pub struct IDERange {
    pub start: ASTIndex,
    pub len: usize,
}

impl IDERange {
    pub fn new(start: ASTIndex, len: usize) -> Self {
        Self { start, len }
    }

    pub fn contains(&self, index: &ASTIndex) -> bool {
        if self.start.module_id() != index.module_id()
            || self.start.module_namespace() != index.module_namespace()
        {
            return false;
        }
        self.start.position().row == index.position().row
            && index.position().column >= self.start.position().column
            && index.position().column <= (self.start.position().column + self.len - 1)
    }
}

#[derive(Debug, Clone)]
pub struct IDESelectableItem {
    pub start: ASTIndex,
    pub len: usize,
    pub target: Option<IDESelectableItemTarget>,
}

impl Display for IDESelectableItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "{} -> {}",
            self.start,
            OptionDisplay(&self.target)
        ))
    }
}

impl IDESelectableItem {
    pub fn new(index: ASTIndex, len: usize, target: Option<IDESelectableItemTarget>) -> Self {
        Self {
            start: index,
            len,
            target,
        }
    }

    pub fn contains(&self, index: &ASTIndex) -> bool {
        IDERange::new(self.start.clone(), self.len).contains(index)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IDESelectableItemTarget {
    Ref(ASTIndex, Option<ASTType>),
    Function(ASTIndex, ASTType, String),
    Type(Option<ASTIndex>, ASTType),
    Itself(ASTType), // used for let or parameters
}

impl Display for IDESelectableItemTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let descr = match self {
            IDESelectableItemTarget::Ref(index, t) => format!("Ref({index}, {})", OptionDisplay(t)),
            IDESelectableItemTarget::Function(index, t, descr) => {
                format!("Function({index}, {t}, {descr})")
            }
            IDESelectableItemTarget::Type(index, t) => {
                format!("Type({}, {t})", OptionDisplay(index))
            }
            IDESelectableItemTarget::Itself(t) => {
                format!("Itself({t}")
            }
        };

        f.write_str(&descr)
    }
}

impl IDESelectableItemTarget {
    pub fn index(&self) -> Option<ASTIndex> {
        match self {
            IDESelectableItemTarget::Ref(index, _) => Some(index.clone()),
            IDESelectableItemTarget::Function(index, _, _) => Some(index.clone()),
            IDESelectableItemTarget::Type(index, _) => index.clone(),
            IDESelectableItemTarget::Itself(_) => None,
        }
    }

    pub fn completion_type(&self) -> Option<ASTType> {
        match self {
            IDESelectableItemTarget::Ref(_, t) => t.clone(),
            IDESelectableItemTarget::Function(_, t, _) => Some(t.clone()),
            IDESelectableItemTarget::Type(_, t) => Some(t.clone()),
            IDESelectableItemTarget::Itself(t) => Some(t.clone()),
        }
    }
}

#[derive(Debug)]
pub enum IDECompletionTrigger {
    Invoked,
    Character(char),
    IncompleteCompletion,
}

pub enum IDECompletionType {
    SelectableItem(ASTIndex, Option<String>),
    Identifier(String),
}

pub struct IDEHelperBuilder<'a> {
    entries: HashMap<ModuleId, (&'a ASTModule, ModuleNamespace, bool)>,
}

impl<'a> IDEHelperBuilder<'a> {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn add(
        mut self,
        module: &'a ASTModule,
        id: ModuleId,
        namespace: ModuleNamespace,
        add_builtin: bool,
    ) -> Self {
        self.entries
            .insert(id, (module, namespace, add_builtin))
            .map(|it| panic!("Already added {it:?}"));

        self
    }

    pub fn build(self) -> IDEHelper {
        let mut modules_container = ASTModulesContainer::new();

        for (id, (module, namespace, add_builtin)) in self.entries.iter() {
            modules_container.add(module, namespace.clone(), id.clone(), *add_builtin);
        }

        let mut static_val_context = ValContext::new(None);

        let mut type_checker = ASTTypeChecker::new();
        let mut selectable_items = Vec::new();

        for (id, (module, namespace, _add_builtin)) in self.entries.iter() {
            //module_info_to_enh_info.insert(info.module_info(), info.clone());
            let mut val_context = ValContext::new(None);

            type_checker.add_body(
                &mut val_context,
                &mut static_val_context,
                &module.body,
                None,
                &namespace,
                &id,
                &modules_container,
            );
        }

        for (id, (module, namespace, _add_builtin)) in self.entries.iter() {
            let info = ModuleInfo::new(namespace.clone(), id.clone());

            for function in module.functions.iter() {
                type_checker.add_function(
                    &function,
                    &static_val_context,
                    &namespace,
                    &id,
                    &modules_container,
                );

                let start = ASTIndex::new(namespace.clone(), id.clone(), function.position.clone());

                selectable_items.push(IDESelectableItem::new(start, function.name.len(), None));

                for par in function.parameters.iter() {
                    let par_type = &par.ast_type;

                    self.add_par_selectable(
                        par_type,
                        &info,
                        &modules_container,
                        &mut selectable_items,
                    );
                }
            }
        }

        for (index, entry) in type_checker.result.map.iter() {
            let ast_type = entry.filter().clone().and_then(|it| {
                if let ASTTypeFilter::Exact(exact, _id) = it {
                    Some(exact)
                } else {
                    None
                }
            });

            let selectable_item = match entry.info() {
                ASTTypeCheckInfo::Call(name, function_signatures) => {
                    let target = if function_signatures.len() == 1 {
                        let (function_signature, function_index) =
                            function_signatures.first().unwrap();

                        Some(IDESelectableItemTarget::Function(
                            function_index.clone(),
                            function_signature.return_type.clone(),
                            format!("{function_signature}"),
                        ))
                    } else {
                        None
                    };

                    Some(IDESelectableItem::new(index.clone(), name.len(), target))
                }
                ASTTypeCheckInfo::LambdaCall(function_signature, lambda_index) => {
                    Some(IDESelectableItem::new(
                        index.clone(),
                        function_signature.name.len(),
                        Some(IDESelectableItemTarget::Function(
                            lambda_index.clone(),
                            function_signature.return_type.clone(),
                            format!("{function_signature}"),
                        )),
                    ))
                }
                ASTTypeCheckInfo::Ref(name, ref_index) => Some(IDESelectableItem::new(
                    index.clone(),
                    name.len(),
                    Some(IDESelectableItemTarget::Ref(
                        ref_index.clone(),
                        ast_type.clone(),
                    )),
                )),
                ASTTypeCheckInfo::Let(name, _is_const) => Some(IDESelectableItem::new(
                    index.clone(),
                    name.len(),
                    ast_type.map(|it| IDESelectableItemTarget::Itself(it)),
                )),
                ASTTypeCheckInfo::Value(len) => {
                    if let Some(ref t) = ast_type {
                        if !matches!(
                            t,
                            ASTType::Builtin(BuiltinTypeKind::Lambda {
                                parameters: _,
                                return_type: _
                            })
                        ) {
                            Some(IDESelectableItem::new(
                                index.clone(),
                                *len,
                                Some(IDESelectableItemTarget::Type(None, t.clone())),
                            ))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                ASTTypeCheckInfo::Param(name) => Some(IDESelectableItem::new(
                    index.clone(),
                    name.len(),
                    ast_type.map(|it| IDESelectableItemTarget::Itself(it)),
                )),
                ASTTypeCheckInfo::Lambda => None,
            };

            if let Some(si) = selectable_item {
                selectable_items.push(si);
            }
        }

        let errors = type_checker.errors;

        IDEHelper::new(modules_container, selectable_items, errors)
    }

    fn add_par_selectable(
        &self,
        par_type: &ASTType,
        info: &ModuleInfo,
        modules_container: &ASTModulesContainer,
        selectable_items: &mut Vec<IDESelectableItem>,
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

            let ct_start = ASTIndex::new(
                info.namespace().clone(),
                info.id().clone(),
                position.clone(),
            );

            if let Some(ct_index) =
                IDEHelper::get_custom_type_index(modules_container, &info.namespace(), name)
            {
                let (_, ct_namespace, _) = self.entries.get(&ct_index.module_id()).unwrap();

                let def_info = ModuleInfo::new(ct_namespace.clone(), ct_index.module_id().clone());
                let def_index = ASTIndex::new(
                    def_info.namespace().clone(),
                    def_info.id().clone(),
                    ct_index.position().clone(),
                );

                selectable_items.push(IDESelectableItem::new(
                    ct_start,
                    name.len(),
                    Some(IDESelectableItemTarget::Type(
                        Some(def_index),
                        par_type.clone(),
                    )),
                ));
            }
        }
    }
}

pub fn get_ide_helper_from_catalog<ID, NAMESPACE>(
    catalog: &dyn ModulesCatalog<ID, NAMESPACE>,
    add_builtin: bool,
) -> IDEHelper {
    let mut builder = IDEHelperBuilder::new();

    for (module, _id, _namespace, module_id, module_namespace) in catalog.catalog() {
        builder = builder.add(
            &module,
            module_id.clone(),
            module_namespace.clone(),
            add_builtin,
        );
    }

    builder.build()
}

pub fn get_ide_helper_from_project(project: &RasmProject) -> (IDEHelper, Vec<CompilationError>) {
    let mut statics = Statics::new();

    let mut builder = IDEHelperBuilder::new();

    let (modules, errors) = project.get_all_modules(
        &mut statics,
        &RasmProjectRunType::Main,
        &CompileTarget::C(COptions::default()),
        false,
        &CommandLineOptions::default(),
    );

    for (module, info) in modules.iter() {
        builder = builder.add(&module, info.module_id(), info.module_namespace(), false);
    }

    (builder.build(), errors)
}

#[derive(PartialEq, Eq)]
enum CharAtResult {
    Char(char),
    EndOfLine,
    Outside,
}

pub struct IDEHelper {
    modules_container: ASTModulesContainer,
    selectable_items: Vec<IDESelectableItem>,
    errors: Vec<ASTTypeCheckError>,
}

impl IDEHelper {
    fn new(
        modules_container: ASTModulesContainer,
        selectable_items: Vec<IDESelectableItem>,
        errors: Vec<ASTTypeCheckError>,
    ) -> Self {
        Self {
            modules_container,
            selectable_items,
            errors,
        }
    }

    pub fn find(&self, index: &ASTIndex) -> Vec<IDESelectableItem> {
        let mut result = Vec::new();

        for selectable_item in self.selectable_items.iter() {
            if selectable_item.contains(index) {
                result.push(selectable_item.clone());
            }
        }

        result
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
                    match Self::char_at_index(&lines, &index) {
                        CharAtResult::Char(c) => {
                            if c == '.' {
                                // it could be a number
                                completion_type =
                                    self.dot_completion(&lines, &index, Some(prefix), module_info);
                                break;
                            } else if c.is_whitespace() {
                            } else if c == '{' || c == ';' || c == '=' {
                                // prefix could be a number
                                completion_type = Some(IDECompletionType::Identifier(prefix));
                                break;
                            } else if c.is_alphanumeric() {
                                prefix.insert(0, c);
                            } else {
                                break;
                            }
                        }
                        CharAtResult::EndOfLine => {}
                        CharAtResult::Outside => {
                            return Ok(CompletionResult::NotFound("Out of bounds".to_owned()));
                        }
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
            IDECompletionType::SelectableItem(index, prefix) => {
                for selectable_item in self.selectable_items.iter() {
                    if selectable_item.contains(&index) {
                        if let Some(ref target) = selectable_item.target {
                            if let Some(ast_type) = target.completion_type() {
                                return Self::completion_for_type(
                                    &ast_type,
                                    module_info,
                                    &self.modules_container,
                                    &prefix,
                                );
                            }
                        }
                    }
                }
            }
            IDECompletionType::Identifier(prefix) => {
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

    pub fn errors(&self) -> &Vec<ASTTypeCheckError> {
        &self.errors
    }

    pub fn container(&self) -> &ASTModulesContainer {
        &self.modules_container
    }

    fn dot_completion(
        &self,
        lines: &Vec<&str>,
        index: &ASTPosition,
        prefix: Option<String>,
        module_info: &ModuleInfo,
    ) -> Option<IDECompletionType> {
        let index = if let Some(i) = Self::move_left(lines, index) {
            i
        } else {
            return None;
        };
        if let Some(index) = Self::find_last_char_excluding(&lines, &index, &|c| !c.is_whitespace())
            .and_then(|it| {
                if Self::char_at_index(lines, &it) == CharAtResult::Char('"') {
                    Self::move_left(lines, &it)
                } else {
                    Self::find_last_char_excluding(&lines, &it, &|c| c.is_alphabetic())
                }
            })
        {
            Some(IDECompletionType::SelectableItem(
                ASTIndex::new(
                    module_info.namespace().clone(),
                    module_info.id().clone(),
                    ASTPosition::new(index.row, index.column),
                ),
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
            if !function.signature.modifiers.public
                && &function.namespace != module_info.namespace()
            {
                continue;
            }
            if let Some(p) = prefix {
                if !function.signature.name.starts_with(p) {
                    continue;
                }
            }
            if !function.signature.parameters_types.is_empty() {
                let parameter_type = function.signature.parameters_types.get(0).unwrap();
                if filter.is_compatible(parameter_type, module_info.namespace(), modules_container)
                {
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
            if !function.signature.modifiers.public
                && &function.namespace != module_info.namespace()
            {
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
            match Self::char_at_index(&lines, &result) {
                CharAtResult::Char(c) => {
                    if find(c) {
                        return Some(result);
                    } else if c == ')' {
                        result =
                            Self::find_open_bracket(&lines, &Self::move_left(&lines, &result)?)?;
                        return Self::move_left(&lines, &result);
                    }
                    result = Self::move_left(lines, &result)?;
                }
                CharAtResult::EndOfLine => {
                    return None;
                }
                CharAtResult::Outside => {
                    return None;
                }
            }
        }
    }

    fn char_at_index(lines: &Vec<&str>, index: &ASTPosition) -> CharAtResult {
        if let Some(line) = lines.get(index.row - 1) {
            if index.column == line.len() + 1 {
                CharAtResult::EndOfLine
            } else if let Some(c) = line.chars().nth(index.column - 1) {
                CharAtResult::Char(c)
            } else {
                CharAtResult::Outside
            }
        } else {
            CharAtResult::Outside
        }
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
            if let CharAtResult::Char(c) = Self::char_at_index(lines, &result) {
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
            } else {
                return None;
            }
        }

        Some(result)
    }

    pub fn references(&self, index: &ASTIndex) -> Vec<IDESelectableItem> {
        let mut items = self.find(index);

        if items.len() == 1 {
            let item = items.remove(0);

            let mut result = Vec::new();
            for se in self.selectable_items.iter() {
                if let Some(ref target) = se.target {
                    if let Some(i) = target.index() {
                        if i == item.start {
                            result.push(se.clone());
                        }
                    }
                }
            }

            return result;
        }

        Vec::new()
    }

    pub fn rename(&self, index: &ASTIndex, new_name: String) -> Result<Vec<IDETextEdit>, String> {
        let mut result = Vec::new();

        let items = self.find(&index);
        if let Some(item) = items.first() {
            let root_index_o = if let Some(ref target) = item.target {
                if matches!(target, IDESelectableItemTarget::Itself(_)) {
                    result.push(IDETextEdit::new(
                        item.start.clone(),
                        item.len,
                        new_name.to_owned(),
                    ));
                    Some(item.start.clone())
                } else {
                    if let Some(index) = target.index() {
                        result.push(IDETextEdit::new(index, item.len, new_name.to_owned()));
                    }
                    target.index()
                }
            } else {
                result.push(IDETextEdit::new(
                    item.start.clone(),
                    item.len,
                    new_name.to_owned(),
                ));
                Some(item.start.clone())
            };

            if let Some(root_index) = root_index_o {
                let references = self.references(&root_index);

                for reference in references.iter() {
                    result.push(IDETextEdit {
                        from: reference.start.clone(),
                        len: item.len,
                        text: new_name.clone(),
                    });
                }
            }
        }

        if result
            .iter()
            .all(|it| it.from.module_id() == index.module_id())
        {
            Ok(result)
        } else {
            // TODO we want to be able to rename symbols of the current lib
            Err("Rename of symbols outside current module is not yet supported.".to_owned())
        }
    }

    fn get_custom_type_index(
        modules_container: &ASTModulesContainer,
        module_namespace: &ModuleNamespace,
        name: &str,
    ) -> Option<ASTIndex> {
        if let Some((id, def)) = modules_container.get_enum_def(module_namespace, name) {
            Some(ASTIndex::new(
                id.namespace().clone(),
                id.id().clone(),
                def.position.clone(),
            ))
        } else if let Some((id, def)) = modules_container.get_struct_def(module_namespace, name) {
            Some(ASTIndex::new(
                id.namespace().clone(),
                id.id().clone(),
                def.position.clone(),
            ))
        } else if let Some((id, def)) = modules_container.get_type_def(module_namespace, name) {
            Some(ASTIndex::new(
                id.namespace().clone(),
                id.id().clone(),
                def.position.clone(),
            ))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::env;
    use std::iter::zip;
    use std::path::{Path, PathBuf};

    use rasm_core::codegen::c::options::COptions;
    use rasm_core::codegen::compile_target::CompileTarget;
    use rasm_core::errors::CompilationError;
    use rasm_core::project::RasmProject;
    use rasm_parser::catalog::{ASTIndex, ModuleId, ModuleInfo, ModuleNamespace};
    use rasm_parser::lexer::Lexer;
    use rasm_parser::parser::ast::{ASTFunctionSignature, ASTPosition, ASTType, BuiltinTypeKind};
    use rasm_parser::parser::Parser;
    use rasm_utils::OptionDisplay;

    use crate::completion_service::{CompletionResult, CompletionTrigger};
    use crate::ide_helper::IDEHelperBuilder;

    use super::{
        get_ide_helper_from_project, IDEHelper, IDESelectableItem, IDESelectableItemTarget,
    };

    #[test]
    fn simple() {
        let (project, helper) = get_helper("resources/test/simple.rasm");

        let std_lib_project = stdlib_project();

        assert_eq!(
            vec_selectable_item_to_vec_target_index(helper.find(&get_index(
                &project,
                "simple.rasm",
                3,
                15,
            ))),
            vec![get_index(&project, "simple.rasm", 1, 5)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(helper.find(&get_index(
                &project,
                "simple.rasm",
                6,
                13
            ))),
            vec![get_index(&project, "simple.rasm", 5, 16)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(helper.find(&get_index(
                &project,
                "simple.rasm",
                3,
                2,
            ))),
            vec![get_index(&project, "simple.rasm", 5, 4)]
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(helper.find(&get_index(
                &project,
                "simple.rasm",
                6,
                9,
            ))),
            vec![get_index(
                &std_lib_project,
                "src/main/rasm/print.rasm",
                12,
                8
            )]
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(helper.find(&get_index(
                &project,
                "simple.rasm",
                10,
                15,
            ))),
            vec![get_index(
                &std_lib_project,
                "src/main/rasm/option.rasm",
                2,
                3
            )]
        );
    }

    #[test]
    fn types() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        let std_lib_project = stdlib_project();

        assert_eq!(
            vec_selectable_item_to_vec_target_index(helper.find(&get_index(
                &project,
                "types.rasm",
                15,
                23
            ))),
            vec![get_index(
                &std_lib_project,
                "src/main/rasm/option.rasm",
                1,
                10
            )],
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(helper.find(&get_index(
                &project,
                "types.rasm",
                19,
                23,
            ))),
            vec![get_index(&project, "types.rasm", 1, 8)],
        );

        assert_eq!(
            vec_selectable_item_to_vec_target_index(helper.find(&get_index(
                &project,
                "types.rasm",
                23,
                23,
            ))),
            vec![get_index(&std_lib_project, "src/main/c/vec.rasm", 1, 10)],
        );
    }

    #[test]
    fn complex_expression_completions() {
        env::set_var("RASM_STDLIB", "../../../stdlib");

        let values = get_completion_values(
            Some(RasmProject::new(PathBuf::from(
                "resources/test/complex_expression.rasm",
            ))),
            "complex_expression.rasm",
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
        env::set_var("RASM_STDLIB", "../../../stdlib");

        let values = get_find(
            Some(RasmProject::new(PathBuf::from(
                "resources/test/complex_expression.rasm",
            ))),
            "complex_expression.rasm",
            11,
            13,
        );

        // println!("values {}", SliceDisplay(&values));

        assert_eq!(1, values.len());

        assert!(format!("{}", values.get(0).unwrap()).ends_with("if.rasm:9:8"));
    }

    #[test]
    fn struct_constructor() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        assert_eq!(
            vec_selectable_item_to_vec_target_index(helper.find(&get_index(
                &project,
                "types.rasm",
                6,
                19,
            ))),
            vec![get_index(&project, "types.rasm", 1, 8)],
        );
    }

    #[test]
    fn types_1() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        let found = helper.find(&get_index(&project, "types.rasm", 27, 31));

        assert_eq!(
            vec_selectable_item_to_vec_target_index(found),
            vec!(get_index(&project, "types.rasm", 1, 8,)),
        );
    }

    #[test]
    fn types_2() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        let found = helper.find(&get_index(&project, "types.rasm", 9, 1));

        assert_eq!(
            vec_selectable_item_to_vec_target_index(found),
            vec!(get_index(&project, "types.rasm", 15, 4,)),
        );
    }

    #[test]
    fn types_3() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        let found = helper.find(&get_index(&project, "types.rasm", 9, 13));

        assert_eq!(
            vec_selectable_item_to_vec_target_index(found),
            vec!(get_index(&project, "types.rasm", 5, 5,)),
        );
    }

    #[test]
    fn types_completion_struct_property() {
        env::set_var("RASM_STDLIB", "../../../stdlib");

        let values = get_completion_values(
            Some(RasmProject::new(PathBuf::from("resources/test/types.rasm"))),
            "types.rasm",
            12,
            16,
            CompletionTrigger::Character('.'),
        )
        .unwrap();

        assert_eq!(3, values.into_iter().filter(|it| it == "anI32").count());
    }

    #[test]
    fn types_completion_match() {
        env::set_var("RASM_STDLIB", "../../../stdlib");

        let values = get_completion_values(
            Some(RasmProject::new(PathBuf::from("resources/test/types.rasm"))),
            "types.rasm",
            13,
            7,
            CompletionTrigger::Character('.'),
        )
        .unwrap();

        assert_eq!(1, values.into_iter().filter(|it| it == "match").count());
    }

    #[test]
    fn types_completion_dot() {
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let values = get_completion_values(
            Some(RasmProject::new(PathBuf::from("resources/test/types.rasm"))),
            "types.rasm",
            36,
            16,
            CompletionTrigger::Character('.'),
        )
        .unwrap();
        assert_eq!(3, values.into_iter().filter(|it| it == "anI32").count());
    }

    #[test]
    fn types_completion_invoked_function_call() {
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let values = get_completion_values(
            Some(RasmProject::new(PathBuf::from("resources/test/types.rasm"))),
            "types.rasm",
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
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let values = get_completion_values(
            Some(RasmProject::new(PathBuf::from("resources/test/types.rasm"))),
            "types.rasm",
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
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let values = get_completion_values(
            Some(RasmProject::new(PathBuf::from("resources/test/types.rasm"))),
            "types.rasm",
            32,
            8,
            CompletionTrigger::Invoked,
        )
        .unwrap();

        assert_eq!(vec!["structVec"], values);
    }

    #[test]
    fn types_completion_dot_string() {
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let values = get_completion_values(
            Some(RasmProject::new(PathBuf::from("resources/test/types.rasm"))),
            "types.rasm",
            40,
            14,
            CompletionTrigger::Character('.'),
        )
        .unwrap();

        assert_eq!(1, values.into_iter().filter(|it| it == "substr").count());
    }

    #[test]
    fn types_completion_dot_char() {
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let values = get_completion_values(
            Some(RasmProject::new(PathBuf::from("resources/test/types.rasm"))),
            "types.rasm",
            44,
            9,
            CompletionTrigger::Character('.'),
        )
        .unwrap();

        assert_eq!(1, values.into_iter().filter(|it| it == "isAlpha").count());
    }

    #[test]
    fn types_lambda_param_type() {
        let (project, helper) = get_helper("resources/test/types.rasm");

        let mut selectable_items = helper.find(&get_index(&project, "types.rasm", 32, 28));

        if selectable_items.len() == 1 {
            let selectable_item = selectable_items.remove(0);
            if let Some(ref target) = selectable_item.target {
                if let Some(ASTType::Custom {
                    name,
                    param_types: _,
                    position: _,
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

    #[test]
    fn types_flatten() {
        let (_project, helper) = get_helper("resources/test/types.rasm");

        let stdlib_project = stdlib_project();

        let mut selectable_items = helper.find(&get_index(
            &stdlib_project,
            "src/main/rasm/result.rasm",
            11,
            9,
        ));

        if selectable_items.len() == 1 {
            let selectable_item = selectable_items.remove(0);
            let expected_index = get_index(&stdlib_project, "src/main/rasm/result.rasm", 14, 8);
            if let Some(IDESelectableItemTarget::Function(index, _, _)) = selectable_item.target {
                assert_eq!(index, expected_index);
            } else {
                panic!("Found {:?}", selectable_item.target);
            }
        } else {
            panic!("Found {} elements", selectable_items.len());
        }
    }

    #[test]
    fn enums() {
        let (project, helper) = get_helper("resources/test/enums.rasm");

        let mut items = helper.find(&get_index(&project, "enums.rasm", 17, 13));

        assert_eq!(1, items.len());

        let item = items.remove(0);

        if let Some(IDESelectableItemTarget::Function(index, _, descr)) = item.target {
            assert_eq!(
                get_index_with_builtin(&project, "enums.rasm", 10, 6, Some("match".to_owned())),
                index
            );
            assert!(descr.starts_with("match"));
        } else {
            panic!("Found {:?}", item.target);
        }
    }

    #[test]
    fn enums_1() {
        let (project, helper) = get_helper("resources/test/enums.rasm");

        let mut items = helper.find(&get_index(&project, "enums.rasm", 17, 40));

        assert_eq!(1, items.len());

        let item = items.remove(0);

        if let Some(IDESelectableItemTarget::Ref(index, _)) = item.target {
            assert_eq!(get_index(&project, "enums.rasm", 17, 21), index);
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
        test_references(
            "resources/test/types.rasm",
            6,
            7,
            vec![(10, 13), (12, 9)],
            "types.rasm",
        );
    }

    #[test]
    fn reference1() {
        test_references(
            "resources/test/references.rasm",
            2,
            14,
            vec![(3, 13)],
            "references.rasm",
        );
    }

    #[test]
    fn rename() {
        test_rename(
            "resources/test/references.rasm",
            "s",
            2,
            14,
            Ok(vec![(2, 13, 2), (3, 13, 2)]),
            "references.rasm",
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
            "simple.rasm",
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
            "simple.rasm",
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
            "types.rasm",
        );
    }

    #[test]
    fn incomplete_source_completion() {
        let result = get_completion_values_from_str(
            "let s = \"\";\n\
            let a = s.\n\
        fn f1(s: str) {\n\
        }",
            2,
            10,
            CompletionTrigger::Character('.'),
        );

        assert_eq!(vec!["f1".to_owned()], result.unwrap());
    }

    #[test]
    fn incomplete_source_completion_invoked() {
        let result = get_completion_values_from_str(
            "let s = \"\";\n\
            let a = s.\n\
        fn f1(s: str) {\n\
        }",
            2,
            11,
            CompletionTrigger::Invoked,
        );

        assert_eq!(vec!["f1".to_owned()], result.unwrap());
    }

    #[test]
    fn test_disambiguate_functions() {
        let (project, helper) = get_helper("resources/test/disambiguate_functions.rasm");

        let mut items = helper.find(&get_index(&project, "disambiguate_functions.rasm", 2, 6));

        assert_eq!(1, items.len());

        let item = items.remove(0);

        if item.target.is_none() {
            panic!("Expected a target.")
        }
    }

    #[test]
    #[ignore]
    fn duplicated_signatures_in_breakout() {
        let (_project, helper) = get_helper("../rasm/resources/examples/breakout");

        let container = helper.container();

        let signatures_by_name_and_len =
            container
                .signatures()
                .iter()
                .fold(HashMap::new(), |mut accum, it| {
                    let entry = accum.entry((
                        it.signature.name.clone(),
                        it.signature.parameters_types.len(),
                    ));
                    let vec = entry.or_insert(Vec::new());
                    vec.push(it.signature.clone());
                    accum
                });

        for ((name, len), signatures) in signatures_by_name_and_len.into_iter() {
            for signature in signatures.iter() {
                let same_signatures = signatures
                    .clone()
                    .into_iter()
                    .filter(|it| {
                        same_signature(it, signature)
                            && (it.modifiers.public || signature.modifiers.public)
                    })
                    .collect::<Vec<_>>();
                if same_signatures.len() > 1 {
                    let mut message = format!("same signature for {name}, {len}\n");
                    for s in same_signatures.iter() {
                        message += &format!("     {s}\n");
                    }
                    panic!("{message}");
                }
            }
        }
    }

    fn same_signature(s1: &ASTFunctionSignature, s2: &ASTFunctionSignature) -> bool {
        if s1.parameters_types.len() != s2.parameters_types.len() {
            return false;
        }
        zip(s1.parameters_types.iter(), s2.parameters_types.iter()).all(|(t1, t2)| {
            (t1.is_strictly_generic() && not_a_lambda(t2))
                || (t2.is_strictly_generic() && not_a_lambda(t1))
                || t1 == t2
        })
    }

    fn not_a_lambda(t: &ASTType) -> bool {
        !matches!(
            t,
            ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters: _,
                return_type: _
            })
        )
    }

    fn stdlib_project() -> RasmProject {
        RasmProject::new(Path::new("../stdlib").to_path_buf())
    }

    fn test_references(
        file: &str,
        row: usize,
        column: usize,
        expected: Vec<(usize, usize)>,
        file_name: &str,
    ) {
        let (project, helper) = get_helper(file);

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

        let items = helper.references(&get_index(&project, file_name, row, column));

        let mut found = items
            .into_iter()
            .map(|it| (it.start.position().row, it.start.position().column))
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
        file_name: &str,
    ) {
        /*
        let (project, eh_module, module) = get_reference_finder(file, None);
        let finder = get_reference_finder2_for_project(&project);
        */

        let (project, helper) = get_helper(file);

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

        let edits = helper.rename(
            &get_index(&project, file_name, row, column),
            new_name.to_owned(),
        );

        let found = edits.map(|it| {
            let mut f = it
                .into_iter()
                .map(|it| (it.from.position().row, it.from.position().column, it.len))
                .collect::<Vec<_>>();
            f.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
            f
        });

        assert_eq!(expected, found);
    }

    fn get_index(project: &RasmProject, file_n: &str, row: usize, column: usize) -> ASTIndex {
        get_index_with_builtin(project, file_n, row, column, None)
    }

    fn get_index_with_builtin(
        project: &RasmProject,
        file_n: &str,
        row: usize,
        column: usize,
        builtin: Option<String>,
    ) -> ASTIndex {
        let path = Path::new(file_n);

        let (_, _, info) = project
            .get_module(
                &project
                    .from_relative_to_root(path)
                    .as_path()
                    .canonicalize()
                    .unwrap()
                    .to_path_buf(),
                &CompileTarget::C(COptions::default()),
            )
            .unwrap();

        ASTIndex::new(
            info.module_namespace(),
            info.module_id(),
            ASTPosition {
                row,
                column,
                builtin,
            },
        )
    }

    fn vec_selectable_item_to_vec_target_index(vec: Vec<IDESelectableItem>) -> Vec<ASTIndex> {
        vec.iter()
            .flat_map(|it| it.target.clone().and_then(|item| item.index()))
            .collect::<Vec<_>>()
    }

    fn get_helper(project_path: &str) -> (RasmProject, IDEHelper) {
        let (project, helper, _errors) = get_helper_with_errors(project_path);
        (project, helper)
    }

    fn get_helper_with_errors(
        project_path: &str,
    ) -> (RasmProject, IDEHelper, Vec<CompilationError>) {
        env::set_var("RASM_STDLIB", "../../../stdlib");

        let file_name = Path::new(project_path);
        let project = RasmProject::new(file_name.to_path_buf());

        let (helper, errors) = get_ide_helper_from_project(&project);
        (project, helper, errors)
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
        let (helper, _errors) = get_ide_helper_from_project(&project);

        let path = project
            .from_relative_to_root(PathBuf::from(file_name).as_path())
            .canonicalize()
            .unwrap();

        let info = get_index(&project, file_name, 0, 0).info();

        match helper.get_completions(
            project.content_from_file(&path).unwrap(),
            &ASTPosition::new(row, col),
            &trigger,
            &info, //&helper.get_module_info_from_path(&path).unwrap(),
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

    fn get_completion_values_from_str(
        content: &str,
        row: usize,
        col: usize,
        trigger: CompletionTrigger,
    ) -> Result<Vec<String>, String> {
        let lexer = Lexer::new(content.to_owned());
        let (module, _errors) = Parser::new(lexer).parse();
        let module_info =
            ModuleInfo::new(ModuleNamespace("ns".to_owned()), ModuleId("id".to_owned()));

        let helper = IDEHelperBuilder::new()
            .add(
                &module,
                module_info.id().clone(),
                module_info.namespace().clone(),
                true,
            )
            .build();

        match helper.get_completions(
            content.to_owned(),
            &ASTPosition::new(row, col),
            &trigger,
            &module_info,
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
    ) -> Vec<ASTIndex> {
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let project = if let Some(project) = project {
            project
        } else {
            RasmProject::new(PathBuf::from(file_name))
        };
        let (helper, _errors) = get_ide_helper_from_project(&project);

        let index = get_index(&&project, file_name, row, col);

        vec_selectable_item_to_vec_target_index(helper.find(&index))
    }
}
