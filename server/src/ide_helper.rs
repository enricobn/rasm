use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::io;

use linked_hash_map::LinkedHashMap;
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
use rasm_parser::catalog::{ASTIndex, ModuleId, ModuleInfo, ModuleNamespace};
use rasm_parser::lexer::Lexer;
use rasm_parser::parser::ast::{
    ASTBuiltinFunctionType, ASTExpression, ASTFunctionDef, ASTPosition, ASTStatement, ASTType,
    BuiltinTypeKind, CustomTypeDef,
};
use rasm_parser::parser::Parser;
use rasm_utils::OptionDisplay;

use crate::ast_tree::{ASTElement, ASTTree};
use crate::completion_service::{CompletionItem, CompletionResult, CompletionTrigger};
use crate::statement_finder::StatementFinder;

pub enum IDESymbolKind {
    Struct,
    Enum,
    Type,
    Function,
}

pub struct IDESymbolInformation {
    pub name: String,
    pub kind: IDESymbolKind,
    pub index: ASTIndex,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IDETextEdit {
    pub from: ASTIndex,
    pub to: ASTIndex,
    pub text: String,
}

impl IDETextEdit {
    pub fn new(from: ASTIndex, to: ASTIndex, text: String) -> Self {
        Self { from, to, text }
    }
    pub fn same_line(from: ASTIndex, len: usize, text: String) -> Self {
        let to = from.mv_right(len);
        Self { from, to, text }
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

pub fn get_ide_helper_from_project(project: &RasmProject) -> (IDEHelper, Vec<CompilationError>) {
    let mut statics = Statics::new();

    let (container, _catalog, errors) = project.container_and_catalog(
        &mut statics,
        &RasmProjectRunType::Main,
        &CompileTarget::C(COptions::default()),
        false,
        &CommandLineOptions::default(),
    );

    (IDEHelper::from_container(container), errors)
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
    pub fn from_container(modules_container: ASTModulesContainer) -> IDEHelper {
        let mut static_val_context = ValContext::new(None);

        let mut type_checker = ASTTypeChecker::new();
        let mut selectable_items = Vec::new();

        for (id, namespace, module) in modules_container.modules().into_iter() {
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

        for (id, namespace, module) in modules_container.modules().into_iter() {
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

                    Self::add_selectable_type(
                        par_type,
                        &info,
                        &modules_container,
                        &mut selectable_items,
                    );
                }

                Self::add_selectable_type(
                    &function.return_type,
                    &info,
                    &modules_container,
                    &mut selectable_items,
                );
            }
            /*
            for s in module.structs.iter() {
                // let start = ASTIndex::new(namespace.clone(), id.clone(), s.position.clone());
                //selectable_items.push(IDESelectableItem::new(start, s.name.len(), None));

                for p in s.properties.iter() {
                    let start = ASTIndex::new(namespace.clone(), id.clone(), p.position.clone());
                    selectable_items.push(IDESelectableItem::new(start, p.name.len(), None));
                }
            }
            */
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

    fn add_selectable_type(
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
            if position.builtin.is_some() {
                return;
            }
            for parameter_type in param_types.iter() {
                Self::add_selectable_type(
                    parameter_type,
                    info,
                    modules_container,
                    selectable_items,
                );
            }

            let ct_start = ASTIndex::new(
                info.namespace().clone(),
                info.id().clone(),
                position.clone(),
            );

            if let Some(ct_index) =
                IDEHelper::get_custom_type_index(modules_container, &info.namespace(), name)
            {
                let ct_namespace = modules_container
                    .module_namespace(&ct_index.module_id())
                    .unwrap();

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
                if selectable_item.start.position().builtin == index.position().builtin {
                    result.push(selectable_item.clone());
                }
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
        } /*else {
              for i in items.iter() {
                  println!(
                      "references item {i} {}",
                      OptionDisplay(
                          &(i.target
                              .clone()
                              .and_then(|it| it.index())
                              .and_then(|it| it.position().clone().builtin))
                      )
                  );
              }
          }*/

        Vec::new()
    }

    pub fn rename(&self, index: &ASTIndex, new_name: String) -> Result<Vec<IDETextEdit>, String> {
        let mut result = Vec::new();

        let items = self.find(&index);
        if let Some(item) = items.first() {
            let root_index_o = if let Some(ref target) = item.target {
                if matches!(target, IDESelectableItemTarget::Itself(_)) {
                    result.push(IDETextEdit::same_line(
                        item.start.clone(),
                        item.len,
                        new_name.to_owned(),
                    ));
                    Some(item.start.clone())
                } else {
                    if let Some(index) = target.index() {
                        result.push(IDETextEdit::same_line(index, item.len, new_name.to_owned()));
                    }
                    target.index()
                }
            } else {
                result.push(IDETextEdit::same_line(
                    item.start.clone(),
                    item.len,
                    new_name.to_owned(),
                ));
                Some(item.start.clone())
            };

            if let Some(root_index) = root_index_o {
                match root_index.position().builtin {
                    Some(ASTBuiltinFunctionType::Match)
                    | Some(ASTBuiltinFunctionType::MatchOne) => {
                        return Err("Rename of builtin function.".to_owned());
                    }
                    _ => {}
                }
                let references = self.references(&root_index);

                for reference in references.iter() {
                    result.push(IDETextEdit::same_line(
                        reference.start.clone(),
                        item.len,
                        new_name.clone(),
                    ));
                }
            }
        }

        if result.iter().any(|it| {
            self.modules_container
                .is_readonly_module(&it.from.module_id())
        }) {
            /*
            for edit in result.iter() {
                println!("edit {} : {}", edit.from.module_id(), edit.from.position());
            }
            */
            // TODO we want to be able to rename symbols of the current lib
            Err("Rename of symbols outside current library.".to_owned())
        } else {
            // TODO it can happen for example renaming a type that is the type of a property in a struct, because there are multiple
            // builtin functions that "insist" on the same property (getter, setter, setter with lambda ...), but it
            // could be better to find another way, for example inspecting the ASTPosition.builtin value
            result.sort_by(|a, b| a.from.position().partial_cmp(b.from.position()).unwrap());
            result.dedup();
            Ok(result)
        }
    }

    pub fn symbols(&self) -> Vec<IDESymbolInformation> {
        let mut result = self
            .modules_container
            .enum_defs()
            .into_iter()
            .map(|it| {
                self.type_def_to_symbol(&it.1, it.0.namespace(), it.0.id(), IDESymbolKind::Enum)
            })
            .collect::<Vec<_>>();
        result.append(
            &mut self
                .modules_container
                .struct_defs()
                .into_iter()
                .map(|it| {
                    self.type_def_to_symbol(
                        &it.1,
                        it.0.namespace(),
                        it.0.id(),
                        IDESymbolKind::Struct,
                    )
                })
                .collect::<Vec<_>>(),
        );
        result.append(
            &mut self
                .modules_container
                .type_defs()
                .into_iter()
                .map(|it| {
                    self.type_def_to_symbol(&it.1, it.0.namespace(), it.0.id(), IDESymbolKind::Type)
                })
                .collect::<Vec<_>>(),
        );
        result
    }

    fn type_def_to_symbol(
        &self,
        type_def: &dyn CustomTypeDef,
        namespace: &ModuleNamespace,
        id: &ModuleId,
        kind: IDESymbolKind,
    ) -> IDESymbolInformation {
        IDESymbolInformation {
            name: type_def.name().to_owned(),
            kind,
            index: ASTIndex::new(namespace.clone(), id.clone(), type_def.position().clone()),
        }
    }

    fn function_to_symbol(
        &self,
        function: &ASTFunctionDef,
        namespace: &ModuleNamespace,
        id: &ModuleId,
    ) -> IDESymbolInformation {
        IDESymbolInformation {
            name: function.name.clone(),
            kind: IDESymbolKind::Function,
            index: ASTIndex::new(namespace.clone(), id.clone(), function.position.clone()),
        }
    }

    pub fn module_symbols(&self, module_id: &ModuleId) -> Vec<IDESymbolInformation> {
        if let Some(module) = self.modules_container.module(module_id) {
            let namespace = self.modules_container.module_namespace(module_id).unwrap();
            let mut symbols: Vec<IDESymbolInformation> = module
                .enums
                .iter()
                .map(|it| self.type_def_to_symbol(it, namespace, module_id, IDESymbolKind::Enum))
                .collect();
            symbols.append(
                &mut module
                    .structs
                    .iter()
                    .map(|it| {
                        self.type_def_to_symbol(it, namespace, module_id, IDESymbolKind::Enum)
                    })
                    .collect(),
            );

            symbols.append(
                &mut module
                    .types
                    .iter()
                    .map(|it| {
                        self.type_def_to_symbol(it, namespace, module_id, IDESymbolKind::Enum)
                    })
                    .collect(),
            );

            symbols.append(
                &mut module
                    .functions
                    .iter()
                    .filter(|it| it.position.builtin.is_none())
                    .map(|it| self.function_to_symbol(it, namespace, module_id))
                    .collect(),
            );

            return symbols;
        }
        vec![]
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

    /// find the statement start position of the expression that starts at index
    pub fn statement_start_position(&self, index: &ASTIndex) -> Option<ASTPosition> {
        StatementFinder {}.statement_start_position(index, &self.modules_container)
    }

    pub fn try_extract_let(
        &self,
        expression_text: &str,
        start_index: &ASTIndex,
        end_index: &ASTIndex,
    ) -> Option<Vec<IDETextEdit>> {
        let mut new_text = expression_text.to_owned();
        new_text.push_str(";");
        let parser = Parser::new(Lexer::new(new_text.to_owned()));
        let (_, errors) = parser.parse();

        let module_namespace = start_index.module_namespace().clone();
        let module_id = start_index.module_id().clone();

        if errors.is_empty() {
            if let Some(start_position) = self.statement_start_position(start_index) {
                let mut changes = Vec::new();
                changes.push(IDETextEdit::new(
                    start_index.clone(),
                    end_index.clone(),
                    "valName".to_owned(),
                ));

                new_text.insert_str(0, "let valName = ");
                new_text.push_str("\n");
                if start_position.column > 0 {
                    new_text.push_str(" ".repeat(start_position.column - 1).as_str());
                }

                let insert_index = ASTIndex::new(module_namespace, module_id, start_position);

                changes.push(IDETextEdit::same_line(insert_index, 0, new_text));

                Some(changes)
                //}
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn try_extract_function(
        &self,
        original_code: &str,
        start_index: &ASTIndex,
        end_index: &ASTIndex,
        last_position: ASTPosition,
    ) -> Option<Vec<IDETextEdit>> {
        let (ends_with_semicolon, code) = if original_code.ends_with(';') {
            (true, original_code.to_owned())
        } else {
            let mut new_code = original_code.to_owned();
            new_code.push(';');
            (false, new_code)
        };
        let parser = Parser::new(Lexer::new(code.clone()));
        let (module, errors) = parser.parse();

        if !errors.is_empty() {
            return None;
        }

        let mut container = ASTModulesContainer::new();
        container.add(
            module,
            start_index.module_namespace().clone(),
            start_index.module_id().clone(),
            false,
            true,
        );

        let module = container.module(start_index.module_id())?;
        let mod_position = StatementFinder {}.module_position(start_index, &container)?;

        let tree = ASTTree::new(&mod_position.body());

        // println!("{tree}");

        let last_statement = tree.last_statement()?;

        let last_element = tree.get_element(last_statement)?;

        if let ASTElement::Statement(stmt) = last_element {
            if matches!(stmt, ASTStatement::LetStatement(_, _, _, _)) {
                return None;
            }
        } else {
            return None;
        }

        let last_statement_type = self.type_from_diff(
            end_index.module_id(),
            end_index.module_namespace(),
            last_statement,
            start_index.position().row - 1,
            if end_index.position().row == start_index.position().row {
                start_index.position().column - 1
            } else {
                0
            },
        )?;

        // println!("last_statement_type {last_statement_type}");

        let mut parameters_defs = Vec::new();
        let mut parameters_values = Vec::new();
        let parameters = Self::extract_vals_body(&module.body, &HashSet::new());
        for par in parameters {
            let diff_col = if par.1.row == 1 {
                start_index.position().column - 1
            } else {
                0
            };
            let par_type = self.type_from_diff(
                end_index.module_id(),
                end_index.module_namespace(),
                &par.1,
                start_index.position().row - 1,
                diff_col,
            )?;
            // println!("{} {}", par.0, par_type);
            parameters_defs.push(format!("{}: {}", par.0, par_type));
            parameters_values.push(par.0);
        }

        let mut function_code = format!(
            "\n\nfn newFunction({}) -> {} {{\n",
            parameters_defs.join(", "),
            last_statement_type
        );

        function_code.push_str(&code);
        function_code.push_str("\n}");

        let mut call_code = format!("newFunction({})", parameters_values.join(", "));

        if ends_with_semicolon {
            call_code.push(';');
        }

        //println!("{function_code}");

        Some(vec![
            IDETextEdit::same_line(
                start_index.with_position(last_position.mv_right(1)),
                0,
                function_code,
            ),
            IDETextEdit::new(start_index.clone(), end_index.clone(), call_code),
        ])
    }

    fn type_from_diff(
        &self,
        module_id: &ModuleId,
        module_namespace: &ModuleNamespace,
        position: &ASTPosition,
        diff_row: usize,
        diff_col: usize,
    ) -> Option<ASTType> {
        let position_in_source =
            ASTPosition::new(position.row + diff_row, position.column + diff_col);

        let mut selectable_items = self.find(&ASTIndex::new(
            module_namespace.clone(),
            module_id.clone(),
            position_in_source,
        ));

        if selectable_items.len() != 1 {
            return None;
        }

        let last_statement_target = selectable_items.remove(0).target?;

        last_statement_target.completion_type()
    }

    fn extract_vals_body(
        body: &Vec<ASTStatement>,
        excluding: &HashSet<String>,
    ) -> LinkedHashMap<String, ASTPosition> {
        let mut result = LinkedHashMap::new();
        let mut excluding = excluding.clone();

        for statement in body.iter() {
            let statement_result = match statement {
                ASTStatement::Expression(expr) => Self::extract_vals_expr(expr, &excluding),
                ASTStatement::LetStatement(name, expr, _, _) => {
                    excluding.insert(name.to_owned());
                    Self::extract_vals_expr(expr, &excluding)
                }
            };
            result.extend(statement_result);
        }

        result
    }

    fn extract_vals_expr(
        expr: &ASTExpression,
        excluding: &HashSet<String>,
    ) -> LinkedHashMap<String, ASTPosition> {
        match expr {
            ASTExpression::ASTFunctionCallExpression(function_call) => {
                let mut result = LinkedHashMap::new();
                for e in function_call.parameters.iter() {
                    result.extend(Self::extract_vals_expr(e, excluding));
                }
                result
            }
            ASTExpression::ValueRef(name, position) => {
                if excluding.contains(name) {
                    LinkedHashMap::new()
                } else {
                    let mut result = LinkedHashMap::new();
                    result.insert(name.clone(), position.clone());
                    result
                }
            }
            ASTExpression::Value(_, _) => LinkedHashMap::new(),
            ASTExpression::Lambda(lambda_def) => {
                let mut excluding = excluding.clone();
                for (name, _) in lambda_def.parameter_names.iter() {
                    excluding.insert(name.clone());
                }
                Self::extract_vals_body(&lambda_def.body, &excluding)
            }
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
    use rasm_core::type_check::ast_modules_container::ASTModulesContainer;
    use rasm_parser::catalog::{ASTIndex, ModuleId, ModuleInfo, ModuleNamespace};
    use rasm_parser::lexer::Lexer;
    use rasm_parser::parser::ast::{
        ASTBuiltinFunctionType, ASTFunctionSignature, ASTPosition, ASTType, BuiltinTypeKind,
    };
    use rasm_parser::parser::Parser;
    use rasm_utils::test_utils::init_minimal_log;
    use rasm_utils::{reset_indent, OptionDisplay};

    use crate::completion_service::{CompletionResult, CompletionTrigger};

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
                get_index_with_builtin(
                    &project,
                    "enums.rasm",
                    10,
                    6,
                    Some(ASTBuiltinFunctionType::Match)
                ),
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
            Err("Rename of symbols outside current library.".to_owned()),
            "types.rasm",
        );
    }

    #[test]
    fn rename_match() {
        test_rename(
            "resources/test/enums.rasm",
            "aName",
            17,
            12,
            Err("Rename of builtin function.".to_owned()),
            "enums.rasm",
        );
    }

    #[test]
    fn rename_struct_property() {
        test_rename(
            "resources/test/types.rasm",
            "aName",
            12,
            20,
            Ok(vec![(2, 5, 5), (12, 17, 5), (36, 17, 5)]),
            "types.rasm",
        );
    }

    #[test]
    fn rename_struct_by_constructor() {
        test_rename(
            "resources/test/types.rasm",
            "aName",
            6,
            19,
            Ok(vec![
                (1, 8, 7),
                (6, 15, 7),
                (19, 23, 7),
                (27, 27, 7),
                (31, 26, 7),
                (31, 43, 7),
                (36, 5, 7),
            ]),
            "types.rasm",
        );
    }

    #[test]
    fn rename_struct_by_constructor_with_sef_ref() {
        test_rename(
            "resources/test/typesselfref.rasm",
            "aName",
            6,
            9,
            Ok(vec![(1, 8, 7), (2, 20, 7), (6, 9, 7)]),
            "typesselfref.rasm",
        );
    }

    #[test]
    fn rename_in_multiple_modules() {
        test_rename_with_module_ns(
            "../rasm/resources/examples/breakout",
            "aName",
            216,
            30,
            Ok(vec![
                ("breakout_breakout".to_owned(), 173, 12, 9),
                ("breakout_breakout".to_owned(), 183, 16, 9),
                ("breakout_breakout".to_owned(), 216, 26, 9),
                ("breakout_game".to_owned(), 32, 26, 9),
                ("breakout_game".to_owned(), 50, 22, 9),
                ("breakout_game".to_owned(), 86, 44, 9),
                ("breakout_game".to_owned(), 92, 40, 9),
                ("breakout_menu".to_owned(), 5, 26, 9),
                ("breakout_menu".to_owned(), 15, 44, 9),
            ]),
            "src/main/rasm/breakout.rasm",
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

    #[test]
    fn test_statement_start_position_simple() {
        init_minimal_log();

        let (project, helper) = get_helper("resources/test/simple.rasm");

        if let Some((_, _, info)) = project.get_module(
            Path::new("resources/test/simple.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(3, 13),
            );

            reset_indent!();

            if let Some(i) = helper.statement_start_position(&index) {
                assert_eq!(ASTPosition::new(3, 1), i);
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
        }
    }

    #[test]
    fn test_statement_start_position_lambda() {
        let (project, helper) = get_helper("resources/test/references.rasm");

        if let Some((_, _, info)) = project.get_module(
            Path::new("resources/test/references.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(3, 13),
            );
            if let Some(i) = helper.statement_start_position(&index) {
                assert_eq!(ASTPosition::new(3, 5), i);
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
        }
    }

    #[test]
    fn test_statement_start_position_dot_notation() {
        let (project, helper) = get_helper("resources/test/references.rasm");

        if let Some((_, _, info)) = project.get_module(
            Path::new("resources/test/references.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(2, 10),
            );
            if let Some(i) = helper.statement_start_position(&index) {
                assert_eq!(ASTPosition::new(1, 1), i);
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
        }
    }

    #[test]
    fn test_statement_start_position_corner_case1() {
        let (project, helper) = get_helper("resources/test/statement_start_position.rasm");

        if let Some((_, _, info)) = project.get_module(
            Path::new("resources/test/statement_start_position.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(6, 4),
            );
            if let Some(i) = helper.statement_start_position(&index) {
                assert_eq!(ASTPosition::new(3, 1), i);
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
        }
    }

    #[test]
    fn test_statement_start_position_corner_case2() {
        let (project, helper) = get_helper("resources/test/statement_start_position.rasm");

        if let Some((_, _, info)) = project.get_module(
            Path::new("resources/test/statement_start_position.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(1, 9),
            );
            if let Some(i) = helper.statement_start_position(&index) {
                assert_eq!(ASTPosition::new(1, 1), i);
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
        }
    }

    #[test]
    fn test_statement_start_position_in_function() {
        let (project, helper) = get_helper("resources/test/statement_start_position.rasm");

        if let Some((_, _, info)) = project.get_module(
            Path::new("resources/test/statement_start_position.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(17, 13),
            );
            if let Some(i) = helper.statement_start_position(&index) {
                assert_eq!(ASTPosition::new(17, 5), i);
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
        }
    }

    #[test]
    fn test_statement_start_position_breakout() {
        let (project, helper) = get_helper("../rasm/resources/examples/breakout");

        if let Some((_, _, info)) = project.get_module(
            Path::new("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(325, 31),
            );
            if let Some(i) = helper.statement_start_position(&index) {
                assert_eq!(ASTPosition::new(325, 5), i);
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
        }
    }

    #[test]
    fn test_extract_function_breakout() {
        let (project, helper) = get_helper("../rasm/resources/examples/breakout");

        if let Some((_, _, info)) = project.get_module(
            Path::new("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let start_index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(255, 13),
            );
            let end_index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(257, 93),
            );
            let code = "let newHighScores = highScores.add(score);
            writeHighScores(newHighScores);
            State(resources, newKeys, Stage::Menu(MenuState(newHighScores)), newHighScores);";
            if let Some(mut result) =
                helper.try_extract_function(code, &start_index, &end_index, ASTPosition::new(0, 0))
            {
                let edit = result.remove(0);
                assert_eq!("\n\nfn newFunction(highScores: Vec<HighScore>, score: i32, resources: Resources, newKeys: Vec<i32>) -> State {
let newHighScores = highScores.add(score);
            writeHighScores(newHighScores);
            State(resources, newKeys, Stage::Menu(MenuState(newHighScores)), newHighScores);
}", edit.text);
                let edit = result.remove(0);
                assert_eq!(
                    "newFunction(highScores, score, resources, newKeys);",
                    edit.text
                );
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
        }
    }

    #[test]
    fn test_extract_function_breakout_1() {
        let (project, helper) = get_helper("../rasm/resources/examples/breakout");

        if let Some((_, _, info)) = project.get_module(
            Path::new("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let start_index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(257, 39),
            );
            let end_index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(257, 76),
            );
            let code = "Stage::Menu(MenuState(newHighScores))";
            if let Some(mut result) =
                helper.try_extract_function(code, &start_index, &end_index, ASTPosition::new(0, 0))
            {
                let edit = result.remove(0);
                assert_eq!(
                    "\n\nfn newFunction(newHighScores: Vec<HighScore>) -> Stage {
Stage::Menu(MenuState(newHighScores));
}",
                    edit.text
                );
                let edit = result.remove(0);
                assert_eq!("newFunction(newHighScores)", edit.text);
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
        }
    }

    #[test]
    fn test_extract_function_breakout_2() {
        let (project, helper) = get_helper("../rasm/resources/examples/breakout");

        if let Some((_, _, info)) = project.get_module(
            Path::new("../rasm/resources/examples/breakout/src/main/rasm/breakout.rasm"),
            &CompileTarget::C(COptions::default()),
        ) {
            let start_index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(257, 13),
            );
            let end_index = ASTIndex::new(
                info.module_namespace(),
                info.module_id(),
                ASTPosition::new(257, 92),
            );
            let code =
                "State(resources, newKeys, Stage::Menu(MenuState(newHighScores)), newHighScores);";
            if let Some(mut result) =
                helper.try_extract_function(code, &start_index, &end_index, ASTPosition::new(0, 0))
            {
                let edit = result.remove(0);
                assert_eq!(
                    "\n\nfn newFunction(resources: Resources, newKeys: Vec<i32>, newHighScores: Vec<HighScore>) -> State {
State(resources, newKeys, Stage::Menu(MenuState(newHighScores)), newHighScores);
}", edit.text
                );
                let edit = result.remove(0);
                assert_eq!("newFunction(resources, newKeys, newHighScores);", edit.text);
            } else {
                panic!("Cannot find statement.");
            }
        } else {
            panic!("Cannot find module.");
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
        let (project, helper) = get_helper(file);

        let edits = helper.rename(
            &get_index(&project, file_name, row, column),
            new_name.to_owned(),
        );

        let found = edits.map(|it| {
            let mut f = it
                .into_iter()
                .map(|it| {
                    (
                        it.from.position().row,
                        it.from.position().column,
                        it.to.position().column - it.from.position().column,
                    )
                })
                .collect::<Vec<_>>();
            f.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
            f
        });

        assert_eq!(expected, found);
    }

    fn test_rename_with_module_ns(
        file: &str,
        new_name: &str,
        row: usize,
        column: usize,
        expected: Result<Vec<(String, usize, usize, usize)>, String>,
        file_name: &str,
    ) {
        let (project, helper) = get_helper(file);

        let edits = helper.rename(
            &get_index(&project, file_name, row, column),
            new_name.to_owned(),
        );

        let found = edits.map(|it| {
            let mut f = it
                .into_iter()
                .map(|it| {
                    (
                        format!("{}", it.from.module_namespace()),
                        it.from.position().row,
                        it.from.position().column,
                        it.to.position().column - it.from.position().column,
                    )
                })
                .collect::<Vec<_>>();
            f.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1).then(a.2.cmp(&b.2))));
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
        builtin: Option<ASTBuiltinFunctionType>,
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

        let mut container = ASTModulesContainer::new();
        container.add(
            module,
            module_info.namespace().clone(),
            module_info.id().clone(),
            true,
            false,
        );

        let helper = IDEHelper::from_container(container);

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
