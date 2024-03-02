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

use std::collections::HashMap;

use linked_hash_map::LinkedHashMap;
use log::debug;
use rust_embed::{EmbeddedFile, RustEmbed};

use crate::codegen::backend::Backend;
use crate::codegen::backend::BackendAsm;
use crate::codegen::backend::BackendNasmi386;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{
    AddRefMacro, CCallTextMacroEvaluator, CallTextMacroEvaluator, MacroParam, PrintRefMacro,
    TextMacro, TextMacroEval, TextMacroEvaluator,
};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::val_context::ValContext;
use crate::codegen::{CodeGen, CodeGenAsm, CodeGenOptions, ValKind};
use crate::debug_i;
use crate::parser::ast::{ASTFunctionDef, ASTType, BuiltinTypeKind};
use crate::transformations::functions_creator::{FunctionsCreator, FunctionsCreatorNasmi386};
use crate::transformations::typed_functions_creator::{
    TypedFunctionsCreator, TypedFunctionsCreatorNasmi386,
};
use crate::type_check::typed_ast::{ASTTypedFunctionDef, ASTTypedModule, DefaultFunctionCall};

#[derive(RustEmbed)]
#[folder = "../core/resources/corelib/nasmi386"]
struct Nasmi386CoreLibAssets;

#[derive(Clone)]
pub enum CompileTarget {
    Nasmi36,
}

impl CompileTarget {
    pub fn extension(&self) -> String {
        match self {
            CompileTarget::Nasmi36 => "asm".to_string(),
        }
    }

    pub fn generate(
        &self,
        statics: Statics,
        typed_module: ASTTypedModule,
        options: CodeGenOptions,
    ) -> String {
        match self {
            CompileTarget::Nasmi36 => {
                let backend = BackendNasmi386::new(options.debug);

                CodeGenAsm::new(typed_module, Box::new(backend), options, self.clone())
                    .generate(statics)
            }
        }
    }

    pub fn folder(&self) -> &str {
        match self {
            CompileTarget::Nasmi36 => "nasmi386",
        }
    }

    pub fn functions_creator(&self, debug: bool) -> impl FunctionsCreator {
        FunctionsCreatorNasmi386::new(BackendNasmi386::new(debug), debug)
    }

    pub fn typed_functions_creator(&self, debug: bool) -> impl TypedFunctionsCreator {
        TypedFunctionsCreatorNasmi386::new(BackendNasmi386::new(debug), debug)
    }

    pub fn get_core_lib_files(&self) -> HashMap<String, EmbeddedFile> {
        let mut result = HashMap::new();
        Nasmi386CoreLibAssets::iter()
            .filter(|it| it.ends_with(".rasm"))
            .for_each(|it| {
                if let Some(asset) = Nasmi386CoreLibAssets::get(&it) {
                    result.insert(it.to_string(), asset);
                }
            });
        result
    }

    pub fn call_function_simple(&self, out: &mut String, function_name: &str) {
        self.add(out, &format!("call    {}", function_name), None, true);
    }

    pub fn call_function(
        &self,
        out: &mut String,
        function_name: &str,
        args: &[(&str, Option<&str>)],
        comment: Option<&str>,
        debug: bool,
    ) {
        let backend = BackendNasmi386::new(debug);

        if let Some(c) = comment {
            self.add_comment(out, c, true);
        }

        for (arg, comment) in args.iter().rev() {
            if let Some(c) = comment {
                self.add_comment(out, c, true);
            }
            self.add(
                out,
                &format!("push {} {arg}", backend.word_size()),
                None, //*comment,
                true,
            );
        }
        self.add(out, &format!("call    {}", function_name), None, true);
        self.add(
            out,
            &format!(
                "add  {}, {}",
                backend.stack_pointer(),
                backend.word_len() * args.len()
            ),
            None,
            true,
        );
    }

    /// the difference with call_function is that arguments are Strings and not &str
    pub fn call_function_owned(
        &self,
        out: &mut String,
        function_name: &str,
        args: &[(String, Option<String>)],
        comment: Option<&str>,
        debug: bool,
    ) {
        self.call_function(
            out,
            function_name,
            &args
                .iter()
                .map(|(arg, comment)| (arg.as_str(), comment.as_deref()))
                .collect::<Vec<_>>(),
            comment,
            debug,
        )
    }

    pub fn add_comment(&self, out: &mut String, comment: &str, indent: bool) {
        self.add(out, &format!("; {comment}"), None, indent);
    }

    pub fn add_rows(&self, out: &mut String, code: Vec<&str>, comment: Option<&str>, indent: bool) {
        if let Some(_cm) = comment {
            self.add(out, "", comment, indent);
        }
        for row in code {
            self.add(out, row, None, indent);
        }
    }
    pub fn add(&self, out: &mut String, code: &str, comment: Option<&str>, indent: bool) {
        if code.is_empty() {
            out.push('\n');
        } else {
            let max = 80;
            let s = format!("{:width$}", code, width = max);
            //assert_eq!(s.len(), max, "{}", s);
            if indent {
                out.push_str("    ");
            }
            out.push_str(&s);
        }

        if let Some(c) = comment {
            if code.is_empty() {
                out.push_str("    ");
            }
            out.push_str("; ");
            out.push_str(c);
        }
        out.push('\n');
    }

    pub fn add_empty_line(&self, out: &mut String) {
        out.push('\n');
    }

    pub fn get_evaluator(&self, debug: bool) -> TextMacroEvaluator {
        let mut evaluators: LinkedHashMap<String, Box<dyn TextMacroEval>> = LinkedHashMap::new();
        let backend = BackendNasmi386::new(debug);

        let call_text_macro_evaluator = CallTextMacroEvaluator::new(Box::new(backend.clone()));
        evaluators.insert("call".into(), Box::new(call_text_macro_evaluator));

        let c_call_text_macro_evaluator = CCallTextMacroEvaluator::new(Box::new(backend.clone()));
        evaluators.insert("ccall".into(), Box::new(c_call_text_macro_evaluator));
        evaluators.insert(
            "addRef".into(),
            Box::new(AddRefMacro::new(Box::new(backend.clone()), false)),
        );
        evaluators.insert(
            "deref".into(),
            Box::new(AddRefMacro::new(Box::new(backend.clone()), true)),
        );
        let print_ref_macro = PrintRefMacro::new(Box::new(backend.clone()), self.clone());
        evaluators.insert("printRef".into(), Box::new(print_ref_macro));

        TextMacroEvaluator::new(evaluators)
    }

    /// Returns the name of the functions called in the code
    ///
    /// # Arguments
    ///
    /// * `body`: the code to scan for function calls
    ///
    /// returns: Vec<String>
    pub fn called_functions(
        &self,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&ASTFunctionDef>,
        body: &str,
        context: &ValContext,
        type_def_provider: &dyn TypeDefProvider,
        _statics: &mut Statics,
        debug: bool,
    ) -> Result<Vec<(TextMacro, DefaultFunctionCall)>, String> {
        let mut result = Vec::new();

        let evaluator = self.get_evaluator(debug);

        for (m, i) in evaluator.get_macros(
            self,
            typed_function_def,
            function_def,
            body,
            type_def_provider,
        )? {
            if m.name == "call" {
                debug_i!("found call macro {m}");
                let types: Vec<ASTType> = m
                    .parameters
                    .iter()
                    .skip(1)
                    .map(|it| {
                        let ast_type = match it {
                            MacroParam::Plain(_, opt_type, _) => match opt_type {
                                None => ASTType::Builtin(BuiltinTypeKind::I32),
                                Some(ast_type) => ast_type.clone(),
                            },
                            MacroParam::StringLiteral(_) => {
                                ASTType::Builtin(BuiltinTypeKind::String)
                            }
                            MacroParam::Ref(name, None, _) => {
                                debug_i!("found ref {name}");
                                match context.get(name.strip_prefix('$').unwrap()).unwrap() {
                                    ValKind::ParameterRef(_, par) => par.ast_type.clone(),
                                    ValKind::LetRef(_, ast_type, _) => ast_type.clone(),
                                }
                            }
                            MacroParam::Ref(name, Some(ast_type), _) => {
                                debug_i!("found ref {name} : {ast_type}");
                                ast_type.clone()
                            }
                        };

                        match &ast_type {
                            ASTType::Generic(name) => {
                                if let Some(f) = typed_function_def {
                                    let t = type_def_provider
                                        .get_type_from_custom_typed_type(
                                            f.generic_types.get(name).unwrap(),
                                        )
                                        .unwrap();
                                    debug_i!("Function specified, found type {:?} for {name}", t);
                                    Ok(t)
                                } else {
                                    debug_i!("Function not specified, cannot find type {name}");
                                    Ok(ast_type.clone())
                                }
                            }
                            ASTType::Custom {
                                namespace: _,
                                name,
                                param_types: _,
                                index: _,
                            } => {
                                let result = if let Some(f) = typed_function_def {
                                    if let Some(t) = f.generic_types.get(name) {
                                        type_def_provider
                                            .get_type_from_typed_type(t)
                                            .ok_or(format!("name {name} t {t}"))
                                    } else if let Some(t) =
                                        type_def_provider.get_type_from_typed_type_name(name)
                                    {
                                        Ok(t)
                                    } else {
                                        Ok(ast_type.clone())
                                    }
                                } else {
                                    Ok(ast_type.clone())
                                };

                                result
                            }
                            _ => Ok(ast_type),
                        }
                    })
                    .collect::<Result<Vec<ASTType>, String>>()?;

                let function_name =
                    if let Some(MacroParam::Plain(function_name, _, _)) = m.parameters.get(0) {
                        function_name
                    } else {
                        return Err(format!("Cannot find function : {i}"));
                    };

                result.push((m.clone(), DefaultFunctionCall::new(function_name, types, i)));
            }
        }
        Ok(result)
    }

    pub fn remove_comments_from_line(&self, line: String) -> String {
        if let Some(pos) = line.find(';') {
            if pos > 0 {
                line.split_at(pos).0.to_string()
            } else {
                String::new()
            }
        } else {
            line
        }
    }
}
