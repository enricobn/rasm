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

use rust_embed::{EmbeddedFile, RustEmbed};

use crate::codegen::backend::BackendNasmi386;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{TextMacro, TextMacroEvaluator};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::val_context::ValContext;
use crate::codegen::{CodeGen, CodeGenAsm, CodeGenOptions};
use crate::parser::ast::ASTFunctionDef;
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
    Nasmi386(CodeGenOptions),
}

impl CompileTarget {
    pub fn extension(&self) -> String {
        match self {
            CompileTarget::Nasmi386(_) => "asm".to_string(),
        }
    }

    pub fn generate(&self, statics: Statics, typed_module: &ASTTypedModule, debug: bool) -> String {
        match self {
            CompileTarget::Nasmi386(options) => {
                CodeGenAsm::new(options.clone(), debug).generate(&typed_module, statics)
            }
        }
    }

    pub fn folder(&self) -> &str {
        match self {
            CompileTarget::Nasmi386(_) => "nasmi386",
        }
    }

    pub fn functions_creator(&self, debug: bool) -> impl FunctionsCreator {
        match self {
            CompileTarget::Nasmi386(options) => {
                let backend = BackendNasmi386::new(debug);
                FunctionsCreatorNasmi386::new(
                    backend.clone(),
                    debug,
                    CodeGenAsm::new(options.clone(), debug),
                )
            }
        }
    }

    pub fn typed_functions_creator(&self, debug: bool) -> impl TypedFunctionsCreator {
        match self {
            CompileTarget::Nasmi386(options) => {
                let backend = BackendNasmi386::new(debug);
                let code_gen = CodeGenAsm::new(options.clone(), debug);
                TypedFunctionsCreatorNasmi386::new(backend, code_gen, debug)
            }
        }
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

    pub fn get_evaluator(&self, debug: bool) -> TextMacroEvaluator {
        match self {
            CompileTarget::Nasmi386(options) => {
                let code_gen = CodeGenAsm::new(options.clone(), debug);
                code_gen.get_evaluator()
            }
        }
    }

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
        match self {
            CompileTarget::Nasmi386(options) => {
                let code_gen = CodeGenAsm::new(options.clone(), debug);
                code_gen.called_functions(
                    typed_function_def,
                    function_def,
                    body,
                    context,
                    type_def_provider,
                    _statics,
                )
            }
        }
    }
}
