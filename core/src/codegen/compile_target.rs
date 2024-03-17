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

use log::info;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::time::Instant;

use rust_embed::{EmbeddedFile, RustEmbed};
use toml::Value;

use crate::codegen::backend::{Backend, BackendNasmi386};
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{TextMacro, TextMacroEvaluator};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::val_context::ValContext;
use crate::codegen::{get_typed_module, AsmOptions, CodeGen, CodeGenAsm};
use crate::commandline::{CommandLineAction, CommandLineOptions};
use crate::parser::ast::ASTFunctionDef;
use crate::project::RasmProject;
use crate::transformations::functions_creator::{FunctionsCreator, FunctionsCreatorNasmi386};
use crate::transformations::typed_functions_creator::{
    TypedFunctionsCreator, TypedFunctionsCreatorNasmi386,
};
use crate::type_check::typed_ast::{ASTTypedFunctionDef, ASTTypedModule, DefaultFunctionCall};
use crate::utils::OptionDisplay;

#[derive(RustEmbed)]
#[folder = "../core/resources/corelib/nasmi386"]
struct Nasmi386CoreLibAssets;

#[derive(Clone)]
pub enum CompileTarget {
    Nasmi386(AsmOptions),
}

pub const NASMI386: &str = "nasmi386";

impl CompileTarget {
    pub fn from(
        target: String,
        project: &RasmProject,
        command_line_options: &CommandLineOptions,
    ) -> Self {
        match target.as_str() {
            NASMI386 => {
                let mut all_projects = vec![project.clone()];
                all_projects.extend(project.get_all_dependencies());
                let mut requires =
                    get_native_string_array(&all_projects, target.as_str(), "requires");
                requires.push("libc".to_string());

                requires.sort();
                requires.dedup();

                let mut externals =
                    get_native_string_array(&all_projects, target.as_str(), "externals");
                externals.sort();
                externals.dedup();

                let options = AsmOptions {
                    print_memory: command_line_options.print_memory,
                    requires,
                    externals,
                    ..AsmOptions::default()
                };

                CompileTarget::Nasmi386(options)
            }
            _ => {
                panic!("Unknown target {target}");
            }
        }
    }

    pub fn extension(&self) -> String {
        match self {
            CompileTarget::Nasmi386(_) => "asm".to_string(),
        }
    }

    fn generate(&self, statics: Statics, typed_module: &ASTTypedModule, debug: bool) -> String {
        match self {
            CompileTarget::Nasmi386(options) => {
                CodeGenAsm::new(options.clone(), debug).generate(typed_module, statics)
            }
        }
    }

    pub fn folder(&self) -> &str {
        match self {
            CompileTarget::Nasmi386(_) => NASMI386,
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

    pub fn supported_actions(&self) -> Vec<CommandLineAction> {
        match self {
            CompileTarget::Nasmi386(_) => {
                vec![CommandLineAction::Build, CommandLineAction::Test]
            }
        }
    }

    pub fn run(&self, project: RasmProject, command_line_options: CommandLineOptions) {
        if !self
            .supported_actions()
            .contains(&command_line_options.action)
        {
            panic!("unsupported action '{}'", command_line_options.action)
        }

        let out = if let Some(o) = command_line_options.out {
            Path::new(&o).to_path_buf()
        } else {
            project
                .out_file(command_line_options.action == CommandLineAction::Test)
                .expect("undefined out in rasm.toml")
        }
        .with_extension(self.extension());

        info!("out: {}", out.with_extension("").to_string_lossy());

        let start = Instant::now();

        let mut statics = Statics::new();

        let (modules, errors) = project.get_all_modules(
            &mut statics,
            command_line_options.action == CommandLineAction::Test,
            &self,
            command_line_options.debug,
        );

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            panic!()
        }

        let enhanced_ast_module = EnhancedASTModule::new(
            modules,
            &project,
            &mut statics,
            self,
            command_line_options.debug,
        );

        info!("parse ended in {:?}", start.elapsed());

        let start = Instant::now();

        let typed_module = get_typed_module(
            enhanced_ast_module,
            command_line_options.print_memory,
            command_line_options.print_code,
            &mut statics,
            self,
            command_line_options.debug,
        )
        .unwrap_or_else(|e| {
            panic!("{e}");
        });

        info!("type check ended in {:?}", start.elapsed());

        match self {
            CompileTarget::Nasmi386(options) => match command_line_options.action {
                CommandLineAction::Build | CommandLineAction::Test => {
                    let start = Instant::now();

                    let native_code =
                        self.generate(statics, &typed_module, command_line_options.debug);

                    info!("code generation ended in {:?}", start.elapsed());

                    let out_path = Path::new(&out);
                    File::create(out_path)
                        .unwrap_or_else(|_| {
                            panic!("cannot create file {}", out_path.to_str().unwrap())
                        })
                        .write_all(native_code.as_bytes())
                        .unwrap();

                    let backend = BackendNasmi386::new(command_line_options.debug);

                    if command_line_options.only_compile {
                        backend.compile(&out);
                    } else {
                        backend.compile_and_link(&out, &options.requires);
                    }
                }
                _ => {
                    unreachable!()
                }
            },
        }
    }
}

fn get_native_string_array(projects: &[RasmProject], native: &str, key: &str) -> Vec<String> {
    let mut result = projects
        .iter()
        .flat_map(|it| {
            if let Some(ref natives) = it.config.natives {
                if let Some(nasm_i386_value) = natives.get(native) {
                    if let Value::Table(nasm_i386_table) = nasm_i386_value {
                        if let Some(value) = nasm_i386_table.get(key) {
                            if let Value::Array(a) = value {
                                a.iter().map(|req| {
                                    if let Value::String(s) = req {
                                        s.clone()
                                    } else {
                                        panic!(
                                            "{native}/{key} should be an array of strings {}/rasm.toml",
                                            it.root.to_string_lossy()
                                        );
                                    }
                                }).collect::<Vec<_>>()
                            } else {
                                panic!(
                                    "{native}/{key} should be an array in {}/rasm.toml, but is {}",
                                    it.root.to_string_lossy(),
                                    OptionDisplay(&nasm_i386_table.get(key))
                                );
                            }
                        } else {
                            Vec::new()
                        }
                    } else {
                        panic!(
                            "{native} should be a table in {}/rasm.toml",
                            it.root.to_string_lossy()
                        );
                    }
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            }
        })
        .collect::<Vec<_>>();

    result.sort();
    result.dedup_by(|s1, s2| s1 == s2);

    result
}
