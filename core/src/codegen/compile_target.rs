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

use linked_hash_map::LinkedHashMap;
use log::info;
use rasm_utils::OptionDisplay;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::exit;
use std::time::Instant;

use rust_embed::{EmbeddedFile, RustEmbed};
use toml::Value;

use crate::codegen::asm::backend::Backend;
use crate::codegen::c::c_compiler::compile_c;
use crate::codegen::c::code_gen_c::CodeGenC;
use crate::codegen::c::functions_creator_c::CFunctionsCreator;
use crate::codegen::c::options::COptions;
use crate::codegen::enh_ast::{
    EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind,
};
use crate::codegen::enh_val_context::EnhValContext;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{TextMacro, TextMacroEvaluator};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_typed_module, AsmOptions, CodeGen};
use crate::commandline::{CommandLineAction, CommandLineOptions};
use crate::errors::CompilationError;
use crate::project::{RasmProject, RasmProjectRunType};
use crate::transformations::functions_creator::{FunctionsCreator, FunctionsCreatorNasmi386};
use crate::transformations::typed_functions_creator::TypedFunctionsCreator;

use crate::enh_type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedModule, DefaultFunction, DefaultFunctionCall,
};
use crate::type_check::ast_type_checker::ASTTypeChecker;

use super::asm::backend::BackendNasmi386;
use super::asm::code_gen_asm::CodeGenAsm;
use super::asm::typed_functions_creator_asm::TypedFunctionsCreatorNasmi386;
use super::c::c_compiler::CLibAssets;
use super::c::typed_function_creator_c::TypedFunctionsCreatorC;

#[derive(RustEmbed)]
#[folder = "../core/resources/corelib/nasmi386"]
struct Nasmi386CoreLibAssets;

#[derive(Clone)]
pub enum CompileTarget {
    Nasmi386(AsmOptions),
    C(COptions),
}

pub const NASMI386: &str = "nasmi386";
pub const C: &str = "c";

impl CompileTarget {
    pub fn from(
        target: String,
        project: &RasmProject,
        _command_line_options: &CommandLineOptions,
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
                    requires,
                    externals,
                    ..AsmOptions::default()
                };

                CompileTarget::Nasmi386(options)
            }
            C => {
                let mut all_projects = vec![project.clone()];
                all_projects.extend(project.get_all_dependencies());

                let requires = get_native_string_array(&all_projects, target.as_str(), "requires");
                let includes = get_native_string_array(&all_projects, target.as_str(), "includes");

                let options = COptions { requires, includes };

                CompileTarget::C(options)
            }
            _ => {
                panic!("Unknown target {target}");
            }
        }
    }

    pub fn extension(&self) -> String {
        match self {
            CompileTarget::Nasmi386(_) => "asm".to_string(),
            CompileTarget::C(_) => "c".to_string(),
        }
    }

    fn generate(
        &self,
        project: &RasmProject,
        statics: Statics,
        typed_module: &ASTTypedModule,
        cmd_line_options: &CommandLineOptions,
        out_folder: &Path,
    ) -> Vec<(String, String)> {
        match self {
            CompileTarget::Nasmi386(options) => {
                CodeGenAsm::new(options.clone(), cmd_line_options.debug).generate(
                    project,
                    &self,
                    typed_module,
                    statics,
                    cmd_line_options,
                    out_folder,
                )
            }
            CompileTarget::C(options) => CodeGenC::new(options.clone(), cmd_line_options.debug)
                .generate(
                    project,
                    &self,
                    typed_module,
                    statics,
                    cmd_line_options,
                    out_folder,
                ),
        }
    }

    pub fn folder(&self) -> &str {
        match self {
            CompileTarget::Nasmi386(_) => NASMI386,
            CompileTarget::C(_) => C,
        }
    }

    pub fn functions_creator(&self, debug: bool) -> Box<dyn FunctionsCreator> {
        match self {
            CompileTarget::Nasmi386(options) => {
                let backend = BackendNasmi386::new(debug);
                Box::new(FunctionsCreatorNasmi386::new(
                    backend.clone(),
                    CodeGenAsm::new(options.clone(), debug),
                ))
            }
            CompileTarget::C(_) => Box::new(CFunctionsCreator::new()),
        }
    }

    pub fn typed_functions_creator(&self, debug: bool) -> Box<dyn TypedFunctionsCreator> {
        match self {
            CompileTarget::Nasmi386(options) => {
                let backend = BackendNasmi386::new(debug);
                let code_gen = CodeGenAsm::new(options.clone(), debug);
                Box::new(TypedFunctionsCreatorNasmi386::new(backend, code_gen))
            }
            CompileTarget::C(options) => {
                let code_gen = CodeGenC::new(options.clone(), debug);
                Box::new(TypedFunctionsCreatorC::new(code_gen))
            }
        }
    }

    pub fn get_core_lib_files(&self) -> LinkedHashMap<String, EmbeddedFile> {
        let mut result = LinkedHashMap::new();

        match self {
            CompileTarget::Nasmi386(_) => {
                Nasmi386CoreLibAssets::iter()
                    .filter(|it| it.ends_with(".rasm"))
                    .for_each(|it| {
                        if let Some(asset) = Nasmi386CoreLibAssets::get(&it) {
                            result.insert(it.to_string(), asset);
                        }
                    });
            }
            CompileTarget::C(_) => {
                CLibAssets::iter()
                    .filter(|it| it.ends_with(".rasm"))
                    .for_each(|it| {
                        if let Some(asset) = CLibAssets::get(&it) {
                            result.insert(it.to_string(), asset);
                        }
                    });
            }
        }

        result
    }

    pub fn get_evaluator(&self, debug: bool) -> TextMacroEvaluator {
        match self {
            CompileTarget::Nasmi386(options) => {
                let code_gen = CodeGenAsm::new(options.clone(), debug);
                code_gen.get_text_macro_evaluator()
            }
            CompileTarget::C(options) => {
                let code_gen = CodeGenC::new(options.clone(), debug);
                code_gen.get_text_macro_evaluator()
            }
        }
    }

    pub fn called_functions(
        &self,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&EnhASTFunctionDef>,
        body: &str,
        context: &EnhValContext,
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
            CompileTarget::C(options) => {
                let code_gen = CodeGenC::new(options.clone(), debug);
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
            CompileTarget::C(_) => vec![CommandLineAction::Build, CommandLineAction::Test],
        }
    }

    pub fn run(&self, project: RasmProject, command_line_options: CommandLineOptions) {
        if !self
            .supported_actions()
            .contains(&command_line_options.action)
        {
            panic!("unsupported action '{}'", command_line_options.action)
        }

        let out_folder = if let Some(o) = command_line_options.out.clone() {
            Path::new(&o).to_path_buf()
        } else {
            project.out_folder()
        };

        if !out_folder.exists() {
            panic!(
                "out folder does not exist: {}",
                out_folder.to_string_lossy()
            );
        } else {
            info!("out folder: {}", out_folder.to_string_lossy());
        }

        let start = Instant::now();

        let mut statics = Statics::new();

        let run_type = if command_line_options.action == CommandLineAction::Test {
            RasmProjectRunType::Test
        } else {
            RasmProjectRunType::Main
        };

        let (modules, errors) = project.get_all_modules(
            &mut statics,
            &run_type,
            self,
            command_line_options.debug,
            &command_line_options,
        );

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            panic!()
        }

        let mut statics_for_cc = Statics::new();

        let (container, catalog, errors) = project.container_and_catalog(
            &mut statics_for_cc,
            &run_type,
            self,
            command_line_options.debug,
            &command_line_options,
        );

        info!("parse ended in {:?}", start.elapsed());

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            panic!()
        }
        let start = Instant::now();

        let (ast_type_check, _) = ASTTypeChecker::from_modules_container(&container);

        info!("AST type check ended in {:?}", start.elapsed());

        let start = Instant::now();

        let (enhanced_ast_module, errors) = EnhancedASTModule::from_ast(
            modules,
            &project,
            &mut statics,
            self,
            command_line_options.debug,
            true,
        );

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            panic!()
        }

        let typed_module = get_typed_module(
            enhanced_ast_module,
            command_line_options.print_memory,
            command_line_options.print_code,
            &mut statics,
            self,
            command_line_options.debug,
            ast_type_check,
            &catalog,
            &container,
        )
        .unwrap_or_else(|e| Self::raise_error(e));

        info!("type check ended in {:?}", start.elapsed());

        let start = Instant::now();

        let native_codes = self.generate(
            &project,
            statics,
            &typed_module,
            &command_line_options,
            &out_folder,
        );

        let mut out_paths = Vec::new();

        for (native_file, native_code) in native_codes.iter() {
            let out_path = out_folder.as_path().join(&native_file);
            File::create(&out_path)
                .unwrap_or_else(|_| panic!("cannot create file {}", out_path.to_str().unwrap()))
                .write_all(native_code.as_bytes())
                .unwrap();

            out_paths.push(out_path);
        }

        info!("code generation ended in {:?}", start.elapsed());

        match self {
            CompileTarget::Nasmi386(options) => match command_line_options.action {
                CommandLineAction::Build | CommandLineAction::Test => {
                    if out_paths.len() != 1 {
                        panic!("Only one native file to compile is supported!");
                    }

                    let backend = BackendNasmi386::new(command_line_options.debug);

                    let out = out_paths.remove(0);

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
            CompileTarget::C(options) => {
                let start = Instant::now();

                let result = compile_c(
                    &command_line_options,
                    options,
                    &project,
                    &out_folder,
                    out_paths,
                );

                info!("compiler ended in {:?}", start.elapsed());

                if !result.status.success() {
                    panic!("Error running native compiler")
                }
            }
        }
    }

    fn raise_error(error: CompilationError) -> ! {
        /*
        if let CompilationErrorKind::TypeCheck(a, b) = &error.error_kind {
            let important = b.iter().flat_map(|it| it.important()).collect::<Vec<_>>();
            if let Some(e) = important.first() {
                let index = e.main.0.clone();
                let message = e.main.1.clone();

                eprintln!("{message} : {index}");
                eprintln!("error: could not compile due to previous errors");
                exit(-1);
            }
        }
        */
        eprintln!("{error}");
        eprintln!("error: could not compile due to previous errors");
        exit(-1);
    }

    pub fn get_mandatory_functions(&self, module: &EnhancedASTModule) -> Vec<DefaultFunction> {
        let mut result = Vec::new();

        match self {
            CompileTarget::Nasmi386(_) => {
                for def in &module.types {
                    if !def.type_parameters.is_empty() {
                        let name = format!("{}References", def.name);
                        result.push(DefaultFunction::new_2(
                            &name,
                            EnhBuiltinTypeKind::I32,
                            EnhBuiltinTypeKind::I32,
                        ));
                    }
                }
            }
            CompileTarget::C(_) => {
                /*
                for type_def in &module.types {
                    if type_def.is_ref {
                        let suffixes = vec!["Deref", "AddRef"];

                        for suffix in suffixes {
                            let name = format!("{}{suffix}", type_def.name);
                            result.push(DefaultFunction::new_1(&name, BuiltinTypeKind::I32));
                        }
                    }
                }
                */
                //result.push(DefaultFunction::new_1("rasmMalloc", BuiltinTypeKind::I32));
            }
        }

        result
    }

    pub fn get_default_functions(&self, print_allocation: bool) -> Vec<DefaultFunction> {
        let mut default_functions = match self {
            CompileTarget::Nasmi386(_) => {
                vec![
                    DefaultFunction::new_2(
                        "rasmalloc",
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::String,
                    ),
                    DefaultFunction::new_1("exitMain", EnhBuiltinTypeKind::I32),
                    DefaultFunction::new_2(
                        "addRef",
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::String,
                    ),
                    DefaultFunction::new_3(
                        "memcopy",
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::I32,
                    ),
                    DefaultFunction::new_2(
                        "deref",
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::String,
                    ),
                    DefaultFunction::new_1("addStaticStringToHeap", EnhBuiltinTypeKind::I32),
                    DefaultFunction::new_2(
                        "createCmdLineArguments",
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::I32,
                    ),
                    DefaultFunction::new_1("str_addRef", EnhBuiltinTypeKind::String),
                    DefaultFunction::new_1("str_deref", EnhBuiltinTypeKind::String),
                    DefaultFunction::new_3(
                        "addStaticAllocation",
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::I32,
                    ),
                    DefaultFunction::new_3(
                        "addHeap",
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::I32,
                        EnhBuiltinTypeKind::I32,
                    ),
                ]
            }
            CompileTarget::C(_) => vec![
                DefaultFunction {
                    name: "addRef".to_string(),
                    param_types: vec![EnhASTType::Custom {
                        namespace: EnhASTNameSpace::global(),
                        name: "RasmPointer".to_string(),
                        param_types: vec![],
                        index: EnhASTIndex::none(),
                    }],
                },
                DefaultFunction {
                    name: "deref".to_string(),
                    param_types: vec![EnhASTType::Custom {
                        namespace: EnhASTNameSpace::global(),
                        name: "RasmPointer".to_string(),
                        param_types: vec![],
                        index: EnhASTIndex::none(),
                    }],
                },
                /*
                DefaultFunction::new_0("initRasmReferences"),
                */
                DefaultFunction::new_0("freeReferences"),
            ],
        };

        if print_allocation {
            default_functions.append(&mut vec![
                DefaultFunction::new_0("printAllocated"),
                DefaultFunction::new_0("printTableSlotsAllocated"),
            ])
        }

        default_functions.sort_by(|a, b| a.name.cmp(&b.name));
        default_functions
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
