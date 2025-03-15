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
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::process::{exit, Command, Stdio};
use std::time::Instant;
use walkdir::WalkDir;

use rust_embed::{EmbeddedFile, RustEmbed};
use toml::Value;

use crate::codegen::backend::{log_command, Backend, BackendNasmi386};
use crate::codegen::c::any::CInclude;
use crate::codegen::c::code_gen_c::CodeGenC;
use crate::codegen::c::functions_creator::CFunctionsCreator;
use crate::codegen::c::options::COptions;
use crate::codegen::enh_ast::{
    EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind,
};
use crate::codegen::enh_val_context::EnhValContext;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{TextMacro, TextMacroEvaluator};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_typed_module, AsmOptions, CodeGen, CodeGenAsm};
use crate::commandline::{CommandLineAction, CommandLineOptions};
use crate::errors::CompilationError;
use crate::project::{RasmProject, RasmProjectRunType};
use crate::transformations::functions_creator::{FunctionsCreator, FunctionsCreatorNasmi386};
use crate::transformations::typed_functions_creator::{
    TypedFunctionsCreator, TypedFunctionsCreatorNasmi386,
};

use crate::enh_type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedModule, DefaultFunction, DefaultFunctionCall,
};
use crate::type_check::ast_type_checker::ASTTypeChecker;

use super::c::typed_function_creator::TypedFunctionsCreatorC;

#[derive(RustEmbed)]
#[folder = "../core/resources/corelib/nasmi386"]
struct Nasmi386CoreLibAssets;

#[derive(RustEmbed)]
#[folder = "../core/resources/corelib/c"]
struct CLibAssets;

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

    fn generate(&self, statics: Statics, typed_module: &ASTTypedModule, debug: bool) -> String {
        match self {
            CompileTarget::Nasmi386(options) => {
                CodeGenAsm::new(options.clone(), debug).generate(typed_module, statics)
            }
            CompileTarget::C(options) => {
                CodeGenC::new(options.clone(), debug).generate(typed_module, statics)
            }
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
                    debug,
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
                Box::new(TypedFunctionsCreatorNasmi386::new(backend, code_gen, debug))
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

        let out = if let Some(o) = command_line_options.out.clone() {
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
            CompileTarget::C(options) => {
                let start = Instant::now();

                let out_path = Path::new(&out);
                let parent_path = out_path.parent().unwrap();

                let mut source_files_to_include = Vec::new();

                project
                    .get_all_dependencies()
                    .iter()
                    .for_each(|dependency| {
                        if let Some(native_source_folder) =
                            dependency.main_native_source_folder(self.folder())
                        {
                            if native_source_folder.exists() {
                                WalkDir::new(native_source_folder)
                                    .into_iter()
                                    .filter_map(Result::ok)
                                    .filter(|it| it.file_name().to_string_lossy().ends_with(".h"))
                                    .for_each(|it| {
                                        CInclude::add_to_statics(
                                            &mut statics,
                                            format!(
                                                "\"{}\"",
                                                it.clone().file_name().to_string_lossy()
                                            ),
                                        );

                                        let dest = parent_path.to_path_buf().join(Path::new(
                                            it.file_name().to_string_lossy().as_ref(),
                                        ));

                                        info!("including file {}", it.path().to_string_lossy());

                                        fs::copy(it.clone().into_path(), dest).unwrap();
                                    });
                            }
                        }
                    });

                CLibAssets::iter()
                    .filter(|it| it.ends_with(".c") || it.ends_with(".h"))
                    .for_each(|it| {
                        let dest = parent_path.to_path_buf().join(it.to_string());
                        info!("Including {}", dest.to_string_lossy());
                        if it.ends_with(".h") {
                            CInclude::add_to_statics(&mut statics, format!("\"{it}\""));
                        } else {
                            source_files_to_include.push(dest.to_string_lossy().to_string());
                        }
                        if let Some(asset) = CLibAssets::get(&it) {
                            fs::write(dest, asset.data).unwrap();
                        } else {
                            panic!()
                        }
                    });

                let native_code = self.generate(statics, &typed_module, command_line_options.debug);

                File::create(out_path)
                    .unwrap_or_else(|_| panic!("cannot create file {}", out_path.to_str().unwrap()))
                    .write_all(native_code.as_bytes())
                    .unwrap();

                info!("code generation ended in {:?}", start.elapsed());

                let start = Instant::now();
                info!("source file: {}", out_path.to_string_lossy());
                let mut command = Command::new("gcc");

                let mut args = vec![out_path.with_extension("c").to_string_lossy().to_string()];
                args.append(&mut source_files_to_include);
                args.append(&mut vec![
                    "-std=c17".to_string(),
                    "-o".to_string(),
                    out_path.with_extension("").to_string_lossy().to_string(),
                    "-Wno-incompatible-pointer-types".to_string(), // TODO it happens where using $typeName(T) for a reference type, I think it should be struct RasmPointer_ *
                    "-Wno-int-conversion".to_string(),
                ]);
                if command_line_options.release {
                    args.push("-O3".to_string());
                } else {
                    args.push("-g".to_string());
                    args.push("-O0".to_string());
                }

                if !options.includes.is_empty() {
                    for inc in options.includes.iter() {
                        args.push(format!("-I{inc}"));
                    }
                }

                if !options.requires.is_empty() {
                    for req in options.requires.iter() {
                        args.push(format!("-l{req}"));
                    }
                }

                command.args(args);

                log_command(&command);
                let result = command
                    .stderr(Stdio::inherit())
                    .output()
                    .expect("failed to execute gcc");
                info!("compiler ended in {:?}", start.elapsed());

                if !result.status.success() {
                    panic!("Error running gcc")
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
