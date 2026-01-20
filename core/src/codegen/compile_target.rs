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
use rasm_parser::catalog::modules_catalog::ModulesCatalog;

use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio, exit};
use std::time::Instant;

use crate::codegen::asm::backend::Backend;
use crate::codegen::c::c_compiler::compile_c;
use crate::codegen::c::code_gen_c::CodeGenC;
use crate::codegen::c::functions_creator_c::CFunctionsCreator;
use crate::codegen::c::options::COptions;
use crate::codegen::enh_ast::{
    EnhASTFunctionDef, EnhASTNameSpace, EnhBuiltinTypeKind, EnhModuleId, EnhModuleInfo,
};
use crate::codegen::enh_val_context::EnhValContext;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{TextMacro, TextMacroEvaluator};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{AsmOptions, CodeGen, get_typed_module};
use crate::commandline::{CommandLineAction, CommandLineOptions};
use crate::errors::CompilationError;
use crate::macros::macro_call_extractor::extract_macro_calls;
use crate::macros::macro_compiler::resolve_macros;

use crate::project::RasmProject;
use crate::transformations::enrich_container;
use crate::transformations::functions_creator::{FunctionsCreator, FunctionsCreatorNasmi386};
use crate::transformations::typed_functions_creator::TypedFunctionsCreator;

use crate::enh_type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedModule, DefaultFunction, DefaultFunctionCall,
};
use crate::type_check::ast_modules_container::ASTModulesContainer;
use crate::type_check::ast_type_checker::ASTTypeChecker;

use super::asm::backend::BackendNasmi386;
use super::asm::code_gen_asm::CodeGenAsm;
use super::asm::typed_functions_creator_asm::TypedFunctionsCreatorNasmi386;
use super::c::typed_function_creator_c::TypedFunctionsCreatorC;

#[derive(Clone)]
pub enum CompileTarget {
    Nasmi386(AsmOptions),
    C(COptions),
}

pub const NASMI386: &'static str = "nasmi386";
pub const C: &'static str = "c";

impl CompileTarget {
    pub fn from(
        target: String,
        project: &RasmProject,
        _command_line_options: &CommandLineOptions,
    ) -> Self {
        match target.as_str() {
            NASMI386 => {
                let mut all_projects = vec![project.clone()];
                all_projects.extend(project.dependencies_projects());
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
                all_projects.extend(project.dependencies_projects());

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

    pub fn include_extension(&self) -> String {
        match self {
            CompileTarget::Nasmi386(_) => "h".to_string(), // TODO we don't know, since for now it's not used for nasm
            CompileTarget::C(_) => "h".to_string(),
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
            CompileTarget::Nasmi386(options) => CodeGenAsm::new(
                options.clone(),
                cmd_line_options.debug,
                cmd_line_options.memory_debug,
            )
            .generate(
                project,
                &self,
                typed_module,
                statics,
                cmd_line_options,
                out_folder,
            ),
            CompileTarget::C(options) => CodeGenC::new(
                options.clone(),
                cmd_line_options.debug,
                cmd_line_options.memory_debug,
            )
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

    pub fn functions_creator(&self, debug: bool, memory_debug: bool) -> Box<dyn FunctionsCreator> {
        match self {
            CompileTarget::Nasmi386(options) => {
                let backend = BackendNasmi386::new(memory_debug);
                Box::new(FunctionsCreatorNasmi386::new(
                    backend.clone(),
                    CodeGenAsm::new(options.clone(), debug, memory_debug),
                ))
            }
            CompileTarget::C(_) => Box::new(CFunctionsCreator::new(debug)),
        }
    }

    pub fn typed_functions_creator(
        &self,
        debug: bool,
        memory_debug: bool,
    ) -> Box<dyn TypedFunctionsCreator> {
        match self {
            CompileTarget::Nasmi386(options) => {
                let backend = BackendNasmi386::new(memory_debug);
                let code_gen = CodeGenAsm::new(options.clone(), debug, memory_debug);
                Box::new(TypedFunctionsCreatorNasmi386::new(backend, code_gen))
            }
            CompileTarget::C(options) => {
                let code_gen = CodeGenC::new(options.clone(), debug, memory_debug);
                Box::new(TypedFunctionsCreatorC::new(code_gen))
            }
        }
    }

    pub fn get_evaluator(&self, debug: bool, memory_debug: bool) -> TextMacroEvaluator {
        match self {
            CompileTarget::Nasmi386(options) => {
                let code_gen = CodeGenAsm::new(options.clone(), debug, memory_debug);
                code_gen.get_text_macro_evaluator()
            }
            CompileTarget::C(options) => {
                let code_gen = CodeGenC::new(options.clone(), debug, memory_debug);
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
        memory_debug: bool,
    ) -> Result<Vec<(TextMacro, DefaultFunctionCall)>, String> {
        match self {
            CompileTarget::Nasmi386(options) => {
                let code_gen = CodeGenAsm::new(options.clone(), debug, memory_debug);
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
                let code_gen = CodeGenC::new(options.clone(), debug, memory_debug);
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
                vec![
                    CommandLineAction::Build,
                    CommandLineAction::Run,
                    CommandLineAction::Test,
                ]
            }
            CompileTarget::C(_) => vec![
                CommandLineAction::Build,
                CommandLineAction::Run,
                CommandLineAction::Test,
            ],
        }
    }

    pub fn run(&self, project: RasmProject, command_line_options: CommandLineOptions) {
        let start = Instant::now();

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

        let (container, catalog, errors) =
            project.container_and_catalog(&command_line_options.profile, self);

        info!("parse ended in {:?}", start.elapsed());

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            exit(1);
        }

        let out_file = out_folder.join(project.main_out_file_name(&command_line_options));

        if let Err(errors) = self.process_macros_and_compile(
            &project,
            container,
            &catalog,
            &command_line_options,
            out_folder,
            out_file.clone(),
        ) {
            for error in errors {
                eprintln!("{error}");
            }
            eprintln!("error: could not compile due to previous errors");

            exit(1);
        }

        info!("finished in {:?}", start.elapsed());

        if command_line_options.action == CommandLineAction::Run
            || command_line_options.action == CommandLineAction::Test
        {
            let mut command = Command::new(out_file.to_string_lossy().to_string());

            let output = command
                .stderr(Stdio::inherit())
                .stdout(Stdio::inherit())
                .output()
                .unwrap();

            if !output.status.success() {
                exit(1);
            }
        }
    }

    fn process_macros_and_compile(
        &self,
        project: &RasmProject,
        mut container: ASTModulesContainer,
        catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        command_line_options: &CommandLineOptions,
        out_folder: PathBuf,
        out_file: PathBuf,
    ) -> Result<(), Vec<CompilationError>> {
        let extractor = extract_macro_calls(&container, catalog);

        if extractor.is_empty() {
            if command_line_options.print_code {
                for (_, namespace, module) in container.modules().iter() {
                    println!("{}\n{}", namespace, module);
                }
            }

            self.compile_no_macro(
                &project,
                container,
                catalog,
                &command_line_options,
                out_folder,
                out_file,
            )
        } else {
            resolve_macros(
                project,
                self,
                &extractor,
                &mut container,
                catalog,
                command_line_options,
                out_folder.clone(),
            )?;

            let out_file = out_folder.join(
                project
                    .main_out_file_name(&command_line_options)
                    .to_string(),
            );

            info!("compiling final module");

            if let Err(errors) = self.process_macros_and_compile(
                &project,
                container,
                catalog,
                &command_line_options,
                out_folder,
                out_file.clone(),
            ) {
                eprintln!("Errors compiling after evaluating macros",);
                //eprintln!("{}", macro_module_body);

                return Err(errors);
            }
            return Ok(());
        }
    }

    /// Compile the given module without processing any macros.
    /// If you want to process macros, use `process_macros_and_compile`
    ///
    /// This function takes an already parsed module and compiles it
    /// directly, without any macro evaluation.
    ///
    /// Note that this function will not support any macros, even
    /// built-in macros. Any macro calls in the module will result
    /// in a compilation error.
    ///
    /// The function takes in the module to compile, the project that
    /// the module is a part of, the modules catalog, the command
    /// line options, the output folder, and the output file.
    ///
    /// It returns a result containing either no errors, or a
    /// vector of compilation errors. If any errors occur during
    /// compilation, they will be returned in the result.
    pub fn compile_no_macro(
        &self,
        project: &RasmProject,
        container: ASTModulesContainer,
        catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        command_line_options: &CommandLineOptions,
        out_folder: PathBuf,
        out_file: PathBuf,
    ) -> Result<(), Vec<CompilationError>> {
        let start = Instant::now();

        let mut statics = Statics::new();

        let enriched_container = enrich_container(
            &self,
            &mut statics,
            container,
            catalog,
            command_line_options.debug,
            command_line_options.memory_debug,
        );

        let modules = enriched_container
            .modules()
            .into_iter()
            .map(|(id, _, m)| {
                let (eh_id, eh_ns) = catalog.catalog_info(id).unwrap();
                (m.clone(), EnhModuleInfo::new(eh_id.clone(), eh_ns.clone()))
            })
            .collect::<Vec<_>>();

        let (ast_type_check, _) = ASTTypeChecker::from_modules_container(&enriched_container);

        info!("AST type check ended in {:?}", start.elapsed());

        let start = Instant::now();

        let (enhanced_ast_module, errors) = EnhancedASTModule::from_ast(
            modules,
            &mut statics,
            self,
            command_line_options.memory_debug,
            true,
            command_line_options.debug,
        );

        if !errors.is_empty() {
            return Err(errors);
        }

        let typed_module = get_typed_module(
            enhanced_ast_module,
            command_line_options.print_memory,
            command_line_options.print_code,
            &mut statics,
            self,
            command_line_options.memory_debug,
            ast_type_check,
            catalog,
            &enriched_container,
            command_line_options.debug,
        )
        .map_err(|it| vec![it])?;

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
                CommandLineAction::Build | CommandLineAction::Run | CommandLineAction::Test => {
                    if out_paths.len() != 1 {
                        panic!("Only one native file to compile is supported!");
                    }

                    let backend = BackendNasmi386::new(command_line_options.memory_debug);

                    let out = out_paths.remove(0);

                    if command_line_options.only_compile {
                        backend
                            .compile(
                                &out,
                                &out_file.with_extension("o"),
                                command_line_options.release,
                            )
                            .map_err(|it| vec![CompilationError::generic_none(it)])
                    } else {
                        backend
                            .compile_and_link(
                                &out,
                                &out_file.with_extension("o"),
                                &out_file,
                                &options.requires,
                                command_line_options.release,
                            )
                            .map_err(|it| vec![CompilationError::generic_none(it)])
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
                    &out_file,
                )
                .map_err(|message| vec![CompilationError::generic_none(message)])?;

                info!("compiler ended in {:?}", start.elapsed());

                if !result.status.success() {
                    return Err(vec![CompilationError::generic_none(
                        "Error running native compiler".to_owned(),
                    )]);
                }
                Ok(())
            }
        }
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
                            EnhBuiltinTypeKind::Integer,
                            EnhBuiltinTypeKind::Integer,
                        ));
                    }
                }
            }
            CompileTarget::C(_) => {}
        }

        result
    }

    pub fn get_default_functions(&self, print_allocation: bool) -> Vec<DefaultFunction> {
        let mut default_functions = match self {
            CompileTarget::Nasmi386(_) => {
                vec![
                    DefaultFunction::new_2(
                        "rasmalloc",
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::String,
                    ),
                    DefaultFunction::new_1("exitMain", EnhBuiltinTypeKind::Integer),
                    DefaultFunction::new_2(
                        "addRef",
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::String,
                    ),
                    DefaultFunction::new_3(
                        "memcopy",
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::Integer,
                    ),
                    DefaultFunction::new_2(
                        "deref",
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::String,
                    ),
                    DefaultFunction::new_1("addStaticStringToHeap", EnhBuiltinTypeKind::Integer),
                    DefaultFunction::new_2(
                        "createCmdLineArguments",
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::Integer,
                    ),
                    DefaultFunction::new_1("str_addRef", EnhBuiltinTypeKind::String),
                    DefaultFunction::new_1("str_deref", EnhBuiltinTypeKind::String),
                    DefaultFunction::new_3(
                        "addStaticAllocation",
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::Integer,
                    ),
                    DefaultFunction::new_3(
                        "addHeap",
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::Integer,
                        EnhBuiltinTypeKind::Integer,
                    ),
                ]
            }
            CompileTarget::C(_) => vec![DefaultFunction::new_0("freeReferences")],
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

fn get_native_string_array(projects: &[RasmProject], target: &str, key: &str) -> Vec<String> {
    let mut result = projects
        .iter()
        .flat_map(|it| it.get_native_string_array(target, key))
        .collect::<Vec<_>>();

    result.sort();
    result.dedup_by(|s1, s2| s1 == s2);

    result
}

#[cfg(test)]
mod tests {
    use tempdir::TempDir;

    use crate::{
        codegen::{c::options::COptions, compile_target::CompileTarget},
        commandline::{CommandLineAction, CommandLineOptions},
        project::RasmProject,
    };

    #[test]
    fn compile_tostring() {
        compile_test("../rasm/resources/test/macro/tostring_macro.rasm");
    }

    #[test]
    fn compile_vecmacro() {
        compile_test("../rasm/resources/test/macro/vec_macro.rasm");
    }

    #[test]
    fn compile_printmacro() {
        compile_test("../rasm/resources/test/macro/print_macro.rasm");
    }

    fn compile_test(source: &str) {
        let sut = CompileTarget::C(COptions::default());

        let project = RasmProject::new(std::path::PathBuf::from(source));

        let dir = TempDir::new("rasm_int_test").unwrap();

        let command_line_options = CommandLineOptions {
            action: CommandLineAction::Build,
            memory_debug: false,
            print_code: false,
            print_memory: false,
            only_compile: false,
            out: Some(dir.path().to_str().unwrap().to_string()),
            release: false,
            arguments: Vec::new(),
            include_tests: Vec::new(),
            debug: false,
            profile: crate::commandline::RasmProfile::Main,
        };
        sut.run(project, command_line_options);
    }
}
