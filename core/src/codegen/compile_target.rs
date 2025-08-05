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
use rasm_parser::catalog::{ModuleId, ModuleNamespace};
use rasm_parser::lexer::Lexer;
use rasm_parser::parser::ast::{ASTModule, ASTPosition, ASTStatement};
use rasm_parser::parser::Parser;
use rasm_utils::OptionDisplay;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{exit, Command};
use std::time::Instant;

use toml::Value;

use crate::codegen::asm::backend::Backend;
use crate::codegen::c::c_compiler::compile_c;
use crate::codegen::c::code_gen_c::CodeGenC;
use crate::codegen::c::functions_creator_c::CFunctionsCreator;
use crate::codegen::c::options::COptions;
use crate::codegen::enh_ast::{
    EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind, EnhModuleId,
    EnhModuleInfo,
};
use crate::codegen::enh_val_context::EnhValContext;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{TextMacro, TextMacroEvaluator};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_typed_module, AsmOptions, CodeGen};
use crate::commandline::{CommandLineAction, CommandLineOptions};
use crate::errors::CompilationError;
use crate::macros::macro_call_extractor::extract_macro_calls;
use crate::macros::macro_module::create_macro_module;
use crate::project::{RasmProject, RasmProjectRunType};
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

        let run_type = if command_line_options.action == CommandLineAction::Test {
            RasmProjectRunType::Test
        } else {
            RasmProjectRunType::Main
        };

        let (container, mut catalog, errors) = project.container_and_catalog(&run_type, self);

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            exit(1);
        }

        let extractor = extract_macro_calls(container, &catalog);

        if extractor.calls().is_empty() {
            let out_file = out_folder.join(project.main_out_file_name(&command_line_options));
            self.compile(
                &project,
                extractor.container,
                &catalog,
                start,
                &command_line_options,
                out_folder,
                out_file,
            );
        } else {
            let macro_module = create_macro_module(&extractor);

            // println!("macro module:\n {}", macro_module);

            let mut container = extractor.container.clone();
            container.remove_body();

            container.add(
                macro_module,
                ModuleNamespace("".to_owned()),
                ModuleId("__macro".to_owned()),
                false,
                true,
            );

            let orig_catalog = catalog.clone_catalog();
            let mut original_container = extractor.container.clone();

            catalog.add(
                EnhModuleId::Other("__macro".to_owned()),
                EnhASTNameSpace::global(),
            );

            let out_file = out_folder.join(format!(
                "{}_macro",
                project.main_out_file_name(&command_line_options)
            ));

            info!("compiling macro module");

            self.compile(
                &project,
                container,
                &catalog,
                start,
                &command_line_options,
                out_folder.clone(),
                out_file.clone(),
            );

            for call in extractor.calls() {
                let mut command = Command::new(out_file.clone());
                command.arg(call.id.to_string());

                let output = command.output().unwrap();
                let output_s = String::from_utf8_lossy(&output.stdout).to_string();
                let mut output_lines = output_s.lines();
                if let Some(first) = output_lines.next() {
                    let output_string = output_lines.collect::<Vec<_>>().join("\n");
                    if first == "MacroError" {
                        if let Some(info) = catalog.catalog_info(call.module_id()) {
                            let index = EnhASTIndex::new(info.0.path(), call.position().clone());
                            panic!("{} in {}", output_string, index);
                        } else {
                            panic!("{} in {}", output_string, call.position());
                        }
                    }

                    let lexer = Lexer::new(output_string);

                    let parser = Parser::new(lexer);
                    let (new_module, errors) = parser.parse();

                    // println!("created module from macro:\n {}", new_module);

                    if !errors.is_empty() {
                        for error in errors {
                            eprintln!("{error}");
                        }
                        panic!()
                    }

                    if let Some((mut module, module_namespace)) =
                        original_container.remove_module(&call.module_id)
                    {
                        if !new_module.body.is_empty() {
                            self.replace_in_module(&mut module, &call.position, new_module.body);
                        }
                        module.functions.extend(new_module.functions);

                        original_container.add(
                            module,
                            module_namespace,
                            call.module_id.clone(),
                            false,
                            false,
                        );
                    }
                }

                // println!("output_s:\n{output_s}");
            }

            let out_file = out_folder.join(
                project
                    .main_out_file_name(&command_line_options)
                    .to_string(),
            );

            info!("compiling final module");

            self.compile(
                &project,
                original_container,
                orig_catalog.as_ref(),
                start,
                &command_line_options,
                out_folder,
                out_file.clone(),
            );
        }
    }

    fn replace_in_module(
        &self,
        module: &mut ASTModule,
        position: &ASTPosition,
        new_body: Vec<ASTStatement>,
    ) {
        if let Some(mut i) = module
            .body
            .iter()
            .enumerate()
            .find(|(_, it)| it.position() == position)
            .map(|(i, _)| i)
        {
            module.body.remove(i);

            for statement in new_body {
                module.body.insert(i, statement);
                i += 1;
            }
        }

        //println!("{module}");
    }

    fn compile(
        &self,
        project: &RasmProject,
        container: ASTModulesContainer,
        catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        start: Instant,
        command_line_options: &CommandLineOptions,
        out_folder: PathBuf,
        out_file: PathBuf,
    ) {
        let mut statics = Statics::new();

        let enriched_container = enrich_container(
            &self,
            &mut statics,
            container,
            catalog,
            command_line_options.debug,
        );

        let modules = enriched_container
            .modules()
            .into_iter()
            .map(|(id, _, m)| {
                let (eh_id, eh_ns) = catalog.catalog_info(id).unwrap();
                (m.clone(), EnhModuleInfo::new(eh_id.clone(), eh_ns.clone()))
            })
            .collect::<Vec<_>>();

        info!("parse ended in {:?}", start.elapsed());

        let start = Instant::now();

        let (ast_type_check, _) = ASTTypeChecker::from_modules_container(&enriched_container);

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
            catalog,
            &enriched_container,
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
                        backend.compile(&out, &out_file.with_extension("o"));
                    } else {
                        backend.compile_and_link(
                            &out,
                            &out_file.with_extension("o"),
                            &out_file,
                            &options.requires,
                        );
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
                            EnhBuiltinTypeKind::Integer,
                            EnhBuiltinTypeKind::Integer,
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
