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
use rasm_parser::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTLambdaDef, ASTModule, ASTPosition,
    ASTStatement,
};
use rasm_parser::parser::{Parser, ParserError};
use rasm_utils::OptionDisplay;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{exit, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
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
use crate::macros::macro_call_extractor::{extract_macro_calls, MacroCall, MacroResultType};
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

static COUNT_MACRO_ID: AtomicUsize = AtomicUsize::new(0);

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
                all_projects.extend(project.all_projects());
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
                all_projects.extend(project.all_projects());

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

        let (container, catalog, errors) = project.container_and_catalog(&run_type, self);

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            exit(1);
        }

        let out_file = out_folder.join(project.main_out_file_name(&command_line_options));

        if let Err(errors) = self.process_macro_and_compile(
            &project,
            container,
            &catalog,
            start,
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

        if command_line_options.action == CommandLineAction::Test
            || command_line_options.action == CommandLineAction::Run
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

    fn process_macro_and_compile(
        &self,
        project: &RasmProject,
        mut container: ASTModulesContainer,
        catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        start: Instant,
        command_line_options: &CommandLineOptions,
        out_folder: PathBuf,
        out_file: PathBuf,
    ) -> Result<(), Vec<CompilationError>> {
        if COUNT_MACRO_ID.load(Ordering::SeqCst) > 10 {
            return Err(vec![CompilationError::generic_none(
                "Recursion in macro evaluation".to_owned(),
            )]);
        }
        let extractor = extract_macro_calls(&container, catalog);

        /*
        println!("extracted {} macro calls", extractor.calls().len());

        for call in extractor.calls() {
            println!(
                "transformed macro {} : {}",
                call.position(),
                call.transformed_macro
            );
        }
        */

        if extractor.calls().is_empty() {
            self.compile(
                &project,
                container,
                catalog,
                start,
                &command_line_options,
                out_folder,
                out_file,
            )
        } else {
            let macro_module_body = create_macro_module(&container, &extractor);

            // println!("macro module:\n{macro_module_body}");

            let (macro_module, macro_module_errors) =
                Parser::new(Lexer::new(macro_module_body.clone())).parse();

            // it should not happens
            if !macro_module_errors.is_empty() {
                eprintln!("Errors parsing macro module:\n{macro_module_body}");
                return Err(macro_module_errors
                    .into_iter()
                    .map(|it| CompilationError::from_parser_error(it, None))
                    .collect());
            }

            let mut original_container = container.clone();
            let orig_catalog = catalog.clone_catalog();
            let mut new_catalog = catalog.clone_catalog();

            container.remove_body();

            let macro_id = format!(
                "{}_macro_{}",
                project.config.package.name.clone(),
                COUNT_MACRO_ID.fetch_add(1, Ordering::SeqCst)
            );

            container.add(
                macro_module,
                ModuleNamespace("".to_owned()),
                ModuleId(macro_id.clone()),
                false,
                true,
            );

            new_catalog.add(
                EnhModuleId::Other(macro_id.clone()),
                EnhASTNameSpace::global(),
            );

            let macro_out_file = out_folder.join(macro_id.clone());

            info!("compiling macro module");

            if let Err(errors) = self.compile(
                &project,
                container,
                new_catalog.as_ref(),
                start,
                &command_line_options,
                out_folder.clone(),
                macro_out_file.clone(),
            ) {
                eprintln!("Errors compiling macro module\n{}", macro_module_body);

                return Err(errors);
            }

            // TODO move code in macro module

            let macro_modules = extractor
                .calls()
                .par_iter()
                .map(|it| {
                    (
                        *it,
                        Self::evaluate_macro(new_catalog.as_ref(), macro_out_file.clone(), it),
                    )
                })
                .collect::<Vec<_>>();

            let macro_errors = macro_modules
                .iter()
                .flat_map(|it| it.1 .1.iter())
                .collect::<Vec<_>>();

            if !macro_errors.is_empty() {
                eprintln!("Errors evaluating macros");
                return Err(macro_errors
                    .into_iter()
                    .map(|it| CompilationError::from_parser_error(it.clone(), None))
                    .collect());
            }

            let mut macro_modules_map: HashMap<
                ModuleId,
                Vec<(&MacroCall, (ASTModule, Vec<ParserError>))>,
            > = HashMap::new();

            for it in macro_modules {
                macro_modules_map
                    .entry(it.0.module_id.clone())
                    .or_insert(Vec::new())
                    .push(it);
            }

            for (key, value) in macro_modules_map {
                if let Some((mut module, module_namespace)) = original_container.remove_module(&key)
                {
                    for (call, (new_module, _)) in value {
                        match call.macro_result_type {
                            MacroResultType::Module => {
                                if !new_module.body.is_empty() {
                                    self.replace_statements_in_module(
                                        &mut module,
                                        &call.position,
                                        new_module.body,
                                    );
                                }
                                module.functions.extend(new_module.functions);
                            }
                            MacroResultType::Expression => {
                                if new_module.body.len() != 1 {
                                    return Err(vec![CompilationError::generic_none(format!(
                                        "Expected one single expression, evaluating macro {}",
                                        call.transformed_macro
                                    ))]);
                                }

                                let statement = new_module.body[0].clone();

                                let expression =
                                    if let ASTStatement::ASTExpressionStatement(e, _) = statement {
                                        e
                                    } else {
                                        return Err(vec![CompilationError::generic_none(format!(
                                            "Expected expression statement, evaluating macro {}",
                                            call.transformed_macro
                                        ))]);
                                    };

                                // TODO otimize
                                self.replace_expression_in_module(
                                    &mut module,
                                    &call.position,
                                    &expression,
                                );
                            }
                        }
                    }

                    for s in module.structs.iter_mut() {
                        s.attribute_macros.clear();
                    }

                    for e in module.enums.iter_mut() {
                        e.attribute_macros.clear();
                    }

                    // println!("new module:\n{module}");

                    original_container.add(
                        module,
                        module_namespace.clone(),
                        key.clone(),
                        false,
                        false,
                    );
                }
            }

            let out_file = out_folder.join(
                project
                    .main_out_file_name(&command_line_options)
                    .to_string(),
            );

            info!("compiling final module");

            if let Err(errors) = self.process_macro_and_compile(
                &project,
                original_container,
                orig_catalog.as_ref(),
                start,
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

    fn evaluate_macro(
        catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
        macro_program: PathBuf,
        call: &MacroCall,
    ) -> (ASTModule, Vec<ParserError>) {
        // println!("evaluating macro:\n{}", call.transformed_macro);

        let mut command = Command::new(macro_program.clone());
        command.arg(call.id.to_string());

        let output = command.output().unwrap();
        let output_stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let output_stderr = String::from_utf8_lossy(&output.stderr).to_string();

        if !output_stderr.is_empty() {
            panic!(
                "Error in macro. Calling {} {}:\n{}",
                macro_program.to_string_lossy(),
                call.id,
                output_stderr
            );
        }

        if output_stdout.is_empty() {
            panic!(
                "Not output in macro. Calling {} {}",
                macro_program.to_string_lossy(),
                call.id
            );
        }

        /*
        println!("calling {} {}", macro_program.to_string_lossy(), call.id);
        println!("output_stderr:\n{output_stderr}");
        println!("output_stderr:\n{output_stderr}");
        */

        let mut output_lines = output_stdout.lines();
        if let Some(first) = output_lines.next() {
            let mut output_string = output_lines.collect::<Vec<_>>().join("\n");
            if first == "Error" {
                if let Some(info) = catalog.catalog_info(call.module_id()) {
                    let index = EnhASTIndex::new(info.0.path(), call.position().clone());
                    panic!("{} in {}", output_string, index);
                } else {
                    panic!("{} in {}", output_string, call.position());
                }
            } else if first == "Expression" {
                output_string.push(';');
            }

            let lexer = Lexer::new(output_string);

            let parser = Parser::new(lexer);
            let result = parser.parse();
            // println!("result:\n{}", result.0);
            result
        } else {
            panic!("Expected a macro result.")
        }
    }

    fn replace_statements_in_module(
        &self,
        module: &mut ASTModule,
        position: &ASTPosition,
        new_body: Vec<ASTStatement>,
    ) {
        self.replace_statements_in_body(&mut module.body, position, new_body.clone());

        for function in module.functions.iter_mut() {
            if let ASTFunctionBody::RASMBody(ref mut body) = function.body {
                self.replace_statements_in_body(body, position, new_body.clone());
            }
        }

        //println!("{module}");
    }

    fn replace_statements_in_body(
        &self,
        body: &mut Vec<ASTStatement>,
        position: &ASTPosition,
        new_body: Vec<ASTStatement>,
    ) {
        if let Some(mut i) = body
            .iter()
            .enumerate()
            .find(|(_, it)| {
                if let ASTStatement::ASTExpressionStatement(expr, _) = it {
                    expr.position().id == position.id
                } else {
                    false
                }
            })
            .map(|(i, _)| i)
        {
            body.remove(i);

            for statement in new_body.iter() {
                body.insert(i, statement.clone());
                i += 1;
            }
        }

        for statement in body.iter_mut() {
            match statement {
                ASTStatement::ASTExpressionStatement(expr, _) => {
                    self.replace_statements_in_expression(expr, position, new_body.clone());
                }
                ASTStatement::ASTLetStatement(_, expr, _) => {
                    self.replace_statements_in_expression(expr, position, new_body.clone());
                }
                ASTStatement::ASTConstStatement(_, expr, _, _) => {
                    self.replace_statements_in_expression(expr, position, new_body.clone());
                }
            }
        }
    }

    fn replace_statements_in_expression(
        &self,
        expr: &mut ASTExpression,
        position: &ASTPosition,
        new_body: Vec<ASTStatement>,
    ) {
        match expr {
            ASTExpression::ASTFunctionCallExpression(astfunction_call) => {
                for parameter in astfunction_call.parameters_mut() {
                    self.replace_statements_in_expression(parameter, position, new_body.clone());
                }
            }
            ASTExpression::ASTValueRefExpression(_, _) => {}
            ASTExpression::ASTValueExpression(_, _) => {}
            ASTExpression::ASTLambdaExpression(astlambda_def) => {
                self.replace_statements_in_body(&mut astlambda_def.body, position, new_body);
            }
        }
    }

    fn replace_expression_in_module(
        &self,
        module: &mut ASTModule,
        position: &ASTPosition,
        expression: &ASTExpression,
    ) {
        module.body = self.replace_expression_in_body(&module.body, position, expression);

        for function in module.functions.iter_mut() {
            if let ASTFunctionBody::RASMBody(ref mut body) = function.body {
                *body = self.replace_expression_in_body(body, position, expression);
            }
        }

        // println!("module:\n{module}");
    }

    fn replace_expression_in_body(
        &self,
        body: &Vec<ASTStatement>,
        position: &ASTPosition,
        expression: &ASTExpression,
    ) -> Vec<ASTStatement> {
        let mut new_body = Vec::new();
        for statement in body.iter() {
            match statement {
                ASTStatement::ASTExpressionStatement(astexpression, astposition) => {
                    new_body.push(ASTStatement::ASTExpressionStatement(
                        self.replace_expression_in_expression(astexpression, position, &expression),
                        astposition.clone(),
                    ));
                }
                ASTStatement::ASTLetStatement(name, astexpression, astposition) => {
                    new_body.push(ASTStatement::ASTLetStatement(
                        name.clone(),
                        self.replace_expression_in_expression(astexpression, position, &expression),
                        astposition.clone(),
                    ));
                }
                ASTStatement::ASTConstStatement(name, astexpression, astposition, astmodifiers) => {
                    new_body.push(ASTStatement::ASTConstStatement(
                        name.clone(),
                        self.replace_expression_in_expression(astexpression, position, &expression),
                        astposition.clone(),
                        astmodifiers.clone(),
                    ));
                }
            }
        }

        new_body
    }

    fn replace_expression_in_expression(
        &self,
        from: &ASTExpression,
        position: &ASTPosition,
        to: &ASTExpression,
    ) -> ASTExpression {
        if from.position().id == position.id {
            to.clone()
        } else {
            match from {
                ASTExpression::ASTFunctionCallExpression(call) => {
                    let new_parameters = call
                        .parameters()
                        .iter()
                        .map(|it| self.replace_expression_in_expression(it, position, to))
                        .collect::<Vec<_>>();
                    ASTExpression::ASTFunctionCallExpression(ASTFunctionCall::new(
                        call.function_name().clone(),
                        new_parameters,
                        call.position().copy(),
                        call.generics().clone(),
                        call.target().clone(),
                        call.is_macro(),
                    ))
                }
                ASTExpression::ASTValueRefExpression(_, _) => from.clone(),
                ASTExpression::ASTValueExpression(_, _) => from.clone(),
                ASTExpression::ASTLambdaExpression(lambda_def) => {
                    ASTExpression::ASTLambdaExpression(ASTLambdaDef {
                        parameter_names: lambda_def.parameter_names.clone(),
                        body: self.replace_expression_in_body(&lambda_def.body, position, &to),
                        position: lambda_def.position.copy(),
                    })
                }
            }
        }
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
    ) -> Result<(), Vec<CompilationError>> {
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
            return Err(errors);
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
                CommandLineAction::Build | CommandLineAction::Test | CommandLineAction::Run => {
                    if out_paths.len() != 1 {
                        panic!("Only one native file to compile is supported!");
                    }

                    let backend = BackendNasmi386::new(command_line_options.debug);

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

fn get_native_string_array(projects: &[RasmProject], target: &str, key: &str) -> Vec<String> {
    let mut result = projects
        .iter()
        .flat_map(|it| {
            if let Some(ref targets) = it.config.targets {
                if let Some(nasm_i386_value) = targets.get(target) {
                    if let Value::Table(nasm_i386_table) = nasm_i386_value {
                        if let Some(value) = nasm_i386_table.get(key) {
                            if let Value::Array(a) = value {
                                a.iter().map(|req| {
                                    if let Value::String(s) = req {
                                        s.clone()
                                    } else {
                                        panic!(
                                            "{target}/{key} should be an array of strings {}/rasm.toml",
                                            it.root.to_string_lossy()
                                        );
                                    }
                                }).collect::<Vec<_>>()
                            } else {
                                panic!(
                                    "{target}/{key} should be an array in {}/rasm.toml, but is {}",
                                    it.root.to_string_lossy(),
                                    OptionDisplay(&nasm_i386_table.get(key))
                                );
                            }
                        } else {
                            Vec::new()
                        }
                    } else {
                        panic!(
                            "{target} should be a table in {}/rasm.toml",
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
            debug: false,
            print_code: false,
            print_memory: false,
            only_compile: false,
            out: Some(dir.path().to_str().unwrap().to_string()),
            release: false,
            arguments: Vec::new(),
            include_tests: Vec::new(),
        };
        sut.run(project, command_line_options);
    }
}
