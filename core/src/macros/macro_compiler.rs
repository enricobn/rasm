use std::{collections::HashMap, path::PathBuf, process::Command};

use rasm_parser::{
    catalog::{ModuleId, modules_catalog::ModulesCatalog},
    lexer::Lexer,
    parser::{Parser, ParserError, ast::ASTModule},
};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    codegen::{
        compile_target::CompileTarget,
        enh_ast::{EnhASTIndex, EnhASTNameSpace, EnhModuleId},
    },
    commandline::{CommandLineAction, CommandLineOptions},
    errors::CompilationError,
    macros::macro_call_extractor::MacroCall,
    project::RasmProject,
    type_check::ast_modules_container::ASTModulesContainer,
};

pub fn compile_macros<'a>(
    project: &RasmProject,
    target: &CompileTarget,
    macros_modules_container: ASTModulesContainer,
    macros_catalog: Box<dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>>,
    command_line_options: &CommandLineOptions,
    calls: Vec<&'a MacroCall>,
    out_folder: &PathBuf,
    macro_out_file: &PathBuf,
) -> Result<
    HashMap<ModuleId, Vec<(&'a MacroCall, (ASTModule, Vec<ParserError>))>>,
    Vec<CompilationError>,
> {
    let clo = CommandLineOptions {
        action: CommandLineAction::Build,
        memory_debug: false,
        print_code: false,
        print_memory: false,
        only_compile: false,
        out: command_line_options.out.clone(),
        release: command_line_options.release,
        arguments: Vec::new(),
        include_tests: Vec::new(),
        debug: false,
        profile: command_line_options.profile.clone(),
    };

    if let Err(errors) = target.compile(
        &project,
        macros_modules_container,
        macros_catalog.as_ref(),
        &clo,
        out_folder.clone(),
        macro_out_file.clone(),
    ) {
        eprintln!("Errors compiling macro module");

        return Err(errors);
    }

    // TODO move code in macro module

    let macro_modules = calls
        .par_iter()
        .map(|it| {
            (
                *it,
                evaluate_macro(macros_catalog.as_ref(), macro_out_file.clone(), it),
            )
        })
        .collect::<Vec<_>>();

    let macro_errors = macro_modules
        .iter()
        .flat_map(|it| it.1.1.iter())
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
        Vec<(&'a MacroCall, (ASTModule, Vec<ParserError>))>,
    > = HashMap::new();

    for it in macro_modules {
        macro_modules_map
            .entry(it.0.module_id.clone())
            .or_insert(Vec::new())
            .push(it);
    }
    Ok(macro_modules_map)
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
        let output_string = output_lines.collect::<Vec<_>>().join("\n");
        if first == "Error" {
            if let Some(info) = catalog.catalog_info(call.module_id()) {
                let index = EnhASTIndex::new(info.0.path(), call.position().clone());
                panic!("{} in {}", output_string, index);
            } else {
                panic!("{} in {}", output_string, call.position());
            }
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
