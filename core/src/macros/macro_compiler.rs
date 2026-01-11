use std::{
    collections::HashMap,
    path::PathBuf,
    process::Command,
    sync::atomic::{AtomicUsize, Ordering},
};

use log::info;
use rasm_parser::{
    catalog::{ModuleId, ModuleNamespace, modules_catalog::ModulesCatalog},
    lexer::Lexer,
    parser::{
        Parser, ParserError,
        ast::{
            ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTLambdaDef, ASTModule, ASTPosition,
            ASTStatement,
        },
    },
};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    codegen::{
        compile_target::CompileTarget,
        enh_ast::{EnhASTIndex, EnhASTNameSpace, EnhModuleId},
    },
    commandline::{CommandLineAction, CommandLineOptions},
    errors::CompilationError,
    macros::{
        macro_call_extractor::{MacroCall, MacroCallExtractor, MacroResultType},
        macro_module::create_macro_module,
    },
    project::RasmProject,
    type_check::ast_modules_container::ASTModulesContainer,
};

static COUNT_MACRO_ID: AtomicUsize = AtomicUsize::new(0);

pub fn resolve_macros(
    project: &RasmProject,
    target: &CompileTarget,
    extractor: &MacroCallExtractor,
    container: &mut ASTModulesContainer,
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    command_line_options: &CommandLineOptions,
    out_folder: PathBuf,
) -> Result<(), Vec<CompilationError>> {
    let calls = extractor.resolvable_calls();

    let macro_module = create_macro_module(&container, catalog, &calls)?;

    let mut macro_container = container.clone();
    let mut new_catalog = catalog.clone_catalog();

    macro_container.remove_body();

    let macro_id = format!(
        "{}_macro_{}",
        project.name(),
        COUNT_MACRO_ID.fetch_add(1, Ordering::SeqCst)
    );

    if COUNT_MACRO_ID.load(Ordering::SeqCst) > 10 {
        return Err(vec![CompilationError::generic_none(
            "More than 10 macro loops, perhaps a recursion in macro evaluation?".to_owned(),
        )]);
    }

    macro_container.add(
        macro_module,
        ModuleNamespace::global(),
        ModuleId(macro_id.clone()),
        false,
        true,
    );

    new_catalog.add(
        EnhModuleId::Other(macro_id.clone()),
        EnhASTNameSpace::global(),
    );

    let macro_out_file = out_folder.join(macro_id.clone());

    compile_macros(
        project,
        target,
        macro_container,
        new_catalog,
        command_line_options,
        calls,
        &out_folder,
        &macro_out_file,
        container,
    )
}

fn compile_macros<'a>(
    project: &RasmProject,
    target: &CompileTarget,
    container: ASTModulesContainer,
    new_catalog: Box<dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>>,
    command_line_options: &CommandLineOptions,
    calls: Vec<&'a MacroCall>,
    out_folder: &PathBuf,
    macro_out_file: &PathBuf,
    original_container: &mut ASTModulesContainer,
) -> Result<(), Vec<CompilationError>> {
    let macro_modules_map = compile_macros_internal(
        project,
        target,
        container,
        new_catalog,
        command_line_options,
        calls,
        &out_folder,
        &macro_out_file,
    )?;

    for (key, value) in macro_modules_map {
        if let Some((mut module, module_namespace)) = original_container.remove_module(&key) {
            for (call, new_module) in value {
                match call.macro_result_type {
                    MacroResultType::Statement => {
                        replace_statements_in_module(&mut module, &call.position, new_module.body);
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
                        replace_expression_in_module(&mut module, &call.position, &expression);
                        module.functions.extend(new_module.functions);
                    }
                    MacroResultType::Attribute => {
                        module.functions.extend(new_module.functions);
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

            original_container.add(module, module_namespace.clone(), key.clone(), false, false);
        }
    }
    Ok(())
}

fn compile_macros_internal<'a>(
    project: &RasmProject,
    target: &CompileTarget,
    macros_modules_container: ASTModulesContainer,
    macros_catalog: Box<dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>>,
    command_line_options: &CommandLineOptions,
    calls: Vec<&'a MacroCall>,
    out_folder: &PathBuf,
    macro_out_file: &PathBuf,
) -> Result<HashMap<ModuleId, Vec<(&'a MacroCall, ASTModule)>>, Vec<CompilationError>> {
    info!("compiling macro module");

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

    info!("evaluating macros");

    let evaluation_results = calls
        .par_iter()
        .map(|it| {
            (
                *it,
                evaluate_macro(macros_catalog.as_ref(), macro_out_file.clone(), it),
            )
        })
        .collect::<Vec<_>>();

    let macro_errors = evaluation_results
        .iter()
        .flat_map(|it| match &it.1 {
            Ok((_, errors)) => errors
                .into_iter()
                .map(|it| CompilationError::from_parser_error(it.clone(), None))
                .collect(),
            Err(e) => vec![CompilationError::generic_none(e.clone())],
        })
        .collect::<Vec<_>>();

    if !macro_errors.is_empty() {
        eprintln!("Errors evaluating macros");
        return Err(macro_errors);
    }

    let mut macro_modules_map: HashMap<ModuleId, Vec<(&'a MacroCall, ASTModule)>> = HashMap::new();

    for (call, result) in evaluation_results {
        if let Ok((module, _)) = result {
            macro_modules_map
                .entry(call.module_id.clone())
                .or_insert(Vec::new())
                .push((call, module));
        }
    }

    Ok(macro_modules_map)
}

fn replace_statements_in_module(
    module: &mut ASTModule,
    position: &ASTPosition,
    new_body: Vec<ASTStatement>,
) {
    replace_statements_in_body(&mut module.body, position, new_body.clone());

    for function in module.functions.iter_mut() {
        if let ASTFunctionBody::RASMBody(ref mut body) = function.body {
            replace_statements_in_body(body, position, new_body.clone());
        }
    }

    //println!("{module}");
}

fn replace_statements_in_body(
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
                replace_statements_in_expression(expr, position, new_body.clone());
            }
            ASTStatement::ASTLetStatement(_, expr, _) => {
                replace_statements_in_expression(expr, position, new_body.clone());
            }
            ASTStatement::ASTConstStatement(_, expr, _, _) => {
                replace_statements_in_expression(expr, position, new_body.clone());
            }
        }
    }
}

fn replace_statements_in_expression(
    expr: &mut ASTExpression,
    position: &ASTPosition,
    new_body: Vec<ASTStatement>,
) {
    match expr {
        ASTExpression::ASTFunctionCallExpression(astfunction_call) => {
            for parameter in astfunction_call.parameters_mut() {
                replace_statements_in_expression(parameter, position, new_body.clone());
            }
        }
        ASTExpression::ASTValueRefExpression(_, _) => {}
        ASTExpression::ASTValueExpression(_, _) => {}
        ASTExpression::ASTLambdaExpression(astlambda_def) => {
            replace_statements_in_body(&mut astlambda_def.body, position, new_body);
        }
    }
}

fn replace_expression_in_module(
    module: &mut ASTModule,
    position: &ASTPosition,
    expression: &ASTExpression,
) {
    module.body = replace_expression_in_body(&module.body, position, expression);

    for function in module.functions.iter_mut() {
        if let ASTFunctionBody::RASMBody(ref mut body) = function.body {
            *body = replace_expression_in_body(body, position, expression);
        }
    }

    // println!("module:\n{module}");
}

fn replace_expression_in_body(
    body: &Vec<ASTStatement>,
    position: &ASTPosition,
    expression: &ASTExpression,
) -> Vec<ASTStatement> {
    let mut new_body = Vec::new();
    for statement in body.iter() {
        match statement {
            ASTStatement::ASTExpressionStatement(astexpression, astposition) => {
                new_body.push(ASTStatement::ASTExpressionStatement(
                    replace_expression_in_expression(astexpression, position, &expression),
                    astposition.clone(),
                ));
            }
            ASTStatement::ASTLetStatement(name, astexpression, astposition) => {
                new_body.push(ASTStatement::ASTLetStatement(
                    name.clone(),
                    replace_expression_in_expression(astexpression, position, &expression),
                    astposition.clone(),
                ));
            }
            ASTStatement::ASTConstStatement(name, astexpression, astposition, astmodifiers) => {
                new_body.push(ASTStatement::ASTConstStatement(
                    name.clone(),
                    replace_expression_in_expression(astexpression, position, &expression),
                    astposition.clone(),
                    astmodifiers.clone(),
                ));
            }
        }
    }

    new_body
}

fn replace_expression_in_expression(
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
                    .map(|it| replace_expression_in_expression(it, position, to))
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
                    body: replace_expression_in_body(&lambda_def.body, position, &to),
                    position: lambda_def.position.copy(),
                })
            }
        }
    }
}

fn evaluate_macro(
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    macro_program: PathBuf,
    call: &MacroCall,
) -> Result<(ASTModule, Vec<ParserError>), String> {
    // println!("evaluating macro:\n{}", call.transformed_macro);

    let mut command = Command::new(macro_program.clone());
    command.arg(call.id.to_string());

    let output = command.output().unwrap();
    let output_stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let output_stderr = String::from_utf8_lossy(&output.stderr).to_string();

    if !output_stderr.is_empty() {
        return Err(format!(
            "Error in macro. Calling {} {}:\n{}",
            macro_program.to_string_lossy(),
            call.id,
            output_stderr
        ));
    }

    if output_stdout.is_empty() {
        return Err(format!(
            "Not output in macro. Calling {} {}",
            macro_program.to_string_lossy(),
            call.id
        ));
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
                return Err(format!("{} in {}", output_string, index));
            } else {
                return Err(format!("{} in {}", output_string, call.position()));
            }
        }

        let lexer = Lexer::new(output_string);

        let parser = Parser::new(lexer);
        let result = parser.parse();
        // println!("result:\n{}", result.0);
        Ok(result)
    } else {
        Err("Expected a macro result.".to_owned())
    }
}
