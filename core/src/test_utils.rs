use std::{env, path::PathBuf};

use crate::{
    codegen::{
        compile_target::CompileTarget, enhanced_module::EnhancedASTModule, statics::Statics,
    },
    commandline::CommandLineOptions,
    enh_type_check::typed_ast::{convert_to_typed_module, ASTTypedModule},
    errors::CompilationError,
    project::{RasmProject, RasmProjectRunType},
    type_check::{ast_modules_container::ASTModulesContainer, ast_type_checker::ASTTypeChecker},
};

pub fn project_and_container(
    target: &CompileTarget,
    project_path: &str,
) -> (RasmProject, ASTModulesContainer) {
    let path = env::current_dir().unwrap();
    env::set_var(
        "RASM_STDLIB",
        &format!(
            "{}",
            path.join("../stdlib")
                .canonicalize()
                .unwrap()
                .to_string_lossy()
        ),
    );
    let rasm_project = RasmProject::new(PathBuf::from(project_path));
    let project = rasm_project;

    let mut statics = Statics::new();
    let (modules, _errors) = project.get_all_modules(
        &mut statics,
        &RasmProjectRunType::Main,
        &target,
        false,
        &CommandLineOptions::default(),
    );

    let mut container = ASTModulesContainer::new();

    for (module, info) in modules.into_iter() {
        container.add(
            module,
            info.module_namespace(),
            info.module_id(),
            false,
            !info.namespace.is_same_lib(&project.config.package.name),
        );
    }

    (project, container)
}

pub fn project_to_ast_typed_module(
    project: &RasmProject,
    target: &CompileTarget,
) -> Result<(ASTTypedModule, Statics), Vec<CompilationError>> {
    let mut statics = Statics::new();

    let run_type = RasmProjectRunType::Main;
    let command_line_options = CommandLineOptions::default();

    let (modules, _errors) = project.get_all_modules(
        &mut statics,
        &run_type,
        &target,
        false,
        &command_line_options,
    );

    //resolved_module.print();

    let mut statics_for_cc = Statics::new();

    let (container, catalog, _) = project.container_and_catalog(
        &mut statics_for_cc,
        &run_type,
        &target,
        command_line_options.debug,
        &command_line_options,
    );

    let (module, errors) =
        EnhancedASTModule::from_ast(modules, &project, &mut statics, &target, false, true);

    if !errors.is_empty() {
        return Err(errors);
    }

    let mandatory_functions = target.get_mandatory_functions(&module);

    let default_functions = target.get_default_functions(false);

    match convert_to_typed_module(
        module,
        false,
        mandatory_functions,
        &mut statics,
        default_functions,
        &target,
        false,
        ASTTypeChecker::from_modules_container(&container).0,
        &catalog,
        &container,
    ) {
        Ok(module) => Ok((module, statics)),
        Err(e) => Err(vec![e]),
    }

    //print_typed_module(&typed_module.0);
}
