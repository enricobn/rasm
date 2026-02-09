use std::path::PathBuf;

use rasm_parser::catalog::modules_catalog::ModulesCatalog;

use crate::{
    codegen::{
        compile_target::CompileTarget, enh_ast::EnhModuleInfo, enhanced_module::EnhancedASTModule,
        get_typed_module, statics::Statics,
    },
    commandline::RasmProfile,
    enh_type_check::typed_ast::ASTTypedModule,
    errors::CompilationError,
    project::RasmProject,
    transformations::enrich_container,
    type_check::{ast_modules_container::ASTModulesContainer, ast_type_checker::ASTTypeChecker},
};

pub fn project_and_container(
    target: &CompileTarget,
    project_path: &str,
) -> (RasmProject, ASTModulesContainer) {
    let project = RasmProject::new(PathBuf::from(project_path));

    let (container, catalog, _errors) = project.container_and_catalog(&RasmProfile::Main, target);

    let container = enrich_container(
        &target,
        &mut Statics::new(),
        container,
        &catalog,
        false,
        false,
    );

    (project, container)
}

pub fn project_to_ast_typed_module(
    project: &RasmProject,
    target: &CompileTarget,
    profile: &RasmProfile,
) -> Result<(ASTTypedModule, Statics), Vec<CompilationError>> {
    let mut statics = Statics::new();

    let (container, catalog, parse_errors) = project.container_and_catalog(&profile, &target);

    if !parse_errors.is_empty() {
        return Err(parse_errors);
    }

    let container = enrich_container(target, &mut statics, container, &catalog, false, false);

    let modules = container
        .modules()
        .into_iter()
        .map(|(id, _, m)| {
            let (eh_id, eh_ns) = catalog.catalog_info(id).unwrap();
            (m.clone(), EnhModuleInfo::new(eh_id.clone(), eh_ns.clone()))
        })
        .collect::<Vec<_>>();

    let (module, errors) =
        EnhancedASTModule::from_ast(modules, &mut statics, &target, false, true, false);

    if !errors.is_empty() {
        return Err(errors);
    }

    match get_typed_module(
        module,
        false,
        false,
        &mut statics,
        target,
        false,
        ASTTypeChecker::from_modules_container(&container).0,
        &catalog,
        &container,
        false,
    ) {
        Ok(module) => Ok((module, statics)),
        Err(e) => Err(vec![e]),
    }

    //print_typed_module(&typed_module.0);
}
