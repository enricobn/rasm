use std::path::PathBuf;

use rasm_parser::catalog::modules_catalog::ModulesCatalog;

use crate::{
    codegen::{
        compile_target::CompileTarget, enh_ast::EnhModuleInfo, enhanced_module::EnhancedASTModule,
        statics::Statics,
    },
    enh_type_check::typed_ast::{convert_to_typed_module, ASTTypedModule},
    errors::CompilationError,
    project::{RasmProject, RasmProjectRunType},
    transformations::enrich_container,
    type_check::{ast_modules_container::ASTModulesContainer, ast_type_checker::ASTTypeChecker},
};

pub fn project_and_container(
    target: &CompileTarget,
    project_path: &str,
) -> (RasmProject, ASTModulesContainer) {
    let rasm_project = RasmProject::new(PathBuf::from(project_path));
    let project = rasm_project;

    let (container, catalog, _errors) =
        project.container_and_catalog(&RasmProjectRunType::Main, &target);

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
) -> Result<(ASTTypedModule, Statics), Vec<CompilationError>> {
    let mut statics = Statics::new();

    let run_type = RasmProjectRunType::Main;

    let (container, catalog, _) = project.container_and_catalog(&run_type, &target);

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
        EnhancedASTModule::from_ast(modules, &project, &mut statics, &target, false, true, false);

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
        false,
    ) {
        Ok(module) => Ok((module, statics)),
        Err(e) => Err(vec![e]),
    }

    //print_typed_module(&typed_module.0);
}
