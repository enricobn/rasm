use std::{env, path::PathBuf};

use crate::{
    codegen::{compile_target::CompileTarget, statics::Statics},
    commandline::CommandLineOptions,
    project::{RasmProject, RasmProjectRunType},
    type_check::ast_modules_container::ASTModulesContainer,
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
