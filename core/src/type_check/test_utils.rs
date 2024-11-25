use std::{env, path::PathBuf};

use crate::{
    codegen::{compile_target::CompileTarget, statics::Statics},
    commandline::CommandLineOptions,
    project::RasmProject,
};

use super::ast_modules_container::ASTModulesContainer;

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
        crate::project::RasmProjectRunType::Main,
        &target,
        false,
        &env::temp_dir().join("tmp"),
        &CommandLineOptions::default(),
    );

    let mut container = ASTModulesContainer::new();

    for (module, info) in modules.iter() {
        container.add(module, info.module_namespace(), info.module_id(), false);
    }

    (project, container)
}
