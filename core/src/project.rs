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

use derivative::Derivative;
use rasm_utils::OptionDisplay;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Display;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::codegen::compile_target::{C, CompileTarget, NASMI386};
use crate::codegen::enh_ast::{EnhASTIndex, EnhASTNameSpace, EnhModuleId, EnhModuleInfo};
use crate::commandline::{CommandLineOptions, RasmProfile};
use crate::pm::repository::{
    LocalPackageRepository, PackageManager, PackageManagerImpl, RepositoryAuthentication,
    VersionFilter,
};
use crate::project_catalog::RasmProjectCatalog;
use crate::type_check::ast_modules_container::ASTModulesContainer;
use itertools::Itertools;
use linked_hash_map::LinkedHashMap;
use log::{info, warn};
use pathdiff::diff_paths;
use rasm_parser::catalog::modules_catalog::ModulesCatalog;
use rasm_utils::debug_indent::{enable_log, log_enabled};
use rayon::prelude::*;
use rust_embed::RustEmbed;
use semver::Version;
use serde::Deserialize;
use toml::map::Map;
use toml::{Table, Value};
use walkdir::WalkDir;

use crate::errors::{CompilationError, CompilationErrorKind};
use rasm_parser::lexer::Lexer;
use rasm_parser::parser::Parser;
use rasm_parser::parser::ast::ASTExpression::{self, ASTFunctionCallExpression};
use rasm_parser::parser::ast::{ASTModifiers, ASTModule, ASTPosition, ASTStatement, ASTValue};

const STD_LIB_VERSION: &str = "0.1";

#[derive(Debug, Clone)]
pub struct RasmProject {
    root: PathBuf,
    config: RasmConfig,
    from_file: bool,
    // TODO don't like them here, is something that has to do with the IDE
    in_memory_files: LinkedHashMap<PathBuf, String>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct RasmSubProject {
    pub path: String,
}

impl RasmSubProject {
    pub fn is_main(&self) -> bool {
        self.path == "main"
    }

    pub fn is_test(&self) -> bool {
        self.path == "test"
    }

    pub fn main() -> Self {
        Self {
            path: "main".to_string(),
        }
    }

    pub fn test() -> Self {
        Self {
            path: "test".to_string(),
        }
    }

    pub fn safe_name(&self) -> String {
        self.path.replace('/', "_")
    }
}

#[derive(RustEmbed)]
#[folder = "../core/resources/corelib"]
pub struct RasmCoreLibAssets;

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialEq)]
pub struct ResolvedDependency {
    version: Version,
    #[derivative(PartialEq = "ignore")]
    project: RasmProject,
}

impl Display for ResolvedDependency {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}={}", self.project.config.package.name, self.version)
    }
}

impl ResolvedDependency {
    pub fn new(version: Version, project: RasmProject) -> Self {
        Self { version, project }
    }

    pub fn project(&self) -> &RasmProject {
        &self.project
    }

    pub fn version(&self) -> &Version {
        &self.version
    }
}

impl RasmProject {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root: root.clone(),
            config: get_rasm_config(root.as_path()),
            from_file: !root.is_dir(),
            in_memory_files: LinkedHashMap::new(),
        }
    }

    pub fn profiles(&self) -> Vec<RasmProfile> {
        let mut profiles = vec![RasmProfile::Main];

        if self.rasm_source_folder(&RasmSubProject::test()).is_some() {
            profiles.push(RasmProfile::Test);
        }

        let examples = self.root.join(self.source_folder()).join("examples");
        if examples.exists() {
            for entry in fs::read_dir(examples).unwrap().sorted_by(|a, b| {
                a.as_ref()
                    .unwrap()
                    .file_name()
                    .cmp(&b.as_ref().unwrap().file_name())
            }) {
                let path = entry.unwrap().path();
                if path.is_dir() {
                    if let Some(example_path) =
                        diff_paths(path, self.root.join(self.source_folder()))
                    {
                        if self
                            .rasm_source_folder(&RasmSubProject {
                                path: example_path.to_str().unwrap().to_string(),
                            })
                            .is_some()
                        {
                            profiles.push(RasmProfile::Custom {
                                path: example_path.to_str().unwrap().to_string(),
                            });
                        }
                    }
                }
            }
        }

        profiles
    }

    pub fn add_in_memory_file(&mut self, path: PathBuf, source: String) {
        self.in_memory_files
            .insert(path.canonicalize().unwrap(), source);
    }

    pub fn main_src_file(&self, sub_project: &RasmSubProject) -> Option<PathBuf> {
        let path = if sub_project.is_main() {
            self.config.package.main.clone().and_then(|main| {
                self.rasm_source_folder(sub_project)
                    .map(|it| it.join(Path::new(&main)))
            })
        } else {
            self.rasm_source_folder(sub_project)
                .map(|it| it.join("main.rasm"))
        };

        if let Some(path) = path {
            if path.exists() { Some(path) } else { None }
        } else {
            None
        }
    }

    pub fn out_folder(&self) -> PathBuf {
        if !self.root.is_dir() {
            return PathBuf::from(".");
        }

        let target = self.root.join("target");

        if !target.exists() {
            fs::create_dir(&target).unwrap_or_else(|_| {
                panic!("Error creating target folder {}", target.to_string_lossy())
            });
        }

        target

        /*
        if is_test {
            Some(target.join("test"))
        } else {
            self.config
                .package
                .out
                .clone()
                .map(|it| target.join(Path::new(&it)))
        }
        */
    }

    pub fn rasm_source_folder(&self, sub_project: &RasmSubProject) -> Option<PathBuf> {
        if self.is_dir() {
            let path = Path::new(&self.root).join(
                Path::new(
                    &self
                        .config
                        .package
                        .source_folder
                        .as_ref()
                        .unwrap_or(&"src".to_string()),
                )
                .join(&sub_project.path)
                .join("rasm"),
            );

            if path.exists() { Some(path) } else { None }
        } else {
            if !sub_project.is_main() {
                None
            } else {
                Some(Path::new(&self.config.package.source_folder.as_ref().unwrap()).to_path_buf())
            }
        }
    }

    pub fn source_folder(&self) -> PathBuf {
        if self.is_dir() {
            PathBuf::from(
                &self
                    .config
                    .package
                    .source_folder
                    .as_ref()
                    .unwrap_or(&"src".to_string()),
            )
        } else {
            Path::new(&self.config.package.source_folder.as_ref().unwrap()).to_path_buf()
        }
    }

    pub fn native_source_folder(
        &self,
        sub_project: &RasmSubProject,
        native: &str,
    ) -> Option<PathBuf> {
        if self.is_dir() {
            let path = Path::new(&self.root).join(
                Path::new(
                    self.config
                        .package
                        .source_folder
                        .as_ref()
                        .unwrap_or(&"src".to_string()),
                )
                .join(&sub_project.path)
                .join(native),
            );
            if path.exists() { Some(path) } else { None }
        } else {
            None
        }
    }

    fn is_dir(&self) -> bool {
        Path::new(&self.root).is_dir()
    }

    pub fn from_relative_to_root(&self, path: &Path) -> PathBuf {
        if path.is_absolute() {
            path.to_path_buf()
        } else if self.root.is_dir() {
            self.root.join(path)
        } else {
            self.root.parent().unwrap().join(path)
        }
    }

    pub fn from_relative_to_main_src(
        &self,
        sub_project: &RasmSubProject,
        path: &Path,
    ) -> Option<PathBuf> {
        if path.is_absolute() {
            Some(path.to_path_buf())
        } else {
            self.rasm_source_folder(sub_project).map(|it| it.join(path))
        }
    }

    pub fn relative_to_main_rasm_source_folder(
        &self,
        sub_project: &RasmSubProject,
        path: &Path,
    ) -> Option<PathBuf> {
        let base = if self.root.is_dir() {
            self.rasm_source_folder(sub_project)
                .map(fs::canonicalize)
                .and_then(|it| it.ok())
        } else {
            self.root
                .parent()
                .map(fs::canonicalize)
                .and_then(|it| it.ok())
        };

        if let Some(base) = base {
            diff_paths(
                path.canonicalize()
                    .unwrap_or_else(|_| panic!("cannot canonicalize {:?}", path.to_str())),
                base,
            )
        } else {
            None
        }
    }

    pub fn relative_to_source_folder(&self, path: &Path) -> Option<PathBuf> {
        diff_paths(
            path.canonicalize()
                .unwrap_or_else(|_| panic!("cannot canonicalize {:?}", path.to_str())),
            if self.root.is_dir() {
                self.root.join(self.source_folder()).canonicalize().unwrap()
            } else {
                self.root.parent().unwrap().canonicalize().unwrap()
            },
        )
    }

    pub fn main_out_file_name(&self, command_line_options: &CommandLineOptions) -> String {
        if command_line_options.profile == RasmProfile::Main {
            self.config.package.name.clone()
        } else {
            format!(
                "{}_{}",
                self.config.package.name,
                command_line_options.profile.safe_name()
            )
        }
    }

    fn all_modules(
        &self,
        sub_project: &RasmSubProject,
        target: &CompileTarget,
        is_principal_sub_project: bool,
    ) -> (Vec<(ASTModule, EnhModuleInfo)>, Vec<CompilationError>) {
        let mut modules = Vec::new();
        let mut errors = Vec::new();
        let mut pairs = Vec::new();

        if let Some(rasm_source_folder) = self.rasm_source_folder(sub_project) {
            pairs.push(self.get_modules(
                rasm_source_folder,
                target,
                sub_project,
                is_principal_sub_project,
            ));
        }

        if let Some(native_folder) = self.native_source_folder(sub_project, target.folder()) {
            pairs.push(self.get_modules(
                native_folder,
                target,
                sub_project,
                is_principal_sub_project,
            ));
        }

        /*
        if profile != &RasmProfile::Main {
            if let Some(native_folder) =
                self.native_source_folder(&RasmProfile::Main, target.folder())
            {
                pairs.push(self.get_modules(native_folder, target, profile));
            }
        }
        */

        let log_enabled = log_enabled();

        if is_principal_sub_project {
            pairs.append(
                &mut self
                    .all_projects()
                    .into_par_iter()
                    .map(|dependency| {
                        enable_log(log_enabled);
                        info!("including dependency {}", dependency.config.package.name);
                        //TODO include tests?

                        let mut dep_modules = if let Some(source_folder) =
                            dependency.rasm_source_folder(&RasmSubProject::main())
                        {
                            dependency.get_modules(
                                source_folder,
                                target,
                                &RasmSubProject::main(),
                                false,
                            )
                        } else {
                            println!("no source folder for {}", dependency.config.package.name);
                            Vec::new()
                        };

                        if let Some(native_source_folder) = dependency
                            .native_source_folder(&RasmSubProject::main(), target.folder())
                        {
                            if native_source_folder.exists() {
                                dep_modules.extend(dependency.get_modules(
                                    native_source_folder,
                                    target,
                                    &RasmSubProject::main(),
                                    false,
                                ));
                            }
                        }
                        dep_modules
                    })
                    .collect::<Vec<_>>(),
            );
        }

        pairs
            .into_iter()
            .flatten()
            .for_each(|(project_module, module_errors, info)| {
                modules.push((project_module, info));
                errors.extend(module_errors);
            });

        (modules, errors)
    }

    fn core_modules(
        &self,
        target: &CompileTarget,
    ) -> Vec<(ASTModule, Vec<CompilationError>, EnhModuleInfo)> {
        RasmCoreLibAssets::iter()
            .filter(|it| it.ends_with(".rasm"))
            .filter_map(|it| {
                if it.starts_with("rasm") || it.starts_with(target.folder()) {
                    if let Some(asset) = RasmCoreLibAssets::get(&it) {
                        Some(self.core_module(&it, &asset.data))
                    } else {
                        panic!()
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    pub fn container_and_catalog(
        &self,
        command_line_profile: &RasmProfile,
        target: &CompileTarget,
    ) -> (
        ASTModulesContainer,
        impl ModulesCatalog<EnhModuleId, EnhASTNameSpace> + use<>,
        Vec<CompilationError>,
    ) {
        let mut catalog = RasmProjectCatalog::new();
        let mut container = ASTModulesContainer::new();
        let (mut modules, errors) = self.get_all_modules(command_line_profile, target);

        // let start = Instant::now();

        let mut resources_body = Vec::new();

        Self::add_folder(
            &mut resources_body,
            "RASMSOURCEFOLDER",
            self.root.join(self.source_folder()),
        );

        let resources_module = ASTModule {
            body: resources_body,
            functions: Vec::new(),
            enums: Vec::new(),
            structs: Vec::new(),
            types: Vec::new(),
        };

        modules.push((
            resources_module,
            EnhModuleInfo::new(
                EnhModuleId::Other("resources".to_owned()),
                EnhASTNameSpace::global(),
            ),
        ));

        for (module, info) in modules {
            container.add(
                module,
                info.module_namespace(),
                info.module_id(),
                false,
                !info.namespace.is_same_lib(&self.config.package.name),
            );
            catalog.add(info.id, info.namespace);
        }

        // println!("container_and_catalog takes {:?}", start.elapsed());

        (container, catalog, errors)
    }

    pub fn get_all_modules(
        &self,
        command_line_profile: &RasmProfile,
        target: &CompileTarget,
    ) -> (Vec<(ASTModule, EnhModuleInfo)>, Vec<CompilationError>) {
        if !self
            .rasm_source_folder(&command_line_profile.principal_sub_project())
            .is_some()
        {
            panic!(
                "Cannot find rasm source folder for profile {}",
                command_line_profile.path()
            );
        }
        let (mut modules, mut errors) = self.all_modules(
            &RasmSubProject::main(),
            target,
            command_line_profile == &RasmProfile::Main,
        );

        let core_modules = self.core_modules(target);

        for (module, e, info) in core_modules {
            modules.push((module, info));
            errors.extend(e);
        }

        if command_line_profile != &RasmProfile::Main {
            modules.iter_mut().for_each(|(it, _info)| {
                let new_body = it
                    .body
                    .iter()
                    .filter(|st| {
                        if let ASTStatement::ASTConstStatement(_, _, _, _) = st {
                            true
                        } else {
                            false
                        }
                    })
                    .cloned()
                    .collect::<Vec<_>>();
                it.body = new_body;
            });

            let (profile_modules, profile_errors) =
                self.all_modules(&command_line_profile.principal_sub_project(), target, true);

            modules.extend(profile_modules);
            errors.extend(profile_errors);
        }

        (modules, errors)
    }

    fn targets(&self) -> Vec<String> {
        let targets = vec![NASMI386.to_owned(), C.to_owned()]
            .into_iter()
            .filter(|it| {
                // TODO is correct to detect natives only from main profile?
                self.native_source_folder(&RasmSubProject::main(), it)
                    .is_some()
            })
            .collect_vec();

        if targets.is_empty() {
            vec![NASMI386.to_owned(), C.to_owned()]
        } else {
            targets
        }
    }

    pub fn all_targets(&self) -> Vec<String> {
        let mut targets = self.targets();

        for dep in self.all_projects().iter() {
            let dep_targets = dep.targets();
            let mut new_targets = Vec::new();
            for target in targets {
                if dep_targets.contains(&target) {
                    new_targets.push(target);
                } else {
                    info!(
                        "Dependency {} does not have target {}",
                        dep.config.package.name, target
                    );
                }
            }
            targets = new_targets;
        }

        targets
    }

    fn get_modules(
        &self,
        source_folder: PathBuf,
        target: &CompileTarget,
        sub_project: &RasmSubProject,
        is_principal_sub_project: bool,
    ) -> Vec<(ASTModule, Vec<CompilationError>, EnhModuleInfo)> {
        if self.from_file {
            let main_src_file = self.main_src_file(sub_project).unwrap();

            let name = main_src_file
                .with_extension("")
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string();

            let path = PathBuf::from(&main_src_file).canonicalize().unwrap();
            let namespace = EnhASTNameSpace::new(self.config.package.name.clone(), name);
            let (module, errors) = self.module_from_file(&path);

            vec![(
                module,
                errors,
                EnhModuleInfo::new(EnhModuleId::Path(path), namespace),
            )]
        } else {
            let log_enabled = log_enabled();
            WalkDir::new(source_folder)
                .into_iter()
                .collect::<Vec<_>>()
                .into_par_iter()
                .filter_map(Result::ok)
                .filter(|it| it.file_name().to_str().unwrap().ends_with(".rasm"))
                .filter_map(|entry| {
                    enable_log(log_enabled);
                    self.get_module(entry.path(), target, sub_project, is_principal_sub_project)
                })
                .collect::<Vec<_>>()
        }
    }

    /*
    fn get_natives(&self) -> Vec<PathBuf> {
        fs::read_dir(
            Path::new(&self.root).join(
                Path::new(
                    self.config
                        .package
                        .source_folder
                        .as_ref()
                        .unwrap_or(&"src".to_string()),
                )
                .join("main"),
            ),
        )
        .unwrap()
        .map(|it| it.unwrap().path())
        .collect()
    }
    */

    pub fn get_module(
        &self,
        path: &Path,
        target: &CompileTarget,
        sub_project: &RasmSubProject,
        is_principal_sub_project: bool,
    ) -> Option<(ASTModule, Vec<CompilationError>, EnhModuleInfo)> {
        let mut source_folder_opt = None;
        let mut body_opt = None;

        if let Some(native_source_folder) = self.native_source_folder(sub_project, target.folder())
        {
            let path = path.canonicalize().unwrap();

            if path
                .canonicalize()
                .unwrap()
                .starts_with(native_source_folder.canonicalize().unwrap())
            {
                body_opt = Some(false);
                source_folder_opt = Some(native_source_folder);
            }
        }

        let rasm_source_folder = self.rasm_source_folder(sub_project);
        if let Some(rasm_source_folder) = rasm_source_folder
            && path
                .canonicalize()
                .unwrap()
                .starts_with(rasm_source_folder.canonicalize().unwrap())
        {
            body_opt = if let Some(main_src_file) = self.main_src_file(sub_project) {
                if is_principal_sub_project {
                    Some(
                        path.canonicalize().unwrap()
                            == main_src_file.as_path().canonicalize().unwrap(),
                    )
                } else {
                    Some(false)
                }
            } else {
                Some(false)
            };
            source_folder_opt = Some(rasm_source_folder);
        }

        if let Some(source_folder) = source_folder_opt
            && let Some(body) = body_opt
        {
            let relative_path = diff_paths(
                path.canonicalize().unwrap(),
                source_folder.canonicalize().expect(&format!(
                    "cannot find source folder {}",
                    source_folder.to_string_lossy()
                )),
            )
            .unwrap()
            .with_extension("")
            .to_string_lossy()
            .to_string();
            let namespace = EnhASTNameSpace::new(
                self.config.package.name.clone(),
                if sub_project.is_main() || sub_project.is_test() {
                    relative_path
                } else {
                    format!("{}_{}", sub_project.safe_name(), relative_path)
                },
            );
            info!(
                "including file {} namespace {namespace}",
                path.to_str().unwrap()
            );

            let (entry_module, mut module_errors) =
                self.module_from_file(&path.canonicalize().unwrap());
            // const statements are allowed
            let first_body_statement = entry_module.body.iter().find(|it| match it {
                ASTStatement::ASTExpressionStatement(ASTFunctionCallExpression(_), _) => true,
                ASTStatement::ASTLetStatement(_, _, _) => true,
                _ => false,
            });

            if body {
                let is_main = if let Some(main_src_file) = self.main_src_file(sub_project) {
                    path.canonicalize()
                        .unwrap_or_else(|_| panic!("Cannot find {}", path.to_string_lossy()))
                        == main_src_file
                            .canonicalize()
                            .expect("Cannot find main source file")
                } else {
                    false
                };

                if is_main {
                    if first_body_statement.is_none() {
                        Self::add_generic_error(
                            path,
                            &mut module_errors,
                            "Main file should have a body",
                        );
                    }
                } else if let Some(fbs) = first_body_statement {
                    Self::add_generic_error(
                        path,
                        &mut module_errors,
                        &format!(
                            "Only main file should have a body {}, but main is {}",
                            fbs.position(),
                            self.main_src_file(sub_project).unwrap().to_string_lossy()
                        ),
                    );
                }
            } else if let Some(fbs) = first_body_statement {
                if !is_principal_sub_project {
                    Self::add_generic_error(
                        path,
                        &mut module_errors,
                        &format!("Only main file should have a body {},", fbs.position()),
                    );
                }
            }
            Some((
                entry_module,
                module_errors,
                EnhModuleInfo::new(
                    EnhModuleId::Path(path.to_path_buf().canonicalize().unwrap()),
                    namespace,
                ),
            ))
        } else {
            None
        }
    }

    fn add_generic_error(path: &Path, module_errors: &mut Vec<CompilationError>, message: &str) {
        module_errors.push(Self::generic_error(path, message))
    }

    fn generic_error(path: &Path, message: &str) -> CompilationError {
        CompilationError {
            index: EnhASTIndex::new(Some(path.to_path_buf()), ASTPosition::none()),
            error_kind: CompilationErrorKind::Generic(message.to_owned()),
        }
    }

    pub fn dependencies(&self) -> Result<HashMap<String, VersionFilter>, String> {
        let mut result = HashMap::new();

        if let Some(dependencies) = &self.config.dependencies {
            for (dependency_name, dependency) in dependencies {
                if let Value::String(version) = dependency {
                    result.insert(dependency_name.clone(), VersionFilter::parse(version)?);
                } else {
                    return Err(format!(
                        "Unsupport dependency type for {dependency} in project {}. It should be a string",
                        self.config.package.name
                    ));
                }
            }
        }

        Ok(result)
    }

    /// Given a package manager, this function returns all the possible versions of the project's dependencies.
    ///
    /// It returns a vector of hash maps, where each hash map represents a combination of dependencies
    /// that can be used to build the project.
    /// The keys of the hash map are the names of the dependencies, and the values are the versions of the dependencies.
    ///
    /// If there is no version of a dependency that matches the filter, it returns an error.
    ///
    /// The error message will contain the name of the dependency, the filter, and the name of the project.
    fn possible_dependencies(
        &self,
        package_manager: &dyn PackageManager,
    ) -> Result<Vec<HashMap<String, Version>>, String> {
        let mut result: Vec<HashMap<String, Version>> = Vec::new();

        for (lib, filter) in self.dependencies()? {
            let valid_versions = package_manager
                .versions(&lib)
                .into_iter()
                .filter(|v| filter.matches(v))
                .collect_vec();

            if valid_versions.is_empty() {
                return Err(format!(
                    "No version of {} that matches {} in project {}",
                    lib, filter, self.config.package.name
                ));
            }

            let mut new_result = Vec::new();

            for version in valid_versions.iter() {
                if result.is_empty() {
                    let mut row = HashMap::new();
                    row.insert(lib.clone(), version.clone());
                    new_result.push(row);
                } else {
                    for row in result.iter() {
                        let mut new_row = row.clone();
                        new_row.insert(lib.clone(), version.clone());
                        new_result.push(new_row);
                    }
                }
            }

            result = new_result;
        }

        Ok(result)
    }

    fn all_possible_dependencies(
        &self,
        package_manager: &dyn PackageManager,
    ) -> Result<Vec<BTreeMap<String, Version>>, String> {
        let mut result = Vec::new();

        for row in self.possible_dependencies(package_manager)?.into_iter() {
            for (lib, version) in row.iter() {
                let project = package_manager.get_package(&lib, &version).unwrap();
                let lib_possible_dependencies = project.possible_dependencies(package_manager)?;

                if lib_possible_dependencies.is_empty() {
                    let mut new_row = BTreeMap::new();
                    new_row.extend(row.clone());
                    result.push(new_row);
                    continue;
                }
                for lib_row in lib_possible_dependencies {
                    let mut new_row = BTreeMap::new();
                    new_row.extend(row.clone());
                    new_row.extend(lib_row);
                    result.push(new_row);
                }
            }
        }

        result.sort();
        result.dedup();

        let mut filtered_result = Vec::new();
        for row in result.iter() {
            let mut is_valid = true;
            for (lib, version) in row.iter() {
                let project = package_manager.get_package(&lib, &version).unwrap();
                if !project
                    .dependencies()?
                    .iter()
                    .all(|(lib_dependency, filter)| {
                        row.get(lib_dependency).is_some_and(|v| filter.matches(v))
                    })
                {
                    is_valid = false;
                }
            }

            if is_valid {
                filtered_result.push(row.clone());
            }
        }

        if !self.dependencies()?.is_empty() {
            if filtered_result.is_empty() {
                return Err(format!(
                    "No valid dependencies found for {}",
                    self.config.package.name
                ));
            }
        }

        Ok(filtered_result)
    }

    pub fn package_manager(&self) -> impl PackageManager {
        let mut package_manager = PackageManagerImpl::new();
        package_manager
            .register_repository(
                "_local".to_owned(),
                LocalPackageRepository::new(),
                RepositoryAuthentication::None,
            )
            .unwrap();
        package_manager
    }

    pub fn all_projects(&self) -> Vec<RasmProject> {
        let package_manager = self.package_manager();

        let rows = self.all_possible_dependencies(&package_manager).unwrap();

        let mut result = Vec::new();

        if rows.is_empty() {
            warn!("No dependencies found");
            return result;
        }
        for (lib, version) in rows.last().unwrap() {
            let project = package_manager.get_package(&lib, &version).unwrap();
            result.push(project);
        }

        result
    }

    pub fn content_from_file(&self, file: &PathBuf) -> std::io::Result<String> {
        if let Some(content) = self.in_memory_files.get(file) {
            Ok(content.to_string())
        } else {
            fs::read_to_string(file)
        }
    }

    fn module_from_file(
        &self,
        file: &PathBuf,
        //namespace: ASTNameSpace,
    ) -> (ASTModule, Vec<CompilationError>) {
        let main_path = Path::new(file);

        if let Some(content) = self.in_memory_files.get(file) {
            let lexer = Lexer::new(content.clone());
            let parser = Parser::new(lexer);
            let (module, errors) = parser.parse();
            return (
                module,
                errors
                    .into_iter()
                    .map(|it| CompilationError::from_parser_error(it, Some(file.clone())))
                    .collect::<Vec<_>>(),
            );
        }

        match Lexer::from_file(main_path) {
            Ok(lexer) => {
                let parser = Parser::new(lexer);
                let (module, errors) = parser.parse();
                (
                    module,
                    errors
                        .into_iter()
                        .map(|it| CompilationError::from_parser_error(it, Some(file.clone())))
                        .collect::<Vec<_>>(),
                )
            }
            Err(err) => {
                panic!("An error occurred: {}", err)
            }
        }
    }

    fn core_module(
        &self,
        main_file: &str,
        data: &[u8],
    ) -> (ASTModule, Vec<CompilationError>, EnhModuleInfo) {
        // info!("core module for {main_file}");
        // print!("source:\n{}", String::from_utf8_lossy(data));
        let main_path = Path::new(&main_file);

        let namespace = EnhASTNameSpace::new(
            "::core".to_string(),
            main_path.with_extension("").to_string_lossy().to_string(),
        );
        // for now, we don't set the file name in the Lexer and the Parser, because it is not readable as a standard file,
        // and we can get errors trying to open it for example in an IDE
        let lexer = Lexer::new(String::from_utf8_lossy(data).to_string());

        let parser = Parser::new(lexer);
        let (module, errors) = parser.parse();
        (
            module,
            errors
                .into_iter()
                .map(|it| CompilationError::from_parser_error(it, Some(main_path.to_path_buf())))
                .collect::<Vec<_>>(),
            EnhModuleInfo::new(EnhModuleId::Path(main_path.to_path_buf()), namespace),
        )
    }

    fn add_folder(statements: &mut Vec<ASTStatement>, name: &str, folder: PathBuf) {
        let path = if folder.exists() {
            folder.canonicalize().unwrap().to_string_lossy().to_string()
        } else {
            String::new()
        };
        let stmt = ASTStatement::ASTConstStatement(
            name.to_owned(),
            ASTExpression::ASTValueExpression(ASTValue::ASTStringValue(path), ASTPosition::none()),
            ASTPosition::none(),
            ASTModifiers::public(),
        );
        statements.push(stmt);
    }

    pub fn name(&self) -> &str {
        &self.config.package.name
    }

    pub fn version(&self) -> &str {
        &self.config.package.version
    }

    pub fn root(&self) -> &PathBuf {
        &self.root
    }

    pub fn get_native_string_array(&self, target: &str, key: &str) -> Vec<String> {
        if let Some(ref targets) = self.config.targets {
            if let Some(target_value) = targets.get(target) {
                if let Value::Table(target_table) = target_value {
                    if let Some(value) = target_table.get(key) {
                        if let Value::Array(a) = value {
                            a.iter().map(|req| {
                                    if let Value::String(s) = req {
                                        s.clone()
                                    } else {
                                        panic!(
                                            "{target}/{key} should be an array of strings {}/rasm.toml",
                                            self.root.to_string_lossy()
                                        );
                                    }
                                }).collect::<Vec<_>>()
                        } else {
                            panic!(
                                "{target}/{key} should be an array in {}/rasm.toml, but is {}",
                                self.root.to_string_lossy(),
                                OptionDisplay(&target_table.get(key))
                            );
                        }
                    } else {
                        Vec::new()
                    }
                } else {
                    panic!(
                        "{target} should be a table in {}/rasm.toml",
                        self.root.to_string_lossy()
                    );
                }
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        }
    }

    pub fn main(&self) -> &Option<String> {
        &self.config.package.main
    }
}

#[derive(Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct RasmPackage {
    pub name: String,
    pub version: String,
    pub main: Option<String>,
    pub source_folder: Option<String>,
}

#[derive(Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct RasmConfig {
    pub package: RasmPackage,
    pub dependencies: Option<Table>,
    pub targets: Option<Table>,
}

fn get_rasm_config(src_path: &Path) -> RasmConfig {
    if !src_path.exists() {
        panic!("path does not exists {}", src_path.to_string_lossy());
    }
    if src_path.is_dir() {
        get_rasm_config_from_directory(src_path)
    } else {
        get_rasm_config_from_file(src_path)
    }
}

fn get_rasm_config_from_file(src_path: &Path) -> RasmConfig {
    let parent = src_path.parent().unwrap().to_string_lossy().to_string();
    let main_name = src_path.file_name().unwrap().to_string_lossy().to_string();
    let name = src_path
        .with_extension("")
        .file_name()
        .unwrap()
        .to_string_lossy()
        .to_string();
    let mut dependencies_map = Map::new();

    dependencies_map.insert(
        "stdlib".to_owned(),
        Value::String(STD_LIB_VERSION.to_owned()),
    );

    RasmConfig {
        package: RasmPackage {
            name,
            version: "1.0.0".to_owned(),
            source_folder: Some(parent.to_owned()),
            main: Some(main_name.to_string()),
        },
        dependencies: Some(dependencies_map),
        targets: None,
    }
}

fn get_rasm_config_from_directory(src_path: &Path) -> RasmConfig {
    let toml_file = src_path.join(Path::new("rasm.toml"));
    if !toml_file.exists() {
        panic!("Cannot find rasm.toml");
    }
    let mut s = String::new();
    if let Ok(mut file) = File::open(toml_file.clone()) {
        if let Ok(_size) = file.read_to_string(&mut s) {
            match toml::from_str::<RasmConfig>(&s) {
                Ok(config) => {
                    if let Err(msg) = semver::Version::parse(&config.package.version) {
                        panic!("Invalid semver version in rasm.toml: {}", msg);
                    }
                    config
                }
                Err(err) => panic!("Error parsing {:?} :\n{}", toml_file, err),
            }
        } else {
            panic!("Cannot parse rasm.toml");
        }
    } else {
        panic!("Cannot open rasm.toml");
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::env;
    use std::path::{Path, PathBuf};

    use linked_hash_map::LinkedHashMap;
    use rasm_parser::catalog::modules_catalog::ModulesCatalog;
    use semver::Version;
    use toml::Value;

    use crate::codegen::c::options::COptions;
    use crate::codegen::compile_target::CompileTarget;
    use crate::codegen::enh_ast::EnhModuleId;
    use crate::commandline::RasmProfile;
    use crate::pm::repository::PackageManager;
    use crate::project::{RasmConfig, RasmPackage};

    use super::RasmProject;

    struct PackageManagerMock {
        projects: HashMap<String, Vec<RasmProject>>,
    }

    impl PackageManager for PackageManagerMock {
        fn get_package(&self, name: &str, version: &Version) -> Option<RasmProject> {
            self.projects
                .get(name)
                .and_then(|projects| {
                    projects
                        .iter()
                        .find(|p| p.config.package.version == version.to_string())
                })
                .cloned()
        }

        fn install_package(
            &self,
            _repository_id: &str,
            _project: &RasmProject,
            _command_line_options: &crate::commandline::CommandLineOptions,
        ) -> Result<(), String> {
            todo!()
        }

        fn versions(&self, lib: &str) -> Vec<Version> {
            self.projects
                .get(lib)
                .map_or(Ok(Vec::new()), |packages| {
                    packages
                        .iter()
                        .map(|p| {
                            Version::parse(&p.config.package.version).map_err(|e| e.to_string())
                        })
                        .collect::<Result<Vec<Version>, String>>()
                })
                .unwrap()
        }
    }

    #[test]
    fn test_canonilize() {
        let current_dir = env::current_dir().unwrap();

        let path = Path::new("resources/test/../test/./test1.rasm");

        assert_eq!(
            path.canonicalize().unwrap(),
            current_dir.join(PathBuf::from("resources/test/test1.rasm"))
        );
    }

    #[test]
    fn test_catalog() {
        let sut = RasmProject::new(PathBuf::from("resources/test/helloworld.rasm"));

        let target = CompileTarget::C(COptions::default());
        let (_container, catalog, _errors) = sut.container_and_catalog(&RasmProfile::Main, &target);

        let info = catalog.info(&EnhModuleId::Path(
            PathBuf::from("resources/test/helloworld.rasm")
                .canonicalize()
                .unwrap(),
        ));

        assert_eq!(
            "helloworld:helloworld",
            format!("{}", info.unwrap().namespace())
        );
    }

    #[test]
    fn resolved_dependencies() {
        let sut = test_project("main", "1.0.0", vec![("A", "1.0"), ("B", "1.0")]);

        let mut projects = HashMap::new();
        projects.insert(
            "A".to_owned(),
            vec![
                test_project("A", "1.0.0", vec![("C", "1.0")]),
                test_project("A", "1.0.1", vec![("C", "1.1")]),
            ],
        );
        projects.insert(
            "B".to_owned(),
            vec![
                test_project("B", "1.0.0", vec![("C", "1.1")]),
                test_project("B", "1.1.0", vec![("C", "2.0")]),
            ],
        );
        projects.insert(
            "C".to_owned(),
            vec![
                test_project("C", "1.0.0", vec![]),
                test_project("C", "1.1.0", vec![]),
                test_project("C", "2.0.0", vec![]),
            ],
        );

        let package_manager = PackageManagerMock { projects };

        let rows = sut.all_possible_dependencies(&package_manager).unwrap();

        println!("Resolved dependencies");
        for row in rows.iter() {
            for (lib, version) in row {
                print!("{} {}, ", lib, version);
            }
            println!();
        }
    }

    #[test]
    fn test_profiles() {
        let sut = RasmProject::new(PathBuf::from("resources/test/profiles"));

        assert_eq!(
            sut.profiles(),
            vec![
                RasmProfile::Main,
                RasmProfile::Test,
                RasmProfile::Custom {
                    path: "examples/example1".to_owned()
                },
                RasmProfile::Custom {
                    path: "examples/helloworld".to_owned()
                }
            ]
        );
    }

    fn test_project(name: &str, version: &str, dependencies: Vec<(&str, &str)>) -> RasmProject {
        let config = RasmConfig {
            package: RasmPackage {
                name: name.to_owned(),
                version: version.to_owned(),
                main: None,
                source_folder: None,
            },
            dependencies: Some(
                dependencies
                    .into_iter()
                    .map(|(d, v)| (d.to_owned(), Value::String(v.to_owned())))
                    .collect(),
            ),
            targets: None,
        };
        RasmProject {
            root: PathBuf::new(),
            config: config,
            from_file: false,
            in_memory_files: LinkedHashMap::new(),
        }
    }
}
