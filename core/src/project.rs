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

use std::cmp::Ordering;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::codegen::c::any::CInclude;
use crate::codegen::compile_target::CompileTarget;
use crate::commandline::CommandLineOptions;
use linked_hash_map::LinkedHashMap;
use log::info;
use pathdiff::diff_paths;
use rayon::prelude::*;
use rust_embed::RustEmbed;
use serde::Deserialize;
use toml::map::Map;
use toml::{Table, Value};
use walkdir::WalkDir;

use crate::codegen::get_std_lib_path;
use crate::codegen::statics::Statics;
use crate::errors::{CompilationError, CompilationErrorKind};
use crate::lexer::Lexer;
use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
use crate::parser::ast::{ASTIndex, ASTModule, ASTNameSpace, ASTStatement, ASTType};
use crate::parser::Parser;
use crate::transformations::enrich_module;

#[derive(Debug, Clone)]
pub struct RasmProject {
    pub root: PathBuf,
    pub config: RasmConfig,
    pub from_file: bool,
    pub in_memory_files: LinkedHashMap<PathBuf, String>,
}
#[derive(RustEmbed)]
#[folder = "../core/resources/corelib/rasm"]
pub struct RasmCoreLibAssets;

impl RasmProject {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root: root.clone(),
            config: get_rasm_config(root.as_path()),
            from_file: !root.is_dir(),
            in_memory_files: LinkedHashMap::new(),
        }
    }

    pub fn add_in_memory_file(&mut self, path: PathBuf, source: String) {
        self.in_memory_files
            .insert(path.canonicalize().unwrap(), source);
    }

    pub fn main_src_file(&self) -> Option<PathBuf> {
        self.config
            .package
            .main
            .clone()
            .map(|it| self.main_rasm_source_folder().join(Path::new(&it)))
    }

    pub fn out_file(&self, is_test: bool) -> Option<PathBuf> {
        if !self.root.is_dir() {
            if is_test {
                panic!("Unsupported option test when compiling a single file.")
            }
            return Some(PathBuf::from(
                self.root.with_extension("").file_name().unwrap(),
            ));
        }

        let target = self.root.join("target");
        if !target.exists() {
            fs::create_dir(&target).unwrap_or_else(|_| {
                panic!("Error creating target folder {}", target.to_string_lossy())
            });
        }

        if is_test {
            Some(target.join("test"))
        } else {
            self.config
                .package
                .out
                .clone()
                .map(|it| target.join(Path::new(&it)))
        }
    }

    pub fn main_rasm_source_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.root).join(
                Path::new(
                    &self
                        .config
                        .package
                        .source_folder
                        .as_ref()
                        .unwrap_or(&"src".to_string()),
                )
                .join("main/rasm"),
            )
        } else {
            Path::new(&self.config.package.source_folder.as_ref().unwrap()).to_path_buf()
        }
    }

    pub fn source_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.root).join(Path::new(
                &self
                    .config
                    .package
                    .source_folder
                    .as_ref()
                    .unwrap_or(&"src".to_string()),
            ))
        } else {
            Path::new(&self.config.package.source_folder.as_ref().unwrap()).to_path_buf()
        }
    }

    pub fn main_resources_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.root).join(
                Path::new(
                    &self
                        .config
                        .package
                        .source_folder
                        .as_ref()
                        .unwrap_or(&"src".to_string()),
                )
                .join("main/resources"),
            )
        } else {
            Path::new(&self.config.package.source_folder.as_ref().unwrap()).to_path_buf()
        }
    }

    pub fn test_rasm_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.root).join(
                Path::new(
                    &self
                        .config
                        .package
                        .source_folder
                        .as_ref()
                        .unwrap_or(&"src".to_string()),
                )
                .join("test/rasm"),
            )
        } else {
            Path::new(&self.config.package.source_folder.as_ref().unwrap()).to_path_buf()
        }
    }

    pub fn test_resources_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.root).join(
                Path::new(
                    self.config
                        .package
                        .source_folder
                        .as_ref()
                        .unwrap_or(&"src".to_string()),
                )
                .join("test/resources"),
            )
        } else {
            Path::new(self.config.package.source_folder.as_ref().unwrap()).to_path_buf()
        }
    }

    pub fn main_native_source_folder(&self, native: &str) -> Option<PathBuf> {
        if self.is_dir() {
            Some(
                Path::new(&self.root).join(
                    Path::new(
                        self.config
                            .package
                            .source_folder
                            .as_ref()
                            .unwrap_or(&"src".to_string()),
                    )
                    .join("main")
                    .join(native),
                ),
            )
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

    pub fn from_relative_to_main_src(&self, path: &Path) -> PathBuf {
        if path.is_absolute() {
            path.to_path_buf()
        } else {
            self.main_rasm_source_folder().join(path)
        }
    }

    pub fn relative_to_main_rasm_source_folder(&self, path: &Path) -> Option<PathBuf> {
        diff_paths(
            path.canonicalize()
                .unwrap_or_else(|_| panic!("cannot canonicalize {:?}", path.to_str())),
            if self.root.is_dir() {
                self.main_rasm_source_folder().canonicalize().unwrap()
            } else {
                self.root.parent().unwrap().canonicalize().unwrap()
            },
        )
    }

    pub fn relative_to_source_folder(&self, path: &Path) -> Option<PathBuf> {
        diff_paths(
            path.canonicalize()
                .unwrap_or_else(|_| panic!("cannot canonicalize {:?}", path.to_str())),
            if self.root.is_dir() {
                self.source_folder().canonicalize().unwrap()
            } else {
                self.root.parent().unwrap().canonicalize().unwrap()
            },
        )
    }

    pub fn relative_to_root_test(&self, path: &Path) -> Option<PathBuf> {
        diff_paths(
            path.canonicalize()
                .unwrap_or_else(|_| panic!("cannot canonicalize {:?}", path.to_str())),
            if self.root.is_dir() {
                self.test_rasm_folder().canonicalize().unwrap()
            } else {
                self.root.parent().unwrap().canonicalize().unwrap()
            },
        )
    }

    fn all_test_modules(
        &self,
        statics: &mut Statics,
        target: &CompileTarget,
        debug: bool,
    ) -> (Vec<ASTModule>, Vec<CompilationError>) {
        info!("Reading tests");

        let mut modules = Vec::new();
        let mut errors = Vec::new();

        self.get_modules(self.test_rasm_folder(), target)
            .into_iter()
            .for_each(|(mut project_module, module_errors)| {
                enrich_module(&target, statics, &mut project_module, debug);
                modules.push(project_module);
                errors.extend(module_errors);
            });

        (modules, errors)
    }

    fn all_modules(
        &self,
        statics: &mut Statics,
        target: &CompileTarget,
        debug: bool,
        for_tests: bool,
        out: &PathBuf,
    ) -> (Vec<ASTModule>, Vec<CompilationError>) {
        let mut modules = Vec::new();
        let mut errors = Vec::new();
        let mut pairs = vec![self.core_modules(target)];

        pairs.push(self.get_modules(self.main_rasm_source_folder(), target));

        if let Some(native_folder) = self.main_native_source_folder(target.folder()) {
            if native_folder.exists() {
                pairs.push(self.get_modules(native_folder, target));
            }
        }

        pairs.append(
            &mut self
                .get_all_dependencies()
                .into_par_iter()
                .map(|dependency| {
                    info!("including dependency {}", dependency.config.package.name);
                    //TODO include tests?
                    let mut dep_modules =
                        dependency.get_modules(dependency.main_rasm_source_folder(), target);
                    if let Some(native_source_folder) =
                        dependency.main_native_source_folder(target.folder())
                    {
                        if native_source_folder.exists() {
                            dep_modules
                                .extend(dependency.get_modules(native_source_folder, target));
                        }
                    }
                    dep_modules
                })
                .collect::<Vec<_>>(),
        );

        if matches!(target, CompileTarget::C(..)) {
            self.get_all_dependencies().iter().for_each(|dependency| {
                if let Some(native_source_folder) =
                    dependency.main_native_source_folder(target.folder())
                {
                    if native_source_folder.exists() {
                        WalkDir::new(native_source_folder)
                            .into_iter()
                            .filter_map(Result::ok)
                            .filter(|it| it.file_name().to_string_lossy().ends_with(".h"))
                            .for_each(|it| {
                                CInclude::add_to_statics(
                                    statics,
                                    format!("\"{}\"", it.clone().file_name().to_string_lossy()),
                                );

                                let dest = out
                                    .parent()
                                    .unwrap()
                                    .join(Path::new(it.file_name().to_string_lossy().as_ref()));

                                info!("including file {}", it.path().to_string_lossy());

                                fs::copy(it.clone().into_path(), dest).unwrap();
                            });
                    }
                }
            });
        }

        pairs
            .into_iter()
            .flatten()
            .for_each(|(mut project_module, module_errors)| {
                enrich_module(target, statics, &mut project_module, debug);
                modules.push(project_module);
                errors.extend(module_errors);
            });

        (modules, errors)
    }

    fn core_modules(&self, target: &CompileTarget) -> Vec<(ASTModule, Vec<CompilationError>)> {
        let mut result = RasmCoreLibAssets::iter()
            .filter(|it| it.ends_with(".rasm"))
            .map(|it| {
                if let Some(asset) = RasmCoreLibAssets::get(&it) {
                    self.core_module(&it, &asset.data)
                } else {
                    panic!()
                }
            })
            .collect::<Vec<_>>();

        result.append(
            &mut target
                .get_core_lib_files()
                .iter()
                .map(|it| self.core_module(it.0, &it.1.data))
                .collect::<Vec<_>>(),
        );

        result
    }

    pub fn get_all_modules(
        &self,
        statics: &mut Statics,
        for_tests: bool,
        target: &CompileTarget,
        debug: bool,
        out: &PathBuf,
        options: &CommandLineOptions,
    ) -> (Vec<ASTModule>, Vec<CompilationError>) {
        let (mut modules, mut errors) = self.all_modules(statics, target, debug, for_tests, out);

        if for_tests {
            modules.iter_mut().for_each(|it| {
                let new_body = it
                    .body
                    .iter()
                    .filter(|st| {
                        if let ASTStatement::LetStatement(_, _, is_const, _) = st {
                            *is_const
                        } else {
                            false
                        }
                    })
                    .cloned()
                    .collect::<Vec<_>>();
                it.body = new_body;
            });
            let (test_modules, test_errors) = self.all_test_modules(statics, target, debug);

            let test_module = self.main_test_module(&mut errors, &test_modules, options);

            modules.push(test_module);
            modules.extend(test_modules);
            errors.extend(test_errors);
        }

        (modules, errors)
    }

    fn main_test_module(
        &self,
        errors: &mut Vec<CompilationError>,
        test_modules: &[ASTModule],
        options: &CommandLineOptions,
    ) -> ASTModule {
        let mut module_src = String::new();

        module_src.push_str("let test = argv(1).getOrElse(\"_ALL_\");");

        module_src.push_str("let tests = Vec()");

        let mut count = 0;

        test_modules
            .iter()
            .flat_map(|it| it.functions.iter())
            .filter(|it| {
                it.modifiers.public
                    && it.name.starts_with("test")
                    && (options.include_tests.is_empty()
                        || options.include_tests.contains(&it.name))
            })
            .for_each(|it| {
                let valid = if let ASTType::Custom {
                    namespace: _,
                    name,
                    param_types: _,
                    index: _,
                } = &it.return_type
                {
                    name == "Assertions"
                } else {
                    false
                };

                if !valid {
                    errors.push(CompilationError {
                        index: it.index.clone(),
                        error_kind: CompilationErrorKind::Generic(
                            "Test function must return Assertions".to_string(),
                        ),
                    });
                } else {
                    module_src.push_str(&format!(
                        ".push(RunTest(\"{}\", \"{}\",{{{}();}}))\n",
                        it.name, it.index, it.name
                    ));
                    count += 1;
                }
            });

        if count == 0 {
            module_src = "println(\"No tests found!\");\n".to_string();
        } else {
            module_src.push_str(";\n");
            module_src.push_str("if(tests.filter(fn(it){test.eq(\"_ALL_\").or(it.name.eq(test));}).foldLeft(false, fn(prev,it) {prev.or(it.run());}), { println(\"Errors\");}, {});\n");
        }

        // let mut test_main_module_body = Vec::new();
        // let mut expr = ASTExpression::Value(ValueType::Boolean(false), ASTIndex::none());
        let namespace = ASTNameSpace::new(self.config.package.name.clone(), "".to_string());

        let (module, errors) = Parser::new(Lexer::new(module_src.clone(), None), None)
            .parse(&Path::new(""), &namespace);

        if !errors.is_empty() {
            println!("generated test code:\n{module_src}");

            for error in errors {
                println!("{error}");
            }
            panic!();
        }

        return module;
    }

    fn get_modules(
        &self,
        source_folder: PathBuf,
        target: &CompileTarget,
    ) -> Vec<(ASTModule, Vec<CompilationError>)> {
        if self.from_file {
            let main_src_file = self.main_src_file().unwrap();

            let name = main_src_file
                .with_extension("")
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string();

            vec![self.module_from_file(
                &PathBuf::from(&main_src_file).canonicalize().unwrap(),
                ASTNameSpace::new(name.clone(), name),
            )]
        } else {
            WalkDir::new(source_folder)
                .into_iter()
                .collect::<Vec<_>>()
                .into_par_iter()
                .filter_map(Result::ok)
                .filter(|it| it.file_name().to_str().unwrap().ends_with(".rasm"))
                .filter_map(|entry| {
                    let path = entry.path();

                    self.get_module(path, target)
                })
                .collect::<Vec<_>>()
        }
    }

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

    pub fn get_module(
        &self,
        path: &Path,
        target: &CompileTarget,
    ) -> Option<(ASTModule, Vec<CompilationError>)> {
        let (source_folder, body) = if path
            .canonicalize()
            .unwrap()
            .starts_with(self.main_rasm_source_folder().canonicalize().unwrap())
        {
            let body = if let Some(main_src_file) = self.main_src_file() {
                path.canonicalize().unwrap() == main_src_file.as_path().canonicalize().unwrap()
            } else {
                false
            };
            (self.main_rasm_source_folder(), body)
        } else if path.starts_with(self.test_rasm_folder()) {
            (self.test_rasm_folder(), false)
        } else {
            if let Some(native_source_folder) = self.main_native_source_folder(target.folder()) {
                if !native_source_folder.exists() {
                    return None;
                }
                (native_source_folder, false)
            } else {
                return self
                    .get_dependencies()
                    .iter()
                    .filter_map(|project| project.get_module(path, target))
                    .next();
            }
        };

        let namespace = ASTNameSpace::new(
            self.config.package.name.clone(),
            diff_paths(
                path.canonicalize().unwrap(),
                source_folder.canonicalize().expect(&format!(
                    "cannot find source folder {}",
                    source_folder.to_string_lossy()
                )),
            )
            .unwrap()
            .with_extension("")
            .to_string_lossy()
            .to_string(),
        );
        info!(
            "including file {} namespace {namespace}",
            path.to_str().unwrap()
        );

        let (entry_module, mut module_errors) =
            self.module_from_file(&path.canonicalize().unwrap(), namespace);
        // const statements are allowed
        let first_body_statement = entry_module.body.iter().find(|it| match it {
            ASTStatement::Expression(ASTFunctionCallExpression(_)) => true,
            ASTStatement::LetStatement(_, _, is_const, _) => !is_const,
            _ => false,
        });

        if body {
            let is_main = if let Some(main_src_file) = self.main_src_file() {
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
                    &format!("Only main file should have a body {}", fbs.get_index()),
                );
            }
        } else if let Some(fbs) = first_body_statement {
            Self::add_generic_error(
                path,
                &mut module_errors,
                &format!("Only main file should have a body {}", fbs.get_index()),
            );
        }
        Some((entry_module, module_errors))
    }

    fn add_generic_error(path: &Path, module_errors: &mut Vec<CompilationError>, message: &str) {
        module_errors.push(Self::generic_error(path, message))
    }

    fn generic_error(path: &Path, message: &str) -> CompilationError {
        CompilationError {
            index: ASTIndex::new(Some(path.to_path_buf()), 0, 0),
            error_kind: CompilationErrorKind::Generic(message.to_owned()),
        }
    }

    pub fn get_all_dependencies(&self) -> Vec<RasmProject> {
        let mut result = Vec::new();

        if let Some(dependencies) = &self.config.dependencies {
            for dependency in dependencies.values() {
                if let Value::Table(table) = dependency {
                    if let Some(path_value) = table.get("path") {
                        if let Value::String(path) = path_value {
                            let path_buf = if Path::new(path).is_absolute() {
                                PathBuf::from(path)
                            } else {
                                let path_from_relative_to_root =
                                    self.from_relative_to_root(Path::new(path));

                                if !path_from_relative_to_root.exists() {
                                    panic!(
                                        "Cannot find path ot dependency {dependency} in project {}",
                                        self.config.package.name
                                    );
                                }

                                path_from_relative_to_root
                                .canonicalize()
                                .unwrap_or_else(|_| {
                                    panic!(
                                        "error canonicalizing {path}, path_from_relative_to_root {}, root {}",
                                        path_from_relative_to_root.to_string_lossy(),
                                        self.root.to_string_lossy()
                                    )
                                })
                            };

                            let dependency_project = RasmProject::new(path_buf);
                            result.append(&mut dependency_project.get_all_dependencies());
                            result.push(dependency_project);
                        } else {
                            panic!("Unsupported path value for {dependency} : {path_value}");
                        }
                    } else {
                        panic!("Cannot find path for {dependency}");
                    }
                } else {
                    panic!("Unsupported dependency type {dependency}");
                }
            }
        }

        result.sort_by(|a, b| a.root.cmp(&b.root));
        result.dedup_by(|a, b| a.root.cmp(&b.root) == Ordering::Equal);

        result
    }

    fn get_dependencies(&self) -> Vec<RasmProject> {
        let mut result = Vec::new();

        if let Some(dependencies) = &self.config.dependencies {
            for dependency in dependencies.values() {
                if let Value::Table(table) = dependency {
                    if let Some(path_value) = table.get("path") {
                        if let Value::String(path) = path_value {
                            let path_from_relative_to_root =
                                self.from_relative_to_root(Path::new(path));
                            let path_buf = path_from_relative_to_root
                                .canonicalize()
                                .unwrap_or_else(|_| {
                                    panic!(
                                        "error canonicalizing {path}, path_from_relative_to_root {}, root {}",
                                        path_from_relative_to_root.to_string_lossy(),
                                        self.root.to_string_lossy()
                                    )
                                });

                            let dependency_project = RasmProject::new(path_buf);
                            result.push(dependency_project);
                        } else {
                            panic!("Unsupported path value for {dependency} : {path_value}");
                        }
                    } else {
                        panic!("Cannot find path for {dependency}");
                    }
                } else {
                    panic!("Unsupported dependency type {dependency}");
                }
            }
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
        namespace: ASTNameSpace,
    ) -> (ASTModule, Vec<CompilationError>) {
        let main_path = Path::new(file);

        if let Some(content) = self.in_memory_files.get(file) {
            let lexer = Lexer::new(content.clone(), Some(file.clone()));
            let mut parser = Parser::new(lexer, Some(file.clone()));
            let (module, errors) = parser.parse(main_path, &namespace);
            return (module, errors);
        }

        match Lexer::from_file(main_path) {
            Ok(lexer) => {
                let mut parser = Parser::new(lexer, Some(file.clone()));
                let (module, errors) = parser.parse(main_path, &namespace);
                (module, errors)
            }
            Err(err) => {
                panic!("An error occurred: {}", err)
            }
        }
    }

    fn core_module(&self, main_file: &str, data: &[u8]) -> (ASTModule, Vec<CompilationError>) {
        let main_path = Path::new(&main_file);
        // for now, we don't set the file name in the Lexer and the Parser, because it is not readable as a standard file,
        // and we can get errors trying to open it for example in an IDE
        let lexer = Lexer::new(String::from_utf8_lossy(data).parse().unwrap(), None);

        let mut parser = Parser::new(lexer, None);
        let (module, errors) = parser.parse(
            main_path,
            &ASTNameSpace::new(
                "::core".to_string(),
                main_path.with_extension("").to_string_lossy().to_string(),
            ),
        );
        (module, errors)
    }
}

#[derive(Deserialize, Debug, Clone)]
pub struct RasmPackage {
    pub name: String,
    pub version: String,
    pub main: Option<String>,
    pub out: Option<String>,
    pub source_folder: Option<String>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct RasmConfig {
    pub package: RasmPackage,
    pub dependencies: Option<Table>,
    pub natives: Option<Table>,
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
    let mut stdlib = Map::new();
    if let Some(stdlib_path) = get_std_lib_path() {
        stdlib.insert("path".to_owned(), Value::String(stdlib_path));
    } else {
        eprintln!("cannot find stdlib path. Define RASM_STDLIB environment variable.");
    }
    dependencies_map.insert("stdlib".to_owned(), Value::Table(stdlib));

    RasmConfig {
        package: RasmPackage {
            name,
            version: "1.0.0".to_owned(),
            source_folder: Some(parent.to_owned()),
            main: Some(main_name.to_string()),
            out: Some(src_path.with_extension("").to_string_lossy().to_string()),
        },
        dependencies: Some(dependencies_map),
        natives: None,
    }
}

fn get_rasm_config_from_directory(src_path: &Path) -> RasmConfig {
    let toml_file = src_path.join(Path::new("rasm.toml"));
    if !toml_file.exists() {
        panic!("Cannot find rasm.toml");
    }
    let mut s = String::new();
    if let Ok(mut file) = File::open(toml_file) {
        if let Ok(_size) = file.read_to_string(&mut s) {
            match toml::from_str::<RasmConfig>(&s) {
                Ok(config) => config,
                Err(err) => panic!("Error parsing rasm.toml:\n{}", err),
            }
        } else {
            panic!("Cannot read rasm.toml");
        }
    } else {
        panic!("Cannot open rasm.toml");
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::path::{Path, PathBuf};

    #[test]
    fn test_canonilize() {
        let current_dir = env::current_dir().unwrap();

        let path = Path::new("resources/test/../test/./test1.rasm");

        assert_eq!(
            path.canonicalize().unwrap(),
            current_dir.join(PathBuf::from("resources/test/test1.rasm"))
        );
    }
}
