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

use log::info;
use pathdiff::diff_paths;
use rayon::prelude::*;
use serde::Deserialize;
use toml::map::Map;
use toml::{Table, Value};
use walkdir::WalkDir;

use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::errors::{CompilationError, CompilationErrorKind};
use crate::lexer::Lexer;
use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
use crate::parser::ast::{
    ASTExpression, ASTFunctionCall, ASTIndex, ASTLambdaDef, ASTModule, ASTNameSpace, ASTStatement,
    ASTType, ValueType,
};
use crate::parser::Parser;
use crate::transformations::enrich_module;

#[derive(Debug)]
pub struct RasmProject {
    root: PathBuf,
    pub config: RasmConfig,
    from_file: bool,
}

impl RasmProject {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root: root.clone(),
            config: get_rasm_config(root.as_path()),
            from_file: !root.is_dir(),
        }
    }

    pub fn main_src_file(&self) -> Option<PathBuf> {
        self.config
            .package
            .main
            .clone()
            .map(|it| self.source_folder().join(Path::new(&it)))
    }

    pub fn out_file(&self, is_test: bool) -> Option<PathBuf> {
        let target = self.root.join("target");
        if !target.exists() {
            fs::create_dir(&target).expect("Error creating target folder");
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

    pub fn test_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.root).join(Path::new(
                &self
                    .config
                    .package
                    .test_folder
                    .as_ref()
                    .unwrap_or(&"test".to_string()),
            ))
        } else {
            Path::new(&self.config.package.source_folder.as_ref().unwrap()).to_path_buf()
        }
    }

    pub fn resource_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.root).join(Path::new(
                &self
                    .config
                    .package
                    .resource_folder
                    .as_ref()
                    .unwrap_or(&"resources".to_string()),
            ))
        } else {
            Path::new(&self.config.package.resource_folder.as_ref().unwrap()).to_path_buf()
        }
    }

    pub fn test_source_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.root).join(Path::new(
                self.config
                    .package
                    .source_folder
                    .as_ref()
                    .unwrap_or(&"test".to_string()),
            ))
        } else {
            Path::new(&self.config.package.source_folder.as_ref().unwrap()).to_path_buf()
        }
    }

    pub fn test_resource_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.root).join(Path::new(
                self.config
                    .package
                    .resource_folder
                    .as_ref()
                    .unwrap_or(&"testresources".to_string()),
            ))
        } else {
            Path::new(self.config.package.resource_folder.as_ref().unwrap()).to_path_buf()
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

    pub fn relative_to_root(&self, path: &Path) -> Option<PathBuf> {
        diff_paths(
            path.canonicalize()
                .unwrap_or_else(|_| panic!("cannot canonicalize {:?}", path.to_str())),
            if self.root.is_dir() {
                self.root.canonicalize().unwrap()
            } else {
                self.root.parent().unwrap().canonicalize().unwrap()
            },
        )
    }

    fn all_test_modules(
        &self,
        backend: &mut dyn Backend,
        statics: &mut Statics,
    ) -> (Vec<ASTModule>, Vec<CompilationError>) {
        info!("Reading tests");

        let mut modules = Vec::new();
        let mut errors = Vec::new();

        self.get_modules(true, self.test_folder())
            .into_iter()
            .for_each(|(mut project_module, module_errors)| {
                backend.add_module(&project_module);
                enrich_module(backend, statics, &mut project_module);
                modules.push(project_module);
                errors.extend(module_errors);
            });

        (modules, errors)
    }

    fn all_modules(
        &self,
        backend: &mut dyn Backend,
        statics: &mut Statics,
    ) -> (Vec<ASTModule>, Vec<CompilationError>) {
        let mut modules = Vec::new();
        let mut errors = Vec::new();

        self.get_modules(true, self.source_folder())
            .into_iter()
            .for_each(|(mut project_module, module_errors)| {
                backend.add_module(&project_module);
                enrich_module(backend, statics, &mut project_module);
                modules.push(project_module);
                errors.extend(module_errors);
            });

        self.get_all_dependencies()
            .into_par_iter()
            .flat_map_iter(|dependency| {
                info!("including dependency {}", dependency.config.package.name);

                dependency.get_modules(false, dependency.source_folder())
            })
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|(mut project_module, module_errors)| {
                backend.add_module(&project_module);
                enrich_module(backend, statics, &mut project_module);
                modules.push(project_module);
                errors.extend(module_errors);
            });

        (modules, errors)
    }

    pub fn get_all_modules(
        &self,
        backend: &mut dyn Backend,
        statics: &mut Statics,
        for_tests: bool,
    ) -> (Vec<ASTModule>, Vec<CompilationError>) {
        let (mut modules, mut errors) = self.all_modules(backend, statics);

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
            let (test_modules, test_errors) = self.all_test_modules(backend, statics);

            let test_module = self.main_test_module(&mut errors, &test_modules);

            modules.push(test_module);
            modules.extend(test_modules);
            errors.extend(test_errors);
        }

        (modules, errors)
    }

    fn main_test_module(
        &self,
        errors: &mut Vec<CompilationError>,
        test_modules: &Vec<ASTModule>,
    ) -> ASTModule {
        let mut test_main_module_body = Vec::new();
        let mut expr = ASTExpression::Value(ValueType::Boolean(false), ASTIndex::none());

        test_modules
            .iter()
            .flat_map(|it| it.functions.iter())
            .filter(|it| it.name.starts_with("test"))
            .for_each(|it| {
                let valid = if let ASTType::Custom {
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
                    let test_call = ASTFunctionCall {
                        original_function_name: it.original_name.to_string(),
                        function_name: it.name.clone(),
                        parameters: Vec::new(),
                        index: ASTIndex::none(),
                        generics: Vec::new(),
                    };
                    let lambda_def = ASTLambdaDef {
                        parameter_names: Vec::new(),
                        body: vec![ASTStatement::Expression(ASTFunctionCallExpression(
                            test_call,
                        ))],
                        index: ASTIndex::none(),
                    };
                    let run_test_call = ASTFunctionCall {
                        original_function_name: "runTest".to_string(),
                        function_name: "runTest".to_string(),
                        parameters: vec![
                            ASTExpression::StringLiteral(it.name.clone()),
                            ASTExpression::StringLiteral(format!("{}", it.index)),
                            ASTExpression::Lambda(lambda_def),
                        ],
                        index: ASTIndex::none(),
                        generics: Vec::new(),
                    };
                    expr =
                        Self::or_expression(expr.clone(), ASTFunctionCallExpression(run_test_call));
                }
            });

        test_main_module_body.push(ASTStatement::LetStatement(
            "results".to_string(),
            expr,
            false,
            ASTIndex::none(),
        ));

        let panic_call = ASTFunctionCall {
            original_function_name: "panic".to_string(),
            function_name: "panic".to_string(),
            parameters: vec![ASTExpression::StringLiteral("Tests failed.".to_string())],
            index: ASTIndex::none(),
            generics: Vec::new(),
        };
        let panic_lambda = ASTLambdaDef {
            parameter_names: Vec::new(),
            body: vec![ASTStatement::Expression(ASTFunctionCallExpression(
                panic_call,
            ))],
            index: ASTIndex::none(),
        };
        let empty_lambda = ASTLambdaDef {
            parameter_names: Vec::new(),
            body: vec![],
            index: ASTIndex::none(),
        };

        let if_call = ASTFunctionCall {
            original_function_name: "if".to_string(),
            function_name: "if".to_string(),
            parameters: vec![
                ASTExpression::ValueRef("results".to_string(), ASTIndex::none()),
                ASTExpression::Lambda(panic_lambda),
                ASTExpression::Lambda(empty_lambda),
            ],
            index: ASTIndex::none(),
            generics: Vec::new(),
        };

        let if_expression = ASTFunctionCallExpression(if_call);

        test_main_module_body.push(ASTStatement::Expression(if_expression));

        ASTModule {
            path: Default::default(),
            body: test_main_module_body,
            functions: vec![],
            enums: vec![],
            structs: vec![],
            requires: Default::default(),
            externals: Default::default(),
            types: vec![],
            namespace: ASTNameSpace::new(self.config.package.name.clone(), "".to_string()),
        }
    }

    fn or_expression(e1: ASTExpression, e2: ASTExpression) -> ASTExpression {
        let call = ASTFunctionCall {
            original_function_name: "or".to_string(),
            function_name: "or".to_string(),
            parameters: vec![e1, e2],
            index: ASTIndex::none(),
            generics: Vec::new(),
        };
        ASTFunctionCallExpression(call)
    }

    fn get_modules(
        &self,
        body: bool,
        source_folder: PathBuf,
    ) -> Vec<(ASTModule, Vec<CompilationError>)> {
        if self.from_file {
            vec![self.module_from_file(&PathBuf::from(&self.main_src_file().unwrap()))]
        } else {
            WalkDir::new(source_folder)
                .into_iter()
                .collect::<Vec<_>>()
                .into_par_iter()
                .filter_map(Result::ok)
                .filter(|it| it.file_name().to_str().unwrap().ends_with(".rasm"))
                .map(|entry| {
                    let path = entry.path();
                    info!("including file {}", path.to_str().unwrap());

                    let (entry_module, mut module_errors) =
                        self.module_from_file(&path.canonicalize().unwrap());
                    // const statements are allowed
                    let has_body = entry_module.body.iter().any(|it| match it {
                        ASTStatement::Expression(_) => false,
                        ASTStatement::LetStatement(_, _, is_const, _) => !is_const,
                    });

                    if body {
                        let is_main = if let Some(main_src_file) = self.main_src_file() {
                            path.canonicalize().unwrap_or_else(|_| {
                                panic!("Cannot find {}", path.to_string_lossy())
                            }) == main_src_file
                                .canonicalize()
                                .expect("Cannot find main source file")
                        } else {
                            false
                        };

                        if is_main {
                            if !has_body {
                                Self::add_generic_error(
                                    path,
                                    &mut module_errors,
                                    "Main file should have a body",
                                );
                            }
                        } else if has_body {
                            Self::add_generic_error(
                                path,
                                &mut module_errors,
                                "Only main file should have a body",
                            );
                        }
                    } else if has_body {
                        Self::add_generic_error(
                            path,
                            &mut module_errors,
                            "Only main file should have a body",
                        );
                    }
                    (entry_module, module_errors)
                })
                .collect::<Vec<_>>()
        }
    }

    fn add_generic_error(path: &Path, module_errors: &mut Vec<CompilationError>, message: &str) {
        module_errors.push(CompilationError {
            index: ASTIndex::new(Some(path.to_path_buf()), 0, 0),
            error_kind: CompilationErrorKind::Generic(message.to_owned()),
        })
    }

    fn get_all_dependencies(&self) -> Vec<RasmProject> {
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

    fn module_from_file(&self, main_file: &PathBuf) -> (ASTModule, Vec<CompilationError>) {
        let main_path = Path::new(&main_file);
        match Lexer::from_file(main_path) {
            Ok(lexer) => {
                let mut parser = Parser::new(lexer, Some(main_path.to_path_buf()));
                let (module, errors) = parser.parse(Some(self), main_path);
                (module, errors)
            }
            Err(err) => {
                panic!("An error occurred: {}", err)
            }
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct RasmPackage {
    pub name: String,
    pub version: String,
    pub main: Option<String>,
    pub out: Option<String>,
    pub source_folder: Option<String>,
    pub resource_folder: Option<String>,
    pub test_folder: Option<String>,
    pub test_resource_folder: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct RasmConfig {
    pub package: RasmPackage,
    pub dependencies: Option<Table>,
}

fn get_rasm_config(src_path: &Path) -> RasmConfig {
    if src_path.is_dir() {
        get_rasm_config_from_directory(src_path)
    } else {
        get_rasm_config_from_file(src_path)
    }
}

fn get_rasm_config_from_file(src_path: &Path) -> RasmConfig {
    let parent = src_path.parent().unwrap().to_string_lossy().to_string();
    let name = src_path.file_name().unwrap().to_string_lossy().to_string();
    let mut dependencies_map = Map::new();
    let mut stdlib = Map::new();
    stdlib.insert(
        "path".to_owned(),
        Value::String(CodeGen::get_std_lib_path()),
    );
    dependencies_map.insert("stdlib".to_owned(), Value::Table(stdlib));

    RasmConfig {
        package: RasmPackage {
            name: name.to_owned(),
            version: "1.0.0".to_owned(),
            source_folder: Some(parent.to_owned()),
            main: Some(name.to_owned()),
            out: Some(src_path.with_extension("").to_string_lossy().to_string()),
            resource_folder: Some(parent.to_owned()),
            test_folder: Some(parent.to_owned()),
            test_resource_folder: Some(parent.to_owned()),
        },
        dependencies: Some(dependencies_map),
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
        println!("current dir {:?}", current_dir);

        let path = Path::new("resources/test/../test/./test1.rasm");

        println!("path {:?}", path.canonicalize().unwrap());
        assert_eq!(
            path.canonicalize().unwrap(),
            current_dir.join(PathBuf::from("resources/test/test1.rasm"))
        );
    }
}
