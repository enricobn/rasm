use std::cmp::Ordering;
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
use crate::parser::ast::{ASTIndex, ASTModule, ASTStatement};
use crate::parser::Parser;
use crate::transformations::enrich_module;

#[derive(Debug)]
pub struct RasmProject {
    src: PathBuf,
    config: RasmConfig,
    from_file: bool,
}

impl RasmProject {
    pub fn new(src: PathBuf) -> Self {
        Self {
            src: src.clone(),
            config: get_rasm_config(src.as_path()),
            from_file: !src.is_dir(),
        }
    }

    pub fn main_src_file(&self) -> Option<PathBuf> {
        self.config.package.main.clone().map(|it| {
            if self.is_dir() {
                Path::new(&self.src)
                    .join(Path::new(&self.config.package.source_folder))
                    .join(Path::new(&it))
            } else {
                Path::new(&self.config.package.source_folder).join(Path::new(&it))
            }
        })
    }

    pub fn resource_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.src).join(Path::new(&self.config.package.resource_folder))
        } else {
            Path::new(&self.config.package.resource_folder).to_path_buf()
        }
    }

    pub fn source_folder(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.src).join(Path::new(&self.config.package.source_folder))
        } else {
            Path::new(&self.config.package.source_folder).to_path_buf()
        }
    }

    fn is_dir(&self) -> bool {
        Path::new(&self.src).is_dir()
    }

    pub fn from_relative_to_root(&self, path: &Path) -> PathBuf {
        if path.is_absolute() {
            path.to_path_buf()
        } else {
            self.source_folder().join(path)
        }
    }

    pub fn relative_to_root(&self, path: &Path) -> Option<PathBuf> {
        diff_paths(
            path.canonicalize()
                .unwrap_or_else(|_| panic!("cannot canonicalize {:?}", path.to_str())),
            self.source_folder().canonicalize().unwrap(),
        )
    }

    pub fn get_all_modules(
        &self,
        backend: &mut dyn Backend,
        statics: &mut Statics,
    ) -> (Vec<ASTModule>, Vec<CompilationError>) {
        info!("Reading project {:?}", self);

        let mut modules = Vec::new();
        let mut errors = Vec::new();

        self.get_modules(true)
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

                dependency.get_modules(false)
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

    fn get_modules(&self, body: bool) -> Vec<(ASTModule, Vec<CompilationError>)> {
        if self.from_file {
            vec![Self::module_from_file(&PathBuf::from(
                &self.main_src_file().unwrap(),
            ))]
        } else {
            WalkDir::new(self.source_folder())
                .into_iter()
                .collect::<Vec<_>>()
                .into_par_iter()
                .filter_map(Result::ok)
                .filter(|it| it.file_name().to_str().unwrap().ends_with(".rasm"))
                .map(|entry| {
                    let path = entry.path();
                    info!("including file {}", path.to_str().unwrap());

                    let (entry_module, mut module_errors) =
                        Self::module_from_file(&path.canonicalize().unwrap());
                    // const statements are allowed
                    let has_body = entry_module.body.iter().any(|it| match it {
                        ASTStatement::Expression(_) => false,
                        ASTStatement::LetStatement(_, _, is_const, _) => !is_const,
                    });

                    if body {
                        if path.canonicalize().unwrap()
                            == self.main_src_file().unwrap().canonicalize().unwrap()
                        {
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
                            let path_buf = self
                                .from_relative_to_root(Path::new(path))
                                .canonicalize()
                                .unwrap_or_else(|_| {
                                    panic!(
                                        "error canonicalizing {path}, root {}",
                                        self.source_folder().to_str().unwrap()
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

        result.sort_by(|a, b| a.src.cmp(&b.src));
        result.dedup_by(|a, b| a.src.cmp(&b.src) == Ordering::Equal);

        result
    }

    fn module_from_file(main_file: &PathBuf) -> (ASTModule, Vec<CompilationError>) {
        let main_path = Path::new(&main_file);
        match Lexer::from_file(main_path) {
            Ok(lexer) => {
                let lexer_errors = lexer.get_errors().clone();
                let mut parser = Parser::new(lexer, Some(main_path.to_path_buf()));
                let (module, mut errors) = parser.parse(main_path);
                errors.extend(lexer_errors);
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
    pub source_folder: String,
    pub main: Option<String>,
    pub resource_folder: String,
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
    let parent = src_path.parent().unwrap().to_str().unwrap();
    let name = src_path.file_name().unwrap().to_str().unwrap();
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
            source_folder: parent.to_owned(),
            main: Some(name.to_owned()),
            resource_folder: parent.to_owned(),
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
            toml::from_str::<RasmConfig>(&s).unwrap()
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
