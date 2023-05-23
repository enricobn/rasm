use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use log::info;
use pathdiff::diff_paths;
use serde::Deserialize;

use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::lexer::Lexer;
use crate::parser::ast::ASTModule;
use crate::parser::Parser;
use crate::transformations::enrich_module;

#[derive(Debug)]
pub struct RasmProject {
    src: PathBuf,
    config: RasmConfig,
}

impl RasmProject {
    pub fn new(src: PathBuf) -> Self {
        Self {
            src: src.clone(),
            config: get_rasm_config(src.as_path()),
        }
    }

    pub fn main_src_file(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.src)
                .join(Path::new(&self.config.package.source_folder))
                .join(Path::new(&self.config.package.main))
        } else {
            Path::new(&self.config.package.source_folder).join(Path::new(&self.config.package.main))
        }
    }

    pub fn std_lib_path(&self) -> PathBuf {
        if self.is_dir() {
            Path::new(&self.src).join(Path::new(&self.config.package.std_lib_path))
        } else {
            Path::new(&self.config.package.std_lib_path).to_path_buf()
        }
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
        self.source_folder().join(path)
    }

    pub fn relative_to_root(&self, path: &Path) -> Option<PathBuf> {
        diff_paths(
            path.canonicalize().unwrap(),
            self.source_folder().canonicalize().unwrap(),
        )
    }

    pub fn get_module(&self, backend: &dyn Backend, statics: &mut Statics) -> ASTModule {
        let main_src_file = self.main_src_file();
        let file_path = Path::new(&main_src_file);
        let std_lib_path = self.std_lib_path();
        let mut module = match Lexer::from_file(file_path) {
            Ok(lexer) => {
                info!("Lexer ended");
                let mut parser = Parser::new(lexer, Some(file_path.to_path_buf()));
                parser.parse(file_path, Path::new(&std_lib_path))
            }
            Err(err) => {
                panic!("An error occurred: {}", err)
            }
        };

        enrich_module(backend, self.resource_folder(), statics, &mut module);

        module
    }
}

#[derive(Deserialize, Debug)]
pub struct RasmPackage {
    pub name: String,
    pub version: String,
    pub source_folder: String,
    pub std_lib_path: String,
    pub main: String,
    pub resource_folder: String,
}

#[derive(Deserialize, Debug)]
pub struct RasmConfig {
    pub package: RasmPackage,
}

fn get_rasm_config(src_path: &Path) -> RasmConfig {
    if src_path.is_dir() {
        info!("project from folder {:?}", src_path);

        get_rasm_config_from_directory(src_path)
    } else {
        info!("project from file {:?}", src_path);

        get_rasm_config_from_file(src_path)
    }
}

fn get_rasm_config_from_file(src_path: &Path) -> RasmConfig {
    let parent = src_path.parent().unwrap().to_str().unwrap();
    let name = src_path.file_name().unwrap().to_str().unwrap();
    RasmConfig {
        package: RasmPackage {
            name: name.to_owned(),
            version: "1.0.0".to_owned(),
            source_folder: parent.to_owned(),
            std_lib_path: CodeGen::get_std_lib_path(),
            main: name.to_owned(),
            resource_folder: parent.to_owned(),
        },
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
