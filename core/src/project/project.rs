use std::fs::File;
use std::io::Read;
use std::path::Path;

use log::info;
use serde::Deserialize;

use crate::codegen::CodeGen;

#[derive(Debug)]
pub struct RasmProject {
    src: String,
    config: RasmConfig,
}

impl RasmProject {
    pub fn new(src: String) -> Self {
        Self {
            src: src.clone(),
            config: get_rasm_config(Path::new(&src)),
        }
    }

    pub fn main_src_file(&self) -> String {
        Path::new(&self.config.package.source_folder)
            .join(Path::new(&self.config.package.main))
            .to_str()
            .unwrap()
            .to_owned()
    }

    pub fn std_lib_path(&self) -> String {
        Path::new(&self.config.package.std_lib_path)
            .to_str()
            .unwrap()
            .to_owned()
    }

    pub fn resource_folder(&self) -> String {
        Path::new(&self.src)
            .join(Path::new(&self.config.package.resource_folder))
            .to_str()
            .unwrap()
            .to_owned()
    }

    pub fn source_folder(&self) -> String {
        self.config.package.source_folder.clone()
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
        info!("project form folder {:?}", src_path);

        get_rasm_config_from_directory(src_path)
    } else {
        info!("project form file {:?}", src_path);

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
            let mut config = toml::from_str::<RasmConfig>(&s).unwrap();
            // we transform the relative path to the main dir to a relative path to the current dir
            config.package.std_lib_path = src_path
                .join(Path::new(&config.package.std_lib_path))
                .to_str()
                .unwrap()
                .to_owned();
            config
        } else {
            panic!("Cannot read rasm.toml");
        }
    } else {
        panic!("Cannot open rasm.toml");
    }
}
