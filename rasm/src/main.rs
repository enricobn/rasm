extern crate core;

use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::ops::Add;
use std::path::Path;
use std::process::exit;

use clap::{Arg, ArgAction, Command};
use env_logger::Builder;
use log::info;
use serde::{Deserialize, Serialize};
use toml::Table;

use rasm_core::codegen::CodeGen;

use crate::compiler::Compiler;

pub mod compiler;

#[derive(Deserialize, Debug)]
pub struct RasmPackage {
    name: String,
    version: String,
    source_folder: String,
    std_lib_path: String,
    main: String,
    resource_folder: String,
}

#[derive(Deserialize, Debug)]
struct RasmConfig {
    package: RasmPackage,
}

fn main() {
    Builder::from_default_env()
        .format(|buf, record| {
            writeln!(
                buf,
                "{} [{}] - {}",
                chrono::Local::now().format("%Y-%m-%d %H:%M:%S.%3f"),
                //record.file().unwrap_or("unknown"),
                //record.line().unwrap_or(0),
                record.level(),
                record.args()
            )
        })
        .init();

    let matches = Command::new("RASM lang")
        .version("0.1.0-alpha.0")
        .arg(
            Arg::new("SRC")
                .help("Sets the input directory or file to use")
                .required(false)
                .index(1),
        )
        .arg(
            Arg::new("OUTPUT")
                .help("Sets the output file to use")
                .required(false)
                .index(2),
        )
        .arg(
            Arg::new("compile")
                .help("produces .asm and .o files")
                .long("compile")
                .action(ArgAction::SetTrue)
                .required(false),
        )
        .arg(
            Arg::new("message-format")
                .help("for vscode")
                .long("message-format")
                .required(false),
        )
        .get_matches();

    let current_path = env::current_dir().unwrap();

    info!("Current dir: {:?}", current_path);

    let src = matches
        .get_one::<String>("SRC")
        .cloned()
        .unwrap_or(".".to_owned());
    let src_path = Path::new(&src);

    let (src_file, std_lib_path, resource_folder) = if src_path.is_dir() {
        info!("compiling project form folder {:?}", src_path);
        let toml_file = src_path.join(Path::new("rasm.toml"));
        if !toml_file.exists() {
            panic!("Cannot find rasm.toml");
        }
        let mut s = String::new();
        if let Ok(mut file) = File::open(toml_file) {
            if let Ok(size) = file.read_to_string(&mut s) {
                let config: RasmConfig = toml::from_str(&s).unwrap();
                (
                    src_path
                        .join(Path::new(&config.package.source_folder))
                        .join(Path::new(&config.package.main))
                        .to_str()
                        .unwrap()
                        .to_owned(),
                    src_path
                        .join(Path::new(&config.package.std_lib_path))
                        .to_str()
                        .unwrap()
                        .to_owned(),
                    src_path
                        .join(Path::new(&config.package.resource_folder))
                        .to_str()
                        .unwrap()
                        .to_owned(),
                )
            } else {
                panic!("Cannot read rasm.toml");
            }
        } else {
            panic!("Cannot open rasm.toml");
        }
    } else {
        (
            src,
            CodeGen::get_std_lib_path(),
            current_path.to_str().unwrap().to_owned(),
        )
    };

    info!("resource folder {}", resource_folder);

    let out = if let Some(o) = matches.get_one::<String>("OUTPUT") {
        o.clone()
    } else {
        let without_extension = Path::new(&src_file).with_extension("");
        let file_name = without_extension.file_name().unwrap();
        let file_name_str = file_name.to_str().unwrap();
        String::new().add(file_name_str)
    }
    .add(".asm");

    Compiler::compile(
        src_file,
        out,
        std_lib_path,
        matches.get_flag("compile"),
        resource_folder,
    );
}
