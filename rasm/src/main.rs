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

use rasm_core::codegen::CodeGen;
use rasm_core::project::project;
use rasm_core::project::project::RasmProject;

use crate::compiler::Compiler;

pub mod compiler;

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

    let project = RasmProject::new(src_path.to_path_buf());

    info!("project {:?}", project);

    let main_src_file = project.main_src_file();
    info!("main {:?}", main_src_file);

    let std_lib_path = project.std_lib_path();
    info!("stdlib path {:?}", std_lib_path);

    let resource_folder = project.resource_folder();
    info!("resource folder {:?}", resource_folder);

    let out = if let Some(o) = matches.get_one::<String>("OUTPUT") {
        Path::new(o).to_path_buf()
    } else {
        Path::new(&main_src_file).with_extension("")
    }
    .with_extension("asm");

    Compiler::compile(
        main_src_file,
        out,
        std_lib_path,
        resource_folder,
        matches.get_flag("compile"),
    );
}
