extern crate core;

use std::env;
use std::io::Write;
use std::path::Path;
use std::time::Instant;

use clap::{Arg, ArgAction, Command};
use env_logger::Builder;
use log::debug;
use log::info;
use rasm_core::debug_i;

use rasm_core::project::RasmProject;

use crate::compiler::Compiler;

pub mod compiler;

fn main() {
    let start = Instant::now();

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
            Arg::new("ACTION")
                .help("The action to perform, that can be: build, run, test")
                .required(true)
                .value_parser(["build", "run", "test"])
                .index(1),
        )
        .arg(
            Arg::new("file")
                .short('f')
                .help("Sets the input directory or file to use")
                .required(false),
        )
        .arg(
            Arg::new("out")
                .short('o')
                .help("Sets the output file to use")
                .required(false),
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

    let action = matches.get_one::<String>("ACTION").cloned().unwrap();

    let src = matches
        .get_one::<String>("file")
        .cloned()
        .unwrap_or(".".to_owned());
    let src_path = Path::new(&src);

    let project = RasmProject::new(src_path.to_path_buf());

    debug_i!("project {:?}", project);

    let main_src_file = project
        .main_src_file()
        .expect("undefined main in rasm.toml");
    info!("main: {:?}", main_src_file);

    let resource_folder = project.resource_folder();
    info!("resource folder: {:?}", resource_folder);

    let compiler = Compiler::new(project, matches.get_one::<String>("out"), action == "test");
    compiler.compile(matches.get_flag("compile"));

    info!("finished in {:?}", start.elapsed());
}
