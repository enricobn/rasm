extern crate core;

use std::env;
use std::io::Write;
use std::ops::Add;
use std::path::Path;

use clap::{Arg, ArgAction, Command};
use env_logger::Builder;
use log::info;

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
                .help("Sets the input file to use")
                .required(true)
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

    let path = env::current_dir();

    info!("Current dir: {:?}", path);

    let src = matches.get_one::<String>("SRC").unwrap();
    let out = if let Some(o) = matches.get_one::<String>("OUTPUT") {
        o.clone()
    } else {
        let without_extension = Path::new(src).with_extension("");
        let file_name = without_extension.file_name().unwrap();
        let file_name_str = file_name.to_str().unwrap();
        String::new().add(file_name_str)
    }
    .add(".asm");

    Compiler::compile(
        src.to_string(),
        out,
        get_std_lib_path(),
        matches.get_flag("compile"),
    );
}
