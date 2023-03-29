extern crate core;

use std::env;
use std::io::Write;
use std::ops::Add;
use std::path::Path;

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

    let path = env::current_dir();

    info!("Current dir: {:?}", path);

    let args: Vec<String> = env::args().collect();
    info!("Arguments: {:?}", args);

    let src = args.get(1).unwrap();
    let out = if args.len() < 3 {
        let without_extension = Path::new(src).with_extension("");
        let file_name = without_extension.file_name().unwrap();
        let file_name_str = file_name.to_str().unwrap();
        String::new().add(file_name_str).add(".asm")
    } else {
        args.get(2).unwrap().clone().add(".asm")
    };

    Compiler::compile(
        src.to_string(),
        out,
        env::var("RASM_STDLIB").unwrap_or("stdlib".to_owned()),
    );
}
