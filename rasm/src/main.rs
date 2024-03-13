extern crate core;

use std::env;
use std::io::Write;
use std::path::Path;
use std::time::Instant;

use clap::{Arg, ArgAction, Command};
use env_logger::Builder;
use log::debug;
use log::info;
use rasm_core::codegen::compile_target::CompileTarget;
use rasm_core::codegen::CodeGenOptions;
use rasm_core::debug_i;

use rasm_core::project::RasmProject;
use rasm_server::server::rasm_server;

use crate::compiler::Compiler;
use rasm_core::utils::OptionDisplay;
use toml::Value;

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
                .help("the action to perform")
                .required(true)
                .value_parser(["build", "test", "server"])
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("out")
                .short('o')
                .help("the output file to create")
                .required(false),
        )
        .arg(
            Arg::new("compile")
                .help("creates only .asm and .o files")
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
        .arg(
            Arg::new("debug")
                .help("prints debug informations at runtime (verbose)")
                .long("debug")
                .short('d')
                .action(ArgAction::SetTrue)
                .required(false),
        )
        .arg(
            Arg::new("memoryinfo")
                .help("prints memory informations")
                .long("memoryinfo")
                .short('m')
                .action(ArgAction::SetTrue)
                .required(false),
        )
        .arg(
            Arg::new("printcode")
                .help("prints code")
                .long("printcode")
                .short('p')
                .action(ArgAction::SetTrue)
                .required(false),
        )
        .arg(
            Arg::new("file")
                .help("the input directory or file to use")
                .required(false)
                .index(2),
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

    if action == "server" {
        rasm_server(project);
    } else {
        debug_i!("project {:?}", project);

        let resource_folder = project.main_resource_folder();
        info!("resource folder: {:?}", resource_folder);

        let debug = matches.get_flag("debug");
        let print_memory = matches.get_flag("memoryinfo");
        let print_code = matches.get_flag("printcode");

        let mut all_projects = vec![project.clone()];
        all_projects.extend(project.get_all_dependencies());
        let mut requires = get_native_string_array(&all_projects, "nasmi386", "requires");
        requires.push("libc".to_string());

        requires.sort();
        requires.dedup_by(|s1, s2| s1 == s2);

        let externals = get_native_string_array(&all_projects, "nasmi386", "externals");

        let options = CodeGenOptions {
            print_memory,
            requires,
            externals,
            ..CodeGenOptions::default()
        };

        let target = CompileTarget::Nasmi386(options.clone());

        let compiler = Compiler::new(
            project,
            matches.get_one::<String>("out"),
            action == "test",
            options,
            target,
            debug,
            print_code,
        );
        compiler.compile(matches.get_flag("compile"));

        info!("finished in {:?}", start.elapsed());
    }

    fn get_native_string_array(projects: &[RasmProject], native: &str, key: &str) -> Vec<String> {
        let mut result = projects
            .iter()
            .flat_map(|it| {
                if let Some(ref natives) = it.config.natives {
                    if let Some(nasm_i386_value) = natives.get(native) {
                        if let Value::Table(nasm_i386_table) = nasm_i386_value {
                            if let Some(value) = nasm_i386_table.get(key) {
                                if let Value::Array(a) = value {
                                    a.iter().map(|req| {
                                        if let Value::String(s) = req {
                                            s.clone()
                                        } else {
                                            panic!(
                                                "{native}/{key} should be an array of strings {}/rasm.toml",
                                                it.root.to_string_lossy()
                                            );
                                        }
                                    }).collect::<Vec<_>>()
                                } else {
                                    panic!(
                                        "{native}/{key} should be an array in {}/rasm.toml, but is {}",
                                        it.root.to_string_lossy(),
                                        OptionDisplay(&nasm_i386_table.get(key))
                                    );
                                }
                            } else {
                                Vec::new()
                            }
                        } else {
                            panic!(
                                "{native} should be a table in {}/rasm.toml",
                                it.root.to_string_lossy()
                            );
                        }
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                }
            })
            .collect::<Vec<_>>();

        result.sort();
        result.dedup_by(|s1, s2| s1 == s2);

        result
    }
}
