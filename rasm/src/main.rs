extern crate core;

use std::env;
use std::io::Write;
use std::path::Path;
use std::time::Instant;

use clap::{Arg, ArgAction, Command};
use env_logger::Builder;
use log::info;
use rasm_core::codegen::compile_target::{CompileTarget, C, NASMI386};
use rasm_utils::debug_i;

use rasm_core::project::RasmProject;
use rasm_server::server::rasm_server;

use rasm_core::commandline::{CommandLineAction, CommandLineOptions};
use rasm_ui::UI;

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
                .value_parser(["build", "test", "server", "ui"])
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("target")
                .short('t')
                .help("the compiler target")
                .required(true)
                .value_parser([NASMI386, C])
                .default_value(NASMI386)
                .required(false),
        )
        .arg(
            Arg::new("out")
                .short('o')
                .help("the output folder of generated artifacts, if not set, the \"target\" folder under the project's root")
                .required(false),
        )
        .arg(
            Arg::new("compile")
                .help("creates only .asm/.c and .o files")
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
                .help("prints debug informations at runtime (very verbose)")
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
                .help("the input directory or file")
                .required(false)
                .index(2),
        )
        .arg(
            Arg::new("release")
                .help("optimize for release")
                .long("release")
                .short('r')
                .action(ArgAction::SetTrue)
                .required(false),
        )
        .arg(
            Arg::new("arguments")
                .help("arguments to be passed to main/test when run")
                .long("arguments")
                .required(false),
        )
        .arg(
            Arg::new("include-tests")
                .help("a comma separated list of test functions to be included")
                .long("include-tests")
                .required(false),
        )
        .get_matches();

    let current_path = env::current_dir().unwrap();

    info!("Current dir: {:?}", current_path);

    let src = matches
        .get_one::<String>("file")
        .cloned()
        .unwrap_or(".".to_string());

    let action = match matches
        .get_one::<String>("ACTION")
        .cloned()
        .unwrap()
        .as_str()
    {
        "build" => CommandLineAction::Build,
        "test" => CommandLineAction::Test,
        "server" => CommandLineAction::Server,
        "ui" => CommandLineAction::UI,
        it => panic!("Unsupported action {it}"),
    };

    let command_line_options = CommandLineOptions {
        action,
        debug: matches.get_flag("debug"),
        print_code: matches.get_flag("printcode"),
        print_memory: matches.get_flag("memoryinfo"),
        only_compile: matches.get_flag("compile"),
        out: matches.get_one::<String>("out").cloned(),
        release: matches.get_flag("release"),
        arguments: matches
            .get_one::<String>("arguments")
            .map_or(Vec::new(), |it| vec![it.clone()]),
        include_tests: matches
            .get_one::<String>("include-tests")
            .map_or(Vec::new(), |it| {
                it.split(',').map(|it| it.to_string()).into_iter().collect()
            }),
    };

    let src_path = Path::new(&src);

    let project = RasmProject::new(src_path.to_path_buf());

    let target = CompileTarget::from(
        matches.get_one::<String>("target").cloned().unwrap(),
        &project,
        &command_line_options,
    );

    if command_line_options.action == CommandLineAction::Server {
        rasm_server(project);
    } else if command_line_options.action == CommandLineAction::UI {
        UI::show(project, target);
    } else {
        debug_i!("project {:?}", project);

        let resource_folder = project.main_resources_folder();
        info!("resource folder: {:?}", resource_folder);

        target.run(project, command_line_options);

        info!("finished in {:?}", start.elapsed());
    }
}
