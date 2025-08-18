use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
};

use log::info;

use crate::{
    codegen::asm::backend::log_command, commandline::CommandLineOptions, project::RasmProject,
};

use rust_embed::RustEmbed;

use super::options::COptions;

#[derive(RustEmbed)]
#[folder = "../core/resources/corelib/c"]
pub struct CLibAssets;

pub fn compile_c(
    command_line_options: &CommandLineOptions,
    options: &COptions,
    project: &RasmProject,
    out_folder: &Path,
    src_paths: Vec<PathBuf>,
    out_file: &PathBuf,
) -> Result<Output, String> {
    let make_file_template =
        String::from_utf8(CLibAssets::get("Makefile").unwrap().data.to_vec()).unwrap();
    let mut make_file_content = String::new();

    make_file_content.push_str("SAVE-TEMPS =\n");
    if command_line_options.release {
        make_file_content.push_str("OPTIMIZE = -O3\n");
        make_file_content.push_str("DEBUG =\n");
    } else {
        make_file_content.push_str("OPTIMIZE = -O0\n");
        make_file_content.push_str("DEBUG = -g\n");
    }

    make_file_content.push_str("OUT = ");
    make_file_content.push_str(out_file.file_name().unwrap().to_str().unwrap());

    let additional_files = generate_pre_compile_artifacts(out_folder);

    make_file_content.push_str("\nOBJECTS =");

    for add_file in src_paths.iter() {
        let mut f = PathBuf::from(add_file);
        f.set_extension("o");
        make_file_content.push_str(" ");
        make_file_content.push_str(f.file_name().unwrap().to_str().unwrap());
    }

    for add_file in additional_files.iter() {
        let mut f = PathBuf::from(add_file);
        f.set_extension("o");
        make_file_content.push_str(" ");
        make_file_content.push_str(f.file_name().unwrap().to_str().unwrap());
    }
    make_file_content.push_str("\n");
    make_file_content.push_str("MAIN = ");
    make_file_content.push_str(&project.main_out_file_name(command_line_options));
    make_file_content.push_str("\n");

    make_file_content.push_str("INCLUDE =");
    for inc in options.includes.iter() {
        make_file_content.push_str(&format!(" -I{inc}"));
    }
    make_file_content.push_str("\n");

    make_file_content.push_str("LIB =");
    for req in options.requires.iter() {
        make_file_content.push_str(&format!(" -l{req}"));
    }
    make_file_content.push_str("\n");

    make_file_content.push_str("\n");

    make_file_content.push_str(&make_file_template);

    let make_file = out_folder.to_path_buf().join("Makefile");

    fs::write(make_file, make_file_content).unwrap();

    let parallelism = format!("-j{}", num_cpus::get_physical());

    let mut command = Command::new("make");

    let args = vec![&parallelism, "-C", out_folder.to_str().unwrap()];

    command.args(args);

    log_command(&command);
    command
        .stderr(Stdio::inherit())
        .output()
        .map_err(|err| format!("failed to execute native compiler: {err}"))
}

fn generate_pre_compile_artifacts(out_folder: &Path) -> Vec<String> {
    let mut source_files_to_include = Vec::new();

    CLibAssets::iter()
        .filter(|it| it.ends_with(".c"))
        .for_each(|it| {
            let dest = out_folder.to_path_buf().join(it.to_string());

            info!("Including {}", dest.to_string_lossy());
            source_files_to_include.push(dest.to_string_lossy().to_string());
            if let Some(asset) = CLibAssets::get(&it) {
                fs::write(dest, asset.data).unwrap();
            } else {
                panic!()
            }
        });

    source_files_to_include
}
