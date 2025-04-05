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
    mut out_paths: Vec<PathBuf>,
) -> Output {
    if out_paths.len() != 1 {
        panic!("Only one native file to compile is supported!");
    }

    let mut command = Command::new("cc");
    let out_path = out_paths.remove(0);

    let additional_files = generate_pre_compile_artifacts(out_folder);

    let executable = out_folder.join(&project.config.package.name);

    let mut args = vec![out_path.to_string_lossy().to_string()];
    args.extend(additional_files);
    args.extend(vec![
        "-std=c17".to_string(),
        "-o".to_string(),
        executable.to_string_lossy().to_string(),
        "-Wno-incompatible-pointer-types".to_string(), // TODO it happens where using $typeName(T) for a reference type, I think it should be struct RasmPointer_ *
        "-Wno-int-conversion".to_string(),
    ]);
    if command_line_options.release {
        args.push("-O3".to_string());
    } else {
        args.push("-g".to_string());
        args.push("-O0".to_string());
    }

    if !options.includes.is_empty() {
        for inc in options.includes.iter() {
            args.push(format!("-I{inc}"));
        }
    }

    if !options.requires.is_empty() {
        for req in options.requires.iter() {
            args.push(format!("-l{req}"));
        }
    }

    command.args(args);

    log_command(&command);
    command
        .stderr(Stdio::inherit())
        .output()
        .expect("failed to execute native compiler")
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
