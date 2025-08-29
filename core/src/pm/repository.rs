use std::{
    fs::{self, DirBuilder},
    io::{self, stdout, Write},
    path::{Path, PathBuf},
};

use dirs::home_dir;
use itertools::Itertools;
use linked_hash_map::LinkedHashMap;
use log::info;

use crate::{
    codegen::compile_target::CompileTarget,
    commandline::{CommandLineAction, CommandLineOptions},
    project::RasmProject,
};

pub enum VersionFilter {
    Exact(String),
    Compatible(String),
    Latest,
}

impl VersionFilter {
    pub fn matches(&self, version: &str) -> bool {
        match self {
            VersionFilter::Exact(v) => v == version,
            VersionFilter::Compatible(v) => version.starts_with(v),
            VersionFilter::Latest => true,
        }
    }
}

pub enum RepositoryAuthentication {
    None,
}

pub struct RepositoryAuthorization {
    read: bool,
    write: bool,
}

pub trait PackageRepository {
    fn get_package(&self, name: &str, version: &str) -> Result<RasmProject, String>;

    fn install_package(
        &self,
        project: &RasmProject,
        command_line_options: &CommandLineOptions,
    ) -> Result<(), String>;

    fn authenticate(
        &self,
        auth: &RepositoryAuthentication,
    ) -> Result<RepositoryAuthorization, String>;

    fn versions(&self, lib: &str) -> Vec<String>;

    fn libs(&self) -> Vec<String>;
}

pub trait PackageManager {
    fn register_repository(
        &mut self,
        id: String,
        repository: impl PackageRepository + 'static,
        authentication: RepositoryAuthentication,
    ) -> Result<(), String>;

    fn get_package(&self, name: &str, version: &VersionFilter) -> Option<RasmProject>;

    fn install_package(
        &self,
        repository_id: Option<&str>,
        project: &RasmProject,
        command_line_options: &CommandLineOptions,
    ) -> Result<(), String>;
}

struct LocalPackageRepository {}

impl PackageRepository for LocalPackageRepository {
    fn get_package(&self, name: &str, version: &str) -> Result<RasmProject, String> {
        let path = self.repository_folder().join(name).join(version);

        if !path.exists() {
            return Err(format!("version {} of {} not found", version, name));
        }
        /*
        let mut matches = self
            .versions(name)
            .into_iter()
            .filter(|(v, _)| version.matches(v))
            .collect_vec();
        if matches.len() == 0 {
            return Err(format!("no version of {} found", name));
        }
        if matches.len() > 1 {
            matches.sort_by(|a, b| a.0.cmp(&b.0).reverse());
        }
        */

        Ok(RasmProject::new(path))
    }

    fn install_package(
        &self,
        project: &RasmProject,
        command_line_options: &CommandLineOptions,
    ) -> Result<(), String> {
        if project.config.package.main.is_some() {
            return Err("You cannot install a project with a main".to_owned());
        }

        let destination_folder = self
            .repository_folder()
            .join(project.config.package.name.clone())
            .join(project.config.package.version.clone());

        info!(
            "installing {} to {}",
            project.root.as_os_str().to_string_lossy(),
            destination_folder.as_os_str().to_string_lossy()
        );

        if destination_folder.exists() {
            print!("library has already been installed, do you want to overwrite it? (y/n) ");
            stdout().flush().unwrap();
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            if input.trim() != "y" {
                return Err("Aborting".to_owned());
            }
        }

        if project.main_test_src_file().is_some() {
            let mut test_command_line_options = command_line_options.clone();
            test_command_line_options.action = CommandLineAction::Test;
            for native in project.targets() {
                info!("running tests for native {}", native);
                let native_target =
                    CompileTarget::from(native.clone(), &project, &command_line_options);
                native_target.run(project.clone(), test_command_line_options.clone());
            }
        }

        if destination_folder.exists() {
            fs::remove_dir_all(&destination_folder).unwrap();
        }

        DirBuilder::new()
            .recursive(true)
            .create(&destination_folder)
            .unwrap();

        fs::copy(
            project.root.join("rasm.toml"),
            destination_folder.join("rasm.toml"),
        )
        .expect("rasm.toml copy failed");

        let source_folder = project
            .config
            .package
            .source_folder
            .clone()
            .unwrap_or("src".to_owned());

        if let Err(msg) = copy_dir_all(
            project.root.join(source_folder.clone()),
            destination_folder.join(source_folder),
        ) {
            panic!("copy failed: {}", msg);
        }

        println!(
            "Successfully installed {} to {}",
            project.config.package.name,
            destination_folder.as_os_str().to_string_lossy()
        );
        Ok(())
    }

    fn authenticate(
        &self,
        _auth: &RepositoryAuthentication,
    ) -> Result<RepositoryAuthorization, String> {
        Ok(RepositoryAuthorization {
            read: true,
            write: true,
        })
    }

    fn versions(&self, lib: &str) -> Vec<String> {
        self.repository_folder()
            .join(lib)
            .read_dir()
            .unwrap()
            .map(|entry| {
                let entry = entry.unwrap();
                let path = entry.path();
                let version = path.file_name().unwrap().to_str().unwrap().to_owned();
                version
            })
            .collect()
    }

    fn libs(&self) -> Vec<String> {
        self.repository_folder()
            .read_dir()
            .unwrap()
            .map(|entry| {
                let entry = entry.unwrap();
                let path = entry.path();
                let lib = path.file_name().unwrap().to_str().unwrap().to_owned();
                lib
            })
            .collect()
    }
}

impl LocalPackageRepository {
    pub fn new() -> Self {
        Self {}
    }

    fn repository_folder(&self) -> PathBuf {
        let home_dir = home_dir().expect("home dir not found");

        home_dir.join(".rasm/repository")
    }
}

pub struct PackageManagerImpl {
    repositories: LinkedHashMap<String, (Box<dyn PackageRepository>, RepositoryAuthorization)>,
}

impl PackageManager for PackageManagerImpl {
    fn register_repository(
        &mut self,
        id: String,
        repository: impl PackageRepository + 'static,
        authentication: RepositoryAuthentication,
    ) -> Result<(), String> {
        if self.repositories.contains_key(&id) {
            return Err(format!("repository {} already registered", id));
        }
        let authorization = repository.authenticate(&authentication)?;
        self.repositories.insert(
            id,
            (
                Box::new(repository) as Box<dyn PackageRepository>,
                authorization,
            ),
        );
        Ok(())
    }

    fn get_package(&self, lib: &str, version_filter: &VersionFilter) -> Option<RasmProject> {
        let mut versions = LinkedHashMap::new();

        // we get all the matching versions from all the repositories retaining repository precedence
        for (_, (repository, authorization)) in &self.repositories {
            if authorization.read {
                for version in repository.versions(lib) {
                    if version_filter.matches(&version) {
                        if !versions.contains_key(&version) {
                            versions.insert(version, repository);
                        }
                    }
                }
            }
        }

        let mut versions = versions
            .iter()
            .map(|(version, repository)| (version.clone(), repository))
            .collect_vec();

        // TODO does we retain repository precedence?
        versions.sort_by(|a, b| a.0.cmp(&b.0).reverse());

        for (version, repository) in versions {
            // TODO must we fail if the package is not found?
            if let Ok(project) = repository.get_package(lib, &version) {
                return Some(project);
            }
        }

        None
    }

    fn install_package(
        &self,
        repository_id: Option<&str>,
        project: &RasmProject,
        command_line_options: &CommandLineOptions,
    ) -> Result<(), String> {
        let id = if let Some(id) = repository_id {
            id
        } else {
            "_local"
        };
        let (repository, authentication) = if let Some(value) = self.repositories.get(id) {
            value
        } else {
            return Err(format!("repository {} not found", id));
        };
        if !authentication.write {
            return Err("not authorized to write".to_owned());
        }
        repository.install_package(project, command_line_options)
    }
}

impl PackageManagerImpl {
    pub fn new() -> Self {
        let mut repositories = LinkedHashMap::new();
        let local_repository = LocalPackageRepository::new();
        let authorization = local_repository
            .authenticate(&RepositoryAuthentication::None)
            .unwrap();
        repositories.insert(
            "_local".to_owned(),
            (
                Box::new(local_repository) as Box<dyn PackageRepository>,
                authorization,
            ),
        );
        Self { repositories }
    }
}

fn copy_dir_all(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> io::Result<()> {
    fs::create_dir_all(&dst)?; // Create destination directory if it doesn't exist

    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let file_type = entry.file_type()?;

        let src_path = entry.path();
        let dst_path = dst.as_ref().join(entry.file_name());

        if file_type.is_dir() {
            copy_dir_all(&src_path, &dst_path)?; // Recursive copy for directories
        } else {
            fs::copy(&src_path, &dst_path)?; // Copy file
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {

    use crate::pm::repository::PackageManagerImpl;

    #[test]
    pub fn test() {
        let package_manager = PackageManagerImpl::new();

        assert_eq!(1, package_manager.);
    }
}
