use std::{
    fmt::Display,
    fs::{self, DirBuilder},
    io::{self, stdout, Write},
    path::{Path, PathBuf},
};

use dirs::home_dir;
use linked_hash_map::LinkedHashMap;
use log::info;
use semver::Version;

use crate::{
    codegen::compile_target::CompileTarget,
    commandline::{CommandLineAction, CommandLineOptions},
    project::RasmProject,
};

pub enum VersionFilter {
    Exact(Version),
    Compatible(u64, u64),
}

impl Display for VersionFilter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VersionFilter::Exact(v) => write!(f, "Exact {}", v),
            VersionFilter::Compatible(major, minor) => {
                write!(f, "Compatible with {}.{}", major, minor)
            }
        }
    }
}

impl VersionFilter {
    pub fn parse(version: &str) -> Result<VersionFilter, String> {
        if let Ok(version) = Version::parse(version) {
            Ok(VersionFilter::Exact(version))
        } else if let Some((major, minor)) = version.split_once('.') {
            if let Ok(major) = major.parse::<u64>() {
                if let Ok(minor) = minor.parse::<u64>() {
                    Ok(VersionFilter::Compatible(major, minor))
                } else {
                    Err(format!("Invalid minor version: {}", minor))
                }
            } else {
                Err(format!("Invalid major version: {}", major))
            }
        } else {
            Err(format!("Invalid version: {}", version))
        }
    }

    pub fn matches(&self, version: &Version) -> bool {
        match self {
            VersionFilter::Exact(v) => v == version,
            VersionFilter::Compatible(major, minor) => {
                version.major == *major && version.minor >= *minor
            }
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
    fn package(&self, name: &str, version: &Version) -> Result<RasmProject, String>;

    fn install_package(
        &self,
        project: &RasmProject,
        command_line_options: &CommandLineOptions,
    ) -> Result<(), String>;

    fn authenticate(
        &self,
        auth: &RepositoryAuthentication,
    ) -> Result<RepositoryAuthorization, String>;

    fn versions(&self, lib: &str) -> Vec<Version>;

    fn libs(&self) -> Vec<String>;
}

pub trait PackageManager {
    fn get_package(&self, name: &str, version: &Version) -> Option<RasmProject>;

    fn install_package(
        &self,
        repository_id: &str,
        project: &RasmProject,
        command_line_options: &CommandLineOptions,
    ) -> Result<(), String>;

    /*
    fn version(&self, lib: &str, version_filter: &VersionFilter) -> Option<String>;
    */

    fn versions(&self, lib: &str) -> Vec<Version>;
}

pub struct LocalPackageRepository {}

impl PackageRepository for LocalPackageRepository {
    fn package(&self, name: &str, version: &Version) -> Result<RasmProject, String> {
        let path = self
            .repository_folder()
            .join(name)
            .join(version.to_string());

        if !path.exists() {
            return Err(format!("version {} of {} not found", version, name));
        }

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

    fn versions(&self, lib: &str) -> Vec<Version> {
        let path = self.repository_folder().join(lib);
        if !path.exists() {
            return Vec::new();
        }
        path.read_dir()
            .unwrap()
            .map(|entry| {
                let entry = entry.unwrap();
                let path = entry.path();
                let version = path.file_name().unwrap().to_str().unwrap().to_owned();
                Version::parse(&version).unwrap()
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
    fn get_package(&self, lib: &str, version: &Version) -> Option<RasmProject> {
        for (_, (repository, authorization)) in &self.repositories {
            if authorization.read {
                if let Ok(project) = repository.package(lib, version) {
                    return Some(project);
                }
            }
        }

        None
    }

    fn install_package(
        &self,
        repository_id: &str,
        project: &RasmProject,
        command_line_options: &CommandLineOptions,
    ) -> Result<(), String> {
        let (repository, authentication) = if let Some(value) = self.repositories.get(repository_id)
        {
            value
        } else {
            return Err(format!("repository {} not found", repository_id));
        };
        if !authentication.write {
            return Err("not authorized to write".to_owned());
        }
        repository.install_package(project, command_line_options)
    }

    /*
    fn version(&self, lib: &str, version_filter: &VersionFilter) -> Option<String> {
        let mut versions = Vec::new();

        // we get all the matching versions from all the repositories retaining repository precedence
        for (_, (repository, authorization)) in &self.repositories {
            if authorization.read {
                for version in repository.versions(lib) {
                    if version_filter.matches(&version) {
                        if !versions.contains(&version) {
                            versions.push(version);
                        }
                    }
                }
            }
        }

        versions.sort_by(|a, b| a.cmp(b).reverse());

        if versions.len() > 0 {
            let version = versions[0].clone();
            Some(version)
        } else {
            None
        }
    }
    */

    fn versions(&self, lib: &str) -> Vec<Version> {
        let mut versions = Vec::new();
        for (_, (repository, auth)) in self.repositories.iter() {
            if auth.read {
                for version in repository.versions(lib) {
                    if !versions.contains(&version) {
                        versions.push(version);
                    }
                }
            }
        }
        versions
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
        Self {
            repositories: LinkedHashMap::new(),
        }
    }

    pub fn register_repository(
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

    use semver::Version;

    use crate::pm::repository::{
        PackageManager, PackageManagerImpl, PackageRepository, RepositoryAuthentication,
        RepositoryAuthorization,
    };

    #[test]
    pub fn test() {
        let mut package_manager = PackageManagerImpl::new();

        package_manager
            .register_repository(
                "_test".to_owned(),
                TestRepository {},
                RepositoryAuthentication::None,
            )
            .unwrap();

        assert_eq!(
            vec![
                Version::parse("0.1.0").unwrap(),
                Version::parse("0.1.1").unwrap()
            ],
            package_manager.versions("lib1")
        );

        assert_eq!(
            vec![
                Version::parse("1.0.0").unwrap(),
                Version::parse("1.0.1").unwrap(),
                Version::parse("2.0.1").unwrap()
            ],
            package_manager.versions("lib2")
        );
    }

    pub struct TestRepository {}

    impl PackageRepository for TestRepository {
        fn package(
            &self,
            _name: &str,
            _version: &Version,
        ) -> Result<crate::project::RasmProject, String> {
            todo!()
        }

        fn install_package(
            &self,
            _project: &crate::project::RasmProject,
            _command_line_options: &crate::commandline::CommandLineOptions,
        ) -> Result<(), String> {
            todo!()
        }

        fn authenticate(
            &self,
            _auth: &super::RepositoryAuthentication,
        ) -> Result<super::RepositoryAuthorization, String> {
            Ok(RepositoryAuthorization {
                read: true,
                write: false,
            })
        }

        fn versions(&self, lib: &str) -> Vec<Version> {
            if lib == "lib1" {
                vec![
                    Version::parse("0.1.0").unwrap(),
                    Version::parse("0.1.1").unwrap(),
                ]
            } else if lib == "lib2" {
                vec![
                    Version::parse("1.0.0").unwrap(),
                    Version::parse("1.0.1").unwrap(),
                    Version::parse("2.0.1").unwrap(),
                ]
            } else {
                Vec::new()
            }
        }

        fn libs(&self) -> Vec<String> {
            vec!["lib1".to_owned(), "lib2".to_owned()]
        }
    }
}
