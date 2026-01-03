/*
 *     RASM compiler.
 *     Copyright (C) 2022-2023  Enrico Benedetti
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

use strum_macros::Display;

use crate::project::RasmSubProject;

#[derive(PartialEq, Display, Clone)]
pub enum CommandLineAction {
    Build,
    Install,
    Run,
    Server,
    UI,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum RasmProfile {
    Main,
    Test,
    Custom { path: String },
}

impl RasmProfile {
    pub fn path(&self) -> &str {
        match self {
            RasmProfile::Main => "main",
            RasmProfile::Test => "test",
            RasmProfile::Custom { path } => path,
        }
    }

    pub fn safe_name(&self) -> String {
        match self {
            RasmProfile::Main => "main".into(),
            RasmProfile::Test => "test".into(),
            RasmProfile::Custom { path } => path.replace('/', "_"),
        }
    }

    pub fn principal_sub_project(&self) -> RasmSubProject {
        RasmSubProject {
            path: self.path().to_string(),
        }
    }
}

#[derive(Clone)]
pub struct CommandLineOptions {
    pub action: CommandLineAction,
    pub memory_debug: bool,
    pub print_code: bool,
    pub print_memory: bool,
    pub only_compile: bool,
    pub out: Option<String>,
    pub release: bool,
    pub arguments: Vec<String>,
    pub include_tests: Vec<String>,
    pub debug: bool,
    pub profile: RasmProfile,
}

impl Default for CommandLineOptions {
    fn default() -> Self {
        Self {
            action: CommandLineAction::Run,
            memory_debug: false,
            print_code: false,
            print_memory: false,
            only_compile: false,
            release: false,
            out: None,
            arguments: Vec::new(),
            include_tests: Vec::new(),
            debug: false,
            profile: RasmProfile::Test,
        }
    }
}
