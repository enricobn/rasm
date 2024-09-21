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

#[derive(PartialEq, Display)]
pub enum CommandLineAction {
    Build,
    Test,
    Server,
    UI,
}

pub struct CommandLineOptions {
    pub action: CommandLineAction,
    pub debug: bool,
    pub print_code: bool,
    pub print_memory: bool,
    pub only_compile: bool,
    pub out: Option<String>,
    pub release: bool,
    pub arguments: Vec<String>,
    pub include_tests: Vec<String>,
}

impl Default for CommandLineOptions {
    fn default() -> Self {
        Self {
            action: CommandLineAction::Test,
            debug: false,
            print_code: false,
            print_memory: false,
            only_compile: false,
            release: false,
            out: None,
            arguments: Vec::new(),
            include_tests: Vec::new(),
        }
    }
}
