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

use crate::codegen::CodeGenOptions;

#[derive(Clone)]
pub struct COptions {
    pub requires: Vec<String>,
    pub includes: Vec<String>,
}

impl Default for COptions {
    fn default() -> Self {
        Self {
            requires: vec![],
            includes: Vec::new(),
        }
    }
}

impl CodeGenOptions for COptions {
    fn dereference(&self) -> bool {
        true
    }
}
