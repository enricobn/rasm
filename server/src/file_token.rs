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

use std::fmt::{Display, Formatter};
use std::io;
use std::path::PathBuf;

use rasm_core::codegen::eh_ast::ASTIndex;

#[derive(Debug, Clone)]
pub struct FileToken {
    pub start: ASTIndex,
    pub len: usize,
}

impl FileToken {
    pub fn new(start: ASTIndex, len: usize) -> Self {
        Self { start, len }
    }

    pub fn contains(&self, index: &ASTIndex) -> io::Result<bool> {
        Ok(index.row == self.start.row
            && index.column >= self.start.column
            && index.column <= (self.start.column + self.len - 1)
            && Self::path_matches(&index.file_name, &self.start.file_name)?)
    }

    fn path_matches(op1: &Option<PathBuf>, op2: &Option<PathBuf>) -> io::Result<bool> {
        if let Some(p1) = op1 {
            if let Some(p2) = op2 {
                if p1.file_name() != p2.file_name() {
                    return Ok(false);
                }
                let p1_canon = p1.canonicalize()?;

                let p2_canon = p2.canonicalize()?;

                return Ok(p1_canon == p2_canon);
            }
        }

        Ok(false)
    }
}

impl Display for FileToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let file_name = self
            .start
            .file_name
            .as_ref()
            .and_then(|it| it.to_str().map(|s| format!("file:///{s}")))
            .unwrap_or("".to_string());

        f.write_str(&format!(
            "{file_name} {}:{} len {}",
            self.start.row, self.start.column, self.len
        ))
    }
}
