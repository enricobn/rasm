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
use std::path::PathBuf;

use crate::codegen::enh_ast::EnhASTIndex;
use crate::enh_type_check::enh_type_check_error::EnhTypeCheckError;
use rasm_parser::parser::ParserError;

#[derive(Clone, Debug)]
pub struct CompilationError {
    pub index: EnhASTIndex,
    pub error_kind: CompilationErrorKind,
}

impl CompilationError {
    pub fn from_parser_error(error: ParserError, file_name: Option<PathBuf>) -> Self {
        Self {
            index: EnhASTIndex::new(file_name, error.position().clone()),
            error_kind: CompilationErrorKind::Parser(error.message),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilationErrorKind {
    Generic(String),
    Lexer(String),
    Parser(String),
    TypeCheck(String, Vec<EnhTypeCheckError>),
    Verify(String),
}

impl Display for CompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.error_kind {
            CompilationErrorKind::Generic(message) => {
                f.write_str(message)?;
                f.write_str(&format!(" in {}", self.index))
            }
            CompilationErrorKind::Lexer(message) => {
                f.write_str(message)?;
                f.write_str(&format!(" in {}", self.index))
            }
            CompilationErrorKind::Parser(message) => {
                f.write_str(message)?;
                f.write_str(&format!(" in {}", self.index))
            }
            CompilationErrorKind::TypeCheck(message, error) => {
                f.write_str(&format!("{message}\n"))?;
                for e in error {
                    f.write_str(&format!("{e}\n"))?
                }

                Ok(())
            }
            CompilationErrorKind::Verify(message) => {
                f.write_str(message)?;
                f.write_str(&format!(" in {}", self.index))
            }
        }
    }
}
