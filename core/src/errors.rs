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

use crate::parser::ast::ASTIndex;
use crate::type_check::type_check_error::TypeCheckError;

#[derive(Clone, Debug)]
pub struct CompilationError {
    pub index: ASTIndex,
    pub error_kind: CompilationErrorKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilationErrorKind {
    Generic(String),
    Lexer(String),
    Parser(String),
    TypeCheck(String, Vec<TypeCheckError>),
    Verify(String),
}

impl Display for CompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.error_kind {
            CompilationErrorKind::Generic(message) => f.write_str(message)?,
            CompilationErrorKind::Lexer(message) => f.write_str(message)?,
            CompilationErrorKind::Parser(message) => f.write_str(message)?,
            CompilationErrorKind::TypeCheck(message, error) => {
                f.write_str(&format!("{message}\n"))?;
                for e in error {
                    f.write_str(&format!("{e}\n"))?
                }
            }
            CompilationErrorKind::Verify(message) => f.write_str(message)?,
        }

        f.write_str(&format!(" in {}", self.index))
    }
}
