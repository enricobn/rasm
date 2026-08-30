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

use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

use crate::codegen::enh_ast::{EnhASTIndex, EnhModuleId};
use crate::enh_type_check::enh_type_check_error::EnhTypeCheckError;
use rasm_parser::parser::ParserError;

#[derive(Clone, Debug)]
pub struct CompilationError {
    pub index: EnhASTIndex,
    pub error_kind: CompilationErrorKind,
}

impl CompilationError {
    pub fn from_parser_error(error: ParserError, file_name: Option<PathBuf>) -> Self {
        let module_id = file_name
            .map(EnhModuleId::Path)
            .unwrap_or_else(EnhModuleId::none);
        Self {
            index: EnhASTIndex::new(module_id, error.position().clone()),
            error_kind: CompilationErrorKind::Parser(error.message),
        }
    }

    pub fn generic(index: EnhASTIndex, message: String) -> Self {
        Self {
            index,
            error_kind: CompilationErrorKind::Generic(message),
        }
    }

    pub fn generic_none(message: String) -> Self {
        Self {
            index: EnhASTIndex::none(),
            error_kind: CompilationErrorKind::Generic(message),
        }
    }

    pub fn message(&self) -> &str {
        match &self.error_kind {
            CompilationErrorKind::Generic(message) => message,
            CompilationErrorKind::Lexer(message) => message,
            CompilationErrorKind::Parser(message) => message,
            CompilationErrorKind::TypeCheck(message, _) => message,
            CompilationErrorKind::Verify(message) => message,
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
                f.write_str(&format!("{message} in {}\n", self.index))?;
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

pub fn filter_compilation_errors(errors: Vec<CompilationError>) -> Vec<CompilationError> {
    let mut result = Vec::new();

    let mut indexes = &mut HashSet::new();

    for error in errors {
        result.push(filter_compilation_error(error, &mut indexes));
    }

    result
}

fn filter_compilation_error(
    error: CompilationError,
    indexes: &mut HashSet<(EnhASTIndex, String)>,
) -> CompilationError {
    match error.error_kind {
        CompilationErrorKind::Generic(_) => error,
        CompilationErrorKind::Lexer(_) => error,
        CompilationErrorKind::Parser(_) => error,
        CompilationErrorKind::TypeCheck(message, enh_type_check_errors) => CompilationError {
            index: error.index,
            error_kind: CompilationErrorKind::TypeCheck(
                message,
                filter_enh_type_check_errors(enh_type_check_errors, indexes),
            ),
        },
        CompilationErrorKind::Verify(_) => error,
    }
}

fn filter_enh_type_check_errors(
    errors: Vec<EnhTypeCheckError>,
    indexes: &mut HashSet<(EnhASTIndex, String)>,
) -> Vec<EnhTypeCheckError> {
    let mut result = Vec::new();

    for error in errors {
        if !indexes.contains(&(error.main.0.clone(), error.main.1.clone())) {
            indexes.insert((error.main.0.clone(), error.main.1.clone()));
            if let Some(error) = filter_enh_type_check_error(error, indexes) {
                result.push(error);
            }
        }
    }

    result
}

fn filter_enh_type_check_error(
    error: EnhTypeCheckError,
    indexes: &mut HashSet<(EnhASTIndex, String)>,
) -> Option<EnhTypeCheckError> {
    let (index, message, stack) = error.main;
    let messages = error
        .messages
        .iter()
        .map(|(i, m, s)| (i.clone(), m.clone(), s.clone()))
        .collect();
    let children: Vec<EnhTypeCheckError> = error
        .children
        .into_iter()
        .filter(|e| !indexes.contains(&(e.main.0.clone(), e.main.1.clone())))
        .collect();

    let children = filter_enh_type_check_errors(children, indexes);

    Some(EnhTypeCheckError {
        kind: error.kind,
        main: (index, message, stack),
        messages,
        children,
        dummy: error.dummy,
    })
}
