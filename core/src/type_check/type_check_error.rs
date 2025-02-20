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

use crate::codegen::enh_ast::EnhASTIndex;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCheckErrorKind {
    Standard,
    Ignorable,
    Important,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCheckError {
    pub kind: TypeCheckErrorKind,
    pub main: (EnhASTIndex, String, Vec<EnhASTIndex>),
    pub messages: Vec<(EnhASTIndex, String, Vec<EnhASTIndex>)>,
    children: Vec<TypeCheckError>,
    dummy: bool,
}

impl TypeCheckError {
    pub fn new(index: EnhASTIndex, message: String, stack: Vec<EnhASTIndex>) -> Self {
        TypeCheckError {
            kind: TypeCheckErrorKind::Standard,
            main: (index, message, stack),
            messages: Vec::new(),
            children: Vec::new(),
            dummy: false,
        }
    }

    pub fn new_with_kind(
        index: EnhASTIndex,
        message: String,
        stack: Vec<EnhASTIndex>,
        kind: TypeCheckErrorKind,
    ) -> Self {
        TypeCheckError {
            kind,
            main: (index, message, stack),
            messages: Vec::new(),
            children: Vec::new(),
            dummy: false,
        }
    }

    pub fn dummy() -> Self {
        TypeCheckError {
            kind: TypeCheckErrorKind::Ignorable,
            main: (EnhASTIndex::none(), "Dummy".to_string(), Vec::new()),
            messages: Vec::new(),
            children: Vec::new(),
            dummy: true,
        }
    }

    pub fn add(self, index: EnhASTIndex, message: String, stack: Vec<EnhASTIndex>) -> Self {
        let mut result = self.clone();

        result.messages.push((index, message, stack));
        result
    }

    pub fn add_errors(self, errors: Vec<TypeCheckError>) -> Self {
        let mut result = self.clone();

        result.children.extend(errors);
        result
    }

    fn write_one(
        &self,
        f: &mut Formatter<'_>,
        indent: usize,
        prev_stack: &Vec<EnhASTIndex>,
    ) -> std::fmt::Result {
        let spaces = " ".repeat(indent * 2);

        let kind = if self.kind == TypeCheckErrorKind::Important {
            "!"
        } else {
            ""
        };

        f.write_str(&format!(
            "{spaces}{kind}{} : {}\n",
            self.main.1, self.main.0
        ))?;

        if matches!(self.kind, TypeCheckErrorKind::Ignorable) {
            //f.write_str(&format!("{spaces}  ignored\n"))?;
            // return Ok(());
        }

        if !self.messages.is_empty() {
            // f.write_str(&format!("{spaces}messages :\n"))?;

            for (index, message, stack) in self.messages.iter() {
                if index.file_name.is_some() {
                    f.write_str(&format!("{spaces}{} : {}\n", message, index))?;

                    /*
                    for i in stack.iter().rev() {
                        f.write_str(&format!("{spaces}  {}\n", i))?;
                    }
                    */
                }
            }
            //f.write_str(&format!("{spaces}end messages\n"))?;
        }

        let inner_stack = self
            .main
            .2
            .iter()
            .filter(|it| !prev_stack.contains(it))
            .collect::<Vec<_>>();

        if !inner_stack.is_empty() {
            f.write_str(&format!("{spaces}call stack :\n"))?;

            for i in inner_stack.iter().rev() {
                if i.file_name.is_some() {
                    f.write_str(&format!("{spaces}{}\n", i))?;
                }
            }
        }

        if !self.children.is_empty() {
            f.write_str(&format!("{spaces}children :\n"))?;

            let mut stack = self.main.2.clone();
            stack.push(self.main.0.clone());

            for child in self.children.iter() {
                child.write_one(f, indent + 1, &stack)?;
            }
        }

        Ok(())
    }

    pub fn is_dummy(&self) -> bool {
        self.dummy
    }

    pub fn important(&self) -> Vec<&TypeCheckError> {
        if self.kind == TypeCheckErrorKind::Important {
            vec![self]
        } else {
            self.children.iter().flat_map(|it| it.important()).collect()
        }
    }
}

impl Display for TypeCheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.write_one(f, 0, &Vec::new())
    }
}
