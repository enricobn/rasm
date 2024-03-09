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

use crate::parser::ast::ASTIndex;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct TypeCheckError {
    main: (ASTIndex, String, Vec<ASTIndex>),
    messages: Vec<(ASTIndex, String, Vec<ASTIndex>)>,
    children: Vec<TypeCheckError>,
    dummy: bool,
}

impl TypeCheckError {
    pub fn new(index: ASTIndex, message: String, stack: Vec<ASTIndex>) -> Self {
        TypeCheckError {
            main: (index, message, stack),
            messages: Vec::new(),
            children: Vec::new(),
            dummy: false,
        }
    }

    pub fn dummy() -> Self {
        TypeCheckError {
            main: (ASTIndex::none(), "Dummy".to_string(), Vec::new()),
            messages: Vec::new(),
            children: Vec::new(),
            dummy: true,
        }
    }

    pub fn add(self, index: ASTIndex, message: String, stack: Vec<ASTIndex>) -> Self {
        let mut result = self.clone();

        result.messages.push((index, message, stack));
        result
    }

    pub fn add_errors(self, errors: Vec<TypeCheckError>) -> Self {
        let mut result = self.clone();

        result.children.extend(errors);
        result
    }

    fn write_one(&self, f: &mut Formatter<'_>, indent: usize) -> std::fmt::Result {
        let spaces = " ".repeat(indent * 2);
        f.write_str(&format!("{spaces}{} : {}\n", self.main.1, self.main.0))?;
        for (index, message, _stack) in self.messages.iter() {
            f.write_str(&format!("{spaces}  {} : {}\n", message, index))?;

            /*
            for i in stack {
                f.write_str(&format!("{}\n", i))?;
            }

             */
        }
        for child in self.children.iter() {
            child.write_one(f, indent + 1)?;
        }
        Ok(())
    }

    pub fn is_dummy(&self) -> bool {
        self.dummy
    }
}

impl Display for TypeCheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.write_one(f, 0)
    }
}
