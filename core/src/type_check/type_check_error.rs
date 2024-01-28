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
    messages: Vec<(ASTIndex, String, Vec<ASTIndex>)>,
}

impl TypeCheckError {
    pub fn new(index: ASTIndex, message: String, stack: Vec<ASTIndex>) -> Self {
        TypeCheckError {
            messages: vec![(index, message, stack)],
        }
    }

    pub fn add(self, index: ASTIndex, message: String, stack: Vec<ASTIndex>) -> Self {
        let mut result = self.messages.clone();
        result.push((index, message, stack));
        TypeCheckError { messages: result }
    }

    pub fn add_errors(self, errors: Vec<TypeCheckError>) -> Self {
        let mut result = self.messages.clone();

        result.append(
            &mut errors
                .into_iter()
                .flat_map(|it| it.messages.into_iter())
                .collect::<Vec<_>>(),
        );
        TypeCheckError { messages: result }
    }
}

impl Display for TypeCheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (index, message, _stack) in self.messages.iter() {
            f.write_str(&format!("{} : {}\n", message, index))?;
            /*
            for i in stack {
                f.write_str(&format!("{}\n", i))?;
            }

             */
        }
        Ok(())
    }
}
