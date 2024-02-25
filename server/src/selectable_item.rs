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

use crate::file_token::FileToken;
use rasm_core::parser::ast::{ASTExpression, ASTIndex, ASTNameSpace, ASTType};
use std::fmt::{Display, Formatter};
use std::io;

#[derive(Debug, Clone)]
pub struct SelectableItem {
    file_token: FileToken,
    pub point_to: ASTIndex,
    expr: Option<ASTExpression>,
    pub ast_type: Option<ASTType>,
    pub ast_type_index: Option<ASTIndex>,
    pub namespace: ASTNameSpace,
    pub target: SelectableItemTarget,
}

impl Display for SelectableItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{} -> {}", self.file_token, self.point_to))
    }
}

impl SelectableItem {
    pub fn new(
        start: ASTIndex,
        len: usize,
        point_to: ASTIndex,
        expr: Option<ASTExpression>,
        ast_type: Option<ASTType>,
        ast_type_index: Option<ASTIndex>,
        namespace: ASTNameSpace,
        target: SelectableItemTarget,
    ) -> Self {
        SelectableItem {
            file_token: FileToken::new(start, len),
            point_to,
            expr,
            ast_type,
            ast_type_index,
            namespace,
            target,
        }
    }

    pub fn contains(&self, index: &ASTIndex) -> io::Result<bool> {
        self.file_token.contains(index)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SelectableItemTarget {
    Ref,
    Function,
    Type,
}
