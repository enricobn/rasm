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
use rasm_core::parser::ast::{ASTIndex, ASTNameSpace, ASTType};
use rasm_core::utils::OptionDisplay;
use std::fmt::{Display, Formatter};
use std::io;

#[derive(Debug, Clone)]
pub struct SelectableItem {
    pub file_token: FileToken,
    pub namespace: ASTNameSpace,
    pub target: SelectableItemTarget,
}

impl Display for SelectableItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{} -> {}", self.file_token, self.target))
    }
}

impl SelectableItem {
    pub fn new(
        start: ASTIndex,
        len: usize,
        namespace: ASTNameSpace,
        target: SelectableItemTarget,
    ) -> Self {
        SelectableItem {
            file_token: FileToken::new(start, len),
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
    Ref(ASTIndex, Option<ASTType>),
    Function(ASTIndex, ASTType, String),
    Type(Option<ASTIndex>, ASTType),
}

impl Display for SelectableItemTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let descr = match self {
            SelectableItemTarget::Ref(index, t) => format!("Ref({index}, {})", OptionDisplay(t)),
            SelectableItemTarget::Function(index, t, descr) => {
                format!("Function({index}, {t}, {descr})")
            }
            SelectableItemTarget::Type(index, t) => format!("Type({}, {t})", OptionDisplay(index)),
        };

        f.write_str(&descr)
    }
}

impl SelectableItemTarget {
    pub fn index(&self) -> Option<ASTIndex> {
        match self {
            SelectableItemTarget::Ref(index, _) => Some(index.clone()),
            SelectableItemTarget::Function(index, _, _) => Some(index.clone()),
            SelectableItemTarget::Type(index, _) => index.clone(),
        }
    }

    pub fn completion_type(&self) -> Option<ASTType> {
        match self {
            SelectableItemTarget::Ref(_, t) => t.clone(),
            SelectableItemTarget::Function(_, t, _) => Some(t.clone()),
            SelectableItemTarget::Type(_, t) => Some(t.clone()),
        }
    }
}
