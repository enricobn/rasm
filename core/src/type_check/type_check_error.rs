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

use backtrace::Backtrace;

#[derive(Debug)]
pub struct TypeCheckError {
    pub message: String,
}

impl Display for TypeCheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let bt = Backtrace::new();
        println!("{:?}", bt);
        f.write_str(&format!("TypeCheckError({})", &self.message))
    }
}

impl From<&str> for TypeCheckError {
    fn from(s: &str) -> Self {
        TypeCheckError { message: s.into() }
    }
}

impl From<String> for TypeCheckError {
    fn from(s: String) -> Self {
        TypeCheckError { message: s }
    }
}
