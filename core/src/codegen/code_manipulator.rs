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

pub trait CodeManipulator {
    fn add_comment(&self, out: &mut String, comment: &str, indent: bool);

    fn add_rows(&self, out: &mut String, code: Vec<&str>, comment: Option<&str>, indent: bool) {
        if let Some(cm) = comment {
            self.add_comment(out, cm, indent);
        }
        for row in code {
            self.add(out, row, None, indent);
        }
    }

    fn add(&self, out: &mut String, code: &str, comment: Option<&str>, indent: bool) {
        if code.is_empty() {
            out.push('\n');
        } else {
            let max = 80;
            let s = format!("{:width$}", code, width = max);
            //assert_eq!(s.len(), max, "{}", s);
            if indent {
                out.push_str("    ");
            }
            out.push_str(&s);
        }

        if let Some(c) = comment {
            self.add_comment(out, c, indent);
        }
        out.push('\n');
    }

    fn add_empty_line(&self, out: &mut String) {
        out.push('\n');
    }

    fn remove_comments_from_line(&self, line: String) -> String;
}

#[derive(Clone)]
pub struct CodeManipulatorNasm;

impl CodeManipulatorNasm {
    pub fn new() -> Self {
        Self {}
    }
}

impl CodeManipulator for CodeManipulatorNasm {
    fn add_comment(&self, out: &mut String, comment: &str, indent: bool) {
        self.add(out, &format!("; {comment}"), None, indent);
    }

    fn remove_comments_from_line(&self, line: String) -> String {
        if let Some(pos) = line.find(';') {
            if pos > 0 {
                line.split_at(pos).0.to_string()
            } else {
                String::new()
            }
        } else {
            line
        }
    }
}
