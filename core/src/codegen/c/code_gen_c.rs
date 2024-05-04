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

use crate::codegen::statics::Statics;
use crate::type_check::typed_ast::ASTTypedModule;

pub struct CodeGenC;

impl CodeGenC {
    pub fn generate(&self, typed_module: &ASTTypedModule, statics: Statics) -> String {
        let mut result = String::new();

        let custom = statics.custom();
        if let Some(includes) = custom.get("include") {
            let mut cloned_includes = includes.clone();
            cloned_includes.sort();
            cloned_includes.dedup();

            for i in cloned_includes {
                result.push_str(&format!("#include {i}\n"));
            }

            if !includes.is_empty() {
                result.push('\n');
            }
        }

        result.push_str("int main () {\n");
        result.push_str("  printf(\"%s\", \"Hello world\\n\");\n");
        result.push_str("  return 0;\n");
        result.push_str("}\n");
        result
    }
}
