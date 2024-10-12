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

use crate::codegen::eh_ast::ASTIndex;
use crate::type_check::traverse_typed_ast::TraverseTypedAST;
use crate::type_check::typed_ast::{ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedModule};
use lazy_static::lazy_static;
use linked_hash_set::LinkedHashSet;
use regex::Regex;

lazy_static! {
    static ref RE: Regex = Regex::new(r"call (.*)").unwrap();
}

pub struct UsedFunctions {
    functions: LinkedHashSet<String>,
}

impl UsedFunctions {
    fn new() -> Self {
        Self {
            functions: LinkedHashSet::new(),
        }
    }

    pub fn find(module: &ASTTypedModule) -> LinkedHashSet<String> {
        let mut used_functions = Self::new();

        used_functions.traverse(module);

        used_functions.functions
    }
}

impl TraverseTypedAST for UsedFunctions {
    fn found_call(&mut self, call: &ASTTypedFunctionCall) {
        self.functions.insert(call.function_name.clone());
    }

    fn found_let(&mut self, _name: &str, _is_const: bool, _index: &ASTIndex) {}

    fn found_function_def(&mut self, _function: &ASTTypedFunctionDef) {}

    fn found_asm(
        &mut self,
        _module: &ASTTypedModule,
        _function: &ASTTypedFunctionDef,
        native_code: &str,
    ) {
        let used_functions = Self::get_used_functions(native_code);

        self.functions.extend(used_functions);
        /*
        let evaluator = TextMacroEvaluator::new();
        match evaluator.get_macros(self.backend, Some(function), None, asm, module) {
            Ok(macros) => {
                macros.iter().for_each(|(m, _)| {
                    println!("found macro {m}");
                    if m.name == "call" {
                        if let Some(MacroParam::Plain(name, _, _)) = m.parameters.get(0) {
                            self.functions.push(name.clone());
                        }
                    }
                });
            }
            Err(error) => {
                println!("error {error}");
            }
        }

         */
    }
}

impl UsedFunctions {
    pub fn get_used_functions(native_code: &str) -> LinkedHashSet<String> {
        let mut used_functions: LinkedHashSet<String> = LinkedHashSet::new();

        for line in native_code.lines() {
            let trimmed_line = if let Some(found) = line.find(';') {
                &line[..found]
            } else {
                line
            };
            if !trimmed_line.contains('[') {
                for (_, [name]) in RE.captures_iter(trimmed_line).map(|c| c.extract()) {
                    used_functions.insert(name.trim().to_string());
                }
            }
        }
        used_functions
    }
}

#[cfg(test)]
mod tests {
    use regex::Regex;

    #[test]
    #[ignore]
    fn test() {
        let re = Regex::new(r"[^;]+.*call (.*)").unwrap();

        let mut functions = Vec::new();

        for (_, [name]) in re
            .captures_iter("call pippo\ncall pluto\n")
            .map(|c| c.extract())
        {
            functions.push(name);
        }

        assert_eq!(vec!["pippo", "pluto"], functions);
    }
}
