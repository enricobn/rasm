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
use std::fmt::Display;

use rasm_parser::parser::ast::{ASTBuiltinTypeKind, ASTFunctionSignature, ASTType};
use rasm_utils::OptionDisplay;

#[derive(PartialEq, Debug, Clone)]
pub struct CompletionItem {
    pub value: String,
    pub descr: String,
    pub sort: Option<String>,
    pub insert: Option<String>,
}

impl Display for CompletionItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "CompletionItem({}, {}, {}, ...)",
            self.value,
            self.descr,
            OptionDisplay(&self.sort),
        ))
    }
}

impl CompletionItem {
    pub fn for_function_signature(
        function: &ASTFunctionSignature,
        remove_first_arg: bool,
    ) -> Option<Self> {
        if function.parameters_types.is_empty() {
            return None;
        }
        let parameter_type = &function.parameters_types.get(0).unwrap();
        let coeff = Self::generic_type_coeff(parameter_type);
        let sort_value = format!("{:0>20}{}", coeff, function.name);

        let function = function.clone().remove_generic_prefix();

        Some(CompletionItem {
            value: function.name.clone(),
            descr: Self::signature_descr(&function),
            sort: Some(sort_value),
            insert: Some(Self::signature_insert(&function, remove_first_arg)),
        })
    }

    ///
    /// return a coefficient that is higher for how the type is generic
    ///
    fn generic_type_coeff(ast_type: &ASTType) -> usize {
        Self::generic_type_coeff_internal(ast_type, usize::MAX / 100)
    }

    fn generic_type_coeff_internal(ast_type: &ASTType, coeff: usize) -> usize {
        if ast_type.is_generic() {
            match ast_type {
                ASTType::ASTBuiltinType(_) => 0,
                ASTType::ASTGenericType(_, _, _) => coeff,
                ASTType::ASTCustomType {
                    name: _,
                    param_types,
                    position: _,
                } => param_types
                    .iter()
                    .map(|it| Self::generic_type_coeff_internal(it, coeff / 100))
                    .sum(),
                ASTType::ASTUnitType => 0,
            }
        } else {
            0
        }
    }

    fn signature_descr(function: &ASTFunctionSignature) -> String {
        let generic_types = if function.generics.is_empty() {
            "".into()
        } else {
            format!("<{}>", function.generics.join(","))
        };

        let rt = if function.return_type != ASTType::ASTUnitType {
            format!("{}", function.return_type)
        } else {
            "()".into()
        };

        let args = function
            .parameters_types
            .iter()
            .map(|it| format!("{}", it))
            .collect::<Vec<String>>()
            .join(",");
        format!("{}{generic_types}({args}) -> {rt}", function.name)
    }

    fn signature_insert(function: &ASTFunctionSignature, remove_first_arg: bool) -> String {
        let args = function
            .parameters_types
            .iter()
            .skip(if remove_first_arg { 1 } else { 0 }) // skip self (1)
            .map(|it| match &it {
                ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                    parameters,
                    return_type: _,
                }) => {
                    let mut all_names = HashSet::new();

                    let par_names = parameters
                        .iter()
                        .enumerate()
                        .map(|(_pos, ast_type)| {
                            let base_name = Self::type_base_name(ast_type);
                            let mut name = base_name.clone();
                            let mut i = 1;
                            while all_names.contains(&name) {
                                name = format!("{base_name}{i}");
                                i += 1;
                            }
                            all_names.insert(name.clone());
                            name
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    if par_names.is_empty() {
                        "\n    {  }".to_string()
                    } else {
                        format!("\n    fn({par_names}) {{  }}")
                    }
                }
                _ => format!("{it}"),
            })
            .collect::<Vec<String>>()
            .join(", ");
        if args.is_empty() {
            format!("{}", function.name)
        } else {
            format!("{}({args})", function.name)
        }
    }

    fn type_base_name(ast_type: &ASTType) -> String {
        match ast_type {
            ASTType::ASTBuiltinType(kind) => match kind {
                ASTBuiltinTypeKind::ASTBooleanType => "b".to_owned(),
                ASTBuiltinTypeKind::ASTCharType => "c".to_owned(),
                ASTBuiltinTypeKind::ASTIntegerType => "i".to_owned(),
                ASTBuiltinTypeKind::ASTFloatType => "f".to_owned(),
                ASTBuiltinTypeKind::ASTStringType => "s".to_owned(),
                ASTBuiltinTypeKind::ASTLambdaType {
                    parameters: _,
                    return_type: _,
                } => "fun".to_owned(),
            },
            ASTType::ASTGenericType(_, _, _) => "gen".to_owned(),
            ASTType::ASTCustomType {
                name,
                param_types: _,
                position: _,
            } => {
                // from https://stackoverflow.com/questions/38406793/why-is-capitalizing-the-first-letter-of-a-string-so-convoluted-in-rust
                let mut n = name.clone();
                n[0..1].make_ascii_lowercase();
                n
            }
            ASTType::ASTUnitType => "unit".to_owned(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum CompletionResult {
    Found(Vec<CompletionItem>),
    NotFound(String),
}

#[derive(Debug)]
pub enum CompletionTrigger {
    Invoked,
    Character(char),
    IncompleteCompletion,
}

#[cfg(test)]
mod tests {
    use rasm_parser::parser::ast::{
        ASTBuiltinTypeKind, ASTFunctionSignature, ASTModifiers, ASTPosition, ASTType,
    };

    use crate::completion_service::CompletionItem;

    #[test]
    fn test_completion_item_for_function() {
        let function1 = ASTFunctionSignature {
            name: "add".to_string(),
            generics: vec![],
            parameters_types: vec![ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTStringType)],
            return_type: ASTType::ASTUnitType,
            modifiers: ASTModifiers::private(),
        };

        let mut function2 = function1.clone();
        function2.parameters_types = vec![ASTType::ASTGenericType(
            ASTPosition::none(),
            "T".to_string(),
            Vec::new(),
        )];

        let c = CompletionItem::for_function_signature(&function1, false).unwrap();
        assert_eq!(c.value, "add");
        assert_eq!(c.insert, Some("add(str)".to_owned()));

        let c = CompletionItem::for_function_signature(&function1, true).unwrap();
        assert_eq!(c.insert, Some("add".to_owned()));

        let c = CompletionItem::for_function_signature(&function2, false).unwrap();
        assert_eq!(c.value, "add");
        assert_eq!(c.insert, Some("add(T)".to_owned()));

        let c = CompletionItem::for_function_signature(&function2, true).unwrap();
        assert_eq!(c.insert, Some("add".to_owned()));
    }
}
