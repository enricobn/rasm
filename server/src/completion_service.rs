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
use std::io;

use rasm_core::codegen::enh_ast::{EnhASTFunctionDef, EnhASTIndex, EnhASTType, EnhBuiltinTypeKind};
use rasm_core::new_type_check2;
use rasm_core::type_check::typed_ast::ASTTypedType;
use rasm_parser::parser::ast::{ASTFunctionSignature, ASTType, BuiltinTypeKind};
use rasm_utils::OptionDisplay;

use crate::file_token::FileToken;

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
    pub fn for_function(function: &EnhASTFunctionDef) -> Option<Self> {
        if function.parameters.is_empty() {
            return None;
        }
        let parameter_type = &function.parameters.get(0).unwrap().ast_type;
        let coeff = new_type_check2::TypeCheck::generic_type_coeff(parameter_type);
        let sort_value = format!("{:0>20}{}", coeff, function.original_name);

        Some(CompletionItem {
            value: function.original_name.clone(),
            descr: Self::function_descr(function),
            sort: Some(sort_value),
            insert: Some(Self::function_insert(function)),
        })
    }

    pub fn for_function_signature(function: &ASTFunctionSignature) -> Option<Self> {
        if function.parameters_types.is_empty() {
            return None;
        }
        let parameter_type = &function.parameters_types.get(0).unwrap();
        let coeff = Self::generic_type_coeff(parameter_type);
        let sort_value = format!("{:0>20}{}", coeff, function.name);

        Some(CompletionItem {
            value: function.name.clone(),
            descr: Self::signature_descr(function),
            sort: Some(sort_value),
            insert: Some(Self::signature_insert(function)),
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
                ASTType::Builtin(_) => 0,
                ASTType::Generic(_, _) => coeff,
                ASTType::Custom {
                    name: _,
                    param_types,
                    position: _,
                } => param_types
                    .iter()
                    .map(|it| Self::generic_type_coeff_internal(it, coeff / 100))
                    .sum(),
                ASTType::Unit => 0,
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

        let rt = if function.return_type != ASTType::Unit {
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

    fn function_descr(function: &EnhASTFunctionDef) -> String {
        let generic_types = if function.generic_types.is_empty() {
            "".into()
        } else {
            format!("<{}>", function.generic_types.join(","))
        };

        let rt = if function.return_type != EnhASTType::Unit {
            format!("{}", function.return_type)
        } else {
            "()".into()
        };

        let args = function
            .parameters
            .iter()
            .map(|it| format!("{}", it))
            .collect::<Vec<String>>()
            .join(",");
        format!("{}{generic_types}({args}) -> {rt}", function.original_name)
    }

    fn function_insert(function: &EnhASTFunctionDef) -> String {
        let args = function
            .parameters
            .iter()
            .skip(1)
            .map(|it| match &it.ast_type {
                EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                    parameters,
                    return_type: _,
                }) => {
                    let par_names = parameters
                        .iter()
                        .enumerate()
                        .map(|(pos, _ast_type)| format!("par{pos}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    if par_names.is_empty() {
                        "\n    {  }".to_string()
                    } else {
                        format!("\n    fn({par_names}) {{  }}")
                    }
                }
                _ => it.name.clone(),
            })
            .collect::<Vec<String>>()
            .join(", ");
        format!("{}({args});", function.original_name)
    }

    fn signature_insert(function: &ASTFunctionSignature) -> String {
        let args = function
            .parameters_types
            .iter()
            .skip(1)
            .map(|it| match &it {
                ASTType::Builtin(BuiltinTypeKind::Lambda {
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
        format!("{}({args});", function.name)
    }

    fn type_base_name(ast_type: &ASTType) -> String {
        match ast_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::Bool => "b".to_owned(),
                BuiltinTypeKind::Char => "c".to_owned(),
                BuiltinTypeKind::I32 => "i".to_owned(),
                BuiltinTypeKind::F32 => "f".to_owned(),
                BuiltinTypeKind::String => "s".to_owned(),
                BuiltinTypeKind::Lambda {
                    parameters: _,
                    return_type: _,
                } => "fun".to_owned(),
            },
            ASTType::Generic(_, _) => "gen".to_owned(),
            ASTType::Custom {
                name,
                param_types: _,
                position: _,
            } => {
                // from https://stackoverflow.com/questions/38406793/why-is-capitalizing-the-first-letter-of-a-string-so-convoluted-in-rust
                let mut n = name.clone();
                n[0..1].make_ascii_lowercase();
                n
            }
            ASTType::Unit => "unit".to_owned(),
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

pub enum CompletableItemResult {
    Found(CompletableItem),
    NotFound(String),
}

#[derive(Clone)]
pub struct CompletableItem {
    file_token: FileToken,
    ast_typed_type: ASTTypedType,
}

impl CompletableItem {
    fn new(start: EnhASTIndex, len: usize, ast_typed_type: ASTTypedType) -> Self {
        Self {
            file_token: FileToken::new(start, len),
            ast_typed_type,
        }
    }

    fn contains(&self, index: &EnhASTIndex) -> io::Result<bool> {
        self.file_token.contains(index)
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use env_logger::Builder;

    use rasm_core::codegen::enh_ast::{
        EnhASTFunctionBody, EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTParameterDef,
        EnhASTType, EnhBuiltinTypeKind,
    };
    use rasm_core::type_check::resolved_generic_types::ResolvedGenericTypes;
    use rasm_parser::parser::ast::ASTModifiers;

    use crate::completion_service::CompletionItem;

    #[test]
    fn test_completion_item_for_function() {
        let function1 = EnhASTFunctionDef {
            original_name: "add".to_string(),
            name: "add_0".to_string(),
            parameters: vec![EnhASTParameterDef {
                name: "par".to_string(),
                ast_type: EnhASTType::Builtin(EnhBuiltinTypeKind::String),
                ast_index: EnhASTIndex::none(),
            }],
            return_type: EnhASTType::Unit,
            body: EnhASTFunctionBody::RASMBody(vec![]),
            inline: false,
            generic_types: vec![],
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: EnhASTIndex::none(),
            modifiers: ASTModifiers::private(),
            namespace: test_namespace(), // HENRY
            rank: 0,
            target: None,
        };

        let mut function2 = function1.clone();
        function2.parameters = vec![EnhASTParameterDef {
            name: "par".to_string(),
            ast_type: EnhASTType::Generic(EnhASTIndex::none(), "T".to_string()),
            ast_index: EnhASTIndex::none(),
        }];

        CompletionItem::for_function(&function1).unwrap();
        CompletionItem::for_function(&function2).unwrap();
    }

    fn init() {
        Builder::from_default_env()
            .format(|buf, record| {
                writeln!(
                    buf,
                    "{} [{}] - {}",
                    chrono::Local::now().format("%Y-%m-%d %H:%M:%S.%3f"),
                    record.level(),
                    record.args()
                )
            })
            .try_init()
            .unwrap_or(());
    }

    pub fn test_namespace() -> EnhASTNameSpace {
        EnhASTNameSpace::new("test".to_string(), "test".to_string())
    }
}
