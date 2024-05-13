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

use crate::codegen::c::any::CInclude;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{MacroParam, TextMacro, TextMacroEval};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::type_check::typed_ast::{ASTTypedFunctionDef, ASTTypedType, DefaultFunctionCall};

pub struct CIncludeMacro;

impl TextMacroEval for CIncludeMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        match text_macro.parameters.get(0).unwrap() {
            MacroParam::Plain(s, _, _) => {
                CInclude::add_to_statics(statics, s.clone());
            }
            MacroParam::StringLiteral(s) => {
                CInclude::add_to_statics(statics, format!("\"{s}\""));
            }
            MacroParam::Ref(_, _, _) => {}
        }
        String::new()
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CStructDeclarationMacro;

impl TextMacroEval for CStructDeclarationMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        if let Some(MacroParam::Plain(var_name, _, _)) = text_macro.parameters.get(0) {
            if let Some(def) = function_def {
                if let ASTTypedType::Struct { namespace, name } = &def.return_type {
                    CInclude::add_to_statics(statics, "<stdlib.h>".to_string()); // for malloc

                    let safe_name = format!("{}_{}", namespace.safe_name(), name);
                    format!("struct {safe_name}* {var_name} = malloc(sizeof(struct {safe_name}));")
                } else {
                    panic!(
                        "Error in structDeclaration macro. Function does not return a struct {}",
                        text_macro.index
                    )
                }
            } else {
                panic!(
                    "Error in structDeclaration macro. Function not present {}",
                    text_macro.index
                )
            }
        } else {
            panic!("Error in structDeclaration macro. Expected one plain parameter as the name of the var to declare {}", text_macro.index)
        }
    }

    fn is_pre_macro(&self) -> bool {
        true
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CStructTypeMacro;

impl TextMacroEval for CStructTypeMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        if let Some(def) = function_def {
            if let ASTTypedType::Struct { namespace, name } = &def.return_type {
                CInclude::add_to_statics(statics, "<stdlib.h>".to_string()); // for malloc

                let safe_name = format!("{}_{}", namespace.safe_name(), name);
                format!("struct {safe_name}")
            } else {
                panic!(
                    "Error in structDeclaration macro. Function does not return a struct {}",
                    text_macro.index
                )
            }
        } else {
            panic!(
                "Error in structDeclaration macro. Function not present {}",
                text_macro.index
            )
        }
    }

    fn is_pre_macro(&self) -> bool {
        true
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CEnumVariantDeclarationMacro;

impl TextMacroEval for CEnumVariantDeclarationMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        if let Some(MacroParam::Plain(var_name, _, _)) = text_macro.parameters.get(0) {
            if let Some(MacroParam::Plain(variant_name, _, _)) = text_macro.parameters.get(1) {
                if let Some(def) = function_def {
                    if let ASTTypedType::Enum { namespace, name } = &def.return_type {
                        CInclude::add_to_statics(statics, "<stdlib.h>".to_string()); // for malloc

                        let safe_name =
                            format!("{}_{}_{}", namespace.safe_name(), name, variant_name);
                        format!(
                            "struct {safe_name}* {var_name} = malloc(sizeof(struct {safe_name}));"
                        )
                    } else {
                        panic!(
                            "Error in enumVariantDeclaration macro. Function does not return an enum {}",
                            text_macro.index
                        )
                    }
                } else {
                    panic!(
                        "Error in enumVariantDeclaration macro. Function not present {}",
                        text_macro.index
                    )
                }
            } else {
                panic!("Error in enumVariantDeclaration macro. Expected the second plain parameter as the name of the variant {}", text_macro.index)
            }
        } else {
            panic!("Error in enumVariantDeclaration macro. Expected the first plain parameter as the name of the var to declare {}", text_macro.index)
        }
    }

    fn is_pre_macro(&self) -> bool {
        true
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CEnumVariantAssignmentMacro;

impl TextMacroEval for CEnumVariantAssignmentMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        if let Some(MacroParam::Plain(var_name, _, _)) = text_macro.parameters.get(0) {
            if let Some(MacroParam::Plain(variant_name, _, _)) = text_macro.parameters.get(1) {
                if let Some(def) = function_def {
                    if let Some(ASTTypedType::Enum { namespace, name }) =
                        &def.parameters.get(0).map(|it| &it.ast_type)
                    {
                        let safe_name =
                            format!("{}_{}_{}", namespace.safe_name(), name, variant_name);
                        format!("struct {safe_name}* {var_name} = value->variant;")
                    } else {
                        panic!(
                                "Error in enumVariantDeclaration macro. Function does not return an enum {}",
                                text_macro.index
                            )
                    }
                } else {
                    panic!(
                        "Error in enumVariantDeclaration macro. Function not present {}",
                        text_macro.index
                    )
                }
            } else {
                panic!("Error in enumVariantDeclaration macro. Expected the thirs plain parameter as the name of the variant {}", text_macro.index)
            }
        } else {
            panic!("Error in enumVariantDeclaration macro. Expected the secont plain parameter as the name of the var to declare {}", text_macro.index)
        }
    }

    fn is_pre_macro(&self) -> bool {
        true
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CEnumDeclarationMacro;

impl TextMacroEval for CEnumDeclarationMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        if let Some(MacroParam::Plain(var_name, _, _)) = text_macro.parameters.get(0) {
            if let Some(def) = function_def {
                if let ASTTypedType::Enum { namespace, name } = &def.return_type {
                    CInclude::add_to_statics(statics, "<stdlib.h>".to_string()); // for malloc

                    let safe_name = format!("{}_{}", namespace.safe_name(), name);
                    format!("struct {safe_name}* {var_name} = malloc(sizeof(struct {safe_name}));")
                } else {
                    panic!(
                        "Error in enumDeclaration macro. Function does not return an enum {}",
                        text_macro.index
                    )
                }
            } else {
                panic!(
                    "Error in enumDeclaration macro. Function not present {}",
                    text_macro.index
                )
            }
        } else {
            panic!("Error in enumDeclaration macro. Expected a plain parameter as the name of the var to declare {}", text_macro.index)
        }
    }

    fn is_pre_macro(&self) -> bool {
        true
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CCallMacro;

impl TextMacroEval for CCallMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        let function_name =
            if let Some(MacroParam::Plain(function_name, _, _)) = text_macro.parameters.get(0) {
                function_name
            } else {
                panic!("Error getting the function name");
            };

        let parameters = text_macro
            .parameters
            .iter()
            .skip(1)
            .map(|it| match it {
                MacroParam::Plain(value, _, _) => value.to_string(),
                MacroParam::StringLiteral(s) => format!("\"{s}\""),
                MacroParam::Ref(value, _, _) => value.to_string(),
            })
            .collect::<Vec<_>>();

        format!("{function_name}({});", parameters.join(", "))
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}
