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
use crate::codegen::get_reference_type_name;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{MacroParam, RefType, TextMacro, TextMacroEval};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::parser::ast::ASTType;
use crate::type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedType, CustomTypedTypeDef, DefaultFunctionCall,
};
use crate::utils::OptionDisplay;

use super::any::CLambdas;
use super::code_gen_c::CodeGenC;

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
                    format!(
                        "struct RasmPointer_* {var_name} = rasmMalloc(sizeof(struct {safe_name}));struct {safe_name} *{var_name}_ = (struct {safe_name} *){var_name}->address;"
                    )
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
            } else if let ASTTypedType::Struct { namespace, name } =
                &def.parameters.first().unwrap().ast_type
            {
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
                            "struct RasmPointer_* {var_name}_ = rasmMalloc(sizeof(struct {safe_name}));\n") + 
                            &format!("struct {safe_name}* {var_name} = (struct {safe_name}*){var_name}_->address;")
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
                        let value_address_as_enum = format!("((struct Enum*)value->address)");
                        format!("struct {safe_name}* {var_name} = (struct {safe_name}*)((struct RasmPointer_*){value_address_as_enum}->variant)->address;")
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
                    format!("struct RasmPointer_* {var_name} = rasmMalloc(sizeof(struct Enum));")
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

pub struct CAddRefMacro {
    code_gen: CodeGenC,
    ref_type: RefType,
    dereference_enabled: bool,
}

impl CAddRefMacro {
    pub fn new(code_gen: CodeGenC, ref_type: RefType, dereference_enabled: bool) -> Self {
        Self {
            code_gen,
            ref_type,
            dereference_enabled,
        }
    }
}

impl TextMacroEval for CAddRefMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        let mut result = String::new();

        if !self.dereference_enabled {
            return result;
        }
        if let Some(fd) = function_def {
            if fd.name == "addRef" {
                return result;
            }

            let (address, ast_typed_type) = match text_macro.parameters.get(0) {
                Some(MacroParam::Plain(address, _ast_type, Some(ast_typed_type))) => {
                    (address, ast_typed_type)
                }
                Some(MacroParam::Ref(address, _ast_type, Some(ast_typed_type))) => {
                    (address, ast_typed_type)
                }
                _ => panic!(
                    "Error: addRef/deref macro, a typed type must be specified in function {}:{} but got {}: {}",
                    OptionDisplay(&function_def),
                    OptionDisplay(&function_def.map(|it|&it.index)),
                    text_macro.parameters.get(0).unwrap(),
                    text_macro.index
                ),
            };

            if let Some(type_name) = get_reference_type_name(ast_typed_type, type_def_provider) {
                let descr = &format!("addref macro type {type_name}");

                match self.ref_type {
                    RefType::Deref => {
                        self.code_gen.call_deref(
                            &mut result,
                            address,
                            &type_name,
                            descr,
                            type_def_provider,
                            statics,
                        );
                    }
                    RefType::AddRef => {
                        self.code_gen.call_add_ref(
                            &mut result,
                            address,
                            &type_name,
                            descr,
                            type_def_provider,
                            statics,
                        );
                    }
                }
            }
        }
        result
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CTypeNameMacro;

impl CTypeNameMacro {
    pub fn new() -> Self {
        Self
    }
}

impl TextMacroEval for CTypeNameMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        let value = text_macro.parameters.get(0).unwrap();
        if let MacroParam::Ref(name, ast_type, ast_type_type) = value {
            let t = ast_type_type.clone().unwrap();

            CLambdas::add_to_statics_if_lambda(&t, statics);

            CodeGenC::type_to_string(&t, statics)
        } else {
            panic!("First argument should be a reference to a value.")
        }
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CCastAddress;

impl CCastAddress {
    pub fn new() -> Self {
        Self
    }
}

impl TextMacroEval for CCastAddress {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        let value = text_macro.parameters.get(0).unwrap();
        if let MacroParam::Ref(name, ast_type, ast_type_type) = value {
            let t = ast_type_type.clone().unwrap();

            CLambdas::add_to_statics_if_lambda(&t, statics);

            format!(
                "(({}){name}->address)",
                CodeGenC::type_to_string(&t, statics)
            )
        } else {
            panic!("First argument should be a reference to a value.")
        }
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CEnumSimpleMacro;

impl CEnumSimpleMacro {
    pub fn new() -> Self {
        Self
    }
}

impl TextMacroEval for CEnumSimpleMacro {
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
                    let mut result = String::new();
                    let (namespace, name) = if let Some(ASTTypedType::Enum { namespace, name }) =
                        &def.parameters.get(0).map(|it| &it.ast_type)
                    {
                        (namespace, name)
                    } else if let ASTTypedType::Enum { namespace, name } = &def.return_type {
                        (namespace, name)
                    } else {
                        panic!(
                            "Function does not return an enum or first parameter is not an enum {}",
                            text_macro.index
                        )
                    };

                    if let Some(enum_def) = type_def_provider.get_enum_def_by_name(name) {
                        if let Some((i, variant)) = enum_def
                            .variants
                            .iter()
                            .enumerate()
                            .find(|(i, v)| &v.name == variant_name)
                        {
                            if let ASTType::Custom {
                                namespace,
                                name,
                                param_types,
                                index,
                            } = &enum_def.ast_type
                            {
                                result.push_str(&format!(
                                    "struct RasmPointer_ *{var_name} = {};",
                                    CodeGenC::variant_const_name(
                                        enum_def.namespace(),
                                        name,
                                        &variant.name
                                    )
                                ));
                            } else {
                                panic!();
                            }
                        } else {
                            panic!(
                                "Cannot find variant {variant_name} for enum {name} : {}",
                                text_macro.index
                            )
                        }
                        result
                    } else {
                        panic!("Cannot find enum {name} : {}", text_macro.index)
                    }
                } else {
                    panic!("Function not present {}", text_macro.index)
                }
            } else {
                panic!("Error in enumVariantDeclaration macro. Expected the thirs plain parameter as the name of the variant {}", text_macro.index)
            }
        } else {
            panic!("Expected a parameter with the name of the variable to be created.")
        }
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}
