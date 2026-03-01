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

use itertools::Itertools;
use rasm_utils::OptionDisplay;

use crate::codegen::c::any::{CInclude, CIncludeType};
use crate::codegen::c::typed_function_creator_c::TypedFunctionsCreatorC;
use crate::codegen::enh_ast::{EnhASTFunctionDef, EnhASTType};
use crate::codegen::get_reference_type_name;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{MacroParam, RefType, TextMacro, TextMacroEval, parse_type};
use crate::codegen::type_def_body::{TypeDefBodyCache, TypeDefBodyTarget};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::enh_type_check::enh_resolved_generic_types::EnhResolvedGenericTypes;
use crate::enh_type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedType, BuiltinTypedTypeKind, CustomTypedTypeDef,
    DefaultFunctionCall,
};

use super::any::CLambdas;
use super::code_gen_c::{CCodeManipulator, CodeGenC};

pub struct CIncludeMacro;

impl TextMacroEval for CIncludeMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        _typed_function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        match text_macro.parameters.get(0).unwrap() {
            MacroParam::Plain(s, _, _) => {
                CInclude::add_to_statics(statics, CIncludeType::Header(s.clone()));
            }
            MacroParam::StringLiteral(s) => {
                CInclude::add_to_statics(statics, CIncludeType::Header(format!("\"{s}\"")));
            }
            MacroParam::Ref(_, _, _) => {}
        }
        Ok(String::new())
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
        typed_function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        if let Some(MacroParam::Plain(var_name, _, _)) = text_macro.parameters.get(0) {
            if let Some(def) = typed_function_def {
                if let ASTTypedType::Struct { namespace, name } = &def.return_type {
                    CInclude::add_to_statics(
                        statics,
                        CIncludeType::Header("<stdlib.h>".to_string()),
                    ); // for malloc

                    let safe_name = format!("{}_{}", namespace.safe_name(), name);
                    Ok(format!(
                        "struct RasmPointer_* {var_name} = rasmMalloc(sizeof(struct {safe_name}));struct {safe_name} *{var_name}_ = (struct {safe_name} *){var_name}->address;"
                    ))
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
            panic!(
                "Error in structDeclaration macro. Expected one plain parameter as the name of the var to declare {}",
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

pub struct CStructTypeMacro;

impl TextMacroEval for CStructTypeMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        if let Some(def) = typed_function_def {
            if let ASTTypedType::Struct { namespace, name } = &def.return_type {
                CInclude::add_to_statics(statics, CIncludeType::Header("<stdlib.h>".to_string())); // for malloc

                let safe_name = format!("{}_{}", namespace.safe_name(), name);
                Ok(format!("struct {safe_name}"))
            } else if let ASTTypedType::Struct { namespace, name } =
                &def.parameters.first().unwrap().ast_type
            {
                CInclude::add_to_statics(statics, CIncludeType::Header("<stdlib.h>".to_string())); // for malloc

                let safe_name = format!("{}_{}", namespace.safe_name(), name);
                Ok(format!("struct {safe_name}"))
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
        typed_function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        if let Some(MacroParam::Plain(var_name, _, _)) = text_macro.parameters.get(0) {
            if let Some(MacroParam::Plain(variant_name, _, _)) = text_macro.parameters.get(1) {
                if let Some(def) = typed_function_def {
                    if let ASTTypedType::Enum { namespace, name } = &def.return_type {
                        CInclude::add_to_statics(
                            statics,
                            CIncludeType::Header("<stdlib.h>".to_string()),
                        ); // for malloc

                        let safe_name =
                            format!("{}_{}_{}", namespace.safe_name(), name, variant_name);
                        Ok(format!(
                            "struct RasmPointer_* {var_name}_ = rasmMalloc(sizeof(struct {safe_name}));\n"
                        ) + &format!(
                            "struct {safe_name}* {var_name} = (struct {safe_name}*){var_name}_->address;"
                        ))
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
                panic!(
                    "Error in enumVariantDeclaration macro. Expected the second plain parameter as the name of the variant {}",
                    text_macro.index
                )
            }
        } else {
            panic!(
                "Error in enumVariantDeclaration macro. Expected the first plain parameter as the name of the var to declare {}",
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

pub struct CEnumVariantAssignmentMacro;

impl TextMacroEval for CEnumVariantAssignmentMacro {
    fn eval_macro(
        &self,
        _statics: &mut Statics,
        text_macro: &TextMacro,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        if let Some(MacroParam::Plain(var_name, _, _)) = text_macro.parameters.get(0) {
            if let Some(MacroParam::Plain(variant_name, _, _)) = text_macro.parameters.get(1) {
                if let Some(def) = typed_function_def {
                    if let Some(ASTTypedType::Enum { namespace, name }) =
                        &def.parameters.get(0).map(|it| &it.ast_type)
                    {
                        let safe_name =
                            format!("{}_{}_{}", namespace.safe_name(), name, variant_name);
                        let value_address_as_enum = format!("((struct Enum*)value->address)");
                        Ok(format!(
                            "struct {safe_name}* {var_name} = (struct {safe_name}*)((struct RasmPointer_*){value_address_as_enum}->variant)->address;"
                        ))
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
                panic!(
                    "Error in enumVariantDeclaration macro. Expected the thirs plain parameter as the name of the variant {}",
                    text_macro.index
                )
            }
        } else {
            panic!(
                "Error in enumVariantDeclaration macro. Expected the secont plain parameter as the name of the var to declare {}",
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

pub struct CEnumVariantMacro;

impl TextMacroEval for CEnumVariantMacro {
    fn eval_macro(
        &self,
        _statics: &mut Statics,
        text_macro: &TextMacro,
        _typed_function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        if let Some(MacroParam::Plain(var_name, _, _)) = text_macro.parameters.get(0) {
            if let Some(MacroParam::Plain(variant_name, _, _)) = text_macro.parameters.get(1) {
                if let Some(MacroParam::Plain(value, _, Some(t))) = text_macro.parameters.get(2) {
                    if let ASTTypedType::Enum { namespace, name } = t {
                        let safe_name =
                            format!("{}_{}_{}", namespace.safe_name(), name, variant_name);
                        let value_address_as_enum = format!("((struct Enum*){value}->address)");
                        Ok(format!(
                            "struct {safe_name}* {var_name} = (struct {safe_name}*)((struct RasmPointer_*){value_address_as_enum}->variant)->address;"
                        ))
                    } else {
                        panic!(
                            "Error in enumVariant macro. Function does not return an enum {}",
                            text_macro.index
                        )
                    }
                } else {
                    panic!(
                        "Error in enumVariant macro. Function not present {}",
                        text_macro.index
                    )
                }
            } else {
                panic!(
                    "Error in enumVariantDeclaration macro. Expected the thirs plain parameter as the name of the variant {}",
                    text_macro.index
                )
            }
        } else {
            panic!(
                "Error in enumVariantDeclaration macro. Expected the secont plain parameter as the name of the var to declare {}",
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

pub struct CEnumDeclarationMacro;

impl TextMacroEval for CEnumDeclarationMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        if let Some(MacroParam::Plain(var_name, _, _)) = text_macro.parameters.get(0) {
            if let Some(def) = typed_function_def {
                if let ASTTypedType::Enum {
                    namespace: _,
                    name: _,
                } = &def.return_type
                {
                    CInclude::add_to_statics(
                        statics,
                        CIncludeType::Header("<stdlib.h>".to_string()),
                    ); // for malloc
                    Ok(format!(
                        "struct RasmPointer_* {var_name} = rasmMalloc(sizeof(struct Enum));"
                    ))
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
            panic!(
                "Error in enumDeclaration macro. Expected a plain parameter as the name of the var to declare {}",
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

pub struct CCallMacro;

impl TextMacroEval for CCallMacro {
    fn eval_macro(
        &self,
        _statics: &mut Statics,
        text_macro: &TextMacro,
        _typed_function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        let function_name =
            if let Some(MacroParam::Plain(function_name, _, _)) = text_macro.parameters.get(0) {
                function_name
            } else {
                return Err("Error getting the function name".to_owned());
            };

        let parameters = text_macro
            .parameters
            .iter()
            .skip(1)
            .map(|it| match it {
                MacroParam::Plain(value, _, _) => value.to_string(),
                MacroParam::StringLiteral(s) => {
                    format!("addStaticStringToHeap(\"{}\")", CodeGenC::escape_string(s))
                }
                MacroParam::Ref(value, _, _) => value.to_string(),
            })
            .collect::<Vec<_>>();

        Ok(format!("{function_name}({})", parameters.join(", ")))
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CAddRefMacro {
    code_manipulator: CCodeManipulator,
    ref_type: RefType,
    dereference_enabled: bool,
}

impl CAddRefMacro {
    pub fn new(
        code_manipulator: CCodeManipulator,
        ref_type: RefType,
        dereference_enabled: bool,
    ) -> Self {
        Self {
            code_manipulator,
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
        _typed_function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        let mut result = String::new();

        if !self.dereference_enabled {
            return Ok(result);
        }

        // we take the expression and the type from the first parameter
        let (expression, ast_typed_type) = if text_macro.parameters.len() == 1 {
            get_par_text_and_type(text_macro, 0)?
        // we take the type from the first parameter, and the expression from the second
        } else if text_macro.parameters.len() == 2 {
            let (_, ast_typed_type) = get_par_text_and_type(text_macro, 0)?;
            (get_par_text(text_macro, 1)?, ast_typed_type)
        // we take the lambda type from the first parameter, the expression from the second,
        // and the lambda parameter index from the third with which we determine the type of the expression
        } else if text_macro.parameters.len() == 3 {
            let (_address, lambda_ast_typed_type) = get_par_text_and_type(text_macro, 0)?;

            let par_index = get_par_number(text_macro, 2, "lambda parameter index")?;

            let expression = match text_macro.parameters.get(1) {
                Some(MacroParam::Plain(plain_value, _, _)) => plain_value,
                _ => {
                    return Err(
                        "second argument should be the value for which add the reference"
                            .to_owned(),
                    );
                }
            };

            let ast_typed_type =
                if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda { parameters, .. }) =
                    &lambda_ast_typed_type
                {
                    &parameters[par_index]
                } else {
                    return Err(
                        "Error in addRef macro, expected a lambda as first parameter".to_owned(),
                    );
                };

            (expression.clone(), ast_typed_type.clone())
        } else {
            return Err(format!(
                "Error in {} macro, expected one, two or three parameters",
                self.ref_type.function_name()
            ));
        };

        add_ref_deref_code(
            self.ref_type,
            statics,
            type_def_provider,
            &self.code_manipulator,
            &mut result,
            &expression,
            &ast_typed_type,
        );

        Ok(result)
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
        typed_function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
        function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        let t = get_type_from_parameter(
            text_macro,
            0,
            statics,
            typed_function_def,
            function_def,
            type_def_provider,
        )?;

        Ok(CodeGenC::type_to_string(&t, statics))
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CRealTypeNameMacro;

impl CRealTypeNameMacro {
    pub fn new() -> Self {
        Self
    }
}

impl TextMacroEval for CRealTypeNameMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
        function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        let t = if text_macro.parameters.len() == 1 {
            get_type_from_parameter(
                text_macro,
                0,
                statics,
                typed_function_def,
                function_def,
                type_def_provider,
            )?
        } else {
            let lambda_ast_typed_type = get_type_from_parameter(
                text_macro,
                0,
                statics,
                typed_function_def,
                function_def,
                type_def_provider,
            )?;
            let index = get_par_number(text_macro, 1, "lambda parameter index")?;
            let ast_typed_type =
                if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda { parameters, .. }) =
                    &lambda_ast_typed_type
                {
                    &parameters[index]
                } else {
                    return Err(
                        "Error in realTypeName macro, expected a lambda as first parameter"
                            .to_owned(),
                    );
                };

            ast_typed_type.clone()
        };

        Ok(CodeGenC::real_type_to_string(&t))
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
        _typed_function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        let value = text_macro.parameters.get(0).unwrap();
        if let MacroParam::Ref(name, _ast_type, Some(t)) = value {
            CLambdas::add_to_statics_if_lambda(t, statics);

            Ok(format!(
                "(({}){name}->address)",
                CodeGenC::type_to_string(t, statics)
            ))
        } else if let MacroParam::Plain(name, _, Some(t)) = value {
            CLambdas::add_to_statics_if_lambda(t, statics);

            Ok(format!(
                "(({}){name}->address)",
                CodeGenC::type_to_string(t, statics)
            ))
        } else {
            Err(format!(
                "First argument should be a reference to a value, or plain arg with type, but got {value}."
            ))
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
        _statics: &mut Statics,
        text_macro: &TextMacro,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        if let Some(MacroParam::Plain(variant_name, _, _)) = text_macro.parameters.get(0) {
            let enum_def =
                if let Some(MacroParam::Plain(enum_name, _, _)) = text_macro.parameters.get(1) {
                    if let Some(def) = typed_function_def {
                        if let Some(enum_def) =
                            type_def_provider.get_enum_def_like_name(&def.namespace, enum_name)
                        {
                            enum_def
                        } else {
                            panic!("Cannot find enum {enum_name} : {}", text_macro.index)
                        }
                    } else {
                        panic!("Function not present {}", text_macro.index);
                    }
                } else if let Some(def) = typed_function_def {
                    let (_namespace, name) = if let Some(ASTTypedType::Enum { namespace, name }) =
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
                    if let Some(enum_def) = type_def_provider.get_enum_def_by_name(&name) {
                        enum_def
                    } else {
                        panic!("Cannot find enum {name} : {}", text_macro.index)
                    }
                } else {
                    panic!("Function not present {}", text_macro.index);
                };

            if let Some((_i, variant)) = enum_def
                .variants
                .iter()
                .enumerate()
                .find(|(_i, v)| &v.name == variant_name)
            {
                let mut result = String::new();
                if let EnhASTType::Custom {
                    namespace: _,
                    name,
                    param_types: _,
                    index: _,
                } = &enum_def.ast_type
                {
                    result.push_str(&CodeGenC::variant_const_name(
                        enum_def.namespace(),
                        name,
                        &variant.name,
                    ));
                    Ok(result)
                } else {
                    panic!();
                }
            } else {
                panic!(
                    "Cannot find variant {variant_name} for enum {} : {}",
                    enum_def.name, text_macro.index
                )
            }
        } else {
            panic!(
                "Error in enumVariantDeclaration macro. Expected the thirs plain parameter as the name of the variant {}",
                text_macro.index
            )
        }
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct CIsRefMacro;

impl CIsRefMacro {
    pub fn new() -> Self {
        Self
    }
}

impl TextMacroEval for CIsRefMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
        _function_def: Option<&EnhASTFunctionDef>,
    ) -> Result<String, String> {
        let type_param = text_macro.parameters.get(0).unwrap();
        let true_value = if let MacroParam::StringLiteral(s) = text_macro.parameters.get(1).unwrap()
        {
            s
        } else {
            return Err("Second parameter should be a string literal".to_owned());
        };
        let false_value =
            if let MacroParam::StringLiteral(s) = text_macro.parameters.get(2).unwrap() {
                s
            } else {
                return Err("Third parameter should be a string literal".to_owned());
            };

        if let MacroParam::Ref(_name, ast_type, ast_type_type) = type_param {
            let t = ast_type_type.clone().unwrap();

            CLambdas::add_to_statics_if_lambda(&t, statics);

            if ast_type
                .clone()
                .unwrap()
                .is_reference(type_def_provider, TypeDefBodyTarget::C)
            {
                Ok(true_value.clone())
            } else {
                Ok(false_value.clone())
            }
        } else if let MacroParam::Plain(name, _, _) = type_param {
            if let Some(def) = typed_function_def {
                let resolved_generic_types =
                    def.resolved_generic_types.clone().remove_generics_prefix();

                // TODO type classes, I would like to resolve something like M<T>
                //let gen_name = format!("{}_{}:{}", def.namespace, def.name, name);
                if let Some(t) = resolved_generic_types.get(&name, &Vec::new()) {
                    CLambdas::add_to_statics_if_lambda(&t, statics);

                    let is_ref = if let Some(type_name) =
                        get_reference_type_name(t, &TypeDefBodyTarget::C)
                    {
                        if let Some(t_def) = type_def_provider.get_type_def_by_name(&type_name) {
                            TypeDefBodyCache::type_body_has_references(
                                &t_def.body,
                                &TypeDefBodyTarget::C,
                            )
                        } else {
                            true
                        }
                    } else {
                        false
                    };
                    if is_ref {
                        Ok(true_value.clone())
                    } else {
                        Ok(false_value.clone())
                    }
                } else {
                    Err(format!(
                        "Cannot find generic type {name} : {}",
                        resolved_generic_types
                    ))
                }
            } else {
                Err(format!(
                    "Cannot resolve generic type {name} without a function."
                ))
            }
        } else {
            Err(format!("First argument should be a reference to a value."))
        }
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

fn add_ref_deref_code(
    ref_type: RefType,
    statics: &Statics,
    type_def_provider: &dyn TypeDefProvider,
    code_manipulator: &CCodeManipulator,
    out: &mut String,
    address: &str,
    ast_typed_type: &ASTTypedType,
) {
    if let Some(type_name) = get_reference_type_name(ast_typed_type, &TypeDefBodyTarget::C) {
        let descr = &format!("addref macro type {type_name}");

        if type_name == "_fn" {
            match ref_type {
                RefType::Deref => {
                    TypedFunctionsCreatorC::addref_deref_lambda(
                        code_manipulator,
                        out,
                        RefType::Deref,
                        &address,
                        ast_typed_type,
                        statics,
                    );
                }
                RefType::AddRef => {
                    TypedFunctionsCreatorC::addref_deref_lambda(
                        code_manipulator,
                        out,
                        RefType::AddRef,
                        &address,
                        ast_typed_type,
                        statics,
                    );
                }
            }
        } else {
            match ref_type {
                RefType::Deref => {
                    CodeGenC::call_deref(
                        code_manipulator,
                        out,
                        address,
                        &type_name,
                        descr,
                        type_def_provider,
                    );
                }
                RefType::AddRef => {
                    CodeGenC::call_add_ref(
                        code_manipulator,
                        out,
                        address,
                        &type_name,
                        descr,
                        type_def_provider,
                    );
                }
            }
        }
    }
}

fn get_par_number(
    text_macro: &TextMacro,
    par_index: usize,
    message: &str,
) -> Result<usize, String> {
    let number = match text_macro.parameters.get(par_index) {
        Some(MacroParam::Plain(plain_value, _, _)) => plain_value
            .parse::<usize>()
            .map_err(|_| format!("Param {par_index}, should be the {message} as a number"))?,
        _ => {
            return Err(format!(
                "Param {par_index}, should be the {message} as a number"
            ));
        }
    };
    Ok(number)
}

fn get_par_text_and_type(
    text_macro: &TextMacro,
    par_index: usize,
) -> Result<(String, ASTTypedType), String> {
    let (address, ast_typed_type) = match text_macro.parameters.get(par_index) {
        Some(MacroParam::Plain(address, _ast_type, Some(ast_typed_type))) => {
            (address, ast_typed_type)
        }
        Some(MacroParam::Ref(address, _ast_type, Some(ast_typed_type))) => {
            (address, ast_typed_type)
        }
        Some(it) => return Err(format!("Cannot determine the type of param {it}")),
        _ => return Err(format!("Cannot determine the type of param {par_index}")),
    };

    Ok((address.clone(), ast_typed_type.clone()))
}

fn get_par_text(text_macro: &TextMacro, par_index: usize) -> Result<String, String> {
    return match text_macro.parameters.get(par_index) {
        Some(MacroParam::Plain(address, _ast_type, _ast_typed_type)) => Ok(address.clone()),
        _ => return Err(format!("Expected plain text for param {par_index}")),
    };
}

fn get_type_from_parameter(
    text_macro: &TextMacro,
    par_index: usize,
    statics: &mut Statics,
    typed_function_def: Option<&ASTTypedFunctionDef>,
    function_def: Option<&EnhASTFunctionDef>,
    type_def_provider: &dyn TypeDefProvider,
) -> Result<ASTTypedType, String> {
    let value = text_macro
        .parameters
        .get(par_index)
        .ok_or(format!("Expected param {par_index}"))?;
    if let MacroParam::Ref(_name, _ast_type, Some(ast_type_type)) = value {
        CLambdas::add_to_statics_if_lambda(ast_type_type, statics);

        Ok(ast_type_type.clone())
    } else if let MacroParam::Plain(name, param_ast_type, param_ast_typed_type) = value {
        let (context_generic_types, resolved_generic_types, namespace) =
            if let Some(function_def) = function_def {
                (
                    function_def
                        .resolved_generic_types
                        .clone()
                        .remove_generics_prefix()
                        .iter()
                        .map(|(k, _)| k.0.clone())
                        .collect_vec(),
                    function_def
                        .resolved_generic_types
                        .clone()
                        .remove_generics_prefix(),
                    function_def.namespace.clone(),
                )
            } else if let Some(function_def) = typed_function_def {
                let mut resolved_generic_types = EnhResolvedGenericTypes::new();
                let mut context_generic_types = Vec::new();
                for ((g_name, _), tt) in function_def
                    .resolved_generic_types
                    .clone()
                    .remove_generics_prefix()
                    .iter()
                {
                    if let Some(ast_type) = type_def_provider.get_type_from_typed_type(tt) {
                        resolved_generic_types.insert(g_name.clone(), Vec::new(), ast_type.clone());
                    }
                    context_generic_types.push(g_name.clone());
                }

                (
                    context_generic_types,
                    resolved_generic_types,
                    function_def.namespace.clone(),
                )
            } else {
                return Err(format!(
                    "Cannot resolve type {name} without a function. {} {}",
                    OptionDisplay(param_ast_type),
                    OptionDisplay(param_ast_typed_type)
                ));
            };

        let (ast_type, ast_typed_type) = parse_type(
            name,
            &context_generic_types.as_slice(),
            type_def_provider,
            &resolved_generic_types,
            &text_macro.index.id(),
            &namespace,
            None, //Some(&function_def.original_name),
        )?;

        if let Some(ast_typed_type) = ast_typed_type {
            Ok(ast_typed_type)
        } else {
            return Err(format!(
                "Cannot resolve type {name}. {} {}",
                OptionDisplay(param_ast_type),
                OptionDisplay(&ast_type)
            ));
        }

        /*
        if let Some(def) = typed_function_def {
            let resolved_generic_types =
                def.resolved_generic_types.clone().remove_generics_prefix();

            // TODO type classes, I would like to resolve something like M<T>
            //let gen_name = format!("{}_{}:{}", def.namespace, def.name, name);
            if let Some(t) = resolved_generic_types.get(&name, &Vec::new()) {
                CLambdas::add_to_statics_if_lambda(&t, statics);

                Ok(t.clone())
            } else {
                Err(format!(
                    "Cannot find generic type {name} : {} in {}",
                    resolved_generic_types, text_macro.index
                ))
            }
        } else {
            Err(format!(
                "Cannot resolve generic type {name} without a function."
            ))
        }
        */
    } else {
        Err(format!(
            "Argument {par_index} should be a reference or a type."
        ))
    }
}

#[cfg(test)]
mod tests {
    use rasm_parser::parser::ast::ASTModifiers;

    use crate::{
        codegen::{
            CodeGen,
            c::{
                code_gen_c::{CCodeManipulator, CodeGenC},
                options::COptions,
                text_macro_c::{CAddRefMacro, CCallMacro, CRealTypeNameMacro, CTypeNameMacro},
            },
            enh_ast::{
                EnhASTFunctionBody, EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTType,
            },
            enh_val_context::EnhValContext,
            statics::Statics,
            text_macro::{RefType, TextMacroEvaluator},
            typedef_provider::DummyTypeDefProvider,
        },
        enh_type_check::{
            enh_resolved_generic_types::EnhResolvedGenericTypes,
            typed_ast::{
                ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedStructDef, ASTTypedType,
                ResolvedGenericTypedTypes,
            },
        },
    };

    #[test]
    fn test_macro() {
        let code_manipulator = CCodeManipulator::new(false);

        let mut evaluator = TextMacroEvaluator::new(code_manipulator.clone());
        evaluator.add("realTypeName", CRealTypeNameMacro::new());
        evaluator.add("typeName", CTypeNameMacro::new());
        evaluator.add(
            "addRef",
            CAddRefMacro::new(code_manipulator, RefType::AddRef, true),
        );

        let function_namespace = EnhASTNameSpace::new("test".into(), "path".into());

        let (a_struct, a_struct_typed_type, a_struct_enh_type) = create_struct_and_types(
            "test",
            function_namespace.clone(),
            ASTModifiers::Public,
            Vec::new(),
        );

        let (vec_struct, _, _) = create_struct_and_types(
            "Vec",
            function_namespace.clone(),
            ASTModifiers::Public,
            vec![a_struct_enh_type.clone()],
        );

        let mut resolved_generic_types = ResolvedGenericTypedTypes::new();
        resolved_generic_types.insert("test:T".into(), Vec::new(), a_struct_typed_type.clone());

        let typed_function_def =
            typed_funtion_def("test", resolved_generic_types, function_namespace.clone());

        let type_def_provider =
            DummyTypeDefProvider::new(vec![], vec![a_struct, vec_struct], vec![]);

        let mut statics = &mut Statics::new();
        let result = evaluator
            .translate(
                &mut statics,
                Some(&typed_function_def),
                None,
                "$addRef(tmp:T)",
                false,
                &type_def_provider,
            )
            .unwrap();

        assert_eq!("addRef(tmp);", result.trim());
    }

    #[test]
    fn test_call_macro() {
        let code_manipulator = CCodeManipulator::new(false);

        let mut evaluator = TextMacroEvaluator::new(code_manipulator.clone());
        evaluator.add("call", CCallMacro {});

        let function_namespace = EnhASTNameSpace::new("test".into(), "path".into());

        let (a_struct, a_struct_typed_type, a_struct_enh_type) = create_struct_and_types(
            "test",
            function_namespace.clone(),
            ASTModifiers::Public,
            Vec::new(),
        );

        let (vec_struct, _, _) = create_struct_and_types(
            "Vec",
            function_namespace.clone(),
            ASTModifiers::Public,
            vec![a_struct_enh_type.clone()],
        );

        let mut resolved_generic_types = ResolvedGenericTypedTypes::new();
        resolved_generic_types.insert("test:T".into(), Vec::new(), a_struct_typed_type.clone());

        let typed_function_def =
            typed_funtion_def("test", resolved_generic_types, function_namespace.clone());

        let type_def_provider =
            DummyTypeDefProvider::new(vec![], vec![a_struct, vec_struct], vec![]);

        let mut statics = &mut Statics::new();
        let result = evaluator
            .translate(
                &mut statics,
                Some(&typed_function_def),
                None,
                "$call(aFunction,tmp:T)",
                false,
                &type_def_provider,
            )
            .unwrap();

        assert_eq!("aFunction(tmp)", result);
    }

    #[test]
    fn test_native_calls() {
        let sut = CodeGenC::new(COptions::default(), false, false);

        let function_namespace = EnhASTNameSpace::new("test".into(), "path".into());

        let (a_struct, a_struct_typed_type, _) = create_struct_and_types(
            "AStruct",
            function_namespace.clone(),
            ASTModifiers::Public,
            Vec::new(),
        );

        let type_def_provider = DummyTypeDefProvider::new(vec![], vec![a_struct], vec![]);

        let mut resolved_generic_types = ResolvedGenericTypedTypes::new();
        resolved_generic_types.insert("test:T".into(), Vec::new(), a_struct_typed_type.clone());

        let function_def = funtion_def(
            "aFunction",
            resolved_generic_types.to_enh(&type_def_provider),
            function_namespace,
        );
        let body = "$call(aFunction, value:T)";
        let context = &EnhValContext::new(None);
        let statics = &mut Statics::new();

        let result = sut
            .called_functions(
                None, //Some(&typed_function_def),
                Some(&function_def),
                body,
                context,
                &type_def_provider,
                statics,
            )
            .unwrap();

        assert_eq!(1, result.len());
        let (_, call) = result.get(0).unwrap();

        assert_eq!("aFunction(AStruct1)", format!("{call}"));
    }

    fn typed_funtion_def(
        name: &str,
        resolved_generic_types: ResolvedGenericTypedTypes,
        namespace: EnhASTNameSpace,
    ) -> ASTTypedFunctionDef {
        ASTTypedFunctionDef {
            name: format!("{name}1"),
            namespace,
            parameters: vec![],
            return_type: ASTTypedType::Unit,
            resolved_generic_types,
            index: EnhASTIndex::none(),
            body: ASTTypedFunctionBody::RASMBody(Vec::new()),
            original_name: name.into(),
        }
    }

    fn funtion_def(
        name: &str,
        resolved_generic_types: EnhResolvedGenericTypes,
        namespace: EnhASTNameSpace,
    ) -> EnhASTFunctionDef {
        EnhASTFunctionDef {
            name: format!("{name}1"),
            parameters: vec![],
            return_type: EnhASTType::Unit,
            original_name: name.into(),
            body: EnhASTFunctionBody::RASMBody(Vec::new()),
            generic_types: Vec::new(),
            resolved_generic_types,
            index: EnhASTIndex::none(),
            modifiers: ASTModifiers::Public,
            namespace,
            rank: 0,
            target: None,
        }
    }

    fn create_struct_and_types(
        base_name: &str,
        namespace: EnhASTNameSpace,
        modifiers: ASTModifiers,
        param_types: Vec<EnhASTType>,
    ) -> (ASTTypedStructDef, ASTTypedType, EnhASTType) {
        let a_struct_typed_type = ASTTypedType::Struct {
            namespace: namespace.clone(),
            name: format!("{base_name}1"),
        };

        let a_struct_enh_type = EnhASTType::Custom {
            namespace: namespace.clone(),
            name: format!("{base_name}1"),
            param_types,
            index: EnhASTIndex::none(),
        };

        let struct_def = ASTTypedStructDef {
            namespace,
            name: format!("{base_name}1"),
            modifiers,
            properties: Vec::new(),
            ast_type: a_struct_enh_type.clone(),
            ast_typed_type: a_struct_typed_type.clone(),
            index: EnhASTIndex::none(),
        };

        (struct_def, a_struct_typed_type, a_struct_enh_type)
    }
}
