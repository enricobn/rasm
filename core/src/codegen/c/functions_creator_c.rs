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

use crate::codegen::c::code_gen_c::CCodeManipulator;
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::enh_ast::EnhModuleInfo;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::transformations::functions_creator::FunctionsCreator;
use rasm_parser::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTModule, ASTStructDef, ASTStructPropertyDef,
};

use super::code_gen_c::CodeGenC;

pub struct CFunctionsCreator {
    code_manipulator: CCodeManipulator,
}

impl CFunctionsCreator {
    pub fn new(debug: bool) -> Self {
        Self {
            code_manipulator: CCodeManipulator::new(debug),
        }
    }

    fn struct_type_to_string(struct_def: &ASTStructDef) -> String {
        if struct_def.type_parameters.is_empty() {
            struct_def.name.clone()
        } else {
            struct_def.name.clone() + "<" + &struct_def.type_parameters.join(",") + ">"
        }
    }
}

impl FunctionsCreator for CFunctionsCreator {
    fn create_globals(&self, _module: &mut EnhancedASTModule, _statics: &mut Statics) {
        // TODO
    }

    fn enum_match_body(&self, _name: &str, enum_def: &ASTEnumDef) -> String {
        let mut result = String::new();

        self.code_manipulator.add(
            &mut result,
            &format!("struct Enum *enum_value = (struct Enum *) $value->address;"),
            None,
            true,
        );

        for (i, variant) in enum_def.variants.iter().enumerate() {
            let stmt = if i == 0 {
                "if"
            } else if i < enum_def.variants.len() - 1 {
                "else if"
            } else {
                "else"
            };

            if i < enum_def.variants.len() - 1 || enum_def.variants.len() == 1 {
                self.code_manipulator.add(
                    &mut result,
                    &format!("{stmt} (enum_value->variant_num == {i}) {{"),
                    None,
                    true,
                );
            } else {
                self.code_manipulator
                    .add(&mut result, &format!("{stmt} {{"), None, true);
            }

            if variant.parameters.is_empty() {
                self.code_manipulator.add_rows(
                    &mut result,
                    vec![
                        &format!("return $castAddress(${})", variant.name),
                        &format!("->functionPtr(${});", variant.name),
                    ],
                    None,
                    true,
                );
            } else {
                self.code_manipulator.add(
                    &mut result,
                    &format!("$enumVariantAssignment(variant, {})", variant.name),
                    None,
                    true,
                );

                let mut args = Vec::new();

                for parameter in variant.parameters.iter() {
                    args.push(format!("variant->{}", parameter.name));
                }

                //args.push("$value->variant".to_string());
                args.push(format!("${}", variant.name));

                self.code_manipulator.add_rows(
                    &mut result,
                    vec![
                        &format!("return $castAddress(${})", variant.name),
                        &format!("->functionPtr({});", args.join(", ")),
                    ],
                    None,
                    true,
                );
            }
            self.code_manipulator.add(&mut result, "}", None, true);
        }

        /*
        TODO check for unknown, but at the start
        self.code_manipulator.add_rows(
            &mut result,
            vec![&format!(
                "printf(\"unknown variant for {}\\n\");",
                enum_def.name
            )],
            None,
            true,
        );
        */

        result
    }

    fn enum_match_one_body(&self, enum_def: &ASTEnumDef, variant: &ASTEnumVariantDef) -> String {
        let mut result = String::new();

        self.code_manipulator.add(
            &mut result,
            &format!("struct Enum *enum_value = (struct Enum *) $value->address;"),
            None,
            true,
        );

        if let Some(i) = enum_def
            .variants
            .iter()
            .position(|it| it.name == variant.name)
        {
            self.code_manipulator.add(
                &mut result,
                &format!("if (enum_value->variant_num == {i}) {{"),
                None,
                true,
            );

            if !variant.parameters.is_empty() {
                self.code_manipulator.add(
                    &mut result,
                    &format!("$enumVariantAssignment(variant, {})", variant.name),
                    None,
                    true,
                );
            }

            let mut args = Vec::new();

            for parameter in variant.parameters.iter() {
                args.push(format!("variant->{}", parameter.name));
            }

            //args.push("$value->variant".to_string());
            args.push(format!("${}", variant.name));

            self.code_manipulator.add(
                &mut result,
                &format!(
                    "return $castAddress(${})->functionPtr({});",
                    variant.name,
                    args.join(", ")
                ),
                None,
                true,
            );
            self.code_manipulator.add_rows(
                &mut result,
                vec![
                    "} else {",
                    "return $castAddress($elseLambda)->functionPtr(elseLambda);",
                    "}",
                ],
                None,
                true,
            );
        } else {
            // it should not happen
            panic!("Cannot find variant with name `{}`", variant.name);
        }

        result
    }

    fn debug(&self) -> bool {
        false
    }

    fn enum_variant_constructor_body(
        &self,
        _module: &ASTModule,
        enum_def: &ASTEnumDef,
        _statics: &mut Statics,
        variant_num: usize,
        variant: &ASTEnumVariantDef,
        _descr: &String,
        info: &EnhModuleInfo,
    ) -> String {
        if variant.parameters.is_empty() {
            return format!(
                "return {};",
                CodeGenC::variant_const_name(&info.namespace, &enum_def.name, &variant.name)
            );
        }

        let mut result = String::new();

        self.code_manipulator.add(
            &mut result,
            "struct RasmPointer_* e__ = rasmMalloc(sizeof(struct Enum));",
            None,
            true,
        );

        self.code_manipulator.add(
            &mut result,
            "struct Enum* e_ = (struct Enum*)e__->address;",
            None,
            true,
        );

        self.code_manipulator.add(
            &mut result,
            &format!("$enumVariantDeclaration(v_,{})", variant.name),
            None,
            true,
        );

        for property_def in variant.parameters.iter() {
            self.code_manipulator.add(
                &mut result,
                &format!("v_->{} = ${};", property_def.name, property_def.name),
                None,
                true,
            );
        }

        self.code_manipulator
            .add(&mut result, "e_->variant = v__;", None, true);
        self.code_manipulator.add(
            &mut result,
            &format!("e_->variant_num = {variant_num};"),
            None,
            true,
        );

        self.code_manipulator
            .add(&mut result, "return e__;", None, true);

        result
    }

    fn struct_constructor_body(&self, struct_def: &ASTStructDef) -> String {
        let mut result = String::new();
        let struct_type = Self::struct_type_to_string(struct_def);

        self.code_manipulator.add(
            &mut result,
            &format!("$allocateVar(s__, {struct_type})"),
            None,
            true,
        );

        for property_def in struct_def.properties.iter() {
            self.code_manipulator.add(
                &mut result,
                &format!(
                    "$castAddress(s__:{struct_type})->{} = ${};",
                    property_def.name, property_def.name
                ),
                None,
                true,
            );
        }

        self.code_manipulator
            .add(&mut result, "return s__;", None, true);
        result
    }

    fn struct_property_body(
        &self,
        _i: usize,
        s: &ASTStructDef,
        p: &ASTStructPropertyDef,
    ) -> String {
        let mut result = String::new();

        let struct_type = Self::struct_type_to_string(s);

        self.code_manipulator.add_rows(
            &mut result,
            vec![
                "$inline()",
                &format!("return (($typeName({})", struct_type),
                &format!(")$v->address)->{};", p.name),
            ],
            None,
            false,
        );
        result
    }

    fn struct_setter_body(
        &self,
        _i: usize,
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
    ) -> String {
        let mut result = String::new();
        let struct_type = Self::struct_type_to_string(struct_def);

        self.code_manipulator.add_rows(
            &mut result,
            vec![
                "$include(<string.h>)",
                "struct RasmPointer_ *struct_result = NULL;",
                &format!("$typeName({struct_type}) struct_result_ = NULL;"),
                "if(receiver->count == 1) {",
                "struct_result = receiver;",
                "struct_result_ = $castAddress($receiver);",
                &format!(
                    "$deref(struct_result_->{}:{})",
                    property_def.name, property_def.ast_type
                ),
                "$addRef($v)",
                "} else {",
                &format!("$allocateVar(newStruct, {struct_type})"),
                "memcpy(newStruct->address, $receiver->address, sizeof(",
                &format!("$typeNameNoRef({struct_type})"),
                "));",
                "struct_result = newStruct;",
                "struct_result_ = newStruct->address;",
                "}",
                &format!("struct_result_->{} = $v;", property_def.name),
                "return struct_result;",
            ],
            None,
            true,
        );
        result
    }

    fn struct_setter_lambda_body(
        &self,
        _i: usize,
        struct_def: &ASTStructDef,
        def: &ASTStructPropertyDef,
    ) -> String {
        let mut result = String::new();
        let name = &def.name;

        let struct_type = Self::struct_type_to_string(struct_def);

        self.code_manipulator.add_rows(
            &mut result,
            vec![
                "$include(<string.h>)",
                "struct RasmPointer_ *struct_result = NULL;",
                &format!("$typeName({struct_type}) struct_result_ = NULL;"),
                &format!("$realTypeName({}) old_property_value;", def.ast_type),
                "if(receiver->count == 1) {",
                "struct_result = receiver;",
                "struct_result_ = $castAddress($receiver);",
                &format!("old_property_value = struct_result_->{name};"),
                "} else {",
                &format!("$allocateVar(newStruct, {struct_type})"),
                "memcpy(newStruct->address, $castAddress($receiver), sizeof(",
                &format!("$typeNameNoRef({struct_type})"),
                "));",
                "struct_result = newStruct;",
                &format!("struct_result_ = $castAddress(newStruct:{struct_type});"),
                "}",
                &format!(
                    "struct_result_->{name} = $castAddress($f)->functionPtr(struct_result_->{name}, f);"
                ),
                "if(receiver->count == 1) {",
                &format!("$addRef(struct_result_->{name}:{});", def.ast_type),
                &format!("$deref(old_property_value:{})", def.ast_type),
                "}",
                "return struct_result;",
            ],
            None,
            true,
        );

        result
    }
}
