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
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::parser::ast::{ASTEnumDef, ASTEnumVariantDef, ASTModule, ASTStructDef};
use crate::transformations::functions_creator::FunctionsCreator;

use super::code_gen_c::CodeGenC;

pub struct CFunctionsCreator {
    code_manipulator: CCodeManipulator,
}

impl CFunctionsCreator {
    pub fn new() -> Self {
        Self {
            code_manipulator: CCodeManipulator::new(),
        }
    }
}

impl FunctionsCreator for CFunctionsCreator {
    fn create_globals(&self, module: &mut EnhancedASTModule, statics: &mut Statics) {
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
            self.code_manipulator.add(
                &mut result,
                &format!("if (enum_value->variant_num == {i}) {{"),
                None,
                true,
            );

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

        self.code_manipulator.add(
            &mut result,
            &format!("printf(\"unknown variant for {}\\n\");", enum_def.name),
            None,
            true,
        );

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
        module: &mut ASTModule,
        enum_def: &ASTEnumDef,
        _statics: &mut Statics,
        variant_num: usize,
        variant: &ASTEnumVariantDef,
        _descr: &String,
    ) -> (String, bool) {
        if variant.parameters.is_empty() {
            return (
                format!(
                    "return {};",
                    CodeGenC::variant_const_name(
                        todo!(), //&enum_def.namespace,
                        &enum_def.name,
                        &variant.name
                    )
                ),
                false,
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

        (result, false)
    }

    fn struct_constructor_body(&self, struct_def: &ASTStructDef) -> String {
        let mut result = String::new();
        self.code_manipulator
            .add(&mut result, "$structDeclaration(s__)", None, true);

        self.code_manipulator
            .add(&mut result, "$structType()", None, true);

        self.code_manipulator
            .add(&mut result, "*s_ = ($structType()", None, true);

        self.code_manipulator
            .add(&mut result, " *) s__->address;", None, true);

        for property_def in struct_def.properties.iter() {
            self.code_manipulator.add(
                &mut result,
                &format!("s_->{} = ${};", property_def.name, property_def.name),
                None,
                true,
            );
        }

        self.code_manipulator
            .add(&mut result, "return s__;", None, true);
        result
    }

    fn struct_property_body(&self, _i: usize, name: &str) -> (String, bool) {
        let mut result = String::new();

        self.code_manipulator
            .add(&mut result, &format!("(($typeName($v)"), None, false);
        self.code_manipulator
            .add(&mut result, &format!(")$v->address)->{name}"), None, false);
        (result, true)
    }

    fn struct_setter_body(&self, _i: usize, name: &str) -> String {
        let mut result = String::new();
        self.code_manipulator.add_rows(
            &mut result,
            vec![
                "$include(<string.h>)",
                "$structDeclaration(newStruct)",
                "memcpy(newStruct->address, $receiver->address, sizeof(",
                "$structType()",
                "));",
                &format!("newStruct_->{name} = $v;"),
                "return newStruct;",
            ],
            None,
            true,
        );
        result
    }

    fn struct_setter_lambda_body(&self, _i: usize, name: &str) -> String {
        let mut result = String::new();
        self.code_manipulator.add_rows(
            &mut result,
            vec![
                "$include(<string.h>)",
                "$structDeclaration(newStruct)",
                &format!("$structType()* s = ($structType()*)newStruct->address;"),
                "memcpy(s, $castAddress($receiver), sizeof(",
                "$structType()",
                "));",
                &format!("s->{name} = $castAddress($f)->functionPtr(s->{name}, f);"),
                "return newStruct;",
            ],
            None,
            true,
        );
        result
    }
}
