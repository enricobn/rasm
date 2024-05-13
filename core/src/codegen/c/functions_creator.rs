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

    fn enum_match_body(&self, name: &str, enum_def: &ASTEnumDef) -> String {
        let mut result = String::new();

        for (i, variant) in enum_def.variants.iter().enumerate() {
            let safe_name = format!(
                "{}_{}_{}",
                enum_def.namespace.safe_name(),
                enum_def.name,
                variant.name
            );
            self.code_manipulator.add(
                &mut result,
                &format!("if ($value->variant_num == {i}) {{"),
                None,
                true,
            );

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

            self.code_manipulator.add(
                &mut result,
                &format!(
                    "return ${}->functionPtr({});",
                    variant.name,
                    args.join(", ")
                ),
                None,
                true,
            );
            self.code_manipulator.add(&mut result, "}", None, true);
        }

        result
    }

    fn enum_match_one_body(&self, enum_def: &ASTEnumDef, variant: &ASTEnumVariantDef) -> String {
        // TODO
        String::new()
    }

    fn debug(&self) -> bool {
        false
    }

    fn enum_variant_constructor_body(
        &self,
        module: &mut ASTModule,
        enum_def: &ASTEnumDef,
        statics: &mut Statics,
        variant_num: usize,
        variant: &ASTEnumVariantDef,
        descr: &String,
    ) -> (String, bool) {
        let mut result = String::new();
        self.code_manipulator
            .add(&mut result, "$enumDeclaration(e_)", None, true);
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
            .add(&mut result, "e_->variant = v_;", None, true);
        self.code_manipulator.add(
            &mut result,
            &format!("e_->variant_num = {variant_num};"),
            None,
            true,
        );

        self.code_manipulator
            .add(&mut result, "return e_;", None, true);

        (result, false)
    }

    fn struct_constructor_body(&self, struct_def: &ASTStructDef) -> String {
        let mut result = String::new();
        self.code_manipulator
            .add(&mut result, "$structDeclaration(s_)", None, true);
        for property_def in struct_def.properties.iter() {
            self.code_manipulator.add(
                &mut result,
                &format!("s_->{} = ${};", property_def.name, property_def.name),
                None,
                true,
            );
        }

        self.code_manipulator
            .add(&mut result, "return s_;", None, true);
        result
    }

    fn struct_property_body(&self, i: usize, name: &str) -> String {
        let mut result = String::new();
        self.code_manipulator
            .add(&mut result, &format!("$v->{name}"), None, true);
        result
    }

    fn struct_setter_body(&self, i: usize, name: &str) -> String {
        let mut result = String::new();

        self.code_manipulator
            .add(&mut result, "$structDeclaration(newStruct)", None, true);

        self.code_manipulator.add(
            &mut result,
            &format!("memcpy(newStruct, $receiver, sizeof("),
            None,
            true,
        );

        self.code_manipulator
            .add(&mut result, "$structType()", None, true);

        self.code_manipulator.add(&mut result, "));", None, true);

        self.code_manipulator
            .add(&mut result, &format!("newStruct->{name} = $v;"), None, true);
        // TODO it is not correct, it should be copied
        self.code_manipulator
            .add(&mut result, &format!("return newStruct;"), None, true);
        result
    }

    fn struct_setter_lambda_body(&self, i: usize) -> String {
        // TODO
        String::new()
    }
}
