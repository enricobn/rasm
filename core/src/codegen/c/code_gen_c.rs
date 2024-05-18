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

use crate::codegen::c::any::{CConsts, CFunctionsDeclarations, CInclude, CLambda, CLambdas};
use crate::codegen::c::function_call_parameters::CFunctionCallParameters;
use crate::codegen::c::text_macro::{
    CCallMacro, CEnumDeclarationMacro, CEnumVariantAssignmentMacro, CEnumVariantDeclarationMacro,
    CIncludeMacro, CStructDeclarationMacro, CStructTypeMacro,
};
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::function_call_parameters::FunctionCallParameters;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::stack::StackVals;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{TextMacroEval, TextMacroEvaluator};
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{AsmOptions, CodeGen, TypedValKind};
use crate::parser::ast::{ASTIndex, ASTNameSpace, ValueType};
use crate::type_check::typed_ast::{
    ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedType,
    BuiltinTypedTypeKind,
};
use linked_hash_map::LinkedHashMap;

#[derive(Clone)]
pub struct CCodeManipulator;

impl CCodeManipulator {
    pub fn new() -> Self {
        Self {}
    }
}

impl CodeManipulator for CCodeManipulator {
    fn add_comment(&self, out: &mut String, comment: &str, indent: bool) {
        self.add(out, &format!("// {comment}"), None, indent);
    }

    fn remove_comments_from_line(&self, line: String) -> String {
        if let Some(pos) = line.find("//") {
            if pos > 0 {
                line.split_at(pos).0.to_string()
            } else {
                String::new()
            }
        } else {
            line
        }
    }
}

pub struct CodeGenC {
    code_manipulator: CCodeManipulator,
    options: AsmOptions,
}

impl CodeGenC {
    pub fn new() -> Self {
        Self {
            code_manipulator: CCodeManipulator,
            options: AsmOptions::default(),
        }
    }

    pub fn type_to_string(ast_type: &ASTTypedType, statics: &Statics) -> String {
        match ast_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => "char*".to_string(),
                BuiltinTypedTypeKind::I32 => "int".to_string(),
                BuiltinTypedTypeKind::Bool => "int".to_string(),
                BuiltinTypedTypeKind::Char => "char*".to_string(),
                BuiltinTypedTypeKind::F32 => "float".to_string(),
                BuiltinTypedTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    if let Some(name) = statics
                        .any::<CLambdas>()
                        .unwrap()
                        .find_name(parameters, return_type)
                    {
                        format!("struct {name}*")
                    } else {
                        panic!("Cannot find lambda def");
                    }
                }
                _ => todo!("{kind:?}"),
            },
            ASTTypedType::Unit => "void".to_string(),
            ASTTypedType::Struct { namespace, name } => {
                format!("struct {}_{name}*", namespace.safe_name())
            }
            ASTTypedType::Enum { namespace, name } => {
                format!("struct {}_{name}*", namespace.safe_name())
            }
            ASTTypedType::Type {
                namespace,
                name,
                native_type,
            } => native_type
                .clone()
                .expect(&format!(
                    "type in C must define a native type: {namespace}:{name}"
                ))
                .to_string(),
            _ => todo!("{ast_type}"),
        }
    }

    pub fn escape_string(s: &str) -> String {
        let mut result = s.to_string();
        result = result.replace("\\\\", "\\");
        result = result.replace("\"", "\\\"");
        result
    }
}

impl<'a> CodeGen<'a, Box<CFunctionCallParameters>> for CodeGenC {
    fn options(&self) -> &AsmOptions {
        &self.options // TODO
    }

    fn end_main(&self, code: &mut String) {}

    fn transform_before(&self, stack: &StackVals, before: String) -> String {
        before
    }

    fn create_command_line_arguments(&self, generated_code: &mut String) {
        // TODO
    }

    fn call_lambda_parameter(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        _stack_vals: &StackVals,
        _kind: &TypedValKind,
        call_parameters: &Box<CFunctionCallParameters>,
        return_value: bool,
        is_inner_call: bool,
    ) {
        let mut args = call_parameters
            .parameters_values()
            .iter()
            .map(|(name, value)| (value.as_str(), None))
            .collect::<Vec<_>>();
        args.push((function_call.function_name.as_str(), None));
        self.call_function(
            before,
            &format!("{}->functionPtr", function_call.function_name),
            &args,
            None,
            return_value,
            is_inner_call,
        );

        self.code_manipulator.add_comment(
            before,
            &format!("call_lambda_parameter {function_call}"),
            true,
        );
    }

    fn call_lambda(
        &self,
        _function_call: &ASTTypedFunctionCall,
        before: &mut String,
        _stack_vals: &StackVals,
        index_in_lambda_space: usize,
        call_parameters: &Box<CFunctionCallParameters>,
        ast_type_type: &ASTTypedType,
        statics: &Statics,
        return_value: bool,
        is_inner_call: bool,
    ) {
        let mut args = call_parameters
            .parameters_values()
            .iter()
            .map(|(name, value)| (value.as_str(), None))
            .collect::<Vec<_>>();
        let casted_lambda = format!(
            "(({})_lambda->args[{}])",
            CodeGenC::type_to_string(ast_type_type, statics),
            index_in_lambda_space - 1
        );

        args.push((&casted_lambda, None));

        self.call_function(
            before,
            &format!("{casted_lambda}->functionPtr"),
            &args,
            None,
            return_value,
            is_inner_call,
        );
    }

    fn restore_stack(
        &self,
        function_call: &ASTTypedFunctionCall,
        before: &mut String,
        call_parameters: &mut Box<CFunctionCallParameters>,
    ) {
        // TODO
    }

    fn added_to_stack_for_call_parameter(
        &self,
        added_to_stack: &String,
        call_parameters: &Box<CFunctionCallParameters>,
    ) -> String {
        // TODO
        String::new()
    }

    fn function_call_parameters<'b, 'c>(
        &self,
        parameters: &'b Vec<ASTTypedParameterDef>,
        inline: bool,
        immediate: bool,
        stack_vals: &'c StackVals,
        id: usize,
    ) -> Box<CFunctionCallParameters> {
        Box::new(CFunctionCallParameters::new(
            parameters.clone(),
            inline,
            stack_vals.clone(),
            immediate,
        ))
    }

    fn store_function_result_in_stack(
        &self,
        code: &mut String,
        _address_relative_to_bp: i32,
        name: &str,
        typed_type: &ASTTypedType,
        statics: &Statics,
    ) {
        code.insert_str(
            0,
            &format!("{} {} = ", Self::type_to_string(typed_type, statics), name),
        );
    }

    fn add_ref(
        &self,
        name: &str,
        statics: &mut Statics,
        body: &mut String,
        typed_module: &ASTTypedModule,
        index: &ASTIndex,
        type_name: &String,
    ) {
        // TODO
    }

    fn call_deref_for_let_val(
        &self,
        name: &str,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
    ) -> String {
        // TODO
        "// call_deref_for_let_val".to_string()
    }

    fn call_add_ref_for_let_val(
        &self,
        name: &str,
        index: &ASTIndex,
        before: &mut String,
        statics: &mut Statics,
        address_relative_to_bp: &usize,
        type_name: &String,
        typed_module: &ASTTypedModule,
    ) {
        // TODO
    }

    fn set_let_const_for_function_call_result(&self, statics_key: &str, body: &mut String) {
        todo!()
    }

    fn set_let_for_value_ref(
        &self,
        _stack: &StackVals,
        before: &mut String,
        _address_relative_to_bp: usize,
        val_name: &String,
        typed_val_kind: &TypedValKind,
        statics: &Statics,
        name: &str,
    ) -> ASTTypedType {
        // todo!("{val_name}")
        let t = typed_val_kind.typed_type();
        self.add(
            before,
            &format!("{} {name} = {val_name};", Self::type_to_string(t, statics)),
            None,
            true,
        );
        t.clone()
    }

    fn set_let_for_string_literal(
        &self,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        address_relative_to_bp: usize,
        value: &String,
        typed_type: &ASTTypedType,
        stack: &StackVals,
    ) {
        if is_const {
            CConsts::add_to_statics(
                statics,
                format!("const char* {name} = \"{}\";", Self::escape_string(value)),
            );
        } else {
            self.add(
                before,
                &format!("char* {name} = \"{}\";", Self::escape_string(value)),
                None,
                true,
            );
        }
    }

    fn set_let_for_value(
        &self,
        before: &mut String,
        name: &str,
        is_const: bool,
        statics: &mut Statics,
        body: &mut String,
        address_relative_to_bp: usize,
        value_type: &ValueType,
        typed_type: &ASTTypedType,
    ) {
        let value = self.value_to_string(value_type);

        if is_const {
            CConsts::add_to_statics(
                statics,
                format!(
                    "const {} {name} = {};",
                    CodeGenC::type_to_string(typed_type, statics),
                    value
                ),
            );
        } else {
            self.add(
                before,
                &format!(
                    "{} {name} = {};",
                    CodeGenC::type_to_string(typed_type, statics),
                    value
                ),
                None,
                true,
            );
        }
    }

    fn reserve_return_register(&self, out: &mut String, stack: &StackVals) {
        // TODO
    }

    fn function_def(
        &'a self,
        out: &mut String,
        function_def: &ASTTypedFunctionDef,
        statics: &mut Statics,
    ) {
        let mut args = Vec::new();
        for par in function_def.parameters.iter() {
            // probably sometimes we need to add the lambda def here
            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters,
                return_type,
            }) = &par.ast_type
            {
                CLambdas::add_to_statics(
                    statics,
                    CLambda::new(parameters.clone(), return_type.as_ref().clone()),
                );
            }

            let arg_type = Self::type_to_string(&par.ast_type, statics);
            args.push(format!("{arg_type} {}", par.name));
        }

        // probably sometimes we need to add the lambda def here
        if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
            parameters,
            return_type,
        }) = &function_def.return_type
        {
            CLambdas::add_to_statics(
                statics,
                CLambda::new(parameters.clone(), return_type.as_ref().clone()),
            );
        }

        self.add(
            out,
            &format!(
                "{} {}({}) {{",
                Self::type_to_string(&function_def.return_type, statics),
                function_def.name,
                args.join(", ")
            ),
            None,
            false,
        );

        CFunctionsDeclarations::add_to_statics(
            statics,
            format!(
                "{} {}({});",
                Self::type_to_string(&function_def.return_type, statics),
                function_def.name,
                args.join(", ")
            ),
        );
    }

    fn word_len(&self) -> usize {
        todo!()
    }

    fn stack_pointer(&self) -> &str {
        todo!()
    }

    fn word_size(&self) -> &str {
        todo!()
    }

    fn reserve_lambda_space(&self, before: &mut String, stack: &StackVals) {
        // TODO
    }

    fn value_as_return(&self, before: &mut String, v: &str) {
        self.code_manipulator
            .add(before, &format!("return {v};"), None, true);
    }

    fn string_literal_return(&self, statics: &mut Statics, before: &mut String, value: &String) {
        self.code_manipulator
            .add(before, &format!("return \"{value}\";"), None, true);
    }

    fn get_text_macro_evaluator(&self) -> TextMacroEvaluator {
        let mut evaluators: LinkedHashMap<String, Box<dyn TextMacroEval>> = LinkedHashMap::new();
        evaluators.insert("call".to_string(), Box::new(CCallMacro));
        evaluators.insert("include".to_string(), Box::new(CIncludeMacro));
        evaluators.insert(
            "structDeclaration".to_string(),
            Box::new(CStructDeclarationMacro),
        );
        evaluators.insert("structType".to_string(), Box::new(CStructTypeMacro));
        evaluators.insert(
            "enumVariantDeclaration".to_string(),
            Box::new(CEnumVariantDeclarationMacro),
        );
        evaluators.insert(
            "enumDeclaration".to_string(),
            Box::new(CEnumDeclarationMacro),
        );

        evaluators.insert(
            "enumVariantAssignment".to_string(),
            Box::new(CEnumVariantAssignmentMacro),
        );

        TextMacroEvaluator::new(evaluators, Box::new(CCodeManipulator::new()))
    }

    fn print_memory_info(&self, native_code: &mut String) {
        todo!()
    }

    fn optimize_unused_functions(&self) -> bool {
        false // TODO
    }

    fn initialize_static_values(&self, generated_code: &mut String) {
        // TODO
    }

    fn debug(&self) -> bool {
        false
    }

    fn call_function_simple(
        &self,
        out: &mut String,
        function_name: &str,
        call_parameters: Option<&Box<CFunctionCallParameters>>,
        return_value: bool,
        is_inner_call: bool,
    ) {
        let args = call_parameters
            .unwrap()
            .parameters_values()
            .iter()
            .map(|(name, value)| (value.as_str(), None))
            .collect::<Vec<_>>();
        self.call_function(out, function_name, &args, None, return_value, is_inner_call);
        //println!("call_parameters, {}", call_parameters.unwrap().before());
        //todo!("call_function_simple {function_name}")
    }

    fn call_function(
        &self,
        out: &mut String,
        function_name: &str,
        args: &[(&str, Option<&str>)],
        comment: Option<&str>,
        return_value: bool,
        is_inner_call: bool,
    ) {
        let arg_vec = args
            .to_vec()
            .iter()
            .map(|it| it.0.to_string())
            .collect::<Vec<_>>();

        let prefix = if return_value { "return " } else { "" };
        let suffix = if is_inner_call { "" } else { ";" };

        let inner_call = format!("{prefix}{function_name}({}){suffix}", arg_vec.join(", "));

        if is_inner_call {
            out.push_str(&inner_call);
        } else {
            self.add(out, &inner_call, None, true);
        }
    }

    fn add_comment(&self, out: &mut String, comment: &str, indent: bool) {
        self.code_manipulator.add_comment(out, comment, indent);
    }

    fn add_rows(&self, out: &mut String, code: Vec<&str>, comment: Option<&str>, indent: bool) {
        self.code_manipulator.add_rows(out, code, comment, indent);
    }

    fn add(&self, out: &mut String, code: &str, comment: Option<&str>, indent: bool) {
        self.code_manipulator.add(out, code, comment, indent);
    }

    fn add_empty_line(&self, out: &mut String) {
        self.code_manipulator.add_empty_line(out);
    }

    fn remove_comments_from_line(&self, line: String) -> String {
        self.code_manipulator.remove_comments_from_line(line)
    }

    fn preamble(&self, code: &mut String) {}

    fn create_lambda_add_ref_like_function(
        &self,
        namespace: &ASTNameSpace,
        lambda_space: &LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        statics: &mut Statics,
        name: &str,
        is_deref: bool,
    ) -> Option<ASTTypedFunctionDef> {
        todo!()
    }

    fn reserve_local_vals(&self, stack: &StackVals, out: &mut String) {}

    fn generate_statics_code(
        &self,
        statics: &Statics,
        typed_module: &ASTTypedModule,
    ) -> (String, String) {
        // TODO
        let mut before = String::new();
        let after = String::new();

        if let Some(includes) = statics.any::<CInclude>() {
            for inc in includes.unique() {
                self.add(&mut before, &format!("#include {inc}"), None, false);
            }
            self.add_empty_line(&mut before);
        }

        if let Some(consts) = statics.any::<CConsts>() {
            for c in consts.vec.iter() {
                self.add(&mut before, c, None, false);
            }
            self.add_empty_line(&mut before);
        }

        if let Some(clambdas) = statics.any::<CLambdas>() {
            for (i, clambda) in clambdas.lambdas.iter().enumerate() {
                self.add(
                    &mut before,
                    &format!("struct {} {{", clambda.name),
                    None,
                    false,
                );
                self.add(&mut before, "void **args;", None, true);
                let mut args = clambda
                    .args
                    .iter()
                    .map(|it| Self::type_to_string(it, statics))
                    .collect::<Vec<_>>()
                    .join(", ");

                if !args.is_empty() {
                    args.push_str(", ");
                }

                self.add(
                    &mut before,
                    &format!(
                        "{} (*functionPtr)({args}struct {}*);",
                        Self::type_to_string(&clambda.return_type, statics),
                        clambda.name
                    ),
                    None,
                    true,
                );
                self.add(&mut before, "};", None, false);
                self.add_empty_line(&mut before);
            }
            self.add_empty_line(&mut before);
        }

        for s in typed_module.structs.iter() {
            self.add(
                &mut before,
                &format!("struct {}_{} {{", s.namespace.safe_name(), s.name),
                None,
                false,
            );
            for property in s.properties.iter() {
                self.add(
                    &mut before,
                    &format!(
                        "{} {};",
                        CodeGenC::type_to_string(&property.ast_type, statics),
                        property.name
                    ),
                    None,
                    true,
                );
            }
            self.add(&mut before, "};", None, false);
        }

        if !typed_module.structs.is_empty() {
            self.add_empty_line(&mut before);
        }

        for s in typed_module.enums.iter() {
            self.add(
                &mut before,
                &format!("struct {}_{} {{", s.namespace.safe_name(), s.name),
                None,
                false,
            );
            self.add(&mut before, "void *variant;", None, true);
            self.add(&mut before, "int variant_num;", None, true);
            self.add(&mut before, "};", None, false);

            for variant in s.variants.iter() {
                self.add(
                    &mut before,
                    &format!(
                        "struct {}_{}_{} {{",
                        s.namespace.safe_name(),
                        s.name,
                        variant.name
                    ),
                    None,
                    false,
                );

                for property in variant.parameters.iter() {
                    self.add(
                        &mut before,
                        &format!(
                            "{} {};",
                            CodeGenC::type_to_string(&property.ast_type, statics),
                            property.name
                        ),
                        None,
                        true,
                    );
                }

                self.add(&mut before, "};", None, false);
            }
        }

        if !typed_module.enums.is_empty() {
            self.add_empty_line(&mut before);
        }

        if let Some(declarations) = statics.any::<CFunctionsDeclarations>() {
            for c in declarations.vec.iter() {
                self.add(&mut before, c, None, false);
            }
            self.add_empty_line(&mut before);
        }

        self.add(&mut before, "static int _argc;", None, false);
        self.add(&mut before, "static char **_argv;", None, false);
        self.add(&mut before, "int main(int argc, char **argv)", None, false);
        self.add(&mut before, "{", None, false);
        self.add(&mut before, "_argc = argc;", None, true);
        self.add(&mut before, "_argv = argv;", None, true);

        (before, after)
    }

    fn function_preamble(&self, out: &mut String) {
        // TODO
    }

    fn define_debug(&self, out: &mut String) {
        todo!()
    }

    fn restore(&self, stack: &StackVals, out: &mut String) {
        // TODO
    }

    fn function_end(&self, out: &mut String, add_return: bool) {
        self.add(out, "}", None, false);
    }

    fn add_statics(&self, statics: &mut Statics) {
        // TODO
    }

    fn value_to_string(&self, value_type: &ValueType) -> String {
        match value_type {
            ValueType::Boolean(b) => if *b { "1" } else { "0" }.to_string(),
            ValueType::I32(v) => format!("{v}"),
            ValueType::Char(v) => format!("\"{}\"", CodeGenC::escape_string(&v.to_string())),
            ValueType::F32(v) => format!("{v}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::c::code_gen_c::CodeGenC;
    use crate::codegen::statics::Statics;
    use crate::codegen::typedef_provider::DummyTypeDefProvider;
    use crate::codegen::val_context::ValContext;
    use crate::codegen::CodeGen;
    use crate::utils::SliceDisplay;

    #[test]
    fn called_functions() {
        let sut = CodeGenC::new();
        let mut statics = Statics::new();

        let functions = sut
            .called_functions(
                None,
                None,
                //"$call(Option::Some, value:T)",
                "$call(Option::None<T>)",
                &ValContext::new(None),
                &DummyTypeDefProvider::new(),
                &mut statics,
            )
            .unwrap();

        println!(
            "called functions {}",
            SliceDisplay(&functions.iter().map(|it| &it.1).collect::<Vec<_>>())
        );

        assert!(!functions.is_empty());
    }

    #[test]
    fn test_escape() {
        assert_eq!(CodeGenC::escape_string("a\"a"), "a\\\"a");
    }
}
