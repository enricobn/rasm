use rasm_utils::debug_i;

use crate::{
    codegen::{
        statics::Statics,
        text_macro::{MacroParam, TextMacro, TextMacroEval},
        type_def_body::TypeDefBodyTarget,
        typedef_provider::TypeDefProvider,
        CodeGen,
    },
    enh_type_check::typed_ast::{ASTTypedFunctionDef, DefaultFunctionCall},
};

use super::code_gen_asm::CodeGenAsm;

pub struct AsmCallTextMacroEvaluator {
    code_gen: CodeGenAsm,
}

impl AsmCallTextMacroEvaluator {
    pub fn new(code_gen: CodeGenAsm) -> Self {
        Self { code_gen }
    }
}

impl TextMacroEval for AsmCallTextMacroEvaluator {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        _function_def: Option<&ASTTypedFunctionDef>,
        _type_def_provider: &dyn TypeDefProvider,
    ) -> Result<String, String> {
        let function_name =
            if let Some(MacroParam::Plain(function_name, _, _)) = text_macro.parameters.get(0) {
                function_name
            } else {
                panic!("Error getting the function name");
            };

        let mut result = String::new();

        result.push_str(&format!("; call macro, calling {function_name}\n"));

        result.push_str(
            &text_macro
                .parameters
                .iter()
                .skip(1)
                .rev()
                .map(|it| match it {
                    MacroParam::Plain(s, _, _) => {
                        format!("    push dword {s}")
                    }
                    MacroParam::StringLiteral(s) => {
                        let key = statics.add_str(s);
                        format!("    push dword [{key}]")
                    }
                    MacroParam::Ref(s, _, _) => {
                        format!("    push dword {s}")
                    }
                })
                .collect::<Vec<String>>()
                .join("\n"),
        );

        result.push('\n');

        result.push_str(&format!("    call {function_name}\n"));

        result.push_str(&format!(
            "    add {}, {}\n",
            self.code_gen.stack_pointer(),
            (text_macro.parameters.len() - 1) * self.code_gen.word_len()
        ));

        Ok(result)
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct AsmCCallTextMacroEvaluator {
    code_gen: CodeGenAsm,
}

impl AsmCCallTextMacroEvaluator {
    pub fn new(code_gen: CodeGenAsm) -> Self {
        Self { code_gen }
    }
}

impl TextMacroEval for AsmCCallTextMacroEvaluator {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        _function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Result<String, String> {
        debug_i!("translate macro fun {:?}", _function_def);
        let function_name =
            if let Some(MacroParam::Plain(function_name, _, _)) = text_macro.parameters.get(0) {
                function_name
            } else {
                panic!("Error getting the function name");
            };

        let ws = self.code_gen.word_size();
        let sp = self.code_gen.stack_pointer();
        let wl = self.code_gen.word_len();

        let mut result = String::new();

        result.push_str(&format!("; ccall macro, calling {function_name}\n"));
        result.push_str("    push   edx\n");
        result.push_str("    push   ebx\n");
        result.push_str("    push   ecx\n");
        result.push_str(&format!("    mov   {ws} ebx, esp\n"));
        result.push_str(&format!(
            "    sub   {sp}, {}\n",
            wl * (text_macro.parameters.len() - 1)
        ));
        // stack 16 bytes alignment
        result.push_str("    and   esp,0xfffffff0\n");

        let tmp_register = "eax";
        result.push_str(
            &text_macro.parameters
                .iter().enumerate()
                .skip(1)
                .rev()
                .map(|(index, it)| {
                    let i = index - 1;
                    match it {
                        MacroParam::Plain(s, _, _) => {
                            format!("    mov {ws} {tmp_register}, {s}\n    mov {ws} [{sp}+{}], {tmp_register}\n", i * wl)
                        }
                        MacroParam::StringLiteral(s) => {
                            let key = statics.add_str(s);
                            format!("    mov {ws} {tmp_register}, [{key}]\n    mov {ws} {tmp_register},[{tmp_register}]\n    mov {ws} [{sp}+{}], {tmp_register}\n", i * wl)
                        }
                        MacroParam::Ref(s, t, _) => {
                            let is_ref =
                            if let Some(tt) = t {
                                tt.is_reference(type_def_provider, TypeDefBodyTarget::Asm)
                            } else {
                                false
                            };

                            if is_ref {
                                format!("    mov {ws} {tmp_register}, {s}\n    mov {ws} {tmp_register}, [{tmp_register}]\n   mov {ws} [{sp}+{}], {tmp_register}\n", i * wl)
                            } else {
                                format!("    mov {ws} {tmp_register}, {s}\n    mov {ws} [{sp}+{}], {tmp_register}\n", i * wl)
                            }
                        }
                    }
                })
                .collect::<Vec<String>>()
                .join("\n"),
        );

        result.push('\n');

        result.push_str(&format!("    call {function_name}\n"));

        result.push_str("    mov   esp,ebx\n");
        result.push_str("    pop   ecx\n");
        result.push_str("    pop   ebx\n");
        result.push_str("    pop   edx\n");

        Ok(result)
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}
