use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::type_check::typed_ast::{ASTTypedFunctionDef, ASTTypedModule};
use linked_hash_map::LinkedHashMap;
use regex::Regex;

pub enum MacroParam {
    Plain(String),
    StringLiteral(String),
    Ref(String),
}

pub struct TextMacro {
    name: String,
    parameters: Vec<MacroParam>,
}

pub struct TextMacroEvaluator {
    evaluators: LinkedHashMap<String, Box<dyn TextMacroEval>>,
}

impl TextMacroEvaluator {
    pub fn new() -> Self {
        let mut evaluators: LinkedHashMap<String, Box<dyn TextMacroEval>> = LinkedHashMap::new();
        evaluators.insert("call".into(), Box::new(CallTextMacroEvaluator::new()));
        evaluators.insert("ccall".into(), Box::new(CCallTextMacroEvaluator::new()));
        evaluators.insert("addRef".into(), Box::new(AddRefMacro::new(false)));
        evaluators.insert("deref".into(), Box::new(AddRefMacro::new(true)));
        Self { evaluators }
    }

    pub fn translate(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        function_def: Option<&ASTTypedFunctionDef>,
        body: &str,
        module: Option<&ASTTypedModule>,
    ) -> String {
        let re = Regex::new(r"\$([A-Za-z]*)\((.*)\)").unwrap();

        let mut result = Vec::new();

        let lines = body.lines();

        for s in lines {
            let stripped_comments = backend.remove_comments_from_line(s.to_string());
            let matches = re.captures_iter(&stripped_comments);

            let mut line_result = s.to_string();

            for cap in matches {
                let whole = cap.get(0).unwrap().as_str();
                let name = cap.get(1).unwrap().as_str();
                let parameters = cap.get(2).unwrap().as_str();

                let text_macro = TextMacro {
                    name: name.into(),
                    parameters: self.parse_params(parameters, function_def),
                };

                let s = self.eval_macro(backend, statics, &text_macro, function_def, module);

                line_result = line_result.replace(whole, &s);
            }

            result.push(line_result);
        }

        let mut new_body = result.join("\n");

        if body.ends_with('\n') {
            new_body.push('\n');
        }

        new_body
    }

    fn parse_params(&self, s: &str, function_def: Option<&ASTTypedFunctionDef>) -> Vec<MacroParam> {
        let mut result = Vec::new();

        enum State {
            None,
            Standard,
            StringLiteral,
        }

        let mut state = State::None;
        let mut actual_param = String::new();

        for c in s.to_string().chars() {
            match state {
                State::Standard => {
                    if c == ',' {
                        let param = self.get_param(&actual_param, function_def);

                        result.push(param);

                        actual_param = String::new();
                        state = State::None;
                    } else {
                        actual_param.push(c);
                    }
                }
                State::None => {
                    if !c.is_whitespace() {
                        if c == '"' {
                            state = State::StringLiteral
                        } else if c != ',' {
                            actual_param.push(c);
                            state = State::Standard
                        }
                    }
                }
                State::StringLiteral => {
                    if c == '"' {
                        result.push(MacroParam::StringLiteral(actual_param));
                        actual_param = String::new();
                        state = State::None;
                    } else {
                        actual_param.push(c);
                    }
                }
            }
        }

        match state {
            State::None => {}
            State::Standard => {
                let param = self.get_param(&actual_param, function_def);
                result.push(param);
            }
            State::StringLiteral => {
                result.push(MacroParam::StringLiteral(actual_param));
            }
        }

        result
    }

    fn get_param(
        &self,
        actual_param: &str,
        function_def: Option<&ASTTypedFunctionDef>,
    ) -> MacroParam {
        let p = actual_param.trim();

        let is_ref = if let Some(par_name) = p.strip_prefix('$') {
            if let Some(par) = function_def
                .map(|it| it.parameters.clone())
                .unwrap_or_default()
                .iter()
                .find(|it| it.name == par_name)
            {
                CodeGen::get_reference_type_name(&par.ast_type).is_some()
            } else {
                false
            }
        } else {
            false
        };

        if is_ref {
            MacroParam::Ref(p.into())
        } else {
            MacroParam::Plain(p.into())
        }
    }

    pub fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        module: Option<&ASTTypedModule>,
    ) -> String {
        let evaluator = self
            .evaluators
            .get(&text_macro.name)
            .unwrap_or_else(|| panic!("{} macro not found", &text_macro.name));
        evaluator.eval_macro(
            backend,
            statics,
            &text_macro.parameters,
            function_def,
            module,
        )
    }
}

trait TextMacroEval {
    fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        function_def: Option<&ASTTypedFunctionDef>,
        module: Option<&ASTTypedModule>,
    ) -> String;
}

struct CallTextMacroEvaluator {}

impl CallTextMacroEvaluator {
    pub fn new() -> Self {
        Self {}
    }
}

impl TextMacroEval for CallTextMacroEvaluator {
    fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        _function_def: Option<&ASTTypedFunctionDef>,
        _module: Option<&ASTTypedModule>,
    ) -> String {
        let function_name = if let Some(MacroParam::Plain(function_name)) = parameters.get(0) {
            function_name
        } else {
            panic!("Error getting the function name");
        };

        let mut result = String::new();

        result.push_str(&format!("; call macro, calling {function_name}\n"));

        result.push_str(
            &parameters
                .iter()
                .skip(1)
                .rev()
                .map(|it| match it {
                    MacroParam::Plain(s) => {
                        format!("    push dword {s}")
                    }
                    MacroParam::StringLiteral(s) => {
                        let key = statics.add_str(s);
                        format!("    push dword [{key}]")
                    }
                    MacroParam::Ref(s) => {
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
            backend.stack_pointer(),
            (parameters.len() - 1) * backend.word_len()
        ));

        result
    }
}

struct CCallTextMacroEvaluator {}

impl CCallTextMacroEvaluator {
    pub fn new() -> Self {
        Self {}
    }
}

impl TextMacroEval for CCallTextMacroEvaluator {
    fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        _function_def: Option<&ASTTypedFunctionDef>,
        _module: Option<&ASTTypedModule>,
    ) -> String {
        let function_name = if let Some(MacroParam::Plain(function_name)) = parameters.get(0) {
            function_name
        } else {
            panic!("Error getting the function name");
        };

        let ws = backend.word_size();
        let sp = backend.stack_pointer();
        let wl = backend.word_len();

        let mut result = String::new();

        result.push_str(&format!("; ccall macro, calling {function_name}\n"));
        result.push_str("    push   edx\n");
        result.push_str("    push   ebx\n");
        result.push_str("    push   ecx\n");
        result.push_str(&format!("    mov   {ws} ebx, esp\n"));
        result.push_str(&format!(
            "    sub   {sp}, {}\n",
            wl * (parameters.len() - 1)
        ));
        // stack 16 bytes alignment
        result.push_str("    and   esp,0xfffffff0\n");

        result.push_str(
            &parameters
                .iter().enumerate()
                .skip(1)
                .rev()
                .map(|(index, it)| {
                    let i = index - 1;
                    match it {
                        MacroParam::Plain(s) => {
                            format!("    mov {ws} ecx, {s}\n    mov {ws} [{sp}+{}], ecx\n", i * wl)
                        }
                        MacroParam::StringLiteral(s) => {
                            let key = statics.add_str(s);
                            format!("    mov {ws} ecx, [{key}]\n    mov {ws} ecx,[ecx]\n    mov {ws} [{sp}+{}], ecx\n", i * wl)
                        }
                        MacroParam::Ref(s) => {
                            format!("    mov {ws} ecx, {s}\n    mov {ws} ecx, [ecx]\n   mov {ws} [{sp}+{}], ecx\n", i * wl)
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

        result
    }
}

struct AddRefMacro {
    deref: bool,
}

impl AddRefMacro {
    fn new(deref: bool) -> Self {
        Self { deref }
    }
}

impl TextMacroEval for AddRefMacro {
    fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        function_def: Option<&ASTTypedFunctionDef>,
        module: Option<&ASTTypedModule>,
    ) -> String {
        if let Some(fd) = function_def {
            if let Some(MacroParam::Plain(generic_type_name)) = parameters.get(0) {
                if let Some(MacroParam::Plain(address)) = parameters.get(1) {
                    if let Some(ast_type) = fd.generic_types.get(generic_type_name) {
                        if let Some(type_name) = CodeGen::get_reference_type_name(ast_type) {
                            if let Some(ast_module) = module {
                                let mut result = String::new();
                                if self.deref {
                                    result.push_str(
                                        &backend.call_deref(
                                            address, &type_name, "", ast_module, statics,
                                        ),
                                    );
                                } else {
                                    backend.call_add_ref(
                                        &mut result,
                                        address,
                                        &type_name,
                                        "",
                                        ast_module,
                                        statics,
                                    );
                                }
                                result
                            } else {
                                String::new()
                            }
                        } else {
                            String::new()
                        }
                    } else {
                        panic!("Cannot find generic type {generic_type_name}")
                    }
                } else {
                    panic!("Error getting the address")
                }
            } else {
                panic!("Error getting the generic type name")
            }
        } else {
            String::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::backend::BackendAsm386;
    use crate::codegen::statics::Statics;
    use crate::codegen::text_macro::{MacroParam, TextMacro, TextMacroEvaluator};
    use crate::codegen::MemoryValue;
    use crate::type_check::typed_ast::{
        ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedParameterDef, ASTTypedType,
        BuiltinTypedTypeKind,
    };
    use linked_hash_map::LinkedHashMap;
    use std::collections::HashSet;

    #[test]
    fn call() {
        let text_macro = TextMacro {
            name: "call".into(),
            parameters: vec![
                MacroParam::Plain("sprintln".into()),
                MacroParam::StringLiteral("Hello world".into()),
            ],
        };

        let backend = backend();
        let mut statics = Statics::new();

        let result =
            TextMacroEvaluator::new().eval_macro(&backend, &mut statics, &text_macro, None, None);

        assert_eq!(
            result,
            "; call macro, calling sprintln\n    push dword [_s_1]\n    call sprintln\n    add esp, 4\n"
        );
    }

    #[test]
    fn parse() {
        let backend = backend();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new().translate(
            &backend,
            &mut statics,
            None,
            "a line\n$call(nprint,10)\nanother line\n",
            None,
        );

        assert_eq!(
            result,
            "a line\n; call macro, calling nprint\n    push dword 10\n    call nprint\n    add esp, 4\n\nanother line\n"
        );
    }

    #[test]
    fn parse_string_par() {
        let backend = backend();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new().translate(
            &backend,
            &mut statics,
            None,
            "a line\n$call(sprintln, \"Hello, world\")\nanother line\n",
            None,
        );

        assert_eq!(
            statics.get("_sv_0"),
            Some(&MemoryValue::StringValue("Hello, world".into()))
        );

        assert_eq!(
            result,
            "a line\n; call macro, calling sprintln\n    push dword [_s_1]\n    call sprintln\n    add esp, 4\n\nanother line\n"
        );
    }

    #[test]
    fn parse_ref_par() {
        let backend = backend();
        let mut statics = Statics::new();

        let function_def = ASTTypedFunctionDef {
            name: "aFun".into(),
            parameters: vec![ASTTypedParameterDef {
                name: "s".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
            }],
            body: ASTTypedFunctionBody::ASMBody("".into()),
            generic_types: LinkedHashMap::new(),
            return_type: None,
            inline: false,
        };

        let result = TextMacroEvaluator::new().translate(
            &backend,
            &mut statics,
            Some(&function_def),
            "a line\n$call(sprintln, $s)\nanother line\n",
            None,
        );

        assert_eq!(
            result,
            "a line\n; call macro, calling sprintln\n    push dword $s\n    call sprintln\n    add esp, 4\n\nanother line\n"
        );
    }

    #[test]
    fn parse_ref_par_c() {
        let backend = backend();
        let mut statics = Statics::new();

        let function_def = ASTTypedFunctionDef {
            name: "aFun".into(),
            parameters: vec![ASTTypedParameterDef {
                name: "s".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
            }],
            body: ASTTypedFunctionBody::ASMBody("".into()),
            generic_types: LinkedHashMap::new(),
            return_type: None,
            inline: false,
        };

        let result = TextMacroEvaluator::new().translate(
            &backend,
            &mut statics,
            Some(&function_def),
            "a line\n$ccall(printf, $s)\nanother line\n",
            None,
        );

        assert_eq!(
            result,
            "a line\n; ccall macro, calling printf\n    push   edx\n    push   ebx\n    push   ecx\n    mov   dword ebx, esp\n    sub   esp, 4\n    and   esp,0xfffffff0\n    mov dword ecx, $s\n    mov dword ecx, [ecx]\n   mov dword [esp+0], ecx\n\n    call printf\n    mov   esp,ebx\n    pop   ecx\n    pop   ebx\n    pop   edx\n\nanother line\n"
        );
    }

    #[test]
    fn test() {
        let backend = backend();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new().translate(
            &backend,
            &mut statics,
            None,
            "mov     eax, 1          ; $call(any)",
            None,
        );

        assert_eq!(result, "mov     eax, 1          ; $call(any)");
    }

    fn backend() -> BackendAsm386 {
        BackendAsm386::new(HashSet::new(), HashSet::new())
    }
}
