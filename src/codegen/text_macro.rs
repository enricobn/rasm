use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::type_check::typed_ast::ASTTypedParameterDef;
use regex::Regex;
use std::collections::HashMap;

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
    evaluators: HashMap<String, Box<dyn TextMacroEval>>,
    parameters: Vec<ASTTypedParameterDef>,
}

impl TextMacroEvaluator {
    pub fn new(parameters: Vec<ASTTypedParameterDef>) -> Self {
        let mut evaluators: HashMap<String, Box<dyn TextMacroEval>> = HashMap::new();
        evaluators.insert("call".into(), Box::new(CallTextMacroEvaluator::new(false)));
        evaluators.insert("ccall".into(), Box::new(CallTextMacroEvaluator::new(true)));
        Self {
            parameters,
            evaluators,
        }
    }

    pub fn translate(&self, backend: &dyn Backend, statics: &mut Statics, s: &str) -> String {
        let re = Regex::new(r"\$([A-Za-z]*)\((.*)\)").unwrap();
        let matches = re.captures_iter(s);

        let mut result = s.to_string();

        for cap in matches {
            let whole = cap.get(0).unwrap().as_str();
            let name = cap.get(1).unwrap().as_str();
            let parameters = cap.get(2).unwrap().as_str();

            let text_macro = TextMacro {
                name: name.into(),
                parameters: self.parse_params(parameters),
            };

            let s = self.eval_macro(backend, statics, &text_macro);

            result = result.replace(whole, &s);
        }

        result
    }

    fn parse_params(&self, s: &str) -> Vec<MacroParam> {
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
                        let param = self.get_param(&mut actual_param);

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
                let param = self.get_param(&mut actual_param);
                result.push(param);
            }
            State::StringLiteral => {
                result.push(MacroParam::StringLiteral(actual_param));
            }
        }

        result
    }

    fn get_param(&self, actual_param: &String) -> MacroParam {
        let p = actual_param.trim();

        let is_ref = if let Some(par_name) = p.strip_prefix("$") {
            if let Some(par) = self.parameters.iter().find(|it| it.name == par_name) {
                CodeGen::get_reference_type_name(&par.type_ref.ast_type).is_some()
            } else {
                false
            }
        } else {
            false
        };

        let param = if is_ref {
            MacroParam::Ref(p.into())
        } else {
            MacroParam::Plain(p.into())
        };

        param
    }

    pub fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        text_macro: &TextMacro,
    ) -> String {
        let evaluator = self
            .evaluators
            .get(&text_macro.name)
            .unwrap_or_else(|| panic!("{} macro not found", &text_macro.name));
        evaluator.eval_macro(backend, statics, &text_macro.parameters)
    }
}

trait TextMacroEval {
    fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &Vec<MacroParam>,
    ) -> String;
}

struct CallTextMacroEvaluator {
    c: bool,
}

impl CallTextMacroEvaluator {
    pub fn new(c: bool) -> Self {
        Self { c }
    }
}

impl TextMacroEval for CallTextMacroEvaluator {
    fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &Vec<MacroParam>,
    ) -> String {
        let function_name = if let Some(MacroParam::Plain(function_name)) = parameters.get(0) {
            function_name
        } else {
            panic!("Error getting the function name");
        };

        let mut result = String::new();

        if self.c {
            result.push_str("push   ebx\n");
        }

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
                        if self.c {
                            format!("    mov dword ebx,[{key}]\npush    dword [ebx]\n")
                        } else {
                            format!("    push dword [{key}]")
                        }
                    }
                    MacroParam::Ref(s) => {
                        if self.c {
                            format!("    mov dword ebx,{s}\npush    dword [ebx]")
                        } else {
                            format!("    push dword {s}")
                        }
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

        if self.c {
            result.push_str("pop   ebx\n");
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::backend::BackendAsm386;
    use crate::codegen::statics::Statics;
    use crate::codegen::text_macro::{MacroParam, TextMacro, TextMacroEvaluator};
    use crate::codegen::MemoryValue;
    use crate::type_check::typed_ast::{
        ASTTypedParameterDef, ASTTypedType, ASTTypedTypeRef, BuiltinTypedTypeKind,
    };
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
            TextMacroEvaluator::new(vec![]).eval_macro(&backend, &mut statics, &text_macro);

        assert_eq!(
            result,
            "    push dword [_s_1]\n    call sprintln\n    add esp, 4\n"
        );
    }

    #[test]
    fn parse() {
        let backend = backend();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new(vec![]).translate(
            &backend,
            &mut statics,
            "a line\n$call(nprint,10)\nanother line\n",
        );

        assert_eq!(
            result,
            "a line\n    push dword 10\n    call nprint\n    add esp, 4\n\nanother line\n"
        );
    }

    #[test]
    fn parse_string_par() {
        let backend = backend();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new(vec![]).translate(
            &backend,
            &mut statics,
            "a line\n$call(sprintln, \"Hello, world\")\nanother line\n",
        );

        assert_eq!(
            statics.get("_sv_0"),
            Some(&MemoryValue::StringValue("Hello, world".into()))
        );

        assert_eq!(
            result,
            "a line\n    push dword [_s_1]\n    call sprintln\n    add esp, 4\n\nanother line\n"
        );
    }

    #[test]
    fn parse_ref_par() {
        let backend = backend();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new(vec![ASTTypedParameterDef {
            name: "s".into(),
            type_ref: ASTTypedTypeRef {
                ast_ref: false,
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::ASTString),
            },
        }])
            .translate(
                &backend,
                &mut statics,
                "a line\n$ccall(sprintln, $s)\nanother line\n",
            );

        assert_eq!(
            result,
            "a line\npush   ebx\n    mov dword ebx,$s\npush    dword [ebx]\n    call sprintln\n    add esp, 4\npop   ebx\n\nanother line\n"
        );
    }

    fn backend() -> BackendAsm386 {
        BackendAsm386::new(HashSet::new(), HashSet::new())
    }
}
