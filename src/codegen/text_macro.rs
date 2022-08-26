use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::parser::tokens_matcher::{Quantifier, TokensMatcher};
use regex::Regex;
use std::collections::HashMap;

pub enum MacroParam {
    Plain(String),
    StringLiteral(String),
}

pub struct TextMacro {
    name: String,
    parameters: Vec<MacroParam>,
}

pub fn parse(s: &str) -> TextMacro {
    todo!()
}

pub struct TextMacroEvaluator {
    evaluators: HashMap<String, Box<dyn TextMacroEval>>,
}

impl TextMacroEvaluator {
    pub fn new() -> Self {
        let mut evaluators: HashMap<String, Box<dyn TextMacroEval>> = HashMap::new();
        evaluators.insert("call".into(), Box::new(CallTextMacroEvaluator {}));
        Self { evaluators }
    }

    pub fn translate(&self, backend: &dyn Backend, statics: &mut Statics, s: &str) -> String {
        let re = Regex::new(r"\$(.*)\((.*)\)").unwrap();
        let matches = re.captures_iter(s);

        let mut result = s.to_string();

        for cap in matches {
            let whole = cap.get(0).unwrap().as_str();
            let name = cap.get(1).unwrap().as_str();
            let parameters = cap.get(2).unwrap().as_str();

            let text_macro = TextMacro {
                name: name.into(),
                parameters: Self::parse_params(parameters),
            };
            println!("found macro {whole}: {name}");

            let s = self.eval_macro(backend, statics, &text_macro);


            result = result.replace(whole, &s);
        }

        result
    }

    fn parse_params(s: &str) -> Vec<MacroParam> {
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
                        result.push(MacroParam::Plain(actual_param.trim().into()));
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
                result.push(MacroParam::Plain(actual_param.trim().into()));
            }
            State::StringLiteral => {
                result.push(MacroParam::StringLiteral(actual_param));
            }
        }

        result
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

struct CallTextMacroEvaluator {}

impl TextMacroEval for CallTextMacroEvaluator {
    fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &Vec<MacroParam>,
    ) -> String {
        let mut result = parameters
            .iter()
            .skip(1)
            .rev()
            .map(|it| match it {
                MacroParam::Plain(s) => {
                    format!("push dword {s}")
                }
                MacroParam::StringLiteral(s) => {
                    let key = statics.add_str(s);
                    format!("push dword {key}")
                }
            })
            .collect::<Vec<String>>()
            .join("\n");

        result.push('\n');

        if let Some(MacroParam::Plain(function_name)) = parameters.get(0) {
            result.push_str(&format!("call {function_name}\n"));
        } else {
            panic!("Error getting the function name");
        }

        result.push_str(&format!(
            "add {}, {}\n",
            backend.stack_pointer(),
            (parameters.len() - 1) * backend.word_len()
        ));

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::backend::BackendAsm386;
    use crate::codegen::statics::Statics;
    use crate::codegen::text_macro::{MacroParam, TextMacro, TextMacroEvaluator};
    use regex::Regex;
    use crate::codegen::MemoryValue;

    #[test]
    fn call() {
        let text_macro = TextMacro {
            name: "call".into(),
            parameters: vec![
                MacroParam::Plain("sprintln".into()),
                MacroParam::StringLiteral("Hello world".into()),
            ],
        };

        let backend = BackendAsm386::new();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new().eval_macro(&backend, &mut statics, &text_macro);

        assert_eq!(result, "push dword _s_0\ncall sprintln\nadd esp, 4\n");
    }

    #[test]
    fn parse() {
        let backend = BackendAsm386::new();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new().translate(&backend, &mut statics, "a line\n$call(nprint,10)\nanother line\n");

        assert_eq!(result, "a line\npush dword 10\ncall nprint\nadd esp, 4\n\nanother line\n");
    }

    #[test]
    fn parse_string_par() {
        let backend = BackendAsm386::new();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new().translate(&backend, &mut statics, "a line\n$call(sprintln, \"Hello, world\")\nanother line\n");

        assert_eq!(statics.get("_s_0"), Some(&MemoryValue::StringValue("Hello, world".into())));

        assert_eq!(result, "a line\npush dword _s_0\ncall sprintln\nadd esp, 4\n\nanother line\n");
    }
}
