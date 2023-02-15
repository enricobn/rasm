use linked_hash_map::LinkedHashMap;
use log::debug;
use regex::Regex;

use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::debug_i;
use crate::lexer::tokens::Token;
use crate::lexer::Lexer;
use crate::parser::ast::{ASTType, BuiltinTypeKind};
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserTrait;
use crate::type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedModule, ASTTypedType, BuiltinTypedTypeKind,
};

#[derive(Debug)]
pub enum MacroParam {
    Plain(String, Option<ASTType>),
    StringLiteral(String),
    Ref(String, Option<ASTType>),
}

#[derive(Debug)]
pub struct TextMacro {
    pub name: String,
    pub parameters: Vec<MacroParam>,
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
        module: &ASTTypedModule,
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

        if let Some(name) = p.strip_prefix('$') {
            match function_def {
                None => panic!("Cannot resolve reference without a function {p}"),
                Some(f) => {
                    let par_type = function_def
                        .and_then(|it| it.parameters.iter().find(|par| par.name == name))
                        .map(|it| it.ast_type.clone());
                    let (par_name, par_type) = Self::parse_typed_argument(name, par_type);
                    if !f.parameters.iter().any(|it| it.name == par_name) {
                        panic!("Cannot find parameter {par_name}");
                    } else {
                        MacroParam::Ref(format!("${par_name}"), par_type)
                    }
                }
            }
        } else {
            let (par_name, par_type) = Self::parse_typed_argument(p, None);

            MacroParam::Plain(par_name.into(), par_type)
        }

        /*
        let is_ref = if let Some(par_def) = p.strip_prefix('$') {
            let (par_name, par_type) = if par_def.contains(":") {
                let vec = par_def.split(":").collect::<Vec<_>>();
                let par_type_name = vec.get(1).unwrap().trim();

                if par_type_name == "i32" {
                    (vec.get(0).unwrap().trim(), Some(ASTType::Builtin(BuiltinTypeKind::I32)))
                } else if par_type_name == "str" {
                    (vec.get(0).unwrap().trim(), Some(ASTType::Builtin(BuiltinTypeKind::String)))
                } else {
                    panic!("Unsupported type {par_type_name}");
                }
            } else {
                (par_def, None)
            };
            if let Some(par) = function_def
                .map(|it| it.parameters.clone())
                .unwrap_or_default()
                .iter()
                .find(|it| it.name == *par_name)
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
            MacroParam::Plain(p.into(), par)
        }

         */
    }

    fn parse_typed_argument(p: &str, typed_type: Option<ASTTypedType>) -> (&str, Option<ASTType>) {
        // TODO the check of :: is a trick since function names could have ::, try to do it better
        let (par_name, par_type) = if p.contains(':') && !p.contains("::") {
            let vec = p.split(':').collect::<Vec<_>>();
            let par_type_name = vec.get(1).unwrap().trim();
            let par_name = vec.first().unwrap().trim();

            // println!("found param type {par_type_name}");

            if par_type_name == "i32" {
                (par_name, Some(ASTType::Builtin(BuiltinTypeKind::I32)))
            } else if par_type_name == "str" {
                (par_name, Some(ASTType::Builtin(BuiltinTypeKind::String)))
            } else {
                let parser = TypeParserHelper::new(par_type_name);
                // Parser::new(lexer, None);

                let type_parser = TypeParser::new(&parser);

                match type_parser.try_parse_ast_type(0, &[]) {
                    None => panic!("Unsupported type {par_type_name}"),
                    Some((ast_type, _)) => (par_name, Some(ast_type)),
                }
            }
        } else if let Some(t) = typed_type {
            let ast_type = match t {
                ASTTypedType::Builtin(kind) => match kind {
                    BuiltinTypedTypeKind::String => ASTType::Builtin(BuiltinTypeKind::String),
                    BuiltinTypedTypeKind::I32 => ASTType::Builtin(BuiltinTypeKind::I32),
                    BuiltinTypedTypeKind::Bool => ASTType::Builtin(BuiltinTypeKind::Bool),
                    BuiltinTypedTypeKind::Char => ASTType::Builtin(BuiltinTypeKind::Char),
                    BuiltinTypedTypeKind::Lambda { .. } => {
                        ASTType::Builtin(BuiltinTypeKind::Lambda {
                            parameters: vec![],
                            return_type: None,
                        })
                    }
                },
                ASTTypedType::Enum { name } => ASTType::Custom {
                    name,
                    param_types: vec![],
                },
                ASTTypedType::Struct { name } => ASTType::Custom {
                    name,
                    param_types: vec![],
                },
                ASTTypedType::Type { name } => ASTType::Custom {
                    name,
                    param_types: vec![],
                },
            };
            (p, Some(ast_type))
        } else {
            (p, None)
        };
        (par_name, par_type)
    }

    pub fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        module: &ASTTypedModule,
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

    pub fn get_macros(
        &self,
        backend: &dyn Backend,
        function_def: Option<&ASTTypedFunctionDef>,
        body: &str,
    ) -> Vec<TextMacro> {
        let re = Regex::new(r"\$([A-Za-z]*)\((.*)\)").unwrap();

        let mut result = Vec::new();

        let lines = body.lines();

        for s in lines {
            let stripped_comments = backend.remove_comments_from_line(s.to_string());
            let matches = re.captures_iter(&stripped_comments);

            for cap in matches {
                let name = cap.get(1).unwrap().as_str();
                let parameters = cap.get(2).unwrap().as_str();

                let text_macro = TextMacro {
                    name: name.into(),
                    parameters: self.parse_params(parameters, function_def),
                };

                result.push(text_macro)
            }
        }

        result
    }
}

struct TypeParserHelper {
    type_tokens: Vec<Token>,
}

impl TypeParserHelper {
    fn new(type_str: &str) -> Self {
        let lexer = Lexer::new(type_str.into(), "".into());
        Self {
            type_tokens: lexer.collect(),
        }
    }
}

impl ParserTrait for TypeParserHelper {
    fn get_i(&self) -> usize {
        0
    }

    fn get_token_n(&self, n: usize) -> Option<&Token> {
        self.type_tokens.get(n)
    }

    fn panic(&self, message: &str) {
        panic!("{message}")
    }
}

trait TextMacroEval {
    fn eval_macro(
        &self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        function_def: Option<&ASTTypedFunctionDef>,
        module: &ASTTypedModule,
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
        _module: &ASTTypedModule,
    ) -> String {
        let function_name = if let Some(MacroParam::Plain(function_name, _)) = parameters.get(0) {
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
                    MacroParam::Plain(s, _) => {
                        format!("    push dword {s}")
                    }
                    MacroParam::StringLiteral(s) => {
                        let key = statics.add_str(s);
                        format!("    push dword [{key}]")
                    }
                    MacroParam::Ref(s, _) => {
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
        module: &ASTTypedModule,
    ) -> String {
        debug_i!("translate macro fun {:?}", _function_def);
        let function_name = if let Some(MacroParam::Plain(function_name, _)) = parameters.get(0) {
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
                        MacroParam::Plain(s, _) => {
                            format!("    mov {ws} ecx, {s}\n    mov {ws} [{sp}+{}], ecx\n", i * wl)
                        }
                        MacroParam::StringLiteral(s) => {
                            let key = statics.add_str(s);
                            format!("    mov {ws} ecx, [{key}]\n    mov {ws} ecx,[ecx]\n    mov {ws} [{sp}+{}], ecx\n", i * wl)
                        }
                        MacroParam::Ref(s, t) => {
                            let is_ref =
                            if let Some(tt) = t {
                                is_reference(tt, module)
                            } else {
                                false
                            };

                            if is_ref {
                                format!("    mov {ws} ecx, {s}\n    mov {ws} ecx, [ecx]\n   mov {ws} [{sp}+{}], ecx\n", i * wl)
                            } else {
                                format!("    mov {ws} ecx, {s}\n    mov {ws} [{sp}+{}], ecx\n", i * wl)
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

        result
    }
}

fn is_reference(ast_type: &ASTType, module: &ASTTypedModule) -> bool {
    if let ASTType::Builtin(BuiltinTypeKind::String) = ast_type {
        true
    } else if let ASTType::Custom {
        name,
        param_types: _,
    } = ast_type
    {
        if let Some(t) = module.types.iter().find(|it| &it.name == name) {
            t.is_ref
        } else {
            true
        }
    } else {
        false
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
        module: &ASTTypedModule,
    ) -> String {
        if let Some(fd) = function_def {
            if fd.name == "addRef_0" {
                return String::new();
            }
            if let Some(MacroParam::Plain(generic_type_name, _)) = parameters.get(0) {
                if let Some(MacroParam::Plain(address, _)) = parameters.get(1) {
                    if let Some(ast_type) = fd.generic_types.get(generic_type_name) {
                        if let Some(type_name) = CodeGen::get_reference_type_name(ast_type, module)
                        {
                            let mut result = String::new();
                            let descr = &format!("addref macro type {type_name}");
                            if self.deref {
                                result.push_str(
                                    &backend
                                        .call_deref(address, &type_name, descr, module, statics),
                                );
                            } else {
                                backend.call_add_ref(
                                    &mut result,
                                    address,
                                    &type_name,
                                    descr,
                                    module,
                                    statics,
                                );
                            }
                            result
                        } else {
                            String::new()
                        }
                    } else {
                        let mut name = generic_type_name.clone();
                        name.push('_');
                        let descr = &format!("addref macro type {name}");
                        if let Some(s) = module.structs.iter().find(|it| it.name.starts_with(&name))
                        {
                            let mut result = String::new();
                            if self.deref {
                                result.push_str(
                                    &backend.call_deref(address, &s.name, descr, module, statics),
                                );
                            } else {
                                backend.call_add_ref(
                                    &mut result,
                                    address,
                                    &s.name,
                                    descr,
                                    module,
                                    statics,
                                );
                            }
                            result
                        } else if let Some(s) =
                            module.enums.iter().find(|it| it.name.starts_with(&name))
                        {
                            let mut result = String::new();
                            if self.deref {
                                result.push_str(
                                    &backend.call_deref(address, &s.name, descr, module, statics),
                                );
                            } else {
                                backend.call_add_ref(
                                    &mut result,
                                    address,
                                    &s.name,
                                    descr,
                                    module,
                                    statics,
                                );
                            }
                            result
                        } else {
                            panic!("cannot find struct nor enum {generic_type_name}");
                        }
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
    use std::collections::HashSet;

    use linked_hash_map::LinkedHashMap;

    use crate::codegen::backend::BackendAsm386;
    use crate::codegen::statics::Statics;
    use crate::codegen::text_macro::{MacroParam, TextMacro, TextMacroEvaluator, TypeParserHelper};
    use crate::codegen::MemoryValue;
    use crate::parser::ast::{ASTType, BuiltinTypeKind};
    use crate::parser::type_parser::TypeParser;
    use crate::type_check::typed_ast::{
        ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef,
        ASTTypedType, BuiltinTypedTypeKind,
    };

    #[test]
    fn call() {
        let text_macro = TextMacro {
            name: "call".into(),
            parameters: vec![
                MacroParam::Plain("println".into(), None),
                MacroParam::StringLiteral("Hello world".into()),
            ],
        };

        let backend = backend();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new().eval_macro(
            &backend,
            &mut statics,
            &text_macro,
            None,
            &module(),
        );

        assert_eq!(
            result,
            "; call macro, calling println\n    push dword [_s_1]\n    call println\n    add esp, 4\n"
        );
    }

    #[test]
    fn translate() {
        let backend = backend();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new().translate(
            &backend,
            &mut statics,
            None,
            "a line\n$call(nprint,10)\nanother line\n",
            &module(),
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
            "a line\n$call(println, \"Hello, world\")\nanother line\n",
            &module(),
        );

        assert_eq!(
            statics.get("_sv_0"),
            Some(&MemoryValue::StringValue("Hello, world".into()))
        );

        assert_eq!(
            result,
            "a line\n; call macro, calling println\n    push dword [_s_1]\n    call println\n    add esp, 4\n\nanother line\n"
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
            "a line\n$call(println, $s)\nanother line\n",
            &module(),
        );

        assert_eq!(
            result,
            "a line\n; call macro, calling println\n    push dword $s\n    call println\n    add esp, 4\n\nanother line\n"
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
            &module(),
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
            &module(),
        );

        assert_eq!(result, "mov     eax, 1          ; $call(any)");
    }

    #[test]
    fn test_get_macros() {
        let backend = backend();

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

        let macros =
            TextMacroEvaluator::new().get_macros(&backend, Some(&function_def), "$call(slen, $s)");

        let param = macros.get(0).unwrap().parameters.get(1).unwrap();
        match param {
            MacroParam::Plain(_, _) => panic!("plain"),
            MacroParam::StringLiteral(_) => panic!("string literal"),
            MacroParam::Ref(r, _) => assert_eq!(r, "$s"),
        }
    }

    #[test]
    #[ignore]
    fn translate_typed() {
        let backend = backend();
        let mut statics = Statics::new();

        let result = TextMacroEvaluator::new().translate(
            &backend,
            &mut statics,
            None,
            "$call(List_0_addRef,eax:List_0)",
            &module(),
        );

        assert_eq!(
            result,
            "; call macro, calling List_0_addRef\n    push dword eax\n    call List_0_addRef\n    add esp, 4\n"
        );
    }

    #[test]
    fn parse_list_str() {
        let parser = TypeParserHelper::new("List<str>");
        let type_parser = TypeParser::new(&parser);

        match type_parser.try_parse_ast_type(0, &[]) {
            None => panic!("Unsupported type"),
            Some((ast_type, _)) => assert_eq!(
                ast_type,
                ASTType::Custom {
                    name: "List".into(),
                    param_types: vec![ASTType::Builtin(BuiltinTypeKind::String)]
                }
            ),
        }
    }

    fn backend() -> BackendAsm386 {
        BackendAsm386::new(HashSet::new(), HashSet::new())
    }

    fn module() -> ASTTypedModule {
        ASTTypedModule {
            body: vec![],
            functions_by_name: Default::default(),
            enums: vec![],
            structs: vec![],
            native_body: "".to_string(),
            types: vec![],
        }
    }
}
