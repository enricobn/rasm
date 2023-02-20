use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};

use linked_hash_map::LinkedHashMap;
use log::debug;
use regex::Regex;

use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::debug_i;
use crate::lexer::tokens::Token;
use crate::lexer::Lexer;
use crate::parser::ast::{ASTIndex, ASTType, BuiltinTypeKind};
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserTrait;
use crate::type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionDef, ASTTypedStructDef, ASTTypedType, ASTTypedTypeDef,
    BuiltinTypedTypeKind,
};

thread_local! {
    static COUNT : RefCell<usize> = RefCell::new(0);
}

pub trait TypeDefProvider {
    fn get_enum_def_by_name(&self, name: &str) -> Option<&ASTTypedEnumDef>;
    fn get_struct_def_by_name(&self, name: &str) -> Option<&ASTTypedStructDef>;
    fn get_type_def_by_name(&self, name: &str) -> Option<&ASTTypedTypeDef>;
    fn get_enum_def_like_name(&self, name: &str) -> Option<&ASTTypedEnumDef>;
    fn get_struct_def_like_name(&self, name: &str) -> Option<&ASTTypedStructDef>;
    fn get_type_def_like_name(&self, name: &str) -> Option<&ASTTypedTypeDef>;
}

#[derive(Debug)]
pub enum MacroParam {
    Plain(String, Option<ASTType>),
    StringLiteral(String),
    Ref(String, Option<ASTType>),
}

impl Display for MacroParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MacroParam::Plain(name, type_o) => {
                f.write_str(name)?;
                if let Some(t) = type_o {
                    f.write_str(&format!(": {t}"))?;
                }
            }
            MacroParam::StringLiteral(s) => {
                f.write_str(&format!("\"{s}\""))?;
            }
            MacroParam::Ref(name, type_o) => {
                f.write_str(name)?;
                if let Some(t) = type_o {
                    f.write_str(&format!(": {t}"))?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct TextMacro {
    pub name: String,
    pub parameters: Vec<MacroParam>,
}

impl Display for TextMacro {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("${}(", self.name))?;
        let parameters = self
            .parameters
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<String>>();
        f.write_str(&parameters.join(", "))?;
        f.write_str(")")
    }
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
        evaluators.insert("printRef".into(), Box::new(PrintRefMacro::new()));
        Self { evaluators }
    }

    pub fn translate(
        &mut self,
        backend: &dyn Backend,
        statics: &mut Statics,
        function_def: Option<&ASTTypedFunctionDef>,
        body: &str,
        dereference: bool,
        pre_macro: bool,
        type_def_provider: &dyn TypeDefProvider,
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

                if let Some(s) = self.eval_macro(
                    backend,
                    statics,
                    &text_macro,
                    function_def,
                    dereference,
                    pre_macro,
                    type_def_provider,
                ) {
                    line_result = line_result.replace(whole, &s);
                }
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
        &mut self,
        backend: &dyn Backend,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        dereference: bool,
        pre_macro: bool,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Option<String> {
        let evaluator = self
            .evaluators
            .get_mut(&text_macro.name)
            .unwrap_or_else(|| panic!("{} macro not found", &text_macro.name));

        if evaluator.is_pre_macro() == pre_macro {
            Some(evaluator.eval_macro(
                backend,
                statics,
                &text_macro.parameters,
                function_def,
                dereference,
                type_def_provider,
            ))
        } else {
            None
        }
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

    fn get_index(&self, n: usize) -> Option<ASTIndex> {
        // TODO
        Some(ASTIndex::none())
    }
}

trait TextMacroEval {
    fn eval_macro(
        &mut self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        function_def: Option<&ASTTypedFunctionDef>,
        dereference: bool,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String;

    fn is_pre_macro(&self) -> bool;
}

struct CallTextMacroEvaluator {}

impl CallTextMacroEvaluator {
    pub fn new() -> Self {
        Self {}
    }
}

impl TextMacroEval for CallTextMacroEvaluator {
    fn eval_macro(
        &mut self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        _function_def: Option<&ASTTypedFunctionDef>,
        dereference: bool,
        type_def_provider: &dyn TypeDefProvider,
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

    fn is_pre_macro(&self) -> bool {
        false
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
        &mut self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        _function_def: Option<&ASTTypedFunctionDef>,
        dereference: bool,
        type_def_provider: &dyn TypeDefProvider,
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
                                is_reference(tt, type_def_provider)
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

    fn is_pre_macro(&self) -> bool {
        false
    }
}

fn is_reference(ast_type: &ASTType, type_def_provider: &dyn TypeDefProvider) -> bool {
    if let ASTType::Builtin(BuiltinTypeKind::String) = ast_type {
        true
    } else if let ASTType::Custom {
        name,
        param_types: _,
    } = ast_type
    {
        if let Some(t) = type_def_provider.get_type_def_by_name(name) {
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
        &mut self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        function_def: Option<&ASTTypedFunctionDef>,
        dereference: bool,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        if !dereference {
            return String::new();
        }
        if let Some(fd) = function_def {
            if fd.name == "addRef_0" {
                return String::new();
            }
            if let Some(MacroParam::Plain(generic_type_name, _)) = parameters.get(0) {
                if let Some(MacroParam::Plain(address, _)) = parameters.get(1) {
                    if let Some(ast_type) = fd.generic_types.get(generic_type_name) {
                        if let Some(type_name) =
                            CodeGen::get_reference_type_name(ast_type, type_def_provider)
                        {
                            let mut result = String::new();
                            let descr = &format!("addref macro type {type_name}");
                            if self.deref {
                                result.push_str(&backend.call_deref(
                                    address,
                                    &type_name,
                                    descr,
                                    type_def_provider,
                                    statics,
                                ));
                            } else {
                                backend.call_add_ref(
                                    &mut result,
                                    address,
                                    &type_name,
                                    descr,
                                    type_def_provider,
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

                        /*
                        let found_structs =
                            type_def_provider.get_struct_def_by_name(&name).is_some();

                        if found_structs {
                            panic!()
                        }

                        let found_enums = type_def_provider.get_enum_def_like_name(&name).is_some();

                        if found_enums {
                            panic!()
                        }

                         */

                        let real_name = if let Some(s) =
                            type_def_provider.get_struct_def_like_name(&name)
                        {
                            s.name.clone()
                        } else if let Some(s) = type_def_provider.get_enum_def_like_name(&name) {
                            s.name.clone()
                        } else if let Some(s) = type_def_provider.get_type_def_like_name(&name) {
                            s.name.clone()
                        } else {
                            panic!("cannot find struct nor enum {generic_type_name}");
                        };

                        let mut result = String::new();
                        if self.deref {
                            result.push_str(&backend.call_deref(
                                address,
                                &real_name,
                                descr,
                                type_def_provider,
                                statics,
                            ));
                        } else {
                            backend.call_add_ref(
                                &mut result,
                                address,
                                &real_name,
                                descr,
                                type_def_provider,
                                statics,
                            );
                        }
                        result
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

    fn is_pre_macro(&self) -> bool {
        false
    }
}

fn get_type(
    orig_name: &str,
    type_def_provider: &dyn TypeDefProvider,
    function_def_opt: Option<&ASTTypedFunctionDef>,
) -> ASTTypedType {
    if let Some(f) = function_def_opt {
        if let Some(t) = f.generic_types.get(orig_name) {
            return t.clone();
        }
    }

    /*
    let mut name = orig_name.to_owned();
    name.push('_');

    let found_structs = conv_context
        .structs
        .iter()
        .filter(|it| it.name.starts_with(&name))
        .count();

    if found_structs > 1 {
        panic!()
    }

    let found_enums = module
        .enums
        .iter()
        .filter(|it| it.name.starts_with(&name))
        .count();

    if found_enums > 1 {
        panic!()
    }

    let found_types = module
        .types
        .iter()
        .filter(|it| it.name.starts_with(&name))
        .count();

    if found_types > 1 {
        panic!()
    }

     */

    if let Some(s) = type_def_provider.get_struct_def_by_name(&orig_name) {
        ASTTypedType::Struct {
            name: s.name.clone(),
        }
    } else if let Some(s) = type_def_provider.get_enum_def_by_name(&orig_name) {
        ASTTypedType::Enum {
            name: s.name.clone(),
        }
    } else if let Some(s) = type_def_provider.get_type_def_by_name(&orig_name) {
        ASTTypedType::Type {
            name: s.name.clone(),
        }
    } else {
        panic!("Cannot find struct, enum or type {orig_name}");
    }
}

struct PrintRefMacro {}

impl TextMacroEval for PrintRefMacro {
    fn eval_macro(
        &mut self,
        backend: &dyn Backend,
        statics: &mut Statics,
        parameters: &[MacroParam],
        function_def: Option<&ASTTypedFunctionDef>,
        dereference: bool,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        let result = match parameters.get(0) {
            None => panic!("cannot find parameter for printRef macro"),
            Some(par) => match par {
                MacroParam::Plain(name, ast_type) => {
                    self.print_ref(name, ast_type, function_def, type_def_provider, 0, backend)
                }
                MacroParam::StringLiteral(_) => {
                    panic!("String is nt a valid parameter for printRef macro ")
                }
                MacroParam::Ref(name, ast_type) => {
                    self.print_ref(name, ast_type, function_def, type_def_provider, 0, backend)
                }
            },
        };

        result
    }

    fn is_pre_macro(&self) -> bool {
        true
    }
}

impl PrintRefMacro {
    fn new() -> Self {
        Self {}
    }

    fn print_ref(
        &mut self,
        src: &str,
        ast_type_o: &Option<ASTType>,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        backend: &dyn Backend,
    ) -> String {
        let mut result = String::new();

        let ast_typed_type = match ast_type_o {
            None => {
                panic!("printRef macro: cannot find the type of the parameter, please specify it")
            }
            Some(ast_type) => match ast_type {
                ASTType::Builtin(_) => panic!("printRef macro: unsupported type {ast_type}"),
                ASTType::Parametric(generic_type_name) => match function_def {
                    None => panic!(),
                    Some(f) => match f.generic_types.get(generic_type_name) {
                        None => {
                            panic!("printRef macro: Cannot find generic type {generic_type_name}")
                        }
                        Some(ast_typed_type) => ast_typed_type.clone(),
                    },
                },
                ASTType::Custom {
                    name: custom_type_name,
                    param_types: _,
                } => get_type(custom_type_name, type_def_provider, function_def),
            },
        };

        let (name, code) = match ast_typed_type {
            ASTTypedType::Builtin(_) => panic!(),
            ASTTypedType::Enum { name } => (
                name.clone(),
                self.print_ref_enum(&name, src, type_def_provider, indent + 1, backend),
            ),
            ASTTypedType::Struct { name } => (
                name.clone(),
                self.print_ref_struct(&name, src, type_def_provider, indent + 1, backend),
            ),
            ASTTypedType::Type { name } => (name, String::new()),
        };
        let ident_string = " ".repeat(indent * 2);
        CodeGen::add(
            &mut result,
            &format!("$call(print, \"{ident_string}{name} \")"),
            None,
            true,
        );
        CodeGen::add(&mut result, &format!("$call(print,{src}:i32)"), None, true);
        CodeGen::add(&mut result, "push    ebx", None, true);
        CodeGen::add(&mut result, "$call(print, \" = \")", None, true);
        CodeGen::add(&mut result, &format!("mov dword ebx, {src}"), None, true);
        CodeGen::add(&mut result, "$call(println,[ebx + 12])", None, true);
        CodeGen::add(&mut result, "pop    ebx", None, true);

        result.push_str(&code);

        result
    }

    fn print_ref_struct(
        &mut self,
        name: &str,
        src: &str,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        backend: &dyn Backend,
    ) -> String {
        let mut result = String::new();
        CodeGen::add(&mut result, "push    ebx", None, true);
        CodeGen::add(&mut result, &format!("mov dword ebx, {src}"), None, true);
        CodeGen::add(&mut result, "mov dword ebx, [ebx]", None, true);
        if let Some(s) = type_def_provider.get_struct_def_by_name(name) {
            for (i, p) in s.properties.iter().enumerate() {
                if let Some(name) = match &p.ast_type {
                    ASTTypedType::Builtin(_) => None,
                    ASTTypedType::Enum { name } => Some(name),
                    ASTTypedType::Struct { name } => Some(name),
                    ASTTypedType::Type { name } => Some(name),
                } {
                    let custom_type = ASTType::Custom {
                        name: name.clone(),
                        param_types: Vec::new(),
                    };
                    let par_result = self.print_ref(
                        &format!("[ebx + {}]", i * backend.word_len()),
                        &Some(custom_type),
                        None,
                        type_def_provider,
                        indent + 1,
                        backend,
                    );
                    result.push_str(&par_result);
                }
            }
        } else {
            panic!("Cannot find struct {name}");
        }
        CodeGen::add(&mut result, "pop    ebx", None, true);
        result
    }

    fn print_ref_enum(
        &mut self,
        name: &str,
        src: &str,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        backend: &dyn Backend,
    ) -> String {
        COUNT.with(|count| {
            *count.borrow_mut() += 1;

            let end_label_name = &format!("._{name}_end_{}", count.borrow());
            let ws = backend.word_size();
            let wl = backend.word_len();

            let mut result = String::new();
            CodeGen::add(&mut result, "push    ebx", None, true);
            CodeGen::add(&mut result, &format!("mov dword ebx, {src}"), None, true);
            CodeGen::add(&mut result, "mov dword ebx, [ebx]", None, true);
            if let Some(s) = type_def_provider.get_enum_def_by_name(name) {
                for (i, variant) in s.variants.iter().enumerate() {
                    let label_name = &format!("._{name}_variant_{}_{i}", count.borrow());
                    if !variant.parameters.is_empty() {
                        CodeGen::add(&mut result, &format!("cmp {ws} [ebx], {}", i), None, true);
                        CodeGen::add(&mut result, &format!("jne {label_name}"), None, true);
                        for (j, par) in variant.parameters.iter().enumerate() {
                            if let Some(name) =
                                CodeGen::get_reference_type_name(&par.ast_type, type_def_provider)
                            {
                                let custom_type = ASTType::Custom {
                                    name: name.clone(),
                                    param_types: Vec::new(),
                                };
                                let par_result = self.print_ref(
                                    &format!("[ebx + {}]", (j + 1) * wl),
                                    &Some(custom_type),
                                    None,
                                    type_def_provider,
                                    indent + 1,
                                    backend,
                                );
                                result.push_str(&par_result);
                            }
                        }
                    }
                    CodeGen::add(&mut result, &format!("jmp {end_label_name}"), None, false);
                    CodeGen::add(&mut result, &format!("{label_name}:"), None, false);
                }
            } else {
                panic!("Cannot find struct {name}");
            }
            CodeGen::add(&mut result, &format!("{end_label_name}:"), None, false);
            CodeGen::add(&mut result, "pop    ebx", None, true);
            result
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use linked_hash_map::LinkedHashMap;

    use crate::codegen::backend::BackendAsm386;
    use crate::codegen::statics::Statics;
    use crate::codegen::text_macro::{
        MacroParam, TextMacro, TextMacroEvaluator, TypeDefProvider, TypeParserHelper,
    };
    use crate::codegen::MemoryValue;
    use crate::parser::ast::{ASTIndex, ASTType, BuiltinTypeKind};
    use crate::parser::type_parser::TypeParser;
    use crate::type_check::typed_ast::{
        ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedParameterDef,
        ASTTypedStructDef, ASTTypedType, ASTTypedTypeDef, BuiltinTypedTypeKind,
    };

    #[derive(Debug)]
    struct DummyTypeDefProvider {}

    impl TypeDefProvider for DummyTypeDefProvider {
        fn get_enum_def_by_name(&self, _name: &str) -> Option<&ASTTypedEnumDef> {
            None
        }

        fn get_struct_def_by_name(&self, _name: &str) -> Option<&ASTTypedStructDef> {
            None
        }

        fn get_type_def_by_name(&self, _name: &str) -> Option<&ASTTypedTypeDef> {
            None
        }

        fn get_enum_def_like_name(&self, _name: &str) -> Option<&ASTTypedEnumDef> {
            None
        }

        fn get_struct_def_like_name(&self, _name: &str) -> Option<&ASTTypedStructDef> {
            None
        }

        fn get_type_def_like_name(&self, _name: &str) -> Option<&ASTTypedTypeDef> {
            None
        }
    }

    impl DummyTypeDefProvider {
        fn new() -> Self {
            Self {}
        }
    }

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
            true,
            false,
            &DummyTypeDefProvider::new(),
        );

        assert_eq!(
            result,
            Some("; call macro, calling println\n    push dword [_s_1]\n    call println\n    add esp, 4\n".to_owned())
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
            true,
            false,
            &DummyTypeDefProvider::new(),
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
            true,
            false,
            &DummyTypeDefProvider::new(),
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
                ast_index: ASTIndex::none(),
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
            true,
            false,
            &DummyTypeDefProvider::new(),
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
                ast_index: ASTIndex::none(),
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
            true,
            false,
            &DummyTypeDefProvider::new(),
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
            true,
            false,
            &DummyTypeDefProvider::new(),
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
                ast_index: ASTIndex::none(),
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
            true,
            false,
            &DummyTypeDefProvider::new(),
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
}
