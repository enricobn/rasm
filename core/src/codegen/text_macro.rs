use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::path::PathBuf;

use lazy_static::lazy_static;
use linked_hash_map::LinkedHashMap;
use log::debug;
use regex::Regex;

use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_reference_type_name, CodeGen, CodeGenAsm};
use crate::debug_i;
use crate::lexer::tokens::Token;
use crate::lexer::Lexer;
use crate::parser::ast::{ASTFunctionDef, ASTIndex, ASTNameSpace, ASTType, BuiltinTypeKind};
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserTrait;
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::substitute;
use crate::type_check::typed_ast::{
    ASTTypedFunctionDef, ASTTypedType, BuiltinTypedTypeKind, DefaultFunctionCall,
};
use crate::utils::OptionDisplay;

thread_local! {
    static COUNT : RefCell<usize> = RefCell::new(0);
}

#[derive(Debug, Clone)]
pub enum MacroParam {
    Plain(String, Option<ASTType>, Option<ASTTypedType>),
    StringLiteral(String),
    Ref(String, Option<ASTType>, Option<ASTTypedType>),
}

impl Display for MacroParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MacroParam::Plain(name, type_o, typed_type_o) => f.write_str(&format!(
                "Plain({name}, {}, {})",
                OptionDisplay(type_o),
                OptionDisplay(typed_type_o)
            )),
            MacroParam::StringLiteral(s) => f.write_str(&format!("\"{s}\"")),
            MacroParam::Ref(name, type_o, typed_type_o) => f.write_str(&format!(
                "Ref({name}, {}, {})",
                OptionDisplay(type_o),
                OptionDisplay(typed_type_o)
            )),
        }
    }
}

impl MacroParam {
    pub fn render(&self) -> String {
        match self {
            MacroParam::Plain(name, _type_o, _typed_type_o) => name.clone(),
            MacroParam::StringLiteral(s) => {
                format!("\"{s}\"")
            }
            MacroParam::Ref(name, _type_o, _typed_type_o) => name.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TextMacro {
    pub name: String,
    pub parameters: Vec<MacroParam>,
    pub index: ASTIndex,
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

impl TextMacro {
    pub fn render(&self) -> String {
        let parameters = self
            .parameters
            .iter()
            .map(|it| it.render())
            .collect::<Vec<String>>()
            .join(", ");
        format!("${}({parameters})", self.name)
    }
}

lazy_static! {
    static ref RE: Regex = Regex::new(r"\$([A-Za-z]*)\((.*)\)").unwrap();
}

pub struct TextMacroEvaluator {
    evaluators: LinkedHashMap<String, Box<dyn TextMacroEval>>,
    code_manipulator: Box<dyn CodeManipulator>,
}

impl TextMacroEvaluator {
    pub fn new(
        evaluators: LinkedHashMap<String, Box<dyn TextMacroEval>>,
        code_manipulator: Box<dyn CodeManipulator>,
    ) -> Self {
        Self {
            evaluators,
            code_manipulator,
        }
    }

    pub fn translate(
        &self,
        statics: &mut Statics,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&ASTFunctionDef>,
        body: &str,
        pre_macro: bool,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Result<String, String> {
        let index = typed_function_def
            .map(|tfd| tfd.index.clone())
            .or_else(|| function_def.map(|fd| fd.index.clone()))
            .unwrap_or(ASTIndex::none());

        let mut result = Vec::new();

        let lines = body.lines();

        for (i, s) in lines.enumerate() {
            let stripped_comments = self
                .code_manipulator
                .remove_comments_from_line(s.to_string());
            let matches = RE.captures_iter(&stripped_comments);

            let mut line_result = s.to_string();

            for cap in matches {
                let whole = cap.get(0).unwrap().as_str();
                let name = cap.get(1).unwrap().as_str();
                let parameters = cap.get(2).unwrap().as_str();

                let text_macro = TextMacro {
                    index: index.mv_down(i),
                    name: name.into(),
                    parameters: self.parse_params(
                        parameters,
                        typed_function_def,
                        function_def,
                        type_def_provider,
                    )?,
                };

                if let Some(s) = self.eval_macro(
                    statics,
                    &text_macro,
                    typed_function_def,
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

        Ok(new_body)
    }

    fn parse_params(
        &self,
        s: &str,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&ASTFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Result<Vec<MacroParam>, String> {
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
                        let param = self.get_param(
                            &actual_param,
                            typed_function_def,
                            function_def,
                            type_def_provider,
                        )?;

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
                let param = self.get_param(
                    &actual_param,
                    typed_function_def,
                    function_def,
                    type_def_provider,
                )?;
                result.push(param);
            }
            State::StringLiteral => {
                result.push(MacroParam::StringLiteral(actual_param));
            }
        }

        Ok(result)
    }

    fn get_param(
        &self,
        actual_param: &str,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&ASTFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Result<MacroParam, String> {
        let p = actual_param.trim();

        let context_generic_types = if let Some(f) = function_def {
            let mut result = f.generic_types.clone();
            for (name, _t) in f.resolved_generic_types.iter() {
                if !result.contains(name) {
                    result.push(name.clone());
                }
            }
            result
        } else {
            Vec::new()
        };

        if let Some(name) = p.strip_prefix('$') {
            match typed_function_def {
                None => match function_def {
                    None => {
                        panic!("Cannot resolve reference without a function {p}")
                    }
                    Some(f) => {
                        let (par_name, par_type, _par_typed_type) = self.parse_typed_argument(
                            &f.namespace,
                            name,
                            None,
                            type_def_provider,
                            &context_generic_types,
                            &f.resolved_generic_types,
                        )?;

                        if let Some(t) = par_type {
                            Ok(MacroParam::Ref(format!("${par_name}"), Some(t), None))
                        } else if let Some(par_type) = f
                            .parameters
                            .iter()
                            .find(|par| par.name == par_name)
                            .map(|it| it.ast_type.clone())
                        {
                            Ok(MacroParam::Ref(
                                format!("${par_name}"),
                                Some(par_type),
                                None,
                            ))
                        } else {
                            panic!("Cannot find parameter {name}");
                        }
                    }
                },
                Some(f) => {
                    let par_typed_type = f
                        .parameters
                        .iter()
                        .find(|par| par.name == name)
                        .map(|it| it.ast_type.clone());
                    let (par_name, par_type, par_typed_type) = self.parse_typed_argument(
                        &f.namespace,
                        name,
                        par_typed_type,
                        type_def_provider,
                        &context_generic_types,
                        &ResolvedGenericTypes::new(),
                    )?;
                    if !f.parameters.iter().any(|it| it.name == par_name) {
                        Err("Cannot find parameter {par_name}".to_owned())
                    } else {
                        Ok(MacroParam::Ref(
                            format!("${par_name}"),
                            par_type,
                            par_typed_type,
                        ))
                    }
                }
            }
        } else {
            let (par_name, par_type, par_typed_type) = if let Some(f) = function_def {
                self.parse_typed_argument(
                    &f.namespace,
                    p,
                    None,
                    type_def_provider,
                    &context_generic_types,
                    &f.resolved_generic_types,
                )?
            } else {
                self.parse_typed_argument(
                    &ASTNameSpace::global(), // TODO is it correct?
                    p,
                    None,
                    type_def_provider,
                    &context_generic_types,
                    &ResolvedGenericTypes::new(),
                )?
            };

            if let Some(ast_type) = &par_type {
                Ok(MacroParam::Plain(
                    par_name,
                    par_type.clone(),
                    par_typed_type.or_else(|| {
                        typed_function_def
                            .and_then(|it| Self::resolve_type(ast_type, it, type_def_provider))
                    }),
                ))
            } else {
                Ok(MacroParam::Plain(
                    par_name,
                    par_type.clone(),
                    par_typed_type,
                ))
            }
        }
    }

    fn resolve_type(
        ast_type: &ASTType,
        function_def: &ASTTypedFunctionDef,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Option<ASTTypedType> {
        if let ASTType::Custom {
            namespace: _,
            name,
            param_types,
            index: _,
        } = ast_type
        {
            if let Some(typed_type) = function_def.generic_types.get(name) {
                Some(typed_type.clone())
            } else if param_types.is_empty() {
                type_def_provider.get_ast_typed_type_from_type_name(name)
            } else {
                let resolved_types = param_types
                    .iter()
                    .map(|it| {
                        if let Some(typed_type) =
                            Self::resolve_type(it, function_def, type_def_provider)
                        {
                            match typed_type {
                                ASTTypedType::Builtin(kind) => match kind {
                                    BuiltinTypedTypeKind::String => {
                                        ASTType::Builtin(BuiltinTypeKind::String)
                                    }
                                    BuiltinTypedTypeKind::I32 => {
                                        ASTType::Builtin(BuiltinTypeKind::I32)
                                    }
                                    BuiltinTypedTypeKind::Bool => {
                                        ASTType::Builtin(BuiltinTypeKind::Bool)
                                    }
                                    BuiltinTypedTypeKind::Char => {
                                        ASTType::Builtin(BuiltinTypeKind::Char)
                                    }
                                    BuiltinTypedTypeKind::F32 => {
                                        ASTType::Builtin(BuiltinTypeKind::F32)
                                    }
                                    BuiltinTypedTypeKind::Lambda { .. } => {
                                        panic!()
                                    }
                                },
                                _ => type_def_provider
                                    .get_type_from_custom_typed_type(&typed_type)
                                    .unwrap(),
                            }
                        } else {
                            it.clone()
                        }
                    })
                    .collect::<Vec<_>>();

                let ast_type_to_resolve = ASTType::Custom {
                    namespace: ast_type.namespace(),
                    name: name.clone(),
                    param_types: resolved_types,
                    index: ASTIndex::none(),
                };

                type_def_provider.get_ast_typed_type_from_ast_type(&ast_type_to_resolve)
            }
        } else {
            None
        }
    }

    fn parse_typed_argument(
        &self,
        namespace: &ASTNameSpace,
        p: &str,
        typed_type: Option<ASTTypedType>,
        type_def_provider: &dyn TypeDefProvider,
        context_generic_types: &[String],
        resolved_generic_types: &ResolvedGenericTypes,
    ) -> Result<(String, Option<ASTType>, Option<ASTTypedType>), String> {
        //println!("parse_typed_argument namespace {namespace}, p {p}");
        // TODO the check of :: is a trick since function names could have ::, try to do it better
        let (par_name, par_type, par_typed_type) = if p.contains(':') && !p.contains("::") {
            let vec = p.split(':').collect::<Vec<_>>();
            let par_type_name = vec.get(1).unwrap().trim();
            let par_name = vec.first().unwrap().trim();

            // println!("found param type {par_type_name}");

            if par_type_name == "i32" {
                (
                    par_name,
                    Some(ASTType::Builtin(BuiltinTypeKind::I32)),
                    Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::I32)),
                )
            } else if par_type_name == "str" {
                (
                    par_name,
                    Some(ASTType::Builtin(BuiltinTypeKind::String)),
                    Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::String)),
                )
            } else if par_type_name == "f32" {
                (
                    par_name,
                    Some(ASTType::Builtin(BuiltinTypeKind::F32)),
                    Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::F32)),
                )
            } else {
                let parser = TypeParserHelper::new(None, par_type_name);
                // Parser::new(lexer, None);

                let type_parser = TypeParser::new(&parser);

                match type_parser.try_parse_ast_type(namespace, 0, context_generic_types)? {
                    None => {
                        panic!("Unsupported type {par_type_name}")
                    }
                    Some((ast_type, _)) => {
                        //println!("parse_typed_argument {ast_type}");
                        let t = if let ASTType::Generic(_name) = &ast_type {
                            if let Some(t) = substitute(&ast_type, resolved_generic_types) {
                                t
                            } else {
                                ast_type
                            }
                        } else {
                            ast_type
                        };
                        (
                            par_name,
                            Some(t),
                            type_def_provider
                                .get_typed_type_def_from_type_name(par_type_name)
                                .map(|it| it.ast_typed_type),
                        )
                    }
                }
            }
        } else if let Some(t) = &typed_type {
            let ast_type = Self::typed_type_to_type(t, type_def_provider);
            (p, Some(ast_type), typed_type.clone())
        } else {
            (p, None, typed_type)
        };
        Ok((par_name.into(), par_type, par_typed_type))
    }

    fn typed_type_to_type(
        typed_type: &ASTTypedType,
        type_def_provider: &dyn TypeDefProvider,
    ) -> ASTType {
        match typed_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => ASTType::Builtin(BuiltinTypeKind::String),
                BuiltinTypedTypeKind::I32 => ASTType::Builtin(BuiltinTypeKind::I32),
                BuiltinTypedTypeKind::Bool => ASTType::Builtin(BuiltinTypeKind::Bool),
                BuiltinTypedTypeKind::Char => ASTType::Builtin(BuiltinTypeKind::Char),
                BuiltinTypedTypeKind::F32 => ASTType::Builtin(BuiltinTypeKind::F32),
                BuiltinTypedTypeKind::Lambda {
                    parameters,
                    return_type,
                } => ASTType::Builtin(BuiltinTypeKind::Lambda {
                    parameters: parameters
                        .iter()
                        .map(|it| Self::typed_type_to_type(it, type_def_provider))
                        .collect::<Vec<_>>(),
                    return_type: Box::new(Self::typed_type_to_type(
                        return_type.borrow(),
                        type_def_provider,
                    )),
                }),
            },
            ASTTypedType::Enum { namespace: _, name } => type_def_provider
                .get_type_from_typed_type_name(name)
                .unwrap(),
            ASTTypedType::Struct { namespace: _, name } => type_def_provider
                .get_type_from_typed_type_name(name)
                .unwrap(),
            ASTTypedType::Type { namespace: _, name } => type_def_provider
                .get_type_from_typed_type_name(name)
                .unwrap(),
            ASTTypedType::Unit => ASTType::Unit,
        }
    }

    pub fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        pre_macro: bool,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Option<String> {
        let evaluator = self
            .evaluators
            .get(&text_macro.name)
            .unwrap_or_else(|| panic!("{} macro not found", &text_macro.name));

        if evaluator.is_pre_macro() == pre_macro {
            Some(evaluator.eval_macro(statics, &text_macro, function_def, type_def_provider))
        } else {
            None
        }
    }

    pub fn get_macros(
        &self,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&ASTFunctionDef>,
        body: &str,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Result<Vec<(TextMacro, usize)>, String> {
        let index = typed_function_def
            .map(|tfd| tfd.index.clone())
            .or_else(|| function_def.map(|fd| fd.index.clone()))
            .unwrap_or(ASTIndex::none());

        let mut result = Vec::new();

        let lines = body.lines();

        for (i, s) in lines.enumerate() {
            let stripped_comments = self
                .code_manipulator
                .remove_comments_from_line(s.to_string());
            let matches = RE.captures_iter(&stripped_comments);

            for cap in matches {
                let name = cap.get(1).unwrap().as_str();
                let parameters = cap.get(2).unwrap().as_str();

                let text_macro = TextMacro {
                    index: index.mv_down(i),
                    name: name.into(),
                    parameters: self.parse_params(
                        parameters,
                        typed_function_def,
                        function_def,
                        type_def_provider,
                    )?,
                };

                result.push((text_macro, i));
            }
        }
        Ok(result)
    }

    pub fn default_function_calls(
        &self,
        macro_name: &str,
    ) -> Result<Vec<DefaultFunctionCall>, String> {
        if let Some(tme) = self.evaluators.get(macro_name) {
            Ok(tme.default_function_calls())
        } else {
            Err(format!("Cannot find macro `{macro_name}`"))
        }
    }
}

struct TypeParserHelper {
    type_tokens: Vec<Token>,
}

impl TypeParserHelper {
    fn new(file_name: Option<PathBuf>, type_str: &str) -> Self {
        let lexer = Lexer::new(type_str.into(), file_name);
        // TODO errors
        let (tokens, _errors) = lexer.process();
        Self {
            type_tokens: tokens,
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
}

pub trait TextMacroEval {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String;

    fn is_pre_macro(&self) -> bool;

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall>;
}

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
    ) -> String {
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

        result
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
    ) -> String {
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

        result.push_str(
            &text_macro.parameters
                .iter().enumerate()
                .skip(1)
                .rev()
                .map(|(index, it)| {
                    let i = index - 1;
                    match it {
                        MacroParam::Plain(s, _, _) => {
                            format!("    mov {ws} ecx, {s}\n    mov {ws} [{sp}+{}], ecx\n", i * wl)
                        }
                        MacroParam::StringLiteral(s) => {
                            let key = statics.add_str(s);
                            format!("    mov {ws} ecx, [{key}]\n    mov {ws} ecx,[ecx]\n    mov {ws} [{sp}+{}], ecx\n", i * wl)
                        }
                        MacroParam::Ref(s, t, _) => {
                            let is_ref =
                            if let Some(tt) = t {
                                tt.is_reference(type_def_provider)
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

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

fn get_type(
    namespace: &ASTNameSpace,
    orig_name: &str,
    type_def_provider: &dyn TypeDefProvider,
    function_def_opt: Option<&ASTTypedFunctionDef>,
) -> ASTTypedType {
    if let Some(f) = function_def_opt {
        if let Some(t) = f.generic_types.get(orig_name) {
            return t.clone();
        }
    }

    if let Some(s) = type_def_provider.get_struct_def_like_name(namespace, orig_name) {
        ASTTypedType::Struct {
            namespace: namespace.clone(),
            name: s.name.clone(),
        }
    } else if let Some(s) = type_def_provider.get_enum_def_like_name(namespace, orig_name) {
        ASTTypedType::Enum {
            namespace: namespace.clone(),
            name: s.name.clone(),
        }
    } else if let Some(s) = type_def_provider.get_type_def_like_name(namespace, orig_name) {
        ASTTypedType::Type {
            namespace: namespace.clone(),
            name: s.name.clone(),
        }
    } else if orig_name == "str" {
        ASTTypedType::Builtin(BuiltinTypedTypeKind::String)
    } else {
        panic!("Cannot find struct, enum or type {orig_name}");
    }
}

pub enum RefType {
    Deref,
    AddRef,
}

pub struct AddRefMacro {
    code_gen: CodeGenAsm,
    ref_type: RefType,
    dereference_enabled: bool,
}

impl AddRefMacro {
    pub fn new(code_gen: CodeGenAsm, ref_type: RefType, dereference_enabled: bool) -> Self {
        Self {
            code_gen,
            ref_type,
            dereference_enabled,
        }
    }
}

impl TextMacroEval for AddRefMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        if !self.dereference_enabled {
            return String::new();
        }
        if let Some(fd) = function_def {
            if fd.name == "addRef" {
                return String::new();
            }

            let (address, ast_typed_type) = match text_macro.parameters.get(0) {
                Some(MacroParam::Plain(address, _ast_type, Some(ast_typed_type))) => {
                    (address, ast_typed_type)
                }
                Some(MacroParam::Ref(address, _ast_type, Some(ast_typed_type))) => {
                    (address, ast_typed_type)
                }
                _ => panic!(
                    "Error: addRef/deref macro, a typed type must be specified in function {} but got {}: {}",
                    OptionDisplay(&function_def),
                    text_macro.parameters.get(0).unwrap(),
                    text_macro.index
                ),
            };

            let type_name = match ast_typed_type {
                ASTTypedType::Builtin(BuiltinTypedTypeKind::String) => "str".to_string(),
                ASTTypedType::Struct { namespace: _, name } => name.clone(),
                ASTTypedType::Enum { namespace: _, name } => name.clone(),
                ASTTypedType::Type { namespace: _, name } => name.clone(),
                _ => return String::new(),
            };

            let mut result = String::new();
            let descr = &format!("addref macro type {type_name}");

            match self.ref_type {
                RefType::Deref => result.push_str(&self.code_gen.call_deref(
                    address,
                    &type_name,
                    descr,
                    type_def_provider,
                    statics,
                )),
                RefType::AddRef => {
                    self.code_gen.call_add_ref(
                        &mut result,
                        address,
                        &type_name,
                        descr,
                        type_def_provider,
                        statics,
                    );
                }
            }

            result
        } else {
            String::new()
        }
    }

    fn is_pre_macro(&self) -> bool {
        false
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

pub struct PrintRefMacro {
    code_gen: CodeGenAsm,
}

impl TextMacroEval for PrintRefMacro {
    fn eval_macro(
        &self,
        _statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        let result = match text_macro.parameters.get(0) {
            None => panic!("cannot find parameter for printRef macro"),
            Some(par) => match par {
                MacroParam::Plain(name, ast_type, ast_typed_type) => self.print_ref(
                    name,
                    ast_type,
                    ast_typed_type,
                    function_def,
                    type_def_provider,
                    0,
                    &self.code_gen,
                ),
                MacroParam::StringLiteral(_) => {
                    panic!("String is nt a valid parameter for printRef macro ")
                }
                MacroParam::Ref(name, ast_type, ast_typed_type) => self.print_ref(
                    name,
                    ast_type,
                    ast_typed_type,
                    function_def,
                    type_def_provider,
                    0,
                    &self.code_gen,
                ),
            },
        };

        result
    }

    fn is_pre_macro(&self) -> bool {
        true
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        vec![
            DefaultFunctionCall::new("print", vec![ASTType::Builtin(BuiltinTypeKind::I32)], 0),
            DefaultFunctionCall::new("println", vec![ASTType::Builtin(BuiltinTypeKind::I32)], 0),
            DefaultFunctionCall::new("print", vec![ASTType::Builtin(BuiltinTypeKind::String)], 0),
            DefaultFunctionCall::new(
                "println",
                vec![ASTType::Builtin(BuiltinTypeKind::String)],
                0,
            ),
            DefaultFunctionCall::new("println", Vec::new(), 0),
        ]
    }
}

impl PrintRefMacro {
    pub fn new(code_gen: CodeGenAsm) -> Self {
        Self { code_gen }
    }

    fn print_ref(
        &self,
        src: &str,
        ast_type_o: &Option<ASTType>,
        ast_typed_type_o: &Option<ASTTypedType>,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        code_gen: &CodeGenAsm,
    ) -> String {
        if indent > 20 {
            return String::new();
        }

        let ast_typed_type = if let Some(ast_typed_type) = ast_typed_type_o {
            ast_typed_type.clone()
        } else if let Some(ast_type) = ast_type_o {
            if let Some(ast_typed_type) =
                type_def_provider.get_ast_typed_type_from_ast_type(ast_type)
            {
                ast_typed_type
            } else {
                match ast_type_o {
                    None => {
                        panic!("printRef macro: cannot find the type of the parameter {src}, please specify it")
                    }
                    Some(ast_type) => match ast_type {
                        ASTType::Builtin(BuiltinTypeKind::String) => {
                            ASTTypedType::Builtin(BuiltinTypedTypeKind::String)
                        }
                        ASTType::Generic(generic_type_name) => match function_def {
                            None => panic!(),
                            Some(f) => match f.generic_types.get(generic_type_name) {
                                None => {
                                    panic!("printRef macro: Cannot find generic type {generic_type_name}")
                                }
                                Some(ast_typed_type) => ast_typed_type.clone(),
                            },
                        },
                        ASTType::Custom {
                            namespace,
                            name: custom_type_name,
                            param_types: _,
                            index: _,
                        } => get_type(namespace, custom_type_name, type_def_provider, function_def),
                        _ => panic!("printRef macro: unsupported type {ast_type}"),
                    },
                }
            }
        } else {
            panic!();
        };

        let (name, code, new_line) = match ast_typed_type {
            ASTTypedType::Builtin(BuiltinTypedTypeKind::String) => {
                ("str".to_owned(), String::new(), true)
            }
            ASTTypedType::Enum { namespace: _, name } => (
                name.clone(),
                self.print_ref_enum(&name, src, type_def_provider, indent + 1, code_gen),
                false,
            ),
            ASTTypedType::Struct { namespace: _, name } => (
                name.clone(),
                self.print_ref_struct(&name, src, type_def_provider, indent + 1, code_gen),
                true,
            ),
            ASTTypedType::Type { namespace: _, name } => (
                name.clone(),
                self.print_ref_type(&name, src, type_def_provider, indent + 1, code_gen),
                true,
            ),
            _ => panic!("unsupported type {ast_typed_type}"),
        };

        let mut result = String::new();

        let ident_string = " ".repeat(indent * 2);
        self.print_str(&mut result, &format!("{ident_string}{name} "), code_gen);

        self.print_i32(&mut result, src, code_gen);
        code_gen.add(&mut result, "push    ebx", None, true);
        code_gen.add(
            &mut result,
            &format!("push    {} {src}", code_gen.word_size()),
            None,
            true,
        );
        code_gen.add(&mut result, "pop    ebx", None, true);
        self.print_str(&mut result, " refcount ", code_gen);
        if new_line {
            self.println_i32(&mut result, "[ebx + 12]", code_gen);
        } else {
            self.print_i32(&mut result, "[ebx + 12]", code_gen);
        }
        code_gen.add(&mut result, "pop    ebx", None, true);

        result.push_str(&code);

        result
    }

    fn println_i32(&self, out: &mut String, value: &str, code_gen: &CodeGenAsm) {
        code_gen.add(
            out,
            //&format!("$call(println_i32_Unit,{value}:i32)"),
            &format!("$call(println,{value}:i32)"),
            None,
            true,
        );
    }

    fn print_i32(&self, out: &mut String, value: &str, code_gen: &CodeGenAsm) {
        code_gen.add(
            out,
            //&format!("$call(print_i32_Unit,{value}:i32)"),
            &format!("$call(print,{value}:i32)"),
            None,
            true,
        );
    }

    fn print_str(&self, out: &mut String, value: &str, code_gen: &CodeGenAsm) {
        code_gen.add(
            out,
            //&format!("$call(print_str_Unit,\"{value}\")"),
            &format!("$call(print,\"{value}\")"),
            None,
            true,
        );
    }

    fn println_str(&self, out: &mut String, value: &str, code_gen: &CodeGenAsm) {
        code_gen.add(
            out,
            //&format!("$call(println_str_Unit,\"{value}\")"),
            &format!("$call(println,\"{value}\")"),
            None,
            true,
        );
    }

    fn print_ref_struct(
        &self,
        name: &str,
        src: &str,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        code_gen: &CodeGenAsm,
    ) -> String {
        let mut result = String::new();
        code_gen.add(&mut result, "push    ebx", None, true);
        code_gen.add(&mut result, &format!("mov dword ebx, {src}"), None, true);
        code_gen.add(&mut result, "mov dword ebx, [ebx]", None, true);
        if let Some(s) = type_def_provider.get_struct_def_by_name(name) {
            for (i, p) in s.properties.iter().enumerate() {
                let ast_type_o = if matches!(
                    p.ast_type,
                    ASTTypedType::Builtin(BuiltinTypedTypeKind::String)
                ) {
                    Some(ASTType::Builtin(BuiltinTypeKind::String))
                } else {
                    type_def_provider.get_type_from_custom_typed_type(&p.ast_type)
                };
                if ast_type_o.is_some() {
                    let par_result = self.print_ref(
                        &format!("[ebx + {}]", i * code_gen.word_len()),
                        &ast_type_o,
                        &None,
                        None,
                        type_def_provider,
                        indent + 1,
                        code_gen,
                    );
                    result.push_str(&par_result);
                }
            }
        } else {
            panic!("Cannot find struct {name}");
        }
        code_gen.add(&mut result, "pop    ebx", None, true);
        result
    }

    fn print_ref_enum(
        &self,
        name: &str,
        src: &str,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        code_gen: &CodeGenAsm,
    ) -> String {
        let count = COUNT.with(|count| {
            *count.borrow_mut() += 1;
            *count.borrow()
        });

        let end_label_name = &format!("._{name}_end_{}", count);
        let ws = code_gen.word_size();
        let wl = code_gen.word_len();

        let mut result = String::new();
        code_gen.add(&mut result, "push    ebx", None, true);
        code_gen.add(&mut result, &format!("mov dword ebx, {src}"), None, true);
        code_gen.add(&mut result, "mov dword ebx, [ebx]", None, true);
        self.print_str(&mut result, " value ", code_gen);
        if let Some(s) = type_def_provider.get_enum_def_by_name(name) {
            for (i, variant) in s.variants.iter().enumerate() {
                let count = COUNT.with(|count| {
                    *count.borrow_mut() += 1;
                    *count.borrow()
                });
                let label_name = &format!("._{name}_{}_{}", variant.name, count);
                code_gen.add(&mut result, &format!("cmp {ws} [ebx], {}", i), None, true);
                code_gen.add(&mut result, &format!("jne {label_name}"), None, true);
                self.println_str(&mut result, &variant.name, code_gen);

                for (j, par) in variant.parameters.iter().enumerate() {
                    if get_reference_type_name(&par.ast_type, type_def_provider).is_some() {
                        let ast_type = if matches!(
                            &par.ast_type,
                            ASTTypedType::Builtin(BuiltinTypedTypeKind::String)
                        ) {
                            Some(ASTType::Builtin(BuiltinTypeKind::String))
                        } else {
                            type_def_provider.get_type_from_custom_typed_type(&par.ast_type)
                        };

                        let par_result = self.print_ref(
                            &format!("[ebx + {}]", (variant.parameters.len() - j) * wl),
                            &ast_type,
                            &None,
                            None,
                            type_def_provider,
                            indent + 1,
                            code_gen,
                        );
                        result.push_str(&par_result);
                    }
                }
                code_gen.add(&mut result, &format!("jmp {end_label_name}"), None, false);
                code_gen.add(&mut result, &format!("{label_name}:"), None, false);
            }
        } else {
            panic!("Cannot find enum {name}");
        }
        self.print_str(&mut result, "unknown ", code_gen);
        self.println_i32(&mut result, "[ebx]", code_gen);
        code_gen.add(&mut result, "$call(exitMain, 1)", None, false);
        code_gen.add(&mut result, &format!("{end_label_name}:"), None, false);
        code_gen.add(&mut result, "pop    ebx", None, true);
        result
    }

    fn print_ref_type(
        &self,
        name: &str,
        src: &str,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        code_gen: &CodeGenAsm,
    ) -> String {
        if let Some(s) = type_def_provider.get_type_def_by_name(name) {
            let count = COUNT.with(|count| {
                *count.borrow_mut() += 1;
                *count.borrow()
            });

            let mut result = String::new();

            code_gen.add(
                &mut result,
                &format!("push  {} eax", code_gen.word_size()),
                None,
                true,
            );
            code_gen.add(
                &mut result,
                &format!("push  {} ebx", code_gen.word_size()),
                None,
                true,
            );
            code_gen.add(
                &mut result,
                &format!("push  {} ecx", code_gen.word_size()),
                None,
                true,
            );
            for (i, (generic_name, ast_typed_type)) in s.generic_types.iter().enumerate() {
                code_gen.add(
                    &mut result,
                    &format!("$call({}References, {src}:i32,{i})", s.original_name),
                    None,
                    true,
                );

                code_gen.add(
                    &mut result,
                    &format!("mov   {} eax,[eax]", code_gen.word_size()),
                    None,
                    true,
                );

                // count
                code_gen.add(
                    &mut result,
                    &format!("mov   {} ebx,[eax]", code_gen.word_size()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("add {} eax,{}", code_gen.word_size(), code_gen.word_len()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!(".loop_{name}_{generic_name}_{count}:"),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("test {} ebx,ebx", code_gen.word_size()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("jz .end_{name}_{generic_name}_{count}"),
                    None,
                    true,
                );
                let inner_result = self.print_ref(
                    "[eax]",
                    &None,
                    &Some(ast_typed_type.clone()),
                    None,
                    type_def_provider,
                    indent + 1,
                    code_gen,
                );
                result.push_str(&inner_result);
                code_gen.add(
                    &mut result,
                    &format!("dec {} ebx", code_gen.word_size()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("add {} eax,{}", code_gen.word_size(), code_gen.word_len()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("jmp .loop_{name}_{generic_name}_{count}"),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!(".end_{name}_{generic_name}_{count}:"),
                    None,
                    true,
                );
            }
            code_gen.add(&mut result, "pop  ecx", None, true);
            code_gen.add(&mut result, "pop  ebx", None, true);
            code_gen.add(&mut result, "pop  eax", None, true);

            result
        } else {
            panic!("print_ref_type, cannot find type {name}");
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::compile_target::CompileTarget;
    use crate::codegen::{AsmOptions, CodeGen, CodeGenAsm};
    use linked_hash_map::LinkedHashMap;

    use crate::codegen::statics::{MemoryValue, Statics};
    use crate::codegen::text_macro::{MacroParam, TextMacro, TypeParserHelper};
    use crate::codegen::typedef_provider::DummyTypeDefProvider;
    use crate::parser::ast::{
        ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModifiers, ASTParameterDef, ASTType,
        BuiltinTypeKind,
    };
    use crate::parser::type_parser::TypeParser;
    use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
    use crate::type_check::typed_ast::{
        ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedParameterDef, ASTTypedType,
        BuiltinTypedTypeKind,
    };
    use crate::utils::tests::test_namespace;

    #[test]
    fn call() {
        let text_macro = TextMacro {
            index: ASTIndex::none(),
            name: "call".into(),
            parameters: vec![
                MacroParam::Plain("println".into(), None, None),
                MacroParam::StringLiteral("Hello world".into()),
            ],
        };

        let mut statics = Statics::new();

        let result = code_gen().get_text_macro_evaluator().eval_macro(
            &mut statics,
            &text_macro,
            None,
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
        let mut statics = Statics::new();

        let result = code_gen()
            .get_text_macro_evaluator()
            .translate(
                &mut statics,
                None,
                None,
                "a line\n$call(nprint,10)\nanother line\n",
                false,
                &DummyTypeDefProvider::new(),
            )
            .unwrap();

        assert_eq!(
            result,
            "a line\n; call macro, calling nprint\n    push dword 10\n    call nprint\n    add esp, 4\n\nanother line\n"
        );
    }

    #[test]
    fn parse_string_par() {
        let mut statics = Statics::new();

        let result = code_gen()
            .get_text_macro_evaluator()
            .translate(
                &mut statics,
                None,
                None,
                "a line\n$call(println, \"Hello, world\")\nanother line\n",
                false,
                &DummyTypeDefProvider::new(),
            )
            .unwrap();

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
        let mut statics = Statics::new();

        let function_def = ASTTypedFunctionDef {
            namespace: test_namespace(),
            name: "aFun".into(),
            original_name: "aFun".into(),
            parameters: vec![ASTTypedParameterDef {
                name: "s".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: ASTIndex::none(),
            }],
            body: ASTTypedFunctionBody::NativeBody("".into()),
            generic_types: LinkedHashMap::new(),
            return_type: ASTTypedType::Unit,
            inline: false,
            index: ASTIndex::none(),
        };

        let result = code_gen()
            .get_text_macro_evaluator()
            .translate(
                &mut statics,
                Some(&function_def),
                None,
                "a line\n$call(println, $s)\nanother line\n",
                false,
                &DummyTypeDefProvider::new(),
            )
            .unwrap();

        assert_eq!(
            result,
            "a line\n; call macro, calling println\n    push dword $s\n    call println\n    add esp, 4\n\nanother line\n"
        );
    }

    #[test]
    fn parse_ref_par_c() {
        let mut statics = Statics::new();

        let function_def = ASTTypedFunctionDef {
            namespace: test_namespace(),
            name: "aFun".into(),
            original_name: "aFun".into(),
            parameters: vec![ASTTypedParameterDef {
                name: "s".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: ASTIndex::none(),
            }],
            body: ASTTypedFunctionBody::NativeBody("".into()),
            generic_types: LinkedHashMap::new(),
            return_type: ASTTypedType::Unit,
            inline: false,
            index: ASTIndex::none(),
        };

        let result = code_gen()
            .get_text_macro_evaluator()
            .translate(
                &mut statics,
                Some(&function_def),
                None,
                "a line\n$ccall(printf, $s)\nanother line\n",
                false,
                &DummyTypeDefProvider::new(),
            )
            .unwrap();

        assert_eq!(
            result,
            "a line\n; ccall macro, calling printf\n    push   edx\n    push   ebx\n    push   ecx\n    mov   dword ebx, esp\n    sub   esp, 4\n    and   esp,0xfffffff0\n    mov dword ecx, $s\n    mov dword ecx, [ecx]\n   mov dword [esp+0], ecx\n\n    call printf\n    mov   esp,ebx\n    pop   ecx\n    pop   ebx\n    pop   edx\n\nanother line\n"
        );
    }

    #[test]
    fn test() {
        let mut statics = Statics::new();

        let result = code_gen()
            .get_text_macro_evaluator()
            .translate(
                &mut statics,
                None,
                None,
                "mov     eax, 1          ; $call(any)",
                false,
                &DummyTypeDefProvider::new(),
            )
            .unwrap();

        assert_eq!(result, "mov     eax, 1          ; $call(any)");
    }

    #[test]
    fn test_get_macros() {
        let function_def = ASTTypedFunctionDef {
            namespace: test_namespace(),
            name: "aFun".into(),
            original_name: "aFun".into(),
            parameters: vec![ASTTypedParameterDef {
                name: "s".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: ASTIndex::none(),
            }],
            body: ASTTypedFunctionBody::NativeBody("".into()),
            generic_types: LinkedHashMap::new(),
            return_type: ASTTypedType::Unit,
            inline: false,
            index: ASTIndex::none(),
        };

        let macros = code_gen()
            .get_text_macro_evaluator()
            .get_macros(
                Some(&function_def),
                None,
                "$call(slen, $s)",
                &DummyTypeDefProvider::new(),
            )
            .unwrap();

        let (m, _i) = macros.get(0).unwrap();
        let param = m.parameters.get(1).unwrap();
        match param {
            MacroParam::Plain(_, _, _) => panic!("plain"),
            MacroParam::StringLiteral(_) => panic!("string literal"),
            MacroParam::Ref(r, _, _) => assert_eq!(r, "$s"),
        }
    }

    #[test]
    #[ignore]
    fn translate_typed() {
        let mut statics = Statics::new();

        let result = code_gen()
            .get_text_macro_evaluator()
            .translate(
                &mut statics,
                None,
                None,
                "$call(List_0_addRef,eax:List_0)",
                false,
                &DummyTypeDefProvider::new(),
            )
            .unwrap();

        assert_eq!(
            result,
            "; call macro, calling List_0_addRef\n    push dword eax\n    call List_0_addRef\n    add esp, 4\n"
        );
    }

    #[test]
    fn translate_ref_to_par_overridden() {
        let code_gen = code_gen();

        let function_def = ASTFunctionDef {
            name: "aFun".into(),
            original_name: "aFun".into(),
            parameters: vec![ASTParameterDef {
                name: "s".into(),
                ast_type: ASTType::Builtin(BuiltinTypeKind::String),
                ast_index: ASTIndex::none(),
            }],
            body: ASTFunctionBody::NativeBody("".into()),
            generic_types: Vec::new(),
            return_type: ASTType::Unit,
            inline: false,
            index: ASTIndex::none(),
            resolved_generic_types: ResolvedGenericTypes::new(),
            modifiers: ASTModifiers::private(),
            namespace: test_namespace(),
            rank: 0,
        };

        let result = code_gen.get_text_macro_evaluator().get_macros(
            None,
            Some(&function_def),
            "$call(List_0_addRef,$s:i32)",
            &DummyTypeDefProvider::new(),
        );

        assert_eq!(
            "Ok([(TextMacro { name: \"call\", parameters: [Plain(\"List_0_addRef\", None, None), Ref(\"$s\", Some(Builtin(I32)), None)], index: ASTIndex { file_name: None, row: 0, column: 0 } }, 0)])",
            &format!("{:?}", result),
        );
    }

    fn code_gen() -> CodeGenAsm {
        CodeGenAsm::new(AsmOptions::default(), false)
    }

    #[test]
    fn parse_list_str() {
        let parser = TypeParserHelper::new(None, "List<str>");
        let type_parser = TypeParser::new(&parser);

        match type_parser
            .try_parse_ast_type(&test_namespace(), 0, &[])
            .unwrap()
        {
            None => panic!("Unsupported type"),
            Some((ast_type, _)) => assert_eq!(
                ast_type,
                ASTType::Custom {
                    namespace: test_namespace(),
                    name: "List".into(),
                    param_types: vec![ASTType::Builtin(BuiltinTypeKind::String)],
                    index: ASTIndex::none()
                }
            ),
        }
    }

    fn target() -> CompileTarget {
        CompileTarget::Nasmi386(AsmOptions::default())
    }
}
