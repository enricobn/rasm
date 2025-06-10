use std::borrow::Borrow;

use std::fmt::{Debug, Display, Formatter};

use lazy_static::lazy_static;
use linked_hash_map::LinkedHashMap;
use linked_hash_set::LinkedHashSet;
use regex::Regex;

use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::enh_ast::{
    EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTType, EnhBuiltinTypeKind,
};
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::enh_type_check::enh_resolved_generic_types::EnhResolvedGenericTypes;
use crate::enh_type_check::typed_ast::{
    ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedType, BuiltinTypedTypeKind,
    DefaultFunctionCall,
};
use crate::type_check::substitute;
use rasm_parser::lexer::tokens::Token;
use rasm_parser::lexer::Lexer;
use rasm_parser::parser::ast;
use rasm_parser::parser::type_parser::TypeParser;
use rasm_parser::parser::ParserTrait;
use rasm_utils::{OptionDisplay, SliceDisplay};

use super::asm::code_gen_asm::CodeGenAsm;
use super::enh_ast::EnhModuleId;

#[derive(Debug, Clone)]
pub enum MacroParam {
    Plain(String, Option<EnhASTType>, Option<ASTTypedType>),
    StringLiteral(String),
    Ref(String, Option<EnhASTType>, Option<ASTTypedType>),
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
    pub index: EnhASTIndex,
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
    static ref RE: Regex = Regex::new(r#"\$([A-Za-z]*)\(((?:[^\")]*|".*?")*)\)"#).unwrap();
}

pub struct TextMacroEvaluator {
    evaluators: LinkedHashMap<String, Box<dyn TextMacroEval>>,
    code_manipulator: Box<dyn CodeManipulator>,
}

impl TextMacroEvaluator {
    pub fn new(code_manipulator: impl CodeManipulator + 'static) -> Self {
        Self {
            evaluators: LinkedHashMap::new(),
            code_manipulator: Box::new(code_manipulator),
        }
    }

    pub fn add(&mut self, key: &str, eval: impl TextMacroEval + 'static) {
        self.evaluators.insert(key.to_owned(), Box::new(eval));
    }

    pub fn translate(
        &self,
        statics: &mut Statics,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&EnhASTFunctionDef>,
        body: &str,
        pre_macro: bool,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Result<String, String> {
        let index = typed_function_def
            .map(|tfd| tfd.index.clone())
            .or_else(|| function_def.map(|fd| fd.index.clone()))
            .unwrap_or(EnhASTIndex::none());

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
        function_def: Option<&EnhASTFunctionDef>,
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
        function_def: Option<&EnhASTFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Result<MacroParam, String> {
        let p = actual_param.trim();

        let context_generic_types = if let Some(f) = function_def {
            let mut result = Vec::new(); //f.generic_types.clone();
                                         // TODO should we use var_types?
            for ((name, _var_types), _t) in f
                .resolved_generic_types
                .clone()
                .remove_generics_prefix()
                .iter()
            {
                if !result.contains(&name) {
                    result.push(name.clone());
                }
            }
            result
        /*} else if let Some(f) = typed_function_def {
        let mut result = Vec::new(); //f.generic_types.clone();
                                     // TODO should we use var_types?
        for ((name, _var_types), _t) in f
            .resolved_generic_types
            .clone()
            .remove_generics_prefix()
            .iter()
        {
            if !result.contains(&name) {
                result.push(name.clone());
            }
        }
        result*/
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
                            &f.resolved_generic_types.clone(), /*.remove_generics_prefix()*/
                            &f.index.id(),
                            Some(&f.original_name),
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
                            panic!("Cannot find parameter {name} : {}", f.index);
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
                        &EnhResolvedGenericTypes::new(),
                        &f.index.id(),
                        Some(&f.original_name),
                    )?;
                    if !f.parameters.iter().any(|it| it.name == par_name) {
                        match &f.body {
                            ASTTypedFunctionBody::RASMBody(_body) => {}
                            ASTTypedFunctionBody::NativeBody(body) => {
                                println!("body\n{body}");
                            }
                        }
                        println!(
                            "parameters {}",
                            SliceDisplay(
                                &f.parameters
                                    .iter()
                                    .map(|it| it.name.clone())
                                    .collect::<Vec<_>>()
                            )
                        );
                        Err(format!("Cannot find parameter {par_name}"))
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
                    &f.resolved_generic_types, /*.remove_generics_prefix()*/
                    &f.index.id(),
                    Some(&f.original_name),
                )?
            } else {
                self.parse_typed_argument(
                    &EnhASTNameSpace::global(), // TODO is it correct?
                    p,
                    None,
                    type_def_provider,
                    &context_generic_types,
                    &EnhResolvedGenericTypes::new(),
                    &EnhModuleId::Other(String::new()), // TODO is it correct?
                    None,                               // TODO is it correct?
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
        ast_type: &EnhASTType,
        function_def: &ASTTypedFunctionDef,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Option<ASTTypedType> {
        if let EnhASTType::Custom {
            namespace: _,
            name,
            param_types,
            index: _,
        } = ast_type
        {
            if let Some(typed_type) = function_def
                .resolved_generic_types
                .clone()
                .remove_generics_prefix()
                .get(name, param_types)
            {
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
                                        EnhASTType::Builtin(EnhBuiltinTypeKind::String)
                                    }
                                    BuiltinTypedTypeKind::I32 => {
                                        EnhASTType::Builtin(EnhBuiltinTypeKind::I32)
                                    }
                                    BuiltinTypedTypeKind::Bool => {
                                        EnhASTType::Builtin(EnhBuiltinTypeKind::Bool)
                                    }
                                    BuiltinTypedTypeKind::Char => {
                                        EnhASTType::Builtin(EnhBuiltinTypeKind::Char)
                                    }
                                    BuiltinTypedTypeKind::F32 => {
                                        EnhASTType::Builtin(EnhBuiltinTypeKind::F32)
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

                let ast_type_to_resolve = EnhASTType::Custom {
                    namespace: ast_type.namespace().clone(),
                    name: name.clone(),
                    param_types: resolved_types,
                    index: EnhASTIndex::none(),
                }
                .fix_namespaces_with(&|ast_type| type_def_provider.get_real_namespace(ast_type));

                type_def_provider.get_ast_typed_type_from_ast_type(&ast_type_to_resolve)
            }
        } else {
            None
        }
    }

    fn parse_typed_argument(
        &self,
        namespace: &EnhASTNameSpace,
        p: &str,
        typed_type: Option<ASTTypedType>,
        type_def_provider: &dyn TypeDefProvider,
        context_generic_types: &[String],
        resolved_generic_types: &EnhResolvedGenericTypes,
        id: &EnhModuleId,
        function_name_for_fix_generics: Option<&str>,
    ) -> Result<(String, Option<EnhASTType>, Option<ASTTypedType>), String> {
        //println!("parse_typed_argument namespace {namespace}, p {p}");
        // TODO the check of :: is a trick since function names could have ::, try to do it better
        let (par_name, par_type, par_typed_type) = if p.contains(':') && !p.contains("::") {
            let vec = p.split(':').collect::<Vec<_>>();
            let par_type_name = vec.get(1).unwrap().trim();
            let par_name = vec.first().unwrap().trim();

            if par_type_name == "i32" {
                (
                    par_name,
                    Some(EnhASTType::Builtin(EnhBuiltinTypeKind::I32)),
                    Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::I32)),
                )
            } else if par_type_name == "str" {
                (
                    par_name,
                    Some(EnhASTType::Builtin(EnhBuiltinTypeKind::String)),
                    Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::String)),
                )
            } else if par_type_name == "f32" {
                (
                    par_name,
                    Some(EnhASTType::Builtin(EnhBuiltinTypeKind::F32)),
                    Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::F32)),
                )
            } else {
                let parser = TypeParserHelper::new(par_type_name);
                // Parser::new(lexer, None);

                let type_parser = TypeParser::new(&parser);

                match type_parser.try_parse_ast_type(0, context_generic_types)? {
                    None => {
                        panic!("Unsupported type {par_type_name}")
                    }
                    Some((ref ast_type, _)) => {
                        //println!("parse_typed_argument {ast_type}");
                        let eh_ast_type = EnhASTType::from_ast(
                            namespace,
                            id,
                            ast_type.clone(),
                            function_name_for_fix_generics,
                        );
                        let t =
                            if let ast::ASTType::Generic(_position, _name, _var_types) = ast_type {
                                if let Some(t) = substitute(&eh_ast_type, resolved_generic_types) {
                                    t
                                } else {
                                    eh_ast_type
                                }
                            } else {
                                eh_ast_type
                            };
                        (
                            par_name,
                            Some(t.clone()),
                            type_def_provider.get_ast_typed_type_from_ast_type(&t),
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
    ) -> EnhASTType {
        match typed_type {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => EnhASTType::Builtin(EnhBuiltinTypeKind::String),
                BuiltinTypedTypeKind::I32 => EnhASTType::Builtin(EnhBuiltinTypeKind::I32),
                BuiltinTypedTypeKind::Bool => EnhASTType::Builtin(EnhBuiltinTypeKind::Bool),
                BuiltinTypedTypeKind::Char => EnhASTType::Builtin(EnhBuiltinTypeKind::Char),
                BuiltinTypedTypeKind::F32 => EnhASTType::Builtin(EnhBuiltinTypeKind::F32),
                BuiltinTypedTypeKind::Lambda {
                    parameters,
                    return_type,
                } => EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
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
            ASTTypedType::Type {
                namespace: _,
                name,
                native_type: _,
                is_ref: _,
            } => type_def_provider
                .get_type_from_typed_type_name(name)
                .unwrap(),
            ASTTypedType::Unit => EnhASTType::Unit,
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
        function_def: Option<&EnhASTFunctionDef>,
        body: &str,
        type_def_provider: &dyn TypeDefProvider,
    ) -> Result<Vec<(TextMacro, usize)>, String> {
        self.get_macros_filter(
            typed_function_def,
            function_def,
            body,
            type_def_provider,
            &|_name, _params| true,
        )
    }

    pub fn get_macros_filter(
        &self,
        typed_function_def: Option<&ASTTypedFunctionDef>,
        function_def: Option<&EnhASTFunctionDef>,
        body: &str,
        type_def_provider: &dyn TypeDefProvider,
        filter: &dyn Fn(&str, &str) -> bool,
    ) -> Result<Vec<(TextMacro, usize)>, String> {
        let index = typed_function_def
            .map(|tfd| tfd.index.clone())
            .or_else(|| function_def.map(|fd| fd.index.clone()))
            .unwrap_or(EnhASTIndex::none());

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

                if !filter(name, parameters) {
                    continue;
                }

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
    fn new(type_str: &str) -> Self {
        let lexer = Lexer::new(type_str.into());
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

pub fn get_type(
    namespace: &EnhASTNameSpace,
    orig_name: &str,
    type_def_provider: &dyn TypeDefProvider,
    function_def_opt: Option<&ASTTypedFunctionDef>,
) -> ASTTypedType {
    if let Some(f) = function_def_opt {
        // TODO we should parse orig_name it could be a type class (M<str>)
        if let Some(t) = f.resolved_generic_types.get(orig_name, &Vec::new()) {
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
            native_type: s.native_type.clone(),
            is_ref: s.is_ref,
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
                    "Error: addRef/deref macro, a typed type must be specified in function {}:{} but got {}: {}",
                    OptionDisplay(&function_def),
                    OptionDisplay(&function_def.map(|it|&it.index)),
                    text_macro.parameters.get(0).unwrap(),
                    text_macro.index
                ),
            };

            let type_name = match ast_typed_type {
                ASTTypedType::Builtin(BuiltinTypedTypeKind::String) => "str".to_string(),
                ASTTypedType::Struct { namespace: _, name } => name.clone(),
                ASTTypedType::Enum { namespace: _, name } => name.clone(),
                ASTTypedType::Type {
                    namespace: _,
                    name,
                    native_type: _,
                    is_ref: _,
                } => name.clone(),
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

pub struct InlineRegistry {
    inline_functions: LinkedHashSet<String>,
}

impl InlineRegistry {
    pub fn add_to_statics(statics: &mut Statics, function: &ASTTypedFunctionDef) {
        if let Some(registry) = statics.any_mut::<InlineRegistry>() {
            registry.inline_functions.insert(function.name.clone());
        } else {
            let mut l = InlineRegistry {
                inline_functions: LinkedHashSet::new(),
            };
            l.inline_functions.insert(function.name.clone());
            statics.add_any(l);
        }
    }

    pub fn is_inline(statics: &Statics, function: &ASTTypedFunctionDef) -> bool {
        if let Some(registry) = statics.any::<InlineRegistry>() {
            registry.inline_functions.contains(&function.name)
        } else {
            false
        }
    }
}

pub struct InlineMacro;

impl InlineMacro {
    pub fn new() -> Self {
        Self
    }
}

impl TextMacroEval for InlineMacro {
    fn eval_macro(
        &self,
        statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        if let Some(f) = function_def {
            InlineRegistry::add_to_statics(statics, &f);
            String::new()
        } else {
            panic!("Cannot use the inline macro outside of a function.")
        }
    }

    fn is_pre_macro(&self) -> bool {
        true
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::asm::code_gen_asm::CodeGenAsm;
    use crate::codegen::c::code_gen_c::CodeGenC;
    use crate::codegen::c::options::COptions;
    use crate::codegen::{AsmOptions, CodeGen};
    use rasm_parser::parser::ast::ASTModifiers;

    use crate::codegen::enh_ast::{
        EnhASTFunctionBody, EnhASTFunctionDef, EnhASTIndex, EnhASTNameSpace, EnhASTParameterDef,
        EnhASTType, EnhBuiltinTypeKind,
    };
    use crate::codegen::statics::{MemoryValue, Statics};
    use crate::codegen::text_macro::{MacroParam, TextMacro};
    use crate::codegen::typedef_provider::DummyTypeDefProvider;
    use crate::enh_type_check::enh_resolved_generic_types::EnhResolvedGenericTypes;
    use crate::enh_type_check::typed_ast::{
        ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedParameterDef, ASTTypedType,
        BuiltinTypedTypeKind, ResolvedGenericTypedTypes,
    };

    #[test]
    fn call() {
        let text_macro = TextMacro {
            index: EnhASTIndex::none(),
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
            &DummyTypeDefProvider::empty(),
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
                &DummyTypeDefProvider::empty(),
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
                &DummyTypeDefProvider::empty(),
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
            namespace: EnhASTNameSpace::global(),
            name: "aFun".into(),
            original_name: "aFun".into(),
            parameters: vec![ASTTypedParameterDef {
                name: "s".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: EnhASTIndex::none(),
            }],
            body: ASTTypedFunctionBody::NativeBody("".into()),
            resolved_generic_types: ResolvedGenericTypedTypes::new(),
            return_type: ASTTypedType::Unit,
            index: EnhASTIndex::none(),
        };

        let result = code_gen()
            .get_text_macro_evaluator()
            .translate(
                &mut statics,
                Some(&function_def),
                None,
                "a line\n$call(println, $s)\nanother line\n",
                false,
                &DummyTypeDefProvider::empty(),
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
            namespace: EnhASTNameSpace::global(),
            name: "aFun".into(),
            original_name: "aFun".into(),
            parameters: vec![ASTTypedParameterDef {
                name: "s".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: EnhASTIndex::none(),
            }],
            body: ASTTypedFunctionBody::NativeBody("".into()),
            resolved_generic_types: ResolvedGenericTypedTypes::new(),
            return_type: ASTTypedType::Unit,
            index: EnhASTIndex::none(),
        };

        let result = code_gen()
            .get_text_macro_evaluator()
            .translate(
                &mut statics,
                Some(&function_def),
                None,
                "a line\n$ccall(printf, $s)\nanother line\n",
                false,
                &DummyTypeDefProvider::empty(),
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
                &DummyTypeDefProvider::empty(),
            )
            .unwrap();

        assert_eq!(result, "mov     eax, 1          ; $call(any)");
    }

    #[test]
    fn test_get_macros() {
        let function_def = ASTTypedFunctionDef {
            namespace: EnhASTNameSpace::global(),
            name: "aFun".into(),
            original_name: "aFun".into(),
            parameters: vec![ASTTypedParameterDef {
                name: "s".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: EnhASTIndex::none(),
            }],
            body: ASTTypedFunctionBody::NativeBody("".into()),
            resolved_generic_types: ResolvedGenericTypedTypes::new(),
            return_type: ASTTypedType::Unit,
            index: EnhASTIndex::none(),
        };

        let macros = code_gen()
            .get_text_macro_evaluator()
            .get_macros(
                Some(&function_def),
                None,
                "$call(slen, $s)",
                &DummyTypeDefProvider::empty(),
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
                &DummyTypeDefProvider::empty(),
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

        let function_def = EnhASTFunctionDef {
            name: "aFun".into(),
            original_name: "aFun".into(),
            parameters: vec![EnhASTParameterDef {
                name: "s".into(),
                ast_type: EnhASTType::Builtin(EnhBuiltinTypeKind::String),
                ast_index: EnhASTIndex::none(),
            }],
            body: EnhASTFunctionBody::NativeBody("".into()),
            generic_types: Vec::new(),
            return_type: EnhASTType::Unit,
            index: EnhASTIndex::none(),
            resolved_generic_types: EnhResolvedGenericTypes::new(),
            modifiers: ASTModifiers::private(),
            namespace: EnhASTNameSpace::global(),
            rank: 0,
            target: None,
        };

        let result = code_gen.get_text_macro_evaluator().get_macros(
            None,
            Some(&function_def),
            "$call(List_0_addRef,$s:i32)",
            &DummyTypeDefProvider::empty(),
        );

        assert_eq!(
            "Ok([(TextMacro { name: \"call\", parameters: [Plain(\"List_0_addRef\", None, None), Ref(\"$s\", Some(Builtin(I32)), None)], index: EnhASTIndex { file_name: None, row: 0, column: 0, builtin: None } }, 0)])",
            &format!("{:?}", result),
        );
    }

    fn code_gen() -> CodeGenAsm {
        CodeGenAsm::new(AsmOptions::default(), false)
    }

    #[test]
    fn parse_param() {
        let function_def = ASTTypedFunctionDef {
            namespace: EnhASTNameSpace::global(),
            name: "f".to_string(),
            original_name: String::new(),
            parameters: vec![ASTTypedParameterDef {
                name: "par".to_string(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::I32),
                ast_index: EnhASTIndex::none(),
            }],
            return_type: ASTTypedType::Unit,
            body: ASTTypedFunctionBody::RASMBody(Vec::new()),
            resolved_generic_types: ResolvedGenericTypedTypes::new(),
            index: EnhASTIndex::none(),
        };

        let result = CodeGenC::new(COptions::default(), false)
            .get_text_macro_evaluator()
            .get_macros(
                Some(&function_def),
                None,
                "($call(aFun,$par))->value",
                &DummyTypeDefProvider::empty(),
            )
            .unwrap();

        assert_eq!(
            format!("{}", result.get(0).unwrap().0),
            "$call(Plain(aFun, None, None), Ref($par, Some(i32), Some(i32)))"
        );
    }
}
