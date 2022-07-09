use std::path::Path;
use log::debug;
use crate::lexer::Lexer;

use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind};
use crate::parser::asm_def_parser::AsmDefParser;
use crate::parser::ast::{ASTEnumDef, ASTExpression, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTModule, ASTParameterDef, ASTType, ASTTypeRef, BuiltinTypeKind};
use crate::parser::ast::ASTFunctionBody::{ASMBody, RASMBody};
use crate::parser::ast::ASTType::Builtin;
use crate::parser::enum_parser::EnumParser;
use crate::parser::type_parser::TypeParser;

pub(crate) mod ast;
mod enum_parser;
mod type_params_parser;
#[cfg(test)]
mod test_utils;
mod asm_def_parser;
mod type_parser;
mod tokens_matcher;
mod tokens_group;
mod matchers;

enum ProcessResult {
    Continue,
    Next,
    Panic,
}

pub struct Parser {
    tokens: Vec<Token>,
    body: Vec<ASTFunctionCall>,
    functions: Vec<ASTFunctionDef>,
    i: usize,
    parser_data: Vec<ParserData>,
    state: Vec<ParserState>,
    included_functions: Vec<ASTFunctionDef>,
    enums: Vec<ASTEnumDef>,
}

#[derive(Clone, Debug)]
enum ParserData {
    FunctionCall(ASTFunctionCall),
    FunctionDef(ASTFunctionDef),
    FunctionDefParameter(ASTParameterDef),
    EnumDef(ASTEnumDef),
    LambdaDef(ASTLambdaDef),
    Val(String)
}

#[derive(Clone, Debug)]
enum ParserState {
    FunctionCall,
    FunctionDef,
    FunctionBody,
    FunctionDefParameter,
    FunctionDefReturnType,
    EnumDef,
    Val
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let tokens = lexer.filter(|it| {
            !matches!(it.kind, TokenKind::WhiteSpaces(_) | TokenKind::Comment(_) | TokenKind::EndOfLine)
        }).collect();
        Self {
            tokens,
            body: Vec::new(),
            functions: Vec::new(),
            i: 0,
            parser_data: Vec::new(),
            state: Vec::new(),
            included_functions: Vec::new(),
            enums: Vec::new(),
        }
    }

    pub fn parse(&mut self, path: &Path) -> ASTModule {
        self.body = Vec::new();
        self.functions = Vec::new();
        self.i = 0;
        self.parser_data = Vec::new();
        self.state = Vec::new();

        let last_token = Token::new(TokenKind::EndOfLine, 0, 0);

        while self.i <= self.tokens.len() {
            let token = if self.i == self.tokens.len() {
                last_token.clone()
            } else {
                self.tokens.get(self.i).unwrap().clone()
            };

            self.debug("");
            debug!("");

            match self.get_state() {
                None => {
                    if !self.parser_data.is_empty() {
                        self.panic("Error");
                    }
                    if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        self.parser_data.push(ParserData::FunctionCall(ASTFunctionCall {
                            function_name,
                            parameters: Vec::new(),
                        }));
                        self.state.push(ParserState::FunctionCall);
                        self.i = next_i;
                        continue;
                    } else if let Some((name, param_types, next_i)) = self.try_parse_function_def() {
                        self.parser_data.push(ParserData::FunctionDef(ASTFunctionDef {
                            name,
                            parameters: Vec::new(),
                            body: RASMBody(Vec::new()),
                            return_type: None,
                            inline: false,
                            param_types,
                        }));
                        self.state.push(ParserState::FunctionDef);
                        self.state.push(ParserState::FunctionDefParameter);
                        self.i = next_i;
                        continue;
                    } else if let Some((name, inline, param_types, next_i)) = AsmDefParser::new(self).try_parse() {
                        self.parser_data.push(ParserData::FunctionDef(ASTFunctionDef {
                            name,
                            parameters: Vec::new(),
                            body: ASMBody("".into()),
                            return_type: None,
                            inline,
                            param_types,
                        }));
                        self.state.push(ParserState::FunctionDef);
                        self.state.push(ParserState::FunctionDefParameter);
                        self.i = next_i;
                        continue;
                    } else if let Some((resource, next_i)) = self.try_parse_include() {
                        let buf = path.with_file_name(resource);
                        let resource_path = buf.as_path();
                        if let Ok(lexer) = Lexer::from_file(resource_path) {
                            let mut parser = Parser::new(lexer);
                            let mut module = parser.parse(resource_path);
                            if !module.body.is_empty() {
                                self.panic(&format!("Cannot include a module with a body: {:?}.", module.body));
                            }
                            self.included_functions.append(&mut module.functions);
                            self.enums.append(&mut module.enums);
                        }
                        self.i = next_i;
                        continue;
                    } else if let Some((name, type_params, next_i)) = EnumParser::new(self).try_parse() {
                        self.parser_data.push(ParserData::EnumDef(ASTEnumDef { name, type_parameters: type_params, variants: Vec::new() }));
                        self.state.push(ParserState::EnumDef);
                        self.i = next_i;
                        continue;
                    } else if let TokenKind::EndOfLine = token.kind {
                        break;
                    }
                    self.panic("Unknown statement");
                }
                Some(ParserState::FunctionCall) => {
                    match self.process_function_call(token) {
                        ProcessResult::Next => {}
                        ProcessResult::Continue => {
                            continue;
                        }
                        ProcessResult::Panic => {
                            self.panic("Error parsing function call");
                        }
                    }
                }
                Some(ParserState::FunctionDef) => {
                    match self.process_function_def(token) {
                        ProcessResult::Next => {}
                        ProcessResult::Continue => {
                            continue;
                        }
                        ProcessResult::Panic => {
                            self.panic("Error parsing function definition");
                        }
                    }
                }
                Some(ParserState::FunctionDefParameter) => {
                    if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
                        if let Some((name, next_i)) = self.try_parse_parameter_def_name() {
                            self.i = next_i;
                            if let Some((type_ref, next_i)) = TypeParser::new(self).try_parse_type_ref(0, &def.param_types) {
                                self.i = next_i;
                                self.parser_data.push(ParserData::FunctionDefParameter(ASTParameterDef { name, type_ref, from_context: false }));
                                self.state.pop();
                                continue;
                            } else {
                                self.panic("");
                            }
                        } else {
                            self.state.pop();
                            continue;
                        }
                    } else {
                        self.panic("Illegal state");
                    }
                }
                Some(ParserState::FunctionBody) => {
                    if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close) = token.kind {
                        self.state.pop();
                        self.i += 1;
                        continue;
                    } else if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        self.parser_data.push(ParserData::FunctionCall(ASTFunctionCall {
                            function_name,
                            parameters: Vec::new(),
                        }));
                        self.state.push(ParserState::FunctionCall);
                        self.i = next_i;
                        continue;
                    } else if let Some((val_name, next_i)) = self.try_parse_reference_to_val() {
                        self.parser_data.push(ParserData::Val(val_name));
                        self.state.push(ParserState::Val);
                        self.i = next_i;
                        continue;
                    }
                    self.panic("Error parsing function body.");
                }
                Some(ParserState::FunctionDefReturnType) => {
                    if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
                        if let Some((type_ref, next_i)) = TypeParser::new(self).try_parse_type_ref(0, &def.param_types) {
                            self.i = next_i;

                            let mut def = def.clone();
                            self.i = next_i;
                            def.return_type = Some(type_ref);
                            let l = self.parser_data.len();
                            self.parser_data[l - 1] = ParserData::FunctionDef(def);
                            self.state.pop();
                            continue;
                        }
                    } else {
                        self.panic("Illegal state.");
                    }
                }
                Some(ParserState::EnumDef) => {
                    if let Some(ParserData::EnumDef(mut def)) = self.last_parser_data() {
                        if let Some((variants, next_i)) = EnumParser::new(self).parse_variants(&def.type_parameters, 0) {
                            def.variants = variants;
                            self.enums.push(def);
                            self.state.pop();
                            self.parser_data.pop();
                            self.i = next_i;
                            continue;
                        } else {
                            panic!("Expected variants.")
                        }
                    } else {
                        panic!("Expected enum data.")
                    }
                }
                Some(ParserState::Val) => {
                    if let Some(ParserData::Val(val_name)) = self.last_parser_data() {
                        if let Some(ParserData::FunctionDef(def)) = self.before_last_parser_data() {
                            debug!("Found val {} in function {}", val_name, def.name);
                            let mut def = def.clone();
                            if let RASMBody(mut calls) = def.body {
                                calls.push(ASTExpression::Val(val_name));
                                def.body = RASMBody(calls);
                                let l = self.parser_data.len();
                                self.parser_data[l - 2] = ParserData::FunctionDef(def);
                                self.parser_data.pop();
                                self.state.pop();
                                continue;
                            } else {
                                panic!("expected rasm body, found {:?}", def.body);
                            }
                        } else if let Some(ParserData::LambdaDef(def)) = self.before_last_parser_data() {
                            let mut def = def.clone();
                            if let RASMBody(mut calls) = def.body {
                                calls.push(ASTExpression::Val(val_name));
                                def.body = RASMBody(calls);
                                let l = self.parser_data.len();
                                self.parser_data[l - 2] = ParserData::LambdaDef(def);
                                self.parser_data.pop();
                                self.state.pop();
                                continue;
                            } else {
                                panic!("expected rasm body, found {:?}", def.body);
                            }
                        } else {
                            panic!("Function def, found {:?}", self.before_last_parser_data());
                        }
                    }
                    panic!("Expected val name, found {:?}", self.last_parser_data());
                }
            }

            self.i += 1;

            //actual.push(token);
        }

        self.functions.append(&mut self.included_functions);

        //println!("ebums: \n{:?}", self.enums);

        ASTModule { body: self.body.clone(), functions: self.functions.clone(), enums: self.enums.clone() }
    }

    fn process_function_call(&mut self, token: Token) -> ProcessResult {
        if let Some(ParserData::FunctionCall(call)) = self.last_parser_data() {
            if let TokenKind::StringLiteral(value) = &token.kind {
                self.add_parameter_to_call_and_update_parser_data(call, ASTExpression::StringLiteral(value.clone()));
                return ProcessResult::Continue;
            } else if let TokenKind::Number(value) = &token.kind {
                self.add_parameter_to_call_and_update_parser_data(call, ASTExpression::Number(value.parse().unwrap()));
                return ProcessResult::Continue;
            } else if let TokenKind::AlphaNumeric(name) = &token.kind {
                if let Some((function_name, next_i)) = self.try_parse_function_call() {
                    self.parser_data.push(ParserData::FunctionCall(ASTFunctionCall {
                        function_name,
                        parameters: Vec::new(),
                    }));
                    self.state.push(ParserState::FunctionCall);
                    self.i = next_i;
                } else {
                    self.add_parameter_to_call_and_update_parser_data(call, ASTExpression::Val(name.clone()));
                }
                return ProcessResult::Continue;
            } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
                self.i += 1;
                return ProcessResult::Continue;
            } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
                let (parameter_names, next_i) = self.parse_lambda_parameters(1);
                self.parser_data.push(ParserData::LambdaDef(ASTLambdaDef {
                    parameter_names,
                    body: RASMBody(Vec::new()),
                }));
                self.state.push(ParserState::FunctionBody);
                self.i = next_i;
                return ProcessResult::Continue;
            } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
                if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) = self.get_token_kind_n(1) {
                    if let Some(ParserData::FunctionDef(def)) = self.before_last_parser_data() {
                        let mut def = def.clone();
                        if let RASMBody(mut calls) = def.body {
                            calls.push(ASTExpression::ASTFunctionCallExpression(call));
                            def.body = RASMBody(calls);
                            let l = self.parser_data.len();
                            self.parser_data[l - 2] = ParserData::FunctionDef(def);
                            self.parser_data.pop();
                            self.i += 2;
                            self.state.pop();
                            return ProcessResult::Continue;
                        }
                    } else if let Some(ParserData::LambdaDef(def)) = self.before_last_parser_data() {
                        let mut def = def.clone();
                        if let RASMBody(mut calls) = def.body {
                            calls.push(ASTExpression::ASTFunctionCallExpression(call));
                            def.body = RASMBody(calls);
                            let l = self.parser_data.len();
                            self.parser_data[l - 2] = ParserData::LambdaDef(def);
                            self.parser_data.pop();
                            self.i += 2;
                            self.state.pop();
                            return ProcessResult::Continue;
                        }
                    } else {
                        self.body.push(call);
                    }
                    self.parser_data.pop();
                    self.state.pop();
                    self.i += 2;
                    return ProcessResult::Continue;
                } else if let Some(ParserData::FunctionCall(before_call)) = self.before_last_parser_data() {
                    let mut before_call = before_call.clone();
                    before_call.parameters.push(ASTExpression::ASTFunctionCallExpression(call));
                    let l = self.parser_data.len();
                    self.parser_data[l - 2] = ParserData::FunctionCall(before_call);
                    self.parser_data.pop();
                    self.state.pop();
                    self.i += 1;
                    return ProcessResult::Continue;
                }
            }
        } else if let Some(ParserData::LambdaDef(def)) = self.last_parser_data() {
            if let Some(ParserData::FunctionCall(call)) = self.before_last_parser_data() {
                let mut actual_call = call.clone();
                actual_call.parameters.push(ASTExpression::Lambda(def));
                let l = self.parser_data.len();
                self.parser_data[l - 2] = ParserData::FunctionCall(actual_call);
                self.parser_data.pop();
                return ProcessResult::Continue;
            }
        }
        ProcessResult::Panic
    }

    fn process_function_def(&mut self, token: Token) -> ProcessResult {
        if let Some(ParserData::FunctionDefParameter(param_def)) = self.last_parser_data() {
            if let Some(ParserData::FunctionDef(def)) = self.before_last_parser_data() {
                let mut def = def.clone();
                def.parameters.push(param_def);
                let l = self.parser_data.len();
                self.parser_data[l - 2] = ParserData::FunctionDef(def);
                self.parser_data.pop();
                return ProcessResult::Continue;
            }
        } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
            if let Some(ParserData::FunctionDef(mut def)) = self.last_parser_data() {
                if let Some((ref type_ref, next_i)) = TypeParser::new(self).try_parse_type_ref(0, &def.param_types) {
                    self.i = next_i;
                    def.return_type = Some(type_ref.clone());
                } else {
                    def.return_type = None
                }
                let l = self.parser_data.len();
                self.parser_data[l - 1] = ParserData::FunctionDef(def);
                self.i += 1;
                return ProcessResult::Continue;
            } else {
                self.panic("");
                panic!();
            };
        } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
            self.state.push(ParserState::FunctionBody);
        } else if let TokenKind::AsmBLock(body) = &token.kind {
            if let Some(ParserData::FunctionDef(mut def)) = self.last_parser_data() {
                def.body = ASMBody(body.into());
                self.functions.push(def);
                self.parser_data.pop();
                self.state.pop();
            }
        } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
            self.state.push(ParserState::FunctionDefParameter);
        } else if let TokenKind::Punctuation(PunctuationKind::RightArrow) = token.kind {
            self.state.push(ParserState::FunctionDefReturnType);
        } else if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
            self.functions.push(def);
            self.parser_data.pop();
            self.state.pop();
            return ProcessResult::Continue;
        } else {
            return ProcessResult::Panic;
        }
        ProcessResult::Next
    }

    fn add_parameter_to_call_and_update_parser_data(&mut self, mut call: ASTFunctionCall, ast_expression: ASTExpression) {
        call.parameters.push(ast_expression);
        let l = self.parser_data.len();
        self.parser_data[l - 1] = ParserData::FunctionCall(call);
        self.i += 1;
    }

    pub fn print(module: &ASTModule) {
        println!("main() {{");
        module.body.iter().for_each(|call| {
            print!("  ");
            Self::print_call(call, false);
        });
        println!("}}");
        module.functions.iter().for_each(|f| {
            Self::print_function_def(f)
        })
    }

    pub fn print_function_def(f: &ASTFunctionDef) {
        match &f.body {
            RASMBody(_) => print!("fn {}(", f.name),
            ASMBody(_) => print!("asm {}(", f.name)
        }
        let mut first = true;
        f.parameters.iter().for_each(|p| {
            if !first {
                print!(",");
            }
            print!("{}:", p.name);
            let type_ref = &p.type_ref;
            Self::print_type_ref(type_ref);
            first = false;
        });
        print!(")");
        if let Some(return_type) = &f.return_type {
            print!(" -> ");
            Self::print_type_ref(return_type);
        }
        match &f.body {
            RASMBody(expressions) => {
                println!(" {{");
                expressions.iter().for_each(|call| {
                    match call {
                        ASTExpression::StringLiteral(s) => {
                            println!("\"{}\";", s);
                        }
                        ASTExpression::ASTFunctionCallExpression(call) => {
                            Self::print_call(call, false)
                        }
                        ASTExpression::Val(name) => {
                            println!("{};", name);
                        }
                        ASTExpression::Number(n) => {
                            println!("{};", n);
                        }
                        ASTExpression::Lambda(_) => {
                            println!("{{ -> ... }};");
                        }
                    }
                    print!("  ");
                });
                println!("}}");
            }
            ASMBody(_) => println!(" {{...}}")
        }
    }

    fn print_type_ref(type_ref: &ASTTypeRef) {
        if type_ref.ast_ref {
            print!("&");
        }
        match &type_ref.ast_type {
            Builtin(bt) => {
                match bt {
                    BuiltinTypeKind::ASTString => print!("str"),
                    BuiltinTypeKind::ASTI32 => print!("i32"),
                    BuiltinTypeKind::Lambda{ return_type: _return_type, parameters: _parameters} => print!("fn"), // TODO
                }
            }
            ASTType::Parametric(name) => print!("{}", name),
            // TODO do it better
            ASTType::Custom { name, param_types } => print!("{}<{:?}>", name, param_types)
        }
    }

    fn get_state(&self) -> Option<&ParserState> {
        self.state.last()
    }

    fn print_call(call: &ASTFunctionCall, as_expression: bool) {
        print!("{}(", call.function_name);
        let mut first = true;
        call.parameters.iter().for_each(|par| {
            if !first {
                print!(",");
            }
            match par {
                ASTExpression::StringLiteral(s) => print!("\"{}\"", s),
                ASTExpression::Number(n) => print!("{}", n),
                ASTExpression::ASTFunctionCallExpression(call) => Self::print_call(call, true),
                ASTExpression::Val(name) => print!("{}", name),
                ASTExpression::Lambda(function_def) => {
                    if let RASMBody(calls) = &function_def.body {
                        print!("{{");
                        calls.iter().for_each(|call| {
                            // TODO
                            //Self::print_call(call, true);
                            print!(";");
                        });
                        print!("}}");
                    } else {
                        panic!("A lambda cannot be an asm function.");
                    }
                }
            }
            first = false;
        });
        if as_expression {
            print!(")");
        } else {
            println!(");");
        }
    }

    fn error_msg(&self, message: &str) -> String {
        let option = self.tokens.get(self.i);

        if let Some(token) = option {
            //self.debug(message);
            format!("{}: {},{}", message, token.row, token.column)
        } else {
            "it wasn't supposed to happen!".into()
        }
    }

    fn panic(&self, message: &str) {
        self.debug(message);
        panic!("{}", self.error_msg(message));
    }

    fn debug(&self, message: &str) {
        debug!("{}", message);
        debug!("i {}", self.i);
        debug!("token {:?}", self.get_token());
        debug!("state {:?}", self.state);
        debug!("data {:?}", self.parser_data);
        //println!("body {:?}", self.body);
        //println!("functions {:?}", self.functions);
    }

    fn try_parse_function_call(&self) -> Option<(String, usize)> {
        // println!("try_parse_function_call {:?}", self.get_token_kind());
        if let Some(TokenKind::AlphaNumeric(function_name)) = self.get_token_kind() {
            if let Some((variant, next_i)) = self.try_parse_enum_constructor() {
                return Some((function_name.clone() + "::" + &variant, next_i));
            } else if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) = self.get_token_kind_n(1) {
                //println!("found function call {}", function_name);
                return Some((function_name.clone(), self.i + 2));
            }
        }
        None
    }

    fn try_parse_enum_constructor(&self) -> Option<(String, usize)> {
        if let (Some(TokenKind::Punctuation(PunctuationKind::Colon)), Some(TokenKind::Punctuation(PunctuationKind::Colon)), Some(TokenKind::AlphaNumeric(variant)),
            Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open))) =
        (self.get_token_kind_n(1), self.get_token_kind_n(2), self.get_token_kind_n(3), self.get_token_kind_n(4)) {
            Some((variant.clone(), self.i + 5))
        } else {
            None
        }
    }

    // TODO type params
    fn try_parse_function_def(&self) -> Option<(String, Vec<String>, usize)> {
        if let Some(TokenKind::KeyWord(KeywordKind::Fn)) = self.get_token_kind() {
            if let Some(Token { kind: TokenKind::AlphaNumeric(function_name), row: _, column: _ }) = self.next_token() {
                if let Some(Token { kind: TokenKind::Bracket(BracketKind::Round, BracketStatus::Open), row: _, column: _ }) = self.next_token2() {
                    return Some((function_name.clone(), Vec::new(), self.i + 3));
                }
            }
        }
        None
    }

    fn try_parse_include(&self) -> Option<(String, usize)> {
        if let Some(TokenKind::KeyWord(KeywordKind::Include)) = self.get_token_kind() {
            if let Some(next_token) = self.next_token() {
                if let TokenKind::StringLiteral(include) = &next_token.kind {
                    return Some((include.into(), self.i + 2));
                }
            } else {
                self.panic("Error parsing include");
            }
        }
        None
    }

    fn try_parse_parameter_def_name(&self) -> Option<(String, usize)> {
        if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind() {
            if let Some(TokenKind::Punctuation(PunctuationKind::Colon)) = self.get_token_kind_n(1) {
                return Some((name.into(), self.i + 2));
            }
        }
        None
    }

    fn next_token(&self) -> Option<&Token> {
        self.tokens.get(self.i + 1)
    }

    fn next_token2(&self) -> Option<&Token> {
        self.tokens.get(self.i + 2)
    }

    fn before_last_parser_data(&self) -> Option<&ParserData> {
        let i = self.parser_data.len();
        if i > 1 {
            self.parser_data.get(i - 2)
        } else {
            None
        }
    }

    fn last_parser_data(&self) -> Option<ParserData> {
        self.parser_data.last().cloned()
    }

    fn parse_lambda_parameters(&self, out_n: usize) -> (Vec<String>, usize) {
        let mut n = out_n;
        let mut parameter_names = Vec::new();
        loop {
            let kind_o = self.get_token_kind_n(n);
            match kind_o {
                None => {
                    self.panic("No token parsing lambda parameters");
                }
                Some(TokenKind::Punctuation(PunctuationKind::RightArrow)) => {
                    n += 1;
                    break;
                }
                Some(TokenKind::Punctuation(PunctuationKind::Comma)) => {
                    n += 1;
                    continue;
                }
                Some(TokenKind::AlphaNumeric(name)) => {
                    parameter_names.push(name.to_string());
                    n += 1;
                    continue;
                }
                _ => {
                    self.panic(&format!("Unexpected token {:?}", kind_o.unwrap()));
                }
            }
        }
        (parameter_names, self.i + n)
    }

    fn try_parse_reference_to_val(&self) -> Option<(String, usize)> {
        if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind() {
            if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) = self.get_token_kind_n(1) {
                return Some((name.clone(), self.get_i() + 2));
            }
        }
        None
    }
}

impl ParserTrait for Parser {
    fn get_i(&self) -> usize {
        self.i
    }

    fn get_token_n(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.i + n)
    }

    fn panic(&self, message: &str) {
        self.panic(message);
    }
}

pub trait ParserTrait {
    fn get_i(&self) -> usize;
    fn get_token_n(&self, n: usize) -> Option<&Token>;
    fn panic(&self, message: &str);
    fn get_token_kind(&self) -> Option<&TokenKind> {
        self.get_token_kind_n(0)
    }
    fn get_token_kind_n(&self, n: usize) -> Option<&TokenKind> {
        self.get_token_n(n).map(|token| &token.kind)
    }
    fn get_token(&self) -> Option<&Token> {
        self.get_token_n(0)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::lexer::Lexer;
    use crate::parser::ast::ASTExpression;
    use crate::parser::Parser;

    #[test]
    fn test() {
        let path = Path::new("resources/test/helloworld.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        parser.parse(path);
    }

    #[test]
    fn test2() {
        let path = Path::new("resources/test/test2.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse(path);

        assert_eq!(1, module.body.len());
    }

    #[test]
    fn test8() {
        let path = Path::new("resources/test/test8.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse(path);

        assert!(!module.functions.is_empty());
        assert_eq!(2, module.functions.get(0).unwrap().parameters.len());
    }

    #[test]
    fn test9() {
        let path = Path::new("resources/test/test9.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse(path);

        assert!(!module.body.is_empty());
        assert_eq!(1, module.body.get(0).unwrap().parameters.len());

        let nprint_parameter = module.body.get(0).unwrap().parameters.get(0);

        if let Some(ASTExpression::ASTFunctionCallExpression(call)) = nprint_parameter {
            assert_eq!("nadd", call.function_name);
            assert_eq!(2, call.parameters.len());
            if let Some(ASTExpression::Number(n)) = call.parameters.get(0) {
                assert_eq!(10, *n);
            } else {
                panic!();
            }
            if let Some(ASTExpression::Number(n)) = call.parameters.get(1) {
                assert_eq!(20, *n);
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test10() {
        let path = Path::new("resources/test/test10.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        parser.parse(path);
    }

    #[test]
    fn test11() {
        let path = Path::new("resources/test/test11.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        parser.parse(path);
    }
}
