use std::collections::HashSet;
use std::path::Path;

use linked_hash_map::LinkedHashMap;
use log::{debug, info};

use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::lexer::tokens::{
    BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind,
};
use crate::lexer::Lexer;
use crate::parser::asm_def_parser::AsmDefParser;
use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
use crate::parser::ast::ASTFunctionBody::{ASMBody, RASMBody};
use crate::parser::ast::ASTStatement::LetStatement;
use crate::parser::ast::{
    ASTEnumDef, ASTExpression, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTLambdaDef, ASTModule,
    ASTParameterDef, ASTStatement, ASTStructDef, ASTTypeDef,
};
use crate::parser::enum_parser::EnumParser;
use crate::parser::matchers::param_types_matcher;
use crate::parser::struct_parser::StructParser;
use crate::parser::tokens_matcher::{TokensMatcher, TokensMatcherTrait};
use crate::parser::type_params_parser::TypeParamsParser;
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserState::StructDef;

mod asm_def_parser;
pub(crate) mod ast;
mod enum_parser;
mod matchers;
mod struct_parser;
#[cfg(test)]
mod test_utils;
mod tokens_group;
pub mod tokens_matcher;
mod type_params_parser;
pub mod type_parser;

enum ProcessResult {
    Continue,
    Next,
    Panic(String),
}

pub struct Parser {
    tokens: Vec<Token>,
    body: Vec<ASTStatement>,
    functions: Vec<ASTFunctionDef>,
    i: usize,
    parser_data: Vec<ParserData>,
    state: Vec<ParserState>,
    included_functions: Vec<ASTFunctionDef>,
    enums: Vec<ASTEnumDef>,
    structs: Vec<ASTStructDef>,
    file_name: Option<String>,
    requires: HashSet<String>,
    externals: HashSet<String>,
    types: Vec<ASTTypeDef>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueType {
    Boolean(bool),
    Number(i32),
    Char(char),
}

#[derive(Clone, Debug)]
enum ParserData {
    EnumDef(ASTEnumDef),
    FunctionCall(ASTFunctionCall),
    FunctionDef(ASTFunctionDef),
    FunctionDefParameter(ASTParameterDef),
    LambdaDef(ASTLambdaDef),
    Let(String),
    Val(ASTExpression, ASTIndex),
    StructDef(ASTStructDef),
}

#[derive(Clone, Debug)]
enum ParserState {
    EnumDef,
    FunctionCall,
    FunctionDef,
    FunctionBody,
    FunctionDefParameter,
    FunctionDefReturnType,
    Let,
    Val,
    StructDef,
}

impl Parser {
    pub fn new(lexer: Lexer, file_name: Option<String>) -> Self {
        let tokens = lexer
            .filter(|it| {
                !matches!(
                    it.kind,
                    TokenKind::WhiteSpaces(_) | TokenKind::Comment(_) | TokenKind::EndOfLine
                )
            })
            .collect();
        Self {
            tokens,
            body: Vec::new(),
            functions: Vec::new(),
            i: 0,
            parser_data: Vec::new(),
            state: Vec::new(),
            included_functions: Vec::new(),
            enums: Vec::new(),
            structs: Vec::new(),
            file_name,
            requires: HashSet::new(),
            externals: HashSet::new(),
            types: Vec::new(),
        }
    }

    pub fn parse(&mut self, path: &Path) -> ASTModule {
        self.body = Vec::new();
        self.functions = Vec::new();
        self.i = 0;
        self.parser_data = Vec::new();
        self.state = Vec::new();

        let std_path = Path::new("resources/stdlib");

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
                        self.parser_data
                            .push(ParserData::FunctionCall(ASTFunctionCall {
                                original_function_name: function_name.clone(),
                                function_name,
                                parameters: Vec::new(),
                                index: self.get_index(0).unwrap(),
                            }));
                        self.state.push(ParserState::FunctionCall);
                        self.i = next_i;
                        continue;
                    } else if let Some((name, param_types, next_i)) = self.try_parse_function_def()
                    {
                        self.parser_data
                            .push(ParserData::FunctionDef(ASTFunctionDef {
                                original_name: name.clone(),
                                name,
                                parameters: Vec::new(),
                                body: RASMBody(Vec::new()),
                                return_type: None,
                                inline: false,
                                param_types,
                                resolved_generic_types: LinkedHashMap::new(),
                            }));
                        self.state.push(ParserState::FunctionDef);
                        self.state.push(ParserState::FunctionDefParameter);
                        self.i = next_i;
                        continue;
                    } else if let Some((name, inline, param_types, next_i)) =
                        AsmDefParser::new(self).try_parse()
                    {
                        self.parser_data
                            .push(ParserData::FunctionDef(ASTFunctionDef {
                                original_name: name.clone(),
                                name,
                                parameters: Vec::new(),
                                body: ASMBody("".into()),
                                return_type: None,
                                inline,
                                param_types,
                                resolved_generic_types: LinkedHashMap::new(),
                            }));
                        self.state.push(ParserState::FunctionDef);
                        self.state.push(ParserState::FunctionDefParameter);
                        self.i = next_i;
                        continue;
                    } else if let Some((resource, next_i)) = self.try_parse_include() {
                        let mut buf = path.with_file_name(&resource);
                        // First we try to get it relative to the current file,
                        // then we try to get the file from the standard lib folder
                        if !buf.exists() {
                            buf = std_path.join(Path::new(&resource));
                        }
                        info!("include {}", buf.as_path().to_str().unwrap());

                        let resource_path = buf.as_path();

                        match Lexer::from_file(resource_path) {
                            Ok(lexer) => {
                                let mut parser = Parser::new(
                                    lexer,
                                    resource_path.to_str().map(|it| it.to_string()),
                                );
                                let mut module = parser.parse(resource_path);
                                if !module.body.is_empty() {
                                    self.panic(&format!(
                                        "Cannot include a module with a body: {:?}.",
                                        module.body
                                    ));
                                }
                                self.included_functions.append(&mut module.functions);
                                self.enums.append(&mut module.enums);
                                self.structs.append(&mut module.structs);
                                self.requires.extend(module.requires);
                                self.externals.extend(module.externals);
                                self.types.extend(module.types);
                            }
                            Err(err) => {
                                self.panic(&format!(
                                    "Error running lexer for {:?}: {err}",
                                    resource_path.to_str()
                                ));
                            }
                        }

                        self.i = next_i;
                        continue;
                    } else if let Some((name, type_params, next_i)) =
                        EnumParser::new(self).try_parse()
                    {
                        self.parser_data.push(ParserData::EnumDef(ASTEnumDef {
                            name,
                            type_parameters: type_params,
                            variants: Vec::new(),
                        }));
                        self.state.push(ParserState::EnumDef);
                        self.i = next_i;
                        continue;
                    } else if let Some((name, type_params, next_i)) =
                        StructParser::new(self).try_parse()
                    {
                        self.parser_data.push(ParserData::StructDef(ASTStructDef {
                            name,
                            type_parameters: type_params,
                            properties: Vec::new(),
                        }));
                        self.state.push(StructDef);
                        self.i = next_i;
                        continue;
                    } else if let TokenKind::KeyWord(KeywordKind::Requires) = token.kind {
                        if let Some(TokenKind::StringLiteral(name)) = &self.get_token_kind_n(1) {
                            self.requires.insert(name.clone());
                            self.i += 2;
                            continue;
                        }
                        self.panic("Cannot parse require")
                    } else if let TokenKind::KeyWord(KeywordKind::Extern) = token.kind {
                        if let Some(TokenKind::StringLiteral(name)) = self.get_token_kind_n(1) {
                            self.externals.insert(name.clone());
                            self.i += 2;
                            continue;
                        }
                        self.panic("Cannot parse external")
                    } else if let TokenKind::EndOfLine = token.kind {
                        break;
                    } else if let Some((name, next_i)) = self.try_parse_let() {
                        self.parser_data.push(ParserData::Let(name));
                        self.state.push(ParserState::Let);
                        self.i = next_i;
                        continue;
                    } else if let TokenKind::KeyWord(KeywordKind::Type) = token.kind {
                        let (type_def, next_i) = self.parse_type_def();
                        if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
                            self.get_token_kind_n(next_i - self.i)
                        {
                            self.types.push(type_def);
                            self.i = next_i + 1;
                            continue;
                        } else {
                            self.panic("Missing semicolon)")
                        }
                    }
                    self.panic("Unknown statement");
                }
                Some(ParserState::FunctionCall) => match self.process_function_call(token) {
                    ProcessResult::Next => {}
                    ProcessResult::Continue => {
                        continue;
                    }
                    ProcessResult::Panic(message) => {
                        self.panic(&format!("Error {}: ", message));
                    }
                },
                Some(ParserState::FunctionDef) => match self.process_function_def(token) {
                    ProcessResult::Next => {}
                    ProcessResult::Continue => {
                        continue;
                    }
                    ProcessResult::Panic(message) => {
                        self.panic(&format!("Error {}: ", message));
                    }
                },
                Some(ParserState::FunctionDefParameter) => {
                    if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
                        if let Some((name, next_i)) = self.try_parse_parameter_def_name() {
                            self.i = next_i;
                            if let Some((ast_type, next_i)) =
                                TypeParser::new(self).try_parse_ast_type(0, &def.param_types)
                            {
                                self.i = next_i;
                                self.parser_data.push(ParserData::FunctionDefParameter(
                                    ASTParameterDef { name, ast_type },
                                ));
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
                    if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close) = token.kind
                    {
                        self.state.pop();
                        self.i += 1;
                        continue;
                    } else if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        self.parser_data
                            .push(ParserData::FunctionCall(ASTFunctionCall {
                                original_function_name: function_name.clone(),
                                function_name,
                                parameters: Vec::new(),
                                index: self.get_index(0).unwrap(),
                            }));
                        self.state.push(ParserState::FunctionCall);
                        self.i = next_i;
                        continue;
                    } else if let Some((expr, next_i)) = self.try_parse_val() {
                        self.parser_data
                            .push(ParserData::Val(expr, self.get_index(0).unwrap()));
                        self.state.push(ParserState::Val);
                        self.i = next_i;
                        continue;
                    } else if let Some((name, next_i)) = self.try_parse_let() {
                        self.parser_data.push(ParserData::Let(name));
                        self.state.push(ParserState::Let);
                        self.i = next_i;
                        continue;
                    }
                    self.panic("Error parsing function body.");
                }
                Some(ParserState::FunctionDefReturnType) => {
                    if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
                        if let Some((ast_type, next_i)) =
                            TypeParser::new(self).try_parse_ast_type(0, &def.param_types)
                        {
                            self.i = next_i;

                            let mut def = def.clone();
                            self.i = next_i;
                            def.return_type = Some(ast_type);
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
                        if let Some((variants, next_i)) =
                            EnumParser::new(self).parse_variants(&def.type_parameters, 0)
                        {
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
                    if let Some(ParserData::Val(expr, _index)) = self.last_parser_data() {
                        let statement = ASTStatement::Expression(expr.clone());

                        if let Some(ParserData::FunctionDef(def)) = self.before_last_parser_data() {
                            debug!("Found {:?} in function {}", expr, def.name);
                            let mut def = def.clone();
                            if let RASMBody(mut calls) = def.body {
                                calls.push(statement);
                                def.body = RASMBody(calls);
                                let l = self.parser_data.len();
                                self.parser_data[l - 2] = ParserData::FunctionDef(def);
                                self.parser_data.pop();
                                self.state.pop();
                                continue;
                            } else {
                                panic!("expected rasm body, found {:?}", def.body);
                            }
                        } else if let Some(ParserData::LambdaDef(def)) =
                            self.before_last_parser_data()
                        {
                            let mut def = def.clone();
                            let mut calls = def.body;

                            calls.push(statement);
                            def.body = calls;
                            let l = self.parser_data.len();
                            self.parser_data[l - 2] = ParserData::LambdaDef(def);
                            self.parser_data.pop();
                            self.state.pop();
                            continue;
                        } else {
                            panic!("Function def, found {:?}", self.before_last_parser_data());
                        }
                    }
                    panic!("Expected val name, found {:?}", self.last_parser_data());
                }
                Some(StructDef) => {
                    if let Some(ParserData::StructDef(mut def)) = self.last_parser_data() {
                        if let Some((properties, next_i)) =
                            StructParser::new(self).parse_properties(&def.type_parameters, 0)
                        {
                            def.properties = properties;
                            self.structs.push(def);
                            self.state.pop();
                            self.parser_data.pop();
                            self.i = next_i;
                            continue;
                        } else {
                            panic!("Expected properties.")
                        }
                    } else {
                        panic!("Expected struct data.")
                    }
                }
                Some(ParserState::Let) => {
                    if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        let call = ASTFunctionCall {
                            original_function_name: function_name.clone(),
                            function_name,
                            parameters: Vec::new(),
                            index: self.get_index(0).unwrap(),
                        };
                        self.parser_data.push(ParserData::FunctionCall(call));
                        self.state.push(ParserState::FunctionCall);
                        self.i = next_i;
                        continue;
                    } else if let Some((expression, next_i)) = self.try_parse_val() {
                        let statement = if let Some(ParserData::Let(name)) = self.last_parser_data()
                        {
                            LetStatement(name, expression)
                        } else {
                            self.panic("expected let");
                        };

                        if let Some(ParserData::FunctionDef(def)) = self.before_last_parser_data() {
                            let mut def = def.clone();
                            if let RASMBody(mut calls) = def.body {
                                calls.push(statement);
                                def.body = RASMBody(calls);
                                let l = self.parser_data.len();
                                self.parser_data[l - 2] = ParserData::FunctionDef(def);
                            }
                        } else if let Some(ParserData::LambdaDef(def)) =
                            self.before_last_parser_data()
                        {
                            let mut def = def.clone();
                            let mut calls = def.body;
                            calls.push(statement);
                            def.body = calls;
                            let l = self.parser_data.len();
                            self.parser_data[l - 2] = ParserData::LambdaDef(def);
                        } else {
                            self.body.push(statement);
                        }
                        self.parser_data.pop();
                        self.i += 2;
                        self.state.pop();
                        continue;
                    }
                    self.panic("Error parsing let, unexpected token");
                }
            }

            self.i += 1;

            //actual.push(token);
        }

        self.functions.append(&mut self.included_functions);

        //println!("ebums: \n{:?}", self.enums);

        ASTModule {
            body: self.body.clone(),
            functions: self.functions.clone(),
            enums: self.enums.clone(),
            structs: self.structs.clone(),
            requires: self.requires.clone(),
            externals: self.externals.clone(),
            types: self.types.clone(),
        }
    }

    fn get_index(&self, n: usize) -> Option<ASTIndex> {
        self.get_token_n(n).map(|it| ASTIndex {
            file_name: self.file_name.clone(),
            row: it.row,
            column: it.column,
        })
    }

    fn get_index_from_token(&self, token: &Token) -> ASTIndex {
        ASTIndex {
            file_name: self.file_name.clone(),
            row: token.row,
            column: token.column,
        }
    }

    fn process_function_call(&mut self, token: Token) -> ProcessResult {
        if let Some(ParserData::FunctionCall(call)) = self.last_parser_data() {
            if let TokenKind::StringLiteral(value) = &token.kind {
                self.add_parameter_to_call_and_update_parser_data(
                    call,
                    ASTExpression::StringLiteral(value.clone()),
                );
                return ProcessResult::Continue;
            } else if let TokenKind::CharLiteral(c) = &token.kind {
                self.add_parameter_to_call_and_update_parser_data(
                    call,
                    ASTExpression::Value(ValueType::Char(*c), self.get_index_from_token(&token)),
                );
                return ProcessResult::Continue;
            } else if let TokenKind::Number(value) = &token.kind {
                self.add_parameter_to_call_and_update_parser_data(
                    call,
                    ASTExpression::Value(
                        ValueType::Number(value.parse().unwrap()),
                        self.get_index_from_token(&token),
                    ),
                );
                return ProcessResult::Continue;
            } else if let TokenKind::KeyWord(KeywordKind::True) = &token.kind {
                self.add_parameter_to_call_and_update_parser_data(
                    call,
                    ASTExpression::Value(
                        ValueType::Boolean(true),
                        self.get_index_from_token(&token),
                    ),
                );
                return ProcessResult::Continue;
            } else if let TokenKind::KeyWord(KeywordKind::False) = &token.kind {
                self.add_parameter_to_call_and_update_parser_data(
                    call,
                    ASTExpression::Value(
                        ValueType::Boolean(false),
                        self.get_index_from_token(&token),
                    ),
                );
                return ProcessResult::Continue;
            } else if let TokenKind::AlphaNumeric(name) = &token.kind {
                /*
                if name == "true" {
                    self.add_parameter_to_call_and_update_parser_data(
                        call,
                        ASTExpression::Value(ValueType::Boolean(true), self.get_index_from_token(&token)),
                    );
                    return ProcessResult::Continue;
                } else if name == "false" {
                    self.add_parameter_to_call_and_update_parser_data(
                        call,
                        ASTExpression::Value(ValueType::Boolean(false), self.get_index_from_token(&token)),
                    );
                    return ProcessResult::Continue;
                } else

                 */
                if let Some((function_name, next_i)) = self.try_parse_function_call() {
                    self.parser_data
                        .push(ParserData::FunctionCall(ASTFunctionCall {
                            original_function_name: function_name.clone(),
                            function_name,
                            parameters: Vec::new(),
                            index: self.get_index(0).unwrap(),
                        }));
                    self.state.push(ParserState::FunctionCall);
                    self.i = next_i;
                } else {
                    self.add_parameter_to_call_and_update_parser_data(
                        call,
                        ASTExpression::ValueRef(name.clone(), self.get_index_from_token(&token)),
                    );
                }
                return ProcessResult::Continue;
            } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
                self.i += 1;
                return ProcessResult::Continue;
            } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
                let (parameter_names, next_i) = self.parse_lambda_parameters(1);
                self.parser_data.push(ParserData::LambdaDef(ASTLambdaDef {
                    parameter_names,
                    body: Vec::new(),
                }));
                self.state.push(ParserState::FunctionBody);
                self.i = next_i;
                return ProcessResult::Continue;
            } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind
            {
                if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
                    self.get_token_kind_n(1)
                {
                    let statement =
                        if let Some(ParserData::Let(name)) = self.before_last_parser_data() {
                            let let_statement = ASTStatement::LetStatement(
                                name.clone(),
                                ASTFunctionCallExpression(call),
                            );
                            self.parser_data.pop();
                            self.state.pop();
                            let_statement
                        } else {
                            ASTStatement::Expression(ASTFunctionCallExpression(call))
                        };

                    if let Some(ParserData::FunctionDef(def)) = self.before_last_parser_data() {
                        let mut def = def.clone();
                        if let RASMBody(mut calls) = def.body {
                            calls.push(statement);
                            def.body = RASMBody(calls);
                            let l = self.parser_data.len();
                            self.parser_data[l - 2] = ParserData::FunctionDef(def);
                            self.parser_data.pop();
                            self.i += 2;
                            self.state.pop();
                            return ProcessResult::Continue;
                        }
                    } else if let Some(ParserData::LambdaDef(def)) = self.before_last_parser_data()
                    {
                        let mut def = def.clone();
                        let mut calls = def.body;
                        calls.push(statement);
                        def.body = calls;
                        let l = self.parser_data.len();
                        self.parser_data[l - 2] = ParserData::LambdaDef(def);
                        self.parser_data.pop();
                        self.i += 2;
                        self.state.pop();
                        return ProcessResult::Continue;
                    } else {
                        self.body.push(statement);
                    }
                    self.parser_data.pop();
                    self.state.pop();
                    self.i += 2;
                    return ProcessResult::Continue;
                } else if let Some(ParserData::FunctionCall(before_call)) =
                    self.before_last_parser_data()
                {
                    let mut before_call = before_call.clone();
                    before_call.parameters.push(ASTFunctionCallExpression(call));
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
        ProcessResult::Panic("processing function call".into())
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
                if let Some((ref ast_type, next_i)) =
                    TypeParser::new(self).try_parse_ast_type(0, &def.param_types)
                {
                    self.i = next_i;
                    def.return_type = Some(ast_type.clone());
                } else {
                    def.return_type = None
                }
                let l = self.parser_data.len();
                self.parser_data[l - 1] = ParserData::FunctionDef(def);
                self.i += 1;
                return ProcessResult::Continue;
            } else {
                self.panic("");
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
            return ProcessResult::Panic("processing function definition".into());
        }
        ProcessResult::Next
    }

    fn add_parameter_to_call_and_update_parser_data(
        &mut self,
        mut call: ASTFunctionCall,
        ast_expression: ASTExpression,
    ) {
        call.parameters.push(ast_expression);
        let l = self.parser_data.len();
        self.parser_data[l - 1] = ParserData::FunctionCall(call);
        self.i += 1;
    }

    pub fn print(module: &ASTModule) {
        module.body.iter().for_each(|call| {
            println!("{call}");
        });
        println!();
        module.functions.iter().for_each(Self::print_function_def)
    }

    pub fn print_enhanced(module: &EnhancedASTModule) {
        module.body.iter().for_each(|call| {
            println!("{call}");
        });
        println!();
        module
            .functions()
            .iter()
            .for_each(|it| Self::print_function_def(it))
    }

    pub fn print_function_def(f: &ASTFunctionDef) {
        match &f.body {
            RASMBody(_) => print!("fn {}", f),
            ASMBody(_) => print!("asm {}", f),
        }
        match &f.body {
            RASMBody(expressions) => {
                println!(" {{");
                expressions.iter().for_each(|call| {
                    println!("  {}", call);
                });
                println!("}}");
            }
            ASMBody(_) => println!(" {{...}}"),
        }
    }

    fn get_state(&self) -> Option<&ParserState> {
        self.state.last()
    }

    fn error_msg(&self, message: &str) -> String {
        let option = self.tokens.get(self.i);

        if let Some(token) = option {
            //self.debug(message);
            format!(
                "{} {:?}:{},{}",
                message, self.file_name, token.row, token.column
            )
        } else {
            format!("{} {:?} : in end of file", message, self.file_name)
        }
    }

    fn panic(&self, message: &str) -> ! {
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
            } else if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                self.get_token_kind_n(1)
            {
                //println!("found function call {}", function_name);
                return Some((function_name.clone(), self.i + 2));
            }
        }
        None
    }

    fn try_parse_enum_constructor(&self) -> Option<(String, usize)> {
        if let (
            Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            Some(TokenKind::AlphaNumeric(variant)),
            Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)),
        ) = (
            self.get_token_kind_n(1),
            self.get_token_kind_n(2),
            self.get_token_kind_n(3),
            self.get_token_kind_n(4),
        ) {
            Some((variant.clone(), self.i + 5))
        } else {
            None
        }
    }

    fn try_parse_function_def(&self) -> Option<(String, Vec<String>, usize)> {
        if let Some(TokenKind::KeyWord(KeywordKind::Fn)) = self.get_token_kind() {
            let param_types_matcher = param_types_matcher();

            let mut matcher = TokensMatcher::default();
            matcher.add_alphanumeric();
            matcher.add_matcher(param_types_matcher);
            matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open));

            if let Some(matcher_result) = matcher.match_tokens(self, 1) {
                let param_types = matcher_result.group_values("type");
                Some((
                    matcher_result.values().first().unwrap().clone(),
                    param_types,
                    self.get_i() + matcher_result.next_n(),
                ))
            } else {
                self.panic("Cannot parse function definition");
            }
        } else {
            None
        }
    }

    fn try_parse_include(&self) -> Option<(String, usize)> {
        if let Some(TokenKind::KeyWord(KeywordKind::Include)) = self.get_token_kind() {
            if let Some(next_token) = self.next_token() {
                if let TokenKind::StringLiteral(include) = &next_token.kind {
                    return Some((include.into(), self.i + 2));
                } else {
                    self.panic(&format!("Unexpected token {:?}", next_token));
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

    fn try_parse_val(&self) -> Option<(ASTExpression, usize)> {
        if let Some(TokenKind::KeyWord(KeywordKind::True)) = self.get_token_kind() {
            if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
                self.get_token_kind_n(1)
            {
                return Some((
                    ASTExpression::Value(ValueType::Boolean(true), self.get_index(0).unwrap()),
                    self.get_i() + 2,
                ));
            }
        } else if let Some(TokenKind::KeyWord(KeywordKind::False)) = self.get_token_kind() {
            if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
                self.get_token_kind_n(1)
            {
                return Some((
                    ASTExpression::Value(ValueType::Boolean(false), self.get_index(0).unwrap()),
                    self.get_i() + 2,
                ));
            }
        } else if let Some(TokenKind::Number(n)) = self.get_token_kind() {
            if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
                self.get_token_kind_n(1)
            {
                return Some((
                    ASTExpression::Value(
                        ValueType::Number(n.parse().unwrap()),
                        self.get_index(0).unwrap(),
                    ),
                    self.get_i() + 2,
                ));
            }
        } else if let Some(TokenKind::CharLiteral(c)) = self.get_token_kind() {
            if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
                self.get_token_kind_n(1)
            {
                return Some((
                    ASTExpression::Value(ValueType::Char(*c), self.get_index(0).unwrap()),
                    self.get_i() + 2,
                ));
            }
        } else if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind() {
            if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
                self.get_token_kind_n(1)
            {
                return Some((
                    ASTExpression::ValueRef(name.clone(), self.get_index(0).unwrap()),
                    self.get_i() + 2,
                ));
            }
        }
        None
    }

    fn try_parse_let(&self) -> Option<(String, usize)> {
        if let Some(TokenKind::KeyWord(KeywordKind::Let)) = self.get_token_kind() {
            if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind_n(1) {
                if let Some(TokenKind::Punctuation(PunctuationKind::Equal)) =
                    self.get_token_kind_n(2)
                {
                    return Some((name.clone(), self.get_i() + 3));
                } else {
                    self.panic(&format!("expected = got {:?}", self.get_token_kind_n(2)));
                }
            } else {
                self.panic(&format!(
                    "expected name, got {:?}",
                    self.get_token_kind_n(1)
                ));
            }
        }
        None
    }

    fn parse_type_def(&self) -> (ASTTypeDef, usize) {
        if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind_n(1) {
            if let Some((type_parameters, next_n)) = TypeParamsParser::new(self).try_parse(2) {
                (
                    ASTTypeDef {
                        type_parameters,
                        name: name.clone(),
                    },
                    next_n,
                )
            } else {
                (
                    ASTTypeDef {
                        type_parameters: Vec::new(),
                        name: name.clone(),
                    },
                    2,
                )
            }
        } else {
            self.panic("Error parsing include")
        }
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

    use linked_hash_map::LinkedHashMap;

    use crate::lexer::Lexer;
    use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionDef, ASTStatement};
    use crate::parser::{Parser, ValueType};

    #[test]
    fn test() {
        let path = Path::new("resources/test/helloworld.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        parser.parse(path);
    }

    #[test]
    fn test2() {
        let path = Path::new("resources/test/test2.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

        assert_eq!(1, module.body.len());
    }

    #[test]
    fn test8() {
        let path = Path::new("resources/test/test8.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

        assert!(!module.functions.is_empty());
        assert_eq!(2, module.functions.get(0).unwrap().parameters.len());
    }

    #[test]
    fn test9() {
        let path = Path::new("resources/test/test9.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

        let par =
            if let Some(ASTStatement::Expression(ASTExpression::ASTFunctionCallExpression(e))) =
                module.body.get(0)
            {
                Some(e)
            } else {
                None
            };

        assert!(!module.body.is_empty());
        assert_eq!(1, par.unwrap().parameters.len());

        let nprint_parameter = par.unwrap().parameters.get(0);

        if let Some(ASTExpression::ASTFunctionCallExpression(call)) = nprint_parameter {
            assert_eq!("add", call.function_name);
            assert_eq!(2, call.parameters.len());
            if let Some(ASTExpression::Value(ValueType::Number(n), _)) = call.parameters.get(0) {
                assert_eq!(10, *n);
            } else {
                panic!();
            }
            if let Some(ASTExpression::Value(ValueType::Number(n), _)) = call.parameters.get(1) {
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
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        parser.parse(path);
    }

    #[test]
    fn test11() {
        let path = Path::new("resources/test/test11.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        parser.parse(path);
    }

    #[test]
    fn function_def_with_type_parameters() {
        let lexer = Lexer::new("fn p<T,T1>() {}".into(), "a test file".into());

        let mut parser = Parser::new(lexer, None);

        let module = parser.parse(Path::new("."));

        let function_def = ASTFunctionDef {
            name: "p".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: Vec::new(),
            return_type: None,
            inline: false,
            param_types: vec!["T".into(), "T1".into()],
            resolved_generic_types: LinkedHashMap::new(),
            original_name: "p".into(),
        };

        assert_eq!(module.functions, vec![function_def]);
    }
}
