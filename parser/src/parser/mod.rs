use ast::ASTPosition;
use lazy_static::lazy_static;
use properties_parser::parse_properties;
use std::fmt::{Display, Formatter};

use crate::lexer::tokens::{
    BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind,
};
use crate::lexer::Lexer;
use crate::parser::asm_def_parser::AsmDefParser;
use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
use crate::parser::ast::ASTFunctionBody::{NativeBody, RASMBody};
use crate::parser::ast::{
    ASTEnumDef, ASTExpression, ASTFunctionCall, ASTFunctionDef, ASTLambdaDef, ASTModifiers,
    ASTModule, ASTParameterDef, ASTStatement, ASTStructDef, ASTType, ASTTypeDef, ASTValueType,
};
use crate::parser::enum_parser::EnumParser;
use crate::parser::matchers::{generic_types_matcher, modifiers_matcher};
use crate::parser::struct_parser::StructParser;
use crate::parser::tokens_matcher::{TokensMatcher, TokensMatcherTrait};
use crate::parser::type_params_parser::TypeParamsParser;
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserState::StructDef;
use rasm_utils::{debug_i, OptionDisplay, SliceDisplay};

mod asm_def_parser;
pub mod ast;
pub mod builtin_functions;
mod enum_parser;
mod matchers;
pub mod properties_parser;
mod struct_parser;
#[cfg(test)]
mod test_utils;
mod tokens_group;
pub mod tokens_matcher;
mod type_params_parser;
pub mod type_parser;

lazy_static! {
    static ref ENUM_PARSER: EnumParser = EnumParser::new();
    static ref STRUCT_PARSER: StructParser = StructParser::new();
    static ref FUNCTION_DEF_MATCHER: TokensMatcher = {
        let mut function_def_matcher = TokensMatcher::default();
        function_def_matcher.add_matcher(modifiers_matcher());
        function_def_matcher.add_kind(TokenKind::KeyWord(KeywordKind::Fn));
        function_def_matcher.add_alphanumeric();
        function_def_matcher.add_matcher(generic_types_matcher());
        function_def_matcher.add_kind(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open));
        function_def_matcher
    };
}

pub struct ParserError {
    position: ASTPosition,
    pub message: String,
}

impl ParserError {
    pub fn new(position: ASTPosition, message: String) -> Self {
        Self { position, message }
    }

    pub fn row(&self) -> usize {
        self.position.row
    }

    pub fn column(&self) -> usize {
        self.position.column
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.message, self.position)
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    body: Vec<ASTStatement>,
    functions: Vec<ASTFunctionDef>,
    i: usize,
    parser_data: Vec<ParserData>,
    state: Vec<ParserState>,
    enums: Vec<ASTEnumDef>,
    structs: Vec<ASTStructDef>,
    types: Vec<ASTTypeDef>,
    errors: Vec<ParserError>,
}

#[derive(Clone, Debug)]
enum ParserData {
    EnumDef(ASTEnumDef),
    FunctionCall(ASTFunctionCall),
    FunctionDef(ASTFunctionDef),
    FunctionDefParameter(ASTParameterDef),
    LambdaDef(ASTLambdaDef),
    Let(String, bool, ASTPosition),
    StructDef(ASTStructDef),
    Expression(ASTExpression),
    Statement(ASTStatement),
}

impl Display for ParserData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserData::EnumDef(e) => {
                write!(f, "EnumDef({}): {}", e.name, e.position)
            }
            ParserData::FunctionCall(call) => {
                write!(f, "FunctionCall({}): {}", call, call.position())
            }
            ParserData::FunctionDef(def) => {
                write!(f, "FunctionDef({}): {}", def, def.position)
            }
            ParserData::FunctionDefParameter(def) => {
                write!(f, "FunctionDefParameter({}): {}", def, def.position)
            }
            ParserData::LambdaDef(def) => {
                write!(f, "LambdaDef({}): {}", def, def.position)
            }
            ParserData::Let(name, _, index) => {
                write!(f, "Let({}): {index}", name)
            }
            ParserData::StructDef(s) => {
                write!(f, "StructDef({}): {}", s.name, s.position)
            }
            ParserData::Expression(e) => {
                write!(f, "Expression({}): {}", e, e.position())
            }
            ParserData::Statement(s) => {
                write!(f, "Statement({}): {}", s, s.position())
            }
        }
    }
}

#[derive(Clone, Debug)]
enum ParserState {
    EnumDef,
    FunctionCall,
    FunctionDef,
    FunctionBody,
    FunctionDefParameter,
    FunctionDefReturnType,
    Expression,
    Statement,
    Let,
    StructDef,
    LambdaExpression,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        //let (lexer_tokens, lexer_errors) = lexer.process();
        let mut lexer_errors = Vec::new();

        let tokens = lexer
            .filter(|(token_o, errs)| {
                lexer_errors.extend(errs.clone());

                if let Some(token) = token_o {
                    !matches!(
                        token.kind,
                        TokenKind::WhiteSpaces(_)
                            | TokenKind::Comment(_)
                            | TokenKind::MultiLineComment(_)
                            | TokenKind::EndOfLine
                    )
                } else {
                    false
                }
            })
            .map(|(token, _)| token.unwrap())
            .collect::<Vec<_>>();

        let errors = lexer_errors
            .into_iter()
            .map(|it| ParserError::new(ASTPosition::new(it.row, it.column), it.message))
            .collect();

        Self {
            tokens,
            body: Vec::new(),
            functions: Vec::new(),
            i: 0,
            parser_data: Vec::new(),
            state: Vec::new(),
            enums: Vec::new(),
            structs: Vec::new(),
            types: Vec::new(),
            errors,
        }
    }

    pub fn parse(mut self) -> (ASTModule, Vec<ParserError>) {
        let last_token = Token::new(TokenKind::EndOfLine, 0, 0);

        let mut count = 0;

        while self.i <= self.tokens.len() {
            count += 1;
            if count > (self.tokens.len() + 1) * 10 {
                self.add_error("undefined parse error".to_owned());
                return self.get_return();
            }
            let mut token = if self.i == self.tokens.len() {
                last_token.clone()
            } else {
                self.tokens.get(self.i).unwrap().clone()
            };

            self.debug("");

            // uniform call with dot notation
            if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
                if let TokenKind::Punctuation(PunctuationKind::Dot) = token.kind {
                    self.i += 1;
                    if let Some((function_name, generics, next_i, function_name_i, target)) =
                        self.try_parse_function_call()
                    {
                        self.parser_data.pop();
                        let call = ASTFunctionCall::new(
                            function_name,
                            vec![expr],
                            self.get_position(function_name_i),
                            generics,
                            target,
                        );
                        self.parser_data.push(ParserData::FunctionCall(call));
                        self.state.push(ParserState::FunctionCall);
                        self.i = next_i;
                        continue;
                    } else if let Some(TokenKind::AlphaNumeric(name)) =
                        self.get_token_kind().cloned()
                    {
                        self.parser_data.pop();
                        let call = ASTFunctionCall::new(
                            name.clone(),
                            vec![expr],
                            self.get_position(0),
                            Vec::new(),
                            None,
                        );
                        self.parser_data
                            .push(ParserData::Expression(ASTFunctionCallExpression(call)));
                        self.i += 1;
                        continue;
                    }
                    // probably it's an error, but we handle it later
                    self.add_error("Unterminated dot expression.".to_string());
                    self.i -= 1;
                    token.kind = TokenKind::Punctuation(PunctuationKind::SemiColon);
                }
            }

            match self.get_state() {
                None => match self.process_none(&token) {
                    Ok(is_continue) => {
                        if is_continue {
                            continue;
                        } else {
                            break;
                        }
                    }
                    Err(message) => {
                        self.add_error(message);
                    }
                },
                Some(ParserState::FunctionCall) => match self.process_function_call(token) {
                    Ok(_) => {
                        continue;
                    }
                    Err(message) => self.add_error(message),
                },
                Some(ParserState::FunctionDef) => match self.process_function_def(token) {
                    Ok(_) => {
                        continue;
                    }
                    Err(message) => self.add_error(message),
                },
                Some(ParserState::FunctionDefParameter) => {
                    match self.process_function_def_parameter(&token) {
                        Ok(_) => {
                            continue;
                        }
                        Err(message) => self.add_error(message),
                    }
                }
                Some(ParserState::FunctionBody) => match self.process_function_body(&token) {
                    Ok(_) => {
                        continue;
                    }
                    Err(message) => self.add_error(message),
                },
                Some(ParserState::FunctionDefReturnType) => {
                    if let Some(ParserData::FunctionDef(mut def)) = self.last_parser_data() {
                        match TypeParser::new(&self).try_parse_ast_type(0, &def.generic_types) {
                            Ok(result) => {
                                if let Some((ast_type, next_i)) = result {
                                    self.i = next_i;
                                    def.return_type = ast_type;

                                    self.set_parser_data(ParserData::FunctionDef(def), 0);

                                    self.state.pop();
                                    continue;
                                }
                            }
                            Err(message) => self.add_error(message),
                        }
                    } else {
                        self.add_error("Illegal state".to_string());
                    }
                }
                Some(ParserState::EnumDef) => {
                    if let Some(ParserData::EnumDef(mut def)) = self.last_parser_data() {
                        match ENUM_PARSER.parse_variants(&self, &def.type_parameters, 0) {
                            Ok(result) => {
                                if let Some((variants, next_i)) = result {
                                    def.variants = variants;
                                    self.enums.push(def);
                                    self.state.pop();
                                    self.parser_data.pop();
                                    self.i = next_i;
                                    continue;
                                } else {
                                    self.add_error("Expected variants".to_string());
                                }
                            }
                            Err(message) => self.add_error(message),
                        }
                    } else {
                        self.add_error("Expected enum data".to_string());
                    }
                }
                Some(StructDef) => {
                    if let Some(ParserData::StructDef(mut def)) = self.last_parser_data() {
                        let (properties, errors, new_n) = parse_properties(
                            &self,
                            &def.type_parameters,
                            0,
                            TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close),
                        );

                        if properties.is_empty() {
                            self.add_error("Expected properties".to_owned());
                        }

                        self.errors.extend(errors);

                        def.properties = properties;
                        self.structs.push(def);
                        self.state.pop();
                        self.parser_data.pop();
                        self.i += new_n;
                        continue;
                    } else {
                        self.add_error("Expected struct data".to_string());
                    }
                }
                Some(ParserState::Let) => {
                    let is_semicolon = matches!(
                        token.kind,
                        TokenKind::Punctuation(PunctuationKind::SemiColon)
                    );
                    if !is_semicolon {
                        self.add_error("Expected expression and semicolon".to_string());
                    }
                    if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
                        if let Some(ParserData::Let(name, is_const, index)) =
                            self.before_last_parser_data()
                        {
                            self.state.pop();
                            self.parser_data.pop();
                            self.parser_data.pop();

                            self.parser_data.push(ParserData::Statement(
                                ASTStatement::LetStatement(name, expr.clone(), is_const, index),
                            ));
                            self.i += 1;
                            continue;
                        }
                    }
                    if is_semicolon {
                        self.add_error("Expected expression".to_string());
                    }
                }
                Some(ParserState::LambdaExpression) => {
                    if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
                        if let Some(ParserData::LambdaDef(mut lambda_def)) =
                            self.before_last_parser_data()
                        {
                            if let RASMBody(statements) = def.body {
                                lambda_def.body = statements;
                                self.state.pop();
                                self.state.pop();
                                self.parser_data.pop();
                                self.parser_data.pop();
                                self.parser_data.push(ParserData::Expression(
                                    ASTExpression::Lambda(lambda_def),
                                ));
                                continue;
                            }
                        }
                    }
                    self.add_error("Error parsing lambda".to_string());
                }
                Some(ParserState::Expression) => match self.process_expression() {
                    Ok(_) => {
                        continue;
                    }
                    Err(message) => self.add_error(message),
                },
                Some(ParserState::Statement) => match self.process_statement(token) {
                    Ok(_) => {
                        continue;
                    }
                    Err(message) => self.add_error(message),
                },
            }

            self.i += 1;
        }

        self.get_return()
    }

    fn get_return(self) -> (ASTModule, Vec<ParserError>) {
        let module = ASTModule {
            body: self.body.clone(),
            functions: self.functions.clone(),
            enums: self.enums.clone(),
            structs: self.structs.clone(),
            types: self.types.clone(),
        };

        (module, self.errors)
    }

    fn add_error(&mut self, message: String) {
        debug_i!("parser error: {message}");

        let position = self
            .get_token_n(0)
            .or_else(|| self.tokens.last())
            .map(|it| it.position.clone())
            .unwrap_or_else(|| ASTPosition::new(0, 0));

        self.errors.push(ParserError::new(position, message));
    }

    ///
    /// returns true if the parser must continue
    ///
    fn process_none(&mut self, token: &Token) -> Result<bool, String> {
        if let Some(ParserData::Statement(stmt)) = self.last_parser_data() {
            self.body.push(stmt);
            self.parser_data.pop();
        } else if let TokenKind::EndOfLine = token.kind {
            return Ok(false);
        } else if !self.parser_data.is_empty() {
            return Err(format!("Error: {}", self.get_position(0)));
        } else if let Some((name_token, generic_types, modifiers, next_i)) =
            self.try_parse_function_def()?
        {
            let name = name_token.alpha().unwrap().clone();

            let function_def = ASTFunctionDef {
                name,
                parameters: Vec::new(),
                body: RASMBody(Vec::new()),
                return_type: ASTType::Unit,
                inline: false,
                generic_types,
                position: name_token.position,
                modifiers,
                target: None,
            };
            self.parser_data.push(ParserData::FunctionDef(function_def));
            self.state.push(ParserState::FunctionDef);
            self.state.push(ParserState::FunctionDefParameter);
            self.i = next_i;
        } else if let Some((name_token, inline, param_types, modifiers, next_i)) =
            AsmDefParser::new(self).try_parse()?
        {
            let name = name_token.alpha().unwrap();
            let function_def = ASTFunctionDef {
                name,
                parameters: Vec::new(),
                body: NativeBody("".into()),
                return_type: ASTType::Unit,
                inline,
                generic_types: param_types,
                position: name_token.position.clone(),
                modifiers,
                target: None,
            };
            self.parser_data.push(ParserData::FunctionDef(function_def));
            self.state.push(ParserState::FunctionDef);
            self.state.push(ParserState::FunctionDefParameter);
            self.i = next_i;
        } else if let Some((name, type_params, modifiers, next_i)) = ENUM_PARSER.try_parse(self) {
            self.parser_data.push(ParserData::EnumDef(ASTEnumDef {
                name: name.alpha().unwrap(),
                type_parameters: type_params,
                variants: Vec::new(),
                position: name.position.clone(),
                modifiers,
            }));
            self.state.push(ParserState::EnumDef);
            self.i = next_i;
        } else if let Some((name_token, type_params, modifiers, next_i)) =
            STRUCT_PARSER.try_parse(self)
        {
            self.parser_data.push(ParserData::StructDef(ASTStructDef {
                name: name_token.alpha().unwrap(),
                type_parameters: type_params,
                properties: Vec::new(),
                position: name_token.position.clone(),
                modifiers,
            }));
            self.state.push(StructDef);
            self.i = next_i;
        } else if let Some((type_def, next_i)) = self.parse_type_def()? {
            let token = self.get_token_kind_n(next_i - self.i);
            if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) = token {
                self.types.push(type_def);
                self.i = next_i + 1;
            } else {
                return Err(format!(
                    "Missing semicolon, got {:?} : {}",
                    token,
                    self.get_position(0)
                ));
            }
        } else {
            self.state.push(ParserState::Statement);
        }
        Ok(true)
    }

    fn process_statement(&mut self, token: Token) -> Result<(), String> {
        if let Some(ParserData::Statement(_st)) = self.last_parser_data() {
            self.state.pop();
        } else if let TokenKind::Punctuation(PunctuationKind::SemiColon) = token.kind {
            if let Some(ParserData::Expression(epr)) = self.last_parser_data() {
                self.state.pop();
                self.parser_data.pop();
                self.parser_data
                    .push(ParserData::Statement(ASTStatement::Expression(epr)));
                self.i += 1;
            } else {
                self.add_error(format!(
                    "Found semicolon without an expression: {}",
                    self.get_position(0)
                ));
                self.state.pop();
                self.parser_data.pop();
                self.i += 1;
            }
        } else if Some(&TokenKind::Bracket(
            BracketKind::Brace,
            BracketStatus::Close,
        )) == self.get_token_kind()
        {
            if matches!(self.last_parser_data(), Some(ParserData::FunctionDef(_))) {
                // Here probably we have an empty function...
                self.state.pop();
            } else {
                self.add_error("Unexpected end of block.".to_string());
                if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
                    self.state.pop();
                    self.parser_data.pop();
                    self.parser_data
                        .push(ParserData::Statement(ASTStatement::Expression(expr)));
                } else {
                    self.state.pop();
                }
            }
        } else if let Some((name, next_i)) = self.try_parse_let(false)? {
            self.parser_data
                .push(ParserData::Let(name, false, self.get_position(1)));
            self.state.push(ParserState::Let);
            self.state.push(ParserState::Expression);
            self.i = next_i;
        } else if let Some((name, next_i)) = self.try_parse_let(true)? {
            self.parser_data
                .push(ParserData::Let(name, true, self.get_position(1)));
            self.state.push(ParserState::Let);
            self.state.push(ParserState::Expression);
            self.i = next_i;
        } else if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
            self.add_error(Self::token_message(
                "Unexpected token",
                self.get_token_kind(),
            ));
            self.state.pop();
            self.parser_data.pop();
            self.parser_data
                .push(ParserData::Statement(ASTStatement::Expression(expr)));
        } else {
            self.state.push(ParserState::Expression);
        }
        Ok(())
    }

    fn token_message(message: &str, token: Option<&TokenKind>) -> String {
        if let Some(t) = token {
            format!("{message} `{t}`")
        } else {
            message.to_string()
        }
    }

    fn process_expression(&mut self) -> Result<(), String> {
        if let Some(ParserData::Expression(_exp)) = self.last_parser_data() {
            self.state.pop();
        } else if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
            self.get_token_kind()
        {
            self.state.pop();
            self.i += 1;
        } else if let Some((function_name, generics, next_i, function_name_index, target)) =
            self.try_parse_function_call()
        {
            let call = ASTFunctionCall::new(
                function_name,
                Vec::new(),
                self.get_position(function_name_index),
                generics,
                target,
            );
            self.parser_data.push(ParserData::FunctionCall(call));
            self.state.push(ParserState::FunctionCall);
            self.i = next_i;
        } else if let Some((expression, next_i)) = self.try_parse_val()? {
            self.parser_data.push(ParserData::Expression(expression));
            self.state.pop();
            self.i = next_i;
        } else if Some(&TokenKind::KeyWord(KeywordKind::Fn)) == self.get_token_kind() {
            if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                self.get_token_kind_n(1)
            {
                let (parameter_names, next_i) = self.parse_lambda_parameters(2)?;

                if let Some(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)) =
                    self.get_token_kind_n(next_i - self.i)
                {
                    self.add_lambda_value(parameter_names);
                    self.i = next_i + 1;
                } else {
                    return Err(format!(
                        "Expected '{{', found {}",
                        OptionDisplay(&self.get_token_kind_n(next_i))
                    ));
                }
            } else {
                return Err(format!(
                    "Expected '(', found {}",
                    OptionDisplay(&self.get_token_kind_n(1))
                ));
            }
        } else if let Some(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)) =
            self.get_token_kind()
        {
            self.add_lambda_value(Vec::new());
            self.i += 1;
        } else if Some(&TokenKind::Bracket(
            BracketKind::Brace,
            BracketStatus::Close,
        )) == self.get_token_kind()
        {
            // Here probably we have an empty function...
            self.state.pop();
        } else if let Some(kind) = self.get_token_kind() {
            return Err(format!("Expected expression, found {}", kind));
        } else {
            if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
                self.functions.push(def);
            }
            return Err("Expected expression".into());
        }
        Ok(())
    }

    fn add_lambda_value(&mut self, parameter_names: Vec<(String, ASTPosition)>) {
        self.parser_data.push(ParserData::LambdaDef(ASTLambdaDef {
            parameter_names,
            body: Vec::new(),
            position: self.get_position(0),
        }));
        let fake_function_def = ASTFunctionDef {
            name: String::new(),
            parameters: Vec::new(),
            body: RASMBody(Vec::new()),
            return_type: ASTType::Unit,
            inline: false,
            generic_types: Vec::new(),
            position: ASTPosition::none(),
            modifiers: ASTModifiers::public(),
            target: None,
        };
        self.parser_data
            .push(ParserData::FunctionDef(fake_function_def));
        self.state.push(ParserState::LambdaExpression);
        self.state.push(ParserState::FunctionBody);
    }

    fn process_function_body(&mut self, token: &Token) -> Result<(), String> {
        if let Some(ParserData::Statement(stmt)) = self.last_parser_data() {
            if let Some(ParserData::FunctionDef(mut def)) = self.before_last_parser_data() {
                let mut statements = match &def.body {
                    RASMBody(statements) => {
                        // TODO try not to clone
                        statements.clone()
                    }
                    NativeBody(_) => {
                        return Err(format!(
                            "Expected function got native:  {}",
                            self.get_position(0)
                        ));
                    }
                };
                statements.push(stmt);
                def.body = RASMBody(statements);

                self.parser_data.pop();
                self.set_parser_data(ParserData::FunctionDef(def), 0);
            } else if let Some(ParserData::LambdaDef(mut def)) = self.before_last_parser_data() {
                def.body.push(stmt);
                self.parser_data.pop();
                self.set_parser_data(ParserData::LambdaDef(def), 0);
            } else {
                return Err(format!("Expected body: {}", self.get_position(0)));
            }
        } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close) = token.kind {
            self.state.pop();
            self.i += 1;
        /*
                   if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
                       self.state.pop();
                       self.i += 1;
                   } else if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
                       let function_def = self
                           .parser_data
                           .iter_mut()
                           .rev()
                           .find(|it| matches!(it, ParserData::FunctionDef(_)));

                       if let Some(ParserData::FunctionDef(ref mut def)) = function_def {
                           if let ASTFunctionBody::RASMBody(body) = &def.body {
                               let mut body = body.clone();
                               body.push(ASTStatement::Expression(expr));
                               def.body = ASTFunctionBody::RASMBody(body);
                               self.functions.push(def.clone());
                               self.parser_data.clear();
                               self.state.clear();
                               self.i += 1;
                           }
                           self.add_error("not terminated expression".to_string());
                       } else {
                           return Err("Expected a function def".to_string());
                       }
                   } else {
                       return Err(format!(
                           "Expected to be inside a function body: {}",
                           self.get_index(0)
                       ));
                   }

        */
        } else {
            self.state.push(ParserState::Statement);
        }
        Ok(())
    }

    fn process_function_def_parameter(&mut self, token: &Token) -> Result<(), String> {
        if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
            if let Some((name, next_i)) = self.try_parse_parameter_def_name() {
                let _n = next_i - self.i;
                self.i = next_i;
                if let Some((ast_type, next_i)) =
                    TypeParser::new(self).try_parse_ast_type(0, &def.generic_types)?
                {
                    self.i = next_i;
                    self.parser_data
                        .push(ParserData::FunctionDefParameter(ASTParameterDef {
                            name,
                            ast_type,
                            position: token.position.clone(),
                        }));
                    self.state.pop();
                    Ok(())
                } else {
                    Err(String::new())
                }
            } else {
                self.state.pop();
                Ok(())
            }
        } else {
            Err(format!("Illegal state: {}", self.get_position(0)))
        }
    }

    fn set_parser_data(&mut self, parser_data: ParserData, n: usize) {
        let l = self.parser_data.len();
        self.parser_data[l - n - 1] = parser_data;
    }

    fn process_function_call(&mut self, token: Token) -> Result<(), String> {
        if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
            if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
                if let Some(ParserData::FunctionCall(call)) = self.before_last_parser_data() {
                    self.parser_data.pop();
                    self.add_parameter_to_call_and_update_parser_data(call, expr);
                    self.i += 1;
                } else {
                    return Err(format!("expected function call: {}", self.get_position(0)));
                }
            } else {
                return Err("expected expression".to_string());
            }
        }
        /*else if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
            if let Some(ParserData::FunctionCall(call)) = self.before_last_parser_data() {
                self.parser_data.pop();
                self.add_parameter_to_call_and_update_parser_data(call, expr);
            } else {
                return Err(format!("expected function call: {}", self.get_index(0)));
            }
        }*/
        else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
            if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
                if let Some(ParserData::FunctionCall(call)) = self.before_last_parser_data() {
                    self.parser_data.pop();
                    self.add_parameter_to_call_and_update_parser_data(call, expr);
                } else {
                    return Err(format!("expected function call: {}", self.get_position(0)));
                }
            }
            if let Some(ParserData::FunctionCall(call)) = self.last_parser_data() {
                self.state.pop();
                self.parser_data.pop();
                self.parser_data.push(ParserData::Expression(
                    ASTExpression::ASTFunctionCallExpression(call),
                ));
                self.i += 1;
            } else {
                return Err(format!("expected function call: {}", self.get_position(0)));
            }
        } else {
            if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
                if let Some(ParserData::FunctionCall(call)) = self.before_last_parser_data() {
                    self.parser_data.pop();
                    self.add_parameter_to_call_and_update_parser_data(call, expr);
                    if let Some(ParserData::FunctionCall(call)) = self.last_parser_data() {
                        self.state.pop();
                        self.parser_data.pop();
                        self.parser_data.push(ParserData::Expression(
                            ASTExpression::ASTFunctionCallExpression(call),
                        ));
                    }
                } else {
                    return Err(format!("expected function call: {}", self.get_position(0)));
                }
            } else {
                self.state.push(ParserState::Expression);
            }
        }
        Ok(())
    }

    fn process_function_def(&mut self, token: Token) -> Result<(), String> {
        if let Some(ParserData::FunctionDefParameter(param_def)) = self.last_parser_data() {
            if let Some(ParserData::FunctionDef(mut def)) = self.before_last_parser_data() {
                def.parameters.push(param_def);
                let l = self.parser_data.len();
                self.parser_data[l - 2] = ParserData::FunctionDef(def);
                self.parser_data.pop();
            } else {
                return Err(format!(
                    "Expected to be inside a function def, but {}",
                    OptionDisplay(&self.before_last_parser_data())
                ));
            }
        } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
            if let Some(TokenKind::Punctuation(PunctuationKind::RightArrow)) =
                self.get_token_kind_n(1)
            {
                self.i += 1;
            } else if let Some(TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open)) =
                self.get_token_kind_n(1)
            {
                self.i += 1;
            } else if let Some(TokenKind::NativeBlock(_body)) = self.get_token_kind_n(1) {
                self.i += 1;
            } else {
                return Err(format!(
                    "Expected return type or block : {}",
                    self.get_position(1)
                ));
            }
        } else if let TokenKind::Punctuation(PunctuationKind::RightArrow) = token.kind {
            self.state.push(ParserState::FunctionDefReturnType);
            self.i += 1;
        } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
            self.state.push(ParserState::FunctionBody);
            self.state.push(ParserState::Statement);
            self.i += 1;
        } else if let TokenKind::NativeBlock(body) = &token.kind {
            if let Some(ParserData::FunctionDef(mut def)) = self.last_parser_data() {
                def.body = NativeBody(body.into());
                self.functions.push(def);
                self.parser_data.pop();
                self.state.pop();
                self.i += 1;
            } else {
                return Err(format!(
                    "Expected to be inside a function def, but {}",
                    OptionDisplay(&self.last_parser_data())
                ));
            }
        } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
            self.state.push(ParserState::FunctionDefParameter);
            self.i += 1;
        } else if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
            self.functions.push(def);
            self.parser_data.pop();
            self.state.pop();
        } else {
            return Err(format!(
                "Expected to be inside a function def, but {}",
                OptionDisplay(&self.last_parser_data())
            ));
        }
        Ok(())
    }

    fn add_parameter_to_call_and_update_parser_data(
        &mut self,
        mut call: ASTFunctionCall,
        ast_expression: ASTExpression,
    ) {
        call.push_parameter(ast_expression);
        let l = self.parser_data.len();
        self.parser_data[l - 1] = ParserData::FunctionCall(call);
    }

    pub fn print_module(module: &ASTModule) {
        module.body.iter().for_each(|call| {
            println!("{call}");
        });
        println!();
        module.functions.iter().for_each(Self::print_function_def)
    }

    pub fn print_function_def(f: &ASTFunctionDef) {
        print!("{}", f);
        match &f.body {
            RASMBody(expressions) => {
                println!(" {{");
                expressions.iter().for_each(|statement| {
                    println!("  {statement}");
                });
                println!("}}");
            }
            NativeBody(body) => println!(" {{\n{body}\n}}"),
        }
    }

    fn get_state(&self) -> Option<&ParserState> {
        self.state.last()
    }

    fn debug(&self, message: &str) {
        debug_i!("{}", message);
        debug_i!("i {}", self.i);
        debug_i!(
            "token {:?} : {}",
            self.get_token().map(|it| &it.kind),
            self.get_position(0)
        );
        debug_i!("state {:?}", self.state);
        debug_i!("data {}", SliceDisplay(&self.parser_data));
        debug_i!("");
    }

    fn try_parse_function_call(
        &mut self,
    ) -> Option<(String, Vec<ASTType>, usize, usize, Option<String>)> {
        if let Some(TokenKind::AlphaNumeric(ref f_name)) = self.get_token_kind() {
            let (function_name, next_n, function_name_n, target) =
                if let Some((function_name, next_n)) = self.try_parse_call_with_target() {
                    (function_name, next_n, next_n - 1, Some(f_name.clone()))
                } else {
                    (f_name.clone(), 1, 0, None)
                };
            let context_generic_types =
                if let Some(function_def) = self.last_parser_data_function_def() {
                    function_def.generic_types.clone()
                } else {
                    Vec::new()
                };
            let mut generic_types = Vec::new();
            let mut n = next_n;
            if let Some(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open)) =
                self.get_token_kind_n(n)
            {
                n += 1;
                loop {
                    if let Some(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Close)) =
                        self.get_token_kind_n(n)
                    {
                        n += 1;
                        break;
                    }
                    if let Some(TokenKind::Punctuation(PunctuationKind::Comma)) =
                        self.get_token_kind_n(n)
                    {
                        n += 1;
                        continue;
                    }
                    match TypeParser::new(self).try_parse_ast_type(n, &context_generic_types) {
                        Ok(Some((ast_type, next_i))) => {
                            generic_types.push(ast_type);
                            n = next_i - self.i;
                        }
                        Ok(None) => {
                            self.errors.push(ParserError::new(
                                self.get_position(n),
                                "Cannot parse type.".to_string(),
                            ));
                            break;
                        }
                        Err(err) => {
                            self.errors
                                .push(ParserError::new(self.get_position(n), err));
                            break;
                        }
                    }
                }
            }
            if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                self.get_token_kind_n(n)
            {
                return Some((
                    function_name,
                    generic_types,
                    self.i + n + 1,
                    function_name_n,
                    target,
                ));
            }
        }
        None
    }

    fn last_parser_data_function_def(&self) -> Option<&ASTFunctionDef> {
        if let Some(ParserData::FunctionDef(def)) = self.parser_data.iter().rev().find(|it| {
            if let ParserData::FunctionDef(def) = it {
                // def.is_empty is a fake lambda function def
                !def.name.is_empty()
            } else {
                false
            }
        }) {
            Some(def)
        } else {
            None
        }
    }

    fn try_parse_call_with_target(&self) -> Option<(String, usize)> {
        if let (
            Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            Some(TokenKind::AlphaNumeric(name)),
        ) = (
            self.get_token_kind_n(1),
            self.get_token_kind_n(2),
            self.get_token_kind_n(3),
        ) {
            Some((name.clone(), 4))
        } else {
            None
        }
    }

    fn try_parse_function_def(
        &self,
    ) -> Result<Option<(Token, Vec<String>, ASTModifiers, usize)>, String> {
        if let Some(matcher_result) = FUNCTION_DEF_MATCHER.match_tokens(self, 0) {
            let param_types = matcher_result.group_alphas("type");
            let mut name_index = 1;
            let modifiers = if matcher_result.group_tokens("modifiers").is_empty() {
                ASTModifiers::private()
            } else {
                name_index += 1;
                ASTModifiers::public()
            };
            Ok(Some((
                matcher_result.get_token_n(name_index).unwrap().clone(),
                param_types,
                modifiers,
                self.get_i() + matcher_result.next_n(),
            )))
        } else {
            Ok(None)
        }
    }

    fn try_parse_parameter_def_name(&self) -> Option<(String, usize)> {
        if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind() {
            if let Some(TokenKind::Punctuation(PunctuationKind::Colon)) = self.get_token_kind_n(1) {
                return Some((name.into(), self.i + 2));
            }
        }
        None
    }

    fn before_last_parser_data(&self) -> Option<ParserData> {
        let i = self.parser_data.len();
        if i > 1 {
            self.parser_data.get(i - 2).cloned()
        } else {
            None
        }
    }

    fn last_parser_data(&self) -> Option<ParserData> {
        self.parser_data.last().cloned()
    }

    fn parse_lambda_parameters(
        &self,
        out_n: usize,
    ) -> Result<(Vec<(String, ASTPosition)>, usize), String> {
        let mut n = out_n;
        let mut parameter_names = Vec::new();
        loop {
            let kind_o = self.get_token_kind_n(n);
            match kind_o {
                None => {
                    return Err(format!(
                        "No token parsing lambda parameters: {}",
                        self.get_position(n)
                    ));
                }
                Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close)) => {
                    n += 1;
                    break;
                }
                Some(TokenKind::Punctuation(PunctuationKind::Comma)) => {
                    n += 1;
                    continue;
                }
                Some(TokenKind::AlphaNumeric(name)) => {
                    parameter_names.push((name.to_string(), self.get_position(n)));
                    n += 1;
                    continue;
                }
                _ => {
                    return Err(format!(
                        "Unexpected token {:?}: {}",
                        kind_o.unwrap(),
                        self.get_position(n),
                    ));
                }
            }
        }
        Ok((parameter_names, self.i + n))
    }

    fn try_parse_val(&self) -> Result<Option<(ASTExpression, usize)>, String> {
        if let Some(TokenKind::KeyWord(KeywordKind::True)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::Value(ASTValueType::Boolean(true), self.get_position(0)),
                self.get_i() + 1,
            )));
        } else if let Some(TokenKind::KeyWord(KeywordKind::False)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::Value(ASTValueType::Boolean(false), self.get_position(0)),
                self.get_i() + 1,
            )));
        } else if let Some(TokenKind::Number(n)) = self.get_token_kind() {
            let (value, next_i) = if let Some(TokenKind::Punctuation(PunctuationKind::Dot)) =
                self.get_token_kind_n(1)
            {
                if let Some(TokenKind::Number(n1)) = self.get_token_kind_n(2) {
                    (
                        ASTValueType::F32((n.to_owned() + "." + n1).parse().map_err(|err| {
                            format!(
                                "Cannot parse '{n}.{n1}' as an f32, {err}: {}",
                                self.get_position(0)
                            )
                        })?),
                        self.get_i() + 3,
                    )
                } else {
                    (
                        ASTValueType::I32(n.parse().map_err(|err| {
                            format!(
                                "Cannot parse '{n}' as an i32, {err}: {}",
                                self.get_position(0)
                            )
                        })?),
                        self.get_i() + 1,
                    )
                }
            } else {
                (
                    ASTValueType::I32(n.parse().map_err(|err| {
                        format!(
                            "Cannot parse '{n}' as an i32, {err}: {}",
                            self.get_position(0)
                        )
                    })?),
                    self.get_i() + 1,
                )
            };

            return Ok(Some((
                ASTExpression::Value(value, self.get_position(0)),
                next_i,
            )));
        } else if let Some(TokenKind::CharLiteral(c)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::Value(ASTValueType::Char(c.clone()), self.get_position(0)),
                self.get_i() + 1,
            )));
        } else if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::ValueRef(name.clone(), self.get_position(0)),
                self.get_i() + 1,
            )));
        } else if let Some(TokenKind::StringLiteral(s)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::Value(ASTValueType::String(s.clone()), self.get_position(0)),
                self.get_i() + 1,
            )));
        }
        Ok(None)
    }

    fn try_parse_let(&self, is_const: bool) -> Result<Option<(String, usize)>, String> {
        let kind = if is_const {
            KeywordKind::Const
        } else {
            KeywordKind::Let
        };
        if Some(&TokenKind::KeyWord(kind)) == self.get_token_kind() {
            if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind_n(1) {
                if let Some(TokenKind::Punctuation(PunctuationKind::Equal)) =
                    self.get_token_kind_n(2)
                {
                    return Ok(Some((name.clone(), self.get_i() + 3)));
                } else {
                    return Err(format!(
                        "expected = got {:?}: {}",
                        self.get_token_kind_n(2),
                        self.get_position(2)
                    ));
                }
            } else {
                return Err(format!(
                    "expected name, got {:?}: {}",
                    self.get_token_kind_n(1),
                    self.get_position(1)
                ));
            }
        }
        Ok(None)
    }

    fn parse_type_def(&self) -> Result<Option<(ASTTypeDef, usize)>, String> {
        let mut base_n = 0;
        let modifiers = if let Some(TokenKind::KeyWord(KeywordKind::Pub)) = self.get_token_kind() {
            base_n += 1;
            ASTModifiers::public()
        } else {
            ASTModifiers::private()
        };
        if let Some(TokenKind::KeyWord(KeywordKind::Type)) = self.get_token_kind_n(base_n) {
            if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind_n(base_n + 1) {
                let (type_parameters, next_i) = if let Some((type_parameters, next_i)) =
                    TypeParamsParser::new(self).try_parse(base_n + 2)?
                {
                    (type_parameters, next_i)
                } else {
                    (Vec::new(), self.get_i() + base_n + 2)
                };

                let mut n = next_i - self.get_i();
                if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                    self.get_token_kind_n(n)
                {
                    let is_ref = match self.get_token_kind_n(n + 1) {
                        None => {
                            return Err(format!(
                                "Unexpected end of stream: {}",
                                self.get_position(0)
                            ))
                        }
                        Some(TokenKind::KeyWord(KeywordKind::False)) => false,
                        Some(TokenKind::KeyWord(KeywordKind::True)) => true,
                        Some(k) => {
                            return Err(format!(
                                "Expected a boolean value but got {k}: {}",
                                self.get_position(0)
                            ))
                        }
                    };

                    let native_type = if let Some(TokenKind::Punctuation(PunctuationKind::Comma)) =
                        self.get_token_kind_n(n + 2)
                    {
                        match self.get_token_kind_n(n + 3) {
                            Some(TokenKind::StringLiteral(native_type_name)) => {
                                n += 2;
                                Some(native_type_name.clone())
                            }
                            Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close)) => {
                                None
                            }
                            _ => {
                                return Err(format!(
                                "Unexpected native type, it should be a string but it is {}: {}",
                                OptionDisplay(&self.get_token_kind_n(n + 3)),
                                self.get_position(n + 3)
                            ));
                            }
                        }
                    } else {
                        None
                    };

                    if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close)) =
                        self.get_token_kind_n(n + 2)
                    {
                        Ok(Some((
                            ASTTypeDef {
                                type_parameters,
                                name: name.clone(),
                                is_ref,
                                position: self.get_position(base_n + 1).clone(),
                                modifiers,
                                native_type,
                            },
                            self.get_i() + n + 3,
                        )))
                    } else {
                        Err(format!("Error parsing type: {}", self.get_position(0)))
                    }
                } else {
                    Err(format!(
                        "Expected '(' but got '{:?}': {}",
                        self.get_token_kind_n(n),
                        self.get_position(n)
                    ))
                }
            } else {
                Err(format!(
                    "Error parsing Type, expected identifier: {}",
                    self.get_position(0)
                ))
            }
        } else {
            Ok(None)
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
}

pub trait ParserTrait {
    fn get_i(&self) -> usize;
    fn get_token_n(&self, n: usize) -> Option<&Token>;
    fn get_token_kind(&self) -> Option<&TokenKind> {
        self.get_token_kind_n(0)
    }
    fn get_token_kind_n(&self, n: usize) -> Option<&TokenKind> {
        self.get_token_n(n).map(|token| &token.kind)
    }
    fn get_token(&self) -> Option<&Token> {
        self.get_token_n(0)
    }

    fn get_position(&self, n: usize) -> ASTPosition {
        self.get_token_n(n)
            .map(|it| it.position.clone())
            .unwrap_or(ASTPosition::none())
    }

    fn wrap_error(&self, message: &str) -> String {
        format!("{message}: {}", self.get_position(0))
    }

    fn error(&self, n: usize, message: String) -> ParserError {
        ParserError {
            position: self.get_position(n),
            message,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use rasm_utils::test_utils::init_minimal_log;
    use rasm_utils::SliceDisplay;

    use crate::lexer::Lexer;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModifiers, ASTModule,
        ASTPosition, ASTStatement, ASTType, ASTValueType, BuiltinTypeKind,
    };
    use crate::parser::Parser;

    use super::ParserError;

    fn init() {
        // let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test() {
        parse("resources/test/helloworld.rasm");
    }

    #[test]
    fn test2() {
        let module = parse("resources/test/test2.rasm");

        assert_eq!(1, module.body.len());
    }

    #[test]
    fn test8() {
        let module = parse("resources/test/test8.rasm");

        assert!(!module.functions.is_empty());
        assert_eq!(2, module.functions.get(0).unwrap().parameters.len());
    }

    #[test]
    fn test9() {
        let module = parse("resources/test/test9.rasm");

        let par =
            if let Some(ASTStatement::Expression(ASTExpression::ASTFunctionCallExpression(e))) =
                module.body.get(0)
            {
                Some(e)
            } else {
                None
            };

        assert!(!module.body.is_empty());
        assert_eq!(1, par.unwrap().parameters().len());

        let nprint_parameter = par.unwrap().parameters().get(0);

        if let Some(ASTExpression::ASTFunctionCallExpression(call)) = nprint_parameter {
            assert_eq!("add", call.function_name());
            assert_eq!(2, call.parameters().len());
            if let Some(ASTExpression::Value(ASTValueType::I32(n), _)) = call.parameters().get(0) {
                assert_eq!(10, *n);
            } else {
                panic!();
            }
            if let Some(ASTExpression::Value(ASTValueType::I32(n), _)) = call.parameters().get(1) {
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
        parse("resources/test/test10.rasm");
    }

    #[test]
    fn test11() {
        parse("resources/test/test11.rasm");
    }

    #[test]
    fn function_def_with_type_parameters() {
        let lexer = Lexer::new("fn p<T,T1>() {}".into());

        let parser = Parser::new(lexer);

        let (module, _) = parser.parse();

        let function_def = ASTFunctionDef {
            name: "p".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: Vec::new(),
            return_type: ASTType::Unit,
            inline: false,
            generic_types: vec!["T".into(), "T1".into()],
            position: ASTPosition::new(1, 4),
            modifiers: ASTModifiers::private(),
            target: None,
        };

        assert_eq!(module.functions, vec![function_def]);
    }

    #[test]
    fn test12() {
        let module = parse("resources/test/test12.rasm");

        let f_def = module.functions.first().unwrap();
        assert_eq!(f_def.return_type, ASTType::Builtin(BuiltinTypeKind::I32));
    }

    #[test]
    fn test14() {
        let (module, errors) = parse_with_errors("resources/test/test14.rasm");

        assert!(!errors.is_empty());

        let function_names = module
            .functions
            .iter()
            .map(|it| it.name.as_str())
            .collect::<Vec<_>>();

        assert_eq!(vec!["fun1", "fun2", "fun3"], function_names);
    }

    #[test]
    fn test_sruct() {
        let (module, errors) = parse_with_errors("resources/test/test_struct.rasm");

        println!("errors {}", SliceDisplay(&errors));
        assert!(errors.is_empty());
        assert!(!module.structs.is_empty());
    }

    #[test]
    fn function_call_with_generics() {
        let lexer = Lexer::new("println<i32>(20);".into());

        let parser = Parser::new(lexer);

        let (module, _errors) = parser.parse();

        let function_call = ASTFunctionCall::new(
            "println".to_string(),
            vec![ASTExpression::Value(
                ASTValueType::I32(20),
                ASTPosition::new(1, 14),
            )],
            ASTPosition::new(1, 1),
            vec![ASTType::Builtin(BuiltinTypeKind::I32)],
            None,
        );

        assert_eq!(
            module.body.first().unwrap(),
            &ASTStatement::Expression(ASTExpression::ASTFunctionCallExpression(function_call))
        );
    }

    #[test]
    fn function_call_with_generics_1() {
        let lexer = Lexer::new("fn function<T>(it: T) { println<T>(it); }".into());

        let parser = Parser::new(lexer);

        let (module, _) = parser.parse();

        let function_call = ASTFunctionCall::new(
            "println".to_string(),
            vec![ASTExpression::ValueRef(
                "it".to_string(),
                ASTPosition::new(1, 36),
            )],
            ASTPosition::new(1, 25),
            vec![ASTType::Generic(ASTPosition::new(1, 34), "T".to_string())],
            None,
        );

        if let ASTFunctionBody::RASMBody(ref body) = module.functions.first().unwrap().body {
            assert_eq!(
                body.first().unwrap(),
                &ASTStatement::Expression(ASTExpression::ASTFunctionCallExpression(function_call))
            );
        } else {
            panic!()
        }
    }

    #[test]
    fn enum_constructor() {
        let lexer = Lexer::new("Some(20);".into());

        let parser = Parser::new(lexer);

        let (module, _errors) = parser.parse();

        let function_call = ASTFunctionCall::new(
            "Some".to_string(),
            vec![ASTExpression::Value(
                ASTValueType::I32(20),
                ASTPosition::new(1, 6),
            )],
            ASTPosition::new(1, 1),
            Vec::new(),
            None,
        );

        assert_eq!(
            module.body.first().unwrap(),
            &ASTStatement::Expression(ASTExpression::ASTFunctionCallExpression(function_call))
        );
    }

    #[test]
    fn test_old_lambda_syntax() {
        let (_, mut errors) = parse_with_errors("resources/test/test16.rasm");

        assert!(!errors.is_empty());

        println!("errors {}", SliceDisplay(&errors));

        let error = errors.remove(0);
        assert_eq!(error.message, "Unexpected token `->`".to_string());
    }

    #[test]
    fn unexpected_end_of_block() {
        let (_, mut errors) = parse_with_errors("resources/test/test17.rasm");

        for error in errors.iter() {
            println!("{error}");
        }

        assert_eq!(1, errors.len());

        let error = errors.remove(0);

        assert_eq!(error.message, "Unexpected end of block.".to_string());
    }

    #[test]
    fn invalid_sequence() {
        init();

        let lexer = Lexer::new("something(current.len -2);".to_string());

        let parser = Parser::new(lexer);

        let (_, errors) = parser.parse();

        // println!("errors {}", SliceDisplay(&errors));

        assert!(!errors.is_empty());

        assert!(format!("{}", errors.last().unwrap()).contains("Expected expression"));
    }

    #[test]
    fn invalid_sequence1() {
        init();

        let lexer = Lexer::new("something(,1);".to_string());

        let parser = Parser::new(lexer);

        let (_, errors) = parser.parse();

        // println!("errors {}", SliceDisplay(&errors));

        assert!(!errors.is_empty());

        assert!(format!("{}", errors.get(0).unwrap()).contains("expected expression"));
    }

    #[test]
    fn valid_sequence() {
        init();

        let lexer = Lexer::new("something(current.len, -2);".to_string());

        let parser = Parser::new(lexer);

        let (_, errors) = parser.parse();

        assert!(errors.is_empty());
    }

    #[test]
    fn test_incomplete_statements() {
        init_minimal_log();
        init();

        let (module, errors) = parse_with_errors("resources/test/incomplete_statements.rasm");

        assert_eq!(module.structs.len(), 1);
        assert_eq!(module.functions.len(), 1);

        assert!(!errors.is_empty());
    }

    fn parse(source: &str) -> ASTModule {
        init();
        let path = Path::new(source);
        let lexer = Lexer::from_file(path).unwrap();
        let parser = Parser::new(lexer);
        let (module, errors) = parser.parse();
        if !errors.is_empty() {
            for error in errors {
                println!("{error}");
            }
            panic!()
        }
        module
    }

    fn parse_with_errors(source: &str) -> (ASTModule, Vec<ParserError>) {
        init();
        let path = Path::new(source);
        let lexer = Lexer::from_file(path).unwrap();
        let parser = Parser::new(lexer);
        parser.parse()
    }
}
