use lazy_static::lazy_static;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

use log::{debug, info};

use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::debug_i;
use crate::errors::{CompilationError, CompilationErrorKind};
use crate::lexer::tokens::{
    BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind,
};
use crate::lexer::Lexer;
use crate::parser::asm_def_parser::AsmDefParser;
use crate::parser::ast::ASTExpression::ASTFunctionCallExpression;
use crate::parser::ast::ASTFunctionBody::{NativeBody, RASMBody};
use crate::parser::ast::{
    ASTEnumDef, ASTExpression, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTLambdaDef,
    ASTModifiers, ASTModule, ASTNameSpace, ASTParameterDef, ASTStatement, ASTStructDef, ASTType,
    ASTTypeDef, ValueType,
};
use crate::parser::enum_parser::EnumParser;
use crate::parser::matchers::{generic_types_matcher, modifiers_matcher};
use crate::parser::struct_parser::StructParser;
use crate::parser::tokens_matcher::{TokensMatcher, TokensMatcherTrait};
use crate::parser::type_params_parser::TypeParamsParser;
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserState::StructDef;
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::utils::{OptionDisplay, SliceDisplay};

mod asm_def_parser;
pub mod ast;
mod enum_parser;
mod matchers;
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
    file_name: Option<PathBuf>,
    requires: HashSet<String>,
    externals: HashSet<String>,
    types: Vec<ASTTypeDef>,
    included_files: HashSet<PathBuf>,
    errors: Vec<CompilationError>,
}

#[derive(Clone, Debug)]
enum ParserData {
    EnumDef(ASTEnumDef),
    FunctionCall(ASTFunctionCall),
    FunctionDef(ASTFunctionDef),
    FunctionDefParameter(ASTParameterDef),
    LambdaDef(ASTLambdaDef),
    Let(String, bool, ASTIndex),
    StructDef(ASTStructDef),
    Expression(ASTExpression),
    Statement(ASTStatement),
}

impl Display for ParserData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserData::EnumDef(e) => {
                write!(f, "EnumDef({}): {}", e.name, e.index)
            }
            ParserData::FunctionCall(call) => {
                write!(f, "FunctionCall({}): {}", call, call.index)
            }
            ParserData::FunctionDef(def) => {
                write!(f, "FunctionDef({}): {}", def, def.index)
            }
            ParserData::FunctionDefParameter(def) => {
                write!(f, "FunctionDefParameter({}): {}", def, def.ast_index)
            }
            ParserData::LambdaDef(def) => {
                write!(f, "LambdaDef({}): {}", def, def.index)
            }
            ParserData::Let(name, _, index) => {
                write!(f, "Let({}): {index}", name)
            }
            ParserData::StructDef(s) => {
                write!(f, "StructDef({}): {}", s.name, s.index)
            }
            ParserData::Expression(e) => {
                write!(f, "Expression({}): {}", e, e.get_index())
            }
            ParserData::Statement(s) => {
                write!(f, "Statement({}): {}", s, s.get_index())
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
    pub fn new(lexer: Lexer, file_name: Option<PathBuf>) -> Self {
        let (lexer_tokens, lexer_errors) = lexer.process();

        let tokens = lexer_tokens
            .into_iter()
            .filter(|it| {
                !matches!(
                    it.kind,
                    TokenKind::WhiteSpaces(_)
                        | TokenKind::Comment(_)
                        | TokenKind::MultiLineComment(_)
                        | TokenKind::EndOfLine
                )
            })
            .collect();
        let included_files = if let Some(file) = &file_name {
            let mut set = HashSet::new();
            set.insert(file.clone());
            set
        } else {
            HashSet::new()
        };

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
            included_files,
            errors: lexer_errors,
        }
    }

    pub fn parse(
        &mut self,
        path: &Path,
        namespace: &ASTNameSpace,
    ) -> (ASTModule, Vec<CompilationError>) {
        if self.i > 0 {
            panic!("Cannot parse twice");
        }
        let last_token = Token::new(TokenKind::EndOfLine, Some(path.to_path_buf()), 0, 0);

        let mut count = 0;

        while self.i <= self.tokens.len() {
            count += 1;
            if count > (self.tokens.len() + 1) * 10 {
                self.add_error("undefined parse error".to_string());
                return self.get_return(&namespace, path);
            }
            let mut token = if self.i == self.tokens.len() {
                last_token.clone()
            } else {
                self.tokens.get(self.i).unwrap().clone()
            };

            self.debug("");
            debug!("");

            // uniform call with dot notation
            if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
                if let TokenKind::Punctuation(PunctuationKind::Dot) = token.kind {
                    self.i += 1;
                    if let Some((function_name, generics, next_i)) =
                        self.try_parse_function_call(namespace)
                    {
                        self.parser_data.pop();
                        let call = ASTFunctionCall {
                            namespace: namespace.clone(),
                            original_function_name: function_name.clone(),
                            function_name,
                            parameters: vec![expr],
                            index: self.get_index(0),
                            generics,
                        };
                        self.parser_data.push(ParserData::FunctionCall(call));
                        self.state.push(ParserState::FunctionCall);
                        self.i = next_i;
                        continue;
                    } else if let Some(TokenKind::AlphaNumeric(name)) =
                        self.get_token_kind().cloned()
                    {
                        self.parser_data.pop();
                        let call = ASTFunctionCall {
                            namespace: namespace.clone(),
                            original_function_name: name.clone(),
                            function_name: name.clone(),
                            parameters: vec![expr],
                            index: self.get_index(0),
                            generics: Vec::new(),
                        };
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
                None => match self.process_none(&namespace, path, &token) {
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
                    match self.process_function_def_parameter(namespace, &token) {
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
                        match TypeParser::new(self).try_parse_ast_type(
                            &namespace,
                            0,
                            &def.generic_types,
                        ) {
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
                        match ENUM_PARSER.parse_variants(&namespace, self, &def.type_parameters, 0)
                        {
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
                        match STRUCT_PARSER.parse_properties(
                            namespace,
                            self,
                            &def.type_parameters,
                            &def.name,
                            0,
                        ) {
                            Ok(result) => {
                                if let Some((properties, next_i)) = result {
                                    def.properties = properties;
                                    self.structs.push(def);
                                    self.state.pop();
                                    self.parser_data.pop();
                                    self.i = next_i;
                                    continue;
                                } else {
                                    self.add_error("Expected properties".to_string());
                                }
                            }
                            Err(message) => self.add_error(message),
                        }
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
                Some(ParserState::Expression) => match self.process_expression(&namespace) {
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

        self.functions.append(&mut self.included_functions);

        self.get_return(namespace, path)
    }

    fn get_return(
        &mut self,
        namespace: &ASTNameSpace,
        path: &Path,
    ) -> (ASTModule, Vec<CompilationError>) {
        let module = ASTModule {
            path: path.to_path_buf(),
            body: self.body.clone(),
            functions: self.functions.clone(),
            enums: self.enums.clone(),
            structs: self.structs.clone(),
            requires: self.requires.clone(),
            externals: self.externals.clone(),
            types: self.types.clone(),
            namespace: namespace.clone(),
        };

        /*
        println!("Module: {}", path.to_string_lossy());
        println!("-----------------------");
        Parser::print_module(&module);
        println!("-----------------------");
        println!();
         */

        (module, self.errors.clone())
    }

    fn add_error(&mut self, message: String) {
        debug_i!("parser error: {message}");

        let index = self
            .get_token_n(0)
            .map(|it| it.index())
            .unwrap_or_else(|| ASTIndex::new(self.file_name.clone(), 0, 0));

        self.errors.push(CompilationError {
            index,
            error_kind: CompilationErrorKind::Parser(message),
        });
    }

    ///
    /// returns true if the parser must continue
    ///
    fn process_none(
        &mut self,
        namespace: &ASTNameSpace,
        path: &Path,
        token: &Token,
    ) -> Result<bool, String> {
        if let Some(ParserData::Statement(stmt)) = self.last_parser_data() {
            self.body.push(stmt);
            self.parser_data.pop();
        } else if let TokenKind::EndOfLine = token.kind {
            return Ok(false);
        } else if !self.parser_data.is_empty() {
            return Err(format!("Error: {}", self.get_index(0)));
        } else if let Some((name_token, generic_types, modifiers, next_i)) =
            self.try_parse_function_def(namespace)?
        {
            let name = name_token.alpha().unwrap().clone();
            let name_len = name.len();
            self.parser_data
                .push(ParserData::FunctionDef(ASTFunctionDef {
                    original_name: name.clone(),
                    name,
                    parameters: Vec::new(),
                    body: RASMBody(Vec::new()),
                    return_type: ASTType::Unit,
                    inline: false,
                    generic_types,
                    resolved_generic_types: ResolvedGenericTypes::new(),
                    index: name_token.index().mv_left(name_len),
                    modifiers,
                    namespace: namespace.clone(),
                }));
            self.state.push(ParserState::FunctionDef);
            self.state.push(ParserState::FunctionDefParameter);
            self.i = next_i;
        } else if let Some((name_token, inline, param_types, modifiers, next_i)) =
            AsmDefParser::new(self).try_parse()?
        {
            let name = name_token.alpha().unwrap();
            let name_len = name.len();
            self.parser_data
                .push(ParserData::FunctionDef(ASTFunctionDef {
                    original_name: name.clone(),
                    name,
                    parameters: Vec::new(),
                    body: NativeBody("".into()),
                    return_type: ASTType::Unit,
                    inline,
                    generic_types: param_types,
                    resolved_generic_types: ResolvedGenericTypes::new(),
                    index: name_token.index().mv_left(name_len),
                    modifiers,
                    namespace: namespace.clone(),
                }));
            self.state.push(ParserState::FunctionDef);
            self.state.push(ParserState::FunctionDefParameter);
            self.i = next_i;
        } else if let Some((resource, next_i)) = self.try_parse_include()? {
            let source_file = path.with_file_name(resource);
            info!("include {}", source_file.to_str().unwrap());

            self.included_files.insert(source_file.clone());

            match Lexer::from_file(source_file.as_path()) {
                Ok(lexer) => {
                    let mut parser = Parser::new(lexer, Some(source_file.clone()));
                    // TODO include should be deprecated, but some tests relies on it
                    //      this is a trick, we cannot know the exact namespace looking only at the file name
                    let namespace = ASTNameSpace::new(
                        "".to_string(),
                        path.with_extension("")
                            .file_name()
                            .unwrap()
                            .to_string_lossy()
                            .to_string(),
                    );
                    let (mut module, errors) = parser.parse(source_file.as_path(), &namespace);
                    self.errors.extend(errors);
                    if !module.body.is_empty() {
                        return Err(format!(
                            "Cannot include a module with a body: {:?}: {}",
                            module.body,
                            self.get_index(0)
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
                    return Err(format!(
                        "Error running lexer for {:?} {err}: {}",
                        source_file.to_str().unwrap(),
                        self.get_index(0)
                    ));
                }
            }

            self.i = next_i;
        } else if let Some((name, type_params, modifiers, next_i)) =
            ENUM_PARSER.try_parse(namespace, self)
        {
            self.parser_data.push(ParserData::EnumDef(ASTEnumDef {
                namespace: namespace.clone(),
                name: name.alpha().unwrap(),
                type_parameters: type_params,
                variants: Vec::new(),
                index: name.index().mv_left(name.alpha().unwrap().len()),
                modifiers,
            }));
            self.state.push(ParserState::EnumDef);
            self.i = next_i;
        } else if let Some((name_token, type_params, modifiers, next_i)) =
            STRUCT_PARSER.try_parse(namespace, self)
        {
            self.parser_data.push(ParserData::StructDef(ASTStructDef {
                namespace: namespace.clone(),
                name: name_token.alpha().unwrap(),
                type_parameters: type_params,
                properties: Vec::new(),
                index: name_token
                    .index()
                    .mv_left(name_token.alpha().unwrap().len()),
                modifiers,
            }));
            self.state.push(StructDef);
            self.i = next_i;
        } else if let TokenKind::KeyWord(KeywordKind::Requires) = token.kind {
            if let Some(TokenKind::StringLiteral(name)) = &self.get_token_kind_n(1) {
                self.requires.insert(name.clone());
                self.i += 2;
            } else {
                return Err(format!("Cannot parse require: {}", self.get_index(0)));
            }
        } else if let TokenKind::KeyWord(KeywordKind::Extern) = token.kind {
            if let Some(TokenKind::StringLiteral(name)) = self.get_token_kind_n(1) {
                self.externals.insert(name.clone());
                self.i += 2;
            } else {
                return Err(format!("Cannot parse external: {}", self.get_index(0)));
            }
        } else if let Some((type_def, next_i)) = self.parse_type_def(namespace)? {
            let token = self.get_token_kind_n(next_i - self.i);
            if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) = token {
                self.types.push(type_def);
                self.i = next_i + 1;
            } else {
                return Err(format!(
                    "Missing semicolon, got {:?} : {}",
                    token,
                    self.get_index(0)
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
                return Err(format!(
                    "Found semicolon without an expression: {}",
                    self.get_index(0)
                ));
            }
        } else if Some(&TokenKind::Bracket(
            BracketKind::Brace,
            BracketStatus::Close,
        )) == self.get_token_kind()
        {
            // Here probably we have an empty function...
            self.state.pop();
        } else if let Some((name, next_i)) = self.try_parse_let(false)? {
            self.parser_data
                .push(ParserData::Let(name, false, self.get_index(1)));
            self.state.push(ParserState::Let);
            self.state.push(ParserState::Expression);
            self.i = next_i;
        } else if let Some((name, next_i)) = self.try_parse_let(true)? {
            self.parser_data
                .push(ParserData::Let(name, true, self.get_index(1)));
            self.state.push(ParserState::Let);
            self.state.push(ParserState::Expression);
            self.i = next_i;
        } else {
            self.state.push(ParserState::Expression);
        }
        Ok(())
    }

    fn process_expression(&mut self, namespace: &ASTNameSpace) -> Result<(), String> {
        if let Some(ParserData::Expression(_exp)) = self.last_parser_data() {
            self.state.pop();
        } else if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
            self.get_token_kind()
        {
            self.state.pop();
            self.i += 1;
        } else if let Some((function_name, generics, next_i)) =
            self.try_parse_function_call(namespace)
        {
            let call = ASTFunctionCall {
                namespace: namespace.clone(),
                original_function_name: function_name.clone(),
                function_name,
                parameters: Vec::new(),
                index: self.get_index(0),
                generics,
            };
            self.parser_data.push(ParserData::FunctionCall(call));
            self.state.push(ParserState::FunctionCall);
            self.i = next_i;
        } else if let Some((expression, next_i)) = self.try_parse_val()? {
            self.parser_data.push(ParserData::Expression(expression));
            self.state.pop();
            self.i = next_i;
        } else if Some(&TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open))
            == self.get_token_kind()
        {
            let (parameter_names, next_i) = self.parse_lambda_parameters(1)?;
            self.parser_data.push(ParserData::LambdaDef(ASTLambdaDef {
                parameter_names,
                body: Vec::new(),
                index: self.get_index(0),
            }));
            let fake_function_def = ASTFunctionDef {
                original_name: String::new(),
                name: String::new(),
                parameters: Vec::new(),
                body: RASMBody(Vec::new()),
                return_type: ASTType::Unit,
                inline: false,
                generic_types: Vec::new(),
                resolved_generic_types: ResolvedGenericTypes::new(),
                index: ASTIndex::none(),
                modifiers: ASTModifiers::public(),
                namespace: namespace.clone(),
            };
            self.parser_data
                .push(ParserData::FunctionDef(fake_function_def));
            self.state.push(ParserState::LambdaExpression);
            self.state.push(ParserState::FunctionBody);
            self.i = next_i;
        } else if Some(&TokenKind::Bracket(
            BracketKind::Brace,
            BracketStatus::Close,
        )) == self.get_token_kind()
        {
            // Here probably we have an empty function...
            self.state.pop();
        } else {
            return Err(format!("Expected expression : {}", self.get_index(0)));
        }
        Ok(())
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
                            self.get_index(0)
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
                return Err(format!("Expected body: {}", self.get_index(0)));
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

    fn process_function_def_parameter(
        &mut self,
        namespace: &ASTNameSpace,
        token: &Token,
    ) -> Result<(), String> {
        if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
            if let Some((name, next_i)) = self.try_parse_parameter_def_name() {
                let _n = next_i - self.i;
                self.i = next_i;
                if let Some((ast_type, next_i)) =
                    TypeParser::new(self).try_parse_ast_type(namespace, 0, &def.generic_types)?
                {
                    self.i = next_i;
                    self.parser_data
                        .push(ParserData::FunctionDefParameter(ASTParameterDef {
                            name,
                            ast_type,
                            ast_index: token.index(),
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
            Err(format!("Illegal state: {}", self.get_index(0)))
        }
    }

    fn set_parser_data(&mut self, parser_data: ParserData, n: usize) {
        let l = self.parser_data.len();
        self.parser_data[l - n - 1] = parser_data;
    }

    /*
    fn get_index_from_token(&self, token: &Token) -> ASTIndex {
        ASTIndex {
            file_name: self.file_name.clone(),
            row: token.row,
            column: token.column,
        }
    }

     */

    fn process_function_call(&mut self, token: Token) -> Result<(), String> {
        if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
            self.i += 1;
        } else if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
            if let Some(ParserData::FunctionCall(call)) = self.before_last_parser_data() {
                self.parser_data.pop();
                self.add_parameter_to_call_and_update_parser_data(call, expr);
            } else {
                return Err(format!("expected function call: {}", self.get_index(0)));
            }
        } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
            if let Some(ParserData::FunctionCall(call)) = self.last_parser_data() {
                self.state.pop();
                self.parser_data.pop();
                self.parser_data.push(ParserData::Expression(
                    ASTExpression::ASTFunctionCallExpression(call),
                ));
                self.i += 1;
            } else {
                return Err(format!("expected function call: {}", self.get_index(0)));
            }
        } else {
            self.state.push(ParserState::Expression);
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
            } else if let Some(TokenKind::NativeBLock(_body)) = self.get_token_kind_n(1) {
                self.i += 1;
            } else {
                return Err(format!(
                    "Expected return type or block : {}",
                    self.get_index(1)
                ));
            }
        } else if let TokenKind::Punctuation(PunctuationKind::RightArrow) = token.kind {
            self.state.push(ParserState::FunctionDefReturnType);
            self.i += 1;
        } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
            self.state.push(ParserState::FunctionBody);
            self.state.push(ParserState::Statement);
            self.i += 1;
        } else if let TokenKind::NativeBLock(body) = &token.kind {
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
        call.parameters.push(ast_expression);
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
        print!("{}", f);
        match &f.body {
            RASMBody(expressions) => {
                println!(" {{");
                expressions.iter().for_each(|call| {
                    println!("  {}", call);
                });
                println!("}}");
            }
            NativeBody(_) => println!(" {{...}}"),
        }
    }

    fn get_state(&self) -> Option<&ParserState> {
        self.state.last()
    }

    fn error_msg(&self, message: &str) -> String {
        let token_option = self.tokens.get(self.i);

        if let Some(token) = token_option {
            let index = ASTIndex {
                file_name: self.file_name.clone(),
                row: token.row,
                column: token.column,
            };
            format!("{}: {}", message, index)
        } else {
            format!("{} {:?} : in end of file", message, self.file_name)
        }
    }

    fn debug(&self, message: &str) {
        debug!("{}", message);
        debug!("i {}", self.i);
        debug!(
            "token {:?} : {}",
            self.get_token().map(|it| &it.kind),
            self.get_index(0)
        );
        debug!("state {:?}", self.state);
        debug!("data {}", SliceDisplay(&self.parser_data));
    }

    fn try_parse_function_call(
        &mut self,
        namespace: &ASTNameSpace,
    ) -> Option<(String, Vec<ASTType>, usize)> {
        if let Some(TokenKind::AlphaNumeric(ref f_name)) = self.get_token_kind() {
            let (function_name, next_n) =
                if let Some((variant, next_n)) = self.try_parse_enum_constructor() {
                    (f_name.clone() + "::" + &variant, next_n)
                } else {
                    (f_name.clone(), 1)
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
                    match TypeParser::new(self).try_parse_ast_type(
                        namespace,
                        n,
                        &context_generic_types,
                    ) {
                        Ok(Some((ast_type, next_i))) => {
                            generic_types.push(ast_type);
                            n = next_i - self.i;
                        }
                        Ok(None) => {
                            self.errors.push(CompilationError {
                                error_kind: CompilationErrorKind::Parser(
                                    "Cannot parse type.".to_string(),
                                ),
                                index: self.get_index(n),
                            });
                            break;
                        }
                        Err(err) => {
                            self.errors.push(CompilationError {
                                error_kind: CompilationErrorKind::Parser(err),
                                index: self.get_index(n),
                            });
                            break;
                        }
                    }
                }
            }
            if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                self.get_token_kind_n(n)
            {
                return Some((function_name.clone(), generic_types, self.i + n + 1));
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

    fn try_parse_enum_constructor(&self) -> Option<(String, usize)> {
        if let (
            Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            Some(TokenKind::Punctuation(PunctuationKind::Colon)),
            Some(TokenKind::AlphaNumeric(variant)),
        ) = (
            self.get_token_kind_n(1),
            self.get_token_kind_n(2),
            self.get_token_kind_n(3),
        ) {
            Some((variant.clone(), 4))
        } else {
            None
        }
    }

    fn try_parse_function_def(
        &self,
        namespace: &ASTNameSpace,
    ) -> Result<Option<(Token, Vec<String>, ASTModifiers, usize)>, String> {
        if let Some(matcher_result) = FUNCTION_DEF_MATCHER.match_tokens(namespace, self, 0) {
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

    fn try_parse_include(&self) -> Result<Option<(String, usize)>, String> {
        if let Some(TokenKind::KeyWord(KeywordKind::Include)) = self.get_token_kind() {
            if let Some(next_token) = self.next_token() {
                if let TokenKind::StringLiteral(include) = &next_token.kind {
                    return Ok(Some((include.into(), self.i + 2)));
                } else {
                    return Err(format!(
                        "Unexpected token {:?}: {}",
                        next_token,
                        self.get_index(1)
                    ));
                }
            } else {
                return Err(format!("Error parsing include: {}", self.get_index(0)));
            }
        }
        Ok(None)
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

    fn before_last_parser_data(&self) -> Option<ParserData> {
        let i = self.parser_data.len();
        if i > 1 {
            self.parser_data.get(i - 2).cloned()
        } else {
            None
        }
    }

    fn get_parser_data(&self, index: usize) -> Option<&ParserData> {
        let len = self.parser_data.len();
        if index < len {
            self.parser_data.get(len - index - 1)
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
    ) -> Result<(Vec<(String, ASTIndex)>, usize), String> {
        let mut n = out_n;
        let mut parameter_names = Vec::new();
        loop {
            let kind_o = self.get_token_kind_n(n);
            match kind_o {
                None => {
                    return Err(format!(
                        "No token parsing lambda parameters: {}",
                        self.get_index(n)
                    ));
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
                    parameter_names.push((name.to_string(), self.get_index(n)));
                    n += 1;
                    continue;
                }
                _ => {
                    return Err(format!(
                        "Unexpected token {:?}: {}",
                        kind_o.unwrap(),
                        self.get_index(n),
                    ));
                }
            }
        }
        Ok((parameter_names, self.i + n))
    }

    fn try_parse_val(&self) -> Result<Option<(ASTExpression, usize)>, String> {
        if let Some(TokenKind::KeyWord(KeywordKind::True)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::Value(ValueType::Boolean(true), self.get_index(0)),
                self.get_i() + 1,
            )));
        } else if let Some(TokenKind::KeyWord(KeywordKind::False)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::Value(ValueType::Boolean(false), self.get_index(0)),
                self.get_i() + 1,
            )));
        } else if let Some(TokenKind::Number(n)) = self.get_token_kind() {
            let (value, next_i) = if let Some(TokenKind::Punctuation(PunctuationKind::Dot)) =
                self.get_token_kind_n(1)
            {
                if let Some(TokenKind::Number(n1)) = self.get_token_kind_n(2) {
                    (
                        ValueType::F32((n.to_owned() + "." + n1).parse().map_err(|err| {
                            format!(
                                "Cannot parse '{n}.{n1}' as an f32, {err}: {}",
                                self.get_index(0)
                            )
                        })?),
                        self.get_i() + 3,
                    )
                } else {
                    (
                        ValueType::I32(n.parse().map_err(|err| {
                            format!("Cannot parse '{n}' as an i32, {err}: {}", self.get_index(0))
                        })?),
                        self.get_i() + 1,
                    )
                }
            } else {
                (
                    ValueType::I32(n.parse().map_err(|err| {
                        format!("Cannot parse '{n}' as an i32, {err}: {}", self.get_index(0))
                    })?),
                    self.get_i() + 1,
                )
            };

            return Ok(Some((
                ASTExpression::Value(value, self.get_index(0)),
                next_i,
            )));
        } else if let Some(TokenKind::CharLiteral(c)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::Value(ValueType::Char(*c), self.get_index(0)),
                self.get_i() + 1,
            )));
        } else if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::ValueRef(name.clone(), self.get_index(0)),
                self.get_i() + 1,
            )));
        } else if let Some(TokenKind::StringLiteral(s)) = self.get_token_kind() {
            return Ok(Some((
                ASTExpression::StringLiteral(s.clone()),
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
                        self.get_index(2)
                    ));
                }
            } else {
                return Err(format!(
                    "expected name, got {:?}: {}",
                    self.get_token_kind_n(1),
                    self.get_index(1)
                ));
            }
        }
        Ok(None)
    }

    fn parse_type_def(
        &self,
        namespace: &ASTNameSpace,
    ) -> Result<Option<(ASTTypeDef, usize)>, String> {
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

                let n = next_i - self.get_i();
                if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                    self.get_token_kind_n(n)
                {
                    let is_ref = match self.get_token_kind_n(n + 1) {
                        None => {
                            return Err(format!("Unexpected end of stream: {}", self.get_index(0)))
                        }
                        Some(TokenKind::KeyWord(KeywordKind::False)) => false,
                        Some(TokenKind::KeyWord(KeywordKind::True)) => true,
                        Some(k) => {
                            return Err(format!(
                                "Expected a boolean value but got {k}: {}",
                                self.get_index(0)
                            ))
                        }
                    };
                    if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close)) =
                        self.get_token_kind_n(n + 2)
                    {
                        Ok(Some((
                            ASTTypeDef {
                                namespace: namespace.clone(),
                                type_parameters,
                                name: name.clone(),
                                is_ref,
                                index: self.get_index(base_n + 1).mv_left(name.len()),
                                modifiers,
                            },
                            self.get_i() + n + 3,
                        )))
                    } else {
                        Err(format!("Error parsing type: {}", self.get_index(0)))
                    }
                } else {
                    Err(format!(
                        "Expected '(' but got '{:?}': {}",
                        self.get_token_kind_n(n),
                        self.get_index(n)
                    ))
                }
            } else {
                Err(format!(
                    "Error parsing Type, expected identifier: {}",
                    self.get_index(0)
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

    fn get_index(&self, n: usize) -> ASTIndex {
        self.get_token_n(n)
            .map(|it| it.index())
            .unwrap_or(ASTIndex::none())
    }

    fn wrap_error(&self, message: &str) -> String {
        format!("{message}: {}", self.get_index(0))
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::errors::CompilationError;
    use crate::lexer::Lexer;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTModifiers,
        ASTModule, ASTNameSpace, ASTStatement, ASTType, BuiltinTypeKind, ValueType,
    };
    use crate::parser::Parser;
    use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
    use crate::utils::SliceDisplay;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
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
        assert_eq!(1, par.unwrap().parameters.len());

        let nprint_parameter = par.unwrap().parameters.get(0);

        if let Some(ASTExpression::ASTFunctionCallExpression(call)) = nprint_parameter {
            assert_eq!("add", call.function_name);
            assert_eq!(2, call.parameters.len());
            if let Some(ASTExpression::Value(ValueType::I32(n), _)) = call.parameters.get(0) {
                assert_eq!(10, *n);
            } else {
                panic!();
            }
            if let Some(ASTExpression::Value(ValueType::I32(n), _)) = call.parameters.get(1) {
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
        let lexer = Lexer::new("fn p<T,T1>() {}".into(), None);

        let mut parser = Parser::new(lexer, None);

        let (module, _) = parser.parse(Path::new(""), &ASTNameSpace::global());

        let function_def = ASTFunctionDef {
            name: "p".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: Vec::new(),
            return_type: ASTType::Unit,
            inline: false,
            generic_types: vec!["T".into(), "T1".into()],
            resolved_generic_types: ResolvedGenericTypes::new(),
            original_name: "p".into(),
            index: ASTIndex::new(None, 1, 4),
            modifiers: ASTModifiers::private(),
            namespace: ASTNameSpace::global(),
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
        for error in errors {
            println!("{error}");
        }

        let function_names = module
            .functions
            .iter()
            .map(|it| it.original_name.as_str())
            .collect::<Vec<_>>();

        assert_eq!(vec!["fun1", "fun2", "fun3"], function_names);
    }

    #[test]
    fn function_call_with_generics() {
        let lexer = Lexer::new("println<i32>(20);".into(), None);

        let mut parser = Parser::new(lexer, None);

        let (module, errors) = parser.parse(Path::new("."), &ASTNameSpace::global());

        println!("{}", SliceDisplay(&errors));

        let function_call = ASTFunctionCall {
            namespace: ASTNameSpace::global(),
            function_name: "println".to_string(),
            original_function_name: "println".to_string(),
            parameters: vec![ASTExpression::Value(
                ValueType::I32(20),
                ASTIndex::new(None, 1, 16),
            )],
            generics: vec![ASTType::Builtin(BuiltinTypeKind::I32)],
            index: ASTIndex::new(None, 1, 8),
        };

        assert_eq!(
            module.body.first().unwrap(),
            &ASTStatement::Expression(ASTExpression::ASTFunctionCallExpression(function_call))
        );
    }

    #[test]
    fn function_call_with_generics_1() {
        let lexer = Lexer::new("fn function<T>(it: T) { println<T>(it); }".into(), None);

        let mut parser = Parser::new(lexer, None);

        let (module, errors) = parser.parse(Path::new("."), &ASTNameSpace::global());

        println!("{}", SliceDisplay(&errors));

        let function_call = ASTFunctionCall {
            namespace: ASTNameSpace::global(),
            function_name: "println".to_string(),
            original_function_name: "println".to_string(),
            parameters: vec![ASTExpression::ValueRef(
                "it".to_string(),
                ASTIndex::new(None, 1, 38),
            )],
            generics: vec![ASTType::Generic("T".to_string())],
            index: ASTIndex::new(None, 1, 32),
        };

        if let ASTFunctionBody::RASMBody(ref body) = module.functions.first().unwrap().body {
            assert_eq!(
                body.first().unwrap(),
                &ASTStatement::Expression(ASTExpression::ASTFunctionCallExpression(function_call))
            );
        } else {
            panic!()
        }
    }

    fn parse(source: &str) -> ASTModule {
        init();
        let path = Path::new(source);
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, Some(path.to_path_buf()));
        parser.parse(path, &ASTNameSpace::global()).0
    }

    fn parse_with_errors(source: &str) -> (ASTModule, Vec<CompilationError>) {
        init();
        let path = Path::new(source);
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, Some(path.to_path_buf()));
        parser.parse(path, &ASTNameSpace::global())
    }
}
