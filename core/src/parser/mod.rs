use std::collections::HashSet;
use std::fmt::{Display, Formatter};
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
use crate::parser::ast::{
    ASTEnumDef, ASTExpression, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTLambdaDef, ASTModule,
    ASTParameterDef, ASTStatement, ASTStructDef, ASTTypeDef, ValueType,
};
use crate::parser::enum_parser::EnumParser;
use crate::parser::matchers::generic_types_matcher;
use crate::parser::struct_parser::StructParser;
use crate::parser::tokens_matcher::{TokensMatcher, TokensMatcherTrait};
use crate::parser::type_params_parser::TypeParamsParser;
use crate::parser::type_parser::TypeParser;
use crate::parser::ParserState::StructDef;
use crate::utils::SliceDisplay;

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
    included_files: HashSet<String>,
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
                write!(f, "EnumDef({})", e.name)
            }
            ParserData::FunctionCall(call) => {
                write!(f, "FunctionCall({})", call)
            }
            ParserData::FunctionDef(def) => {
                write!(f, "FunctionDef({})", def)
            }
            ParserData::FunctionDefParameter(def) => {
                write!(f, "FunctionDefParameter({})", def)
            }
            ParserData::LambdaDef(def) => {
                write!(f, "LambdaDef({})", def)
            }
            ParserData::Let(name, _, _) => {
                write!(f, "Let({})", name)
            }
            ParserData::StructDef(s) => {
                write!(f, "StructDef({})", s.name)
            }
            ParserData::Expression(e) => {
                write!(f, "Expression({})", e)
            }
            ParserData::Statement(s) => {
                write!(f, "Statement({})", s)
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
    pub fn new(lexer: Lexer, file_name: Option<String>) -> Self {
        let tokens = lexer
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
        }
    }

    pub fn parse(&mut self, path: &Path, std_path: &Path) -> ASTModule {
        self.body = Vec::new();
        self.functions = Vec::new();
        self.i = 0;
        self.parser_data = Vec::new();
        self.state = Vec::new();

        let last_token = Token::new(TokenKind::EndOfLine, 0, 0);

        let mut count = 0;

        while self.i <= self.tokens.len() {
            count += 1;
            if count > 10000 {
                panic!();
            }
            let token = if self.i == self.tokens.len() {
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
                    if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        self.parser_data.pop();
                        let call = ASTFunctionCall {
                            original_function_name: function_name.clone(),
                            function_name,
                            parameters: vec![expr],
                            index: self.get_index(0).unwrap(),
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
                            original_function_name: name.clone(),
                            function_name: name.clone(),
                            parameters: vec![expr],
                            index: self.get_index(0).unwrap(),
                        };
                        self.parser_data
                            .push(ParserData::Expression(ASTFunctionCallExpression(call)));
                        self.i += 1;
                        continue;
                    }
                }
            }

            match self.get_state() {
                None => {
                    if self.process_none(path, std_path, &token) {
                        continue;
                    } else {
                        break;
                    }
                }
                Some(ParserState::FunctionCall) => {
                    self.process_function_call(token);
                    continue;
                }
                Some(ParserState::FunctionDef) => {
                    self.process_function_def(token);
                    continue;
                }
                Some(ParserState::FunctionDefParameter) => {
                    self.process_function_def_parameter(&token);
                    continue;
                }
                Some(ParserState::FunctionBody) => {
                    self.process_function_body(&token);
                    continue;
                }
                Some(ParserState::FunctionDefReturnType) => {
                    if let Some(ParserData::FunctionDef(mut def)) = self.last_parser_data() {
                        if let Some((ast_type, next_i)) =
                            TypeParser::new(self).try_parse_ast_type(0, &def.generic_types)
                        {
                            self.i = next_i;
                            def.return_type = Some(ast_type);

                            self.set_parser_data(ParserData::FunctionDef(def), 0);

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
                            panic!("Expected variants : {}", self.get_index(0).unwrap())
                        }
                    } else {
                        panic!("Expected enum data : {}", self.get_index(0).unwrap())
                    }
                }
                Some(StructDef) => {
                    if let Some(ParserData::StructDef(mut def)) = self.last_parser_data() {
                        if let Some((properties, next_i)) = StructParser::new(self)
                            .parse_properties(&def.type_parameters, &def.name, 0)
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
                    if let TokenKind::Punctuation(PunctuationKind::SemiColon) = token.kind {
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
                    }
                    self.panic("Expected expression and semicolon");
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
                    self.panic("Error parsing lambda");
                }
                Some(ParserState::Expression) => {
                    self.process_expression();
                    continue;
                }
                Some(ParserState::Statement) => {
                    self.process_statement(token);
                    continue;
                }
            }

            self.i += 1;
        }

        self.functions.append(&mut self.included_functions);

        ASTModule {
            body: self.body.clone(),
            functions: self.functions.clone(),
            enums: self.enums.clone(),
            structs: self.structs.clone(),
            requires: self.requires.clone(),
            externals: self.externals.clone(),
            types: self.types.clone(),
            included_files: self.included_files.clone(),
        }
    }

    ///
    /// returns true if the parser must continue
    ///
    fn process_none(&mut self, path: &Path, std_path: &Path, token: &Token) -> bool {
        if let Some(ParserData::Statement(stmt)) = self.last_parser_data() {
            self.body.push(stmt);
            self.parser_data.pop();
        } else if let TokenKind::EndOfLine = token.kind {
            return false;
        } else if !self.parser_data.is_empty() {
            self.panic("Error");
        } else if let Some((name, generic_types, next_i)) = self.try_parse_function_def() {
            self.parser_data
                .push(ParserData::FunctionDef(ASTFunctionDef {
                    original_name: name.clone(),
                    name,
                    parameters: Vec::new(),
                    body: RASMBody(Vec::new()),
                    return_type: None,
                    inline: false,
                    generic_types,
                    resolved_generic_types: LinkedHashMap::new(),
                    index: self.get_index(next_i - self.i).unwrap(),
                }));
            self.state.push(ParserState::FunctionDef);
            self.state.push(ParserState::FunctionDefParameter);
            self.i = next_i;
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
                    generic_types: param_types,
                    resolved_generic_types: LinkedHashMap::new(),
                    index: self.get_index(next_i - self.i).unwrap(),
                }));
            self.state.push(ParserState::FunctionDef);
            self.state.push(ParserState::FunctionDefParameter);
            self.i = next_i;
        } else if let Some((resource, next_i)) = self.try_parse_include() {
            let mut buf = path.with_file_name(&resource);
            // First we try to get it relative to the current file,
            // then we try to get the file from the standard lib folder
            if !buf.exists() {
                buf = std_path.join(Path::new(&resource));
            }
            info!("include {}", buf.as_path().to_str().unwrap());

            let resource_path = buf.as_path();

            self.included_files
                .insert(resource_path.to_str().unwrap().to_owned());

            match Lexer::from_file(resource_path) {
                Ok(lexer) => {
                    let mut parser =
                        Parser::new(lexer, resource_path.to_str().map(|it| it.to_string()));
                    let mut module = parser.parse(resource_path, std_path);
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
                    self.included_files.extend(module.included_files)
                }
                Err(err) => {
                    self.panic(&format!(
                        "Error running lexer for {:?}: {err}",
                        resource_path.to_str()
                    ));
                }
            }

            self.i = next_i;
        } else if let Some((name, type_params, next_i)) = EnumParser::new(self).try_parse() {
            self.parser_data.push(ParserData::EnumDef(ASTEnumDef {
                name,
                type_parameters: type_params,
                variants: Vec::new(),
                index: self.get_index_from_token(token),
            }));
            self.state.push(ParserState::EnumDef);
            self.i = next_i;
        } else if let Some((name, type_params, next_i)) = StructParser::new(self).try_parse() {
            self.parser_data.push(ParserData::StructDef(ASTStructDef {
                name,
                type_parameters: type_params,
                properties: Vec::new(),
                index: self.get_index_from_token(token),
            }));
            self.state.push(StructDef);
            self.i = next_i;
        } else if let TokenKind::KeyWord(KeywordKind::Requires) = token.kind {
            if let Some(TokenKind::StringLiteral(name)) = &self.get_token_kind_n(1) {
                self.requires.insert(name.clone());
                self.i += 2;
            } else {
                self.panic("Cannot parse require");
            }
        } else if let TokenKind::KeyWord(KeywordKind::Extern) = token.kind {
            if let Some(TokenKind::StringLiteral(name)) = self.get_token_kind_n(1) {
                self.externals.insert(name.clone());
                self.i += 2;
            } else {
                self.panic("Cannot parse external");
            }
        } else if let TokenKind::KeyWord(KeywordKind::Type) = token.kind {
            let (type_def, next_i) = self.parse_type_def();
            let token = self.get_token_kind_n(next_i - self.i);
            if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) = token {
                self.types.push(type_def);
                self.i = next_i + 1;
            } else {
                self.panic(&format!("Missing semicolon, got {:?}", token))
            }
        } else {
            self.state.push(ParserState::Statement);
        }
        true
    }

    fn process_statement(&mut self, token: Token) {
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
                self.panic("Found semicolon without an expression");
            }
        } else if Some(&TokenKind::Bracket(
            BracketKind::Brace,
            BracketStatus::Close,
        )) == self.get_token_kind()
        {
            // Here probably we have an empty function...
            self.state.pop();
        } else if let Some((name, next_i)) = self.try_parse_let(false) {
            self.parser_data
                .push(ParserData::Let(name, false, self.get_index(1).unwrap()));
            self.state.push(ParserState::Let);
            self.state.push(ParserState::Expression);
            self.i = next_i;
        } else if let Some((name, next_i)) = self.try_parse_let(true) {
            self.parser_data
                .push(ParserData::Let(name, true, self.get_index(1).unwrap()));
            self.state.push(ParserState::Let);
            self.state.push(ParserState::Expression);
            self.i = next_i;
        } else {
            self.state.push(ParserState::Expression);
        }
    }

    fn process_expression(&mut self) {
        if let Some(ParserData::Expression(_exp)) = self.last_parser_data() {
            self.state.pop();
        } else if let Some(TokenKind::Punctuation(PunctuationKind::SemiColon)) =
            self.get_token_kind()
        {
            self.state.pop();
            self.i += 1;
        } else if let Some((function_name, next_i)) = self.try_parse_function_call() {
            let call = ASTFunctionCall {
                original_function_name: function_name.clone(),
                function_name,
                parameters: Vec::new(),
                index: self.get_index(0).unwrap(),
            };
            self.parser_data.push(ParserData::FunctionCall(call));
            self.state.push(ParserState::FunctionCall);
            self.i = next_i;
        } else if let Some((expression, next_i)) = self.try_parse_val() {
            self.parser_data.push(ParserData::Expression(expression));
            self.state.pop();
            self.i = next_i;
        } else if Some(&TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open))
            == self.get_token_kind()
        {
            let (parameter_names, next_i) = self.parse_lambda_parameters(1);
            self.parser_data.push(ParserData::LambdaDef(ASTLambdaDef {
                parameter_names,
                body: Vec::new(),
            }));
            let fake_function_def = ASTFunctionDef {
                original_name: String::new(),
                name: String::new(),
                parameters: Vec::new(),
                body: RASMBody(Vec::new()),
                return_type: None,
                inline: false,
                generic_types: Vec::new(),
                resolved_generic_types: LinkedHashMap::new(),
                index: ASTIndex::none(),
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
            self.panic(&format!(
                "Expected expression : {}",
                self.get_index(0).unwrap()
            ));
        }
    }

    fn process_function_body(&mut self, token: &Token) {
        if let Some(ParserData::Statement(stmt)) = self.last_parser_data() {
            if let Some(ParserData::FunctionDef(mut def)) = self.before_last_parser_data() {
                let mut statements = match &def.body {
                    RASMBody(statements) => {
                        // TODO try not to clone
                        statements.clone()
                    }
                    ASMBody(_) => self.panic("Expected function got asm"),
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
                self.panic("Expected body");
            }
        } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close) = token.kind {
            self.state.pop();
            self.i += 1;
        } else {
            self.state.push(ParserState::Statement);
        }
    }

    fn process_function_def_parameter(&mut self, token: &Token) {
        if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
            if let Some((name, next_i)) = self.try_parse_parameter_def_name() {
                let n = next_i - self.i;
                self.i = next_i;
                if let Some((ast_type, next_i)) =
                    TypeParser::new(self).try_parse_ast_type(0, &def.generic_types)
                {
                    self.i = next_i;
                    self.parser_data
                        .push(ParserData::FunctionDefParameter(ASTParameterDef {
                            name,
                            ast_type,
                            ast_index: self.get_index_from_token(token),
                        }));
                    self.state.pop();
                } else {
                    self.panic("");
                }
            } else {
                self.state.pop();
            }
        } else {
            self.panic("Illegal state");
        }
    }

    fn set_parser_data(&mut self, parser_data: ParserData, n: usize) {
        let l = self.parser_data.len();
        self.parser_data[l - n - 1] = parser_data;
    }

    fn get_index_from_token(&self, token: &Token) -> ASTIndex {
        ASTIndex {
            file_name: self.file_name.clone(),
            row: token.row,
            column: token.column,
        }
    }

    fn process_function_call(&mut self, token: Token) {
        if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
            self.i += 1;
        } else if let Some(ParserData::Expression(expr)) = self.last_parser_data() {
            if let Some(ParserData::FunctionCall(call)) = self.before_last_parser_data() {
                self.parser_data.pop();
                self.add_parameter_to_call_and_update_parser_data(call, expr);
            } else {
                self.panic("expected function call");
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
                self.panic("expected function call");
            }
        } else {
            self.state.push(ParserState::Expression);
        }
    }

    fn process_function_def(&mut self, token: Token) {
        if let Some(ParserData::FunctionDefParameter(param_def)) = self.last_parser_data() {
            if let Some(ParserData::FunctionDef(mut def)) = self.before_last_parser_data() {
                def.parameters.push(param_def);
                let l = self.parser_data.len();
                self.parser_data[l - 2] = ParserData::FunctionDef(def);
                self.parser_data.pop();
            } else {
                self.panic("Expected to be inside a function def");
            }
        } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
            if let Some(ParserData::FunctionDef(mut def)) = self.last_parser_data() {
                if let Some((ref ast_type, next_i)) =
                    TypeParser::new(self).try_parse_ast_type(0, &def.generic_types)
                {
                    self.i = next_i;
                    def.return_type = Some(ast_type.clone());
                } else {
                    def.return_type = None
                }
                let l = self.parser_data.len();
                self.parser_data[l - 1] = ParserData::FunctionDef(def);
                self.i += 1;
            } else {
                self.panic("Expected to be inside a function def");
            };
        } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
            self.state.push(ParserState::FunctionBody);
            self.state.push(ParserState::Statement);
            self.i += 1;
        } else if let TokenKind::AsmBLock(body) = &token.kind {
            if let Some(ParserData::FunctionDef(mut def)) = self.last_parser_data() {
                def.body = ASMBody(body.into());
                self.functions.push(def);
                self.parser_data.pop();
                self.state.pop();
                self.i += 1;
            } else {
                self.panic("Expected to be inside a function def");
            }
        } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
            self.state.push(ParserState::FunctionDefParameter);
            self.i += 1;
        } else if let TokenKind::Punctuation(PunctuationKind::RightArrow) = token.kind {
            self.state.push(ParserState::FunctionDefReturnType);
            self.i += 1;
        } else if let Some(ParserData::FunctionDef(def)) = self.last_parser_data() {
            self.functions.push(def);
            self.parser_data.pop();
            self.state.pop();
        } else {
            self.panic("Expected to be inside a function def");
        }
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

    fn panic(&self, message: &str) -> ! {
        self.debug(message);
        panic!("{}", self.error_msg(message));
    }

    fn debug(&self, message: &str) {
        debug!("{}", message);
        debug!("i {}", self.i);
        debug!(
            "token {:?} : {}",
            self.get_token().map(|it| &it.kind),
            self.get_index(0).unwrap_or(ASTIndex::none())
        );
        debug!("state {:?}", self.state);
        debug!("data {}", SliceDisplay(&self.parser_data));
    }

    fn try_parse_function_call(&self) -> Option<(String, usize)> {
        if let Some(TokenKind::AlphaNumeric(function_name)) = self.get_token_kind() {
            if let Some((variant, next_i)) = self.try_parse_enum_constructor() {
                return Some((function_name.clone() + "::" + &variant, next_i));
            } else if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                self.get_token_kind_n(1)
            {
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
            let param_types_matcher = generic_types_matcher();

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

    fn parse_lambda_parameters(&self, out_n: usize) -> (Vec<(String, ASTIndex)>, usize) {
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
                    parameter_names.push((name.to_string(), self.get_index(n).unwrap()));
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
            return Some((
                ASTExpression::Value(ValueType::Boolean(true), self.get_index(0).unwrap()),
                self.get_i() + 1,
            ));
        } else if let Some(TokenKind::KeyWord(KeywordKind::False)) = self.get_token_kind() {
            return Some((
                ASTExpression::Value(ValueType::Boolean(false), self.get_index(0).unwrap()),
                self.get_i() + 1,
            ));
        } else if let Some(TokenKind::Number(n)) = self.get_token_kind() {
            let (value, next_i) = if let Some(TokenKind::Punctuation(PunctuationKind::Dot)) =
                self.get_token_kind_n(1)
            {
                if let Some(TokenKind::Number(n1)) = self.get_token_kind_n(2) {
                    (
                        ValueType::F32(
                            (n.to_owned() + "." + n1)
                                .parse()
                                .unwrap_or_else(|_| panic!("Cannot parse '{n}.{n1}' as an f32")),
                        ),
                        self.get_i() + 3,
                    )
                } else {
                    (
                        ValueType::I32(
                            n.parse()
                                .unwrap_or_else(|_| panic!("Cannot parse '{n}' as an i32")),
                        ),
                        self.get_i() + 1,
                    )
                }
            } else {
                (
                    ValueType::I32(
                        n.parse()
                            .unwrap_or_else(|_| panic!("Cannot parse '{n}' as an i32")),
                    ),
                    self.get_i() + 1,
                )
            };

            return Some((
                ASTExpression::Value(value, self.get_index(0).unwrap()),
                next_i,
            ));
        } else if let Some(TokenKind::CharLiteral(c)) = self.get_token_kind() {
            return Some((
                ASTExpression::Value(ValueType::Char(*c), self.get_index(0).unwrap()),
                self.get_i() + 1,
            ));
        } else if let Some(TokenKind::AlphaNumeric(name)) = self.get_token_kind() {
            return Some((
                ASTExpression::ValueRef(name.clone(), self.get_index(0).unwrap()),
                self.get_i() + 1,
            ));
        } else if let Some(TokenKind::StringLiteral(s)) = self.get_token_kind() {
            return Some((ASTExpression::StringLiteral(s.clone()), self.get_i() + 1));
        }
        None
    }

    fn try_parse_let(&self, is_const: bool) -> Option<(String, usize)> {
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
            let (type_parameters, next_i) =
                if let Some((type_parameters, next_i)) = TypeParamsParser::new(self).try_parse(2) {
                    (type_parameters, next_i)
                } else {
                    (Vec::new(), self.get_i() + 2)
                };

            let n = next_i - self.get_i();
            if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)) =
                self.get_token_kind_n(n)
            {
                let is_ref = match self.get_token_kind_n(n + 1) {
                    None => panic!("Unexpected end of stream"),
                    Some(TokenKind::KeyWord(KeywordKind::False)) => false,
                    Some(TokenKind::KeyWord(KeywordKind::True)) => true,
                    Some(k) => panic!("Expected a boolean value but got {k}"),
                };
                if let Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close)) =
                    self.get_token_kind_n(n + 2)
                {
                    return (
                        ASTTypeDef {
                            type_parameters,
                            name: name.clone(),
                            is_ref,
                            index: self.get_index(0).unwrap(),
                        },
                        self.get_i() + n + 3,
                    );
                }
            } else {
                panic!(
                    "Expected '(' but got '{:?}': {}",
                    self.get_token_kind_n(n),
                    self.get_index(n).unwrap()
                );
            }
            panic!("Error parsing type: {}", self.get_index(0).unwrap());
        } else {
            self.panic("Error parsing Type, expected identifier")
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

    fn get_index(&self, n: usize) -> Option<ASTIndex> {
        self.get_token_n(n).map(|it| ASTIndex {
            file_name: self.file_name.clone(),
            row: it.row,
            column: it.column,
        })
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

    fn get_index(&self, n: usize) -> Option<ASTIndex>;
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::path::Path;

    use linked_hash_map::LinkedHashMap;

    use crate::lexer::Lexer;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModule, ASTStatement, ASTType,
        BuiltinTypeKind, ValueType,
    };
    use crate::parser::Parser;

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
        let lexer = Lexer::new("fn p<T,T1>() {}".into(), "a test file".into());

        let mut parser = Parser::new(lexer, None);

        let module = parser.parse(Path::new("."), Path::new(&stdlib_path()));

        let function_def = ASTFunctionDef {
            name: "p".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: Vec::new(),
            return_type: None,
            inline: false,
            generic_types: vec!["T".into(), "T1".into()],
            resolved_generic_types: LinkedHashMap::new(),
            original_name: "p".into(),
            index: ASTIndex::new(None, 1, 12),
        };

        assert_eq!(module.functions, vec![function_def]);
    }

    #[test]
    fn test12() {
        let module = parse("resources/test/test12.rasm");

        let f_def = module.functions.first().unwrap();
        assert_eq!(
            f_def.return_type,
            Some(ASTType::Builtin(BuiltinTypeKind::I32))
        );
    }

    fn parse(source: &str) -> ASTModule {
        init();
        let path = Path::new(source);
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path, Path::new(&stdlib_path()));
        module
    }

    fn stdlib_path() -> String {
        env::var("RASM_STDLIB").unwrap_or("../stdlib".to_owned())
    }
}
