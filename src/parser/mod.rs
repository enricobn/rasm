use std::path::Path;

use crate::Lexer;
use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind};
use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef, ASTReturnType, ASTType, ASTTypeRef, BuiltinTypeKind};
use crate::parser::ast::ASTFunctionBody::{ASMBody, RASMBody};
use crate::parser::ast::ASTType::BuiltinType;
use crate::parser::ParserData::{FunctionCallData, FunctionDefData};
use crate::parser::ParserState::FunctionCallState;

pub(crate) mod ast;

enum ProcessResult {
    Continue,
    Next,
    Panic
}

pub struct Parser {
    tokens: Vec<Token>,
    body: Vec<ASTFunctionCall>,
    functions: Vec<ASTFunctionDef>,
    i: usize,
    parser_data: Vec<ParserData>,
    state: Vec<ParserState>,
    included_functions: Vec<ASTFunctionDef>,
}

#[derive(Clone, Debug)]
enum ParserData {
    FunctionCallData(ASTFunctionCall),
    FunctionDefData(ASTFunctionDef),
    FunctionDefParameterData(ASTParameterDef),
}

#[derive(Clone, Debug)]
enum ParserState {
    FunctionCallState,
    FunctionDefState,
    FunctionBodyState,
    FunctionDefParameterState,
    FunctionDefReturnTypeState,
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
                self.get_token().unwrap()
            };

            /*
            self.debug("");
            println!();
             */

            match self.get_state() {
                None => {
                    if !self.parser_data.is_empty() {
                        self.panic("Error");
                    }
                    if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        self.parser_data.push(ParserData::FunctionCallData(ASTFunctionCall {
                            function_name,
                            parameters: Vec::new(),
                        }));
                        self.state.push(FunctionCallState);
                        self.i = next_i;
                        continue;
                    } else if let Some((name, next_i)) = self.try_parse_function_def() {
                        self.parser_data.push(ParserData::FunctionDefData(ASTFunctionDef {
                            name,
                            parameters: Vec::new(),
                            body: ASTFunctionBody::RASMBody(Vec::new()),
                            return_type: None,
                            inline: false,
                        }));
                        self.state.push(ParserState::FunctionDefState);
                        self.state.push(ParserState::FunctionDefParameterState);
                        self.i = next_i;
                        continue;
                    } else if let Some((name, inline, next_i)) = self.try_parse_asm_def() {
                        self.parser_data.push(ParserData::FunctionDefData(ASTFunctionDef {
                            name,
                            parameters: Vec::new(),
                            body: ASTFunctionBody::ASMBody("".into()),
                            return_type: None,
                            inline,
                        }));
                        self.state.push(ParserState::FunctionDefState);
                        self.state.push(ParserState::FunctionDefParameterState);
                        self.i = next_i;
                        continue;
                    } else if let Some((resource, next_i)) = self.try_parse_include() {
                        let buf = path.with_file_name(resource);
                        let resource_path = buf.as_path();
                        if let Ok(lexer) = Lexer::from_file(resource_path) {
                            let mut parser = Parser::new(lexer);
                            let mut module = parser.parse(resource_path);
                            if !module.body.is_empty() {
                                self.panic("Cannot include a module with a body.");
                            }
                            self.included_functions.append(&mut module.functions);
                        }
                        self.i = next_i;
                        continue;
                    } else if let TokenKind::EndOfLine = token.kind {
                        break;
                    }
                    self.panic("Unknown statement");
                }
                Some(ParserState::FunctionCallState) => {
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
                Some(ParserState::FunctionDefState) => {
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
                Some(ParserState::FunctionDefParameterState) => {
                    if let Some((name, next_i)) = self.try_parse_parameter_def_name() {
                        self.i = next_i;

                        if let Some((type_ref, next_i)) = self.try_parse_type_ref() {
                            self.i = next_i;
                            self.parser_data.push(ParserData::FunctionDefParameterData(ASTParameterDef { name, type_ref }));
                            self.state.pop();
                            continue;
                        } else {
                            self.panic("");
                        }
                    } else {
                        self.state.pop();
                        continue;
                    }
                }
                Some(ParserState::FunctionBodyState) => {
                    if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close) = token.kind {
                        self.state.pop();
                        self.i += 1;
                        continue;
                    } else if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        self.parser_data.push(ParserData::FunctionCallData(ASTFunctionCall {
                            function_name,
                            parameters: Vec::new(),
                        }));
                        self.state.push(FunctionCallState);
                        self.i = next_i;
                        continue;
                    }
                    self.panic("Error parsing function body.");
                }
                Some(ParserState::FunctionDefReturnTypeState) => {
                    if let Some((type_ref, next_i)) = self.try_parse_type_ref() {
                        self.i = next_i;
                        if let Some(ParserData::FunctionDefData(def)) = self.last_parser_data() {
                            let mut def = def.clone();
                            let (register, next_i) = self.parse_register(Self::is_asm(&def));
                            self.i = next_i;
                            def.return_type = Some(ASTReturnType { type_ref, register });
                            let l = self.parser_data.len();
                            self.parser_data[l - 1] = FunctionDefData(def);
                            self.state.pop();
                            continue;
                        }
                    }
                }
            }

            self.i += 1;

            //actual.push(token);
        }

        self.functions.append(&mut self.included_functions);

        ASTModule { body: self.body.clone(), functions: self.functions.clone() }
    }

    fn process_function_call(&mut self, token: Token) -> ProcessResult {
        if let Some(FunctionCallData(call)) = self.last_parser_data() {
            if let TokenKind::StringLiteral(value) = &token.kind {
                self.add_parameter_to_call_and_update_parser_data(call, ASTExpression::StringLiteral(value.clone()));
                return ProcessResult::Continue;
            } else if let TokenKind::Number(value) = &token.kind {
                self.add_parameter_to_call_and_update_parser_data(call, ASTExpression::Number(value.parse().unwrap()));
                return ProcessResult::Continue;
            } else if let TokenKind::AlphaNumeric(name) = &token.kind {
                if let Some((function_name, next_i)) = self.try_parse_function_call() {
                    self.parser_data.push(ParserData::FunctionCallData(ASTFunctionCall {
                        function_name,
                        parameters: Vec::new(),
                    }));
                    self.state.push(FunctionCallState);
                    self.i = next_i;
                } else {
                    self.add_parameter_to_call_and_update_parser_data(call, ASTExpression::Var(name.clone()));
                }
                return ProcessResult::Continue;
            } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
                self.i += 1;
                return ProcessResult::Continue;
            } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
                // TODO return type of the lambda for now it's not supported
                let return_type = Some(ASTReturnType { type_ref: ASTTypeRef { ast_type: BuiltinType(BuiltinTypeKind::ASTI32), ast_ref: false }, register: "eax".into() });
                self.parser_data.push(ParserData::FunctionDefData(ASTFunctionDef {
                    name: "lambda".into(),
                    parameters: Vec::new(),
                    body: ASTFunctionBody::RASMBody(Vec::new()),
                    return_type,
                    inline: false,
                }));
                self.state.push(ParserState::FunctionBodyState);
                self.i += 1;
                return ProcessResult::Continue;
            } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
                if self.is_next_token_a_semicolon() {
                    if let Some(FunctionDefData(def)) = self.before_last_parser_data() {
                        let mut def = def.clone();
                        if let ASTFunctionBody::RASMBody(mut calls) = def.body {
                            calls.push(call);
                            def.body = RASMBody(calls);
                            let l = self.parser_data.len();
                            self.parser_data[l - 2] = FunctionDefData(def);
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
                } else if let Some(FunctionCallData(before_call)) = self.before_last_parser_data() {
                    let mut before_call = before_call.clone();
                    before_call.parameters.push(ASTExpression::ASTFunctionCallExpression(call));
                    let l = self.parser_data.len();
                    self.parser_data[l - 2] = FunctionCallData(before_call);
                    self.parser_data.pop();
                    self.state.pop();
                    self.i += 1;
                    return ProcessResult::Continue;
                }
            }
        } else if let Some(FunctionDefData(def)) = self.last_parser_data() {
            if let Some(FunctionCallData(call)) = self.before_last_parser_data() {
                let mut actual_call = call.clone();
                actual_call.parameters.push(ASTExpression::Lambda(def));
                let l = self.parser_data.len();
                self.parser_data[l - 2] = FunctionCallData(actual_call);
                self.parser_data.pop();
                return ProcessResult::Continue;
            }
        }
        ProcessResult::Panic
    }

    fn process_function_def(&mut self, token: Token) -> ProcessResult {
        if let Some(ParserData::FunctionDefParameterData(param_def)) = self.last_parser_data() {
            if let Some(ParserData::FunctionDefData(def)) = self.before_last_parser_data() {
                let mut def = def.clone();
                def.parameters.push(param_def);
                let l = self.parser_data.len();
                self.parser_data[l - 2] = FunctionDefData(def);
                self.parser_data.pop();
                return ProcessResult::Continue;
            }
        } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
            let return_type =
                if let Some((type_ref, next_i)) = self.try_parse_type_ref() {
                    self.i = next_i;
                    let (register, next_i) =
                        if let Some(ParserData::FunctionDefData(def)) = self.before_last_parser_data() {
                            self.parse_register(Self::is_asm(def))
                        } else {
                            self.panic("");
                            panic!();
                        };
                    self.i = next_i;
                    Some(ASTReturnType { type_ref, register })
                } else {
                    None
                };
            if let Some(ParserData::FunctionDefData(mut def)) = self.last_parser_data() {
                def.return_type = return_type;
                let l = self.parser_data.len();
                self.parser_data[l - 1] = FunctionDefData(def);
            } else {
                self.panic("");
            }
            self.i += 1;
            return ProcessResult::Continue;
        } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
            self.state.push(ParserState::FunctionBodyState);
        } else if let TokenKind::AsmBLock(body) = &token.kind {
            if let Some(ParserData::FunctionDefData(mut def)) = self.last_parser_data() {
                def.body = ASMBody(body.into());
                self.functions.push(def);
                self.parser_data.pop();
                self.state.pop();
            }
        } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
            self.state.push(ParserState::FunctionDefParameterState);
        } else if let TokenKind::Punctuation(PunctuationKind::RightArrow) = token.kind {
            self.state.push(ParserState::FunctionDefReturnTypeState);
        } else if let Some(FunctionDefData(def)) = self.last_parser_data() {
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
        self.parser_data[l - 1] = FunctionCallData(call);
        self.i += 1;
    }

    fn is_next_token_a_semicolon(&self) -> bool {
        matches!(self.next_token(), Some(Token { row: _, column: _, kind: TokenKind::Punctuation(PunctuationKind::SemiColon) }))
    }

    fn is_asm(def: &ASTFunctionDef) -> bool {
        matches!(def.body, ASTFunctionBody::ASMBody(_))
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
            ASTFunctionBody::RASMBody(_) => print!("fn {}(", f.name),
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
            Self::print_type_ref(&return_type.type_ref);
            print!("[{}]", return_type.register);
        }
        match &f.body {
            ASTFunctionBody::RASMBody(calls) => {
                println!(" {{");
                calls.iter().for_each(|call| {
                    print!("  ");
                    Self::print_call(call, false)
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
            ASTType::BuiltinType(bt) => {
                match bt {
                    BuiltinTypeKind::ASTString => print!("str"),
                    BuiltinTypeKind::ASTI32 => print!("i32"),
                    BuiltinTypeKind::Lambda => print!("fn")
                }
            }
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
                ASTExpression::Var(name) => print!("{}", name),
                ASTExpression::Lambda(function_def) => {
                    if let ASTFunctionBody::RASMBody(calls) = &function_def.body {
                        print!("{{");
                        calls.iter().for_each(|call| {
                            Self::print_call(call, true);
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
        let option = self.get_token();

        if let Some(token) = option {
            //self.debug(message);
            format!("{}: {},{}", message, token.row, token.column)
        } else {
            "it wasn't supposed to happen!".into()
        }
    }

    fn panic(&self, message: &str) {
        panic!("{}", self.error_msg(message));
    }

    fn debug(&self, message: &str) {
        println!("{} {:?}", message, self.get_token());
        println!("state {:?}", self.state);
        println!("data {:?}", self.parser_data);
        println!("body {:?}", self.body);
        println!("functions {:?}", self.functions);
    }

    fn get_token(&self) -> Option<Token> {
        self.tokens.get(self.i).cloned()
    }

    fn get_token_n(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.i + n)
    }

    fn try_parse_function_call(&self) -> Option<(String, usize)> {
        if let Some(Token { kind: TokenKind::AlphaNumeric(function_name), row: _, column: _ }) = self.get_token() {
            if let Some(Token { kind: TokenKind::Bracket(BracketKind::Round, BracketStatus::Open), row: _, column: _ }) = self.next_token() {
                return Some((function_name, self.i + 2));
            }
        }
        None
    }

    fn try_parse_function_def(&self) -> Option<(String, usize)> {
        if let Some(Token { kind: TokenKind::KeyWord(KeywordKind::Fn), row: _, column: _ }) = self.get_token() {
            if let Some(Token { kind: TokenKind::AlphaNumeric(function_name), row: _, column: _ }) = self.next_token() {
                if let Some(Token { kind: TokenKind::Bracket(BracketKind::Round, BracketStatus::Open), row: _, column: _ }) = self.next_token2() {
                    return Some((function_name.clone(), self.i + 3));
                }
            }
        }
        None
    }

    fn try_parse_asm_def(&self) -> Option<(String, bool, usize)> {
        if let Some(token) = self.get_token() {
            if let TokenKind::KeyWord(KeywordKind::Inline) = &token.kind {
                if let Some((function_name, next_i)) = self.try_parse_asm_def_no_inline(1) {
                    return Some((function_name, true, next_i));
                }
            } else if let Some((function_name, next_i)) = self.try_parse_asm_def_no_inline(0) {
                return Some((function_name, false, next_i));
            }
        }
        None
    }

    fn try_parse_asm_def_no_inline(&self, n: usize) -> Option<(String, usize)> {
        if let Some(token) = self.get_token_n(n) {
            if let TokenKind::KeyWord(KeywordKind::Asm) = &token.kind {
                if let Some(next_token) = self.get_token_n(n + 1) {
                    if let TokenKind::AlphaNumeric(function_name) = &next_token.kind {
                        if let Some(next_token2) = self.get_token_n(n + 2) {
                            if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Open) = next_token2.kind {
                                return Some((function_name.clone(), self.i + n + 3));
                            }
                        }
                    }
                }
            }
        }
        None
    }

    fn try_parse_include(&self) -> Option<(String, usize)> {
        if let Some(token) = self.get_token() {
            if let TokenKind::KeyWord(KeywordKind::Include) = &token.kind {
                if let Some(next_token) = self.next_token() {
                    if let TokenKind::StringLiteral(include) = &next_token.kind {
                        return Some((include.into(), self.i + 2));
                    }
                }
            }
        }
        None
    }

    fn parse_register(&self, mandatory: bool) -> (String, usize) {
        if let Some(token) = self.get_token() {
            if let Some(token2) = self.next_token() {
                if let Some(token3) = self.next_token2() {
                    if let TokenKind::Bracket(BracketKind::Square, BracketStatus::Open) = token.kind {
                        if let TokenKind::AlphaNumeric(name) = &token2.kind {
                            if let TokenKind::Bracket(BracketKind::Square, BracketStatus::Close) = token3.kind {
                                return (name.into(), self.i + 3);
                            }
                        }
                    }
                }
            }
        }

        if mandatory {
            self.panic("Error parsing return type register");
            // TODO I already panicked before
            panic!()
        }
        ("eax".into(), self.i)
    }

    fn try_parse_type_ref(&self) -> Option<(ASTTypeRef, usize)> {
        if let Some(token) = self.get_token() {
            if let TokenKind::Punctuation(PunctuationKind::And) = &token.kind {
                if let Some(next_token) = self.next_token() {
                    if let Some(ast_type) = self.try_parse_ast_type(next_token) {
                        return Some((ASTTypeRef { ast_ref: true, ast_type }, self.i + 2));
                    }
                }
            } else if let Some(ast_type) = self.try_parse_ast_type(&token) {
                return Some((ASTTypeRef { ast_ref: false, ast_type }, self.i + 1));
            }
        }
        None
    }

    fn try_parse_ast_type(&self, token: &Token) -> Option<ASTType> {
        if let TokenKind::AlphaNumeric(type_name) = &token.kind {
            if type_name == "i32" {
                Some(ASTType::BuiltinType(BuiltinTypeKind::ASTI32))
            } else if type_name == "str" {
                Some(ASTType::BuiltinType(BuiltinTypeKind::ASTString))
            } else {
                self.panic(&format!("Unknown type {}", type_name));
                None
            }
        } else if let TokenKind::KeyWord(KeywordKind::Fn) = &token.kind {
            Some(ASTType::BuiltinType(BuiltinTypeKind::Lambda))
        } else {
            None
        }
    }


    fn try_parse_parameter_def_name(&self) -> Option<(String, usize)> {
        if let Some(token) = self.get_token() {
            if let TokenKind::AlphaNumeric(name) = &token.kind {
                if let Some(next_token) = self.next_token() {
                    if let TokenKind::Punctuation(PunctuationKind::Colon) = next_token.kind {
                        return Some((name.into(), self.i + 2));
                    }
                }
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
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::Lexer;
    use crate::parser::ast::ASTExpression;
    use crate::parser::Parser;

    #[test]
    fn test() {
        let path = Path::new("resources/test/helloworld.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse(path);

        Parser::print(&module);
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
        let module = parser.parse(path);

        // TODO for now I test only that it doesn't panic
        Parser::print(&module);
    }

    #[test]
    fn test11() {
        let path = Path::new("resources/test/test11.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse(path);

        // TODO for now I test only that it doesn't panic
        Parser::print(&module);
    }
}