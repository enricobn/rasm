use crate::Lexer;
use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind};
use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef, ASTReturnType, ASTType, ASTTypeRef, BuiltinTypeKind};
use crate::parser::ast::ASTFunctionBody::{ASMBody, RASMBody};
use crate::parser::ParserData::{FunctionCallData, FunctionDefData};
use crate::parser::ParserState::FunctionCallParameterState;

pub(crate) mod ast;

pub struct Parser {
    tokens: Vec<Token>,
    body: Vec<ASTFunctionCall>,
    functions: Vec<ASTFunctionDef>,
    i: usize,
    data: Vec<ParserData>,
    state: Vec<ParserState>,
}

#[derive(Clone, Debug)]
enum ParserData {
    FunctionCallData(ASTFunctionCall),
    FunctionDefData(ASTFunctionDef),
    FunctionDefParameterData(ASTParameterDef),
}

#[derive(Clone, Debug)]
enum ParserState {
    FunctionCallParameterState,
    FunctionDefState,
    FunctionBodyState,
    FunctionDefParameterState,
    FunctionDefReturnTypeState,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let tokens = lexer.filter(|it| {
            match it.kind {
                TokenKind::WhiteSpaces(_) => false,
                TokenKind::Comment(_) => false,
                TokenKind::EndOfLine => false,
                _ => true
            }
        }).collect();
        Self { tokens, body: Vec::new(), functions: Vec::new(), i: 0, data: Vec::new(),
            state: Vec::new()}
    }

    pub fn parse(&mut self) -> ASTModule {
        self.body = Vec::new();
        self.functions = Vec::new();
        self.i = 0;
        self.data = Vec::new();
        self.state = Vec::new();

        while self.i < self.tokens.len() {
            let token = self.get_token().unwrap();

            /*
            self.debug("");
            println!();
             */

            match self.get_state() {
                None => {
                    if !self.data.is_empty() {
                        self.debug_error("Error");
                    }
                    if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        self.data.push(ParserData::FunctionCallData(ASTFunctionCall{function_name: function_name.into(),
                            parameters: Vec::new()}));
                        self.state.push(FunctionCallParameterState);
                        self.i = next_i;
                        continue;
                    } else if let Some((function_name, next_i)) = self.try_parse_function_def() {
                        self.data.push(ParserData::FunctionDefData(ASTFunctionDef {
                            name: function_name.into(),
                            parameters: Vec::new(),
                            body: ASTFunctionBody::RASMBody(Vec::new()),
                            return_type: None,
                        }));
                        self.state.push(ParserState::FunctionDefState);
                        self.state.push(ParserState::FunctionDefParameterState);
                        self.i = next_i;
                        continue;
                    } else if let Some((function_name, next_i)) = self.try_parse_asm_def() {
                        self.data.push(ParserData::FunctionDefData(ASTFunctionDef {
                            name: function_name.into(),
                            parameters: Vec::new(),
                            body: ASTFunctionBody::ASMBody("".into()),
                            return_type: None,
                        }));
                        self.state.push(ParserState::FunctionDefState);
                        self.state.push(ParserState::FunctionDefParameterState);
                        self.i = next_i;
                        continue;
                    }
                    panic!("Error {:?}", token);
                }
                Some(ParserState::FunctionCallParameterState) => {
                    if let Some(FunctionCallData(call)) = self.last_data() {
                        if let TokenKind::StringLiteral(ref value) = token.kind {
                            let mut call = call.clone();
                            call.parameters.push(ASTExpression::StringLiteral(value.clone()));
                            let l = self.data.len();
                            self.data[l - 1] = FunctionCallData(call);
                            self.i += 1;
                            continue;
                        } else if let TokenKind::Number(value) = &token.kind {
                            let mut call = call.clone();
                            call.parameters.push(ASTExpression::Number(value.parse().unwrap()));
                            let l = self.data.len();
                            self.data[l - 1] = FunctionCallData(call);
                            self.i += 1;
                            continue;
                        } else if let TokenKind::AlphaNumeric(name) = &token.kind {
                            if let Some((name, next_i)) = self.try_parse_function_call() {
                                self.data.push(ParserData::FunctionCallData(ASTFunctionCall{function_name: name.into(),
                                    parameters: Vec::new()}));
                                self.state.push(FunctionCallParameterState);
                                self.i = next_i;
                            } else {
                                let mut call = call.clone();
                                call.parameters.push(ASTExpression::Var(name.into()));
                                let l = self.data.len();
                                self.data[l - 1] = FunctionCallData(call);
                                self.i += 1;
                            }
                            continue;
                        } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
                            self.i += 1;
                            continue;
                        } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
                            if self.is_next_token_a_semicolon() {
                                if let Some(FunctionDefData(def)) = self.before_last_data() {
                                    let mut def = def.clone();
                                    if let ASTFunctionBody::RASMBody(mut calls) = def.body {
                                        calls.push(call.clone());
                                        def.body = RASMBody(calls);
                                        let l = self.data.len();
                                        self.data[l - 2] = FunctionDefData(def);
                                        self.data.pop();
                                        self.i += 2;
                                        self.state.pop();
                                        continue;
                                    }
                                } else {
                                    let call = call.clone();
                                    self.body.push(call);
                                }
                                self.data.pop();
                                self.state.pop();
                                self.i += 2;
                                continue;
                            } else if let Some(FunctionCallData(before_call)) = self.before_last_data() {
                                let mut before_call = before_call.clone();
                                before_call.parameters.push(ASTExpression::ASTFunctionCallExpression(call.clone()));
                                let l = self.data.len();
                                self.data[l - 2] = FunctionCallData(before_call);
                                self.data.pop();
                                self.state.pop();
                                self.i += 1;
                                continue;
                            }
                        }
                    }
                    self.debug_error("Error parsing parameter");
                }
                Some(ParserState::FunctionDefState) => {
                    if let Some(ParserData::FunctionDefParameterData(param_def)) = self.last_data() {
                        if let Some(ParserData::FunctionDefData(def)) = self.before_last_data() {
                            let mut def = def.clone();
                            def.parameters.push(param_def.clone());
                            let l = self.data.len();
                            self.data[l - 2] = FunctionDefData(def);
                            self.data.pop();
                            continue;
                        }
                    } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
                        let return_type =
                            if let Some((type_ref, next_i)) = self.try_parse_type_ref() {
                                self.i = next_i;
                                let (register, next_i) =
                                    if let Some(ParserData::FunctionDefData(def)) = self.before_last_data() {
                                        self.parse_register(Self::is_asm(def))
                                    } else {
                                        self.debug_error("");
                                        panic!();
                                    };
                                self.i = next_i;
                                Some(ASTReturnType{type_ref, register})
                            } else {
                                None
                            };
                        if let Some(ParserData::FunctionDefData(def)) = self.last_data() {
                            let mut def = def.clone();
                            def.return_type = return_type;
                            let l = self.data.len();
                            self.data[l - 1] = FunctionDefData(def);
                        } else {
                            self.debug_error("");
                        }
                        self.i += 1;
                        continue;
                    } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
                        self.state.push(ParserState::FunctionBodyState);
                    } else if let TokenKind::AsmBLock(body) = &token.kind {
                        if let Some(ParserData::FunctionDefData(def)) = self.last_data() {
                            let mut def = def.clone();
                            def.body = ASMBody(body.into());
                            self.functions.push(def);
                            self.data.pop();
                            self.state.pop();
                        }
                    } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close) = token.kind {
                        if let Some(FunctionDefData(def)) = self.last_data() {
                            let def = def.clone();
                            self.data.pop();
                            self.functions.push(def);
                            self.state.pop();
                            self.i += 1;
                            continue;
                        }
                        self.debug_error("Error parsing function definition");
                    } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
                        self.state.push(ParserState::FunctionDefParameterState);
                    } else if let TokenKind::Punctuation(PunctuationKind::RightArrow) = token.kind {
                        self.state.push(ParserState::FunctionDefReturnTypeState);
                    } else {
                        self.debug_error("Error parsing function definition");
                    }
                }
                Some(ParserState::FunctionDefParameterState) => {
                    if let Some((name, next_i)) = self.try_parse_parameter_def_name() {
                        self.i = next_i;

                        if let Some((type_ref, next_i)) = self.try_parse_type_ref() {
                            self.i = next_i;
                            self.data.push(ParserData::FunctionDefParameterData(ASTParameterDef {name, type_ref}));
                            self.state.pop();
                            continue;
                        } else {
                            self.debug_error("");
                        }
                    } else {
                        self.state.pop();
                        continue;
                    }
                }
                Some(ParserState::FunctionBodyState) => {
                    if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close) = token.kind {
                        self.state.pop();
                        continue;
                    } else if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        self.data.push(ParserData::FunctionCallData(ASTFunctionCall{function_name: function_name.into(),
                            parameters: Vec::new()}));
                        self.state.push(FunctionCallParameterState);
                        self.i = next_i;
                        continue;
                    }
                }
                Some(ParserState::FunctionDefReturnTypeState) => {
                    if let Some((type_ref, next_i)) = self.try_parse_type_ref() {
                        self.i = next_i;
                        if let Some(ParserData::FunctionDefData(def)) = self.last_data() {
                            let mut def = def.clone();
                            let (register, next_i) = self.parse_register(Self::is_asm(&def));
                            self.i = next_i;
                            def.return_type = Some(ASTReturnType{type_ref, register});
                            let l = self.data.len();
                            self.data[l - 1] = FunctionDefData(def);
                            self.state.pop();
                            continue;
                        }
                    }
                }
            }

            self.i += 1;

            //actual.push(token);
        }

        ASTModule { body: self.body.clone(), functions: self.functions.clone() }
    }

    fn is_next_token_a_semicolon(&self) -> bool{
        if let Some(next_token) = self.next_token() {
            if let TokenKind::Punctuation(PunctuationKind::SemiColon) = next_token.kind {
                return true;
            }
        }
        false
    }

    fn is_asm(def: &ASTFunctionDef) -> bool {
        if let ASTFunctionBody::ASMBody(_) = def.body {
            true
        } else {
            false
        }
    }

    pub fn print(module: &ASTModule) {
        println!("main() {{");
        module.body.iter().for_each(|call| {
            print!("  ");
            Self::print_call(call, false);
        });
        println!("}}");
        module.functions.iter().for_each(|f| {
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
                Self::print_type_ref(&type_ref);
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
                },
                ASMBody(_) => println!(" {{...}}")
            }

        })
    }

    fn print_type_ref(type_ref: &ASTTypeRef) {
        if type_ref.ast_ref {
            print!("&");
        }
        match &type_ref.ast_type {
            ASTType::BuiltinType(bt) => {
                match bt {
                    BuiltinTypeKind::ASTString => print!("str"),
                    BuiltinTypeKind::ASTI32 => print!("i32")
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
                ASTExpression::ASTFunctionCallExpression(call) => Self::print_call(&call, true),
                ASTExpression::Var(name) => print!("{}", name)
            }
            first = false;
        });
        if as_expression {
            print!(")");
        } else {
            println!(");");
        }
    }

    fn debug_error(&self, message: &str) {
        println!("{} {:?}", message, self.get_token());
        println!("state {:?}", self.state);
        println!("data {:?}", self.data);
        panic!();
    }

    fn debug(&self, message: &str) {
        println!("{} {:?}", message, self.get_token());
        println!("state {:?}", self.state);
        println!("data {:?}", self.data);
    }

    fn get_token(&self) -> Option<&Token> {
        self.tokens.get(self.i)
    }

    fn try_parse_function_call(&self) -> Option<(String, usize)> {
        if let Some(token) = self.get_token() {
            if let TokenKind::AlphaNumeric(function_name) = &token.kind {
                if let Some(next_token) = self.next_token() {
                    if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Open) = next_token.kind {
                        return Some((function_name.into(), self.i + 2))
                    }
                }
            }
        }
        None
    }

    fn try_parse_function_def(&self) -> Option<(String, usize)> {
        if let Some(token) = self.get_token() {
            if let TokenKind::KeyWord(KeywordKind::Fn) = &token.kind {
                if let Some(next_token) = self.next_token() {
                    if let TokenKind::AlphaNumeric(function_name) = &next_token.kind {
                        if let Some(next_token2) = self.next_token2() {
                            if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Open) = next_token2.kind {
                                return Some((function_name.into(), self.i + 3))
                            }
                        }
                    }
                }
            }
        }
        None
    }

    fn try_parse_asm_def(&self) -> Option<(String, usize)> {
        if let Some(token) = self.get_token() {
            if let TokenKind::KeyWord(KeywordKind::Asm) = &token.kind {
                if let Some(next_token) = self.next_token() {
                    if let TokenKind::AlphaNumeric(function_name) = &next_token.kind {
                        if let Some(next_token2) = self.next_token2() {
                            if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Open) = next_token2.kind {
                                return Some((function_name.into(), self.i + 3))
                            }
                        }
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
            self.debug_error("Error parsing return type register");
            // TODO I already panicked before
            panic!()
        }
        ("eax".into(), self.i)
    }

    fn try_parse_type_ref(&self) -> Option<(ASTTypeRef, usize)> {
        if let Some(token) = self.get_token() {
            if let TokenKind::AlphaNumeric(type_name) = &token.kind {
                if type_name == "i32" {
                    return Some((ASTTypeRef { ast_ref: false, ast_type: ASTType::BuiltinType(BuiltinTypeKind::ASTI32) }, self.i + 1))
                } else if type_name == "str" {
                    return Some((ASTTypeRef { ast_ref: false, ast_type: ASTType::BuiltinType(BuiltinTypeKind::ASTString) }, self.i + 1))
                }
            } else if let TokenKind::Punctuation(PunctuationKind::And) = &token.kind {
                if let Some(next_token) = self.next_token() {
                    if let TokenKind::AlphaNumeric(type_name) = &next_token.kind {
                        if type_name == "i32" {
                            return Some((ASTTypeRef { ast_ref: true, ast_type: ASTType::BuiltinType(BuiltinTypeKind::ASTI32) }, self.i + 2))
                        } else if type_name == "str" {
                            return Some((ASTTypeRef { ast_ref: true, ast_type: ASTType::BuiltinType(BuiltinTypeKind::ASTString) }, self.i + 2))
                        }
                    }
                }
            }
        }
        None
    }

    fn try_parse_parameter_def_name(&self) -> Option<(String, usize)> {
        if let Some(token) = self.get_token() {
            if let TokenKind::AlphaNumeric(name) = &token.kind {
                if let Some(next_token) = self.next_token() {
                    if let TokenKind::Punctuation(PunctuationKind::Colon) = next_token.kind {
                        return Some((name.into(), self.i + 2))
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

    fn before_last_data(&self) -> Option<&ParserData> {
        let i = self.data.len();
        if i > 1 {
            self.data.get(i - 2)
        } else {
            None
        }
    }

    fn last_data(&self) -> Option<&ParserData> {
        self.data.last()
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
        let module = parser.parse();

        Parser::print(&module);
    }

    #[test]
    fn test2() {
        let path = Path::new("resources/test/test2.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse();

        assert_eq!(1, module.body.len());
    }

    #[test]
    fn test8() {
        let path = Path::new("resources/test/test8.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse();

        assert!(!module.functions.is_empty());
        assert_eq!(2, module.functions.get(0).unwrap().parameters.len());
    }

    #[test]
    fn test9() {
        let path = Path::new("resources/test/test9.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer);
        let module = parser.parse();

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
        let module = parser.parse();

        // TODO for now I test only that it doesn't panic
    }
}