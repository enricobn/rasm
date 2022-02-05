use crate::Lexer;
use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, Token, TokenKind};
use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef, ASTType, ASTTypeRef, BuiltinTypeKind};
use crate::parser::ast::ASTFunctionBody::ASMBody;
use crate::parser::ParserStackElement::{FunctionBody, FunctionCall, FunctionDef, FunctionDefName, FunctionParametersDef, TypeRef};
use crate::parser::ParserState::FunctionCallArgument;

pub(crate) mod ast;

pub struct Parser {
    tokens: Vec<Token>,
    body: Vec<ASTFunctionCall>,
    functions: Vec<ASTFunctionDef>,
    i: usize,
    statuses: Vec<ParserStackElement>,
    state: ParserState
}

#[derive(Clone, Debug)]
enum ParserStackElement {
    FunctionCall(ASTFunctionCall),
    FunctionDef(ASTFunctionDef),
    FunctionDefName,
    FunctionBody,
    FunctionParametersDef(ASTParameterDef),
    TypeRef(Option<ASTTypeRef>),
}

#[derive(Clone, Debug)]
enum ParserState {
    None,
    FunctionCallArgument,
    FunctionDef,
    FunctionDefName,
    FunctionBody,
    FunctionParameterDef,
    TypeRef,
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
        Self { tokens, body: Vec::new(), functions: Vec::new(), i: 0, statuses: Vec::new(),
            state: ParserState::None}
    }

    pub fn parse(&mut self) -> ASTModule {
        self.body = Vec::new();
        self.functions = Vec::new();
        self.i = 0;
        self.statuses = Vec::new();
        self.state = ParserState::None;

        while self.i < self.tokens.len() {
            let token = self.get_token().unwrap();

            //let next2 = self.tokens.get(i + 2);
            //let next3 = self.tokens.get(i + 3);

            //println!("token {:?}", token);
            //println!("statuses {:?}", statuses);

            match self.state {
                ParserState::None => {
                    if !self.statuses.is_empty() {
                        panic!("Error {:?}", token)
                    }
                    if let Some((function_name, next_i)) = self.try_parse_function_call() {
                        self.statuses.push(ParserStackElement::FunctionCall(ASTFunctionCall{function_name: function_name.into(),
                            parameters: Vec::new()}));
                        self.state = FunctionCallArgument;
                        self.i = next_i;
                        continue;
                    } else if let Some((function_name, next_i)) = self.try_parse_function_def() {
                        self.statuses.push(ParserStackElement::FunctionDef(ASTFunctionDef {
                            name: function_name.into(),
                            parameters: Vec::new(),
                            body: ASTFunctionBody::RASMBody(Vec::new()),
                            return_type: None,
                        }));
                        self.state = ParserState::FunctionParameterDef;
                        self.i = next_i;
                        continue;
                    } else if let Some((function_name, next_i)) = self.try_parse_asm_def() {
                        self.statuses.push(ParserStackElement::FunctionDef(ASTFunctionDef {
                            name: function_name.into(),
                            parameters: Vec::new(),
                            body: ASTFunctionBody::ASMBody("".into()),
                            return_type: None,
                        }));
                        self.state = ParserState::FunctionParameterDef;
                        self.i = next_i;
                        continue;
                    }
                    panic!("Error {:?}", token);
                }
                ParserState::FunctionCallArgument => {
                    if let Some(FunctionCall(mut call)) = self.last_status() {
                        if let TokenKind::StringLiteral(ref value) = token.kind {
                            call.parameters.push(ASTExpression::StringLiteral(value.clone()));
                            let l = self.statuses.len();
                            self.statuses[l - 1] = FunctionCall(call);
                            self.i += 1;
                            continue;
                        } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
                            if let Some(next_token) = self.next_token() {
                                if let TokenKind::Punctuation(PunctuationKind::SemiColon) = next_token.kind {
                                    self.body.push(call);
                                    self.statuses.pop();
                                    self.state = ParserState::None;
                                    self.i += 2;
                                    continue;
                                }
                            }
                        } else if let TokenKind::Punctuation(PunctuationKind::Comma) = token.kind {
                            self.i += 1;
                            continue;
                        }
                    }
                    self.debug_error("Error");
                }
                ParserState::FunctionDef => {
                    if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
                        let tr =
                            if let Some((type_ref, next_i)) = self.try_parse_type_ref() {
                                self.i = next_i;
                                Some(type_ref)
                            } else {
                                None
                            };
                        if let Some(ParserStackElement::FunctionParametersDef(param_def)) = self.last_status() {
                            if let Some(ParserStackElement::FunctionDef(mut def)) = self.before_last_status() {
                                def.parameters.push(param_def);
                                def.return_type = tr;
                                let l = self.statuses.len();
                                self.statuses[l - 2] = FunctionDef(def);
                                self.statuses.pop();
                            }
                        } else if let Some(ParserStackElement::FunctionDef(mut def)) = self.last_status() {
                            def.return_type = tr;
                            let l = self.statuses.len();
                            self.statuses[l - 1] = FunctionDef(def);
                        } else {
                            self.debug_error("");
                        }
                        self.i += 1;
                        continue;
                    } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
                        self.state = ParserState::FunctionBody;
                    } else if let TokenKind::AsmBLock(body) = &token.kind {
                        if let Some(ParserStackElement::FunctionDef(mut def)) = self.last_status() {
                            def.body = ASMBody(body.into());
                            self.functions.push(def);
                            self.statuses.pop();
                            self.state = ParserState::None;
                        }
                    } else {
                        self.state = ParserState::FunctionParameterDef;
                    }
                }
                ParserState::FunctionParameterDef => {
                    if let Some((name, next_i)) = self.try_parse_parameter_def_name() {
                        self.i = next_i;

                        if let Some((type_ref, next_i)) = self.try_parse_type_ref() {
                            self.i = next_i;
                            self.statuses.push(ParserStackElement::FunctionParametersDef(ASTParameterDef {name, type_ref}));
                            self.state = ParserState::FunctionDef;
                            continue;
                        } else {
                            // TODO it's a trick to panic, since I don't know why, but if I don't do it,
                            //   I get an error on self.i = next_i;
                            self.state = ParserState::None;
                            continue;
                        }
                    } else {
                        self.state = ParserState::FunctionDef;
                        continue;
                    }
                }
                ParserState::FunctionBody => {
                    // TODO
                    if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close) = token.kind {
                        if let Some(FunctionDef(def)) = self.last_status() {
                            self.statuses.pop();
                            self.functions.push(def);
                            self.state = ParserState::None;
                            self.i += 1;
                            continue;
                        }
                        self.debug_error("");
                    }
                }
                _ => {}
            }

            self.i += 1;

            //actual.push(token);
        }

        ASTModule { body: self.body.clone(), functions: self.functions.clone() }
    }

    pub fn print(module: &ASTModule) {
        module.body.iter().for_each(|call| {
            print!("{}(", call.function_name);
            call.parameters.iter().for_each(|par| {
                match par {
                    ASTExpression::StringLiteral(s) => print!("\"{}\"", s),
                    ASTExpression::ASTFunctionCallExpression(_) => {
                        // TODO
                    }
                }
            });
            println!(");");
        });
        module.functions.iter().for_each(|f| {
            match f.body {
                ASTFunctionBody::RASMBody(_) => print!("fn {}(", f.name),
                ASMBody(_) => print!("asm {}(", f.name)
            }
            f.parameters.iter().for_each(|p| {
                print!("{}:", p.name);
                if p.type_ref.ast_ref {
                    print!("&");
                }
                match &p.type_ref.ast_type {
                    ASTType::BuiltinType(bt) => {
                        match bt {
                            BuiltinTypeKind::ASTString => print!("str"),
                            BuiltinTypeKind::ASTI32 => print!("i32")
                        }
                    }
                }
            });
            println!(")");
        })
    }

    fn debug_error(&self, message: &str) {
        println!("{} {:?}", message, self.get_token());
        println!("{:?}", self.state);
        println!("statuses {:?}", self.statuses);
        panic!();
    }

    fn debug(&self, message: &str) {
        println!("{} {:?}", message, self.get_token());
        println!("{:?}", self.state);
        println!("statuses {:?}", self.statuses);
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

    fn last_token(&self) -> Option<&Token> {
        if self.tokens.len() > 1 {
            self.tokens.get(self.i - 1)
        } else {
            None
        }
    }

    fn before_last_status(&self) -> Option<ParserStackElement> {
        if self.statuses.len() > 1 {
            if let Some(ls) = self.statuses.get(self.statuses.len() - 2) {
                Some(ls.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn before_last_status2(&self) -> Option<ParserStackElement> {
        if self.statuses.len() > 2 {
            if let Some(ls) = self.statuses.get(self.statuses.len() - 3) {
                Some(ls.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn last_status(&self) -> Option<ParserStackElement> {
        if let Some(ls) = self.statuses.last() {
            Some(ls.clone())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::Lexer;
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

        Parser::print(&module);
    }
}