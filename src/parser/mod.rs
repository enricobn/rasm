use crate::Lexer;
use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, Token, TokenKind};
use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef, ASTType, ASTTypeRef, BuiltinTypeKind};
use crate::parser::ast::ASTFunctionBody::ASMBody;
use crate::parser::ParserStatus::{FunctionBody, FunctionCall, FunctionDef, FunctionDefName, FunctionParametersDef, FunctionReturnType};

pub(crate) mod ast;

pub struct Parser {
    tokens: Vec<Token>,
}

#[derive(Clone,Debug)]
enum ParserStatus {
    FunctionCall(ASTFunctionCall),
    FunctionDef(ASTFunctionDef),
    FunctionDefName,
    FunctionBody,
    FunctionParametersDef,
    FunctionReturnType(Option<ASTTypeRef>),
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { tokens: lexer.collect() }
    }

    pub fn parse(&mut self) -> ASTModule {
        let mut body = Vec::new();
        let mut functions = Vec::new();

        let mut i = 0;
        let mut statuses: Vec<ParserStatus> = Vec::new();

        while i < self.tokens.len() {
            let token = self.tokens.get(i).unwrap();
            let next = self.tokens.get(i + 1);
            //let next2 = self.tokens.get(i + 2);
            //let next3 = self.tokens.get(i + 3);

            let last_status = if let Some(ls) = statuses.last() {
                Some(ls.clone())
            } else {
                None
            };

            let before_last_status =
                if statuses.len() > 1 {
                    if let Some(ls) = statuses.get(statuses.len() - 2) {
                        Some(ls.clone())
                    } else {
                        None
                    }
                } else {
                    None
                };
            //println!("token {:?}", token);
            //println!("statuses {:?}", statuses);

            match last_status {
                None => {
                    if let TokenKind::AlphaNumeric(ref name) = token.kind {
                        if let Some(next_token) = next {
                            if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Open) = next_token.kind {
                                statuses.push(ParserStatus::FunctionCall(ASTFunctionCall { function_name: name.clone(), parameters: Vec::new() }));
                                i += 1;
                            }
                        }
                    } else if let TokenKind::KeyWord(KeywordKind::Fn) = token.kind {
                        statuses.push(ParserStatus::FunctionDef(ASTFunctionDef {
                            name: "".into(),
                            parameters: Vec::new(),
                            body: ASTFunctionBody::RASMBody(Vec::new()),
                            return_type: ASTTypeRef { ast_type: None, ast_ref: false },
                        }));
                        statuses.push(FunctionDefName);
                    } else if let TokenKind::KeyWord(KeywordKind::Asm) = token.kind {
                        statuses.push(ParserStatus::FunctionDef(ASTFunctionDef {
                            name: "".into(),
                            parameters: Vec::new(),
                            body: ASTFunctionBody::ASMBody("".into()),
                            return_type: ASTTypeRef { ast_type: None, ast_ref: false },
                        }));
                        statuses.push(FunctionDefName);
                        /*
                        if let Some(_) = next { // spaces
                            if let Some(next_token2) = next2 { // name
                                if let Some(next_token3) = next3 { // (
                                    if let TokenKind::AlphaNumeric(name) = &next_token2.kind {
                                        if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Open) = next_token3.kind {
                                            statuses.push(ParserStatus::FunctionDef(ASTFunctionDef {
                                                name: name.clone(),
                                                parameters: Vec::new(),
                                                body: ASTFunctionBody::ASMBody("".into()),
                                                return_type: ASTTypeRef { ast_type: None, ast_ref: false },
                                            }));
                                            i += 4;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }

                         */
                        //panic!("Error {:?}", token)
                    }
                }
                Some(FunctionCall(mut call)) => {
                    if let TokenKind::StringLiteral(ref value) = token.kind {
                        call.parameters.push(ASTExpression::StringLiteral(value.clone()))
                    } else if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
                        body.push(call);
                        statuses.pop();
                        // TODO other parameters
                    } else {
                        panic!("Error {:?}", token)
                    }
                }
                Some(FunctionDef(_)) => {
                    if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
                        statuses.push(FunctionReturnType(None));
                    }
                }
                Some(FunctionDefName) => {
                    if let TokenKind::WhiteSpaces(_) = token.kind {

                    } else if let TokenKind::AlphaNumeric(name) = &token.kind {
                        if let Some(FunctionDef(mut def)) = before_last_status {
                            if let Some(next_token) = next {
                                if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Open) = next_token.kind {
                                    def.name = name.into();

                                    let l = statuses.len();
                                    statuses[l - 2] = FunctionDef(def);

                                    statuses.pop();
                                    statuses.push(FunctionParametersDef);
                                    i += 2;
                                    continue
                                }
                            }
                        }
                        panic!("Error {:?}", token)
                    } else {
                        panic!("Error {:?}", token)
                    }
                }
                Some(FunctionBody) => {
                    // TODO
                    if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Close) = token.kind {
                        if let Some(FunctionDef(def)) = before_last_status {
                            statuses.pop();
                            statuses.pop();
                            functions.push(def);
                        }
                    }
                }
                Some(FunctionReturnType(_)) => {
                    if let Some(FunctionDef(mut def)) = before_last_status {
                        if let TokenKind::AsmBLock(value) = &token.kind {
                            if let ASTFunctionBody::ASMBody(_) = def.body {
                                def.body = ASMBody(value.clone());
                                statuses.pop();
                                statuses.pop();
                                functions.push(def);
                                i += 1;
                                continue;
                            }
                        } else if let TokenKind::Bracket(BracketKind::Brace, BracketStatus::Open) = token.kind {
                            statuses.pop();
                            statuses.push(FunctionBody);
                            i += 1;
                            continue;
                        } else {
                            // TODO parse return type
                            i += 1;
                            continue;
                        }
                    }
                    panic!("Error {:?}", token)
                }
                Some(FunctionParametersDef) => {
                    // TODO parameters def
                    if let TokenKind::Bracket(BracketKind::Round, BracketStatus::Close) = token.kind {
                        /*
                        if let Some(FunctionDef(def)) = before_last_status {
                            statuses.pop();
                            statuses.push(ParserStatus::FunctionReturnType(None));
                            functions.push(def);
                        }

                         */
                        statuses.pop();
                        statuses.push(ParserStatus::FunctionReturnType(None));
                    }
                }
            }

            i += 1;

            //actual.push(token);
        }

        ASTModule { body, functions }
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

        println!("{:?}", module);
    }
}