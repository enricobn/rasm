use crate::lexer::tokens::BracketKind::Round;
use crate::lexer::tokens::BracketStatus::{Close, Open};
use crate::lexer::tokens::{BracketKind, KeywordKind, PunctuationKind, TokenKind};
use crate::parser::ast::ASTType::{Builtin, Custom, Parametric};
use crate::parser::ast::{ASTType, ASTTypeRef, BuiltinTypeKind};
use crate::parser::ParserTrait;

pub struct TypeParser<'a> {
    parser: &'a dyn ParserTrait,
}

impl<'a> TypeParser<'a> {
    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse_type_ref(
        &self,
        n: usize,
        context_param_types: &[String],
    ) -> Option<(ASTTypeRef, usize)> {
        self.try_parse_type_ref_rec(n, context_param_types, 0)
    }

    fn try_parse_type_ref_rec(
        &self,
        n: usize,
        context_param_types: &[String],
        rec: usize,
    ) -> Option<(ASTTypeRef, usize)> {
        if let Some(kind) = self.parser.get_token_kind_n(n) {
            if let TokenKind::Punctuation(PunctuationKind::And) = kind {
                if let Some((ast_type, next_i)) = self.try_parse(n + 1, context_param_types, rec) {
                    return Some((
                        ASTTypeRef {
                            ast_ref: true,
                            ast_type,
                        },
                        next_i,
                    ));
                }
            } else if let Some((ast_type, next_i)) = self.try_parse(n, context_param_types, rec) {
                return Some((
                    ASTTypeRef {
                        ast_ref: false,
                        ast_type,
                    },
                    next_i,
                ));
            }
        }
        None
    }

    fn try_parse(
        &self,
        n: usize,
        context_param_types: &[String],
        rec: usize,
    ) -> Option<(ASTType, usize)> {
        if let Some(kind) = self.parser.get_token_kind_n(n) {
            let next_i = self.parser.get_i() + n + 1;
            if let TokenKind::AlphaNumeric(type_name) = kind {
                if type_name == "i32" {
                    Some((Builtin(BuiltinTypeKind::ASTI32), next_i))
                } else if type_name == "str" {
                    Some((Builtin(BuiltinTypeKind::ASTString), next_i))
                } else if context_param_types.contains(type_name) {
                    Some((Parametric(type_name.into()), next_i))
                } else {
                    let (param_types, next_i) = if let Some((param_types, next_i)) =
                        self.try_parse_parameter_types(n + 1, context_param_types, rec)
                    {
                        (param_types, next_i)
                    } else {
                        (vec![], next_i)
                    };

                    Some((
                        Custom {
                            name: type_name.into(),
                            param_types,
                        },
                        next_i,
                    ))
                }
            } else if let TokenKind::KeyWord(KeywordKind::Fn) = kind {
                Some(self.parse_fn(n + 1, context_param_types, rec))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn try_parse_parameter_types(
        &self,
        n: usize,
        context_param_types: &[String],
        rec: usize,
    ) -> Option<(Vec<ASTTypeRef>, usize)> {
        let mut types = Vec::new();
        let mut next_i = self.parser.get_i() + n + 1;

        if rec > 10 {
            panic!()
        }

        if let Some(TokenKind::Bracket(BracketKind::Angle, Open)) = self.parser.get_token_kind_n(n)
        {
            let mut inner_n = n + 1;
            loop {
                let kind = self.parser.get_token_kind_n(inner_n);
                if let Some(TokenKind::Bracket(BracketKind::Angle, Close)) = kind {
                    next_i += 1;
                    break;
                } else if let Some(TokenKind::Punctuation(PunctuationKind::Comma)) = kind {
                    inner_n += 1;
                } else if let Some((ast_type, inner_next_i)) =
                self.try_parse_type_ref(inner_n, context_param_types)
                {
                    types.push(ast_type);
                    next_i = inner_next_i;
                    inner_n = next_i - self.parser.get_i();
                } else {
                    panic!("Error parsing type");
                }
            }
            Some((types, next_i))
        } else {
            None
        }
    }

    fn parse_fn(
        &self,
        out_n: usize,
        context_param_types: &[String],
        rec: usize,
    ) -> (ASTType, usize) {
        let mut n = out_n;
        let mut parameters = Vec::new();

        if rec > 10 {
            panic!();
        }

        if let Some(TokenKind::Bracket(Round, Open)) = self.parser.get_token_kind_n(n) {
            n += 1;
            loop {
                if let Some(TokenKind::Bracket(Round, Close)) = self.parser.get_token_kind_n(n) {
                    n += 1;
                    break;
                } else if let Some(TokenKind::Punctuation(PunctuationKind::Comma)) =
                self.parser.get_token_kind_n(n)
                {
                    n += 1;
                    continue;
                }

                if let Some((t, next_i)) =
                self.try_parse_type_ref_rec(n, context_param_types, rec + 1)
                {
                    parameters.push(t);
                    n = next_i - self.parser.get_i();
                    continue;
                } else {
                    self.parser.panic("Error parsing fn type parameter");
                    panic!();
                }
            }

            if let Some(TokenKind::Punctuation(PunctuationKind::RightArrow)) =
            self.parser.get_token_kind_n(n)
            {
                n += 1;
            } else {
                self.parser.panic("Error parsing fn type: expected -> ");
                panic!();
            }

            let return_type = if let (
                Some(TokenKind::Bracket(Round, Open)),
                Some(TokenKind::Bracket(Round, Close)),
            ) = (
                self.parser.get_token_kind_n(n),
                self.parser.get_token_kind_n(n + 1),
            ) {
                n += 2;
                None
            } else if let Some((t, next_i)) =
            self.try_parse_type_ref_rec(n, context_param_types, rec + 1)
            {
                n = next_i - self.parser.get_i();
                Some(Box::new(t))
            } else {
                self.parser.panic("Error parsing fn type parameter");
                panic!();
            };

            (
                Builtin(BuiltinTypeKind::Lambda {
                    return_type,
                    parameters,
                }),
                self.parser.get_i() + n,
            )
        } else {
            self.parser.panic("Error parsing fn type: expected (");
            panic!();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::lambda_unit;
    use crate::parser::test_utils::get_parser;

    #[test]
    fn test_i32() {
        let parse_result = try_parse("i32");
        assert_eq!(Some((Builtin(BuiltinTypeKind::ASTI32), 1)), parse_result);
    }

    #[test]
    fn test_str() {
        let parse_result = try_parse("str");
        assert_eq!(Some((Builtin(BuiltinTypeKind::ASTString), 1)), parse_result);
    }

    #[test]
    fn test_lambda() {
        let parse_result = try_parse("fn() -> ()");
        assert_eq!(Some((lambda_unit(), 6)), parse_result);
    }

    #[test]
    fn test_lambda1() {
        let parse_result = try_parse("fn(&i32,&str) -> &i32");
        assert_eq!(format!("{:?}", parse_result), "Some((Builtin(Lambda { parameters: [ASTTypeRef { ast_type: Builtin(ASTI32), ast_ref: true }, ASTTypeRef { ast_type: Builtin(ASTString), ast_ref: true }], return_type: Some(ASTTypeRef { ast_type: Builtin(ASTI32), ast_ref: true }) }), 11))");
    }

    #[test]
    fn test_lambda2() {
        let parse_result = try_parse("fn(fn() -> (),&str) -> &i32");
        assert_eq!(format!("{:?}", parse_result), "Some((Builtin(Lambda { parameters: [ASTTypeRef { ast_type: Builtin(Lambda { parameters: [], return_type: None }), ast_ref: false }, ASTTypeRef { ast_type: Builtin(ASTString), ast_ref: true }], return_type: Some(ASTTypeRef { ast_type: Builtin(ASTI32), ast_ref: true }) }), 15))");
    }

    #[test]
    fn test_custom_type() {
        let parse_result = try_parse_with_context("Dummy<T,T1>", &["T".into(), "T1".into()]);
        assert_eq!(
            Some((
                Custom {
                    name: "Dummy".into(),
                    param_types: vec![
                        ASTTypeRef::parametric("T", false),
                        ASTTypeRef::parametric("T1", false),
                    ],
                },
                6
            )),
            parse_result
        );
    }

    #[test]
    fn test_param_type() {
        let parse_result = try_parse_with_context("T", &["T".into()]);
        assert_eq!(Some((Parametric("T".into()), 1)), parse_result);
    }

    #[test]
    fn test_not_param_type() {
        let parse_result = try_parse_with_context("T", &["F".into()]);
        assert_eq!(
            Some((
                Custom {
                    name: "T".into(),
                    param_types: vec![],
                },
                1
            )),
            parse_result
        );
    }

    #[test]
    fn test_complex_type() {
        let parse_result = try_parse_with_context("List<Option<T>>", &["T".into()]);
        let option_t =
            ASTTypeRef::custom("Option", false, vec![ASTTypeRef::parametric("T", false)]);
        assert_eq!(
            Some((
                ASTTypeRef::custom("List", false, vec![option_t]).ast_type,
                7
            )),
            parse_result
        );
    }

    fn try_parse(source: &str) -> Option<(ASTType, usize)> {
        try_parse_with_context(source, &[])
    }

    fn try_parse_with_context(source: &str, context: &[String]) -> Option<(ASTType, usize)> {
        let parser = get_parser(source);

        let sut = TypeParser::new(&parser);

        sut.try_parse(0, context, 0)
    }
}
