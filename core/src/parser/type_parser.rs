use crate::lexer::tokens::BracketKind::Round;
use crate::lexer::tokens::BracketStatus::{Close, Open};
use crate::lexer::tokens::{BracketKind, BracketStatus, KeywordKind, PunctuationKind, TokenKind};
use crate::parser::ast::ASTType::{Builtin, Custom, Generic};
use crate::parser::ast::{ASTType, BuiltinTypeKind};
use crate::parser::ParserTrait;

pub struct TypeParser<'a> {
    parser: &'a dyn ParserTrait,
}

impl<'a> TypeParser<'a> {
    pub fn new(parser: &'a dyn ParserTrait) -> Self {
        Self { parser }
    }

    pub fn try_parse_ast_type(
        &self,
        n: usize,
        context_generic_types: &[String],
    ) -> Result<Option<(ASTType, usize)>, String> {
        self.try_parse_ast_type_rec(n, context_generic_types, 0)
    }

    fn try_parse_ast_type_rec(
        &self,
        n: usize,
        context_generic_types: &[String],
        rec: usize,
    ) -> Result<Option<(ASTType, usize)>, String> {
        if let Some((ast_type, next_i)) = self.try_parse(n, context_generic_types, rec)? {
            return Ok(Some((ast_type, next_i)));
        }
        Ok(None)
    }

    fn try_parse(
        &self,
        n: usize,
        context_generic_types: &[String],
        rec: usize,
    ) -> Result<Option<(ASTType, usize)>, String> {
        let result = if let Some(kind) = self.parser.get_token_kind_n(n) {
            let next_i = self.parser.get_i() + n + 1;
            if let TokenKind::AlphaNumeric(type_name) = kind {
                if type_name == "i32" {
                    Some((Builtin(BuiltinTypeKind::I32), next_i))
                } else if type_name == "str" {
                    Some((Builtin(BuiltinTypeKind::String), next_i))
                } else if type_name == "bool" {
                    Some((Builtin(BuiltinTypeKind::Bool), next_i))
                } else if type_name == "char" {
                    Some((Builtin(BuiltinTypeKind::Char), next_i))
                } else if type_name == "f32" {
                    Some((Builtin(BuiltinTypeKind::F32), next_i))
                } else if context_generic_types.contains(type_name) {
                    Some((Generic(type_name.into()), next_i))
                } else {
                    let (param_types, next_i) = if let Some((param_types, next_i)) =
                        self.try_parse_parameter_types(n + 1, context_generic_types, rec)?
                    {
                        (param_types, next_i)
                    } else {
                        (vec![], next_i)
                    };

                    Some((
                        Custom {
                            name: type_name.into(),
                            param_types,
                            index: self.parser.get_index(n),
                        },
                        next_i,
                    ))
                }
            } else if let TokenKind::KeyWord(KeywordKind::Fn) = kind {
                Some(self.parse_fn(n + 1, context_generic_types, rec)?)
            } else if matches!(
                kind,
                TokenKind::Bracket(BracketKind::Round, BracketStatus::Open)
            ) && matches!(
                self.parser.get_token_kind_n(n + 1),
                Some(TokenKind::Bracket(BracketKind::Round, BracketStatus::Close))
            ) {
                Some((ASTType::Unit, next_i + 1))
            } else {
                None
            }
        } else {
            None
        };

        Ok(result)
    }

    fn try_parse_parameter_types(
        &self,
        n: usize,
        context_param_types: &[String],
        rec: usize,
    ) -> Result<Option<(Vec<ASTType>, usize)>, String> {
        let mut types = Vec::new();
        let mut next_i = self.parser.get_i() + n + 1;

        if rec > 10 {
            return Err(format!(
                "Probable recursion parsing parameter type: {}",
                self.parser.get_index(n)
            ));
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
                    self.try_parse_ast_type(inner_n, context_param_types)?
                {
                    types.push(ast_type);
                    next_i = inner_next_i;
                    inner_n = next_i - self.parser.get_i();
                } else {
                    return Err(format!(
                        "Error parsing type: {}",
                        self.parser.get_index(inner_n)
                    ));
                }
            }
            Ok(Some((types, next_i)))
        } else {
            Ok(None)
        }
    }

    fn parse_fn(
        &self,
        out_n: usize,
        context_param_types: &[String],
        rec: usize,
    ) -> Result<(ASTType, usize), String> {
        let mut n = out_n;
        let mut parameters = Vec::new();

        if rec > 10 {
            return Err(format!(
                "Probable recursion parsing function : {}",
                self.parser.get_index(n)
            ));
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
                    self.try_parse_ast_type_rec(n, context_param_types, rec + 1)?
                {
                    parameters.push(t);
                    n = next_i - self.parser.get_i();
                    continue;
                } else {
                    return Err(format!(
                        "Error parsing fn type parameter: {}",
                        self.parser.get_index(n)
                    ));
                }
            }

            if let Some(TokenKind::Punctuation(PunctuationKind::RightArrow)) =
                self.parser.get_token_kind_n(n)
            {
                n += 1;
            } else {
                return Err(format!(
                    "Error parsing fn type, expected '->': {}",
                    self.parser.get_index(n)
                ));
            }

            let return_type = if let (
                Some(TokenKind::Bracket(Round, Open)),
                Some(TokenKind::Bracket(Round, Close)),
            ) = (
                self.parser.get_token_kind_n(n),
                self.parser.get_token_kind_n(n + 1),
            ) {
                n += 2;
                ASTType::Unit
            } else if let Some((t, next_i)) =
                self.try_parse_ast_type_rec(n, context_param_types, rec + 1)?
            {
                n = next_i - self.parser.get_i();
                t
            } else {
                return Err(format!(
                    "Error parsing fn type parameter: {}",
                    self.parser.get_index(n)
                ));
            };

            Ok((
                Builtin(BuiltinTypeKind::Lambda {
                    return_type: Box::new(return_type),
                    parameters,
                }),
                self.parser.get_i() + n,
            ))
        } else {
            Err(format!(
                "Error parsing fn type parameter, expected '(': {}",
                self.parser.get_index(n)
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{lambda_unit, ASTIndex};
    use crate::parser::test_utils::get_parser;

    use super::*;

    #[test]
    fn test_i32() {
        let parse_result = try_parse("i32");
        assert_eq!(Some((Builtin(BuiltinTypeKind::I32), 1)), parse_result);
    }

    #[test]
    fn test_str() {
        let parse_result = try_parse("str");
        assert_eq!(Some((Builtin(BuiltinTypeKind::String), 1)), parse_result);
    }

    #[test]
    fn test_lambda() {
        let parse_result = try_parse("fn() -> ()");
        assert_eq!(Some((lambda_unit(), 6)), parse_result);
    }

    #[test]
    fn test_lambda1() {
        let parse_result = try_parse("fn(i32,str) -> i32");
        assert_eq!(format!("{:?}", parse_result), "Some((Builtin(Lambda { parameters: [Builtin(I32), Builtin(String)], return_type: Builtin(I32) }), 8))");
    }

    #[test]
    fn test_lambda2() {
        let parse_result = try_parse("fn(fn() -> (),str) -> i32");
        assert_eq!(format!("{:?}", parse_result), "Some((Builtin(Lambda { parameters: [Builtin(Lambda { parameters: [], return_type: Unit }), Builtin(String)], return_type: Builtin(I32) }), 13))");
    }

    #[test]
    fn test_custom_type() {
        let parse_result = try_parse_with_context("Dummy<T,T1>", &["T".into(), "T1".into()]);
        assert_eq!(
            Some((
                Custom {
                    name: "Dummy".into(),
                    param_types: vec![Generic("T".into()), Generic("T1".into()),],
                    index: ASTIndex::new(None, 1, 6)
                },
                6
            )),
            parse_result
        );
    }

    #[test]
    fn test_param_type() {
        let parse_result = try_parse_with_context("T", &["T".into()]);
        assert_eq!(Some((Generic("T".into()), 1)), parse_result);
    }

    #[test]
    fn test_not_param_type() {
        let parse_result = try_parse_with_context("T", &["F".into()]);
        assert_eq!(
            Some((
                Custom {
                    name: "T".into(),
                    param_types: vec![],
                    index: ASTIndex::new(None, 1, 2)
                },
                1
            )),
            parse_result
        );
    }

    #[test]
    fn test_complex_type() {
        let parse_result = try_parse_with_context("List<Option<T>>", &["T".into()]);
        let option_t = Custom {
            name: "Option".into(),
            param_types: vec![Generic("T".into())],
            index: ASTIndex::new(None, 1, 12),
        };
        assert_eq!(
            Some((
                Custom {
                    name: "List".into(),
                    param_types: vec![option_t],
                    index: ASTIndex::new(None, 1, 5)
                },
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

        sut.try_parse(0, context, 0).unwrap()
    }
}
