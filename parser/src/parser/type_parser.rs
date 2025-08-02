use crate::lexer::tokens::BracketKind::Round;
use crate::lexer::tokens::BracketStatus::{self, Close, Open};
use crate::lexer::tokens::{BracketKind, KeywordKind, PunctuationKind, TokenKind};
use crate::parser::ast::ASTType::{ASTBuiltinType, ASTCustomType, ASTGenericType};
use crate::parser::ast::{ASTBuiltinTypeKind, ASTType};
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
            if let TokenKind::Reserved(reserved_kind) = kind {
                match reserved_kind {
                    crate::lexer::tokens::ReservedKind::INT => {
                        Some((ASTBuiltinType(ASTBuiltinTypeKind::ASTIntegerType), next_i))
                    }
                    crate::lexer::tokens::ReservedKind::FLOAT => {
                        Some((ASTBuiltinType(ASTBuiltinTypeKind::ASTFloatType), next_i))
                    }
                    crate::lexer::tokens::ReservedKind::STR => {
                        Some((ASTBuiltinType(ASTBuiltinTypeKind::ASTStringType), next_i))
                    }
                    crate::lexer::tokens::ReservedKind::BOOL => {
                        Some((ASTBuiltinType(ASTBuiltinTypeKind::ASTBooleanType), next_i))
                    }
                    crate::lexer::tokens::ReservedKind::CHAR => {
                        Some((ASTBuiltinType(ASTBuiltinTypeKind::ASTCharType), next_i))
                    }
                }
            } else if let TokenKind::AlphaNumeric(type_name) = kind {
                if context_generic_types.contains(type_name) {
                    if matches!(
                        self.parser.get_token_kind_n(n + 1),
                        Some(TokenKind::Bracket(BracketKind::Angle, BracketStatus::Open))
                    ) {
                        if let Some((t, n_i)) = self.try_parse(n + 2, context_generic_types, rec)? {
                            return Ok(Some((
                                ASTGenericType(
                                    self.parser.get_position(n_i - self.parser.get_i()),
                                    type_name.into(),
                                    vec![t],
                                ),
                                n_i + 1,
                            )));
                        }
                    }
                    Some((
                        ASTGenericType(self.parser.get_position(n), type_name.into(), Vec::new()),
                        next_i,
                    ))
                } else {
                    let (param_types, next_i) = if let Some((param_types, next_i)) =
                        self.try_parse_parameter_types(n + 1, context_generic_types, rec)?
                    {
                        (param_types, next_i)
                    } else {
                        (vec![], next_i)
                    };

                    Some((
                        ASTCustomType {
                            name: type_name.into(),
                            param_types,
                            position: self.parser.get_position(n),
                        },
                        next_i,
                    ))
                }
            } else if let TokenKind::KeyWord(KeywordKind::Fn) = kind {
                Some(self.parse_fn(n + 1, context_generic_types, rec)?)
            } else if matches!(kind, TokenKind::Bracket(Round, Open))
                && matches!(
                    self.parser.get_token_kind_n(n + 1),
                    Some(TokenKind::Bracket(Round, Close))
                )
            {
                Some((ASTType::ASTUnitType, next_i + 1))
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
                self.parser.get_position(n)
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
                        self.parser.get_position(inner_n)
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
                self.parser.get_position(n)
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
                        self.parser.get_position(n)
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
                    self.parser.get_position(n)
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
                ASTType::ASTUnitType
            } else if let Some((t, next_i)) =
                self.try_parse_ast_type_rec(n, context_param_types, rec + 1)?
            {
                n = next_i - self.parser.get_i();
                t
            } else {
                return Err(format!(
                    "Error parsing fn type parameter: {}",
                    self.parser.get_position(n)
                ));
            };

            Ok((
                ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                    return_type: Box::new(return_type),
                    parameters,
                }),
                self.parser.get_i() + n,
            ))
        } else {
            Err(format!(
                "Error parsing fn type parameter, expected '(': {}",
                self.parser.get_position(n)
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::tokens::Token;
    use crate::lexer::Lexer;
    use crate::parser::ast::{lambda_unit, ASTPosition};
    use crate::parser::test_utils::get_parser;

    use super::*;

    struct TypeParserHelper {
        type_tokens: Vec<Token>,
    }

    impl TypeParserHelper {
        fn new(type_str: &str) -> Self {
            let lexer = Lexer::new(type_str.into());
            // TODO errors
            let (tokens, _errors) = lexer.process();
            Self {
                type_tokens: tokens,
            }
        }
    }

    impl ParserTrait for TypeParserHelper {
        fn get_i(&self) -> usize {
            0
        }

        fn get_token_n(&self, n: usize) -> Option<&Token> {
            self.type_tokens.get(n)
        }
    }

    #[test]
    fn test_int() {
        let parse_result = try_parse("int");
        assert_eq!(
            Some((ASTBuiltinType(ASTBuiltinTypeKind::ASTIntegerType), 1)),
            parse_result
        );
    }

    #[test]
    fn test_str() {
        let parse_result = try_parse("str");
        assert_eq!(
            Some((ASTBuiltinType(ASTBuiltinTypeKind::ASTStringType), 1)),
            parse_result
        );
    }

    #[test]
    fn test_lambda() {
        let parse_result = try_parse("fn() -> ()");
        assert_eq!(Some((lambda_unit(), 6)), parse_result);
    }

    #[test]
    fn test_lambda1() {
        let parse_result = try_parse("fn(int,str) -> int");
        assert_eq!(format!("{:?}", parse_result), "Some((ASTBuiltinType(ASTLambdaType { parameters: [ASTBuiltinType(ASTIntegerType), ASTBuiltinType(ASTStringType)], return_type: ASTBuiltinType(ASTIntegerType) }), 8))");
    }

    #[test]
    fn test_lambda2() {
        let parse_result = try_parse("fn(fn() -> (),str) -> int");
        assert_eq!(format!("{:?}", parse_result), "Some((ASTBuiltinType(ASTLambdaType { parameters: [ASTBuiltinType(ASTLambdaType { parameters: [], return_type: ASTUnitType }), ASTBuiltinType(ASTStringType)], return_type: ASTBuiltinType(ASTIntegerType) }), 13))");
    }

    #[test]
    fn test_custom_type() {
        let parse_result = try_parse_with_context("Dummy<T,T1>", &["T".into(), "T1".into()]);
        assert_eq!(
            Some((
                ASTCustomType {
                    name: "Dummy".into(),
                    param_types: vec![
                        ASTGenericType(ASTPosition::new(1, 8), "T".into(), Vec::new()),
                        ASTGenericType(ASTPosition::new(1, 11), "T1".into(), Vec::new()),
                    ],
                    position: ASTPosition::new(1, 6)
                },
                6
            )),
            parse_result
        );
    }

    #[test]
    fn test_param_type() {
        let parse_result = try_parse_with_context("T", &["T".into()]);
        assert_eq!(
            Some((
                ASTGenericType(ASTPosition::new(1, 2), "T".into(), Vec::new()),
                1
            )),
            parse_result
        );
    }

    #[test]
    fn test_not_param_type() {
        let parse_result = try_parse_with_context("T", &["F".into()]);
        assert_eq!(
            Some((
                ASTCustomType {
                    name: "T".into(),
                    param_types: vec![],
                    position: ASTPosition::new(1, 2)
                },
                1
            )),
            parse_result
        );
    }

    #[test]
    fn test_complex_type() {
        let parse_result = try_parse_with_context("List<Option<T>>", &["T".into()]);
        let option_t = ASTCustomType {
            name: "Option".into(),
            param_types: vec![ASTGenericType(
                ASTPosition::new(1, 14),
                "T".into(),
                Vec::new(),
            )],
            position: ASTPosition::new(1, 12),
        };
        assert_eq!(
            Some((
                ASTCustomType {
                    name: "List".into(),
                    param_types: vec![option_t],
                    position: ASTPosition::new(1, 5)
                },
                7
            )),
            parse_result
        );
    }

    #[test]
    fn parse_list_str() {
        let parser = TypeParserHelper::new("List<str>");
        let type_parser = TypeParser::new(&parser);

        match type_parser.try_parse_ast_type(0, &[]).unwrap() {
            None => panic!("Unsupported type"),
            Some((ast_type, _)) => assert_eq!(
                ast_type,
                ASTType::ASTCustomType {
                    name: "List".into(),
                    param_types: vec![ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTStringType)],
                    position: ASTPosition::none()
                }
            ),
        }
    }

    #[test]
    fn test_type_classes() {
        let parse_result = try_parse_with_context("B<List<String>>", &["B".to_owned()]);
        let t = parse_result.unwrap().0;

        if matches!(t, ASTType::ASTGenericType(_, _, _)) {
            assert_eq!(format!("{t}"), "B<List<String>>");
        } else {
            panic!("{t:?}")
        }
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
