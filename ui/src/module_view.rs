use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use iced::{
    widget::{
        button, container,
        scrollable::{self, Scrollbar},
        text, Column, Row, Scrollable,
    },
    Background, Color, Element, Length, Padding, Theme,
};
use rasm_core::{
    codegen::{enh_ast::EhModuleInfo, val_context::ASTIndex},
    lexer::{tokens::TokenKind, Lexer},
    parser::ast::{ASTModule, ASTPosition, ASTType, BuiltinTypeKind},
    type_check::ast_modules_container::ASTTypeFilter,
};

use crate::{Message, SelectedModule, UI};

const COMMENT_COLOR: Color = Color::from_rgb(0.2, 0.8, 0.2);
const NATIVE_COLOR: Color = Color::from_rgb(0.8, 0.4, 0.4);
const KEYWORD_COLOR: Color = Color::from_rgb(0.5, 0.5, 1.0);
const STRING_COLOR: Color = Color::from_rgb(0.5, 0.8, 0.8);
const NUMBER_COLOR: Color = Color::from_rgb(0.8, 0.5, 0.8);
const RESERVED_COLOR: Color = KEYWORD_COLOR;

#[derive(PartialEq)]
enum SyntaxKind {
    UnTyped,
    Typed,
}

impl UI {
    pub fn show_module<'a>(&'a self, selected_module: &'a SelectedModule) -> Element<Message> {
        let mut column: Column<'_, Message> = Column::new()
            .spacing(10)
            .push(button("Back").on_press(Message::Home))
            .push(text(
                self.project
                    .relative_to_source_folder(Path::new(&selected_module.path))
                    .unwrap()
                    .to_string_lossy()
                    .to_string(),
            ));
        if let Some((module, _errors, info)) = self
            .project
            .get_module(Path::new(&selected_module.path), &self.target)
        {
            let module_syntax = Self::module_syntax(&module, &info);
            let path = PathBuf::from(&selected_module.path);
            if let Ok(source) = fs::read_to_string(&path) {
                let mut code = Column::new().padding(Padding::new(5.0));

                let lexer = Lexer::new(source);

                let (tokens, _errors) = lexer.process();

                let mut row = Row::new();

                let mut just_added_new_line = false;

                for token in tokens {
                    if matches!(token.kind, TokenKind::EndOfLine) {
                        if just_added_new_line {
                            row = row.push(" ");
                        }
                        code = code.push(row);
                        row = Row::new();
                        just_added_new_line = true;
                    } else {
                        let token_position = ASTPosition::new(token.row, token.column);
                        let token_index = info.index(token_position);
                        match token.kind {
                            TokenKind::AlphaNumeric(s) => {
                                if module_syntax
                                    .get(&token_index)
                                    .unwrap_or(&SyntaxKind::Typed)
                                    == &SyntaxKind::UnTyped
                                {
                                    row = row.push(text(s));
                                    continue;
                                }
                                let content: Element<'a, Message> = if let Some(type_filter) =
                                    selected_module.type_checker_result.get(&token_index)
                                {
                                    let exact_and_not_generic = if let ASTTypeFilter::Exact(
                                        ast_type,
                                        _info,
                                    ) = type_filter
                                    {
                                        !ast_type.is_generic()
                                    } else {
                                        false
                                    };

                                    Self::text_color_button(
                                        s,
                                        format!("{token_index}\n{}", type_filter),
                                        move |theme| {
                                            if exact_and_not_generic {
                                                theme.palette().success
                                            } else {
                                                Color::from_rgb(1.0, 1.0, 0.0)
                                            }
                                        },
                                    )
                                    .into()
                                } else {
                                    Self::text_color_button(
                                        s,
                                        format!("{token_index}"),
                                        move |theme| theme.palette().danger,
                                    )
                                    .into()
                                };
                                row = row.push(content);
                            }
                            TokenKind::NativeBlock(s) => {
                                row = row.push("/{");
                                code = code.push(row);
                                code = code.push(text(s).color(NATIVE_COLOR));
                                code = code.push(text("}/"));
                                row = Row::new();
                            }
                            //TokenKind::Bracket(bracket_kind, bracket_status) => todo!(),
                            TokenKind::Comment(s) => {
                                row = row.push(text(s).color(COMMENT_COLOR));
                                code = code.push(row);
                                row = Row::new();
                            }
                            TokenKind::MultiLineComment(s) => {
                                row = row.push(text(s).color(COMMENT_COLOR))
                            }
                            TokenKind::KeyWord(keyword_kind) => {
                                row = row.push(text(keyword_kind.name()).color(KEYWORD_COLOR))
                            }
                            TokenKind::Number(n) => {
                                row = row.push(text(format!("{n}")).color(NUMBER_COLOR))
                            }
                            //TokenKind::Punctuation(punctuation_kind) => todo!(),
                            TokenKind::StringLiteral(s) => {
                                row = row.push(text(format!("\"{s}\"")).color(STRING_COLOR))
                            }
                            //TokenKind::CharLiteral(_) => todo!(),
                            TokenKind::WhiteSpaces(s) => {
                                row = row.push(text(s));
                            }
                            TokenKind::Reserved(k) => {
                                row = row.push(text(format!("{k}")).color(RESERVED_COLOR));
                            }
                            _ => {
                                row = row.push(text(format!("{}", token.kind)));
                            }
                        }
                        just_added_new_line = false;
                    }
                }

                if !just_added_new_line {
                    code = code.push(row);
                }

                /*
                for function in module.functions.iter() {
                    let row = Row::new()
                        .spacing(10)
                        .push(button("Show").on_press(Message::Function(function.clone())))
                        .push(text(format!("{}", function)));
                    column = column.push(row);
                }
                */
                column = column.push(
                    Scrollable::with_direction(
                        code,
                        scrollable::Direction::Both {
                            vertical: Scrollbar::default(),
                            horizontal: Scrollbar::default(),
                        },
                    )
                    .width(Length::Fill),
                );
            }
        }

        container(column.width(Length::Fill))
            .style(|theme| {
                let mut color = theme.extended_palette().background.base.color.clone();
                color.r = color.r * 0.9;
                color.g = color.g * 0.9;
                color.b = color.b * 0.9;
                container::Style::default().background(Background::Color(color))
            })
            .into()
    }

    fn module_syntax(module: &ASTModule, info: &EhModuleInfo) -> HashMap<ASTIndex, SyntaxKind> {
        let mut result = HashMap::new();

        for s in module.structs.iter() {
            result.insert(
                info.index(s.position.clone()).mv_right(s.name.len()),
                SyntaxKind::UnTyped,
            );

            for p in s.properties.iter() {
                result.insert(info.index(p.position.clone()), SyntaxKind::UnTyped);
                result.extend(Self::set_untyped(&p.ast_type, info));
            }
        }

        for e in module.enums.iter() {
            result.insert(
                info.index(e.position.clone()).mv_right(e.name.len()),
                SyntaxKind::UnTyped,
            );

            for v in e.variants.iter() {
                result.insert(info.index(v.position.clone()).clone(), SyntaxKind::UnTyped);

                for p in v.parameters.iter() {
                    result.insert(info.index(p.position.clone()).clone(), SyntaxKind::UnTyped);
                    result.extend(Self::set_untyped(&p.ast_type, info));
                }
            }
        }

        for f in module.functions.iter() {
            result.insert(
                info.index(f.position.clone()).mv_right(f.name.len()),
                SyntaxKind::UnTyped,
            );
            for p in f.parameters.iter() {
                result.insert(info.index(p.position.clone()), SyntaxKind::UnTyped);
                result.extend(Self::set_untyped(&p.ast_type, info));
            }
            result.extend(Self::set_untyped(&f.return_type, info));
        }

        result
    }

    fn set_untyped(ast_type: &ASTType, info: &EhModuleInfo) -> HashMap<ASTIndex, SyntaxKind> {
        let mut result = HashMap::new();

        match ast_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    for p in parameters.iter() {
                        result.extend(Self::set_untyped(p, info));
                    }
                    result.extend(Self::set_untyped(&return_type, info));
                }
                _ => {}
            },
            ASTType::Generic(index, _) => {
                result.insert(info.index(index.clone()), SyntaxKind::UnTyped);
            }
            ASTType::Custom {
                name,
                param_types,
                position: index,
            } => {
                result.insert(info.index(index.clone()), SyntaxKind::UnTyped);
                for p in param_types.iter() {
                    result.extend(Self::set_untyped(p, info));
                }
            }
            ASTType::Unit => {}
        }

        result
    }
}
