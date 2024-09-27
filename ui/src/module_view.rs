use std::{
    fs,
    path::{Path, PathBuf},
};

use iced::{
    widget::{
        button, container,
        scrollable::{self, Scrollbar},
        text, Column, Row, Scrollable,
    },
    Background, Color, Element, Length, Padding,
};
use rasm_core::{
    lexer::{tokens::TokenKind, Lexer},
    type_check::functions_container::TypeFilter,
};

use crate::{Message, SelectedModule, UI};

const COMMENT_COLOR: Color = Color::from_rgb(0.2, 0.8, 0.2);
const NATIVE_COLOR: Color = Color::from_rgb(0.8, 0.4, 0.4);
const KEYWORD_COLOR: Color = Color::from_rgb(0.5, 0.5, 1.0);
const STRING_COLOR: Color = Color::from_rgb(0.5, 0.8, 0.8);
const NUMBER_COLOR: Color = Color::from_rgb(0.8, 0.5, 0.8);

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
        if let Some((module, _errors)) = self
            .project
            .get_module(Path::new(&selected_module.path), &self.target)
        {
            let path = PathBuf::from(&selected_module.path);
            if let Ok(source) = fs::read_to_string(&path) {
                let mut code = Column::new().padding(Padding::new(5.0));

                let lexer = Lexer::new(source, Some(path));

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
                        let token_index = token.index();
                        match token.kind {
                            TokenKind::AlphaNumeric(s) => {
                                let content: Element<'a, Message> = if let Some(type_filter) =
                                    selected_module.type_map.get(&token_index)
                                {
                                    let exact_and_not_generic =
                                        if let TypeFilter::Exact(ast_type) = type_filter {
                                            !ast_type.is_generic()
                                        } else {
                                            false
                                        };

                                    Self::text_button(s)
                                        .style(move |theme, _status| {
                                            let color = if exact_and_not_generic {
                                                theme.palette().success
                                            } else {
                                                theme.palette().danger
                                            };

                                            let mut style = button::Style::default();
                                            style.text_color = color;
                                            style
                                        })
                                        .on_press(Message::Info(Some(format!("{}", type_filter))))
                                        .padding(Padding::ZERO)
                                        .into()
                                } else {
                                    text(s).into()
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
}
