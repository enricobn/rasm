use std::{
    env, fs,
    path::{Path, PathBuf},
};

use iced::{
    alignment::Vertical,
    widget::{
        button, horizontal_space, row,
        scrollable::{Direction, Scrollbar},
        text, Button, Column, Row, Scrollable,
    },
    Color, Element, Font, Length, Padding, Task, Theme,
};

use rasm_core::{
    codegen::{
        compile_target::CompileTarget, enhanced_module::EnhancedASTModule, statics::Statics,
    },
    commandline::CommandLineOptions,
    lexer::{tokens::TokenKind, Lexer},
    parser::ast::ASTFunctionDef,
    project::RasmProject,
};
use ui_tree::{ui_tree, UINode};

mod ui_tree;

pub struct UI {
    project: RasmProject,
    current_module: Option<String>,
    modules: Vec<String>,
    target: CompileTarget,
    current_function: Option<ASTFunctionDef>,
}

#[derive(Debug, Clone)]
pub enum Message {
    Module(String),
    Function(ASTFunctionDef),
    BackToModule,
    Home,
}

const COMMENT_COLOR: Color = Color::from_rgb(0.2, 0.8, 0.2);
const NATIVE_COLOR: Color = Color::from_rgb(0.8, 0.4, 0.4);
const KEYWORD_COLOR: Color = Color::from_rgb(0.5, 0.5, 1.0);

impl UI {
    pub fn show(project: RasmProject, target: CompileTarget) -> iced::Result {
        for p in project.get_all_dependencies().iter() {
            println!("project {}", p.config.package.name);
        }

        let mut statics = Statics::new();

        let (modules, _errors) = project.get_all_modules(
            &mut statics,
            false,
            &target,
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        );

        iced::application("Rasm project UI", UI::update, UI::view)
            .theme(|_ui| Theme::Dark)
            .default_font(Font::MONOSPACE)
            //.window(Settings::default())
            //.window_size(Size)
            .run_with(|| {
                (
                    UI {
                        project,
                        modules: modules
                            .into_iter()
                            .filter(|it| !it.namespace.is_core())
                            .map(|it| it.path.to_string_lossy().to_owned().to_string())
                            .collect(),
                        target,
                        current_module: None,
                        current_function: None,
                    },
                    Task::none(),
                )
            })
    }

    pub fn view(&self) -> Element<Message> {
        match &self.current_function {
            Some(function) => self.show_function(function),
            None => match &self.current_module {
                Some(s) => self.show_module(s),
                None => self.home(),
            },
        }

        /*
        let (enhanced_ast_module, _errors) =
            EnhancedASTModule::new(modules, &self.project, &mut statics, &target, false);
            */
    }

    pub fn update(&mut self, message: Message) {
        match message {
            Message::Home => {
                self.current_module = None;
                self.current_function = None
            }
            Message::Module(s) => self.current_module = Some(s),
            Message::Function(function) => self.current_function = Some(function),
            Message::BackToModule => self.current_function = None,
        }
    }

    fn text_button<'a>(t: impl text::IntoFragment<'a>) -> Button<'a, Message> {
        button(text(t))
            .style(|theme, status| iced::widget::button::text(theme, status))
            .into()
    }

    fn home(&self) -> Element<Message> {
        let root_o = Self::get_node(
            0,
            &self.project.config.package.name,
            &self.project.source_folder(),
        );
        let tree = if let Some(root) = root_o {
            ui_tree(root)
        } else {
            ui_tree(UINode::Node(
                text(&self.project.config.package.name).into(),
                Vec::new(),
            ))
        };

        Scrollable::with_direction(
            tree,
            Direction::Both {
                vertical: Scrollbar::default(),
                horizontal: Scrollbar::default(),
            },
        )
        .into()

        /*
        let tree = Column::new();
        Self::add_to_tree(
            tree,
            0,
            &self.project.config.package.name,
            &self.project.source_folder(),
        )
        .into()
        */
    }

    fn get_node<'a>(
        indent: usize,
        name: impl text::IntoFragment<'a>,
        path: &PathBuf,
    ) -> Option<UINode<'a, Message>> {
        let mut children = Vec::new();

        if let Ok(dir) = fs::read_dir(path) {
            for r_entry in dir {
                if let Ok(entry) = r_entry {
                    if let Ok(file_type) = entry.file_type() {
                        let entry_name = entry.file_name().to_string_lossy().to_string();
                        if file_type.is_dir() {
                            if let Some(child) = Self::get_node(
                                indent + 1,
                                entry_name.trim().to_string(),
                                &entry.path(),
                            ) {
                                children.push(child);
                            }
                        } else if file_type.is_file() {
                            if let Some(ext) = entry.path().extension() {
                                if ext == "rasm" {
                                    children.push(UINode::Leaf(
                                        Self::text_button(entry_name)
                                            .on_press(Message::Module(
                                                entry
                                                    .path()
                                                    .to_string_lossy()
                                                    .to_owned()
                                                    .to_string(),
                                            ))
                                            .into(),
                                    ));
                                }
                            }
                        }
                    }
                }
            }
        }

        if children.is_empty() {
            None
        } else {
            Some(UINode::Node(text(name).into(), children))
        }
    }

    fn show_module<'a>(&'a self, module_path: &'a str) -> Element<Message> {
        let mut column = Column::new()
            .spacing(10)
            .push(button("Back").on_press(Message::Home))
            .push(text(
                self.project
                    .relative_to_source_folder(Path::new(module_path))
                    .unwrap()
                    .to_string_lossy()
                    .to_string(),
            ));
        if let Some((module, _errors)) = self
            .project
            .get_module(Path::new(module_path), &self.target)
        {
            let path = PathBuf::from(module_path);
            if let Ok(source) = fs::read_to_string(&path) {
                let mut code = Column::new().padding(Padding::new(5.0));

                let lexer = Lexer::new(source, Some(path));

                let (tokens, _errors) = lexer.process();

                let mut row = Row::new();

                let mut just_added_new_line = false;

                for token in tokens {
                    if matches!(token.kind, TokenKind::EndOfLine) {
                        if just_added_new_line {
                            row = row.push(text(""));
                        }
                        code = code.push(row);
                        row = Row::new();
                        just_added_new_line = true;
                    } else {
                        match token.kind {
                            TokenKind::AlphaNumeric(s) => row = row.push(text(s)),
                            TokenKind::NativeBlock(s) => {
                                row = row.push(text(s).color(NATIVE_COLOR))
                            }
                            //TokenKind::Bracket(bracket_kind, bracket_status) => todo!(),
                            TokenKind::Comment(s) => row = row.push(text(s).color(COMMENT_COLOR)),
                            TokenKind::MultiLineComment(s) => {
                                row = row.push(text(s).color(COMMENT_COLOR))
                            }
                            TokenKind::KeyWord(keyword_kind) => {
                                row = row.push(text(keyword_kind.name()).color(KEYWORD_COLOR))
                            }
                            //TokenKind::Number(_) => todo!(),
                            //TokenKind::Punctuation(punctuation_kind) => todo!(),
                            //TokenKind::StringLiteral(_) => todo!(),
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
                        Direction::Both {
                            vertical: Scrollbar::default(),
                            horizontal: Scrollbar::default(),
                        },
                    )
                    .width(Length::Fill),
                );
            }
        }

        column.width(Length::Fill).into()
    }

    fn show_function<'a>(&'a self, function: &'a ASTFunctionDef) -> Element<Message> {
        let mut column = Column::new()
            .spacing(10)
            .push(button("Back").on_press(Message::BackToModule))
            .push(text(format!("{}", function)));

        column.into()
    }
}
