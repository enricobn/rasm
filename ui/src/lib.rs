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

use std::{
    env, fs,
    path::{Path, PathBuf},
};

use iced::{
    widget::{button, text, Column, Row, Scrollable, Text},
    Color, Element, Font, Padding, Task, Theme,
};

use rasm_core::{
    codegen::{
        c::options::COptions, compile_target::CompileTarget, enhanced_module::EnhancedASTModule,
        statics::Statics,
    },
    commandline::CommandLineOptions,
    lexer::{tokens::TokenKind, Lexer},
    parser::ast::ASTFunctionDef,
    project::RasmProject,
};

const COMMENT_COLOR: Color = Color::from_rgb(0.2, 0.8, 0.2);
const NATIVE_COLOR: Color = Color::from_rgb(0.8, 0.4, 0.4);
const KEYWORD_COLOR: Color = Color::from_rgb(0.5, 0.5, 1.0);

impl UI {
    pub fn show(project: RasmProject) -> iced::Result {
        let mut statics = Statics::new();
        let target = CompileTarget::C(COptions::default());

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
            .run_with(|| {
                (
                    UI {
                        project,
                        modules: modules
                            .into_iter()
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

    fn home(&self) -> Element<Message> {
        let mut column = Column::new().push(text(format!(
            "Project {}",
            self.project.config.package.name
        )));
        for module in self.modules.iter() {
            let row = Row::new()
                .spacing(10)
                .push(button("Open").on_press(Message::Module(module.clone())))
                .push(text(module));
            column = column.push(row);
        }

        /*
        let (enhanced_ast_module, _errors) =
            EnhancedASTModule::new(modules, &self.project, &mut statics, &target, false);
            */

        Scrollable::new(column).into()
    }

    fn show_module<'a>(&'a self, module_path: &'a str) -> Element<Message> {
        let mut column = Column::new()
            .spacing(10)
            .push(button("Back").on_press(Message::Home))
            .push(text(module_path));
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
                            TokenKind::AlphaNumeric(s) => row = row.push(Self::monospace(s)),
                            TokenKind::NativeBlock(s) => {
                                row = row.push(Self::monospace(s).color(NATIVE_COLOR))
                            }
                            //TokenKind::Bracket(bracket_kind, bracket_status) => todo!(),
                            TokenKind::Comment(s) => {
                                row = row.push(Self::monospace(s).color(COMMENT_COLOR))
                            }
                            TokenKind::MultiLineComment(s) => {
                                row = row.push(Self::monospace(s).color(COMMENT_COLOR))
                            }
                            TokenKind::KeyWord(keyword_kind) => {
                                row = row
                                    .push(Self::monospace(keyword_kind.name()).color(KEYWORD_COLOR))
                            }
                            //TokenKind::Number(_) => todo!(),
                            //TokenKind::Punctuation(punctuation_kind) => todo!(),
                            //TokenKind::StringLiteral(_) => todo!(),
                            //TokenKind::CharLiteral(_) => todo!(),
                            TokenKind::WhiteSpaces(s) => {
                                row = row.push(Self::monospace(s));
                            }
                            _ => {
                                row = row.push(Self::monospace(format!("{}", token.kind)));
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
                column = column.push(Scrollable::new(code));
            }
        }

        column.into()
    }

    fn monospace(s: String) -> Text<'static> {
        text(s).font(Font::MONOSPACE)
    }

    fn show_function<'a>(&'a self, function: &'a ASTFunctionDef) -> Element<Message> {
        let mut column = Column::new()
            .spacing(10)
            .push(button("Back").on_press(Message::BackToModule))
            .push(text(format!("{}", function)));

        column.into()
    }
}
