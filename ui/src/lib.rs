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

use std::{env, path::Path};

use iced::{
    widget::{button, column, keyed::column, text, Column, Row, Scrollable},
    Element, Task,
};
use rasm_core::{
    codegen::{
        c::options::COptions, compile_target::CompileTarget, enhanced_module::EnhancedASTModule,
        statics::Statics,
    },
    commandline::CommandLineOptions,
    parser::ast::{ASTFunctionDef, ASTModule},
    project::RasmProject,
};

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

        iced::application("Rasm project UI", UI::update, UI::view).run_with(|| {
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
        let mut column = Column::new();
        for module in self.modules.iter() {
            let row = Row::new()
                .spacing(10)
                .push(button("open").on_press(Message::Module(module.clone())))
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
            for function in module.functions.iter() {
                let row = Row::new()
                    .spacing(10)
                    .push(button("show").on_press(Message::Function(function.clone())))
                    .push(text(format!("{}", function)));
                column = column.push(row);
            }
        }
        column.into()
    }

    fn show_function<'a>(&'a self, function: &'a ASTFunctionDef) -> Element<Message> {
        let column = Column::new()
            .spacing(10)
            .push(button("Back").on_press(Message::BackToModule))
            .push(text(format!("{}", function)));

        column.into()
    }
}
