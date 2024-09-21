pub struct UI {
    project: RasmProject,
    current_module: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Message {
    Open(String),
    Home,
}

use std::env;

use iced::{
    widget::{button, column, text, Column, Row, Scrollable},
    Element, Task,
};
use rasm_core::{
    codegen::{
        c::options::COptions, compile_target::CompileTarget, enhanced_module::EnhancedASTModule,
        statics::Statics,
    },
    commandline::CommandLineOptions,
    parser::ast::ASTModule,
    project::RasmProject,
};

impl UI {
    pub fn show(project: RasmProject) -> iced::Result {
        iced::application("Rasm project UI", UI::update, UI::view).run_with(|| {
            (
                UI {
                    project,
                    current_module: None,
                },
                Task::none(),
            )
        })
    }

    pub fn view(&self) -> Element<Message> {
        let mut statics = Statics::new();
        let target = CompileTarget::C(COptions::default());

        let (modules, _errors) = self.project.get_all_modules(
            &mut statics,
            false,
            &target,
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        );

        match &self.current_module {
            Some(s) => self.module(s),
            None => self.home(modules),
        }

        /*
        let (enhanced_ast_module, _errors) =
            EnhancedASTModule::new(modules, &self.project, &mut statics, &target, false);
            */

        //Scrollable::new(column).into()
    }

    pub fn update(&mut self, message: Message) {
        match message {
            Message::Open(s) => self.current_module = Some(s),
            Message::Home => self.current_module = None,
        }
    }

    fn module<'a>(&'a self, module_path: &'a str) -> Element<Message> {
        column!(button("Back").on_press(Message::Home), text(module_path)).into()
    }

    fn home(&self, modules: Vec<ASTModule>) -> Element<Message> {
        let mut column = Column::new();
        for module in modules.iter() {
            let t = module.path.to_string_lossy().to_owned().to_string();
            let row = Row::new()
                .push(button("open").on_press(Message::Open(t.clone())))
                .push(text(t));
            column = column.push(row);
        }

        /*
        let (enhanced_ast_module, _errors) =
            EnhancedASTModule::new(modules, &self.project, &mut statics, &target, false);
            */

        Scrollable::new(column).into()
    }
}
