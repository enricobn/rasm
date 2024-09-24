use std::{env, path::Path};

use iced::{
    widget::{
        button,
        pane_grid::{self, ResizeEvent},
        text, Button, Column,
    },
    Color, Element, Font, Task, Theme,
};

use rasm_core::{
    codegen::{
        compile_target::CompileTarget, enhanced_module::EnhancedASTModule, statics::Statics,
    },
    commandline::CommandLineOptions,
    parser::ast::ASTFunctionDef,
    project::RasmProject,
};

mod module_view;
mod project_tree;
mod ui_tree;

pub struct UI {
    project: RasmProject,
    current_module: Option<String>,
    modules: Vec<String>,
    target: CompileTarget,
    current_function: Option<ASTFunctionDef>,
    pane_state: pane_grid::State<UIPane>,
}

#[derive(Debug, Clone)]
pub enum Message {
    Module(String),
    Function(ASTFunctionDef),
    BackToModule,
    Home,
    ResizeSplit(ResizeEvent),
}

#[derive(Clone, Copy)]
enum UIPane {
    ProjectTree,
    ModuleCode,
    Info,
}

impl UI {
    pub fn show(project: RasmProject, target: CompileTarget) -> iced::Result {
        let mut statics = Statics::new();

        let (modules, _errors) = project.get_all_modules(
            &mut statics,
            false,
            &target,
            false,
            &env::temp_dir().join("tmp"),
            &CommandLineOptions::default(),
        );

        let main = if let Some(main) = &project.config.package.main {
            Some(
                project
                    .from_relative_to_main_src(Path::new(&main))
                    .canonicalize()
                    .unwrap()
                    .to_string_lossy()
                    .to_string(),
            )
        } else {
            None
        };

        let (mut pane_state, pane) = pane_grid::State::new(UIPane::ProjectTree);
        if let Some((module_pane, split)) =
            pane_state.split(pane_grid::Axis::Vertical, pane, UIPane::ModuleCode)
        {
            pane_state.resize(split, 0.2);
            if let Some((info_pane, split)) =
                pane_state.split(pane_grid::Axis::Horizontal, module_pane, UIPane::Info)
            {
                pane_state.resize(split, 0.8);
            }
        }

        iced::application("Rasm project UI", UI::update, UI::view)
            .theme(|_ui| Theme::Dark)
            .default_font(Font::MONOSPACE)
            //.window(Settings::default())
            //.window_size(Size)
            .centered()
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
                        current_module: main,
                        current_function: None,
                        pane_state,
                    },
                    Task::none(),
                )
            })
    }

    fn view<'a>(&'a self) -> Element<'a, Message> {
        iced::widget::pane_grid(&self.pane_state, |id, pane, maximized| match pane {
            UIPane::ProjectTree => pane_grid::Content::new(self.project_tree()),
            UIPane::ModuleCode => match &self.current_module {
                Some(s) => pane_grid::Content::new(self.show_module(s)),
                None => pane_grid::Content::new(Column::new()),
            },
            UIPane::Info => pane_grid::Content::new(Column::new()),
        })
        .spacing(10)
        .style(move |theme| {
            let mut pane_style = iced::widget::pane_grid::default(theme);
            pane_style.hovered_region.border.color = Color::from_rgb(0.0, 0.0, 1.0);
            pane_style
        })
        .on_resize(10, |event| Message::ResizeSplit(event))
        .into()
        /*
        match &self.current_function {
            Some(function) => self.show_function(function),
            None => match &self.current_module {
                Some(s) => self.show_module(s),
                None => self.home(),
            },
        }
        */

        /*
        let (enhanced_ast_module, _errors) =
            EnhancedASTModule::new(modules, &self.project, &mut statics, &target, false);
            */
    }

    fn update(&mut self, message: Message) {
        match message {
            Message::Home => {
                self.current_module = None;
                self.current_function = None
            }
            Message::Module(s) => self.current_module = Some(s),
            Message::Function(function) => self.current_function = Some(function),
            Message::BackToModule => self.current_function = None,
            Message::ResizeSplit(event) => {
                self.pane_state.resize(event.split, event.ratio);
            }
        }
    }

    fn text_button<'a>(t: impl text::IntoFragment<'a>) -> Button<'a, Message> {
        button(text(t))
            .style(|theme, status| iced::widget::button::text(theme, status))
            .into()
    }

    fn show_function<'a>(&'a self, function: &'a ASTFunctionDef) -> Element<Message> {
        let mut column = Column::new()
            .spacing(10)
            .push(button("Back").on_press(Message::BackToModule))
            .push(text(format!("{}", function)));

        column.into()
    }
}
