use std::{collections::HashMap, env, path::Path, time::Instant};

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
    parser::ast::{ASTFunctionDef, ASTIndex},
    project::RasmProject,
    type_check::{function_type_checker::FunctionTypeChecker, functions_container::TypeFilter},
};

mod module_view;
mod project_tree;
mod ui_tree;

pub struct UI {
    project: RasmProject,
    current_module: Option<SelectedModule>,
    target: CompileTarget,
    current_function: Option<ASTFunctionDef>,
    pane_state: pane_grid::State<UIPane>,
    enhanced_ast_module: EnhancedASTModule,
    info: Option<String>,
}

pub struct SelectedModule {
    path: String,
    type_map: HashMap<ASTIndex, TypeFilter>,
}

#[derive(Debug, Clone)]
pub enum Message {
    Module(String),
    Function(ASTFunctionDef),
    BackToModule,
    Home,
    ResizeSplit(ResizeEvent),
    Info(Option<String>),
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

        let (enhanced_ast_module, _errors) =
            EnhancedASTModule::new(modules, &project, &mut statics, &target, false);

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

        let current_module =
            main.map(|it| Self::selected_module(&target, &project, &enhanced_ast_module, &it));

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
                        target,
                        current_module,
                        current_function: None,
                        pane_state,
                        enhanced_ast_module,
                        info: None,
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
            UIPane::Info => {
                let content: Element<'a, Message> = match &self.info {
                    Some(s) => text(s).into(),
                    None => Column::new().into(),
                };
                pane_grid::Content::new(content)
            }
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
    }

    fn update(&mut self, message: Message) {
        match message {
            Message::Home => {
                self.current_module = None;
                self.current_function = None
            }
            Message::Module(s) => {
                self.current_module = Some(Self::selected_module(
                    &self.target,
                    &self.project,
                    &self.enhanced_ast_module,
                    &s,
                ));
            }
            Message::Function(function) => self.current_function = Some(function),
            Message::BackToModule => self.current_function = None,
            Message::ResizeSplit(event) => {
                self.pane_state.resize(event.split, event.ratio);
            }
            Message::Info(info) => self.info = info,
        }
    }

    pub(crate) fn selected_module(
        target: &CompileTarget,
        project: &RasmProject,
        enhanced_ast_module: &EnhancedASTModule,
        path: &str,
    ) -> SelectedModule {
        let start = Instant::now();
        let type_map = if let Some((module, _errors)) = project.get_module(&Path::new(path), target)
        {
            let mut type_map = HashMap::new();
            let function_type_checker = FunctionTypeChecker::new(enhanced_ast_module);
            for function in module.functions {
                type_map.extend(function_type_checker.get_type_map(&function));
            }
            type_map
        } else {
            HashMap::new()
        };

        println!("selected_module takes {:?}", start.elapsed());
        SelectedModule {
            path: path.to_string(),
            type_map,
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
