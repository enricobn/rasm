use std::{env, path::Path, time::Instant};

use iced::{
    widget::{
        button,
        pane_grid::{self, ResizeEvent},
        text, Button, Column,
    },
    Element, Font, Task, Theme,
};

use rasm_core::{
    codegen::{compile_target::CompileTarget, statics::Statics, val_context::ValContext},
    commandline::CommandLineOptions,
    parser::ast::ASTFunctionDef,
    project::RasmProject,
    type_check::{
        ast_modules_container::ASTModulesContainer,
        ast_type_checker::{ASTTypeChecker, ASTTypeCheckerResult},
    },
    utils::SliceDisplay,
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
    enhanced_ast_module: ASTModulesContainer,
    info: Option<String>,
}

pub struct SelectedModule {
    path: String,
    type_checker_result: ASTTypeCheckerResult,
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

        let mut modules_container = ASTModulesContainer::new();

        for (module, info) in modules {
            modules_container.add(module, info.module_id(), info.module_source(), false);
        }

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
            main.map(|it| Self::selected_module(&target, &project, &modules_container, &it));

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
                        enhanced_ast_module: modules_container,
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
        modules_container: &ASTModulesContainer,
        path: &str,
    ) -> SelectedModule {
        let start = Instant::now();
        let type_map =
            if let Some((module, _errors, info)) = project.get_module(&Path::new(path), target) {
                let function_type_checker = ASTTypeChecker::new(&modules_container);
                let mut val_context = ValContext::new(None);
                let mut statics = ValContext::new(None);

                // TODO errors

                let (mut type_map, _, mut errors) = function_type_checker.get_body_type_map(
                    &mut val_context,
                    &mut statics,
                    &module.body,
                    None,
                    &info.module_id(),
                    &info.module_source(),
                );

                for function in module.functions {
                    let (f_result, f_errors) = function_type_checker.get_type_map(
                        &function, //.fix_namespaces(&em).fix_generics(),
                        &mut statics,
                        &info.module_id(),
                        &info.module_source(),
                    );
                    type_map.extend(f_result);
                    errors.extend(f_errors);
                }

                // TODO errors
                if !errors.is_empty() {
                    println!("selected_module errors");
                    errors.iter().for_each(|it| println!("{it}"));
                }

                type_map
            } else {
                ASTTypeCheckerResult::new()
            };

        println!("selected_module takes {:?}", start.elapsed());

        // enhanced_ast_module.print();

        SelectedModule {
            path: path.to_string(),
            type_checker_result: type_map,
        }
    }

    fn text_button<'a>(t: impl text::IntoFragment<'a>) -> Button<'a, Message> {
        button(text(t))
            .style(|theme, status| iced::widget::button::text(theme, status))
            .into()
    }

    fn show_function<'a>(&'a self, function: &'a ASTFunctionDef) -> Element<Message> {
        let column = Column::new()
            .spacing(10)
            .push(button("Back").on_press(Message::BackToModule))
            .push(text(format!("{}", function)));

        column.into()
    }
}
