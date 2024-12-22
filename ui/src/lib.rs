use std::{collections::HashMap, path::Path, time::Instant};

use iced::{
    widget::{
        button::Style,
        pane_grid::{self, ResizeEvent},
        scrollable::{scroll_to, AbsoluteOffset, Id},
        text, Button, Column,
    },
    Color, Element, Font, Padding, Task, Theme,
};

use module_view::TEXT_SCROLLABLE_ID;
use rasm_core::{
    codegen::{compile_target::CompileTarget, statics::Statics, val_context::ValContext},
    commandline::CommandLineOptions,
    project::{RasmProject, RasmProjectRunType},
    type_check::{
        ast_modules_container::ASTModulesContainer,
        ast_type_checker::{ASTTypeChecker, ASTTypeCheckerResult},
    },
};
use rasm_parser::parser::ast::{ASTFunctionDef, ASTPosition};

mod module_view;
mod project_tree;
mod ui_tree;

pub struct UI {
    project: RasmProject,
    current_module: Option<SelectedModule>,
    target: CompileTarget,
    current_function: Option<ASTFunctionDef>,
    pane_state: pane_grid::State<UIPane>,
    modules_container: ASTModulesContainer,
    info: Option<String>,
    selected_token: Option<ASTPosition>,
    text_scroll_positions: HashMap<String, AbsoluteOffset>,
    static_val_context: ValContext,
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
    Info(Option<String>, ASTPosition),
    ScrollText(AbsoluteOffset),
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

        let (modules_container, catalog, errors) = project.container_and_catalog(
            &mut statics,
            &RasmProjectRunType::All,
            &target,
            false,
            &CommandLineOptions::default(),
        );

        let mut static_val_context = ValContext::new(None);

        let mut bodies = Vec::new();

        for (id, namespace, module) in modules_container.modules() {
            bodies.push((module.body.clone(), id.clone(), namespace.clone()));
        }

        let mut ast_type_checker = ASTTypeChecker::new();

        for (body, id, namespace) in bodies {
            let mut val_context = ValContext::new(None);
            ast_type_checker.add_body(
                &mut val_context,
                &mut static_val_context,
                &body,
                None,
                &namespace,
                &id,
                &modules_container,
            );
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
            if let Some((_info_pane, split)) =
                pane_state.split(pane_grid::Axis::Horizontal, module_pane, UIPane::Info)
            {
                pane_state.resize(split, 0.8);
            }
        }

        let current_module = main.map(|it| {
            Self::selected_module(
                &target,
                &project,
                &modules_container,
                &it,
                &static_val_context,
            )
        });

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
                        modules_container,
                        info: None,
                        selected_token: None,
                        text_scroll_positions: HashMap::new(),
                        static_val_context,
                    },
                    Task::none(),
                )
            })
    }

    fn view<'a>(&'a self) -> Element<'a, Message> {
        iced::widget::pane_grid(&self.pane_state, |_id, pane, _maximized| match pane {
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

    fn update(&mut self, message: Message) -> Task<Message> {
        match message {
            Message::Home => {
                self.current_module = None;
                self.current_function = None;
            }
            Message::Module(s) => {
                self.current_module = Some(Self::selected_module(
                    &self.target,
                    &self.project,
                    &self.modules_container,
                    &s,
                    &self.static_val_context,
                ));

                let scroll_position = if let Some(position) = self.text_scroll_positions.get(&s) {
                    position.clone()
                } else {
                    AbsoluteOffset { x: 0.0, y: 0.0 }
                };
                return scroll_to(Id::new(TEXT_SCROLLABLE_ID), scroll_position);
            }
            Message::Function(function) => self.current_function = Some(function),
            Message::BackToModule => self.current_function = None,
            Message::ResizeSplit(event) => {
                self.pane_state.resize(event.split, event.ratio);
            }
            Message::Info(info, position) => {
                self.info = info;
                self.selected_token = Some(position)
            }
            Message::ScrollText(absolute_offset) => {
                if let Some(module) = &self.current_module {
                    self.text_scroll_positions
                        .insert(module.path.clone(), absolute_offset);
                }
            }
        }
        Task::none()
    }

    fn selected_module(
        target: &CompileTarget,
        project: &RasmProject,
        modules_container: &ASTModulesContainer,
        path: &str,
        static_val_context: &ValContext,
    ) -> SelectedModule {
        let mut start = Instant::now();
        let type_checker_result =
            if let Some((module, errors, info)) = project.get_module(&Path::new(path), target) {
                let mut ast_type_checker = ASTTypeChecker::new();
                let mut val_context = ValContext::new(None);

                start = Instant::now();

                if !errors.is_empty() {
                    println!("compilation errors");
                    for error in errors {
                        println!("{error}");
                    }
                }

                let mut tmp_static_val_context = ValContext::new(None);

                ast_type_checker.add_body(
                    &mut val_context,
                    &mut tmp_static_val_context,
                    &module.body,
                    None,
                    &info.module_namespace(),
                    &info.module_id(),
                    modules_container,
                );

                for function in module.functions {
                    let start_function = Instant::now();
                    ast_type_checker.add_function(
                        &function, //.fix_namespaces(&em).fix_generics(),
                        static_val_context,
                        &info.module_namespace(),
                        &info.module_id(),
                        modules_container,
                    );
                    println!(
                        "function {} takes {:?}",
                        function.name,
                        start_function.elapsed()
                    );
                }

                // TODO errors

                if !ast_type_checker.errors.is_empty() {
                    println!("selected_module errors");
                    ast_type_checker
                        .errors
                        .iter()
                        .for_each(|it| println!("{it}"));
                }

                println!("selected_module takes {:?}", start.elapsed());

                ast_type_checker.result
            } else {
                ASTTypeCheckerResult::new()
            };

        // enhanced_ast_module.print();

        SelectedModule {
            path: path.to_string(),
            type_checker_result,
        }
    }

    fn text_button<'a>(t: impl text::IntoFragment<'a>) -> Button<'a, Message> {
        iced::widget::button(text(t))
            .style(|theme, status| iced::widget::button::text(theme, status))
            .into()
    }

    fn text_color_button<'a>(
        &self,
        t: impl text::IntoFragment<'a>,
        message: String,
        style: impl Fn(&Theme) -> Color + 'a,
        position: &ASTPosition,
    ) -> Button<'a, Message> {
        let selected = self
            .selected_token
            .as_ref()
            .filter(|it| it == &position)
            .is_some();
        Self::text_button(t)
            .style(move |theme, _status| {
                let color = style(theme);
                let mut style = Style::default();
                style.text_color = color;
                if selected {
                    style.border = style
                        .border
                        .color(Color::from_rgb(0.4, 0.5, 0.5))
                        .width(2.0);
                }
                style
            })
            .on_press(Message::Info(Some(message), position.clone()))
            .padding(Padding::ZERO)
    }

    fn show_function<'a>(&'a self, function: &'a ASTFunctionDef) -> Element<Message> {
        let column = Column::new()
            .spacing(10)
            .push(iced::widget::button("Back").on_press(Message::BackToModule))
            .push(text(format!("{}", function)));

        column.into()
    }
}
