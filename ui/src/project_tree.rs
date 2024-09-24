use std::{fs, path::PathBuf};

use iced::{
    widget::{
        button,
        scrollable::{Direction, Scrollbar},
        text, Scrollable,
    },
    Background, Element, Length,
};

use crate::{
    ui_tree::{ui_leaf, ui_node, ui_tree, UINode},
    Message, UI,
};

impl UI {
    pub(crate) fn project_tree(&self) -> Element<Message> {
        let root_o = self.get_node(
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
        .width(Length::Fill)
        .into()
    }

    fn get_node<'a>(
        &self,
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
                            if let Some(child) = self.get_node(
                                indent + 1,
                                entry_name.trim().to_string(),
                                &entry.path(),
                            ) {
                                children.push(child);
                            }
                        } else if file_type.is_file() {
                            if let Some(ext) = entry.path().extension() {
                                if ext == "rasm" {
                                    let module = entry
                                        .path()
                                        .canonicalize()
                                        .unwrap()
                                        .to_string_lossy()
                                        .to_string();
                                    let button = if self
                                        .current_module
                                        .as_ref()
                                        .filter(|it| it == &&module)
                                        .is_some()
                                    {
                                        Self::text_button(entry_name).style(|theme, _status| {
                                            button::Style::default().with_background(
                                                Background::from(theme.palette().primary),
                                            )
                                        })
                                    } else {
                                        Self::text_button(entry_name)
                                    };
                                    children
                                        .push(ui_leaf(button.on_press(Message::Module(module))));
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
            Some(ui_node(text(name), children))
        }
    }
}
