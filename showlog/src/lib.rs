use std::{collections::HashSet, fs};

use iced::{
    Element, Length, Padding, Task,
    widget::{Column, Row, Scrollable, button, horizontal_space, row, scrollable, text},
};

pub fn show_log(file_path: &str) -> iced::Result {
    let content = fs::read_to_string(file_path).unwrap_or_else(|e| {
        eprintln!("Error reading {}: {}", file_path, e);
        std::process::exit(1);
    });
    let doc = roxmltree::Document::parse(&content).unwrap_or_else(|e| {
        eprintln!("Error parsing XML: {}", e);
        std::process::exit(1);
    });

    let root_node = build_tree(doc.root(), 0);
    let total_nodes = count_nodes(&root_node);

    iced::application("XML Debug Viewer", App::update, App::view)
        .theme(|_| iced::Theme::Dark)
        .default_font(iced::Font::MONOSPACE)
        .centered()
        .run_with(move || {
            let mut expanded = HashSet::new();
            expanded.insert(0);
            (
                App {
                    root: Some(root_node),
                    expanded,
                    node_count: total_nodes,
                },
                Task::none(),
            )
        })
}

#[derive(Clone)]
struct TreeNode {
    id: usize,
    label: String,
    children: Vec<TreeNode>,
}

fn count_nodes(node: &TreeNode) -> usize {
    1 + node.children.iter().map(count_nodes).sum::<usize>()
}

fn build_tree<'a>(node: roxmltree::Node<'a, 'a>, id_counter: usize) -> TreeNode {
    let tag = node.tag_name().name().to_string();
    let attrs: Vec<String> = node
        .attributes()
        .map(|a| format!(" {}=\"{}\"", a.name(), a.value()))
        .collect();
    let label = if attrs.is_empty() {
        tag.clone()
    } else {
        format!("{}{}", tag, attrs.join(""))
    };

    let child_elements: Vec<_> = node.children().filter(|c| c.is_element()).collect();

    let text_content: Option<String> = node
        .children()
        .filter(|c| c.is_text())
        .map(|c| c.text().unwrap_or("").trim())
        .filter(|t| !t.is_empty())
        .map(|t| t.to_string())
        .next();

    if child_elements.is_empty() {
        if let Some(txt) = text_content {
            return TreeNode {
                id: id_counter,
                label: format!("<{}> {} </{}>", tag, txt, tag),
                children: vec![],
            };
        }
    }

    let mut next_id = id_counter + 1;
    let children = child_elements
        .into_iter()
        .map(|c| {
            let child = build_tree(c, next_id);
            next_id = child.id + count_nodes(&child);
            child
        })
        .collect();

    TreeNode {
        id: id_counter,
        label,
        children,
    }
}

#[derive(Clone, Debug)]
enum Message {
    Toggle(usize),
}

struct App {
    root: Option<TreeNode>,
    expanded: HashSet<usize>,
    node_count: usize,
}

impl App {
    fn view(&self) -> Element<'_, Message> {
        let tree = match &self.root {
            Some(node) => render_tree(node, 0, &self.expanded),
            None => Column::new().push(text("No XML loaded")),
        };

        Scrollable::with_direction(
            Column::new()
                .push(text(format!("{} nodes", self.node_count)))
                .push(tree),
            scrollable::Direction::Both {
                vertical: scrollable::Scrollbar::default(),
                horizontal: scrollable::Scrollbar::default(),
            },
        )
        .width(Length::Fill)
        .height(Length::Fill)
        .into()
    }

    fn update(&mut self, message: Message) -> Task<Message> {
        match message {
            Message::Toggle(id) => {
                if self.expanded.contains(&id) {
                    self.expanded.remove(&id);
                } else {
                    self.expanded.insert(id);
                }
            }
        }
        Task::none()
    }
}

fn render_tree<'a>(
    node: &'a TreeNode,
    indent: usize,
    expanded: &HashSet<usize>,
) -> Column<'a, Message> {
    let mut col = Column::new();

    if node.children.is_empty() {
        col = col.push(indent_row(indent, text(&node.label).into()));
    } else {
        let is_expanded = expanded.contains(&node.id);
        let toggle_char = if is_expanded { '\u{25BC}' } else { '\u{25B6}' };

        col = col.push(indent_row(
            indent,
            row!(
                button(text(toggle_char).shaping(text::Shaping::Advanced))
                    .on_press(Message::Toggle(node.id))
                    .style(|theme, status| iced::widget::button::text(theme, status)),
                text(&node.label)
            )
            .into(),
        ));

        if is_expanded {
            for child in &node.children {
                col = col.push(render_tree(child, indent + 1, expanded));
            }
        }
    }

    col
}

fn indent_row<'a>(indent: usize, element: Element<'a, Message>) -> Element<'a, Message> {
    Row::new()
        .push(horizontal_space().width(24.0 * indent as f32))
        .push(element)
        .padding(Padding::ZERO)
        .into()
}
