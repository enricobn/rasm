use iced::{
    widget::{button, horizontal_space, row, text, Column, Row},
    Element,
};

pub struct UITree {}

pub enum UINode<'a, Message>
where
    Message: Clone,
{
    Node(Element<'a, Message>, Vec<UINode<'a, Message>>),
    Leaf(Element<'a, Message>),
}

pub fn ui_node<'a, Message>(
    element: impl Into<Element<'a, Message>>,
    children: Vec<UINode<'a, Message>>,
) -> UINode<'a, Message>
where
    Message: Clone,
{
    UINode::Node(element.into(), children)
}

pub fn ui_leaf<'a, Message>(element: impl Into<Element<'a, Message>>) -> UINode<'a, Message>
where
    Message: Clone,
{
    UINode::Leaf(element.into())
}

pub fn ui_tree<'a, Message>(root: UINode<'a, Message>) -> Column<'a, Message>
where
    Message: 'a + Clone,
{
    UITree::new(root)
}

impl UITree {
    pub fn new<'a, Message>(root: UINode<'a, Message>) -> Column<Message>
    where
        Message: 'a + Clone,
    {
        let column = Column::new();
        Self::add_to_tree(column, 0, root)
    }

    fn add_to_tree<'a, Message>(
        tree: Column<'a, Message>,
        indent: usize,
        node: UINode<'a, Message>,
    ) -> Column<'a, Message>
    where
        Message: 'a + Clone,
    {
        let mut new_tree;

        match node {
            UINode::Node(element, vec) => {
                new_tree = tree.push(Self::indent_row(
                    indent,
                    row!(
                        // ▶
                        button(text('▼'.to_string()).shaping(text::Shaping::Advanced))
                            .style(|theme, status| iced::widget::button::text(theme, status)),
                        element
                    )
                    .into(),
                ));
                for child in vec {
                    new_tree = Self::add_to_tree(new_tree, indent + 1, child);
                }
            }
            UINode::Leaf(element) => {
                new_tree = tree.push(Self::indent_row(indent, element));
            }
        }
        new_tree
    }

    fn indent_row<'a, Message>(indent: usize, element: Element<'a, Message>) -> Element<Message>
    where
        Message: 'a + Clone,
    {
        let mut row = Row::new();

        row = row.push(horizontal_space().width(40.0 * indent as f32));
        row = row.push(element);
        row.into()
    }
}
