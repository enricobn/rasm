use iced::{
    widget::{button, horizontal_space, row, text, Column, Row},
    Element,
};

pub struct UITree<'a, Message> {
    column: Column<'a, Message>,
}

pub enum UINode<'a, Message>
where
    Message: Clone,
{
    Node(Element<'a, Message>, Vec<UINode<'a, Message>>),
    Leaf(Element<'a, Message>),
}

impl<'a, Message> UINode<'a, Message>
where
    Message: Clone,
{
    pub fn new(root: UINode<'a, Message>) -> Self {
        root
    }
}

pub fn ui_tree<'a, Message>(root: UINode<'a, Message>) -> Column<'a, Message>
where
    Message: 'a + Clone,
{
    UITree::new(root)
}

impl<'a, Message> UITree<'a, Message>
where
    Message: 'a + Clone,
{
    pub fn new(root: UINode<'a, Message>) -> Column<Message> {
        let column = Column::new();
        Self::add_to_tree(column, 0, root)
    }

    fn add_to_tree(
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

    fn indent_row(indent: usize, element: Element<'a, Message>) -> Element<Message>
    where
        Message: 'a + Clone,
    {
        let mut row = Row::new();

        row = row.push(horizontal_space().width(40.0 * indent as f32));
        row = row.push(element);
        row.into()
    }
}

/*
impl<'a, Message, Theme, Renderer> Widget<Message, Theme, Renderer> for UITree<'a, Message>
where
    Renderer: renderer::Renderer,
{
    fn size(&self) -> iced::Size<iced::Length> {
        Size::new(iced::Length::Fill, iced::Length::Fill)
    }

    fn layout(
        &self,
        tree: &mut widget::Tree,
        renderer: &Renderer,
        limits: &iced::advanced::layout::Limits,
    ) -> iced::advanced::layout::Node {
        Node::new(Size::new(100.0, 100.0))
    }

    fn draw(
        &self,
        tree: &widget::Tree,
        renderer: &mut Renderer,
        theme: &Theme,
        style: &renderer::Style,
        layout: iced::advanced::Layout<'_>,
        cursor: iced::advanced::mouse::Cursor,
        viewport: &iced::Rectangle,
    ) {
        self.column
            .draw(tree, renderer, theme, style, layout, cursor, viewport);
    }
}
*/
