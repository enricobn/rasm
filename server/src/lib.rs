use rasm_core::codegen::enh_ast::EnhASTIndex;

pub mod ast_tree;
pub mod completion_service;
pub mod file_token;
pub mod ide_helper;
pub mod selectable_item;
pub mod server;
pub mod statement_finder;
pub mod text_lines;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RasmTextEdit {
    pub from: EnhASTIndex,
    pub len: usize,
    pub text: String,
}

impl RasmTextEdit {
    pub fn new(from: EnhASTIndex, len: usize, text: String) -> Self {
        Self { from, len, text }
    }
}

pub enum CompletionType {
    SelectableItem(EnhASTIndex, Option<String>),
    Identifier(String),
}
