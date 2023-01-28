use std::fmt::{Display, Formatter};

use log::debug;

use crate::debug_i;
use crate::parser::ast::ASTFunctionCall;

#[derive(Debug, Clone)]
pub struct CallStack {
    calls: Vec<ASTFunctionCall>,
}

impl Display for CallStack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            &self
                .calls
                .iter()
                .map(|it| format!("{it}"))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    }
}

impl CallStack {
    pub fn new() -> Self {
        Self { calls: vec![] }
    }

    pub fn add(&self, call: ASTFunctionCall) -> Self {
        if self.exists(&call) {
            debug_i!("error adding {call} stack\n{self}");
        }
        let mut new_calls = self.calls.clone();
        new_calls.push(call);
        Self { calls: new_calls }
    }

    pub fn exists(&self, call: &ASTFunctionCall) -> bool {
        self.calls
            .iter()
            .any(|it| it.function_name == call.function_name)
    }
}
