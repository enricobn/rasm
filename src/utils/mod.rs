#![feature(local_key_cell_methods)]

use std::fmt::{Display, Formatter};

#[macro_use]
pub mod debug_indent;

pub fn format_option<T: Display>(o: &Option<T>) -> String {
    match o {
        None => "None".into(),
        Some(v) => format!("Some({v})"),
    }
}

pub fn format_option_option<T: Display>(o: &Option<Option<T>>) -> String {
    match o {
        None => "None".into(),
        Some(v) => format!("Some({})", format_option(v)),
    }
}
