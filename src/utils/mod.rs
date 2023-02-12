use std::fmt;
use std::fmt::Display;

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

pub struct OptionDisplay<'a, T: 'a>(pub &'a Option<T>);

impl<'a, T: fmt::Display + 'a> fmt::Display for OptionDisplay<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Some(v) => write!(f, "Some({v})"),
            None => write!(f, "None"),
        }
    }
}

// from https://stackoverflow.com/questions/33759072/why-doesnt-vect-implement-the-display-trait
pub struct SliceDisplay<'a, T: 'a>(pub &'a [T]);

impl<'a, T: fmt::Display + 'a> fmt::Display for SliceDisplay<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for item in self.0 {
            if !first {
                write!(f, ", {}", item)?;
            } else {
                write!(f, "{}", item)?;
            }
            first = false;
        }
        Ok(())
    }
}
