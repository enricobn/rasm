use std::fmt;
use std::fmt::Display;
use std::slice::Iter;

#[macro_use]
pub mod debug_indent;

pub fn format_option<T: Display>(o: &Option<T>) -> String {
    match o {
        None => "None".into(),
        Some(v) => format!("Some({v})"),
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

pub struct OptionOptionDisplay<'a, T: 'a>(pub &'a Option<Option<T>>);

impl<'a, T: fmt::Display + 'a> fmt::Display for OptionOptionDisplay<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Some(v) => {
                let inner = OptionDisplay(v);
                write!(f, "Some({inner})")
            }
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

pub fn find_one<T, P>(iter: Iter<T>, predicate: P) -> Option<&T>
where
    P: FnMut(&&T) -> bool,
{
    let filter = iter.filter(predicate).collect::<Vec<_>>();
    let len = filter.len();
    if len == 0 || len > 1 {
        return None;
    }
    filter.get(0).cloned()
}

#[cfg(test)]
pub mod tests {
    use crate::parser::ast::ASTNameSpace;

    pub fn test_namespace() -> ASTNameSpace {
        ASTNameSpace::new("test".to_string(), "test".to_string())
    }
}
