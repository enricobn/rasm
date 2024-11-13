use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::{self, Formatter};
use std::hash::Hash;
use std::slice::Iter;

use linked_hash_map::LinkedHashMap;

pub mod debug_indent;

// TODO is it useful?
pub trait MyToString {
    fn my_to_string(&self) -> String;
}

impl Display for dyn MyToString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.my_to_string())
    }
}

impl<KEY: Display + Hash + Eq, VALUE: Display> MyToString for HashMap<KEY, VALUE> {
    fn my_to_string(&self) -> String {
        let pars: Vec<String> = self
            .iter()
            .map(|(name, it)| format!("{name}={it}"))
            .collect();
        pars.join(",")
    }
}

impl<KEY: Display + Hash + Eq, VALUE: Display> MyToString for LinkedHashMap<KEY, VALUE> {
    fn my_to_string(&self) -> String {
        let pars: Vec<String> = self
            .iter()
            .map(|(name, it)| format!("{name}={it}"))
            .collect();
        pars.join(",")
    }
}

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

pub struct HashMapDisplay<'a, K: 'a, V: 'a>(pub &'a HashMap<K, V>);

impl<'a, K: fmt::Display + 'a, V: fmt::Display + 'a> fmt::Display for HashMapDisplay<'a, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for (k, v) in self.0 {
            if !first {
                write!(f, ", {k} -> {v}")?;
            } else {
                write!(f, "{k} -> {v}")?;
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
    use std::collections::HashMap;

    use crate::MyToString;

    #[test]
    pub fn test_hashmap_mytostring() {
        let mut map = HashMap::new();
        map.insert("key", 1);

        assert_eq!("key=1", format!("{}", map.my_to_string()));
    }
}
