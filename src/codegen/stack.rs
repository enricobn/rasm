use std::cell::RefCell;

use log::debug;

#[derive(Debug, PartialEq, Eq)]
pub enum StackEntryType {
    LetVal,
    RefToDereference,
    Other,
}

#[derive(Debug)]
pub struct StackEntry {
    entry_type: StackEntryType,
    desc: String,
}

#[derive(Debug)]
pub struct StackVals {
    reserved_slots: RefCell<Vec<StackEntry>>,
}

impl StackVals {
    pub fn new() -> Self {
        Self {
            reserved_slots: RefCell::new(Vec::new()),
        }
    }

    pub fn reserve(&self, entry_type: StackEntryType, desc: &str) -> usize {
        self.reserved_slots.borrow_mut().push(StackEntry {
            entry_type,
            desc: desc.to_string(),
        });
        debug!("stack {:?}", self);
        self.len_of_all()
    }

    pub fn len_of_all(&self) -> usize {
        self.reserved_slots.borrow().len()
    }

    pub fn len_of_local_vals(&self) -> usize {
        self.reserved_slots
            .borrow()
            .iter()
            .filter(|it| !matches!(it.entry_type, StackEntryType::Other))
            .count()
    }

    pub fn remove_all(&self) {
        self.reserved_slots.borrow_mut().clear();
    }

    pub fn find_relative_to_bp(&self, entry_type: StackEntryType, desc: &str) -> Option<usize> {
        let mut result = 0;
        let mut found = false;
        for entry in self.reserved_slots.borrow().iter() {
            if !found {
                result += 1;
            }
            if entry.entry_type == entry_type && entry.desc == desc {
                if found {
                    panic!();
                } else {
                    found = true;
                }
            }
        }

        if !found {
            None
        } else {
            Some(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::stack::{StackEntryType, StackVals};

    #[test]
    fn find_relative_to_bp() {
        let stack = StackVals::new();
        assert_eq!(1, stack.reserve(StackEntryType::LetVal, "val1"));
        assert_eq!(2, stack.reserve(StackEntryType::LetVal, "val2"));
        assert_eq!(3, stack.reserve(StackEntryType::RefToDereference, "ref1"));
        assert_eq!(4, stack.reserve(StackEntryType::LetVal, "val3"));

        assert_eq!(
            Some(1),
            stack.find_relative_to_bp(StackEntryType::LetVal, "val1")
        );
        assert_eq!(
            Some(2),
            stack.find_relative_to_bp(StackEntryType::LetVal, "val2")
        );
        assert_eq!(
            Some(3),
            stack.find_relative_to_bp(StackEntryType::RefToDereference, "ref1")
        );
        assert_eq!(
            Some(4),
            stack.find_relative_to_bp(StackEntryType::LetVal, "val3")
        );
    }
}
