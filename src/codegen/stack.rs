use std::cell::RefCell;

use log::debug;

#[derive(Debug, PartialEq)]
pub enum StackEntryType {
    LetVal,
    FunctionCallParameter,
    RefToDereference,
}

#[derive(Debug)]
pub struct StackEntry {
    entry_type: StackEntryType,
    desc: String,
    slots: usize,
}

#[derive(Debug)]
pub struct Stack {
    reserved_slots: RefCell<Vec<StackEntry>>,
}

impl Stack {
    pub fn new() -> Self {
        Self { reserved_slots: RefCell::new(Vec::new()) }
    }

    pub fn reserve(&self, entry_type: StackEntryType, desc: &str, slots: usize) -> usize {
        debug!("reserve {slots} slots in stack for {desc}");
        self.reserved_slots.borrow_mut().push(StackEntry { entry_type, desc: desc.to_string(), slots });
        debug!("stack {:?}", self);
        self.size()
    }

    pub fn size(&self) -> usize {
        self.reserved_slots.borrow().iter().map(|it| it.slots).sum()
    }

    pub fn remove(&self, expected_entry_type: StackEntryType, slots: usize) {
        debug!("removing {slots} slots from stack");
        let mut remaining_slots: i32 = slots as i32;

        while remaining_slots > 0 {
            if let Some(StackEntry { entry_type, desc, slots }) = self.reserved_slots.borrow_mut().pop() {
                if expected_entry_type != entry_type {
                    panic!()
                }
                remaining_slots -= slots as i32;
            } else {
                //break
                panic!();
            }
        }

        if remaining_slots < 0 {
            panic!();
        }
        debug!("stack {:?}", self);
    }

    pub fn remove_all(&self) {
        self.reserved_slots.borrow_mut().clear();
    }

    pub fn find_relative_to_bp(&self, entry_type: StackEntryType, desc: &str) -> usize {
        let mut result = 0;
        let mut found = false;
        for entry in self.reserved_slots.borrow().iter() {
            if !found {
                result += entry.slots;
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
            panic!();
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::stack::{Stack, StackEntryType};

    #[test]
    fn find_relative_to_bp() {
        let stack = Stack::new();
        assert_eq!(4, stack.reserve(StackEntryType::LetVal, "val1", 4));
        assert_eq!(8, stack.reserve(StackEntryType::LetVal, "val2", 4));
        assert_eq!(12, stack.reserve(StackEntryType::RefToDereference, "ref1", 4));
        assert_eq!(16, stack.reserve(StackEntryType::LetVal, "val3", 4));

        assert_eq!(4, stack.find_relative_to_bp(StackEntryType::LetVal, "val1"));
        assert_eq!(8, stack.find_relative_to_bp(StackEntryType::LetVal, "val2"));
        assert_eq!(12, stack.find_relative_to_bp(StackEntryType::RefToDereference, "ref1"));
        assert_eq!(16, stack.find_relative_to_bp(StackEntryType::LetVal, "val3"));
    }
}