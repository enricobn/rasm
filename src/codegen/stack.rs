use std::cell::RefCell;

use log::debug;

#[derive(Debug)]
pub struct Stack {
    reserved_slots: RefCell<Vec<(String, usize)>>,
}

impl Stack {
    pub fn new() -> Self {
        Self { reserved_slots: RefCell::new(Vec::new()) }
    }

    pub fn reserve(&self, desc: &str, slots: usize) {
        debug!("reserve {slots} slots in stack for {desc}");
        self.reserved_slots.borrow_mut().push((desc.into(), slots));
        debug!("stack {:?}", self);
    }

    pub fn size(&self) -> usize {
        self.reserved_slots.borrow().iter().map(|it| it.1).sum()
    }

    pub fn remove(&self, slots: usize) {
        debug!("removing {slots} slots from stack");
        let mut remaining_slots: i32 = slots as i32;

        while remaining_slots > 0 {
            if let Some((_descr, s)) = self.reserved_slots.borrow_mut().pop() {
                remaining_slots -= s as i32;
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
}