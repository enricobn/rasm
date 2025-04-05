use std::cell::RefCell;
use std::rc::Rc;

use linked_hash_set::LinkedHashSet;

use super::asm::code_gen_asm::CodeGenAsm;
use super::CodeGen;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StackEntryType {
    LocalVal,
    TmpRegister(String),
    ReturnRegister,
    LocalFakeAllocation(usize),
}

#[derive(Debug, Clone)]
pub struct StackEntry {
    pub entry_type: StackEntryType,
    pub desc: String,
}

#[derive(Debug, Clone)]
pub struct StackVals {
    reserved_slots: Rc<RefCell<Vec<StackEntry>>>,
}

impl StackVals {
    pub fn new() -> Self {
        Self {
            reserved_slots: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn reserve_local_val(&self, desc: &str) -> usize {
        self.check_desc(desc);

        self.reserved_slots.borrow_mut().push(StackEntry {
            entry_type: StackEntryType::LocalVal,
            desc: desc.to_owned(),
        });
        // debug!("stack {:?}", self);
        self.len_of_local_vals()
    }

    pub fn reserve_local_space(&self, desc: &str, words: usize) -> usize {
        self.check_desc(desc);

        self.reserved_slots.borrow_mut().push(StackEntry {
            entry_type: StackEntryType::LocalFakeAllocation(words),
            desc: desc.to_owned(),
        });
        // debug!("stack {:?}", self);
        self.len_of_local_vals()
    }

    pub fn reserve_return_register(&self, code_gen: &CodeGenAsm, out: &mut String) {
        self.check_desc("return_register");

        code_gen.add(out, "push eax", Some("return register"), true);

        self.reserved_slots.borrow_mut().push(StackEntry {
            entry_type: StackEntryType::ReturnRegister,
            desc: "return_register".to_owned(),
        });
    }

    pub fn reserve_tmp_register(
        &self,
        out: &mut String,
        desc: &str,
        code_gen: &CodeGenAsm,
    ) -> String {
        self.check_desc(desc);

        let self_tmp_registers = self.tmp_registers();

        let available_tmp_registers = code_gen
            .tmp_registers()
            .iter()
            .cloned()
            .filter(|it| !self_tmp_registers.contains(it))
            .collect::<Vec<_>>();

        if let Some(register) = available_tmp_registers.first() {
            code_gen.add(out, &format!("push {}", register), Some(desc), true);

            self.reserved_slots.borrow_mut().push(StackEntry {
                entry_type: StackEntryType::TmpRegister(register.clone()),
                desc: desc.to_owned(),
            });
            register.clone()
        } else {
            panic!("No more registers available");
        }
    }

    fn check_desc(&self, desc: &str) {
        if self
            .reserved_slots
            .borrow()
            .iter()
            .any(|it| it.desc == desc)
        {
            panic!("Duplicated desc: {desc}");
        }
    }

    pub fn release_tmp_register(&self, code_gen: &CodeGenAsm, out: &mut String, desc: &str) {
        let mut register_o = None;
        for entry in self.reserved_slots.borrow().iter().rev() {
            if let StackEntryType::TmpRegister(ref r) = entry.entry_type {
                if entry.desc == desc {
                    register_o = Some(r.to_owned());
                    break;
                } else {
                    panic!("Expected {desc} but got {}", entry.desc);
                }
            }
        }

        if let Some(register) = register_o {
            code_gen.add(out, &format!("pop {}", register), Some(desc), true);
            self.reserved_slots
                .borrow_mut()
                .retain(|it| it.desc != desc);
        } else {
            panic!("Cannot find temp register {desc}");
        }
    }

    fn tmp_registers(&self) -> LinkedHashSet<String> {
        self.reserved_slots
            .borrow()
            .iter()
            .cloned()
            .filter(|it| matches!(it.entry_type, StackEntryType::TmpRegister(_)))
            .map(|it| {
                if let StackEntryType::TmpRegister(register) = it.entry_type {
                    register
                } else {
                    panic!();
                }
            })
            .collect()
    }

    pub fn len_of_all(&self) -> usize {
        self.reserved_slots
            .borrow()
            .iter()
            .map(|it| match it.entry_type {
                StackEntryType::TmpRegister(_) => 1,
                StackEntryType::LocalFakeAllocation(size) => size,
                StackEntryType::LocalVal => 1,
                StackEntryType::ReturnRegister => 1,
            })
            .sum()
    }

    pub fn len_of_local_vals(&self) -> usize {
        self.reserved_slots
            .borrow()
            .iter()
            .map(|it| match it.entry_type {
                StackEntryType::TmpRegister(_) => 0,
                StackEntryType::LocalFakeAllocation(size) => size,
                StackEntryType::LocalVal => 1,
                StackEntryType::ReturnRegister => 0,
            })
            .sum()
    }

    pub fn remove_all(&self) {
        self.reserved_slots.borrow_mut().clear();
    }

    pub fn find_local_val_relative_to_bp(&self, desc: &str) -> Option<usize> {
        let mut result = 0;
        let mut found = false;
        for entry in self.reserved_slots.borrow().iter() {
            match entry.entry_type {
                StackEntryType::LocalVal => {
                    if !found {
                        result += 1;
                    }
                    if entry.desc == desc {
                        if found {
                            panic!("{desc}");
                        } else {
                            found = true;
                        }
                    }
                }
                StackEntryType::TmpRegister(_) => {}
                StackEntryType::ReturnRegister => {}
                StackEntryType::LocalFakeAllocation(size) => {
                    if !found {
                        result += size;
                    }
                }
            }
        }

        if !found {
            None
        } else {
            Some(result)
        }
    }

    pub fn find_tmp_register(&self, desc: &str) -> Option<String> {
        let mut result = None;
        for entry in self.reserved_slots.borrow().iter() {
            if let StackEntryType::TmpRegister(ref register) = entry.entry_type {
                if entry.desc == desc {
                    if result.is_some() {
                        panic!("{desc}");
                    } else {
                        result = Some(register.clone())
                    }
                }
            }
        }
        result
    }

    pub fn reserved_slots(&self) -> &RefCell<Vec<StackEntry>> {
        &self.reserved_slots
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::asm::code_gen_asm::CodeGenAsm;
    use crate::codegen::stack::StackVals;
    use crate::codegen::AsmOptions;

    #[test]
    fn find_relative_to_bp() {
        let mut out = String::new();

        let stack = StackVals::new();
        let code_gen = CodeGenAsm::new(AsmOptions::default(), false);
        assert_eq!(1, stack.reserve_local_val("val1"));
        stack.reserve_return_register(&code_gen, &mut out);
        assert_eq!(2, stack.reserve_local_val("val2"));
        stack.reserve_tmp_register(&mut out, "a_tmp_register", &code_gen);
        assert_eq!(3, stack.reserve_local_val("ref1"));
        assert_eq!(6, stack.reserve_local_space("spc", 3));
        assert_eq!(7, stack.reserve_local_val("val3"));

        assert_eq!(Some(1), stack.find_local_val_relative_to_bp("val1"));
        assert_eq!(Some(2), stack.find_local_val_relative_to_bp("val2"));
        assert_eq!(Some(3), stack.find_local_val_relative_to_bp("ref1"));
        assert_eq!(Some(7), stack.find_local_val_relative_to_bp("val3"));
    }
}
