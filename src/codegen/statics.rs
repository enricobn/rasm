use crate::codegen::{CodeGen, MemoryUnit, MemoryValue};
use linked_hash_map::LinkedHashMap;
use crate::codegen::MemoryValue::Mem;

#[derive(Clone)]
pub struct Statics {
    id: usize,
    statics: LinkedHashMap<String, MemoryValue>,
}

impl Statics {
    pub fn new() -> Self {
        Statics {
            id: 0,
            statics: LinkedHashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: MemoryValue) {
        assert!(self.statics.insert(key, value).is_none())
    }

    pub fn insert_prefix(&mut self, prefix: String, value: MemoryValue) -> String {
        let key = format!("{prefix}_{}", self.id);

        self.id += 1;

        assert!(self.statics.insert(key.clone(), value).is_none());

        key
    }

    pub fn to_code(&self) -> (String, String) {
        let mut data = String::new();
        let mut bss = String::new();

        if !self.statics.is_empty() {
            let mut keys: Vec<&String> = self.statics.keys().collect();
            // sorted for test purposes
            keys.sort();

            for id in keys.iter() {
                let mut def = String::new();
                def.push_str(id);

                match self.statics.get(*id).unwrap() {
                    MemoryValue::StringValue(s) => {
                        def.push_str("\tdb    ");
                        def.push_str(&format!("'{}', 0h", s));
                        CodeGen::add(&mut data, &def, None, true);
                    }
                    MemoryValue::I32Value(i) => {
                        def.push_str("\tdd    ");
                        def.push_str(&format!("{}", i));
                        CodeGen::add(&mut data, &def, None, true);
                    }
                    Mem(len, unit) => {
                        match unit {
                            MemoryUnit::Bytes => {
                                def.push_str("\tresb ")
                            }
                            MemoryUnit::Words => {
                                def.push_str("\tresw ")
                            }
                        }
                        def.push_str(&format!("{}", len));
                        CodeGen::add(&mut bss, &def, None, true);
                    }
                }
            }
        }
        (data, bss)
    }
}
