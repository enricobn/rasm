use std::fmt::format;
use crate::codegen::{CodeGen, MemoryUnit, MemoryValue};
use linked_hash_map::LinkedHashMap;
use crate::codegen::MemoryValue::Mem;
use pad::PadStr;
use crate::codegen::backend::Backend;
use crate::codegen::text_macro::TextMacroEvaluator;

#[derive(Clone)]
pub struct Statics {
    id: usize,
    statics: LinkedHashMap<String, MemoryValue>,
    // label, value_label
    strings_map: LinkedHashMap<String, String>,
}

impl Statics {
    pub fn new() -> Self {
        Self {
            id: 0,
            statics: LinkedHashMap::new(),
            strings_map: LinkedHashMap::new(),
        }
    }

    pub fn add_str(&mut self, s: &str) -> String {
        let value_label = self.insert_prefix("_sv".into(), MemoryValue::StringValue(s.into()));
        let label = self.insert_prefix("_s".into(), MemoryValue::Mem(1, MemoryUnit::Words));

        self.strings_map.insert(label.clone(), value_label);

        label
    }

    pub fn insert(&mut self, key: String, value: MemoryValue) {
        assert!(self.statics.insert(key, value).is_none())
    }

    pub fn insert_prefix(&mut self, prefix: String, value: MemoryValue) -> String {
        let label = format!("{prefix}_{}", self.id);

        self.id += 1;

        assert!(self.statics.insert(label.clone(), value).is_none());

        label
    }

    pub fn generate_code(&mut self, backend: &dyn Backend, debug: bool) -> (String, String, String) {
        let mut data = String::new();
        let mut bss = String::new();

        let mut code = if debug {
            self.print_res(backend)
        } else {
            String::new()
        };

        if !self.statics.is_empty() {
            let mut keys: Vec<&String> = self.statics.keys().collect();
            // sorted for test purposes
            keys.sort();

            for id in keys.iter() {
                let mut def = String::new();
                def.push_str(&id.pad_to_width(50));

                match self.statics.get(*id).unwrap() {
                    MemoryValue::StringValue(s) => {
                        def.push_str("db    ");
                        def.push_str(&format!("'{}', 0h", s));
                        CodeGen::add(&mut data, &def, None, true);
                    }
                    MemoryValue::I32Value(i) => {
                        def.push_str("dd    ");
                        def.push_str(&format!("{}", i));
                        CodeGen::add(&mut data, &def, None, true);
                    }
                    Mem(len, unit) => {
                        match unit {
                            MemoryUnit::Bytes => {
                                def.push_str("resb ")
                            }
                            MemoryUnit::Words => {
                                def.push_str("resd ")
                            }
                        }
                        def.push_str(&format!("{}", len));
                        CodeGen::add(&mut bss, &def, None, true);
                    }
                }
            }
        }

        for (key, value_key) in self.strings_map.iter() {
            CodeGen::add(&mut code, &format!("push dword {value_key}"), None, true);
            CodeGen::add(&mut code, "call addStaticStringToHeap", None, true);
            CodeGen::add(&mut code, &format!("add {},{}", backend.stack_pointer(), backend.word_len()), None, true);
            CodeGen::add(&mut code, &format!("mov dword [{key}], eax"), None, true);
        }
        (data, bss, code)
    }

    pub fn get(&self, key: &str) -> Option<&MemoryValue> {
        self.statics.get(key)
    }

    fn print_res(&mut self, backend: &dyn Backend) -> String {
        let mut asm = String::new();

        CodeGen::add(&mut asm, "%ifdef LOG_DEBUG", None, false);

        let mut keys: Vec<&String> = self.statics.keys().collect();
        // sorted for test purposes
        keys.sort();

        for id in keys.iter() {
            let mut def = String::new();
            def.push_str(&id.pad_to_width(50));

            match self.statics.get(*id).unwrap() {
                MemoryValue::StringValue(_) => {}
                MemoryValue::I32Value(_) => {}
                Mem(_, _) => {
                    Self::print_address(&mut asm, id);
                }
            }
        }
        CodeGen::add(&mut asm, "%endif", None, false);

        TextMacroEvaluator::new().translate(backend, self, &asm)
    }

    fn print_address(asm: &mut String, address: &str) {
        let address_name = address.pad_to_width(50);
        CodeGen::add(asm, &format!("$call(sprint, \"{address_name} \")"), None, false);
        CodeGen::add(asm, &format!("$call(nprintln, {address})"), None, false);
    }
}
