use linked_hash_map::LinkedHashMap;

use crate::codegen::statics::MemoryValue::Mem;
use crate::parser::ast::ASTType;
use crate::type_check::typed_ast::ASTTypedType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemoryValue {
    StringValue(String),
    I32Value(i32),
    Mem(usize, MemoryUnit),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemoryUnit {
    Bytes,
    Words,
}

#[derive(Clone, Debug)]
pub struct ConstEntry {
    pub key: String,
    pub ast_type: ASTType,
}

#[derive(Clone, Debug)]
pub struct ConstTypedEntry {
    pub key: String,
    pub ast_typed_type: ASTTypedType,
}

#[derive(Debug)]
pub struct Statics {
    id: usize,
    statics: LinkedHashMap<String, MemoryValue>,
    // string -> (label, value_label)
    strings_map: LinkedHashMap<String, (String, String)>,
    const_map: LinkedHashMap<String, ConstEntry>,
    const_typed_map: LinkedHashMap<String, ConstTypedEntry>,
    static_allocation: Vec<(String, String)>,
    heap: LinkedHashMap<String, (String, i32)>,
}

impl Statics {
    pub fn new() -> Self {
        Self {
            id: 0,
            statics: LinkedHashMap::new(),
            strings_map: LinkedHashMap::new(),
            const_map: LinkedHashMap::new(),
            const_typed_map: LinkedHashMap::new(),
            static_allocation: Vec::new(),
            heap: LinkedHashMap::new(),
        }
    }

    pub fn add_str(&mut self, s: &str) -> String {
        if let Some((label, _)) = self.strings_map.get(s) {
            label.clone()
        } else {
            let value_label = self.insert_prefix("_sv", MemoryValue::StringValue(s.into()));
            let label = self.insert_prefix("_s", Mem(1, MemoryUnit::Words));

            self.strings_map
                .insert(s.into(), (label.clone(), value_label));

            label
        }
    }

    pub fn add_const(&mut self, name: String, ast_type: ASTType) {
        // TODO it's a trick: during type check passes consts are added again
        if !self.const_map.contains_key(&name) {
            let key = self.insert_prefix("const", MemoryValue::Mem(1, MemoryUnit::Words));
            self.const_map.insert(name, ConstEntry { key, ast_type });
        }
    }

    pub fn add_typed_const(&mut self, name: String, ast_typed_type: ASTTypedType) -> String {
        if let Some(entry) = self.get_const(&name) {
            let key = entry.key.clone();
            self.const_typed_map.insert(
                name,
                ConstTypedEntry {
                    key: key.clone(),
                    ast_typed_type,
                },
            );
            key
        } else {
            panic!("cannot find const {name}");
        }
    }

    pub fn get_const(&self, name: &str) -> Option<&ConstEntry> {
        self.const_map.get(name)
    }

    pub fn get_typed_const(&self, name: &str) -> Option<&ConstTypedEntry> {
        self.const_typed_map.get(name)
    }

    pub fn insert(&mut self, key: String, value: MemoryValue) {
        if let Some(oldValue) = self.statics.insert(key.clone(), value) {
            panic!("already added value for {key}");
        }

        //assert!(self.statics.insert(key.clone(), value.clone()).is_none(), "{key} {:?}", value)
    }

    pub fn insert_prefix(&mut self, prefix: &str, value: MemoryValue) -> String {
        let label = format!("{prefix}_{}", self.id);

        self.id += 1;

        assert!(self.statics.insert(label.clone(), value).is_none());

        label
    }

    pub fn insert_static_allocation(&mut self, value: MemoryValue) -> (String, String) {
        let label_allocation = self.insert_prefix("static_allocation", Mem(5, MemoryUnit::Words));

        let label_memory = self.insert_prefix("static_memory", value);

        self.static_allocation
            .push((label_allocation.to_owned(), label_memory.clone()));

        (label_allocation, label_memory)
    }

    pub fn get(&self, key: &str) -> Option<&MemoryValue> {
        self.statics.get(key)
    }

    pub fn is_empty(&self) -> bool {
        self.statics.is_empty()
    }

    pub fn const_names(&self) -> Vec<&String> {
        self.const_map.keys().collect::<Vec<_>>()
    }

    pub fn typed_const_names(&self) -> Vec<&String> {
        self.const_typed_map.keys().collect::<Vec<_>>()
    }

    pub fn insert_value_in_heap(&mut self, label: &str, descr: &str, value: i32) {
        let descr_key = self.add_str(descr);
        self.insert(label.to_owned(), MemoryValue::I32Value(0));
        self.heap.insert(label.to_owned(), (descr_key, value));
    }

    pub fn statics(&self) -> &LinkedHashMap<String, MemoryValue> {
        &self.statics
    }

    pub fn strings_map(&self) -> &LinkedHashMap<String, (String, String)> {
        &self.strings_map
    }

    pub fn static_allocation(&self) -> &Vec<(String, String)> {
        &self.static_allocation
    }

    pub fn heap(&self) -> &LinkedHashMap<String, (String, i32)> {
        &self.heap
    }
}
