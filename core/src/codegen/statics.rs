use anymap::AnyMap;
use linked_hash_map::LinkedHashMap;
use rasm_parser::parser::ast::ASTModifiers;

use crate::codegen::enh_ast::EnhASTType;
use crate::codegen::statics::MemoryValue::Mem;
use crate::enh_type_check::typed_ast::ASTTypedType;

use super::enh_ast::EnhASTNameSpace;

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
    pub ast_type: EnhASTType,
    pub modifiers: ASTModifiers,
}

#[derive(Clone, Debug)]
pub struct ConstTypedEntry {
    pub key: String,
    pub ast_typed_type: ASTTypedType,
    pub modifiers: ASTModifiers,
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
    any: AnyMap,
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
            any: AnyMap::new(),
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

    pub fn add_const(
        &mut self,
        name: String,
        ast_type: EnhASTType,
        namespace: &EnhASTNameSpace,
        modifiers: &ASTModifiers,
    ) {
        let const_key = Self::const_key(&name, namespace, modifiers);

        // TODO it's a trick: during type check passes consts are added again
        if !self.const_map.contains_key(&const_key) {
            self.insert(const_key.clone(), MemoryValue::Mem(1, MemoryUnit::Words));

            self.const_map.insert(
                const_key.clone(),
                ConstEntry {
                    key: const_key.clone(),
                    ast_type,
                    modifiers: modifiers.clone(),
                },
            );
        }
    }

    pub fn const_key(name: &str, namespace: &EnhASTNameSpace, modifiers: &ASTModifiers) -> String {
        if modifiers.public {
            name.to_owned()
        } else {
            format!("{}_{name}", namespace.safe_name())
        }
    }

    pub fn add_typed_const(
        &mut self,
        name: String,
        ast_typed_type: ASTTypedType,
        namespace: &EnhASTNameSpace,
        modifiers: &ASTModifiers,
    ) -> String {
        let const_key = Self::const_key(&name, namespace, modifiers);
        if let Some(entry) = self.get_const(&const_key, namespace) {
            self.const_typed_map.insert(
                const_key.to_owned(),
                ConstTypedEntry {
                    key: const_key.clone(),
                    ast_typed_type,
                    modifiers: modifiers.clone(),
                },
            );
            const_key
        } else {
            panic!("cannot find const {namespace}:{name} {}", modifiers.public);
        }
    }

    pub fn get_const(&self, name: &str, namespace: &EnhASTNameSpace) -> Option<&ConstEntry> {
        let const_key = Self::const_key(&name, namespace, &ASTModifiers::private());
        if let Some(entry) = self.const_map.get(&const_key) {
            return Some(entry);
        }
        let const_key = Self::const_key(&name, namespace, &ASTModifiers::public());
        self.const_map.get(&const_key)
    }

    pub fn get_typed_const(
        &self,
        name: &str,
        namespace: &EnhASTNameSpace,
    ) -> Option<&ConstTypedEntry> {
        let const_key = Self::const_key(&name, namespace, &ASTModifiers::private());
        if let Some(entry) = self.const_typed_map.get(&const_key) {
            return Some(entry);
        }
        let const_key = Self::const_key(&name, namespace, &ASTModifiers::public());
        self.const_typed_map.get(&const_key)
    }

    pub fn insert(&mut self, key: String, value: MemoryValue) {
        if let Some(_old_value) = self.statics.insert(key.clone(), value) {
            panic!("already added value for {key}");
        }
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

    pub fn add_any<T: 'static>(&mut self, value: T) {
        self.any.insert::<T>(value);
    }

    pub fn any_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.any.get_mut::<T>()
    }

    pub fn any<T: 'static>(&self) -> Option<&T> {
        self.any.get::<T>()
    }
}
