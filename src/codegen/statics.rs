use linked_hash_map::LinkedHashMap;
use pad::PadStr;

use crate::codegen::backend::Backend;
use crate::codegen::text_macro::{TextMacroEvaluator, TypeDefProvider};
use crate::codegen::MemoryValue::Mem;
use crate::codegen::{CodeGen, MemoryUnit, MemoryValue};
use crate::parser::ast::ASTType;
use crate::type_check::typed_ast::{ASTTypedModule, ASTTypedType};

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

#[derive(Clone, Debug)]
pub struct Statics {
    id: usize,
    statics: LinkedHashMap<String, MemoryValue>,
    // string -> (label, value_label)
    strings_map: LinkedHashMap<String, (String, String)>,
    const_map: LinkedHashMap<String, ConstEntry>,
    const_typed_map: LinkedHashMap<String, ConstTypedEntry>,
}

impl Statics {
    pub fn new() -> Self {
        Self {
            id: 0,
            statics: LinkedHashMap::new(),
            strings_map: LinkedHashMap::new(),
            const_map: LinkedHashMap::new(),
            const_typed_map: LinkedHashMap::new(),
        }
    }

    pub fn add_str(&mut self, s: &str) -> String {
        if let Some((label, _)) = self.strings_map.get(s) {
            label.clone()
        } else {
            let value_label = self.insert_prefix("_sv".into(), MemoryValue::StringValue(s.into()));
            let label = self.insert_prefix("_s".into(), Mem(1, MemoryUnit::Words));

            self.strings_map
                .insert(s.into(), (label.clone(), value_label));

            label
        }
    }

    pub fn add_const(&mut self, name: String, ast_type: ASTType) {
        // TODO it's a trick: during type check passes consts are added again
        if !self.const_map.contains_key(&name) {
            let key =
                self.insert_prefix("const".to_owned(), MemoryValue::Mem(1, MemoryUnit::Words));
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
        assert!(self.statics.insert(key, value).is_none())
        //assert!(self.statics.insert(key.clone(), value.clone()).is_none(), "{key} {:?}", value)
    }

    pub fn insert_prefix(&mut self, prefix: String, value: MemoryValue) -> String {
        let label = format!("{prefix}_{}", self.id);

        self.id += 1;

        assert!(self.statics.insert(label.clone(), value).is_none());

        label
    }

    pub fn generate_code(&mut self, backend: &dyn Backend) -> (String, String, String) {
        let mut data = String::new();
        let mut bss = String::new();

        let mut code = String::new();

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

                        let mut result = "'".to_string();

                        // TODO it is a naive way to do it: it is slow and it does not support something like \\n that should result in '\' as a char and 'n' as a char
                        for c in s.replace("\\n", "\n").replace("\\t", "\t").chars() {
                            if c.is_ascii_control() {
                                result.push_str(&format!("',{},'", c as u32));
                            } else {
                                result.push(c)
                            }
                        }

                        result.push_str("', 0h");

                        def.push_str(&result);

                        CodeGen::add(&mut data, &def, None, true);
                    }
                    MemoryValue::I32Value(i) => {
                        def.push_str("dd    ");
                        def.push_str(&format!("{}", i));
                        CodeGen::add(&mut data, &def, None, true);
                    }
                    Mem(len, unit) => {
                        match unit {
                            MemoryUnit::Bytes => def.push_str("resb "),
                            MemoryUnit::Words => def.push_str("resd "),
                        }
                        def.push_str(&format!("{}", len));
                        CodeGen::add(&mut bss, &def, None, true);
                    }
                    MemoryValue::RefToLabel(name) => {
                        def.push_str(&format!("dd    {name}"));
                        CodeGen::add(&mut data, &def, None, true);
                    }
                }
            }
        }

        for (_, (key, value_key)) in self.strings_map.iter() {
            CodeGen::add(&mut code, &format!("push dword {value_key}"), None, true);
            // TODO
            CodeGen::add(&mut code, "call addStaticStringToHeap_0", None, true);
            CodeGen::add(
                &mut code,
                &format!("add {},{}", backend.stack_pointer(), backend.word_len()),
                None,
                true,
            );

            CodeGen::add(&mut code, &format!("mov dword [{key}], eax"), None, true);
        }
        (data, bss, code)
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

    fn print_res(
        &mut self,
        backend: &dyn Backend,
        module: &ASTTypedModule,
        dereference: bool,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
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
                MemoryValue::RefToLabel(_) => {}
            }
        }
        CodeGen::add(&mut asm, "%endif", None, false);

        TextMacroEvaluator::new().translate(
            backend,
            self,
            None,
            &asm,
            dereference,
            false,
            type_def_provider,
        )
    }

    fn print_address(asm: &mut String, address: &str) {
        let address_name = address.pad_to_width(50);
        CodeGen::add(
            asm,
            &format!("$call(print, \"{address_name} \")"),
            None,
            false,
        );
        CodeGen::add(asm, &format!("$call(nprintln, {address})"), None, false);
    }
}
