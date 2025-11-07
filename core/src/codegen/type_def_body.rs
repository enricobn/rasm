use std::{collections::HashMap, sync::Mutex};

use once_cell::sync::Lazy;

// Global thread-safe cache for type def bodies
static GLOBAL_TYPE_DEF_CACHE: Lazy<Mutex<TypeDefBodyCache>> =
    Lazy::new(|| Mutex::new(TypeDefBodyCache::new()));

pub struct TypeDefBodyCache {
    c_cache: HashMap<String, CTypeDefBody>,
    asm_cache: HashMap<String, AsmTypeDefBody>,
}

impl TypeDefBodyCache {
    fn new() -> Self {
        Self {
            c_cache: HashMap::new(),
            asm_cache: HashMap::new(),
        }
    }

    fn get_c_internal(&mut self, body: &str) -> &CTypeDefBody {
        let body = body.trim();
        if !self.c_cache.contains_key(body) {
            self.c_cache
                .insert(body.to_owned(), parse_type_body_c(body));
        }
        self.c_cache.get(body).unwrap()
    }

    fn get_asm_internal(&mut self, body: &str) -> &AsmTypeDefBody {
        let body = body.trim();
        if !self.asm_cache.contains_key(body) {
            self.asm_cache
                .insert(body.to_owned(), parse_type_body_asm(body));
        }
        self.asm_cache.get(body).unwrap()
    }
}

impl TypeDefBodyCache {
    pub fn get_c(body: &str) -> CTypeDefBody {
        let mut cache = GLOBAL_TYPE_DEF_CACHE.lock().unwrap();
        // TODO can we remove clone?
        cache.get_c_internal(body).clone()
    }

    pub fn get_asm(body: &str) -> AsmTypeDefBody {
        let mut cache = GLOBAL_TYPE_DEF_CACHE.lock().unwrap();
        // TODO can we remove clone?
        cache.get_asm_internal(body).clone()
    }

    pub fn type_body_has_references(body: &str, kind: &TypeDefBodyTarget) -> bool {
        let mut cache = GLOBAL_TYPE_DEF_CACHE.lock().unwrap();
        match kind {
            TypeDefBodyTarget::C => cache.get_c_internal(body).has_references,
            TypeDefBodyTarget::Asm => cache.get_asm_internal(body).has_references,
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum TypeDefBodyTarget {
    C,
    Asm,
}

#[derive(Clone)]
pub struct CTypeDefBody {
    pub has_references: bool,
    pub native_type: String,
}

fn parse_type_body_c(body: &str) -> CTypeDefBody {
    let mut properties = parse_type_body(body);

    let has_references = properties
        .remove("hasReferences")
        .expect(&format!("C type body: missing hasReferences property"))
        == "true";

    let native_type = properties
        .remove("nativeType")
        .expect(&format!("C type body: missing nativeType property"))
        .to_owned();

    CTypeDefBody {
        has_references,
        native_type,
    }
}

#[derive(Clone)]
pub struct AsmTypeDefBody {
    pub has_references: bool,
}

fn parse_type_body_asm(body: &str) -> AsmTypeDefBody {
    let mut properties = parse_type_body(body);

    let has_references = properties
        .remove("hasReferences")
        .expect(&format!("asm type body: missing hasReferences property"))
        == "true";

    AsmTypeDefBody { has_references }
}

fn parse_type_body(body: &str) -> HashMap<&str, &str> {
    //    println!("parse_type_body");

    let mut result = HashMap::new();
    for property in body.split(",") {
        let parts = property.split("=").collect::<Vec<&str>>();
        if parts.len() == 2 {
            result.insert(parts[0].trim(), parts[1].trim());
        } else {
            panic!("invalid type body {body}");
        }
    }
    result
}
