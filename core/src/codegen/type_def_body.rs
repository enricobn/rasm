use std::collections::HashMap;

pub enum TypeDefBodyTarget {
    C,
    Asm,
}

pub struct CTypeDefBody {
    pub has_references: bool,
    pub native_type: String,
}

pub fn parse_type_body_c(body: &str) -> CTypeDefBody {
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

pub struct AsmTypeDefBody {
    pub has_references: bool,
}

pub fn parse_type_body_asm(body: &str) -> AsmTypeDefBody {
    let mut properties = parse_type_body(body);

    let has_references = properties
        .remove("hasReferences")
        .expect(&format!("asm type body: missing hasReferences property"))
        == "true";

    AsmTypeDefBody { has_references }
}

pub fn type_body_has_references(body: &str, kind: &TypeDefBodyTarget) -> bool {
    match kind {
        TypeDefBodyTarget::C => parse_type_body_c(body).has_references,
        TypeDefBodyTarget::Asm => parse_type_body_asm(body).has_references,
    }
}

fn parse_type_body(body: &str) -> HashMap<&str, &str> {
    // println!("parse_type_body");
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
