use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::codegen::backend::BackendAsm;
use crate::codegen::get_reference_type_name;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::parser::ast::ASTIndex;
use crate::type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedStructDef, ASTTypedType, ASTTypedTypeDef,
};

pub trait TypedFunctionsCreator {
    fn create(&self, module: &mut ASTTypedModule, statics: &mut Statics) {
        for struct_def in module.structs.clone().iter() {
            if struct_has_references(struct_def, module) {
                self.create_struct_free(struct_def, "deref", "deref", module, statics);
                self.create_struct_free(struct_def, "addRef", "addRef", module, statics);
            }
        }

        for enum_def in module.enums.clone().iter() {
            if enum_has_references(enum_def, module) {
                self.create_enum_free(enum_def, "deref", "deref", module, statics);
                self.create_enum_free(enum_def, "addRef", "addRef", module, statics);
            }
        }

        for typed_type_def in module.types.clone().iter() {
            if typed_type_def.is_ref {
                self.create_type_free(typed_type_def, "deref", "deref", module, statics);
                self.create_type_free(typed_type_def, "addRef", "addRef", module, statics);
            }
        }
    }

    fn create_struct_free(
        &self,
        struct_def: &ASTTypedStructDef,
        asm_function_name: &str,
        function_name_suffix: &str,
        module: &mut ASTTypedModule,
        statics: &mut Statics,
    ) {
        let ast_type = ASTTypedType::Struct {
            name: struct_def.name.clone(),
        };

        let body_str = self.create_struct_free_body(
            struct_def,
            asm_function_name,
            function_name_suffix,
            module,
            statics,
        );
        let body = ASTTypedFunctionBody::ASMBody(body_str);

        self.add_function(
            module,
            ast_type,
            body,
            function_name_suffix,
            &struct_def.name,
        );
    }

    fn create_enum_free(
        &self,
        enum_def: &ASTTypedEnumDef,
        asm_function_name: &str,
        function_name_suffix: &str,
        module: &mut ASTTypedModule,
        statics: &mut Statics,
    ) {
        let ast_type = ASTTypedType::Struct {
            name: enum_def.name.clone(),
        };

        let body_str = self.create_enum_free_body(
            enum_def,
            asm_function_name,
            function_name_suffix,
            module,
            statics,
        );
        let body = ASTTypedFunctionBody::ASMBody(body_str);

        self.add_function(module, ast_type, body, function_name_suffix, &enum_def.name);
    }

    fn create_type_free(
        &self,
        typed_type_def: &ASTTypedTypeDef,
        asm_function_name: &str,
        function_name_suffix: &str,
        module: &mut ASTTypedModule,
        statics: &mut Statics,
    ) {
        let ast_type = ASTTypedType::Struct {
            name: typed_type_def.name.clone(),
        };

        let body_str = self.create_type_free_body(
            typed_type_def,
            asm_function_name,
            function_name_suffix,
            module,
            statics,
        );
        let body = ASTTypedFunctionBody::ASMBody(body_str);

        self.add_function(
            module,
            ast_type,
            body,
            function_name_suffix,
            &typed_type_def.name,
        );
    }

    fn add_function(
        &self,
        module: &mut ASTTypedModule,
        ast_type: ASTTypedType,
        body: ASTTypedFunctionBody,
        function_name_suffix: &str,
        name: &str,
    ) {
        let fun_name = format!("{}_{function_name_suffix}", name);

        let function_def = ASTTypedFunctionDef {
            name: fun_name.clone(),
            original_name: fun_name.clone(),
            parameters: vec![ASTTypedParameterDef {
                name: "address".into(),
                ast_type,
                ast_index: ASTIndex::none(),
            }],
            body,
            inline: false,
            return_type: ASTTypedType::Unit,
            generic_types: LinkedHashMap::new(),
            index: ASTIndex::none(),
        };

        debug!("created function {function_def}");

        module.functions_by_name.insert(fun_name, function_def);
    }

    fn create_struct_free_body(
        &self,
        struct_def: &ASTTypedStructDef,
        asm_function_name: &str,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String;

    fn create_enum_free_body(
        &self,
        enum_def: &ASTTypedEnumDef,
        asm_function_name: &str,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String;

    fn create_type_free_body(
        &self,
        type_def: &ASTTypedTypeDef,
        asm_function_name: &str,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String;
}

pub struct TypedFunctionsCreatorAsm<'a> {
    backend: Box<&'a dyn BackendAsm>,
}

impl<'a> TypedFunctionsCreatorAsm<'a> {
    pub fn new(backend: Box<&'a dyn BackendAsm>) -> Self {
        Self { backend }
    }

    fn loop_vec(
        &self,
        type_def: &ASTTypedTypeDef,
        deref_function_call: String,
        generic_n: usize,
    ) -> String {
        let mut result = String::new();

        self.backend.add(&mut result, "push  eax", None, true);
        self.backend.add(&mut result, "push  ebx", None, true);
        self.backend.add(&mut result, "push  ecx", None, true);

        self.backend.call_function(
            &mut result,
            &format!("{}References_0", type_def.original_name),
            &[("$address", None), (&format!("{generic_n}"), None)],
            None,
        );

        self.backend.add(
            &mut result,
            &format!("mov   {} ebx, [eax]", self.backend.word_size()),
            None,
            true,
        );
        self.backend.add(
            &mut result,
            &format!("mov   {} ecx, [ebx]", self.backend.word_size()),
            None,
            true,
        );
        self.backend.add(
            &mut result,
            &format!("add   ebx, {}", self.backend.word_len()),
            None,
            true,
        );
        self.backend
            .add(&mut result, &format!(".loop_{generic_n}:"), None, false);
        self.backend.add(
            &mut result,
            &format!("cmp   {} ecx, 0", self.backend.word_size()),
            None,
            true,
        );
        self.backend
            .add(&mut result, &format!("jz   .end_{generic_n}"), None, true);
        result.push_str(&deref_function_call);
        result.push('\n');
        self.backend.add(
            &mut result,
            &format!("add   ebx, {}", self.backend.word_len()),
            None,
            true,
        );
        self.backend.add(&mut result, "dec ecx", None, true);
        self.backend
            .add(&mut result, &format!("jmp .loop_{generic_n}"), None, true);
        self.backend
            .add(&mut result, &format!(".end_{generic_n}:"), None, false);

        self.backend.add(&mut result, "pop  ecx", None, true);
        self.backend.add(&mut result, "pop  ebx", None, true);
        self.backend.add(&mut result, "pop  eax", None, true);
        result
    }
}

impl<'a> TypedFunctionsCreator for TypedFunctionsCreatorAsm<'a> {
    fn create_struct_free_body(
        &self,
        struct_def: &ASTTypedStructDef,
        asm_function_name: &str,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String {
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

        let mut result = String::new();

        let descr = if self.backend.debug_asm() {
            format!("type {}", struct_def.name)
        } else {
            String::new()
        };

        let key = statics.add_str(&descr);

        self.backend.call_function(
            &mut result,
            &format!("{asm_function_name}_0"),
            &[("$address", None), (&format!("[{key}]"), None)],
            Some(&descr),
        );

        if struct_has_references(struct_def, module) {
            self.backend
                .add(&mut result, &format!("push {ws} ebx"), None, true);
            self.backend
                .add(&mut result, &format!("mov {ws} ebx, $address"), None, true);
            self.backend
                .add(&mut result, &format!("mov {ws} ebx, [ebx]"), None, true);
            for (i, property) in struct_def.clone().properties.iter().enumerate() {
                if let Some(name) = get_reference_type_name(&property.ast_type, module) {
                    let descr = &format!("{}.{} : {}", struct_def.name, property.name, name);
                    if function_name == "deref" {
                        result.push_str(&self.backend.call_deref(
                            &format!("[ebx + {}]", i * wl),
                            &name,
                            descr,
                            module,
                            statics,
                        ));
                        result.push('\n');
                    } else {
                        self.backend.call_add_ref(
                            &mut result,
                            &format!("[ebx + {}]", i * wl),
                            &name,
                            descr,
                            module,
                            statics,
                        );
                    }
                }
            }

            self.backend.add(&mut result, "pop ebx", None, true);
        }

        result
    }

    fn create_enum_free_body(
        &self,
        enum_def: &ASTTypedEnumDef,
        asm_function_name: &str,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String {
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

        let mut result = String::new();

        let descr = if self.backend.debug_asm() {
            format!("type {}", enum_def.name)
        } else {
            String::new()
        };

        let key = statics.add_str(&descr);

        self.backend.call_function(
            &mut result,
            &format!("{asm_function_name}_0"),
            &[("$address", None), (&format!("[{key}]"), None)],
            Some(&descr),
        );

        if enum_has_references(enum_def, module) {
            self.backend
                .add(&mut result, &format!("push {ws} ebx"), None, true);
            self.backend
                .add(&mut result, &format!("mov {ws} ebx,$address"), None, true);
            self.backend
                .add(&mut result, &format!("mov {ws} ebx, [ebx]"), None, true);
            for (i, variant) in enum_def.clone().variants.iter().enumerate() {
                if !variant.parameters.is_empty() {
                    self.backend
                        .add(&mut result, &format!("cmp {ws} [ebx], {}", i), None, true);
                    self.backend
                        .add(&mut result, &format!("jne ._variant_{i}"), None, true);
                    for (j, par) in variant.parameters.iter().rev().enumerate() {
                        if let Some(name) = get_reference_type_name(&par.ast_type, module) {
                            let descr = &format!("{}.{} : {}", enum_def.name, par.name, name);
                            if function_name == "deref" {
                                result.push_str(&self.backend.call_deref(
                                    &format!("[ebx + {}]", (j + 1) * wl),
                                    &name,
                                    descr,
                                    module,
                                    statics,
                                ));
                                result.push('\n');
                            } else {
                                self.backend.call_add_ref(
                                    &mut result,
                                    &format!("[ebx + {}]", (j + 1) * wl),
                                    &name,
                                    descr,
                                    module,
                                    statics,
                                );
                            }
                        }
                    }
                    self.backend.add(&mut result, "jmp .end", None, true);
                    self.backend
                        .add(&mut result, &format!("._variant_{i}:"), None, false);
                }
            }
            self.backend.add(&mut result, ".end:", None, true);
            self.backend.add(&mut result, "pop ebx", None, true);
        }

        result
    }

    fn create_type_free_body(
        &self,
        type_def: &ASTTypedTypeDef,
        asm_function_name: &str,
        function_name: &str,
        module: &ASTTypedModule,
        statics: &mut Statics,
    ) -> String {
        if !type_def.is_ref {
            return String::new();
        }

        let mut result = String::new();
        let descr = format!("type {}", type_def.name);

        self.backend.call_function(
            &mut result,
            &format!("{asm_function_name}_0"),
            &[("$address", None), ("$descr", None)],
            Some(&descr),
        );

        if type_has_references(type_def) {
            for (i, (_generic_name, generic_type_def)) in type_def.generic_types.iter().enumerate()
            {
                if let Some(name) = get_reference_type_name(generic_type_def, module) {
                    let descr = "$descr";
                    let call_deref = if function_name == "deref" {
                        self.backend
                            .call_deref("[ebx]", &name, descr, module, statics)
                    } else {
                        let mut s = String::new();
                        self.backend
                            .call_add_ref(&mut s, "[ebx]", &name, descr, module, statics);
                        s
                    };

                    let loop_vec = self.loop_vec(type_def, call_deref, i);

                    result.push_str(&loop_vec);
                }
            }
        }
        result
    }
}

pub fn struct_has_references(
    struct_def: &ASTTypedStructDef,
    type_def_provider: &dyn TypeDefProvider,
) -> bool {
    struct_def
        .properties
        .iter()
        .any(|it| get_reference_type_name(&it.ast_type, type_def_provider).is_some())
}

pub fn enum_has_references(
    enum_def: &ASTTypedEnumDef,
    type_def_provider: &dyn TypeDefProvider,
) -> bool {
    enum_def
        .variants
        .iter()
        .flat_map(|it| it.parameters.iter())
        .any(|it| get_reference_type_name(&it.ast_type, type_def_provider).is_some())
}

pub fn type_has_references(type_def: &ASTTypedTypeDef) -> bool {
    type_def.is_ref
}
