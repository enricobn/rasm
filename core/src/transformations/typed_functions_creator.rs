use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::codegen::backend::Backend;
use crate::codegen::backend::{BackendAsm, BackendNasmi386};
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_reference_type_name, CompileTarget};
use crate::parser::ast::{ASTIndex, ASTNameSpace};
use crate::type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedStructDef, ASTTypedType, ASTTypedTypeDef, BuiltinTypedTypeKind,
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
            namespace: struct_def.namespace.clone(),
            name: struct_def.name.clone(),
        };

        let body_str = self.create_struct_free_body(
            struct_def,
            asm_function_name,
            function_name_suffix,
            module,
            statics,
        );
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        self.add_function(
            &struct_def.namespace,
            module,
            ast_type,
            body,
            function_name_suffix,
            &struct_def.name,
            false,
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
            namespace: enum_def.namespace.clone(),
            name: enum_def.name.clone(),
        };

        let body_str = self.create_enum_free_body(
            enum_def,
            asm_function_name,
            function_name_suffix,
            module,
            statics,
        );
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        self.add_function(
            &enum_def.namespace,
            module,
            ast_type,
            body,
            function_name_suffix,
            &enum_def.name,
            false,
        );
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
            namespace: typed_type_def.namespace.clone(),
            name: typed_type_def.name.clone(),
        };

        let body_str = self.create_type_free_body(
            typed_type_def,
            asm_function_name,
            function_name_suffix,
            module,
            statics,
        );
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        self.add_function(
            &typed_type_def.namespace,
            module,
            ast_type,
            body,
            function_name_suffix,
            &typed_type_def.name,
            true,
        );
    }

    fn add_function(
        &self,
        namespace: &ASTNameSpace,
        module: &mut ASTTypedModule,
        ast_type: ASTTypedType,
        body: ASTTypedFunctionBody,
        function_name_suffix: &str,
        name: &str,
        with_descr: bool,
    ) {
        let fun_name = format!("{}_{function_name_suffix}", name);

        let mut parameters = vec![ASTTypedParameterDef {
            name: "address".into(),
            ast_type,
            ast_index: ASTIndex::none(),
        }];

        if with_descr {
            parameters.push(ASTTypedParameterDef {
                name: "descr".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: ASTIndex::none(),
            })
        }

        let function_def = ASTTypedFunctionDef {
            namespace: namespace.clone(),
            name: fun_name.clone(),
            original_name: fun_name.clone(),
            parameters,
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

pub struct TypedFunctionsCreatorNasmi386 {
    backend: BackendNasmi386,
    target: CompileTarget,
    debug: bool,
}

impl TypedFunctionsCreatorNasmi386 {
    pub fn new(backend: BackendNasmi386, debug: bool) -> Self {
        Self {
            backend,
            target: CompileTarget::Nasmi36,
            debug,
        }
    }

    fn loop_vec(
        &self,
        type_def: &ASTTypedTypeDef,
        deref_function_call: String,
        generic_n: usize,
    ) -> String {
        let mut result = String::new();

        self.target.add_rows(
            &mut result,
            vec!["push  eax", "push  ebx", "push  ecx"],
            None,
            true,
        );

        self.target.call_function(
            &mut result,
            &format!("{}References", type_def.original_name),
            &[("$address", None), (&format!("{generic_n}"), None)],
            None,
            self.debug,
        );

        self.target.add_rows(
            &mut result,
            vec![
                &format!("mov   {} ebx, [eax]", self.backend.word_size()),
                &format!("mov   {} ecx, [ebx]", self.backend.word_size()),
                &format!("add   ebx, {}", self.backend.word_len()),
                &format!(".loop_{generic_n}:"),
                &format!("cmp   {} ecx, 0", self.backend.word_size()),
                &format!("jz   .end_{generic_n}"),
                &deref_function_call,
                "\n",
                &format!("add   ebx, {}", self.backend.word_len()),
                "dec ecx",
                &format!("jmp .loop_{generic_n}"),
                &format!(".end_{generic_n}:"),
                "pop  ecx",
                "pop  ebx",
                "pop  eax",
            ],
            None,
            true,
        );
        result
    }
}

impl TypedFunctionsCreator for TypedFunctionsCreatorNasmi386 {
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

        self.target.call_function(
            &mut result,
            asm_function_name,
            &[("$address", None), (&format!("[{key}]"), None)],
            Some(&descr),
            self.debug,
        );

        if struct_has_references(struct_def, module) {
            self.target.add_rows(
                &mut result,
                vec![
                    &format!("push {ws} ebx"),
                    &format!("mov  {ws} ebx, $address"),
                    &format!("mov  {ws} ebx, [ebx]"),
                ],
                None,
                true,
            );
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

            self.target.add(&mut result, "pop ebx", None, true);
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

        if enum_has_references(enum_def, module) {
            self.target.add_rows(
                &mut result,
                vec![
                    &format!("push {ws} ebx"),
                    &format!("mov  {ws} ebx, $address"),
                    &format!("mov  {ws} ebx, [ebx]"),
                ],
                None,
                true,
            );
            for (i, variant) in enum_def.clone().variants.iter().enumerate() {
                if !variant.parameters.is_empty() {
                    self.target.add_rows(
                        &mut result,
                        vec![
                            &format!("cmp {ws} [ebx], {}", i),
                            &format!("jne ._variant_{i}"),
                        ],
                        None,
                        true,
                    );
                    self.target.call_function(
                        &mut result,
                        asm_function_name,
                        &[("$address", None), (&format!("[{key}]"), None)],
                        Some(&descr),
                        self.debug,
                    );
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
                    self.target.add_rows(
                        &mut result,
                        vec!["jmp .end", &format!("._variant_{i}:")],
                        None,
                        true,
                    );
                }
            }
            self.target
                .add_rows(&mut result, vec![".end:", "pop ebx"], None, true);
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

        self.target.call_function(
            &mut result,
            asm_function_name,
            &[("$address", None), ("$descr", None)],
            Some(&descr),
            self.debug,
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
