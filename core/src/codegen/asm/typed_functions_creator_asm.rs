use crate::{
    codegen::{
        enhanced_module::EnhancedASTModule, get_reference_type_name, statics::Statics,
        typedef_provider::TypeDefProvider, CodeGen,
    },
    enh_type_check::typed_ast::{ASTTypedEnumDef, ASTTypedStructDef, ASTTypedTypeDef},
    transformations::typed_functions_creator::{
        enum_has_references, struct_has_references, type_has_references, TypedFunctionsCreator,
    },
};

use super::{
    backend::{Backend, BackendAsm, BackendNasmi386},
    code_gen_asm::CodeGenAsm,
};

pub struct TypedFunctionsCreatorNasmi386 {
    backend: BackendNasmi386,
    code_gen: CodeGenAsm,
}

impl TypedFunctionsCreatorNasmi386 {
    pub fn new(backend: BackendNasmi386, code_gen: CodeGenAsm) -> Self {
        Self { backend, code_gen }
    }

    fn loop_vec(
        &self,
        type_def: &ASTTypedTypeDef,
        deref_function_call: String,
        generic_n: usize,
    ) -> String {
        let mut result = String::new();

        self.code_gen.add_rows(
            &mut result,
            vec!["push  eax", "push  ebx", "push  ecx"],
            None,
            true,
        );

        self.code_gen.call_function(
            &mut result,
            &format!("{}References", type_def.original_name),
            &[("$address", None), (&format!("{generic_n}"), None)],
            None,
            false,
            false,
        );

        self.code_gen.add_rows(
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
        function_name: &str,
        module: &dyn TypeDefProvider,
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

        self.code_gen.call_function(
            &mut result,
            function_name,
            &[("$address", None), (&format!("[{key}]"), None)],
            Some(&descr),
            false,
            false,
        );

        if struct_has_references(struct_def, module) {
            self.code_gen.add_rows(
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
                        result.push_str(&self.code_gen.call_deref(
                            &format!("[ebx + {}]", i * wl),
                            &name,
                            descr,
                            module,
                            statics,
                        ));
                        result.push('\n');
                    } else {
                        self.code_gen.call_add_ref(
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

            self.code_gen.add(&mut result, "pop ebx", None, true);
        }

        result
    }

    fn create_enum_free_body(
        &self,
        enum_def: &ASTTypedEnumDef,
        function_name: &str,
        module: &dyn TypeDefProvider,
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
            self.code_gen.add_rows(
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
                    self.code_gen.add_rows(
                        &mut result,
                        vec![
                            &format!("cmp {ws} [ebx], {}", i),
                            &format!("jne ._variant_{i}"),
                        ],
                        None,
                        true,
                    );
                    self.code_gen.call_function(
                        &mut result,
                        function_name,
                        &[("$address", None), (&format!("[{key}]"), None)],
                        Some(&descr),
                        false,
                        false,
                    );
                    for (j, par) in variant.parameters.iter().rev().enumerate() {
                        if let Some(name) = get_reference_type_name(&par.ast_type, module) {
                            let descr = &format!("{}.{} : {}", enum_def.name, par.name, name);
                            if function_name == "deref" {
                                result.push_str(&self.code_gen.call_deref(
                                    &format!("[ebx + {}]", (j + 1) * wl),
                                    &name,
                                    descr,
                                    module,
                                    statics,
                                ));
                                result.push('\n');
                            } else {
                                self.code_gen.call_add_ref(
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
                    self.code_gen.add_rows(
                        &mut result,
                        vec!["jmp .end", &format!("._variant_{i}:")],
                        None,
                        true,
                    );
                }
            }
            self.code_gen
                .add_rows(&mut result, vec![".end:", "pop ebx"], None, true);
        }

        result
    }

    fn create_type_free_body(
        &self,
        _module: &EnhancedASTModule,
        type_def: &ASTTypedTypeDef,
        function_name: &str,
        typed_module: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String {
        if !type_def.is_ref {
            return String::new();
        }

        let mut result = String::new();
        let descr = format!("type {}", type_def.name);

        self.code_gen.call_function(
            &mut result,
            function_name,
            &[("$address", None), ("$descr", None)],
            Some(&descr),
            false,
            false,
        );

        if type_has_references(type_def) {
            for (i, (_generic_name, generic_type_def)) in type_def.generic_types.iter().enumerate()
            {
                if let Some(name) = get_reference_type_name(generic_type_def, typed_module) {
                    let descr = "$descr";
                    let call_deref = if function_name == "deref" {
                        self.code_gen
                            .call_deref("[ebx]", &name, descr, typed_module, statics)
                    } else {
                        let mut s = String::new();
                        self.code_gen.call_add_ref(
                            &mut s,
                            "[ebx]",
                            &name,
                            descr,
                            typed_module,
                            statics,
                        );
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
