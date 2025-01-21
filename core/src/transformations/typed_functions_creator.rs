use linked_hash_map::LinkedHashMap;
use log::debug;
use rasm_parser::catalog::modules_catalog::ModulesCatalog;

use crate::codegen::backend::Backend;
use crate::codegen::backend::{BackendAsm, BackendNasmi386};
use crate::codegen::enh_ast::{EnhASTIndex, EnhASTNameSpace, EnhModuleId};
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::{get_reference_type_name, CodeGen, CodeGenAsm};
use crate::type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedParameterDef,
    ASTTypedStructDef, ASTTypedType, ASTTypedTypeDef, BuiltinTypedTypeKind,
};

pub trait TypedFunctionsCreator {
    fn create(
        &self,
        module: &EnhancedASTModule,
        typed_module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    ) {
        for struct_def in typed_module.structs().iter() {
            self.for_struct(
                module,
                typed_module,
                functions_by_name,
                statics,
                struct_def,
                modules_catalog,
            );
        }

        for enum_def in typed_module.enums().iter() {
            self.for_enum(module, typed_module, functions_by_name, statics, enum_def);
        }

        for typed_type_def in typed_module.types().iter() {
            self.for_type(
                module,
                typed_module,
                functions_by_name,
                statics,
                typed_type_def,
                modules_catalog,
            );
        }
    }

    fn for_type(
        &self,
        module: &EnhancedASTModule,
        typed_module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
        typed_type_def: &ASTTypedTypeDef,
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    ) {
        if typed_type_def.is_ref {
            self.create_type_free(
                module,
                typed_type_def,
                "deref",
                typed_module,
                functions_by_name,
                statics,
                modules_catalog,
            );
            self.create_type_free(
                module,
                typed_type_def,
                "addRef",
                typed_module,
                functions_by_name,
                statics,
                modules_catalog,
            );
        }
    }

    fn for_enum(
        &self,
        module: &EnhancedASTModule,
        typed_module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
        enum_def: &ASTTypedEnumDef,
    ) {
        if enum_has_references(enum_def, typed_module) {
            self.create_enum_free(enum_def, "deref", typed_module, functions_by_name, statics);
            self.create_enum_free(enum_def, "addRef", typed_module, functions_by_name, statics);
        }
    }

    fn for_struct(
        &self,
        module: &EnhancedASTModule,
        typed_module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
        struct_def: &ASTTypedStructDef,
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    ) {
        if struct_has_references(struct_def, typed_module) {
            self.create_struct_free(
                struct_def,
                "deref",
                typed_module,
                functions_by_name,
                statics,
                modules_catalog,
            );
            self.create_struct_free(
                struct_def,
                "addRef",
                typed_module,
                functions_by_name,
                statics,
                modules_catalog,
            );
        }
    }

    fn create_function(
        &self,
        namespace: &EnhASTNameSpace,
        fun_name: &str,
        ast_type: ASTTypedType,
        body: ASTTypedFunctionBody,
        with_descr: bool,
    ) -> ASTTypedFunctionDef {
        let mut parameters = vec![ASTTypedParameterDef {
            name: "address".into(),
            ast_type,
            ast_index: EnhASTIndex::none(),
        }];

        if with_descr {
            parameters.push(ASTTypedParameterDef {
                name: "descr".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: EnhASTIndex::none(),
            })
        }

        ASTTypedFunctionDef {
            namespace: namespace.clone(),
            name: fun_name.to_string(),
            original_name: fun_name.to_ascii_lowercase(),
            parameters,
            body,
            inline: false,
            return_type: ASTTypedType::Unit,
            generic_types: LinkedHashMap::new(),
            index: EnhASTIndex::none(),
        }
    }

    fn add_function(
        &self,
        namespace: &EnhASTNameSpace,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        ast_type: ASTTypedType,
        body: ASTTypedFunctionBody,
        function_name_suffix: &str,
        name: &str,
        with_descr: bool,
    ) {
        let fun_name = format!("{}_{function_name_suffix}", name);

        let function_def = self.create_function(namespace, &fun_name, ast_type, body, with_descr);

        debug!("created function {function_def}");

        functions_by_name.insert(fun_name, function_def);
    }

    fn create_struct_free(
        &self,
        struct_def: &ASTTypedStructDef,
        function_name: &str,
        module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    ) {
        let body_str = self.create_struct_free_body(
            struct_def,
            function_name,
            module,
            statics,
            modules_catalog,
        );
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        self.add_function(
            &struct_def.namespace,
            functions_by_name,
            struct_def.ast_typed_type.clone(),
            body,
            function_name,
            &struct_def.name,
            false,
        );
    }

    fn create_enum_free(
        &self,
        enum_def: &ASTTypedEnumDef,
        function_name: &str,
        module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
    ) {
        let body_str = self.create_enum_free_body(enum_def, function_name, module, statics);
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        self.add_function(
            &enum_def.namespace,
            functions_by_name,
            enum_def.ast_typed_type.clone(),
            body,
            function_name,
            &enum_def.name,
            false,
        );
    }

    fn create_type_free(
        &self,
        module: &EnhancedASTModule,
        typed_type_def: &ASTTypedTypeDef,
        function_name: &str,
        typed_module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    ) {
        let ast_type = ASTTypedType::Struct {
            namespace: typed_type_def.namespace.clone(),
            name: typed_type_def.name.clone(),
        };

        let body_str = self.create_type_free_body(
            module,
            typed_type_def,
            function_name,
            typed_module,
            statics,
            modules_catalog,
        );
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        self.add_function(
            &typed_type_def.namespace,
            functions_by_name,
            typed_type_def.ast_typed_type.clone(),
            body,
            function_name,
            &typed_type_def.name,
            true,
        );
    }

    fn create_struct_free_body(
        &self,
        struct_def: &ASTTypedStructDef,
        function_name: &str,
        module: &dyn TypeDefProvider,
        statics: &mut Statics,
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    ) -> String;

    fn create_enum_free_body(
        &self,
        enum_def: &ASTTypedEnumDef,
        function_name: &str,
        module: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String;

    fn create_type_free_body(
        &self,
        module: &EnhancedASTModule,
        type_def: &ASTTypedTypeDef,
        function_name: &str,
        typed_module: &dyn TypeDefProvider,
        statics: &mut Statics,
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    ) -> String;
}

pub struct TypedFunctionsCreatorNasmi386 {
    backend: BackendNasmi386,
    debug: bool,
    code_gen: CodeGenAsm,
}

impl TypedFunctionsCreatorNasmi386 {
    pub fn new(backend: BackendNasmi386, code_gen: CodeGenAsm, debug: bool) -> Self {
        Self {
            backend,
            debug,
            code_gen,
        }
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
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
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
        module: &EnhancedASTModule,
        type_def: &ASTTypedTypeDef,
        function_name: &str,
        typed_module: &dyn TypeDefProvider,
        statics: &mut Statics,
        modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
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
