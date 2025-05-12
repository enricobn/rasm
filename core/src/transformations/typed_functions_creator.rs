use linked_hash_map::LinkedHashMap;
use rasm_utils::debug_i;

use crate::codegen::enh_ast::{EnhASTIndex, EnhASTNameSpace};
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::get_reference_type_name;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::enh_type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedParameterDef,
    ASTTypedStructDef, ASTTypedType, ASTTypedTypeDef, BuiltinTypedTypeKind,
    ResolvedGenericTypedTypes,
};

pub trait TypedFunctionsCreator {
    fn create(
        &self,
        module: &EnhancedASTModule,
        typed_module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
    ) {
        for struct_def in typed_module.structs().iter() {
            self.for_struct(module, typed_module, functions_by_name, statics, struct_def);
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
    ) {
        if typed_type_def.is_ref {
            self.create_type_free(
                module,
                typed_type_def,
                "deref",
                typed_module,
                functions_by_name,
                statics,
            );
            self.create_type_free(
                module,
                typed_type_def,
                "addRef",
                typed_module,
                functions_by_name,
                statics,
            );
        }
    }

    fn for_enum(
        &self,
        _module: &EnhancedASTModule,
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
        _module: &EnhancedASTModule,
        typed_module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
        struct_def: &ASTTypedStructDef,
    ) {
        if struct_has_references(struct_def, typed_module) {
            self.create_struct_free(
                struct_def,
                "deref",
                typed_module,
                functions_by_name,
                statics,
            );
            self.create_struct_free(
                struct_def,
                "addRef",
                typed_module,
                functions_by_name,
                statics,
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
            resolved_generic_types: ResolvedGenericTypedTypes::new(),
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

        debug_i!("created function {function_def}");

        functions_by_name.insert(fun_name, function_def);
    }

    fn create_struct_free(
        &self,
        struct_def: &ASTTypedStructDef,
        function_name: &str,
        module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
    ) {
        let body_str = self.create_struct_free_body(struct_def, function_name, module, statics);
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
    ) {
        let body_str = self.create_type_free_body(
            module,
            typed_type_def,
            function_name,
            typed_module,
            statics,
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
    ) -> String;
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
