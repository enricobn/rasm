use linked_hash_map::LinkedHashMap;
use rasm_utils::debug_i;

use crate::codegen::enh_ast::{EnhASTIndex, EnhASTNameSpace};
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::get_reference_type_name;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::RefType;
use crate::codegen::type_def_body::{TypeDefBodyCache, TypeDefBodyTarget};
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
        let is_ref = TypeDefBodyCache::type_body_has_references(
            &typed_type_def.body,
            &self.type_def_body_target(),
        );
        if is_ref {
            self.create_type_free(
                module,
                typed_type_def,
                RefType::Deref,
                typed_module,
                functions_by_name,
                statics,
            );
            self.create_type_free(
                module,
                typed_type_def,
                RefType::AddRef,
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
        if enum_has_references(enum_def, self.type_def_body_target()) {
            self.create_enum_free(
                enum_def,
                RefType::Deref,
                typed_module,
                functions_by_name,
                statics,
            );
            self.create_enum_free(
                enum_def,
                RefType::AddRef,
                typed_module,
                functions_by_name,
                statics,
            );
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
        if struct_has_references(struct_def, self.type_def_body_target()) {
            self.create_struct_free(
                struct_def,
                RefType::Deref,
                typed_module,
                functions_by_name,
                statics,
            );
            self.create_struct_free(
                struct_def,
                RefType::AddRef,
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
        ref_type: RefType,
        module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
    ) {
        let body_str = self.create_struct_free_body(struct_def, ref_type, module, statics);
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        self.add_function(
            &struct_def.namespace,
            functions_by_name,
            struct_def.ast_typed_type.clone(),
            body,
            ref_type.function_name(),
            &struct_def.name,
            false,
        );
    }

    fn create_enum_free(
        &self,
        enum_def: &ASTTypedEnumDef,
        ref_type: RefType,
        module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
    ) {
        let body_str = self.create_enum_free_body(enum_def, ref_type, module, statics);
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        self.add_function(
            &enum_def.namespace,
            functions_by_name,
            enum_def.ast_typed_type.clone(),
            body,
            ref_type.function_name(),
            &enum_def.name,
            false,
        );
    }

    fn create_type_free(
        &self,
        module: &EnhancedASTModule,
        typed_type_def: &ASTTypedTypeDef,
        ref_type: RefType,
        typed_module: &dyn TypeDefProvider,
        functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
        statics: &mut Statics,
    ) {
        let body_str =
            self.create_type_free_body(module, typed_type_def, ref_type, typed_module, statics);
        let body = ASTTypedFunctionBody::NativeBody(body_str);

        self.add_function(
            &typed_type_def.namespace,
            functions_by_name,
            typed_type_def.ast_typed_type.clone(),
            body,
            ref_type.function_name(),
            &typed_type_def.name,
            self.addref_with_descr(),
        );
    }

    fn addref_with_descr(&self) -> bool;

    fn create_struct_free_body(
        &self,
        struct_def: &ASTTypedStructDef,
        ref_type: RefType,
        module: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String;

    fn create_enum_free_body(
        &self,
        enum_def: &ASTTypedEnumDef,
        ref_type: RefType,
        module: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String;

    fn create_type_free_body(
        &self,
        module: &EnhancedASTModule,
        type_def: &ASTTypedTypeDef,
        ref_type: RefType,
        typed_module: &dyn TypeDefProvider,
        statics: &mut Statics,
    ) -> String;

    fn type_def_body_target(&self) -> TypeDefBodyTarget;
}

pub fn struct_has_references(struct_def: &ASTTypedStructDef, target: TypeDefBodyTarget) -> bool {
    struct_def
        .properties
        .iter()
        .any(|it| get_reference_type_name(&it.ast_type, &target).is_some())
}

pub fn enum_has_references(enum_def: &ASTTypedEnumDef, target: TypeDefBodyTarget) -> bool {
    enum_def
        .variants
        .iter()
        .flat_map(|it| it.parameters.iter())
        .any(|it| get_reference_type_name(&it.ast_type, &target).is_some())
}

pub fn type_has_references(type_def: &ASTTypedTypeDef, target: TypeDefBodyTarget) -> bool {
    TypeDefBodyCache::type_body_has_references(&type_def.body, &target)
}
