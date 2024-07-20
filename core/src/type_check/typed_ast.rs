use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

use linked_hash_map::LinkedHashMap;
use linked_hash_set::LinkedHashSet;
use log::{debug, info};

use crate::codegen::compile_target::CompileTarget;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::val_context::{TypedValContext, ValContext};
use crate::codegen::TypedValKind;
use crate::errors::{CompilationError, CompilationErrorKind};
use crate::new_type_check2::TypeCheck;
use crate::parser::ast::ASTFunctionBody::{NativeBody, RASMBody};
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
    ASTIndex, ASTLambdaDef, ASTModifiers, ASTNameSpace, ASTParameterDef, ASTStatement,
    ASTStructDef, ASTStructPropertyDef, ASTType, ASTTypeDef, BuiltinTypeKind, ValueType,
};
use crate::type_check::functions_container::TypeFilter;
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
use crate::type_check::type_check_error::TypeCheckError;
use crate::type_check::{get_new_native_call, substitute, verify};
use crate::utils::SliceDisplay;
use crate::{debug_i, dedent, indent};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedFunctionDef {
    pub namespace: ASTNameSpace,
    pub name: String,
    pub original_name: String,
    pub parameters: Vec<ASTTypedParameterDef>,
    pub return_type: ASTTypedType,
    pub body: ASTTypedFunctionBody,
    pub inline: bool,
    pub generic_types: LinkedHashMap<String, ASTTypedType>,
    pub index: ASTIndex,
}

impl ASTTypedFunctionDef {
    pub fn original_signature(&self) -> String {
        let pars: Vec<String> = self.parameters.iter().map(|it| format!("{}", it)).collect();
        let mut result = format!("{}({})", self.original_name, pars.join(","));
        if self.return_type != ASTTypedType::Unit {
            result.push_str(&format!(" -> {}", self.return_type));
        }
        result
    }
}

impl Display for ASTTypedFunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars: Vec<String> = self.parameters.iter().map(|it| format!("{}", it)).collect();
        f.write_str(&format!("{}({})", self.name, pars.join(",")))?;
        if self.return_type != ASTTypedType::Unit {
            f.write_str(&format!(" -> {}", self.return_type))?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedLambdaDef {
    pub parameter_names: Vec<(String, ASTIndex)>,
    pub body: Vec<ASTTypedStatement>,
    pub index: ASTIndex,
}

impl Display for ASTTypedLambdaDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .parameter_names
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<String>>()
            .join(",");
        let body = self
            .body
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<String>>()
            .join("");

        f.write_str(&format!("{{ {pars} -> {body} }}"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedFunctionBody {
    RASMBody(Vec<ASTTypedStatement>),
    NativeBody(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinTypedTypeKind {
    String,
    I32,
    Bool,
    Char,
    F32,
    Lambda {
        parameters: Vec<ASTTypedType>,
        return_type: Box<ASTTypedType>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ASTTypedType {
    Builtin(BuiltinTypedTypeKind),
    Enum {
        namespace: ASTNameSpace,
        name: String,
    },
    Struct {
        namespace: ASTNameSpace,
        name: String,
    },
    Type {
        namespace: ASTNameSpace,
        name: String,
        is_ref: bool,
        native_type: Option<String>,
    },
    Unit,
}

impl ASTTypedType {
    pub fn is_unit(&self) -> bool {
        self == &ASTTypedType::Unit
    }

    pub fn contains<F>(
        &self,
        type_def_provider: &dyn TypeDefProvider,
        check: &F,
        value_if_recursive: bool,
    ) -> bool
    where
        F: Fn(&ASTTypedType) -> bool,
    {
        let mut already_checked = LinkedHashSet::new();
        self.contains_(
            type_def_provider,
            check,
            value_if_recursive,
            &mut already_checked,
        )
    }

    fn contains_<F>(
        &self,
        type_def_provider: &dyn TypeDefProvider,
        check: &F,
        value_if_recursive: bool,
        already_checked: &mut LinkedHashSet<String>,
    ) -> bool
    where
        F: Fn(&ASTTypedType) -> bool,
    {
        if check(self) {
            return true;
        }
        match self {
            ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters,
                return_type,
            }) => false,
            ASTTypedType::Enum { namespace: _, name } => {
                if already_checked.contains(name) {
                    return value_if_recursive;
                }

                already_checked.insert(name.clone());

                if let Some(e) = type_def_provider.get_enum_def_by_name(name) {
                    let result = e
                        .variants
                        .iter()
                        .flat_map(|it| it.parameters.iter())
                        .any(|it| {
                            it.ast_type.contains_(
                                type_def_provider,
                                check,
                                value_if_recursive,
                                already_checked,
                            )
                        });
                    result
                } else {
                    panic!();
                }
            }
            ASTTypedType::Struct { namespace: _, name } => {
                if already_checked.contains(name) {
                    return value_if_recursive;
                }

                already_checked.insert(name.clone());

                if let Some(s) = type_def_provider.get_struct_def_by_name(name) {
                    let result = s.properties.iter().any(|it| {
                        it.ast_type.contains_(
                            type_def_provider,
                            check,
                            value_if_recursive,
                            already_checked,
                        )
                    });

                    result
                } else {
                    panic!()
                }
            }
            _ => false,
        }
    }
}

impl Display for ASTTypedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::String => f.write_str("str"),
                BuiltinTypedTypeKind::I32 => f.write_str("i32"),
                BuiltinTypedTypeKind::Bool => f.write_str("bool"),
                BuiltinTypedTypeKind::Char => f.write_str("char"),
                BuiltinTypedTypeKind::F32 => f.write_str("f32"),
                BuiltinTypedTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let pars: Vec<String> = parameters.iter().map(|it| format!("{it}")).collect();

                    let formatted_return_type = if return_type.deref() != &ASTTypedType::Unit {
                        format!("{}", return_type)
                    } else {
                        "()".into()
                    };

                    f.write_str(&format!(
                        "fn ({}) -> {}",
                        pars.join(","),
                        formatted_return_type
                    ))
                }
            },
            ASTTypedType::Enum { namespace: _, name } => f.write_str(&name.to_string()),
            ASTTypedType::Struct { namespace: _, name } => f.write_str(&name.to_string()),
            ASTTypedType::Type {
                namespace: _,
                name,
                native_type: _,
                is_ref: _,
            } => f.write_str(&name.to_string()),
            ASTTypedType::Unit => f.write_str("()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedParameterDef {
    pub name: String,
    pub ast_type: ASTTypedType,
    pub ast_index: ASTIndex,
}

impl Display for ASTTypedParameterDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}", self.name, self.ast_type))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedStructPropertyDef {
    pub name: String,
    pub ast_type: ASTTypedType,
}

impl Display for ASTTypedStructPropertyDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}: {}", self.name, self.ast_type))
    }
}

impl ASTTypedParameterDef {
    pub fn new(name: &str, ast_type: ASTTypedType, ast_index: ASTIndex) -> ASTTypedParameterDef {
        Self {
            name: name.into(),
            ast_type,
            ast_index,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedFunctionCall {
    pub namespace: ASTNameSpace,
    pub function_name: String,
    pub original_function_name: String,
    pub parameters: Vec<ASTTypedExpression>,
    pub index: ASTIndex,
}

impl ASTTypedFunctionCall {
    pub fn return_type(
        &self,
        context: &TypedValContext,
        typed_module: &ASTTypedModule,
    ) -> ASTTypedType {
        if let Some(kind) = context.get(&self.function_name) {
            let typed_type = kind.typed_type();

            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: _,
                return_type,
            }) = typed_type
            {
                return_type.deref().clone()
            } else {
                panic!("Expected lambda but got {typed_type}: {}", self.index);
            }
        } else {
            typed_module
                .functions_by_name
                .get(&self.function_name.replace("::", "_"))
                .expect(&self.function_name)
                .return_type
                .clone()
        }
    }
}

impl Display for ASTTypedFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars: Vec<String> = self.parameters.iter().map(|it| format!("{}", it)).collect();

        f.write_str(&format!("{}({})", self.function_name, pars.join(",")))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedExpression {
    StringLiteral(String),
    ASTFunctionCallExpression(ASTTypedFunctionCall),
    ValueRef(String, ASTIndex),
    Value(ValueType, ASTIndex),
    Lambda(ASTTypedLambdaDef),
}

impl ASTTypedExpression {
    pub fn get_index(&self) -> Option<ASTIndex> {
        match self {
            ASTTypedExpression::StringLiteral(_) => None,
            ASTTypedExpression::ASTFunctionCallExpression(call) => Some(call.index.clone()),
            ASTTypedExpression::ValueRef(_, index) => Some(index.clone()),
            ASTTypedExpression::Value(_, index) => Some(index.clone()),
            ASTTypedExpression::Lambda(_lambda) => None,
        }
    }

    pub fn namespace(&self) -> ASTNameSpace {
        match self {
            ASTTypedExpression::ASTFunctionCallExpression(call) => call.namespace.clone(),
            _ => ASTNameSpace::global(), // TODO the others (example lambda)?
        }
    }
}

impl Display for ASTTypedExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypedExpression::StringLiteral(s) => f.write_str(&format!("\"{s}\"")),
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                let pars: Vec<String> =
                    call.parameters.iter().map(|it| format!("{}", it)).collect();
                f.write_str(&format!("{}({})", call.function_name, pars.join(",")))
            }
            ASTTypedExpression::ValueRef(name, _index) => f.write_str(name),
            ASTTypedExpression::Value(val_type, _) => match val_type {
                ValueType::Boolean(b) => f.write_str(&format!("{b}")),
                ValueType::I32(n) => f.write_str(&format!("{n}")),
                ValueType::F32(n) => f.write_str(&format!("{n}")),
                ValueType::Char(c) => f.write_str(&format!("'{c}'")),
            },
            ASTTypedExpression::Lambda(lambda) => f.write_str(&format!("{lambda}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedStatement {
    Expression(ASTTypedExpression),
    LetStatement(String, ASTTypedExpression, bool, ASTIndex),
}

impl ASTTypedStatement {
    pub fn get_index(&self) -> Option<ASTIndex> {
        match self {
            ASTTypedStatement::Expression(e) => e.get_index(),
            ASTTypedStatement::LetStatement(_, _, _, index) => Some(index.clone()),
        }
    }
}

impl Display for ASTTypedStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypedStatement::Expression(e) => f.write_str(&format!("{e};\n")),
            ASTTypedStatement::LetStatement(name, e, is_const, _index) => {
                let keyword = if *is_const { "const" } else { "let" };
                f.write_str(&format!("{keyword} {name} = {e};\n"))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ASTTypedModule {
    pub body: Vec<ASTTypedStatement>,
    pub functions_by_name: LinkedHashMap<String, ASTTypedFunctionDef>,
    pub enums: Vec<ASTTypedEnumDef>,
    pub structs: Vec<ASTTypedStructDef>,
    pub types: Vec<ASTTypedTypeDef>,
}

impl TypeDefProvider for ASTTypedModule {
    fn enums(&self) -> &[ASTTypedEnumDef] {
        &self.enums
    }

    fn structs(&self) -> &[ASTTypedStructDef] {
        &self.structs
    }

    fn types(&self) -> &[ASTTypedTypeDef] {
        &self.types
    }

    fn name(&self) -> String {
        "ASTTypedModule".to_owned()
    }
}

impl ASTTypedModule {}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedEnumDef {
    pub namespace: ASTNameSpace,
    pub modifiers: ASTModifiers,
    pub name: String,
    pub variants: Vec<ASTTypedEnumVariantDef>,
    pub ast_type: ASTType,
    pub ast_typed_type: ASTTypedType,
    pub index: ASTIndex,
}

impl Display for ASTTypedEnumDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let variants = self
            .variants
            .iter()
            .map(|it| format!("  {it}"))
            .collect::<Vec<_>>()
            .join("\n");
        f.write_str(&format!("enum {} {{\n{variants}\n}}", self.name))
    }
}

impl ASTTypedEnumDef {
    pub fn variant_function_name(&self, variant: &ASTTypedEnumVariantDef) -> String {
        let mut result = String::new();
        result.push_str(&self.name);
        result.push_str("::");
        result.push_str(&variant.name);
        result
    }
}

impl CustomTypedTypeDef for ASTTypedEnumDef {
    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &ASTNameSpace {
        &self.namespace
    }

    fn ast_typed_type(&self) -> &ASTTypedType {
        &self.ast_typed_type
    }

    fn ast_type(&self) -> &ASTType {
        &self.ast_type
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedEnumVariantDef {
    pub name: String,
    pub parameters: Vec<ASTTypedParameterDef>,
}

impl Display for ASTTypedEnumVariantDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .parameters
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<_>>()
            .join(",");
        f.write_str(&format!("{}({pars})", self.name))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedStructDef {
    pub namespace: ASTNameSpace,
    pub modifiers: ASTModifiers,
    pub name: String,
    pub properties: Vec<ASTTypedStructPropertyDef>,
    pub ast_type: ASTType,
    pub ast_typed_type: ASTTypedType,
    pub index: ASTIndex,
}

impl CustomTypedTypeDef for ASTTypedStructDef {
    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &ASTNameSpace {
        &self.namespace
    }

    fn ast_typed_type(&self) -> &ASTTypedType {
        &self.ast_typed_type
    }

    fn ast_type(&self) -> &ASTType {
        &self.ast_type
    }
}

impl Display for ASTTypedStructDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .properties
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<_>>()
            .join(",");
        f.write_str(&format!("struct {}:{}({pars})", self.namespace, self.name))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedTypeDef {
    pub namespace: ASTNameSpace,
    pub modifiers: ASTModifiers,
    pub original_name: String,
    pub name: String,
    pub generic_types: LinkedHashMap<String, ASTTypedType>,
    pub is_ref: bool,
    pub ast_type: ASTType,
    pub ast_typed_type: ASTTypedType,
    pub index: ASTIndex,
    pub native_type: Option<String>,
}

pub trait CustomTypedTypeDef: Display + Debug {
    fn modifiers(&self) -> &ASTModifiers;

    fn namespace(&self) -> &ASTNameSpace;

    fn ast_typed_type(&self) -> &ASTTypedType;

    fn ast_type(&self) -> &ASTType;
}

impl CustomTypedTypeDef for ASTTypedTypeDef {
    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &ASTNameSpace {
        &self.namespace
    }

    fn ast_typed_type(&self) -> &ASTTypedType {
        &self.ast_typed_type
    }

    fn ast_type(&self) -> &ASTType {
        &self.ast_type
    }
}

impl Display for ASTTypedTypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let gen_types = self
            .generic_types
            .values()
            .map(|it| format!("{it}"))
            .collect::<Vec<_>>()
            .join(",");
        f.write_str(&format!("type {}<{gen_types}>", self.name))
    }
}

pub struct ConvContext<'a> {
    module: &'a EnhancedASTModule,
    enums: LinkedHashMap<ASTType, ASTTypedType>,
    structs: LinkedHashMap<ASTType, ASTTypedType>,
    types: LinkedHashMap<ASTType, ASTTypedType>,
    enum_defs: Vec<ASTTypedEnumDef>,
    struct_defs: Vec<ASTTypedStructDef>,
    type_defs: Vec<ASTTypedTypeDef>,
    count: usize,
}

impl<'a> TypeDefProvider for ConvContext<'a> {
    fn enums(&self) -> &[ASTTypedEnumDef] {
        &self.enum_defs
    }

    fn structs(&self) -> &[ASTTypedStructDef] {
        &self.struct_defs
    }

    fn types(&self) -> &[ASTTypedTypeDef] {
        &self.type_defs
    }

    fn name(&self) -> String {
        "ConvContext".to_owned()
    }
}

impl<'a> ConvContext<'a> {
    pub fn new(module: &'a EnhancedASTModule) -> Self {
        Self {
            module,
            enums: LinkedHashMap::new(),
            structs: LinkedHashMap::new(),
            types: LinkedHashMap::new(),
            enum_defs: Vec::new(),
            struct_defs: Vec::new(),
            type_defs: Vec::new(),
            count: 0,
        }
    }

    pub fn add_enum(
        &mut self,
        namespace: &ASTNameSpace,
        enum_type: &ASTType,
        enum_def: &ASTEnumDef,
    ) -> ASTTypedType {
        debug_i!("add_enum {enum_type}");
        indent!();

        let result = match enum_type {
            ASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => {
                /*let enum_def = self
                   .module
                   .enums
                   .iter()
                   .find(|it| &it.name == name)
                   .unwrap();

                */

                let cloned_param_types = param_types.clone();
                let mut generic_to_type = ResolvedGenericTypes::new();
                for (i, p) in enum_def.type_parameters.iter().enumerate() {
                    generic_to_type.insert(p.clone(), cloned_param_types.get(i).unwrap().clone());
                }

                self.count += 1;
                let new_name = format!("{name}_{}", self.count);

                let enum_typed_type = ASTTypedType::Enum {
                    namespace: enum_def.namespace.clone(),
                    name: new_name.clone(),
                };

                let variants = enum_def
                    .variants
                    .iter()
                    .map(|it| {
                        enum_variant(
                            namespace,
                            self,
                            it,
                            &generic_to_type,
                            enum_type,
                            &enum_typed_type,
                            "",
                        )
                    })
                    .collect();

                /*
                   during enum variants generation we could generate the same enum type
                */
                if let Some(enum_typed_type) = self.get_enum(enum_type) {
                    dedent!();
                    return enum_typed_type;
                }

                self.enum_defs.push(ASTTypedEnumDef {
                    namespace: enum_def.namespace.clone(),
                    modifiers: enum_def.modifiers.clone(),
                    name: new_name,
                    variants,
                    ast_type: enum_type.clone(),
                    ast_typed_type: enum_typed_type.clone(),
                    index: enum_def.index.clone(),
                });

                self.enums
                    .insert(enum_type.clone(), enum_typed_type.clone());

                enum_typed_type
                // }
            }
            _ => {
                panic!()
            }
        };

        dedent!();
        result
    }

    pub fn get_enum(&self, enum_type: &ASTType) -> Option<ASTTypedType> {
        self.get_def_typed_type(enum_type, &self.enums)
    }

    pub fn add_struct(&mut self, struct_type: &ASTType, struct_def: &ASTStructDef) -> ASTTypedType {
        debug_i!("add_struct {struct_type}");
        indent!();
        let result = match struct_type {
            ASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => {
                let cloned_param_types = param_types.clone();
                let mut generic_to_type = ResolvedGenericTypes::new();
                for (i, p) in struct_def.type_parameters.iter().enumerate() {
                    generic_to_type.insert(
                        p.clone(),
                        cloned_param_types
                            .get(i)
                            .unwrap_or_else(|| {
                                panic!("Cannot find generic type {p} for struct {name}")
                            })
                            .clone(),
                    );
                }

                self.count += 1;
                let new_name = format!("{name}_{}", self.count);

                let struct_typed_type = ASTTypedType::Struct {
                    namespace: struct_def.namespace.clone(),
                    name: new_name.clone(),
                };

                self.structs
                    .insert(struct_type.clone(), struct_typed_type.clone());

                let properties = struct_def
                    .properties
                    .iter()
                    .map(|it| struct_property(&struct_def.namespace, self, it, &generic_to_type))
                    .collect();

                let new_struct_def = ASTTypedStructDef {
                    namespace: struct_def.namespace.clone(),
                    modifiers: struct_def.modifiers.clone(),
                    name: new_name,
                    properties,
                    ast_type: struct_type.clone(),
                    ast_typed_type: struct_typed_type.clone(),
                    index: struct_def.index.clone(),
                };

                self.struct_defs.push(new_struct_def);

                struct_typed_type
            }
            _ => {
                panic!()
            }
        };

        dedent!();
        result
    }

    pub fn get_struct(&self, struct_type: &ASTType) -> Option<ASTTypedType> {
        self.get_def_typed_type(struct_type, &self.structs)
    }

    pub fn add_type(&mut self, ast_type: &ASTType, type_def: &ASTTypeDef) -> ASTTypedType {
        debug_i!("add_type {ast_type}");
        indent!();
        let result = match ast_type {
            ASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => {
                let cloned_param_types = param_types.clone();
                let mut generic_types = LinkedHashMap::new();
                for (i, p) in type_def.type_parameters.iter().enumerate() {
                    generic_types.insert(
                        p.clone(),
                        typed_type(
                            &type_def.namespace,
                            self,
                            cloned_param_types.get(i).unwrap_or_else(|| {
                                panic!("Cannot find generic type {p} for type {name}")
                            }),
                            "",
                        ),
                    );
                }

                self.count += 1;
                let new_name = format!("{name}_{}", self.count);

                let type_typed_type = ASTTypedType::Type {
                    namespace: type_def.namespace.clone(),
                    name: new_name.clone(),
                    native_type: type_def.native_type.clone(),
                    is_ref: type_def.is_ref,
                };

                self.type_defs.push(ASTTypedTypeDef {
                    namespace: type_def.namespace.clone(),
                    original_name: name.clone(),
                    name: new_name,
                    generic_types,
                    is_ref: type_def.is_ref,
                    ast_type: ast_type.clone(),
                    ast_typed_type: type_typed_type.clone(),
                    index: type_def.index.clone(),
                    modifiers: type_def.modifiers.clone(),
                    native_type: type_def.native_type.clone(),
                });

                self.types.insert(ast_type.clone(), type_typed_type.clone());

                type_typed_type
            }
            _ => {
                panic!()
            }
        };

        dedent!();
        result
    }

    pub fn get_type(&self, type_def_type: &ASTType) -> Option<ASTTypedType> {
        self.get_def_typed_type(type_def_type, &self.types)
    }

    fn get_def_typed_type(
        &self,
        ast_type: &ASTType,
        ast_type_to_ast_typed_type_map: &LinkedHashMap<ASTType, ASTTypedType>,
    ) -> Option<ASTTypedType> {
        match ast_type {
            ASTType::Custom {
                namespace: _,
                name: _,
                param_types: _,
                index: _,
            } => ast_type_to_ast_typed_type_map
                .iter()
                .find(|(type_def_ast_type, _ast_typed_type)| {
                    TypeFilter::Exact(type_def_ast_type.clone().clone())
                        .almost_equal(ast_type, self.module)
                        .unwrap_or(false)
                })
                .map(|(_ast_type, ast_typed_type)| ast_typed_type)
                .cloned(),
            _ => {
                panic!()
            }
        }
    }
}

pub fn convert_to_typed_module(
    original_module: &EnhancedASTModule,
    print_module: bool,
    mandatory_functions: Vec<DefaultFunction>,
    statics: &mut Statics,
    default_functions: Vec<DefaultFunction>,
    target: &CompileTarget,
    debug: bool,
) -> Result<ASTTypedModule, CompilationError> {
    let type_check = TypeCheck::new(&original_module.body_namespace, true);

    let module = type_check.type_check(
        original_module,
        statics,
        default_functions,
        mandatory_functions,
        target,
        debug,
    )?;

    let mut conv_context = ConvContext::new(&module);

    let body = module
        .body
        .iter()
        .map(|it| statement(&mut conv_context, it))
        .collect();

    let mut functions_by_name = LinkedHashMap::new();

    // TODO enable?
    // module.check_duplicate_functions();

    for new_function_def in module.functions().into_iter() {
        if functions_by_name.contains_key(&new_function_def.name) {
            continue;
        }

        let converted_function = new_function_def.clone();

        functions_by_name.insert(
            new_function_def.name.clone(),
            function_def(&mut conv_context, &converted_function, &module, statics).map_err(
                |it| {
                    compilation_error(
                        converted_function.index.clone(),
                        format!("Error converting {converted_function}"),
                        vec![it],
                    )
                },
            )?,
        );
    }

    target.typed_functions_creator(debug).create(
        &original_module,
        &conv_context,
        &mut functions_by_name,
        statics,
    );

    let evaluator = target.get_evaluator(debug);

    for (_name, function) in functions_by_name.iter_mut() {
        match &function.body {
            ASTTypedFunctionBody::RASMBody(_) => {}
            ASTTypedFunctionBody::NativeBody(body) => {
                let new_body = evaluator
                    .translate(statics, Some(function), None, body, true, &conv_context)
                    .map_err(|it| {
                        compilation_error(
                            function.index.clone(),
                            format!("{} converting body of {}", it, function),
                            Vec::new(),
                        )
                    })?;

                let mut lines: Vec<String> =
                    new_body.lines().map(|it| it.to_owned()).collect::<Vec<_>>();

                let mut val_context = ValContext::new(None);
                for par in function.parameters.iter() {
                    val_context
                        .insert_par(
                            par.name.clone(),
                            ASTParameterDef::new(
                                &par.name,
                                conv_context
                                    .get_type_from_typed_type(&par.ast_type)
                                    .unwrap_or_else(|| {
                                        panic!("Cannot get type from typed type {}", &par.ast_type)
                                    }),
                                par.ast_index.clone(),
                            ),
                        )
                        .map_err(|e| {
                            let tce =
                                TypeCheckError::new(par.ast_index.clone(), e.clone(), Vec::new());
                            CompilationError {
                                index: par.ast_index.clone(),
                                error_kind: CompilationErrorKind::TypeCheck(e.clone(), vec![tce]),
                            }
                        })?;
                }

                target
                    .called_functions(
                        Some(function),
                        None,
                        &new_body,
                        &val_context,
                        &conv_context,
                        statics,
                        debug,
                    )
                    .map_err(|err| CompilationError {
                        index: function.index.clone(),
                        error_kind: CompilationErrorKind::Generic(err.clone()),
                    })?
                    .iter()
                    .for_each(|(m, it)| {
                        debug_i!("native call to {:?}, in {}", it, function);

                        let filters = it
                            .param_types
                            .iter()
                            .map(|it| TypeFilter::Exact(it.clone()))
                            .collect::<Vec<_>>();
                        if let Some(new_function_def) = module
                            .functions_by_name
                            .find_call(
                                &it.name,
                                &it.name,
                                filters,
                                None,
                                false,
                                &it.index(&function.index),
                                &module,
                            )
                            .unwrap()
                        {
                            debug_i!("converted to {new_function_def}");
                            if it.name != new_function_def.name {
                                lines[it.i] = get_new_native_call(m, &new_function_def.name);
                            }
                        } else {
                            // panic!("cannot find call {function_call}");
                            // TODO I hope it is a predefined function like addRef or deref for a tstruct or enum
                            debug_i!("convert_to_typed_module: cannot find call to {}", it.name);
                        }
                    });

                let new_body = lines.join("\n");

                let new_body = evaluator
                    .translate(
                        statics,
                        Some(function),
                        None,
                        &new_body,
                        false,
                        &conv_context,
                    )
                    .map_err(|it| {
                        compilation_error(
                            function.index.clone(),
                            format!("{} converting body of {}", it, function),
                            Vec::new(),
                        )
                    })?;
                function.body = ASTTypedFunctionBody::NativeBody(new_body);
            }
        }
    }

    let result = ASTTypedModule {
        body,
        structs: conv_context.struct_defs,
        enums: conv_context.enum_defs,
        functions_by_name,
        types: conv_context.type_defs,
    };

    if print_module {
        print_typed_module(&result);
    }

    info!("verify");

    verify::verify(&result, statics)?;

    info!("verify end");

    Ok(result)
}

fn compilation_error(
    index: ASTIndex,
    message: String,
    errors: Vec<TypeCheckError>,
) -> CompilationError {
    CompilationError {
        index,
        error_kind: CompilationErrorKind::TypeCheck(message, errors),
    }
}

pub fn get_type_of_typed_expression(
    module: &ASTTypedModule,
    context: &TypedValContext,
    expr: &ASTTypedExpression,
    ast_type: Option<&ASTTypedType>,
    statics: &mut Statics,
) -> Result<ASTTypedType, CompilationError> {
    debug_i!("get_type_of_typed_expression {expr} {:?}", ast_type);
    indent!();
    let result = match expr {
        ASTTypedExpression::StringLiteral(_) => ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
        ASTTypedExpression::ASTFunctionCallExpression(call) => {
            debug_i!("function call expression");

            if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
                debug_i!("found function in module");
                function_def.return_type.clone()
            } else if let Some(function_def) = module
                .functions_by_name
                .get(&call.function_name.replace("::", "_"))
            {
                debug_i!("found function in module");
                function_def.return_type.clone()
            } else if let Some(TypedValKind::ParameterRef(_, par)) =
                context.get(&call.function_name)
            {
                debug_i!("found function in context");

                if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                    parameters: _,
                    return_type,
                }) = &par.ast_type
                {
                    return_type.as_ref().clone()
                } else {
                    dedent!();
                    return Err(verify::verify_error(
                        call.index.clone(),
                        format!("{} is not a lambda", call.function_name),
                    ));
                }
            } else if let Some(TypedValKind::LetRef(_, t)) = context.get(&call.function_name) {
                debug_i!("found function in context");

                if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                    parameters: _,
                    return_type,
                }) = &t
                {
                    return_type.as_ref().clone()
                } else {
                    dedent!();
                    return Err(verify::verify_error(
                        call.index.clone(),
                        format!("{} is not a lambda", call.function_name),
                    ));
                }
            } else {
                dedent!();
                return Err(verify::verify_error(
                    call.index.clone(),
                    format!("Cannot find function {}", &call.function_name),
                ));
            }
        }
        ASTTypedExpression::ValueRef(name, index) => {
            if let Some(TypedValKind::ParameterRef(_, par)) = context.get(name) {
                par.ast_type.clone()
            } else if let Some(TypedValKind::LetRef(_, ast_type)) = context.get(name) {
                ast_type.clone()
            } else if let Some(entry) = statics.get_typed_const(name) {
                entry.ast_typed_type.clone()
            } else {
                dedent!();
                return Err(verify::verify_error(
                    index.clone(),
                    format!("Unknown val {name}"),
                ));
            }
        }
        ASTTypedExpression::Value(val_type, _) => val_type.to_typed_type(),
        ASTTypedExpression::Lambda(lambda_def) => {
            let mut context = TypedValContext::new(Some(context));

            let (parameters, return_type) = match ast_type {
                None => {
                    dedent!();
                    return Err(verify::verify_error(
                        lambda_def.index.clone(),
                        "Error in lambda".to_string(),
                    ));
                }
                Some(t) => match t {
                    ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                        parameters,
                        return_type,
                    }) => (parameters, return_type),
                    _ => {
                        dedent!();
                        return Err(verify::verify_error(
                            lambda_def.index.clone(),
                            "Not a lambda".to_string(),
                        ));
                    }
                },
            };

            for (i, (name, index)) in lambda_def.parameter_names.iter().enumerate() {
                let parameter_def = ASTTypedParameterDef {
                    name: name.clone(),
                    ast_type: parameters.get(i).unwrap().clone(),
                    ast_index: index.clone(),
                };
                context.insert_par(name.clone(), i, parameter_def);
            }

            for statement in lambda_def.body.iter() {
                match statement {
                    ASTTypedStatement::Expression(_) => {}
                    ASTTypedStatement::LetStatement(name, let_expr, is_const, _index) => {
                        let type_of_expr = get_type_of_typed_expression(
                            module, &context, let_expr, None, statics,
                        )?;

                        if *is_const {
                            return Err(verify::verify_error(
                                lambda_def.index.clone(),
                                "Const not allowed here".to_string(),
                            ));
                        }

                        context.insert_let(name.to_string(), type_of_expr, None);
                    }
                }
            }

            let real_return_type = if let Some(last) = lambda_def.body.iter().last() {
                match last {
                    ASTTypedStatement::Expression(e) => {
                        get_type_of_typed_expression(module, &context, e, ast_type, statics)?
                    }
                    ASTTypedStatement::LetStatement(_, _expr, _is_const, _let_index) => {
                        ASTTypedType::Unit
                    }
                }
            } else {
                ASTTypedType::Unit
            };

            //if return_type.deref() != &ASTTypedType::Unit {
            /*
            println!(
                "expected return type {}, got {real_return_type} in {lambda_def}",
                return_type
            );

             */
            if return_type.deref() != &real_return_type {
                dedent!();
                return Err(verify::verify_error(
                    expr.get_index().unwrap(),
                    format!("Expected {return_type} but got {real_return_type}"),
                ));
            }

            ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: parameters.clone(),
                return_type: Box::new(real_return_type),
            })
        }
    };

    dedent!();
    Ok(result)
}

fn struct_property(
    namespace: &ASTNameSpace,
    conv_context: &mut ConvContext,
    property: &ASTStructPropertyDef,
    generic_to_type: &ResolvedGenericTypes,
) -> ASTTypedStructPropertyDef {
    if let Some(new_type) = substitute(&property.ast_type, generic_to_type) {
        ASTTypedStructPropertyDef {
            name: property.name.clone(),
            ast_type: typed_type(namespace, conv_context, &new_type, ""),
        }
    } else {
        ASTTypedStructPropertyDef {
            name: property.name.clone(),
            ast_type: typed_type(namespace, conv_context, &property.ast_type, ""),
        }
    }
}

pub fn function_def(
    conv_context: &mut ConvContext,
    def: &ASTFunctionDef,
    _module: &EnhancedASTModule,
    _statics: &mut Statics,
) -> Result<ASTTypedFunctionDef, TypeCheckError> {
    if !def.generic_types.is_empty() {
        panic!("function def has generics: {def}");
    }

    let mut generic_types = LinkedHashMap::new();

    for (name, ast_type) in def.resolved_generic_types.iter() {
        let typed_type = typed_type(&ast_type.namespace(), conv_context, ast_type, "");
        generic_types.insert(name.into(), typed_type);
    }

    let function_return_type = typed_type(
        &def.return_type.namespace(),
        conv_context,
        &def.return_type,
        &format!("function {} return type", def.name),
    );

    let typed_function_def = ASTTypedFunctionDef {
        namespace: def.namespace.clone(),
        name: def.name.clone(),
        original_name: def.original_name.clone(),
        body: body(conv_context, &def.body),
        return_type: function_return_type,
        inline: def.inline,
        parameters: def
            .parameters
            .iter()
            .map(|it| {
                parameter_def(
                    &it.ast_type.namespace(),
                    conv_context,
                    it,
                    &format!("function {}", def.name),
                )
            })
            .collect(),
        generic_types: generic_types.clone(),
        index: def.index.clone(),
    };

    /*
    let mut call_stack = CallStack::new();

    match &typed_function_def.body {
        ASTTypedFunctionBody::RASMBody(_) => {}
        ASTTypedFunctionBody::NativeBody(body) => {
            let mut val_context = ValContext::new(None);

            for par in typed_function_def.parameters.iter() {
                val_context.insert_par(
                    par.name.clone(),
                    ASTParameterDef {
                        name: par.name.clone(),
                        ast_type: type_to_untyped_type(&par.ast_type),
                        ast_index: par.ast_index.clone(),
                    },
                );
            }

            let mut evaluator = TextMacroEvaluator::new();

            let mut new_body = evaluator.translate(
                backend,
                statics,
                Some(&typed_function_def),
                None,
                body,
                dereference,
                true,
                conv_context,
            );

            let mut lines = new_body.lines().map(|it| it.to_owned()).collect::<Vec<_>>();

            for (m, it) in backend
                .called_functions(
                    Some(&typed_function_def),
                    None,
                    &new_body,
                    &val_context,
                    conv_context,
                )
                .iter()
            {
                debug_i!(
                    "native call to {:?}, in {}, generic types {:?}",
                    it,
                    typed_function_def.name,
                    &generic_types
                );

                let call_parameters_types = it
                    .param_types
                    .iter()
                    .map(|it| {
                        if let Some(subst) = substitute(it, &def.resolved_generic_types) {
                            Exact(subst)
                        } else {
                            Exact(it.clone())
                        }
                    })
                    .collect::<Vec<_>>();

                let function_def_name_opt = {
                    if let Some(f) = module.find_function(&it.name) {
                        Some(f.name.clone())
                    } else {
                        module
                            .functions_by_name
                            .find_call(
                                &it.name,
                                &it.name,
                                call_parameters_types.clone(),
                                None,
                                true,
                                &it.index(&typed_function_def.index),
                            )?
                            .map(|it| it.name.clone())
                    }
                };

                let function_name = if let Some(function_name) = function_def_name_opt {
                    function_name
                    //     TODO when SomethingConverted?
                } else {
                    let function_call = it.to_call(def);

                    panic!(
                        "cannot find {} {}: {}",
                        function_call,
                        SliceDisplay(&call_parameters_types),
                        function_call.index
                    );
                };

                debug_i!("found function for native call {function_name} ");

                if it.name != function_name {
                    println!("substituted native call");
                    lines[it.i] = get_new_native_call(m, &function_name);
                }
            }

            new_body = lines.join("\n");

            if body != &new_body {
                typed_function_def.body = ASTTypedFunctionBody::NativeBody(new_body);
            }
        }


    }

     */

    Ok(typed_function_def)
}

pub fn type_to_untyped_type(t: &ASTTypedType) -> ASTType {
    match t {
        ASTTypedType::Builtin(kind) => match kind {
            BuiltinTypedTypeKind::String => ASTType::Builtin(BuiltinTypeKind::String),
            BuiltinTypedTypeKind::I32 => ASTType::Builtin(BuiltinTypeKind::I32),
            BuiltinTypedTypeKind::Bool => ASTType::Builtin(BuiltinTypeKind::Bool),
            BuiltinTypedTypeKind::Char => ASTType::Builtin(BuiltinTypeKind::Char),
            BuiltinTypedTypeKind::F32 => ASTType::Builtin(BuiltinTypeKind::F32),
            BuiltinTypedTypeKind::Lambda {
                parameters,
                return_type,
            } => ASTType::Builtin(BuiltinTypeKind::Lambda {
                parameters: parameters.iter().map(type_to_untyped_type).collect(),
                return_type: if return_type.deref().is_unit() {
                    Box::new(ASTType::Unit)
                } else {
                    Box::new(type_to_untyped_type(return_type.deref()))
                },
            }),
        },
        ASTTypedType::Enum { namespace, name } => ASTType::Custom {
            namespace: namespace.clone(),
            name: name.into(),
            param_types: Vec::new(),
            index: ASTIndex::none(),
        },
        ASTTypedType::Struct { namespace, name } => ASTType::Custom {
            namespace: namespace.clone(),
            name: name.into(),
            param_types: Vec::new(),
            index: ASTIndex::none(),
        },
        ASTTypedType::Type {
            namespace,
            name,
            native_type: _,
            is_ref: _,
        } => ASTType::Custom {
            namespace: namespace.clone(),
            name: name.into(),
            param_types: Vec::new(),
            index: ASTIndex::none(),
        },
        ASTTypedType::Unit => ASTType::Unit,
    }
}

fn expression(conv_context: &mut ConvContext, expression: &ASTExpression) -> ASTTypedExpression {
    match expression {
        ASTExpression::StringLiteral(s, _) => ASTTypedExpression::StringLiteral(s.to_string()),
        ASTExpression::ASTFunctionCallExpression(fc) => {
            ASTTypedExpression::ASTFunctionCallExpression(function_call(conv_context, fc))
        }
        ASTExpression::ValueRef(v, index) => ASTTypedExpression::ValueRef(v.clone(), index.clone()),
        ASTExpression::Value(val_type, index) => {
            ASTTypedExpression::Value(val_type.clone(), index.clone())
        }
        ASTExpression::Lambda(l) => ASTTypedExpression::Lambda(lambda_def(conv_context, l)),
        ASTExpression::Any(_ast_type) => {
            panic!("cannot handle Any type");
        }
    }
}

fn lambda_def(conv_context: &mut ConvContext, lambda_def: &ASTLambdaDef) -> ASTTypedLambdaDef {
    ASTTypedLambdaDef {
        parameter_names: lambda_def.parameter_names.clone(),
        body: lambda_def
            .body
            .iter()
            .map(|it| statement(conv_context, it))
            .collect(),
        index: lambda_def.index.clone(),
    }
}

fn body(conv_context: &mut ConvContext, body: &ASTFunctionBody) -> ASTTypedFunctionBody {
    match body {
        RASMBody(body) => ASTTypedFunctionBody::RASMBody(
            body.iter().map(|it| statement(conv_context, it)).collect(),
        ),
        NativeBody(body) => ASTTypedFunctionBody::NativeBody(body.clone()),
    }
}

fn statement(conv_context: &mut ConvContext, it: &ASTStatement) -> ASTTypedStatement {
    match it {
        ASTStatement::Expression(e) => ASTTypedStatement::Expression(expression(conv_context, e)),
        ASTStatement::LetStatement(name, e, is_const, let_index) => {
            ASTTypedStatement::LetStatement(
                name.clone(),
                expression(conv_context, e),
                *is_const,
                let_index.clone(),
            )
        }
    }
}

fn enum_variant(
    namespace: &ASTNameSpace,
    conv_context: &mut ConvContext,
    variant: &ASTEnumVariantDef,
    generic_to_type: &ResolvedGenericTypes,
    enum_type: &ASTType,
    enum_typed_type: &ASTTypedType,
    message: &str,
) -> ASTTypedEnumVariantDef {
    debug_i!(
        "variant {variant}, enum_type {enum_type}, enum_typed_type {:?}, {:?}",
        enum_typed_type,
        generic_to_type
    );
    indent!();
    let result = ASTTypedEnumVariantDef {
        name: variant.name.clone(),
        parameters: variant
            .parameters
            .iter()
            .map(|it| {
                debug_i!("param {it} {enum_type}");
                if &it.ast_type == enum_type {
                    ASTTypedParameterDef {
                        name: it.name.clone(),
                        ast_type: enum_typed_type.clone(),
                        ast_index: it.ast_index.clone(),
                    }
                } else if let Some(new_type) = substitute(&it.ast_type, generic_to_type) {
                    debug_i!("new_type {new_type}");

                    if &new_type == enum_type {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            ast_type: enum_typed_type.clone(),
                            ast_index: it.ast_index.clone(),
                        }
                    } else {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            ast_type: typed_type(namespace, conv_context, &new_type, ""),
                            ast_index: it.ast_index.clone(),
                        }
                    }
                } else {
                    parameter_def(
                        namespace,
                        conv_context,
                        it,
                        &format!("{message}, variant {}", variant.name),
                    )
                }
            })
            .collect(),
    };
    dedent!();

    result
}

fn function_call(
    conv_context: &mut ConvContext,
    function_call: &ASTFunctionCall,
) -> ASTTypedFunctionCall {
    ASTTypedFunctionCall {
        namespace: function_call.namespace.clone(),
        function_name: function_call.function_name.clone(),
        original_function_name: function_call.original_function_name.clone(),
        parameters: function_call
            .parameters
            .iter()
            .map(|it| expression(conv_context, it))
            .collect(),
        index: function_call.index.clone(),
    }
}

fn parameter_def(
    namespace: &ASTNameSpace,
    conv_context: &mut ConvContext,
    parameter_def: &ASTParameterDef,
    message: &str,
) -> ASTTypedParameterDef {
    ASTTypedParameterDef {
        name: parameter_def.name.clone(),
        ast_type: typed_type(
            namespace,
            conv_context,
            &parameter_def.ast_type,
            &format!("{message}: parameter {}", parameter_def.name),
        ),
        ast_index: parameter_def.ast_index.clone(),
    }
}

fn typed_type(
    namespace: &ASTNameSpace,
    conv_context: &mut ConvContext,
    ast_type: &ASTType,
    message: &str,
) -> ASTTypedType {
    let result = match ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::String => ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
            BuiltinTypeKind::I32 => ASTTypedType::Builtin(BuiltinTypedTypeKind::I32),
            BuiltinTypeKind::Bool => ASTTypedType::Builtin(BuiltinTypedTypeKind::Bool),
            BuiltinTypeKind::Char => ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
            BuiltinTypeKind::F32 => ASTTypedType::Builtin(BuiltinTypedTypeKind::F32),
            BuiltinTypeKind::Lambda {
                return_type,
                parameters,
            } => ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: parameters
                    .iter()
                    .map(|it| {
                        typed_type(
                            &it.namespace(),
                            conv_context,
                            it,
                            &(message.to_owned() + ", lambda parameter"),
                        )
                    })
                    .collect(),
                return_type: Box::new(typed_type(
                    &return_type.namespace(),
                    conv_context,
                    return_type,
                    &(message.to_owned() + ", lambda return type"),
                )),
            }),
        },
        ASTType::Generic(p) => {
            panic!("Unresolved generic type '{p}': {message}");
        }
        ASTType::Custom {
            namespace: _,
            name,
            param_types: _,
            index: _,
        } => {
            if let Some(enum_def) = conv_context.module.enums.iter().find(|it| {
                (it.modifiers.public || it.namespace == ast_type.namespace()) && &it.name == name
            }) {
                if let Some(e) = conv_context.get_enum(ast_type) {
                    e
                } else {
                    conv_context.add_enum(namespace, ast_type, enum_def)
                }
            } else if let Some(struct_def) = conv_context.module.structs.iter().find(|it| {
                &it.name == name && (it.modifiers.public || it.namespace == ast_type.namespace())
            }) {
                if let Some(e) = conv_context.get_struct(ast_type) {
                    e
                } else {
                    conv_context.add_struct(ast_type, struct_def)
                }
            } else if let Some(t) = conv_context.module.types.iter().find(|it| {
                (it.modifiers.public || it.namespace == ast_type.namespace()) && &it.name == name
            }) {
                if let Some(e) = conv_context.get_type(ast_type) {
                    e
                } else {
                    conv_context.add_type(ast_type, t)
                }
            } else {
                println!(
                    "{:?}",
                    conv_context
                        .module
                        .types
                        .iter()
                        .map(|it| format!("{}:{}", it.namespace, it.name))
                        .collect::<Vec<_>>()
                );
                panic!("Cannot find custom type {name} from namespace '{namespace}' {message}");
            }
        }
        ASTType::Unit => ASTTypedType::Unit,
    };

    /*
    if format!("{ast_type}").contains("TestModel") {
        println!("  result {result}");
    }

     */

    result
}

pub fn print_typed_module(module: &ASTTypedModule) {
    for enum_def in module.enums.iter() {
        println!("{enum_def}");
    }

    for struct_def in module.structs.iter() {
        println!("{struct_def}");
    }

    for type_def in module.types.iter() {
        println!("{type_def}");
    }

    module.body.iter().for_each(|call| {
        println!("{call}");
    });
    println!();
    module
        .functions_by_name
        .values()
        .for_each(print_function_def)
}

pub fn print_function_def(f: &ASTTypedFunctionDef) {
    match &f.body {
        ASTTypedFunctionBody::RASMBody(_) => print!("fn {}", f),
        ASTTypedFunctionBody::NativeBody(_) => print!("native {}", f),
    }
    match &f.body {
        ASTTypedFunctionBody::RASMBody(expressions) => {
            println!(" {{");
            expressions.iter().for_each(|call| {
                println!("  {}", call);
            });
            println!("}}");
        }
        ASTTypedFunctionBody::NativeBody(body) => println!(" {{\n{body}\n}}"),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefaultFunctionCall {
    pub name: String,
    pub param_types: Vec<ASTType>,
    pub i: usize,
}

impl Display for DefaultFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "{}({})",
            self.name,
            SliceDisplay(&self.param_types)
        ))
    }
}

impl DefaultFunctionCall {
    pub fn new(name: &str, param_types: Vec<ASTType>, i: usize) -> Self {
        Self {
            name: name.into(),
            param_types,
            i,
        }
    }

    pub fn index(&self, function_def_index: &ASTIndex) -> ASTIndex {
        let mut index = function_def_index.clone();
        index.row += self.i;
        index.column = 0;
        index
    }

    pub fn to_call(&self, function_def: &ASTFunctionDef) -> ASTFunctionCall {
        let mut call = DefaultFunction {
            name: self.name.clone(),
            param_types: self.param_types.clone(),
        }
        .to_call(&function_def.namespace.clone());
        call.index = self.index(&function_def.index);
        call
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefaultFunction {
    pub name: String,
    pub param_types: Vec<ASTType>,
}

impl DefaultFunction {
    pub fn new_0(name: &str) -> Self {
        Self {
            name: name.into(),
            param_types: vec![],
        }
    }

    pub fn new_1(name: &str, kind: BuiltinTypeKind) -> Self {
        Self {
            name: name.into(),
            param_types: vec![ASTType::Builtin(kind)],
        }
    }

    pub fn new_2(name: &str, kind1: BuiltinTypeKind, kind2: BuiltinTypeKind) -> Self {
        Self {
            name: name.into(),
            param_types: vec![ASTType::Builtin(kind1), ASTType::Builtin(kind2)],
        }
    }

    pub fn new_3(
        name: &str,
        kind1: BuiltinTypeKind,
        kind2: BuiltinTypeKind,
        kind3: BuiltinTypeKind,
    ) -> Self {
        Self {
            name: name.into(),
            param_types: vec![
                ASTType::Builtin(kind1),
                ASTType::Builtin(kind2),
                ASTType::Builtin(kind3),
            ],
        }
    }

    pub fn to_call(&self, namespace: &ASTNameSpace) -> ASTFunctionCall {
        ASTFunctionCall {
            namespace: namespace.clone(),
            function_name: self.name.clone(),
            original_function_name: self.name.clone(),
            parameters: self
                .param_types
                .iter()
                .map(|it| match it {
                    ASTType::Builtin(kind) => match kind {
                        BuiltinTypeKind::String => {
                            ASTExpression::StringLiteral("".into(), ASTIndex::none())
                        }
                        BuiltinTypeKind::I32 => {
                            ASTExpression::Value(ValueType::I32(0), ASTIndex::none())
                        }
                        BuiltinTypeKind::Bool => {
                            ASTExpression::Value(ValueType::Boolean(true), ASTIndex::none())
                        }
                        BuiltinTypeKind::Char => {
                            ASTExpression::Value(ValueType::Char("a".to_string()), ASTIndex::none())
                        }
                        BuiltinTypeKind::F32 => {
                            ASTExpression::Value(ValueType::F32(1.0), ASTIndex::none())
                        }
                        BuiltinTypeKind::Lambda {
                            parameters: _,
                            return_type: _,
                        } => ASTExpression::Any(it.clone()),
                    },
                    ASTType::Generic(_) => panic!(),
                    ASTType::Custom {
                        namespace: _,
                        name: _,
                        param_types: _,
                        index: _,
                    } => ASTExpression::Any(it.clone()),
                    ASTType::Unit => {
                        panic!("Parameters cannot have unit type");
                    }
                })
                .collect(),
            index: ASTIndex::none(),
            generics: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::enhanced_module::EnhancedASTModule;
    use crate::parser::ast::{
        ASTEnumDef, ASTIndex, ASTModifiers, ASTNameSpace, ASTStructDef, ASTType,
    };
    use crate::type_check::functions_container::FunctionsContainer;
    use crate::type_check::typed_ast::{typed_type, ASTTypedType, ConvContext};
    use linked_hash_map::LinkedHashMap;

    #[test]
    pub fn get_def_typed_type() {
        let first_namespace = ASTNameSpace::new("test".to_string(), "first".to_string());

        let second_namespace = ASTNameSpace::new("test".to_string(), "second".to_string());

        let module = enhanced_module();
        let sut = ConvContext::new(&module);

        let result_first_ast_type = result_ast_type(
            &first_namespace,
            &simple_custom_ast_type("TestModel", &first_namespace),
        );
        let result_first_typed_type = ASTTypedType::Enum {
            namespace: first_namespace.clone(),
            name: "Result_1".to_string(),
        };

        let result_second_ast_type = result_ast_type(
            &second_namespace,
            &simple_custom_ast_type("TestModel", &second_namespace),
        );
        let result_second_typed_type = ASTTypedType::Enum {
            namespace: second_namespace.clone(),
            name: "Result_2".to_string(),
        };

        let mut ast_type_to_ast_typed_type_map = LinkedHashMap::new();
        ast_type_to_ast_typed_type_map
            .insert(result_first_ast_type.clone(), result_first_typed_type);
        ast_type_to_ast_typed_type_map
            .insert(result_second_ast_type.clone(), result_second_typed_type);

        let result =
            sut.get_def_typed_type(&result_first_ast_type, &ast_type_to_ast_typed_type_map);

        assert_eq!(format!("{}", result.unwrap()), "Result_1");

        let result =
            sut.get_def_typed_type(&result_second_ast_type, &ast_type_to_ast_typed_type_map);

        assert_eq!(format!("{}", result.unwrap()), "Result_2");
    }

    #[test]
    fn test_typed_type() {
        let first_namespace = ASTNameSpace::new("test".to_string(), "first".to_string());

        let second_namespace = ASTNameSpace::new("test".to_string(), "second".to_string());

        let module = enhanced_module();
        let mut sut = ConvContext::new(&module);

        let _ = typed_type(
            &first_namespace,
            &mut sut,
            &simple_custom_ast_type("TestModel", &first_namespace),
            "",
        );

        let _ = typed_type(
            &second_namespace,
            &mut sut,
            &simple_custom_ast_type("TestModel", &second_namespace),
            "",
        );

        let _ = typed_type(
            &first_namespace,
            &mut sut,
            &result_ast_type(
                &first_namespace,
                &simple_custom_ast_type("TestModel", &first_namespace),
            ),
            "",
        );

        let _ = typed_type(
            &second_namespace,
            &mut sut,
            &result_ast_type(
                &second_namespace,
                &simple_custom_ast_type("TestModel", &second_namespace),
            ),
            "",
        );
    }

    fn result_ast_type(namespace: &ASTNameSpace, ast_type: &ASTType) -> ASTType {
        ASTType::Custom {
            namespace: namespace.clone(),
            name: "Result".to_string(),
            param_types: vec![ast_type.clone()],
            index: ASTIndex::none(),
        }
    }

    fn enhanced_module() -> EnhancedASTModule {
        let result_type_def = result_type_def();
        let first_namespace = ASTNameSpace::new("test".to_string(), "first".to_string());

        let second_namespace = ASTNameSpace::new("test".to_string(), "second".to_string());

        EnhancedASTModule {
            body: vec![],
            functions_by_name: FunctionsContainer::new(),
            enums: vec![result_type_def.clone()],
            structs: vec![
                simple_struct_def("TestModel", &first_namespace),
                simple_struct_def("TestModel", &second_namespace),
            ],
            types: vec![],
            body_namespace: ASTNameSpace::global(),
        }
    }

    fn result_type_def() -> ASTEnumDef {
        ASTEnumDef {
            namespace: ASTNameSpace::new("std".to_string(), "result".to_string()),
            name: "Result".to_string(),
            modifiers: ASTModifiers::public(),
            variants: vec![],
            type_parameters: vec!["T".to_string()],
            index: ASTIndex::none(),
        }
    }

    fn simple_custom_ast_type(name: &str, namespace: &ASTNameSpace) -> ASTType {
        ASTType::Custom {
            namespace: namespace.clone(),
            name: name.to_string(),
            param_types: vec![],
            index: ASTIndex::none(),
        }
    }

    fn simple_struct_def(name: &str, namespace: &ASTNameSpace) -> ASTStructDef {
        ASTStructDef {
            namespace: namespace.clone(),
            name: name.to_string(),
            type_parameters: vec![],
            properties: vec![],
            index: ASTIndex::none(),
            modifiers: ASTModifiers::private(),
        }
    }
}
