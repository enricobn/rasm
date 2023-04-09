use std::cell::RefCell;
use std::fmt::{Display, Formatter};

use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::codegen::backend::Backend;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::{TextMacroEvaluator, TypeDefProvider};
use crate::codegen::{TypedValContext, TypedValKind, ValContext};
use crate::parser::ast::ASTFunctionBody::{ASMBody, RASMBody};
use crate::parser::ast::{
    ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex,
    ASTLambdaDef, ASTParameterDef, ASTStatement, ASTStructPropertyDef, ASTType, BuiltinTypeKind,
};
use crate::parser::ValueType;
use crate::type_check::call_stack::CallStack;
use crate::type_check::ConvertCallResult::*;
use crate::type_check::{
    convert_call, convert_function_def, get_new_native_call, substitute, TypeConversionContext,
};
use crate::utils::find_one;
use crate::{debug_i, dedent, indent};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedFunctionDef {
    pub name: String,
    pub parameters: Vec<ASTTypedParameterDef>,
    pub return_type: Option<ASTTypedType>,
    pub body: ASTTypedFunctionBody,
    pub inline: bool,
    pub generic_types: LinkedHashMap<String, ASTTypedType>,
}

impl Display for ASTTypedFunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars: Vec<String> = self.parameters.iter().map(|it| format!("{}", it)).collect();
        f.write_str(&format!("{}({})", self.name, pars.join(",")))?;
        if let Some(rt) = &self.return_type {
            f.write_str(&format!(" -> {}", rt))?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedLambdaDef {
    pub parameter_names: Vec<(String, ASTIndex)>,
    pub body: Vec<ASTTypedStatement>,
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
    ASMBody(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinTypedTypeKind {
    String,
    I32,
    Bool,
    Char,
    F32,
    Lambda {
        parameters: Vec<ASTTypedType>,
        return_type: Option<Box<ASTTypedType>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ASTTypedType {
    Builtin(BuiltinTypedTypeKind),
    Enum { name: String },
    Struct { name: String },
    Type { name: String },
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

                    let formatted_return_type = if let Some(rt) = return_type {
                        format!("{}", *rt)
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
            ASTTypedType::Enum { name } => f.write_str(&name.to_string()),
            ASTTypedType::Struct { name } => f.write_str(&name.to_string()),
            ASTTypedType::Type { name } => f.write_str(&name.to_string()),
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
    pub function_name: String,
    pub parameters: Vec<ASTTypedExpression>,
    pub index: ASTIndex,
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
    Any(ASTTypedType),
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
            ASTTypedExpression::Any(ast_type) => f.write_str(&format!("Any({ast_type})")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedStatement {
    Expression(ASTTypedExpression),
    LetStatement(String, ASTTypedExpression, bool, ASTIndex),
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
    pub native_body: String,
    pub types: Vec<ASTTypedTypeDef>,
}

impl TypeDefProvider for ASTTypedModule {
    fn get_enum_def_by_name(&self, name: &str) -> Option<&ASTTypedEnumDef> {
        self.enums.iter().find(|it| it.name == name)
    }

    fn get_struct_def_by_name(&self, name: &str) -> Option<&ASTTypedStructDef> {
        self.structs.iter().find(|it| it.name == name)
    }

    fn get_type_def_by_name(&self, name: &str) -> Option<&ASTTypedTypeDef> {
        self.types.iter().find(|it| it.name == name)
    }

    fn get_enum_def_like_name(&self, name: &str) -> Option<&ASTTypedEnumDef> {
        find_one(self.enums.iter(), |it| it.name.starts_with(name))
    }

    fn get_struct_def_like_name(&self, name: &str) -> Option<&ASTTypedStructDef> {
        find_one(self.structs.iter(), |it| it.name.starts_with(name))
    }

    fn get_type_def_like_name(&self, name: &str) -> Option<&ASTTypedTypeDef> {
        find_one(self.types.iter(), |it| it.name == name)
    }

    fn get_type_from_typed_type(&self, typed_type_to_find: &ASTTypedType) -> Option<ASTType> {
        if let Some(e) = find_one(self.enums.iter(), |it| {
            &it.ast_typed_type == typed_type_to_find
        }) {
            Some(e.clone().ast_type)
        } else if let Some(s) = find_one(self.structs.iter(), |it| {
            &it.ast_typed_type == typed_type_to_find
        }) {
            Some(s.clone().ast_type)
        } else {
            find_one(self.types.iter(), |it| {
                &it.ast_typed_type == typed_type_to_find
            })
            .map(|t| t.clone().ast_type)
        }
    }

    fn get_ast_typed_type_from_type_name(&self, name: &str) -> Option<ASTTypedType> {
        if let Some(e) = find_one(self.enums.iter(), |it| {
            if let ASTType::Custom {
                name: ast_type_name,
                param_types,
            } = &it.ast_type
            {
                ast_type_name == name
            } else {
                panic!()
            }
        }) {
            Some(e.clone().ast_typed_type)
        } else if let Some(s) = find_one(self.structs.iter(), |it| {
            if let ASTType::Custom {
                name: ast_type_name,
                param_types,
            } = &it.ast_type
            {
                ast_type_name == name
            } else {
                panic!()
            }
        }) {
            Some(s.clone().ast_typed_type)
        } else {
            find_one(self.types.iter(), |it| {
                if let ASTType::Custom {
                    name: ast_type_name,
                    param_types: _,
                } = &it.ast_type
                {
                    ast_type_name == name
                } else {
                    panic!()
                }
            })
            .map(|t| t.clone().ast_typed_type)
        }
    }

    fn get_ast_typed_type_from_ast_type(&self, ast_type: &ASTType) -> Option<ASTTypedType> {
        if let Some(e) = find_one(self.enums.iter(), |it| &it.ast_type == ast_type) {
            Some(e.clone().ast_typed_type)
        } else if let Some(s) = find_one(self.structs.iter(), |it| &it.ast_type == ast_type) {
            Some(s.clone().ast_typed_type)
        } else {
            find_one(self.types.iter(), |it| &it.ast_type == ast_type)
                .map(|t| t.clone().ast_typed_type)
        }
    }

    fn get_typed_type_def_from_type_name(&self, type_to_find: &str) -> Option<ASTTypedTypeDef> {
        find_one(self.types.iter(), |it| match &it.ast_type {
            ASTType::Builtin(_) => false,
            ASTType::Generic(_) => false,
            ASTType::Custom {
                name,
                param_types: _,
            } => name == type_to_find,
        })
        .cloned()
    }

    fn name(&self) -> String {
        "ASTTypedModule".to_owned()
    }
}

impl ASTTypedModule {}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedEnumDef {
    pub name: String,
    pub variants: Vec<ASTTypedEnumVariantDef>,
    pub ast_type: ASTType,
    pub ast_typed_type: ASTTypedType,
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
    pub name: String,
    pub properties: Vec<ASTTypedStructPropertyDef>,
    pub ast_type: ASTType,
    pub ast_typed_type: ASTTypedType,
}

impl Display for ASTTypedStructDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .properties
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<_>>()
            .join(",");
        f.write_str(&format!("struct {}({pars})", self.name))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedTypeDef {
    pub original_name: String,
    pub name: String,
    pub generic_types: LinkedHashMap<String, ASTTypedType>,
    pub is_ref: bool,
    pub ast_type: ASTType,
    pub ast_typed_type: ASTTypedType,
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
    fn get_enum_def_by_name(&self, name: &str) -> Option<&ASTTypedEnumDef> {
        self.enum_defs.iter().find(|it| it.name == name)
    }

    fn get_struct_def_by_name(&self, name: &str) -> Option<&ASTTypedStructDef> {
        self.struct_defs.iter().find(|it| it.name == name)
    }

    fn get_type_def_by_name(&self, name: &str) -> Option<&ASTTypedTypeDef> {
        self.type_defs.iter().find(|it| it.name == name)
    }

    fn get_enum_def_like_name(&self, name: &str) -> Option<&ASTTypedEnumDef> {
        find_one(self.enum_defs.iter(), |it| it.name.starts_with(name))
    }

    fn get_struct_def_like_name(&self, name: &str) -> Option<&ASTTypedStructDef> {
        find_one(self.struct_defs.iter(), |it| it.name.starts_with(name))
    }

    fn get_type_def_like_name(&self, name: &str) -> Option<&ASTTypedTypeDef> {
        find_one(self.type_defs.iter(), |it| it.name.starts_with(name))
    }

    fn get_type_from_typed_type(&self, typed_type_to_find: &ASTTypedType) -> Option<ASTType> {
        if let Some((ast_type, _ast_typed_type)) = self
            .enums
            .iter()
            .find(|(_ast_type, ast_typed_type)| ast_typed_type == &typed_type_to_find)
        {
            Some(ast_type.clone())
        } else if let Some((ast_type, _ast_typed_type)) = self
            .structs
            .iter()
            .find(|(_ast_type, ast_typed_type)| ast_typed_type == &typed_type_to_find)
        {
            Some(ast_type.clone())
        } else if let Some((ast_type, _ast_typed_type)) = self
            .types
            .iter()
            .find(|(_ast_type, ast_typed_type)| ast_typed_type == &typed_type_to_find)
        {
            Some(ast_type.clone())
        } else {
            None
        }
    }

    fn get_ast_typed_type_from_type_name(&self, name: &str) -> Option<ASTTypedType> {
        self.type_defs
            .iter()
            .find(|it| it.original_name == name)
            .map(|it| it.ast_typed_type.clone())
    }

    fn get_ast_typed_type_from_ast_type(&self, ast_type: &ASTType) -> Option<ASTTypedType> {
        if let Some(e) = find_one(self.enum_defs.iter(), |it| &it.ast_type == ast_type) {
            Some(e.clone().ast_typed_type)
        } else if let Some(s) = find_one(self.struct_defs.iter(), |it| &it.ast_type == ast_type) {
            Some(s.clone().ast_typed_type)
        } else {
            find_one(self.type_defs.iter(), |it| &it.ast_type == ast_type)
                .map(|t| t.clone().ast_typed_type)
        }
    }

    fn get_typed_type_def_from_type_name(&self, type_to_find: &str) -> Option<ASTTypedTypeDef> {
        find_one(self.type_defs.iter(), |it| match &it.ast_type {
            ASTType::Builtin(_) => false,
            ASTType::Generic(_) => false,
            ASTType::Custom {
                name,
                param_types: _,
            } => name == type_to_find,
        })
        .cloned()
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

    pub fn add_enum(&mut self, enum_type: &ASTType) -> ASTTypedType {
        debug!("add_enum {enum_type}");
        self.count += 1;
        if self.count > 100 {
            // TODO why???
            panic!();
        }
        match enum_type {
            ASTType::Custom { name, param_types } => {
                let enum_def = self
                    .module
                    .enums
                    .iter()
                    .find(|it| &it.name == name)
                    .unwrap();

                let cloned_param_types = param_types.clone();
                let mut generic_to_type = LinkedHashMap::new();
                for (i, p) in enum_def.type_parameters.iter().enumerate() {
                    generic_to_type.insert(p.clone(), cloned_param_types.get(i).unwrap().clone());
                }

                let new_name = format!("{name}_{}", self.enums.len());
                let enum_typed_type = ASTTypedType::Enum {
                    name: new_name.clone(),
                };

                let variants = enum_def
                    .variants
                    .iter()
                    .map(|it| {
                        enum_variant(self, it, &generic_to_type, enum_type, &enum_typed_type, "")
                    })
                    .collect();

                if let Some(found) = self.enum_defs.iter().find(|it| it.variants == variants) {
                    self.enums
                        .values()
                        .find(|it| match it {
                            ASTTypedType::Enum { name } => name == &found.name,
                            _ => panic!(),
                        })
                        .unwrap()
                        .clone()
                } else {
                    self.enum_defs.push(ASTTypedEnumDef {
                        name: new_name,
                        variants,
                        ast_type: enum_type.clone(),
                        ast_typed_type: enum_typed_type.clone(),
                    });

                    self.enums
                        .insert(enum_type.clone(), enum_typed_type.clone());

                    enum_typed_type
                }
            }
            _ => {
                panic!()
            }
        }
    }

    pub fn get_enum(&self, enum_type: &ASTType) -> Option<ASTTypedType> {
        self.enums.get(enum_type).cloned()
    }

    pub fn add_struct(&mut self, struct_type: &ASTType) -> ASTTypedType {
        debug!("add_struct {struct_type}");
        self.count += 1;
        if self.count > 100 {
            // TODO why???
            panic!();
        }
        match struct_type {
            ASTType::Custom { name, param_types } => {
                let struct_def = self
                    .module
                    .structs
                    .iter()
                    .find(|it| &it.name == name)
                    .unwrap();

                let cloned_param_types = param_types.clone();
                let mut generic_to_type = LinkedHashMap::new();
                for (i, p) in struct_def.type_parameters.iter().enumerate() {
                    generic_to_type.insert(
                        p.clone(),
                        cloned_param_types
                            .get(i)
                            .unwrap_or_else(|| {
                                panic!("Cannot find parametric type {p} for struct {name}")
                            })
                            .clone(),
                    );
                }

                let new_name = format!("{name}_{}", self.structs.len());
                let struct_typed_type = ASTTypedType::Struct {
                    name: new_name.clone(),
                };

                self.structs
                    .insert(struct_type.clone(), struct_typed_type.clone());

                let properties = struct_def
                    .properties
                    .iter()
                    .map(|it| struct_property(self, it, &generic_to_type))
                    .collect();

                self.struct_defs.push(ASTTypedStructDef {
                    name: new_name,
                    properties,
                    ast_type: struct_type.clone(),
                    ast_typed_type: struct_typed_type.clone(),
                });

                struct_typed_type
            }
            _ => {
                panic!()
            }
        }
    }

    pub fn get_struct(&self, enum_type: &ASTType) -> Option<ASTTypedType> {
        self.structs.get(enum_type).cloned()
    }

    pub fn add_type(&mut self, ast_type: &ASTType, is_ref: bool) -> ASTTypedType {
        debug!("add_type {ast_type}");
        self.count += 1;
        if self.count > 100 {
            // TODO why???
            panic!();
        }
        match ast_type {
            ASTType::Custom { name, param_types } => {
                let type_def = self
                    .module
                    .types
                    .iter()
                    .find(|it| &it.name == name)
                    .unwrap();

                let cloned_param_types = param_types.clone();
                let mut generic_types = LinkedHashMap::new();
                for (i, p) in type_def.type_parameters.iter().enumerate() {
                    generic_types.insert(
                        p.clone(),
                        typed_type(
                            self,
                            cloned_param_types.get(i).unwrap_or_else(|| {
                                panic!("Cannot find parametric type {p} for type {name}")
                            }),
                            "",
                        ),
                    );
                }

                let new_name = format!("{name}_{}", self.types.len());
                let type_typed_type = ASTTypedType::Type {
                    name: new_name.clone(),
                };

                self.type_defs.push(ASTTypedTypeDef {
                    original_name: name.clone(),
                    name: new_name,
                    generic_types,
                    is_ref,
                    ast_type: ast_type.clone(),
                    ast_typed_type: type_typed_type.clone(),
                });

                self.types.insert(ast_type.clone(), type_typed_type.clone());

                type_typed_type
            }
            _ => {
                panic!()
            }
        }
    }

    pub fn get_type(&self, type_def_type: &ASTType) -> Option<ASTTypedType> {
        self.types.get(type_def_type).cloned()
    }
}

pub fn convert_to_typed_module(
    module: &EnhancedASTModule,
    new_body: Vec<ASTStatement>,
    typed_context: &RefCell<TypeConversionContext>,
    debug_asm: bool,
    print_allocation: bool,
    printl_module: bool,
    mandatory_functions: Vec<DefaultFunctionCall>,
    backend: &dyn Backend,
    statics: &mut Statics,
    dereference: bool,
) -> ASTTypedModule {
    let mut conv_context = ConvContext::new(module);

    mandatory_functions
        .into_iter()
        .for_each(|it| add_default_function(module, it, true, typed_context, backend, statics));

    let default_functions = &mut vec![
        DefaultFunctionCall::new_2("malloc", BuiltinTypeKind::I32, BuiltinTypeKind::String),
        DefaultFunctionCall::new_1("exitMain", BuiltinTypeKind::I32),
        DefaultFunctionCall::new_2("addRef", BuiltinTypeKind::I32, BuiltinTypeKind::String),
        DefaultFunctionCall::new_3(
            "memcopy",
            BuiltinTypeKind::I32,
            BuiltinTypeKind::I32,
            BuiltinTypeKind::I32,
        ),
        DefaultFunctionCall::new_2("deref", BuiltinTypeKind::I32, BuiltinTypeKind::String),
        //DefaultFunctionCall::new_0("negativeCount"),
        //DefaultFunctionCall::new_0("invalidAddress"),
        //DefaultFunctionCall::new_1("removeFromReused", BuiltinTypeKind::I32),
        DefaultFunctionCall::new_1("addStaticStringToHeap", BuiltinTypeKind::I32),
        DefaultFunctionCall::new_2(
            "createCmdLineArguments",
            BuiltinTypeKind::I32,
            BuiltinTypeKind::I32,
        ),
        DefaultFunctionCall::new_1("str_addRef", BuiltinTypeKind::String),
        DefaultFunctionCall::new_1("str_deref", BuiltinTypeKind::String),
        //"str_addRef",
        //"str_deref",
        //"sysOpen",
        //"sysRead",
        //"sysClose",
        //"fileSize",
        //"freeMem",
    ];

    if print_allocation {
        default_functions.append(&mut vec![
            DefaultFunctionCall::new_0("printAllocated"),
            DefaultFunctionCall::new_0("printTableSlotsAllocated"),
            DefaultFunctionCall::new_0("printAllocatedString"),
            DefaultFunctionCall::new_0("printTableSlotsAllocatedString"),
            //"nprint",
        ])
    }

    if debug_asm {
        /*
        TODO
        default_functions.append(&mut vec![
            "startMalloc",
            "nprintln",
            "newAddress",
            "newAddressOk",
            "notAllocated",
            "allocate",
            "printTab",
            "startAddRef",
            "endAddRef",
            "deallocated",
            "startDeref",
            "endDeref",
            "reused",
            "endMalloc",
            "nprint",
            "printRefCount",
            "printReplacedReused",
            "addReused",
        ])
         */
    }

    default_functions.sort_by(|a, b| a.name.cmp(&b.name));
    //default_functions.dedup_by(|a, b| a.name == b.name);

    for it in default_functions {
        add_default_function(module, it.clone(), false, typed_context, backend, statics)
    }

    let mut new_typed_context = typed_context.clone();

    let mut functions_by_name = LinkedHashMap::new();

    let mut count = 0;

    loop {
        debug_i!("typed context loop {count}");

        if count > 100 {
            panic!()
        }

        indent!();

        new_typed_context.borrow().debug_i();

        let cloned_typed_context = new_typed_context.clone();

        functions_by_name.clear();

        let mut somethin_converted = false;

        for new_function_def in new_typed_context.borrow().functions().into_iter() {
            debug_i!("converting function {new_function_def}");
            let resolved_param_types = LinkedHashMap::new();
            let converted_function = if let Some(function_converted) = convert_function_def(
                backend,
                module,
                &cloned_typed_context,
                &resolved_param_types,
                new_function_def,
                statics,
            )
            .unwrap()
            {
                somethin_converted = true;
                function_converted
            } else {
                new_function_def.clone()
            };

            functions_by_name.insert(
                new_function_def.name.clone(),
                function_def(
                    &mut conv_context,
                    &converted_function,
                    backend,
                    module,
                    &cloned_typed_context,
                    statics,
                    dereference,
                ),
            );
        }

        if somethin_converted
            || new_typed_context.borrow().len() != cloned_typed_context.borrow().len()
        {
            debug_i!(
                "old vs new {} {}",
                new_typed_context.borrow().len(),
                cloned_typed_context.borrow().len()
            );

            new_typed_context = cloned_typed_context.clone();
            count += 1;
            dedent!();
            continue;
        } else {
            dedent!();
            break;
        }
    }

    let mut result = ASTTypedModule {
        body: new_body
            .iter()
            .map(|it| statement(&mut conv_context, it))
            .collect(),
        structs: conv_context.struct_defs,
        enums: conv_context.enum_defs,
        functions_by_name: LinkedHashMap::new(),
        native_body: module.native_body.clone(),
        types: conv_context.type_defs,
    };

    let mut evaluator = TextMacroEvaluator::new();

    functions_by_name
        .iter_mut()
        .for_each(|it| match &it.1.body {
            ASTTypedFunctionBody::RASMBody(_) => {}
            ASTTypedFunctionBody::ASMBody(body) => {
                let new_body = evaluator.translate(
                    backend,
                    statics,
                    Some(it.1),
                    body,
                    dereference,
                    true,
                    &result,
                );

                let new_body = evaluator.translate(
                    backend,
                    statics,
                    Some(it.1),
                    &new_body,
                    dereference,
                    false,
                    &result,
                );
                it.1.body = ASTTypedFunctionBody::ASMBody(new_body);
            }
        });

    result.functions_by_name = functions_by_name;

    if printl_module {
        print_typed_module(&result);
    }

    verify(&result, statics);

    result
}

fn verify(module: &ASTTypedModule, statics: &mut Statics) {
    let mut context = TypedValContext::new(None);

    for statement in module.body.iter() {
        verify_statement(module, &mut context, statement, statics);
    }

    for function_def in module.functions_by_name.values() {
        let mut context = TypedValContext::new(None);

        for (i, par) in function_def.parameters.iter().enumerate() {
            context.insert_par(par.name.clone(), i, par.clone());
        }

        if let ASTTypedFunctionBody::RASMBody(expressions) = &function_def.body {
            for statement in expressions.iter() {
                verify_statement(module, &mut context, statement, statics)
            }
        }
    }
}

fn verify_statement(
    module: &ASTTypedModule,
    context: &mut TypedValContext,
    statement: &ASTTypedStatement,
    statics: &mut Statics,
) {
    match statement {
        ASTTypedStatement::Expression(e) => {
            if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                verify_function_call(module, context, call, statics);
            }
        }
        ASTTypedStatement::LetStatement(name, e, is_const, _let_index) => {
            if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                let ast_typed_type =
                    if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
                        function_def.return_type.clone()
                    } else if let Some(function_def) = module
                        .functions_by_name
                        .get(&call.function_name.replace("::", "_"))
                    {
                        function_def.return_type.clone()
                    } else if let Some(TypedValKind::ParameterRef(_, parameter_ref)) =
                        context.get(&call.function_name)
                    {
                        if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                            parameters: _,
                            return_type,
                        }) = &parameter_ref.ast_type
                        {
                            return_type.clone().map(|it| *it)
                        } else {
                            panic!()
                        }
                    } else if let Some(TypedValKind::LetRef(_, ast_type)) =
                        context.get(&call.function_name)
                    {
                        if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                            parameters: _,
                            return_type,
                        }) = &ast_type
                        {
                            return_type.clone().map(|it| *it)
                        } else {
                            panic!()
                        }
                    } else {
                        panic!("{call}")
                    };

                if *is_const {
                    statics.add_typed_const(name.to_owned(), ast_typed_type.unwrap());
                } else {
                    context.insert_let(name.clone(), ast_typed_type.unwrap(), None);
                }
            } else if let Some(ast_typed_type) =
                get_type_of_typed_expression(module, context, e, None, statics)
            {
                if *is_const {
                    statics.add_typed_const(name.to_owned(), ast_typed_type);
                } else {
                    context.insert_let(name.clone(), ast_typed_type, None);
                }
            } else {
                panic!("unsupported let")
            }

            if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                verify_function_call(module, context, call, statics);
            }
        }
    }
}

fn verify_function_call(
    module: &ASTTypedModule,
    context: &TypedValContext,
    call: &ASTTypedFunctionCall,
    statics: &Statics,
) {
    debug!("verify_function_call {call}");

    let parameters_types =
        if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
            function_def
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect::<Vec<ASTTypedType>>()
        } else if let Some(function_def) = module
            .functions_by_name
            .get(&call.function_name.replace("::", "_"))
        {
            function_def
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect::<Vec<ASTTypedType>>()
        } else if let Some(TypedValKind::ParameterRef(_, parameter_ref)) =
            context.get(&call.function_name)
        {
            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters,
                return_type: _,
            }) = &parameter_ref.ast_type
            {
                parameters.to_vec()
            } else {
                panic!();
            }
        } else if let Some(TypedValKind::LetRef(_, ast_type)) = context.get(&call.function_name) {
            if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters,
                return_type: _,
            }) = &ast_type
            {
                parameters.to_vec()
            } else {
                panic!();
            }
        } else {
            panic!(
                "cannot find function for call {call} : {} functions: {:?}",
                call.index,
                module.functions_by_name.keys().collect::<Vec<_>>()
            );
        };

    for (i, expr) in call.parameters.iter().enumerate() {
        let par = parameters_types.get(i).unwrap().clone();
        debug!("par type {par}");
        assert_eq!(
            get_type_of_typed_expression(module, context, expr, Some(&par), statics).unwrap(),
            par,
            "expression {:?}",
            expr
        );
    }
}

pub fn get_type_of_typed_expression(
    module: &ASTTypedModule,
    context: &TypedValContext,
    expr: &ASTTypedExpression,
    ast_type: Option<&ASTTypedType>,
    statics: &Statics,
) -> Option<ASTTypedType> {
    debug!("get_type_of_typed_expression {expr} {:?}", ast_type);
    match expr {
        ASTTypedExpression::StringLiteral(_) => {
            Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::String))
        }
        ASTTypedExpression::ASTFunctionCallExpression(call) => {
            debug!("function call expression");

            if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
                debug!("found function in module");
                function_def.return_type.clone()
            } else if let Some(function_def) = module
                .functions_by_name
                .get(&call.function_name.replace("::", "_"))
            {
                debug!("found function in module");
                function_def.return_type.clone()
            } else if let Some(TypedValKind::ParameterRef(_, par)) =
                context.get(&call.function_name)
            {
                debug!("found function in context");

                if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                    parameters: _,
                    return_type,
                }) = &par.ast_type
                {
                    return_type.clone().map(|it| it.as_ref().clone())
                } else {
                    panic!("expected lambda, found: {}", &par.ast_type);
                }
            } else {
                panic!("Cannot find function {}", &call.function_name);
            }
        }
        ASTTypedExpression::ValueRef(name, _) => {
            if let Some(TypedValKind::ParameterRef(_, par)) = context.get(name) {
                Some(par.ast_type.clone())
            } else if let Some(TypedValKind::LetRef(_, ast_type)) = context.get(name) {
                Some(ast_type.clone())
            } else if let Some(entry) = statics.get_typed_const(name) {
                Some(entry.ast_typed_type.clone())
            } else {
                panic!(
                    "Unknown val {name} context: {:?} statics: {:?}",
                    context,
                    statics.typed_const_names()
                );
            }
        }
        ASTTypedExpression::Value(val_type, _) => match val_type {
            ValueType::Boolean(_) => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Bool)),
            ValueType::I32(_) => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::I32)),
            ValueType::Char(_) => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Char)),
            ValueType::F32(_) => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::F32)),
        },
        ASTTypedExpression::Lambda(lambda_def) => {
            let mut context = TypedValContext::new(Some(context));

            let (parameters, return_type) = match ast_type {
                None => {
                    panic!()
                }
                Some(t) => match t {
                    ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                        parameters,
                        return_type,
                    }) => (parameters, return_type),
                    _ => panic!(),
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
                    ASTTypedStatement::Expression(e) => {
                        if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                            verify_function_call(module, &context, call, statics);
                        }
                    }
                    ASTTypedStatement::LetStatement(name, e, is_const, _let_index) => {
                        if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                            let ast_typed_type = module
                                .functions_by_name
                                .get(&call.function_name.replace("::", "_"))
                                .unwrap()
                                .return_type
                                .clone()
                                .unwrap();
                            if *is_const {
                                panic!("const not allowed here")
                            } else {
                                context.insert_let(name.clone(), ast_typed_type, None);
                            }
                            verify_function_call(module, &context, call, statics);
                        } else if let Some(ast_typed_type) =
                            get_type_of_typed_expression(module, &context, e, None, statics)
                        {
                            if *is_const {
                                panic!("const not allowed here");
                            } else {
                                context.insert_let(name.clone(), ast_typed_type, None);
                            }
                        } else {
                            panic!("unsupported let")
                        }
                    }
                }
            }

            let real_return_type = if let Some(last) = lambda_def.body.iter().last() {
                match last {
                    ASTTypedStatement::Expression(e) => {
                        get_type_of_typed_expression(module, &context, e, None, statics)
                    }
                    ASTTypedStatement::LetStatement(_, e, _is_const, _let_index) => {
                        get_type_of_typed_expression(module, &context, e, None, statics)
                    }
                }
            } else {
                None
            };

            if let Some(t) = return_type {
                let rt = real_return_type
                    .clone()
                    .unwrap_or_else(|| panic!("expected {:?}, but got None", t));
                assert_eq!(t.as_ref(), &rt, "expression {:?}", expr)
            } else if real_return_type.is_some() {
                panic!()
            }

            Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: parameters.clone(),
                return_type: real_return_type.map(Box::new),
            }))
        }
        ASTTypedExpression::Any(ast_type) => Some(ast_type.clone()),
    }
}

fn add_default_function(
    module: &EnhancedASTModule,
    function_call: DefaultFunctionCall,
    mandatory: bool,
    typed_context: &RefCell<TypeConversionContext>,
    backend: &dyn Backend,
    statics: &Statics,
) {
    let context = ValContext::new(None);

    let call = function_call.to_call();

    match convert_call(
        module,
        &context,
        &call,
        typed_context,
        None,
        backend,
        &CallStack::new(),
        statics,
    ) {
        Err(e) => {
            panic!(
                "Error converting mandatory function {} : {e}",
                function_call.name
            );
        }
        Ok(NothingToConvert) => {
            debug_i!("no new call for default function {call}");
        }
        Ok(SomethingConverted) => {
            debug_i!("something converted, but not entirely for default function {call}");
        }
        Ok(Converted(new_call)) => {
            debug_i!("new call {new_call} for default function {call}");
        }
    }
}

fn struct_property(
    conv_context: &mut ConvContext,
    property: &ASTStructPropertyDef,
    generic_to_type: &LinkedHashMap<String, ASTType>,
) -> ASTTypedStructPropertyDef {
    if let Some(new_type) = substitute(&property.ast_type, generic_to_type) {
        ASTTypedStructPropertyDef {
            name: property.name.clone(),
            ast_type: typed_type(conv_context, &new_type, ""),
        }
    } else {
        ASTTypedStructPropertyDef {
            name: property.name.clone(),
            ast_type: typed_type(conv_context, &property.ast_type, ""),
        }
    }
}

pub fn function_def(
    conv_context: &mut ConvContext,
    def: &ASTFunctionDef,
    backend: &dyn Backend,
    module: &EnhancedASTModule,
    typed_context: &RefCell<TypeConversionContext>,
    statics: &mut Statics,
    dereference: bool,
) -> ASTTypedFunctionDef {
    if !def.generic_types.is_empty() {
        panic!("function def has generics: {def}");
    }

    let mut generic_types = LinkedHashMap::new();

    for (name, ast_type) in def.resolved_generic_types.iter() {
        let typed_type = typed_type(conv_context, ast_type, "");
        generic_types.insert(name.into(), typed_type);
    }

    let mut typed_function_def = ASTTypedFunctionDef {
        name: def.name.clone(),
        body: body(conv_context, &def.body),
        return_type: def.return_type.clone().map(|it| {
            typed_type(
                conv_context,
                &it,
                &format!("function {} return type", def.name),
            )
        }),
        inline: def.inline,
        parameters: def
            .parameters
            .iter()
            .map(|it| parameter_def(conv_context, it, &format!("function {}", def.name)))
            .collect(),
        generic_types: generic_types.clone(),
    };

    match &typed_function_def.body {
        ASTTypedFunctionBody::RASMBody(_) => {}
        ASTTypedFunctionBody::ASMBody(body) => {
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
                body,
                dereference,
                true,
                conv_context,
            );

            let mut lines = new_body.lines().map(|it| it.to_owned()).collect::<Vec<_>>();

            backend
                .called_functions(
                    Some(&typed_function_def),
                    &new_body,
                    &val_context,
                    conv_context,
                )
                .iter()
                .for_each(|(m, it)| {
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
                            substitute(it, &def.resolved_generic_types).or_else(|| Some(it.clone()))
                        })
                        .collect::<Vec<Option<ASTType>>>();

                    let cloned_typed_context = typed_context.clone().into_inner();

                    let function_def_opt = cloned_typed_context.find_call(
                        &it.name,
                        &it.name,
                        Some(call_parameters_types.clone()),
                        None,
                    );

                    let function_name = if let Some(functiond_def) = function_def_opt {
                        functiond_def.name.clone()
                        //     TODO when SomethingConverted?
                    } else {
                        let function_call = it.to_call();

                        if let Ok(Converted(new_call)) = convert_call(
                            module,
                            &ValContext::new(None),
                            &function_call,
                            typed_context,
                            None,
                            backend,
                            &CallStack::new(),
                            statics,
                        ) {
                            debug_i!("new_call {:?}", new_call);
                            new_call.function_name
                        } else {
                            let function_def_opt = module.find_call(
                                &it.name,
                                &it.name,
                                Some(call_parameters_types.clone()),
                                None,
                            );

                            if let Some(functiond_def) = function_def_opt {
                                if let Some(rf) = typed_context
                                    .borrow_mut()
                                    .try_add_new(&it.name, functiond_def)
                                {
                                    rf.name
                                } else {
                                    panic!("cannot find {}", function_call);
                                }
                            } else {
                                module.debug_i();
                                panic!(
                                    "cannot find {} {:?}: {}",
                                    function_call, call_parameters_types, function_call.index
                                );
                            }
                        }
                    };

                    debug_i!("found function for native call {function_name} ");

                    if it.name != function_name {
                        lines[it.i] = get_new_native_call(m, &function_name);
                    }
                });

            new_body = lines.join("\n");

            if body != &new_body {
                typed_function_def.body = ASTTypedFunctionBody::ASMBody(new_body);
            }
        }
    }

    typed_function_def
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
                parameters: parameters
                    .iter()
                    .map(|it| type_to_untyped_type(it))
                    .collect(),
                return_type: return_type
                    .clone()
                    .map(|it| Box::new(type_to_untyped_type(&it))),
            }),
        },
        ASTTypedType::Enum { name } => ASTType::Custom {
            name: name.into(),
            param_types: Vec::new(),
        },
        ASTTypedType::Struct { name } => ASTType::Custom {
            name: name.into(),
            param_types: Vec::new(),
        },
        ASTTypedType::Type { name } => ASTType::Custom {
            name: name.into(),
            param_types: Vec::new(),
        },
    }
}

fn expression(conv_context: &mut ConvContext, expression: &ASTExpression) -> ASTTypedExpression {
    match expression {
        ASTExpression::StringLiteral(s) => ASTTypedExpression::StringLiteral(s.to_string()),
        ASTExpression::ASTFunctionCallExpression(fc) => {
            ASTTypedExpression::ASTFunctionCallExpression(function_call(conv_context, fc))
        }
        ASTExpression::ValueRef(v, index) => ASTTypedExpression::ValueRef(v.clone(), index.clone()),
        ASTExpression::Value(val_type, index) => {
            ASTTypedExpression::Value(val_type.clone(), index.clone())
        }
        ASTExpression::Lambda(l) => ASTTypedExpression::Lambda(lambda_def(conv_context, l)),
        ASTExpression::Any(ast_type) => ASTTypedExpression::Any(typed_type(
            conv_context,
            ast_type,
            &format!("Any({ast_type})"),
        )),
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
    }
}

fn body(conv_context: &mut ConvContext, body: &ASTFunctionBody) -> ASTTypedFunctionBody {
    match body {
        RASMBody(body) => ASTTypedFunctionBody::RASMBody(
            body.iter().map(|it| statement(conv_context, it)).collect(),
        ),
        ASMBody(body) => ASTTypedFunctionBody::ASMBody(body.clone()),
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
    conv_context: &mut ConvContext,
    variant: &ASTEnumVariantDef,
    generic_to_type: &LinkedHashMap<String, ASTType>,
    enum_type: &ASTType,
    enum_typed_type: &ASTTypedType,
    message: &str,
) -> ASTTypedEnumVariantDef {
    debug!(
        "variant {variant}, enum_type {enum_type}, enum_typed_type {:?}, {:?}",
        enum_typed_type, generic_to_type
    );
    ASTTypedEnumVariantDef {
        name: variant.name.clone(),
        parameters: variant
            .parameters
            .iter()
            .map(|it| {
                debug!("param {it} {enum_type}");
                if &it.ast_type == enum_type {
                    ASTTypedParameterDef {
                        name: it.name.clone(),
                        ast_type: enum_typed_type.clone(),
                        ast_index: it.ast_index.clone(),
                    }
                } else if let Some(new_type) = substitute(&it.ast_type, generic_to_type) {
                    debug!("new_type {new_type}");

                    if &new_type == enum_type {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            ast_type: enum_typed_type.clone(),
                            ast_index: it.ast_index.clone(),
                        }
                    } else {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            ast_type: typed_type(conv_context, &new_type, ""),
                            ast_index: it.ast_index.clone(),
                        }
                    }
                } else {
                    parameter_def(
                        conv_context,
                        it,
                        &format!("{message}, variant {}", variant.name),
                    )
                }
            })
            .collect(),
    }
}

fn function_call(
    conv_context: &mut ConvContext,
    function_call: &ASTFunctionCall,
) -> ASTTypedFunctionCall {
    ASTTypedFunctionCall {
        function_name: function_call.function_name.clone(),
        parameters: function_call
            .parameters
            .iter()
            .map(|it| expression(conv_context, it))
            .collect(),
        index: function_call.index.clone(),
    }
}

fn parameter_def(
    conv_context: &mut ConvContext,
    parameter_def: &ASTParameterDef,
    message: &str,
) -> ASTTypedParameterDef {
    ASTTypedParameterDef {
        name: parameter_def.name.clone(),
        ast_type: typed_type(
            conv_context,
            &parameter_def.ast_type,
            &format!("{message}: parameter {}", parameter_def.name),
        ),
        ast_index: parameter_def.ast_index.clone(),
    }
}

fn typed_type(conv_context: &mut ConvContext, ast_type: &ASTType, message: &str) -> ASTTypedType {
    match ast_type {
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
                            conv_context,
                            it,
                            &(message.to_owned() + ", lambda parameter"),
                        )
                    })
                    .collect(),
                return_type: return_type.clone().map(|it| {
                    Box::new(typed_type(
                        conv_context,
                        &it,
                        &(message.to_owned() + ", lambda return type"),
                    ))
                }),
            }),
        },
        ASTType::Generic(p) => {
            panic!("Unresolved parametric type '{p}': {message}");
        }
        ASTType::Custom {
            name,
            param_types: _,
        } => {
            if conv_context.module.enums.iter().any(|it| &it.name == name) {
                if let Some(e) = conv_context.get_enum(ast_type) {
                    e
                } else {
                    conv_context.add_enum(ast_type)
                }
            } else if conv_context
                .module
                .structs
                .iter()
                .any(|it| &it.name == name)
            {
                if let Some(e) = conv_context.get_struct(ast_type) {
                    e
                } else {
                    conv_context.add_struct(ast_type)
                }
            } else if let Some(t) = conv_context.module.types.iter().find(|it| &it.name == name) {
                if let Some(e) = conv_context.get_type(ast_type) {
                    e
                } else {
                    conv_context.add_type(ast_type, t.is_ref)
                }
            } else {
                println!(
                    "{:?}",
                    conv_context
                        .types
                        .iter()
                        .map(|it| format!("{:?}", it))
                        .collect::<Vec<_>>()
                );
                panic!("Cannot find custom type {name}");
            }
        }
    }
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
        ASTTypedFunctionBody::ASMBody(_) => print!("asm {}", f),
    }
    match &f.body {
        ASTTypedFunctionBody::RASMBody(expressions) => {
            println!(" {{");
            expressions.iter().for_each(|call| {
                println!("  {}", call);
            });
            println!("}}");
        }
        ASTTypedFunctionBody::ASMBody(_) => println!(" {{...}}"),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefaultFunctionCall {
    pub name: String,
    pub param_types: Vec<ASTType>,
    pub i: usize,
}

impl DefaultFunctionCall {
    pub fn new(name: &str, param_types: Vec<ASTType>, i: usize) -> Self {
        Self {
            name: name.into(),
            param_types,
            i,
        }
    }

    pub fn new_0(name: &str) -> Self {
        Self {
            name: name.into(),
            param_types: vec![],
            i: 0,
        }
    }

    pub fn new_1(name: &str, kind: BuiltinTypeKind) -> Self {
        Self {
            name: name.into(),
            param_types: vec![ASTType::Builtin(kind)],
            i: 0,
        }
    }

    pub fn new_2(name: &str, kind1: BuiltinTypeKind, kind2: BuiltinTypeKind) -> Self {
        Self {
            name: name.into(),
            param_types: vec![ASTType::Builtin(kind1), ASTType::Builtin(kind2)],
            i: 0,
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
            i: 0,
        }
    }

    pub fn to_call(&self) -> ASTFunctionCall {
        ASTFunctionCall {
            function_name: self.name.clone(),
            original_function_name: self.name.clone(),
            parameters: self
                .param_types
                .iter()
                .map(|it| match it {
                    ASTType::Builtin(kind) => match kind {
                        BuiltinTypeKind::String => ASTExpression::StringLiteral("".into()),
                        BuiltinTypeKind::I32 => {
                            ASTExpression::Value(ValueType::I32(0), ASTIndex::none())
                        }
                        BuiltinTypeKind::Bool => {
                            ASTExpression::Value(ValueType::Boolean(true), ASTIndex::none())
                        }
                        BuiltinTypeKind::Char => {
                            ASTExpression::Value(ValueType::Char('a'), ASTIndex::none())
                        }
                        BuiltinTypeKind::F32 => {
                            ASTExpression::Value(ValueType::F32(1.0), ASTIndex::none())
                        }
                        BuiltinTypeKind::Lambda {
                            parameters,
                            return_type,
                        } => ASTExpression::Any(it.clone()),
                    },
                    ASTType::Generic(_) => panic!(),
                    ASTType::Custom {
                        name,
                        param_types: _,
                    } => ASTExpression::Any(it.clone()),
                })
                .collect(),
            index: ASTIndex::none(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedFunctionCall {
    pub name: String,
    pub param_types: Vec<ASTTypedType>,
}

impl TypedFunctionCall {
    pub fn new(name: &str, param_types: Vec<ASTTypedType>) -> Self {
        Self {
            name: name.into(),
            param_types,
        }
    }
}