use crate::codegen::{EnhancedASTModule, MemoryValue};
use crate::parser::ast::{
    ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
    ASTLambdaDef, ASTParameterDef, ASTStructPropertyDef, ASTType, ASTTypeRef, BuiltinTypeKind,
};
use crate::type_check::{substitute, TypeConversionContext};
use linked_hash_map::LinkedHashMap;
use log::debug;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::parser::ast::ASTFunctionBody::{ASMBody, RASMBody};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedFunctionDef {
    pub name: String,
    pub parameters: Vec<ASTTypedParameterDef>,
    pub return_type: Option<ASTTypedTypeRef>,
    pub body: ASTTypedFunctionBody,
    pub inline: bool,
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
    pub parameter_names: Vec<String>,
    pub body: Vec<ASTTypedExpression>,
}

impl Display for ASTTypedLambdaDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self.parameter_names.join(",");
        let body = self.body.iter().map(|it| format!("{it};")).collect::<Vec<String>>().join("");

        f.write_str(&format!("{{ {pars} -> {body} }}"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedFunctionBody {
    RASMBody(Vec<ASTTypedExpression>),
    ASMBody(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinTypedTypeKind {
    ASTString,
    ASTI32,
    Lambda {
        parameters: Vec<ASTTypedTypeRef>,
        return_type: Option<Box<ASTTypedTypeRef>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedType {
    Builtin(BuiltinTypedTypeKind),
    Enum {
        name: String,
        //param_types: Vec<ASTTypedTypeRef>,
    },
    Struct {
        name: String,
        //param_types: Vec<ASTTypedTypeRef>,
    },
}

impl Display for ASTTypedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypedType::Builtin(kind) => match kind {
                BuiltinTypedTypeKind::ASTString => f.write_str("str"),
                BuiltinTypedTypeKind::ASTI32 => f.write_str("i32"),
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
            //BuiltinTypedTypeKind::Parametric(name) => f.write_str(name),
            /*
            ASTType::Custom { name, param_types } => {
                let pars: Vec<String> = param_types.iter().map(|it| format!("{it}")).collect();

                f.write_str(&format!("{name}<{}>", pars.join(",")))
            }

             */
            ASTTypedType::Enum { name } => {
                f.write_str(&format!("{name}"))
            }
            ASTTypedType::Struct { name } => {
                f.write_str(&format!("{name}"))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedParameterDef {
    pub name: String,
    pub type_ref: ASTTypedTypeRef,
}

impl Display for ASTTypedParameterDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}", self.name, self.type_ref))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedStructPropertyDef {
    pub name: String,
    pub type_ref: ASTTypedTypeRef,
}

impl ASTTypedParameterDef {
    pub fn new(name: &str, type_ref: ASTTypedTypeRef) -> ASTTypedParameterDef {
        Self {
            name: name.into(),
            type_ref,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedTypeRef {
    pub ast_type: ASTTypedType,
    pub ast_ref: bool,
}

impl Display for ASTTypedTypeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.ast_ref {
            f.write_str("&")?
        }
        f.write_str(&format!("{}", self.ast_type))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedFunctionCall {
    pub function_name: String,
    pub parameters: Vec<ASTTypedExpression>,
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
    Val(String),
    Number(i32),
    Lambda(ASTTypedLambdaDef),
    //EnumConstructor { name: String, variant: String, parameters: Vec<ASTExpression> },
}

impl Display for ASTTypedExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypedExpression::StringLiteral(s) => f.write_str(&"\"s\"".to_string()),
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                let pars: Vec<String> =
                    call.parameters.iter().map(|it| format!("{}", it)).collect();
                f.write_str(&format!("{}({})", call.function_name, pars.join(",")))
            }
            ASTTypedExpression::Val(p) => f.write_str(p),
            ASTTypedExpression::Number(b) => f.write_str(&format!("{b}")),
            ASTTypedExpression::Lambda(lambda) => f.write_str(&format!("{lambda}")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTTypedModule {
    pub body: Vec<ASTTypedFunctionCall>,
    pub functions_by_name: LinkedHashMap<String, ASTTypedFunctionDef>,
    pub enums: Vec<ASTTypedEnumDef>,
    pub structs: Vec<ASTTypedStructDef>,
    pub native_body: String,
    pub statics: LinkedHashMap<String, MemoryValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedEnumDef {
    pub name: String,
    pub variants: Vec<ASTTypedEnumVariantDef>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedStructDef {
    pub name: String,
    pub properties: Vec<ASTTypedStructPropertyDef>,
}

struct ConvContext<'a> {
    module: &'a EnhancedASTModule,
    enums: HashMap<ASTType, ASTTypedType>,
    structs: HashMap<ASTType, ASTTypedType>,
    enum_defs: Vec<ASTTypedEnumDef>,
    struct_defs: Vec<ASTTypedStructDef>,
    count: usize,
}

impl<'a> ConvContext<'a> {
    fn new(module: &'a EnhancedASTModule) -> Self {
        Self {
            module,
            enums: HashMap::new(),
            structs: HashMap::new(),
            enum_defs: Vec::new(),
            struct_defs: Vec::new(),
            count: 0,
        }
    }

    pub fn add_enum(&mut self, enum_type: &ASTType) -> ASTTypedType {
        debug!("add_enum {enum_type}");
        self.count += 1;
        if self.count > 100 {
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
                let mut generic_to_type = HashMap::new();
                for (i, p) in enum_def.type_parameters.iter().enumerate() {
                    generic_to_type.insert(
                        p.clone(),
                        cloned_param_types.get(i).unwrap().ast_type.clone(),
                    );
                }

                let new_name = format!("{name}_{}", self.enums.len());
                let enum_typed_type = ASTTypedType::Enum {
                    name: new_name.clone(),
                };

                let variants = enum_def
                    .variants
                    .iter()
                    .map(|it| {
                        enum_variant(self, it, &generic_to_type, &enum_type, &enum_typed_type, "")
                    })
                    .collect();

                self.enum_defs.push(ASTTypedEnumDef {
                    name: new_name,
                    variants,
                });

                self.enums
                    .insert(enum_type.clone(), enum_typed_type.clone());

                enum_typed_type
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
                let mut generic_to_type = HashMap::new();
                for (i, p) in struct_def.type_parameters.iter().enumerate() {
                    generic_to_type.insert(
                        p.clone(),
                        cloned_param_types.get(i).unwrap().ast_type.clone(),
                    );
                }

                let new_name = format!("{name}_{}", self.structs.len());
                let struct_typed_type = ASTTypedType::Struct {
                    name: new_name.clone(),
                };

                let properties = struct_def
                    .properties
                    .iter()
                    .map(|it| struct_property(self, it, &generic_to_type))
                    .collect();

                self.struct_defs.push(ASTTypedStructDef {
                    name: new_name.clone(),
                    properties,
                });

                self.structs
                    .insert(struct_type.clone(), struct_typed_type.clone());

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
}

pub fn convert_to_typed_module(
    module: &EnhancedASTModule,
    new_body: Vec<ASTFunctionCall>,
    typed_context: &mut TypeConversionContext,
) -> ASTTypedModule {
    let mut conv_context = ConvContext::new(module);

    let mut functions_by_name = LinkedHashMap::new();

    for new_function_def in typed_context.iter() {
        functions_by_name.insert(
            new_function_def.name.clone(),
            function_def(&mut conv_context, &new_function_def),
        );
    }

    vec!["malloc", "exit", "sprint", "outOfHeapSpace", "outOfMemory", "slen", "sprintln", "println", "addRef", "memcopy"].iter().for_each(|it| {
        add_mandatory_function(module, &mut conv_context, &mut functions_by_name, it)
    });

    ASTTypedModule {
        body: new_body.iter().map(function_call).collect(),
        structs: conv_context.struct_defs,
        enums: conv_context.enum_defs,
        functions_by_name,
        statics: module.statics.clone(),
        native_body: module.native_body.clone(),
    }
}

fn add_mandatory_function(
    module: &EnhancedASTModule,
    conv_context: &mut ConvContext,
    functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
    function_name: &str,
) {
    if let Some(f) = module.functions_by_name.get(function_name) {
        functions_by_name.insert(
            function_name.into(),
            function_def(
                conv_context,
                f,
            ),
        );
    }
}

fn struct_property(
    conv_context: &mut ConvContext,
    property: &ASTStructPropertyDef,
    generic_to_type: &HashMap<String, ASTType>,
) -> ASTTypedStructPropertyDef {
    if let Some(new_type) = substitute(&property.type_ref, generic_to_type) {
        ASTTypedStructPropertyDef {
            name: property.name.clone(),
            type_ref: type_ref(conv_context, &new_type, ""),
        }
    } else {
        ASTTypedStructPropertyDef {
            name: property.name.clone(),
            type_ref: type_ref(conv_context, &property.type_ref, ""),
        }
    }
}

fn function_def(conv_context: &mut ConvContext, def: &ASTFunctionDef) -> ASTTypedFunctionDef {
    if !def.param_types.is_empty() {
        panic!("function def has generics: {def}");
    }
    ASTTypedFunctionDef {
        name: def.name.clone(),
        body: body(&def.body),
        return_type: def.return_type.clone().map(|it| {
            type_ref(
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
    }
}

fn expression(expression: &ASTExpression) -> ASTTypedExpression {
    match expression {
        ASTExpression::StringLiteral(s) => ASTTypedExpression::StringLiteral(s.to_string()),
        ASTExpression::ASTFunctionCallExpression(fc) => {
            ASTTypedExpression::ASTFunctionCallExpression(function_call(fc))
        }
        ASTExpression::Val(v) => ASTTypedExpression::Val(v.clone()),
        ASTExpression::Number(n) => ASTTypedExpression::Number(*n),
        ASTExpression::Lambda(l) => ASTTypedExpression::Lambda(lambda_def(l)),
    }
}

fn lambda_def(lambda_def: &ASTLambdaDef) -> ASTTypedLambdaDef {
    ASTTypedLambdaDef {
        parameter_names: lambda_def.parameter_names.clone(),
        body: lambda_def.body.iter().map(expression).collect(),
    }
}

fn body(body: &ASTFunctionBody) -> ASTTypedFunctionBody {
    match body {
        RASMBody(body) => {
            ASTTypedFunctionBody::RASMBody(body.iter().map(expression).collect())
        }
        ASMBody(body) => ASTTypedFunctionBody::ASMBody(body.clone()),
    }
}

fn enum_variant(
    conv_context: &mut ConvContext,
    variant: &ASTEnumVariantDef,
    generic_to_type: &HashMap<String, ASTType>,
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
                if &it.type_ref.ast_type == enum_type {
                    ASTTypedParameterDef {
                        name: it.name.clone(),
                        type_ref: ASTTypedTypeRef {
                            ast_ref: it.type_ref.ast_ref,
                            ast_type: enum_typed_type.clone(),
                        },
                    }
                } else if let Some(new_type) = substitute(&it.type_ref, generic_to_type) {
                    debug!("new_type {new_type}");

                    if &new_type.ast_type == enum_type {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            type_ref: ASTTypedTypeRef {
                                ast_ref: it.type_ref.ast_ref,
                                ast_type: enum_typed_type.clone(),
                            },
                        }
                    } else {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            type_ref: type_ref(conv_context, &new_type, ""),
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

fn function_call(function_call: &ASTFunctionCall) -> ASTTypedFunctionCall {
    ASTTypedFunctionCall {
        function_name: function_call.function_name.clone(),
        parameters: function_call.parameters.iter().map(expression).collect(),
    }
}

fn parameter_def(
    conv_context: &mut ConvContext,
    parameter_def: &ASTParameterDef,
    message: &str,
) -> ASTTypedParameterDef {
    ASTTypedParameterDef {
        name: parameter_def.name.clone(),
        type_ref: type_ref(
            conv_context,
            &parameter_def.type_ref,
            &format!("{message}: parameter {}", parameter_def.name),
        ),
    }
}

fn type_ref(conv_context: &mut ConvContext, t_ref: &ASTTypeRef, message: &str) -> ASTTypedTypeRef {
    let ast_type = match &t_ref.ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::ASTString => ASTTypedType::Builtin(BuiltinTypedTypeKind::ASTString),
            BuiltinTypeKind::ASTI32 => ASTTypedType::Builtin(BuiltinTypedTypeKind::ASTI32),
            BuiltinTypeKind::Lambda {
                return_type,
                parameters,
            } => ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: parameters
                    .iter()
                    .map(|it| {
                        type_ref(
                            conv_context,
                            it,
                            &(message.to_owned() + ", lambda parameter"),
                        )
                    })
                    .collect(),
                return_type: return_type.clone().map(|it| {
                    Box::new(type_ref(
                        conv_context,
                        &it,
                        &(message.to_owned() + ", lambda return type"),
                    ))
                }),
            }),
        },
        ASTType::Parametric(p) => {
            panic!("Unresolved parametric type '{p}': {message}");
        }
        ASTType::Custom { name, param_types } => {
            /*
            if let Some(enum_def) = self.module.enums.iter().find(|it| &it.name == name) {
                ASTTypedType::Enum {
                    name: name.clone(),
                    param_types: param_types
                        .iter()
                        .map(|it| type_ref(it, &format!("{message}, enum {name}")))
                        .collect(),
                }
            } else if let Some(struct_def) =
            self.module.structs.iter().find(|it| &it.name == name)
            {
                ASTTypedType::Struct {
                    name: name.clone(),
                    param_types: param_types
                        .iter()
                        .map(|it| type_ref(it, &format!("{message}, struct {name}")))
                        .collect(),
                }
            } else if let Some(resolved_enum_def) = self
                .enums
                .iter()
                .flat_map(|it| it.1.iter())
                .find(|it| &it.enum_def.name == name)
            {
                ASTTypedType::Enum {
                    name: name.clone(),
                    param_types: resolved_enum_def
                        .parameter_types
                        .iter()
                        .map(|it| self.type_ref(it, &format!("{message}, enum {name}")))
                        .collect(),
                }
            } else {
                panic!("Cannot find Custom type '{name}'");
            }

             */
            if let Some(_) = conv_context.module.enums.iter().find(|it| &it.name == name) {
                if let Some(e) = conv_context.get_enum(&t_ref.ast_type) {
                    e
                } else {
                    conv_context.add_enum(&t_ref.ast_type)
                }
            } else if let Some(_) = conv_context
                .module
                .structs
                .iter()
                .find(|it| &it.name == name)
            {
                if let Some(e) = conv_context.get_struct(&t_ref.ast_type) {
                    e
                } else {
                    conv_context.add_struct(&t_ref.ast_type)
                }
            } else {
                panic!("Cannot find custom tyype {}", name);
            }
        }
    };

    ASTTypedTypeRef {
        ast_type,
        ast_ref: t_ref.ast_ref,
    }
}

pub fn print_typed_module(module: &ASTTypedModule) {
    module.body.iter().for_each(|call| {
        println!("{call}");
    });
    println!();
    module.functions_by_name.values().for_each(|f| {
        print_function_def(f)
    })
}

pub fn print_function_def(f: &ASTTypedFunctionDef) {
    match &f.body {
        ASTTypedFunctionBody::RASMBody(_) => print!("fn {}", f),
        ASTTypedFunctionBody::ASMBody(_) => print!("asm {}", f)
    }
    match &f.body {
        ASTTypedFunctionBody::RASMBody(expressions) => {
            println!(" {{");
            expressions.iter().for_each(|call| {
                println!("  {}", call);
            });
            println!("}}");
        }
        ASTTypedFunctionBody::ASMBody(_) => println!(" {{...}}")
    }
}
