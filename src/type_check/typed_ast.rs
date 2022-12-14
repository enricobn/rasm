use crate::codegen::{EnhancedASTModule, TypedValContext, TypedValKind};
use crate::parser::ast::ASTFunctionBody::{ASMBody, RASMBody};
use crate::parser::ast::{
    ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex,
    ASTLambdaDef, ASTParameterDef, ASTStatement, ASTStructPropertyDef, ASTType, BuiltinTypeKind,
};
use crate::parser::ValueType;
use crate::type_check::{substitute, TypeConversionContext};
use linked_hash_map::LinkedHashMap;
use log::debug;
use std::fmt::{Display, Formatter};

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
    pub parameter_names: Vec<String>,
    pub body: Vec<ASTTypedStatement>,
}

impl Display for ASTTypedLambdaDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self.parameter_names.join(",");
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

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinTypedTypeKind {
    String,
    I32,
    Bool,
    Char,
    Lambda {
        parameters: Vec<ASTTypedType>,
        return_type: Option<Box<ASTTypedType>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
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

impl ASTTypedParameterDef {
    pub fn new(name: &str, ast_type: ASTTypedType) -> ASTTypedParameterDef {
        Self {
            name: name.into(),
            ast_type,
        }
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
    CharLiteral(char),
    ASTFunctionCallExpression(ASTTypedFunctionCall),
    ValueRef(String, ASTIndex),
    Value(ValueType, ASTIndex),
    Lambda(ASTTypedLambdaDef),
    //EnumConstructor { name: String, variant: String, parameters: Vec<ASTExpression> },
}

impl Display for ASTTypedExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypedExpression::StringLiteral(s) => f.write_str(&format!("\"{s}\"")),
            ASTTypedExpression::CharLiteral(c) => f.write_str(&format!("'{c}'")),
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                let pars: Vec<String> =
                    call.parameters.iter().map(|it| format!("{}", it)).collect();
                f.write_str(&format!("{}({})", call.function_name, pars.join(",")))
            }
            ASTTypedExpression::ValueRef(name, _index) => f.write_str(name),
            ASTTypedExpression::Value(val_type, _) => match val_type {
                ValueType::Boolean(b) => f.write_str(&format!("{b}")),
                ValueType::Number(n) => f.write_str(&format!("{n}")),
                ValueType::Char(c) => f.write_str(&format!("'{c}'")),
            },
            ASTTypedExpression::Lambda(lambda) => f.write_str(&format!("{lambda}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedStatement {
    Expression(ASTTypedExpression),
    LetStatement(String, ASTTypedExpression),
}

impl Display for ASTTypedStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypedStatement::Expression(e) => f.write_str(&format!("{e};\n")),
            ASTTypedStatement::LetStatement(name, e) => {
                f.write_str(&format!("let {name} = {e};\n"))
            }
        }
    }
}

#[derive(Clone)]
pub struct ASTTypedModule {
    pub body: Vec<ASTTypedStatement>,
    pub functions_by_name: LinkedHashMap<String, ASTTypedFunctionDef>,
    pub enums: Vec<ASTTypedEnumDef>,
    pub structs: Vec<ASTTypedStructDef>,
    pub native_body: String,
    pub types: Vec<ASTTypedTypeDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedEnumDef {
    pub name: String,
    pub variants: Vec<ASTTypedEnumVariantDef>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedTypeDef {
    pub original_name: String,
    pub name: String,
    pub generic_types: LinkedHashMap<String, ASTTypedType>,
}

struct ConvContext<'a> {
    module: &'a EnhancedASTModule,
    enums: LinkedHashMap<ASTType, ASTTypedType>,
    structs: LinkedHashMap<ASTType, ASTTypedType>,
    types: LinkedHashMap<ASTType, ASTTypedType>,
    enum_defs: Vec<ASTTypedEnumDef>,
    struct_defs: Vec<ASTTypedStructDef>,
    type_defs: Vec<ASTTypedTypeDef>,
    count: usize,
}

impl<'a> ConvContext<'a> {
    fn new(module: &'a EnhancedASTModule) -> Self {
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

                let properties = struct_def
                    .properties
                    .iter()
                    .map(|it| struct_property(self, it, &generic_to_type))
                    .collect();

                self.struct_defs.push(ASTTypedStructDef {
                    name: new_name,
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

    pub fn add_type(&mut self, ast_type: &ASTType) -> ASTTypedType {
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
                            &cloned_param_types.get(i).unwrap_or_else(|| {
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
    typed_context: &mut TypeConversionContext,
    debug_asm: bool,
    print_allocation: bool,
    printl_module: bool,
    mandatory_functions: Vec<String>,
) -> ASTTypedModule {
    let mut conv_context = ConvContext::new(module);

    let mut functions_by_name = LinkedHashMap::new();

    for new_function_def in typed_context.iter() {
        functions_by_name.insert(
            new_function_def.name.clone(),
            function_def(&mut conv_context, &new_function_def),
        );
    }

    mandatory_functions
        .iter()
        .for_each(|it| add_function(module, &mut conv_context, &mut functions_by_name, it, true));

    let default_functions = &mut vec![
        "malloc",
        "exitMain",
        "sprint",
        //"outOfHeapSpace",
        //"outOfMemory",
        "slen",
        "sprintln",
        "println",
        "addRef",
        "memcopy",
        "deref",
        "negativeCount",
        "invalidAddress",
        "nprintln",
        "nprint",
        "removeFromReused",
        "addStaticStringToHeap",
        "createCmdLineArguments",
        "str_addRef",
        "str_deref",
        "sysOpen",
        "sysRead",
        "sysClose",
        "fileSize",
        "freeMem",
    ];

    if print_allocation {
        default_functions.append(&mut vec![
            "printAllocated",
            "printTableSlotsAllocated",
            "printAllocatedString",
            "printTableSlotsAllocatedString",
            "nprint",
        ])
    }

    if debug_asm {
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
    }

    default_functions.sort();
    default_functions.dedup();

    default_functions
        .iter()
        .for_each(|it| add_function(module, &mut conv_context, &mut functions_by_name, it, false));

    let result = ASTTypedModule {
        body: new_body.iter().map(statement).collect(),
        structs: conv_context.struct_defs,
        enums: conv_context.enum_defs,
        functions_by_name,
        native_body: module.native_body.clone(),
        types: conv_context.type_defs,
    };

    if printl_module {
        print_typed_module(&result);
    }

    verify(&result);

    result
}

fn verify(module: &ASTTypedModule) {
    let mut context = TypedValContext::new(None);

    for statement in module.body.iter() {
        verify_statement(module, &mut context, statement);
    }

    for function_def in module.functions_by_name.values() {
        let mut context = TypedValContext::new(None);

        for (i, par) in function_def.parameters.iter().enumerate() {
            context.insert_par(par.name.clone(), i, par.clone());
        }

        if let ASTTypedFunctionBody::RASMBody(expressions) = &function_def.body {
            for statement in expressions.iter() {
                verify_statement(module, &mut context, statement)
            }
        }
    }
}

fn verify_statement(
    module: &ASTTypedModule,
    context: &mut TypedValContext,
    statement: &ASTTypedStatement,
) {
    match statement {
        ASTTypedStatement::Expression(e) => {
            if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                verify_function_call(module, context, call);
            }
        }
        ASTTypedStatement::LetStatement(name, e) => {
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
                    } else {
                        panic!("{call}")
                    };
                context.insert_let(name.clone(), ast_typed_type.unwrap());
            }

            if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                verify_function_call(module, context, call);
            }
        }
    }
}

fn verify_function_call(
    module: &ASTTypedModule,
    context: &TypedValContext,
    call: &ASTTypedFunctionCall,
) {
    debug!("call {call}");

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
                parameters
                    .iter()
                    .map(|it| it.clone())
                    .collect::<Vec<ASTTypedType>>()
            } else {
                panic!();
            }
        } else {
            panic!("{call}");
        };

    for (i, expr) in call.parameters.iter().enumerate() {
        let par = parameters_types.get(i).unwrap().clone();
        debug!("par type {par}");
        assert_eq!(
            get_type_of_typed_expression(module, context, expr, Some(&par)).unwrap(),
            par,
            "expression {:?}",
            expr
        );
    }
}

fn get_type_of_typed_expression(
    module: &ASTTypedModule,
    context: &TypedValContext,
    expr: &ASTTypedExpression,
    ast_type: Option<&ASTTypedType>,
) -> Option<ASTTypedType> {
    debug!("expression {expr} {:?}", ast_type);
    match expr {
        ASTTypedExpression::StringLiteral(_) => {
            Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::String))
        }
        ASTTypedExpression::CharLiteral(_) => {
            Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Char))
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
            } else {
                panic!("Unknown val {name}");
            }
        }
        ASTTypedExpression::Value(val_type, _) => match val_type {
            ValueType::Boolean(_) => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Bool)),
            ValueType::Number(_) => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::I32)),
            ValueType::Char(_) => Some(ASTTypedType::Builtin(BuiltinTypedTypeKind::Char)),
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

            for (i, name) in lambda_def.parameter_names.iter().enumerate() {
                let parameter_def = ASTTypedParameterDef {
                    name: name.clone(),
                    ast_type: parameters.get(i).unwrap().clone(),
                };
                context.insert_par(name.clone(), i, parameter_def);
            }

            for statement in lambda_def.body.iter() {
                match statement {
                    ASTTypedStatement::Expression(e) => {
                        if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                            verify_function_call(module, &context, call);
                        }
                    }
                    ASTTypedStatement::LetStatement(name, e) => {
                        if let ASTTypedExpression::ASTFunctionCallExpression(call) = e {
                            let ast_typed_type = module
                                .functions_by_name
                                .get(&call.function_name.replace("::", "_"))
                                .unwrap()
                                .return_type
                                .clone()
                                .unwrap();
                            context.insert_let(name.clone(), ast_typed_type);
                            verify_function_call(module, &context, call);
                        }
                    }
                }
            }

            let real_return_type = if let Some(last) = lambda_def.body.iter().last() {
                match last {
                    ASTTypedStatement::Expression(e) => {
                        get_type_of_typed_expression(module, &context, e, None)
                    }
                    ASTTypedStatement::LetStatement(_, e) => {
                        get_type_of_typed_expression(module, &context, e, None)
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
    }
}

fn add_function(
    module: &EnhancedASTModule,
    conv_context: &mut ConvContext,
    functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
    function_name: &str,
    mandatory: bool,
) {
    if let Some(f) = module.functions_by_name.get(function_name) {
        functions_by_name.insert(function_name.into(), function_def(conv_context, f));
    } else if mandatory {
        panic!("Cannot find mandatory function {function_name}");
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

fn function_def(conv_context: &mut ConvContext, def: &ASTFunctionDef) -> ASTTypedFunctionDef {
    if !def.param_types.is_empty() {
        panic!("function def has generics: {def}");
    }

    let mut generic_types = LinkedHashMap::new();

    for (name, ast_type) in def.resolved_generic_types.iter() {
        let typed_type = typed_type(conv_context, ast_type, "");
        generic_types.insert(name.into(), typed_type);
    }

    ASTTypedFunctionDef {
        name: def.name.clone(),
        body: body(&def.body),
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
        generic_types,
    }
}

fn expression(expression: &ASTExpression) -> ASTTypedExpression {
    match expression {
        ASTExpression::StringLiteral(s) => ASTTypedExpression::StringLiteral(s.to_string()),
        ASTExpression::ASTFunctionCallExpression(fc) => {
            ASTTypedExpression::ASTFunctionCallExpression(function_call(fc))
        }
        ASTExpression::ValueRef(v, index) => ASTTypedExpression::ValueRef(v.clone(), index.clone()),
        ASTExpression::Value(val_type, index) => {
            ASTTypedExpression::Value(val_type.clone(), index.clone())
        }
        ASTExpression::Lambda(l) => ASTTypedExpression::Lambda(lambda_def(l)),
    }
}

fn lambda_def(lambda_def: &ASTLambdaDef) -> ASTTypedLambdaDef {
    ASTTypedLambdaDef {
        parameter_names: lambda_def.parameter_names.clone(),
        body: lambda_def.body.iter().map(statement).collect(),
    }
}

fn body(body: &ASTFunctionBody) -> ASTTypedFunctionBody {
    match body {
        RASMBody(body) => ASTTypedFunctionBody::RASMBody(body.iter().map(statement).collect()),
        ASMBody(body) => ASTTypedFunctionBody::ASMBody(body.clone()),
    }
}

fn statement(it: &ASTStatement) -> ASTTypedStatement {
    match it {
        ASTStatement::Expression(e) => ASTTypedStatement::Expression(expression(e)),
        ASTStatement::LetStatement(name, e) => {
            ASTTypedStatement::LetStatement(name.clone(), expression(e))
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
                    }
                } else if let Some(new_type) = substitute(&it.ast_type, generic_to_type) {
                    debug!("new_type {new_type}");

                    if &new_type == enum_type {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            ast_type: enum_typed_type.clone(),
                        }
                    } else {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            ast_type: typed_type(conv_context, &new_type, ""),
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
        ast_type: typed_type(
            conv_context,
            &parameter_def.ast_type,
            &format!("{message}: parameter {}", parameter_def.name),
        ),
    }
}

fn typed_type(conv_context: &mut ConvContext, ast_type: &ASTType, message: &str) -> ASTTypedType {
    match ast_type {
        ASTType::Builtin(kind) => match kind {
            BuiltinTypeKind::String => ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
            BuiltinTypeKind::I32 => ASTTypedType::Builtin(BuiltinTypedTypeKind::I32),
            BuiltinTypeKind::Bool => ASTTypedType::Builtin(BuiltinTypedTypeKind::Bool),
            BuiltinTypeKind::Char => ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
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
        ASTType::Parametric(p) => {
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
            } else if conv_context.module.types.iter().any(|it| &it.name == name) {
                if let Some(e) = conv_context.get_type(ast_type) {
                    e
                } else {
                    conv_context.add_type(ast_type)
                }
            } else {
                panic!("Cannot find custom type {name}");
            }
        }
    }
}

pub fn print_typed_module(module: &ASTTypedModule) {
    for enum_def in module.enums.iter() {
        println!("{enum_def}");
    }
    /* TODO
    for struct_def in module.structs.iter() {
        println!("{struct_def}");
    }
     */

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
