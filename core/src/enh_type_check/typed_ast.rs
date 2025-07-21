use core::panic;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

use linked_hash_map::LinkedHashMap;
use linked_hash_set::LinkedHashSet;
use log::info;
use rasm_parser::catalog::modules_catalog::ModulesCatalog;

use crate::codegen::c::code_gen_c::value_type_to_typed_type;
use crate::codegen::compile_target::CompileTarget;
use crate::codegen::enh_ast::{
    EnhASTExpression, EnhASTFunctionBody, EnhASTFunctionCall, EnhASTFunctionDef, EnhASTIndex,
    EnhASTLambdaDef, EnhASTNameSpace, EnhASTParameterDef, EnhASTStatement, EnhASTType,
    EnhBuiltinTypeKind, EnhModuleId,
};
use crate::codegen::enh_val_context::{EnhValContext, TypedValContext};
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::TypedValKind;
use crate::enh_type_check::conv_context::{
    conv_to_typed_parameter_def, conv_to_typed_type, ConvContext,
};
use crate::enh_type_check::enh_functions_container::EnhTypeFilter;
use crate::enh_type_check::enh_type_check::EnhTypeCheck;
use crate::enh_type_check::enh_type_check_error::EnhTypeCheckError;
use crate::enh_type_check::verify;
use crate::errors::{CompilationError, CompilationErrorKind};
use crate::type_check::ast_modules_container::ASTModulesContainer;
use crate::type_check::ast_type_checker::ASTTypeChecker;
use crate::type_check::get_new_native_call;
use rasm_parser::parser::ast::{ASTModifiers, ASTType, ASTValueType};
use rasm_utils::{debug_i, dedent, indent, SliceDisplay};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedFunctionDef {
    pub namespace: EnhASTNameSpace,
    pub name: String,
    pub original_name: String,
    pub parameters: Vec<ASTTypedParameterDef>,
    pub return_type: ASTTypedType,
    pub body: ASTTypedFunctionBody,
    pub resolved_generic_types: ResolvedGenericTypedTypes,
    pub index: EnhASTIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedGenericTypedTypes {
    map: LinkedHashMap<String, LinkedHashMap<Vec<EnhASTType>, ASTTypedType>>,
}

impl ResolvedGenericTypedTypes {
    pub fn new() -> Self {
        Self {
            map: LinkedHashMap::new(),
        }
    }

    pub fn insert(
        &mut self,
        name: String,
        var_types: Vec<EnhASTType>,
        typed_type: ASTTypedType,
    ) -> Option<ASTTypedType> {
        let new = self.map.entry(name).or_insert(LinkedHashMap::new());
        new.insert(var_types, typed_type)
    }

    pub fn get(&self, name: &str, var_types: &Vec<EnhASTType>) -> Option<&ASTTypedType> {
        self.map.get(name).and_then(|it| it.get(var_types))
    }

    pub fn iter(&self) -> impl Iterator<Item = ((String, Vec<EnhASTType>), &ASTTypedType)> {
        self.map.iter().flat_map(|(key, inner_map)| {
            inner_map
                .iter()
                .map(move |(vec_key, val)| ((key.clone(), vec_key.clone()), val))
        })
    }

    pub fn remove_generics_prefix(self) -> Self {
        let mut new = LinkedHashMap::new();

        for (name, inner) in self.map.into_iter() {
            let inner_new = new
                .entry(ASTType::get_original_generic(&name).unwrap().to_owned())
                .or_insert(LinkedHashMap::new());

            for (var_types, t) in inner.into_iter() {
                inner_new
                    .entry(
                        var_types
                            .into_iter()
                            .map(|it| it.remove_generics_prefix())
                            .collect(),
                    )
                    .or_insert(t);
            }
        }
        ResolvedGenericTypedTypes { map: new }
    }
}

impl Display for ResolvedGenericTypedTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for ((name, par_types), t) in self.iter() {
            writeln!(f, "{name}/<{}>={t}", SliceDisplay(&par_types))?;
        }
        Ok(())
    }
}

impl ASTTypedFunctionDef {
    pub fn original_signature(&self, typed_module: &ASTTypedModule) -> String {
        let pars: Vec<String> = self
            .parameters
            .iter()
            .map(|it| Self::original_par(typed_module, it))
            .collect();
        let mut result = format!("{}({})", self.original_name, pars.join(","));
        if self.return_type != ASTTypedType::Unit {
            result.push_str(&format!(
                " -> {}",
                Self::original_type(typed_module, &self.return_type)
            ));
        }
        result
    }

    fn original_par(typed_module: &ASTTypedModule, par: &ASTTypedParameterDef) -> String {
        format!(
            "{}: {}",
            par.name,
            Self::original_type(typed_module, &par.ast_type)
        )
    }

    fn original_type(typed_module: &ASTTypedModule, typed_type: &ASTTypedType) -> String {
        if let Some(enh_type) = typed_module.get_type_from_typed_type(typed_type) {
            format!("{enh_type}")
        } else {
            format!("{typed_type}")
        }
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
    pub parameter_names: Vec<(String, EnhASTIndex)>,
    pub body: Vec<ASTTypedStatement>,
    pub index: EnhASTIndex,
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
        namespace: EnhASTNameSpace,
        name: String,
    },
    Struct {
        namespace: EnhASTNameSpace,
        name: String,
    },
    Type {
        namespace: EnhASTNameSpace,
        name: String,
        body: String,
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
                parameters: _,
                return_type: _,
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

    pub fn namespace(&self) -> Option<EnhASTNameSpace> {
        match self {
            ASTTypedType::Builtin(_) => None,
            ASTTypedType::Enum { namespace, name: _ } => Some(namespace.clone()),
            ASTTypedType::Struct { namespace, name: _ } => Some(namespace.clone()),
            ASTTypedType::Type {
                namespace,
                name: _,
                body: _,
            } => Some(namespace.clone()),
            ASTTypedType::Unit => None,
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
                body: _,
            } => f.write_str(&name.to_string()),
            ASTTypedType::Unit => f.write_str("()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedParameterDef {
    pub name: String,
    pub ast_type: ASTTypedType,
    pub ast_index: EnhASTIndex,
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
    pub fn new(name: &str, ast_type: ASTTypedType, ast_index: EnhASTIndex) -> ASTTypedParameterDef {
        Self {
            name: name.into(),
            ast_type,
            ast_index,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedFunctionCall {
    pub namespace: EnhASTNameSpace,
    pub function_name: String,
    pub original_function_name: String,
    pub parameters: Vec<ASTTypedExpression>,
    pub index: EnhASTIndex,
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
                .get(&self.function_name)
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
    ASTFunctionCallExpression(ASTTypedFunctionCall),
    ValueRef(String, EnhASTIndex, EnhASTNameSpace),
    Value(ASTValueType, EnhASTIndex),
    Lambda(ASTTypedLambdaDef),
}

impl ASTTypedExpression {
    pub fn get_index(&self) -> Option<EnhASTIndex> {
        match self {
            ASTTypedExpression::ASTFunctionCallExpression(call) => Some(call.index.clone()),
            ASTTypedExpression::ValueRef(_, index, _namespace) => Some(index.clone()),
            ASTTypedExpression::Value(_, index) => Some(index.clone()),
            ASTTypedExpression::Lambda(_lambda) => None,
        }
    }

    pub fn namespace(&self) -> EnhASTNameSpace {
        match self {
            ASTTypedExpression::ASTFunctionCallExpression(call) => call.namespace.clone(),
            ASTTypedExpression::ValueRef(_, _index, namespace) => namespace.clone(),
            _ => EnhASTNameSpace::global(), // TODO the others (example lambda)?
        }
    }
}

impl Display for ASTTypedExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                let pars: Vec<String> =
                    call.parameters.iter().map(|it| format!("{}", it)).collect();
                write!(f, "{}({})", call.function_name, pars.join(","))
            }
            ASTTypedExpression::ValueRef(name, _index, _) => f.write_str(name),
            ASTTypedExpression::Value(val_type, _) => write!(f, "{val_type}"),
            ASTTypedExpression::Lambda(lambda) => write!(f, "{lambda}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTTypedStatement {
    Expression(ASTTypedExpression),
    LetStatement(String, ASTTypedExpression, EnhASTIndex),
    ConstStatement(
        String,
        ASTTypedExpression,
        EnhASTIndex,
        EnhASTNameSpace,
        ASTModifiers,
    ),
}

impl ASTTypedStatement {
    pub fn get_index(&self) -> Option<EnhASTIndex> {
        match self {
            ASTTypedStatement::Expression(e) => e.get_index(),
            ASTTypedStatement::LetStatement(_, _, index) => Some(index.clone()),
            ASTTypedStatement::ConstStatement(_, _, index, _, _) => Some(index.clone()),
        }
    }
}

impl Display for ASTTypedStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTTypedStatement::Expression(e) => f.write_str(&format!("{e};\n")),
            ASTTypedStatement::LetStatement(name, e, _index) => {
                f.write_str(&format!("let {name} = {e};\n"))
            }
            ASTTypedStatement::ConstStatement(name, e, _index, _namespace, modifiers) => {
                let prefix = if modifiers.public { "pub " } else { "" };
                f.write_str(&format!("{prefix} const {name} = {e};\n"))
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
    pub namespace: EnhASTNameSpace,
    pub modifiers: ASTModifiers,
    pub name: String,
    pub variants: Vec<ASTTypedEnumVariantDef>,
    pub ast_type: EnhASTType,
    pub ast_typed_type: ASTTypedType,
    pub index: EnhASTIndex,
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

impl CustomTypedTypeDef for ASTTypedEnumDef {
    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &EnhASTNameSpace {
        &self.namespace
    }

    fn ast_typed_type(&self) -> &ASTTypedType {
        &self.ast_typed_type
    }

    fn ast_type(&self) -> &EnhASTType {
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
    pub namespace: EnhASTNameSpace,
    pub modifiers: ASTModifiers,
    pub name: String,
    pub properties: Vec<ASTTypedStructPropertyDef>,
    pub ast_type: EnhASTType,
    pub ast_typed_type: ASTTypedType,
    pub index: EnhASTIndex,
}

impl CustomTypedTypeDef for ASTTypedStructDef {
    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &EnhASTNameSpace {
        &self.namespace
    }

    fn ast_typed_type(&self) -> &ASTTypedType {
        &self.ast_typed_type
    }

    fn ast_type(&self) -> &EnhASTType {
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
    pub namespace: EnhASTNameSpace,
    pub modifiers: ASTModifiers,
    pub original_name: String,
    pub name: String,
    pub generic_types: ResolvedGenericTypedTypes,
    pub body: String,
    pub ast_type: EnhASTType,
    pub ast_typed_type: ASTTypedType,
    pub index: EnhASTIndex,
}

pub trait CustomTypedTypeDef: Display + Debug {
    fn modifiers(&self) -> &ASTModifiers;

    fn namespace(&self) -> &EnhASTNameSpace;

    fn ast_typed_type(&self) -> &ASTTypedType;

    fn ast_type(&self) -> &EnhASTType;

    fn custom_ast_type_name(&self) -> Option<String> {
        if let EnhASTType::Custom {
            namespace: _,
            name,
            param_types: _,
            index: _,
        } = self.ast_type()
        {
            Some(name.clone())
        } else {
            None
        }
    }
}

impl CustomTypedTypeDef for ASTTypedTypeDef {
    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn namespace(&self) -> &EnhASTNameSpace {
        &self.namespace
    }

    fn ast_typed_type(&self) -> &ASTTypedType {
        &self.ast_typed_type
    }

    fn ast_type(&self) -> &EnhASTType {
        &self.ast_type
    }
}

impl Display for ASTTypedTypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let gen_types = self
            .generic_types
            .iter()
            .map(|((_name, _var_types), it)| format!("{it}"))
            .collect::<Vec<_>>()
            .join(",");
        f.write_str(&format!("type {}<{gen_types}>", self.name))
    }
}

pub fn convert_to_typed_module(
    original_module: EnhancedASTModule,
    print_module: bool,
    mandatory_functions: Vec<DefaultFunction>,
    statics: &mut Statics,
    default_functions: Vec<DefaultFunction>,
    target: &CompileTarget,
    debug: bool,
    ast_type_checker: ASTTypeChecker,
    modules_catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    modules_container: &ASTModulesContainer,
) -> Result<ASTTypedModule, CompilationError> {
    let type_check = EnhTypeCheck::new(
        target.clone(),
        debug,
        ast_type_checker,
        modules_catalog,
        modules_container,
    );

    let module = type_check.type_check(
        original_module.clone(),
        statics,
        default_functions,
        mandatory_functions,
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

    // TODO we cannot use module because it does not contain the AddRef and Deref functions required for types,
    // since they are not referenced in code, but added by the compiler itself.
    // It could be an idea to add them as default functions in CompileTarget, but then we must remove them
    // after typed functions creator.
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
                /*
                let function_def = function
                    .index
                    .to_ast_index(modules_catalog)
                    .and_then(|it| modules_container.function(&it))
                    .map(|it| {
                        EnhASTFunctionDef::from_ast(
                            &function.index.id(),
                            &function.namespace,
                            it.clone(),
                            false,
                        )
                    });
                    */

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

                let mut val_context = EnhValContext::new(None);
                for par in function.parameters.iter() {
                    val_context
                        .insert_par(
                            par.name.clone(),
                            EnhASTParameterDef::new(
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
                            let tce = EnhTypeCheckError::new(
                                par.ast_index.clone(),
                                e.clone(),
                                Vec::new(),
                            );
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
                            .map(|it| EnhTypeFilter::Exact(it.clone()))
                            .collect::<Vec<_>>();
                        if let Some(new_function_def) = module
                            .functions_by_name
                            .find_call(
                                &it.name,
                                &it.name,
                                &filters,
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
    index: EnhASTIndex,
    message: String,
    errors: Vec<EnhTypeCheckError>,
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
        ASTTypedExpression::ASTFunctionCallExpression(call) => {
            debug_i!("function call expression");

            if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
                debug_i!("found function in module");
                function_def.return_type.clone()
            } else if let Some(function_def) = module.functions_by_name.get(&call.function_name) {
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
        ASTTypedExpression::ValueRef(name, index, namespace) => {
            if let Some(TypedValKind::ParameterRef(_, par)) = context.get(name) {
                par.ast_type.clone()
            } else if let Some(TypedValKind::LetRef(_, ast_type)) = context.get(name) {
                ast_type.clone()
            } else if let Some(entry) = statics.get_typed_const(name, namespace) {
                entry.ast_typed_type.clone()
            } else {
                if let Some(f) = module.functions_by_name.get(name) {
                    ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                        parameters: f
                            .parameters
                            .iter()
                            .map(|it| it.ast_type.clone())
                            .collect::<Vec<_>>(),
                        return_type: Box::new(f.return_type.clone()),
                    })
                } else {
                    dedent!();
                    return Err(verify::verify_error(
                        index.clone(),
                        format!("Unknown val {name}"),
                    ));
                }
            }
        }
        ASTTypedExpression::Value(val_type, _) => value_type_to_typed_type(val_type),
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
                    ASTTypedStatement::LetStatement(name, let_expr, _index) => {
                        let type_of_expr = get_type_of_typed_expression(
                            module, &context, let_expr, None, statics,
                        )?;

                        context.insert_let(name.to_string(), type_of_expr, None);
                    }
                    ASTTypedStatement::ConstStatement(
                        _name,
                        _expr,
                        _index,
                        _namespace,
                        _modifiers,
                    ) => {
                        return Err(verify::verify_error(
                            lambda_def.index.clone(),
                            "Const not allowed here".to_string(),
                        ));
                    }
                }
            }

            let real_return_type = if let Some(last) = lambda_def.body.iter().last() {
                match last {
                    ASTTypedStatement::Expression(e) => {
                        get_type_of_typed_expression(module, &context, e, ast_type, statics)?
                    }
                    ASTTypedStatement::LetStatement(_, _expr, _let_index) => ASTTypedType::Unit,
                    ASTTypedStatement::ConstStatement(_, _expr, _index, _namespace, _modifiers) => {
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

pub fn function_def(
    conv_context: &mut ConvContext,
    def: &EnhASTFunctionDef,
    _module: &EnhancedASTModule,
    _statics: &mut Statics,
) -> Result<ASTTypedFunctionDef, EnhTypeCheckError> {
    if !def.generic_types.is_empty() {
        panic!("function def has generics: {def}");
    }

    let mut generic_types = ResolvedGenericTypedTypes::new();

    for ((name, var_types), ast_type) in def.resolved_generic_types.iter() {
        let typed_type = conv_to_typed_type(&ast_type.namespace(), conv_context, ast_type, "");
        generic_types.insert(name, var_types, typed_type);
    }

    let function_return_type = conv_to_typed_type(
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
        parameters: def
            .parameters
            .iter()
            .map(|it| {
                conv_to_typed_parameter_def(
                    &it.ast_type.namespace(),
                    conv_context,
                    it,
                    &format!("function {}", def.name),
                )
            })
            .collect(),
        resolved_generic_types: generic_types.clone(),
        index: def.index.clone(),
    };

    /*
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

pub fn type_to_untyped_type(t: &ASTTypedType) -> EnhASTType {
    match t {
        ASTTypedType::Builtin(kind) => match kind {
            BuiltinTypedTypeKind::String => EnhASTType::Builtin(EnhBuiltinTypeKind::String),
            BuiltinTypedTypeKind::I32 => EnhASTType::Builtin(EnhBuiltinTypeKind::I32),
            BuiltinTypedTypeKind::Bool => EnhASTType::Builtin(EnhBuiltinTypeKind::Bool),
            BuiltinTypedTypeKind::Char => EnhASTType::Builtin(EnhBuiltinTypeKind::Char),
            BuiltinTypedTypeKind::F32 => EnhASTType::Builtin(EnhBuiltinTypeKind::F32),
            BuiltinTypedTypeKind::Lambda {
                parameters,
                return_type,
            } => EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                parameters: parameters.iter().map(type_to_untyped_type).collect(),
                return_type: if return_type.deref().is_unit() {
                    Box::new(EnhASTType::Unit)
                } else {
                    Box::new(type_to_untyped_type(return_type.deref()))
                },
            }),
        },
        ASTTypedType::Enum { namespace, name } => EnhASTType::Custom {
            namespace: namespace.clone(),
            name: name.into(),
            param_types: Vec::new(),
            index: EnhASTIndex::none(),
        },
        ASTTypedType::Struct { namespace, name } => EnhASTType::Custom {
            namespace: namespace.clone(),
            name: name.into(),
            param_types: Vec::new(),
            index: EnhASTIndex::none(),
        },
        ASTTypedType::Type {
            namespace,
            name,
            body: _,
        } => EnhASTType::Custom {
            namespace: namespace.clone(),
            name: name.into(),
            param_types: Vec::new(),
            index: EnhASTIndex::none(),
        },
        ASTTypedType::Unit => EnhASTType::Unit,
    }
}

fn expression(conv_context: &mut ConvContext, expression: &EnhASTExpression) -> ASTTypedExpression {
    match expression {
        EnhASTExpression::ASTFunctionCallExpression(fc) => {
            ASTTypedExpression::ASTFunctionCallExpression(function_call(conv_context, fc))
        }
        EnhASTExpression::ValueRef(v, index, namespace) => {
            ASTTypedExpression::ValueRef(v.clone(), index.clone(), namespace.clone())
        }
        EnhASTExpression::Value(val_type, index) => {
            ASTTypedExpression::Value(val_type.clone(), index.clone())
        }
        EnhASTExpression::Lambda(l) => ASTTypedExpression::Lambda(lambda_def(conv_context, l)),
        EnhASTExpression::Any(_ast_type) => {
            panic!("cannot handle Any type");
        }
    }
}

fn lambda_def(conv_context: &mut ConvContext, lambda_def: &EnhASTLambdaDef) -> ASTTypedLambdaDef {
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

fn body(conv_context: &mut ConvContext, body: &EnhASTFunctionBody) -> ASTTypedFunctionBody {
    match body {
        EnhASTFunctionBody::RASMBody(body) => ASTTypedFunctionBody::RASMBody(
            body.iter().map(|it| statement(conv_context, it)).collect(),
        ),
        EnhASTFunctionBody::NativeBody(body) => ASTTypedFunctionBody::NativeBody(body.clone()),
    }
}

fn statement(conv_context: &mut ConvContext, it: &EnhASTStatement) -> ASTTypedStatement {
    match it {
        EnhASTStatement::Expression(e) => {
            ASTTypedStatement::Expression(expression(conv_context, e))
        }
        EnhASTStatement::LetStatement(name, e, let_index) => ASTTypedStatement::LetStatement(
            name.clone(),
            expression(conv_context, e),
            let_index.clone(),
        ),
        EnhASTStatement::ConstStatement(name, e, const_index, namespace, modifiers) => {
            ASTTypedStatement::ConstStatement(
                name.clone(),
                expression(conv_context, e),
                const_index.clone(),
                namespace.clone(),
                modifiers.clone(),
            )
        }
    }
}

fn function_call(
    conv_context: &mut ConvContext,
    function_call: &EnhASTFunctionCall,
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
    pub param_types: Vec<EnhASTType>,
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
    pub fn new(name: &str, param_types: Vec<EnhASTType>, i: usize) -> Self {
        Self {
            name: name.into(),
            param_types,
            i,
        }
    }

    pub fn index(&self, function_def_index: &EnhASTIndex) -> EnhASTIndex {
        function_def_index.mv_down(self.i)
    }

    pub fn to_call(&self, function_def: &EnhASTFunctionDef) -> EnhASTFunctionCall {
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
    pub param_types: Vec<EnhASTType>,
}

impl DefaultFunction {
    pub fn new_0(name: &str) -> Self {
        Self {
            name: name.into(),
            param_types: vec![],
        }
    }

    pub fn new_1(name: &str, kind: EnhBuiltinTypeKind) -> Self {
        Self {
            name: name.into(),
            param_types: vec![EnhASTType::Builtin(kind)],
        }
    }

    pub fn new_2(name: &str, kind1: EnhBuiltinTypeKind, kind2: EnhBuiltinTypeKind) -> Self {
        Self {
            name: name.into(),
            param_types: vec![EnhASTType::Builtin(kind1), EnhASTType::Builtin(kind2)],
        }
    }

    pub fn new_3(
        name: &str,
        kind1: EnhBuiltinTypeKind,
        kind2: EnhBuiltinTypeKind,
        kind3: EnhBuiltinTypeKind,
    ) -> Self {
        Self {
            name: name.into(),
            param_types: vec![
                EnhASTType::Builtin(kind1),
                EnhASTType::Builtin(kind2),
                EnhASTType::Builtin(kind3),
            ],
        }
    }

    pub fn to_call(&self, namespace: &EnhASTNameSpace) -> EnhASTFunctionCall {
        EnhASTFunctionCall {
            namespace: namespace.clone(),
            function_name: self.name.clone(),
            original_function_name: self.name.clone(),
            parameters: self
                .param_types
                .iter()
                .map(|it| match it {
                    EnhASTType::Builtin(kind) => match kind {
                        EnhBuiltinTypeKind::String => EnhASTExpression::Value(
                            ASTValueType::String(String::new()),
                            EnhASTIndex::none(),
                        ),
                        EnhBuiltinTypeKind::I32 => {
                            EnhASTExpression::Value(ASTValueType::I32(0), EnhASTIndex::none())
                        }
                        EnhBuiltinTypeKind::Bool => EnhASTExpression::Value(
                            ASTValueType::Boolean(true),
                            EnhASTIndex::none(),
                        ),
                        EnhBuiltinTypeKind::Char => EnhASTExpression::Value(
                            ASTValueType::Char("a".to_string()),
                            EnhASTIndex::none(),
                        ),
                        EnhBuiltinTypeKind::F32 => {
                            EnhASTExpression::Value(ASTValueType::F32(1.0), EnhASTIndex::none())
                        }
                        EnhBuiltinTypeKind::Lambda {
                            parameters: _,
                            return_type: _,
                        } => EnhASTExpression::Any(it.clone()),
                    },
                    EnhASTType::Generic(_, _, _) => panic!(),
                    EnhASTType::Custom {
                        namespace: _,
                        name: _,
                        param_types: _,
                        index: _,
                    } => EnhASTExpression::Any(it.clone()),
                    EnhASTType::Unit => {
                        panic!("Parameters cannot have unit type");
                    }
                })
                .collect(),
            index: EnhASTIndex::none(),
            generics: Vec::new(),
            target: None,
        }
    }
}
