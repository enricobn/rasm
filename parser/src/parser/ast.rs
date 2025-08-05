use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::iter::zip;
use std::ops::Deref;
use std::sync::atomic::AtomicUsize;

use derivative::Derivative;
use itertools::Itertools;

static ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, PartialEq, Eq, Hash, strum_macros::Display, PartialOrd, Ord)]
pub enum ASTBuiltinFunctionType {
    Match,
    MatchOne,
    StructGetter,
    StructLambdaCall,
    StructSetter,
    StructLambdaSetter,
    StructConstructor,
    EnumVariantConstructor,
    Other(String),
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq, Hash, PartialOrd, Ord)]
pub struct ASTPosition {
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialOrd = "ignore")]
    #[derivative(Ord = "ignore")]
    pub id: usize,
    pub row: usize,
    pub column: usize,
    /// used to identify builtin functions and disambiguate the ones related to the same type/property
    pub builtin: Option<ASTBuiltinFunctionType>,
}

impl ASTPosition {
    pub fn new(row: usize, column: usize) -> Self {
        Self {
            id: ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            row,
            column,
            builtin: None,
        }
    }

    pub fn builtin(position: &ASTPosition, builtin_type: ASTBuiltinFunctionType) -> Self {
        Self {
            id: ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            row: position.row,
            column: position.column,
            builtin: Some(builtin_type),
        }
    }

    pub fn none() -> Self {
        Self {
            id: ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            row: 0,
            column: 0,
            builtin: None,
        }
    }

    pub fn mv_left(&self, len: usize) -> Self {
        Self {
            id: ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            row: self.row,
            column: self.column - len,
            builtin: self.builtin.clone(),
        }
    }

    pub fn mv_right(&self, len: usize) -> Self {
        Self {
            id: ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            row: self.row,
            column: self.column + len,
            builtin: self.builtin.clone(),
        }
    }

    pub fn mv_down(&self, len: usize) -> Self {
        Self {
            id: ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            row: self.row + len,
            column: self.column,
            builtin: self.builtin.clone(),
        }
    }

    pub fn after(&self, position: &ASTPosition) -> bool {
        self.row > position.row || (self.row == position.row && self.column > position.column)
    }
}

impl Display for ASTPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.row, self.column)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub return_type: ASTType,
    pub body: ASTFunctionBody,
    pub generic_types: Vec<String>,
    pub position: ASTPosition,
    pub modifiers: ASTModifiers,
    pub target: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ASTFunctionSignature {
    pub name: String,
    pub generics: Vec<String>,
    pub parameters_types: Vec<ASTType>,
    pub return_type: ASTType,
    pub modifiers: ASTModifiers,
}

impl Display for ASTFunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let generics = if self.generics.is_empty() {
            ""
        } else {
            &format!("<{}>", self.generics.iter().join(", "))
        };

        write!(
            f,
            "{}{}({})",
            self.name,
            generics,
            self.parameters_types
                .iter()
                .map(|it| format!("{it}"))
                .join(", ")
        )?;
        if !self.return_type.is_unit() {
            write!(f, " -> {}", self.return_type)?;
        }
        Ok(())
    }
}

impl ASTFunctionSignature {
    pub fn generics_prefix(&self, prefix: &str) -> String {
        format!("{}_{}", prefix, self.name)
    }

    pub fn add_generic_prefix(self, prefix: &str) -> Self {
        let generics_prefix = self.generics_prefix(prefix);
        let mut result = self;
        result.parameters_types = result
            .parameters_types
            .into_iter()
            .map(|it| it.add_generic_prefix(&generics_prefix))
            .collect();
        result.return_type = result.return_type.add_generic_prefix(&generics_prefix);
        result.generics = result
            .generics
            .into_iter()
            .map(|it| format!("{generics_prefix}:{it}"))
            .collect();

        result
    }

    pub fn remove_generic_prefix(self) -> Self {
        let mut result = self;
        result.parameters_types = result
            .parameters_types
            .into_iter()
            .map(|it| it.remove_generic_prefix())
            .collect();
        result.return_type = result.return_type.remove_generic_prefix();
        result.generics = result
            .generics
            .into_iter()
            .map(|it| ASTType::remove_generic_prefix_from_str(&it).to_owned())
            .collect();

        result
    }

    pub fn is_generic(&self) -> bool {
        !self.generics.is_empty()
    }
}

impl Display for ASTFunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let modifiers = if self.modifiers.public { "pub " } else { "" };
        let generic_types = if self.generic_types.is_empty() {
            "".into()
        } else {
            format!("<{}>", self.generic_types.join(","))
        };

        let rt = if self.return_type != ASTType::ASTUnitType {
            format!("{}", self.return_type)
        } else {
            "()".to_owned()
        };

        let fun_or_asm = if let ASTFunctionBody::RASMBody(_) = self.body {
            "fn"
        } else {
            "native"
        };

        let args = self
            .parameters
            .iter()
            .map(|it| format!("{}", it))
            .collect::<Vec<String>>()
            .join(",");
        f.write_str(&format!(
            "{modifiers}{} {}{generic_types}({args}) -> {rt}",
            fun_or_asm, self.name
        ))
    }
}

impl ASTFunctionDef {
    pub fn signature(&self) -> ASTFunctionSignature {
        ASTFunctionSignature {
            name: self.name.clone(),
            generics: self.generic_types.clone(),
            parameters_types: self
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect(),
            return_type: self.return_type.clone(),
            modifiers: self.modifiers.clone(),
        }
    }

    pub fn from_signature(
        signature: ASTFunctionSignature,
        is_public: bool,
        position: ASTPosition,
        parameters_names: Vec<String>,
        parameters_positions: Vec<ASTPosition>,
        body: ASTFunctionBody,
        target: Option<String>,
    ) -> Self {
        assert_eq!(signature.parameters_types.len(), parameters_names.len());
        assert_eq!(signature.parameters_types.len(), parameters_positions.len());
        Self {
            name: signature.name,
            parameters: zip(
                signature.parameters_types,
                zip(parameters_names, parameters_positions),
            )
            .into_iter()
            .map(|(ast_type, (name, position))| ASTParameterDef {
                name,
                ast_type,
                position,
            })
            .collect(),
            return_type: signature.return_type,
            body,
            generic_types: signature.generics,
            position,
            modifiers: ASTModifiers { public: is_public },
            target,
        }
    }

    pub fn is_generic(&self) -> bool {
        !self.generic_types.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTLambdaDef {
    pub parameter_names: Vec<(String, ASTPosition)>,
    pub body: Vec<ASTStatement>,
    pub position: ASTPosition,
}

impl Display for ASTLambdaDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .parameter_names
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>()
            .join(",");
        let body = self
            .body
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<String>>()
            .join("");

        if pars.is_empty() {
            f.write_str(&format!("{{ {body} }}"))
        } else {
            f.write_str(&format!("fn({pars}) {{ {body} }}"))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTFunctionBody {
    RASMBody(Vec<ASTStatement>),
    NativeBody(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ASTBuiltinTypeKind {
    ASTBooleanType,
    ASTCharType,
    ASTIntegerType,
    ASTFloatType,
    ASTStringType,
    ASTLambdaType {
        parameters: Vec<ASTType>,
        return_type: Box<ASTType>,
    },
}

#[derive(Derivative)]
#[derivative(PartialEq, Hash)]
#[derive(Debug, Clone, Eq)]
pub enum ASTType {
    ASTBuiltinType(ASTBuiltinTypeKind),
    ASTGenericType(
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        ASTPosition,
        String,
        Vec<ASTType>,
    ),
    ASTCustomType {
        name: String,
        param_types: Vec<ASTType>,
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        position: ASTPosition,
    },
    ASTUnitType,
}

impl ASTType {
    pub fn is_unit(&self) -> bool {
        self == &ASTType::ASTUnitType
    }

    pub fn is_generic(&self) -> bool {
        return match self {
            ASTType::ASTBuiltinType(kind) => match kind {
                ASTBuiltinTypeKind::ASTStringType => false,
                ASTBuiltinTypeKind::ASTIntegerType => false,
                ASTBuiltinTypeKind::ASTBooleanType => false,
                ASTBuiltinTypeKind::ASTCharType => false,
                ASTBuiltinTypeKind::ASTFloatType => false,
                ASTBuiltinTypeKind::ASTLambdaType {
                    parameters,
                    return_type,
                } => return_type.is_generic() || parameters.iter().any(Self::is_generic),
            },
            ASTType::ASTGenericType(_, _, _) => true,
            ASTType::ASTCustomType {
                name: _,
                param_types: pt,
                position: _,
            } => pt.iter().any(|it| it.is_generic()),
            ASTType::ASTUnitType => false,
        };
    }

    /// Returns true if this type is exactly a "full" generic type. Returns false even if it's a Custom generic type or a lambda generic type
    pub fn is_strictly_generic(&self) -> bool {
        return matches!(self, ASTType::ASTGenericType(..));
    }

    pub fn add_generic_prefix(self, prefix: &dyn Display) -> Self {
        if format!("{prefix}").contains(":") {
            panic!("unsupported prefix {prefix}");
        }
        if let ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
            parameters,
            return_type,
        }) = self
        {
            return ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                parameters: parameters
                    .into_iter()
                    .map(|it| it.add_generic_prefix(prefix))
                    .collect(),
                return_type: Box::new(return_type.add_generic_prefix(prefix)),
            });
        } else if let ASTType::ASTGenericType(position, name, var_types) = self {
            if name.contains(":") {
                panic!("generic has already been prefixed");
            }
            return ASTType::ASTGenericType(
                position,
                format!("{prefix}:{name}"),
                var_types
                    .into_iter()
                    .map(|it| it.add_generic_prefix(prefix))
                    .collect(),
            );
        } else if let ASTType::ASTCustomType {
            name,
            param_types,
            position,
        } = self
        {
            return ASTType::ASTCustomType {
                name,
                param_types: param_types
                    .into_iter()
                    .map(|it| it.add_generic_prefix(prefix))
                    .collect(),
                position,
            };
        }

        self
    }

    pub fn remove_generic_prefix(self) -> Self {
        if let ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
            parameters,
            return_type,
        }) = self
        {
            ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                parameters: parameters
                    .into_iter()
                    .map(|it| it.remove_generic_prefix())
                    .collect(),
                return_type: Box::new(return_type.remove_generic_prefix()),
            })
        } else if let ASTType::ASTGenericType(ref position, ref name, ref var_types) = self {
            if let Some(original_generic) = Self::get_original_generic(name) {
                ASTType::ASTGenericType(
                    position.clone(),
                    original_generic.to_owned(),
                    var_types
                        .into_iter()
                        .map(|it| it.clone().remove_generic_prefix())
                        .collect(),
                )
            } else {
                self
            }
        } else if let ASTType::ASTCustomType {
            name,
            param_types,
            position,
        } = self
        {
            ASTType::ASTCustomType {
                name,
                param_types: param_types
                    .into_iter()
                    .map(|it| it.remove_generic_prefix())
                    .collect(),
                position,
            }
        } else {
            self
        }
    }

    pub fn get_original_generic(name: &str) -> Option<&str> {
        name.find(':').map(|i| name.split_at(i + 1).1)
    }

    pub fn remove_generic_prefix_from_str(name: &str) -> &str {
        Self::get_original_generic(name).unwrap_or(name)
    }

    pub fn generics(&self) -> HashSet<String> {
        let mut result = HashSet::new();
        match self {
            ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
                parameters,
                return_type,
            }) => {
                for p in parameters.iter() {
                    result.extend(p.generics());
                }
                result.extend(return_type.generics());
            }
            ASTType::ASTGenericType(_, name, var_types) => {
                result.insert(name.clone());
                for t in var_types {
                    result.extend(t.generics());
                }
            }
            ASTType::ASTCustomType {
                name: _,
                param_types,
                position: _,
            } => {
                for t in param_types.iter() {
                    result.extend(t.generics());
                }
            }
            _ => {}
        }
        result
    }
}

impl Display for ASTType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTType::ASTBuiltinType(kind) => match kind {
                ASTBuiltinTypeKind::ASTStringType => f.write_str("str"),
                ASTBuiltinTypeKind::ASTIntegerType => f.write_str("int"),
                ASTBuiltinTypeKind::ASTBooleanType => f.write_str("bool"),
                ASTBuiltinTypeKind::ASTCharType => f.write_str("char"),
                ASTBuiltinTypeKind::ASTFloatType => f.write_str("float"),
                ASTBuiltinTypeKind::ASTLambdaType {
                    parameters,
                    return_type,
                } => {
                    let pars: Vec<String> = parameters.iter().map(|it| format!("{it}")).collect();

                    f.write_str(&format!(
                        "fn ({}) -> {}",
                        pars.join(","),
                        return_type.deref()
                    ))
                }
            },
            ASTType::ASTGenericType(_, name, var_types) => {
                f.write_str(name)?;
                if !var_types.is_empty() {
                    f.write_str("<")?;
                    let mut i = 0;
                    for t in var_types {
                        if i > 0 {
                            f.write_str(",")?;
                        }
                        write!(f, "{t}")?;
                        i += 1;
                    }
                    f.write_str(">")
                } else {
                    Ok(())
                }
            }
            ASTType::ASTCustomType {
                name,
                param_types,
                position: _,
            } => {
                let pars: Vec<String> = param_types.iter().map(|it| format!("{it}")).collect();

                if pars.is_empty() {
                    f.write_str(name)
                } else {
                    f.write_str(&format!("{name}<{}>", pars.join(",")))
                }
            }
            ASTType::ASTUnitType => f.write_str("()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTParameterDef {
    pub name: String,
    pub ast_type: ASTType,
    pub position: ASTPosition,
}

impl Display for ASTParameterDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}: {}", self.name, self.ast_type))
    }
}

impl ASTParameterDef {
    pub fn new(name: &str, ast_type: ASTType, position: ASTPosition) -> ASTParameterDef {
        ASTParameterDef {
            name: name.into(),
            ast_type,
            position,
        }
    }

    pub fn fix_generics(self, prefix: &dyn Display) -> Self {
        let mut result = self;
        result.ast_type = result.ast_type.add_generic_prefix(prefix);
        result
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTStructPropertyDef {
    pub name: String,
    pub ast_type: ASTType,
    pub position: ASTPosition,
}

impl Display for ASTStructPropertyDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}: {}", self.name, self.ast_type))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionCall {
    function_name: String,
    parameters: Vec<ASTExpression>,
    position: ASTPosition,
    generics: Vec<ASTType>,
    target: Option<String>,
    is_macro: bool,
}

impl ASTFunctionCall {
    pub fn new(
        function_name: String,
        parameters: Vec<ASTExpression>,
        position: ASTPosition,
        generics: Vec<ASTType>,
        target: Option<String>,
        is_macro: bool,
    ) -> Self {
        Self {
            function_name,
            parameters,
            position,
            generics,
            target,
            is_macro,
        }
    }

    pub fn function_name(&self) -> &String {
        &self.function_name
    }

    pub fn parameters(&self) -> &Vec<ASTExpression> {
        &self.parameters
    }

    pub fn position(&self) -> &ASTPosition {
        &self.position
    }

    pub fn generics(&self) -> &Vec<ASTType> {
        &self.generics
    }

    // TODO I don't like it
    pub fn push_parameter(&mut self, expr: ASTExpression) {
        self.parameters.push(expr);
    }

    pub fn target(&self) -> &Option<String> {
        &self.target
    }

    pub fn is_macro(&self) -> bool {
        self.is_macro
    }
}

impl Display for ASTFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars: Vec<String> = self.parameters.iter().map(|it| format!("{}", it)).collect();

        f.write_str(&format!("{}({})", self.function_name, pars.join(",")))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ASTValue {
    ASTStringValue(String),
    ASTBooleanValue(bool),
    ASTIntegerValue(i64),
    ASTCharValue(String),
    ASTFloatValue(f64),
}

impl Display for ASTValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTValue::ASTStringValue(s) => f.write_str(&format!("\"{s}\"")),
            ASTValue::ASTBooleanValue(b) => f.write_str(&format!("{b}")),
            ASTValue::ASTIntegerValue(n) => f.write_str(&format!("{n}")),
            ASTValue::ASTFloatValue(n) => f.write_str(&format!("{n}")),
            ASTValue::ASTCharValue(c) => f.write_str(&format!("'{c}'")),
        }
    }
}

impl ASTValue {
    pub fn to_type(&self) -> ASTType {
        match self {
            ASTValue::ASTBooleanValue(_) => {
                ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTBooleanType)
            }
            ASTValue::ASTIntegerValue(_) => {
                ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTIntegerType)
            }
            ASTValue::ASTCharValue(_) => ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTCharType),
            ASTValue::ASTFloatValue(_) => ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTFloatType),
            ASTValue::ASTStringValue(_) => {
                ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTStringType)
            }
        }
    }

    pub fn token_len(&self) -> usize {
        match self {
            ASTValue::ASTStringValue(s) => s.len() + 2,
            ASTValue::ASTBooleanValue(v) => {
                if *v {
                    4
                } else {
                    5
                }
            }
            ASTValue::ASTIntegerValue(n) => {
                // TODO it's not precise: 000100
                let mut result = n.abs().checked_ilog10().unwrap_or(0) as usize + 1;
                if *n < 0 {
                    result += 1;
                }
                result
            }
            ASTValue::ASTCharValue(s) => s.len() + 2,
            ASTValue::ASTFloatValue(n) => format!("{n}").len(), // TODO it's slow and not precise: 000.100
        }
    }
}

// TODO can we do partialeq? It depends on ASTPOsition
#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpression {
    ASTFunctionCallExpression(ASTFunctionCall),
    ASTValueRefExpression(String, ASTPosition),
    ASTValueExpression(ASTValue, ASTPosition),
    ASTLambdaExpression(ASTLambdaDef),
}

impl ASTExpression {
    pub fn position(&self) -> &ASTPosition {
        match self {
            ASTExpression::ASTFunctionCallExpression(call) => &call.position,
            ASTExpression::ASTValueRefExpression(_, position) => position,
            ASTExpression::ASTValueExpression(_, position) => position,
            ASTExpression::ASTLambdaExpression(def) => &def.position,
        }
    }
}

impl Display for ASTExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTExpression::ASTFunctionCallExpression(call) => {
                /*let pars: Vec<String> =
                    call.parameters.iter().map(|it| format!("{}", it)).collect();


                f.write_str(&format!("{}({})", call.function_name, pars.join(",")))

                 */
                f.write_str(&format!("{call}"))
            }
            ASTExpression::ASTValueRefExpression(name, _) => f.write_str(name),
            ASTExpression::ASTValueExpression(val_type, _) => match val_type {
                ASTValue::ASTStringValue(s) => f.write_str(&format!("\"{s}\"")),
                ASTValue::ASTBooleanValue(b) => f.write_str(&format!("{b}")),
                ASTValue::ASTIntegerValue(n) => f.write_str(&format!("{n}")),
                ASTValue::ASTFloatValue(n) => f.write_str(&format!("{n}")),
                ASTValue::ASTCharValue(c) => f.write_str(&format!("'{c}'")),
            },
            ASTExpression::ASTLambdaExpression(lambda) => f.write_str(&format!("{lambda}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTStatement {
    ASTExpressionStatement(ASTExpression),
    ASTLetStatement(String, ASTExpression, ASTPosition),
    ASTConstStatement(String, ASTExpression, ASTPosition, ASTModifiers),
}

impl ASTStatement {
    pub fn position(&self) -> &ASTPosition {
        match self {
            ASTStatement::ASTExpressionStatement(expr) => expr.position(),
            ASTStatement::ASTLetStatement(_, _, position) => position,
            ASTStatement::ASTConstStatement(_, _, position, _) => position,
        }
    }
}

impl Display for ASTStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTStatement::ASTExpressionStatement(e) => f.write_str(&format!("{e};\n")),
            ASTStatement::ASTLetStatement(name, e, _) => {
                let keyword = "let";
                f.write_str(&format!("{keyword} {name} = {e};\n"))
            }
            ASTStatement::ASTConstStatement(name, e, _, modifiers) => {
                let keyword = if modifiers.public {
                    "pub const"
                } else {
                    "const"
                };
                f.write_str(&format!("{keyword} {name} = {e};\n"))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTModifiers {
    pub public: bool,
}

impl ASTModifiers {
    pub fn public() -> Self {
        Self { public: true }
    }

    pub fn private() -> Self {
        Self { public: false }
    }
}

#[derive(Debug, Clone)]
pub struct ASTModule {
    pub body: Vec<ASTStatement>,
    pub functions: Vec<ASTFunctionDef>,
    pub enums: Vec<ASTEnumDef>,
    pub structs: Vec<ASTStructDef>,
    pub types: Vec<ASTTypeDef>,
}

impl ASTModule {
    pub fn empty() -> Self {
        ASTModule {
            body: vec![],
            functions: vec![],
            enums: vec![],
            structs: vec![],
            types: vec![],
        }
    }

    pub fn add_function(&mut self, function_def: ASTFunctionDef) {
        self.functions.push(function_def);
    }

    pub fn add(&mut self, mut module: ASTModule) {
        self.body.append(&mut module.body);
        self.functions.append(&mut module.functions);
        self.enums.append(&mut module.enums);
        self.structs.append(&mut module.structs);
        self.types.extend(module.types);
    }

    pub fn print(&self) {
        for s in self.structs.iter() {
            println!("{s}");
        }

        for e in self.enums.iter() {
            println!("{e}");
        }

        for t in self.types.iter() {
            println!("{t}");
        }

        for s in self.body.iter() {
            println!("{s}");
        }

        for f in self.functions.iter() {
            println!("{f}");
        }
    }
}

impl Display for ASTModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for s in self.structs.iter() {
            f.write_str(&format!("{s}"))?;
        }
        for e in self.enums.iter() {
            f.write_str(&format!("{e}"))?;
        }
        for t in self.types.iter() {
            f.write_str(&format!("{t}"))?;
        }
        for s in self.body.iter() {
            f.write_str(&format!("{s}"))?;
        }
        for function in self.functions.iter() {
            f.write_str(&format!("{function} {{\n"))?;
            match &function.body {
                ASTFunctionBody::RASMBody(body) => {
                    for s in body.iter() {
                        f.write_str(&format!("  {s}"))?;
                    }
                }
                ASTFunctionBody::NativeBody(body) => f.write_str(&body)?,
            }
            f.write_str("}\n\n")?;
        }
        Ok(())
    }
}

pub trait CustomTypeDef: Display {
    fn name(&self) -> &str;
    fn modifiers(&self) -> &ASTModifiers;
    fn position(&self) -> &ASTPosition;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTEnumDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub variants: Vec<ASTEnumVariantDef>,
    pub position: ASTPosition,
    pub modifiers: ASTModifiers,
}

impl CustomTypeDef for ASTEnumDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn position(&self) -> &ASTPosition {
        &self.position
    }
}

impl Display for ASTEnumDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let variants = self
            .variants
            .iter()
            .map(|it| format!("  {it}"))
            .collect::<Vec<_>>()
            .join("\n");
        f.write_str(&format!("enum {} {{\n{variants}\n}}\n\n", self.name))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTEnumVariantDef {
    pub name: String,
    pub parameters: Vec<ASTParameterDef>,
    pub position: ASTPosition,
}

impl Display for ASTEnumVariantDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .parameters
            .iter()
            .map(|it| format!("{it}"))
            .collect::<Vec<String>>()
            .join(",");
        f.write_str(&format!("{}({})", self.name, pars))
    }
}

#[derive(Debug, Clone)]
pub struct ASTStructDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub properties: Vec<ASTStructPropertyDef>,
    pub position: ASTPosition,
    pub modifiers: ASTModifiers,
    pub attribute_macros: Vec<ASTFunctionCall>,
}

impl CustomTypeDef for ASTStructDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn position(&self) -> &ASTPosition {
        &self.position
    }
}

impl Display for ASTStructDef {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTTypeDef {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub body: String,
    pub position: ASTPosition,
    pub modifiers: ASTModifiers,
}

impl CustomTypeDef for ASTTypeDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn modifiers(&self) -> &ASTModifiers {
        &self.modifiers
    }

    fn position(&self) -> &ASTPosition {
        &self.position
    }
}

impl Display for ASTTypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("type {}", self.name))
    }
}

pub fn lambda(return_type: ASTType) -> ASTType {
    ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
        parameters: Vec::new(),
        return_type: Box::new(return_type),
    })
}

pub fn lambda_unit() -> ASTType {
    lambda(ASTType::ASTUnitType)
}

#[cfg(test)]
mod tests {

    use crate::parser::ast::{
        ASTBuiltinTypeKind, ASTFunctionBody, ASTFunctionDef, ASTModifiers, ASTParameterDef,
        ASTPosition, ASTType,
    };

    #[test]
    fn display_custom_type() {
        let inner_type = ASTType::ASTCustomType {
            name: "Option".to_owned(),
            param_types: vec![ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTStringType)],
            position: ASTPosition::none(),
        };

        let ast_type = ASTType::ASTCustomType {
            name: "List".to_owned(),
            param_types: vec![inner_type],
            position: ASTPosition::none(),
        };
        assert_eq!(format!("{ast_type}"), "List<Option<str>>");
    }

    #[test]
    fn display_function_def() {
        let inner_type = ASTType::ASTCustomType {
            name: "Option".to_owned(),
            param_types: vec![ASTType::ASTGenericType(
                ASTPosition::none(),
                "T".to_string(),
                Vec::new(),
            )],
            position: ASTPosition::none(),
        };

        let ast_type = ASTType::ASTCustomType {
            name: "List".to_owned(),
            param_types: vec![inner_type],
            position: ASTPosition::none(),
        };

        let def = ASTFunctionDef {
            name: "aFun".to_string(),
            parameters: vec![ASTParameterDef {
                name: "aPar".to_string(),
                ast_type,
                position: ASTPosition::none(),
            }],
            return_type: ASTType::ASTGenericType(ASTPosition::none(), "T".to_string(), Vec::new()),
            body: ASTFunctionBody::RASMBody(vec![]),
            generic_types: vec!["T".to_string()],
            position: ASTPosition::none(),
            modifiers: ASTModifiers::private(),
            target: None,
        };

        assert_eq!(format!("{def}"), "fn aFun<T>(aPar: List<Option<T>>) -> T");
    }

    #[test]
    fn display_type_class() {
        let t = ASTType::ASTGenericType(
            ASTPosition::none(),
            "M".to_owned(),
            vec![ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTStringType)],
        );

        assert_eq!("M<str>", format!("{t}"));
    }

    #[test]
    fn position_order() {
        let b = ASTPosition::new(1, 2);
        let a = ASTPosition::new(1, 1);

        assert!(a.id > b.id);
        assert!(a.cmp(&b).is_lt());
    }
}
