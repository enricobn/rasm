/*
 *     RASM compiler.
 *     Copyright (C) 2022-2023  Enrico Benedetti
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

use itertools::Itertools;
use linked_hash_map::LinkedHashMap;

use crate::codegen::c::typed_function_creator_c::TypedFunctionsCreatorC;
use crate::codegen::code_manipulator::CodeManipulator;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::statics::Statics;
use crate::codegen::text_macro::RefType;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::TypedValKind;
use crate::enh_type_check::typed_ast::{ASTTypedType, BuiltinTypedTypeKind};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering};

use super::code_gen_c::{CCodeManipulator, CodeGenC};

pub struct CConsts {
    pub vec: Vec<(String, String, Option<String>)>,
}

impl CConsts {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    fn add(&mut self, name: String, def: String, value: Option<String>) {
        self.vec.push((name, def, value))
    }

    pub fn add_to_statics(statics: &mut Statics, name: String, def: String, value: Option<String>) {
        if let Some(c) = statics.any_mut::<CConsts>() {
            c.add(name, def, value);
        } else {
            let mut c = CConsts::new();
            c.add(name, def, value);
            statics.add_any(c);
        }
    }
}

pub struct CInclude {
    vec: Vec<String>,
}

impl CInclude {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    pub fn add(&mut self, c: String) {
        self.vec.push(c)
    }

    pub fn unique(&self) -> Vec<String> {
        let mut includes = self.vec.clone();
        includes.sort();
        includes.dedup();
        includes
    }

    pub fn add_to_statics(statics: &mut Statics, inc_string: String) {
        if let Some(i) = statics.any_mut::<CInclude>() {
            i.add(inc_string);
        } else {
            let mut i = CInclude::new();
            i.add(inc_string);
            statics.add_any(i);
        }
    }
}

static ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Eq, Clone)]
pub struct CLambda {
    pub name: String,
    pub args: Vec<ASTTypedType>,
    pub return_type: ASTTypedType,
}

impl PartialEq for CLambda {
    fn eq(&self, other: &Self) -> bool {
        self.same(&other.args, &other.return_type)
    }
}

impl Hash for CLambda {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let args = self
            .args
            .iter()
            .map(|it| CodeGenC::real_type_to_string(it))
            .join(",");
        args.hash(state);
        let rt = CodeGenC::real_type_to_string(&self.return_type);
        rt.hash(state);
    }
}

impl CLambda {
    pub fn new(args: Vec<ASTTypedType>, return_type: ASTTypedType) -> Self {
        let name = format!("Lambda_{}", ID.fetch_add(1, Ordering::SeqCst));
        Self {
            args,
            return_type,
            name,
        }
    }

    fn same(&self, args: &Vec<ASTTypedType>, return_type: &ASTTypedType) -> bool {
        let self_args = self
            .args
            .iter()
            .map(|it| CodeGenC::real_type_to_string(it))
            .join(",");
        let other_args = args
            .iter()
            .map(|it| CodeGenC::real_type_to_string(it))
            .join(",");
        self_args == other_args
            && CodeGenC::real_type_to_string(&self.return_type)
                == CodeGenC::real_type_to_string(return_type)
    }
}

pub struct CLambdas {
    pub lambdas: LinkedHashMap<CLambda, CLambda>,
}

impl CLambdas {
    pub fn new() -> Self {
        Self {
            lambdas: LinkedHashMap::new(),
        }
    }

    fn add(&mut self, clambda: CLambda) {
        self.lambdas.insert(clambda.clone(), clambda);
    }

    pub fn find_name_in_statics(
        statics: &Statics,
        args: &Vec<ASTTypedType>,
        return_type: &ASTTypedType,
    ) -> Option<String> {
        statics
            .any::<CLambdas>()
            .and_then(|it| it.find_name(args, return_type))
    }

    fn find_name(&self, args: &Vec<ASTTypedType>, return_type: &ASTTypedType) -> Option<String> {
        self.lambdas
            .values()
            .find(|it| it.same(args, return_type))
            .map(|it| it.name.clone())
    }

    pub fn add_to_statics(statics: &mut Statics, c_lambda: CLambda) -> String {
        if let Some(l) = statics.any_mut::<CLambdas>() {
            if let Some(found) = l.lambdas.get(&c_lambda) {
                found.name.clone()
            } else {
                let name = c_lambda.name.clone();
                l.add(c_lambda);
                name
            }
        } else {
            let mut lambdas = CLambdas::new();
            let name = c_lambda.name.clone();
            lambdas.add(c_lambda);
            statics.add_any(lambdas);
            name
        }
    }

    pub fn add_to_statics_if_lambda(typed_type: &ASTTypedType, statics: &mut Statics) {
        if let ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
            parameters,
            return_type,
        }) = typed_type
        {
            CLambdas::add_to_statics(
                statics,
                CLambda::new(parameters.clone(), return_type.as_ref().clone()),
            );
        }
    }
}

pub struct CFunctionsDeclarations {
    pub vec: Vec<String>,
}

impl CFunctionsDeclarations {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }
    pub fn add_to_statics(statics: &mut Statics, def: String) {
        if let Some(declarations) = statics.any_mut::<CFunctionsDeclarations>() {
            declarations.vec.push(def)
        } else {
            let mut declarations = CFunctionsDeclarations::new();
            declarations.vec.push(def);
            statics.add_any(declarations);
        }
    }
}

pub struct CStruct {
    pub name: String,
    map: LinkedHashMap<String, String>,
}

impl CStruct {
    fn new(name: String, map: LinkedHashMap<String, String>) -> Self {
        Self { name, map }
    }

    /*
    pub fn add(&mut self, name: String, type_as_string: String) {
        if self.map.insert(name.clone(), type_as_string).is_some() {
            panic!("{name} already defined");
        }
    }
    */

    pub fn generate(&self, code_manipulator: &CCodeManipulator) -> String {
        let mut body = String::new();
        code_manipulator.add(&mut body, &format!("struct {} {{", self.name), None, false);
        for (name, type_as_string) in self.map.iter() {
            code_manipulator.add(&mut body, &format!("{type_as_string} {name} ;"), None, true);
        }
        code_manipulator.add(&mut body, "};", None, false);

        body
    }
}

pub struct CStructs {
    pub structs: Vec<CStruct>,
}

impl CStructs {
    fn new() -> Self {
        Self {
            structs: Vec::new(),
        }
    }

    pub fn add_struct_to_statics(
        statics: &mut Statics,
        name: String,
        map: LinkedHashMap<String, String>,
    ) {
        if let Some(structs) = statics.any_mut::<CStructs>() {
            structs.structs.push(CStruct::new(name, map));
        } else {
            let mut structs = CStructs::new();
            structs.structs.push(CStruct::new(name, map));
            statics.add_any(structs)
        }
    }

    pub fn add_lambda_space_to_statics(
        statics: &mut Statics,
        lambda_space: &LambdaSpace,
    ) -> String {
        let mut map = LinkedHashMap::new();

        for (name, kind) in lambda_space.iter() {
            map.insert(
                name.clone(),
                CodeGenC::real_type_to_string(&kind.typed_type()),
            );
        }

        if let Some(structs) = statics.any_mut::<CStructs>() {
            if let Some(s) = structs.structs.iter().find(|it| it.map == map) {
                return s.name.clone();
            }
        }

        let name = format!("LambdaSpace_{}", ID.fetch_add(1, Ordering::SeqCst));
        Self::add_struct_to_statics(statics, name.clone(), map);
        name
    }
}

pub struct CStrings {
    pub map: HashMap<String, String>,
}

impl CStrings {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn add(&mut self, value: String) -> &str {
        self.map
            .entry(value)
            .or_insert(format!("string_{}", ID.fetch_add(1, Ordering::SeqCst)))
    }

    pub fn add_to_statics(statics: &mut Statics, value: String) -> String {
        if let Some(c) = statics.any_mut::<CStrings>() {
            c.add(value).to_owned()
        } else {
            let mut c = CStrings::new();
            let result = c.add(value).to_owned();
            statics.add_any(c);
            result
        }
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct CLambdaAddRefDerefFunctionKey {
    function_type: RefType,
    lambda_name: String,
    optimize_lambda: bool,
    optimize_lambda_space: bool,
    lambda_refs: LinkedHashMap<String, TypedValKind>,
}

#[derive(Clone)]
pub struct CLamdaAddRefDerefFunctions {
    map: HashMap<CLambdaAddRefDerefFunctionKey, String>,
}

impl CLamdaAddRefDerefFunctions {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn add(
        &mut self,
        statics: &mut Statics,
        key: CLambdaAddRefDerefFunctionKey,
        lambda_space: &mut LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        typed_function_creator: &TypedFunctionsCreatorC,
    ) -> &str {
        self.map.entry(key.clone()).or_insert_with(|| {
            let function_def = typed_function_creator.create_lambda_free(
                &key.lambda_name,
                lambda_space,
                key.function_type,
                type_def_provider,
                statics,
                key.optimize_lambda,
                key.optimize_lambda_space,
            );
            let name = function_def.name.clone();
            lambda_space.add_ref_function(function_def);
            name
        })
    }

    pub fn add_to_statics(
        statics: &mut Statics,
        function_type: RefType,
        lambda_name: String,
        optimize_lambda: bool,
        optimize_lambda_space: bool,
        lambda_space: &mut LambdaSpace,
        type_def_provider: &dyn TypeDefProvider,
        typed_function_creator: &TypedFunctionsCreatorC,
    ) -> String {
        let key = CLambdaAddRefDerefFunctionKey {
            function_type,
            lambda_name,
            optimize_lambda,
            optimize_lambda_space,
            lambda_refs: lambda_space.values().clone(),
        };

        if statics.any::<CLamdaAddRefDerefFunctions>().is_none() {
            let c = CLamdaAddRefDerefFunctions::new();
            statics.add_any(c);
        }

        let c = statics.any::<CLamdaAddRefDerefFunctions>().unwrap();

        if c.map.contains_key(&key) {
            return c.map.get(&key).unwrap().clone();
        }

        let mut copy = c.clone();

        let result = copy
            .add(
                statics,
                key,
                lambda_space,
                type_def_provider,
                typed_function_creator,
            )
            .to_owned();

        statics.add_any(copy);

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::c::any::{CLambda, CLambdas};
    use crate::codegen::statics::Statics;
    use crate::enh_type_check::typed_ast::{ASTTypedType, BuiltinTypedTypeKind};

    #[test]
    fn unique_c_lambda() {
        let mut statics = Statics::new();

        let name = CLambdas::add_to_statics(
            &mut statics,
            CLambda::new(
                vec![ASTTypedType::Builtin(BuiltinTypedTypeKind::String)],
                ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
            ),
        );

        let new_name = CLambdas::add_to_statics(
            &mut statics,
            CLambda::new(
                vec![ASTTypedType::Builtin(BuiltinTypedTypeKind::String)],
                ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
            ),
        );

        assert_eq!(name, new_name);
    }

    #[test]
    fn not_unique_c_lambda() {
        let mut statics = Statics::new();

        let name = CLambdas::add_to_statics(
            &mut statics,
            CLambda::new(
                vec![ASTTypedType::Builtin(BuiltinTypedTypeKind::String)],
                ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
            ),
        );

        let new_name = CLambdas::add_to_statics(
            &mut statics,
            CLambda::new(
                vec![ASTTypedType::Builtin(BuiltinTypedTypeKind::Integer)],
                ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
            ),
        );

        assert_ne!(name, new_name);
    }
}
