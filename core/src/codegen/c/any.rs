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

use crate::codegen::statics::Statics;
use crate::type_check::typed_ast::ASTTypedType;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct CConsts {
    pub vec: Vec<String>,
}

impl CConsts {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    pub fn add(&mut self, c: String) {
        self.vec.push(c)
    }

    pub fn add_to_statics(statics: &mut Statics, const_def: String) {
        if let Some(c) = statics.any_mut::<CConsts>() {
            c.add(const_def);
        } else {
            let mut c = CConsts::new();
            c.add(const_def);
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

static LAMBDA_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Eq)]
pub struct CLambda {
    pub name: String,
    pub args: Vec<ASTTypedType>,
    pub return_type: ASTTypedType,
}

impl PartialEq for CLambda {
    fn eq(&self, other: &Self) -> bool {
        self.args == other.args && self.return_type == other.return_type
    }
}

impl Hash for CLambda {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.args.hash(state);
        self.return_type.hash(state);
    }
}

impl CLambda {
    pub fn new(args: Vec<ASTTypedType>, return_type: ASTTypedType) -> Self {
        let name = format!("Lambda_{}", LAMBDA_ID.fetch_add(1, Ordering::SeqCst));
        Self {
            args,
            return_type,
            name,
        }
    }
}

pub struct CLambdas {
    pub lambdas: HashSet<CLambda>,
}

impl CLambdas {
    pub fn new() -> Self {
        Self {
            lambdas: HashSet::new(),
        }
    }

    pub fn add(&mut self, clambda: CLambda) {
        self.lambdas.insert(clambda);
    }

    pub fn find_name(&self, args: &Vec<ASTTypedType>, return_type: &ASTTypedType) -> Option<&str> {
        self.lambdas
            .iter()
            .find(|it| &it.args == args && &it.return_type == return_type)
            .map(|it| it.name.as_str())
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

#[cfg(test)]
mod tests {
    use crate::codegen::c::any::{CLambda, CLambdas};
    use crate::codegen::statics::Statics;
    use crate::type_check::typed_ast::{ASTTypedType, BuiltinTypedTypeKind};

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
                vec![ASTTypedType::Builtin(BuiltinTypedTypeKind::Char)],
                ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
            ),
        );

        assert_ne!(name, new_name);
    }
}
