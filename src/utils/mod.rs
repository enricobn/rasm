use std::fmt;
use std::fmt::Display;
use std::slice::Iter;

#[macro_use]
pub mod debug_indent;

pub fn format_option<T: Display>(o: &Option<T>) -> String {
    match o {
        None => "None".into(),
        Some(v) => format!("Some({v})"),
    }
}

pub fn format_option_option<T: Display>(o: &Option<Option<T>>) -> String {
    match o {
        None => "None".into(),
        Some(v) => format!("Some({})", format_option(v)),
    }
}

pub struct OptionDisplay<'a, T: 'a>(pub &'a Option<T>);

impl<'a, T: fmt::Display + 'a> fmt::Display for OptionDisplay<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Some(v) => write!(f, "Some({v})"),
            None => write!(f, "None"),
        }
    }
}

// from https://stackoverflow.com/questions/33759072/why-doesnt-vect-implement-the-display-trait
pub struct SliceDisplay<'a, T: 'a>(pub &'a [T]);

impl<'a, T: fmt::Display + 'a> fmt::Display for SliceDisplay<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for item in self.0 {
            if !first {
                write!(f, ", {}", item)?;
            } else {
                write!(f, "{}", item)?;
            }
            first = false;
        }
        Ok(())
    }
}

pub fn find_one<T, P>(iter: Iter<T>, predicate: P) -> Option<&T>
where
    P: FnMut(&&T) -> bool,
{
    let filter = iter.filter(predicate).collect::<Vec<_>>();
    let len = filter.len();
    if len == 0 || len > 1 {
        return None;
    }
    filter.get(0).cloned()
}

#[cfg(test)]
pub mod tests {
    use crate::codegen::text_macro::TypeDefProvider;
    use crate::parser::ast::ASTType;
    use crate::type_check::typed_ast::{
        ASTTypedEnumDef, ASTTypedStructDef, ASTTypedType, ASTTypedTypeDef,
    };

    #[derive(Debug)]
    pub struct DummyTypeDefProvider {}

    impl TypeDefProvider for DummyTypeDefProvider {
        fn get_enum_def_by_name(&self, _name: &str) -> Option<&ASTTypedEnumDef> {
            None
        }

        fn get_struct_def_by_name(&self, _name: &str) -> Option<&ASTTypedStructDef> {
            None
        }

        fn get_type_def_by_name(&self, _name: &str) -> Option<&ASTTypedTypeDef> {
            None
        }

        fn get_enum_def_like_name(&self, _name: &str) -> Option<&ASTTypedEnumDef> {
            None
        }

        fn get_struct_def_like_name(&self, _name: &str) -> Option<&ASTTypedStructDef> {
            None
        }

        fn get_type_def_like_name(&self, _name: &str) -> Option<&ASTTypedTypeDef> {
            None
        }

        fn get_type_from_typed_type(&self, typed_type_to_find: &ASTTypedType) -> Option<ASTType> {
            None
        }

        fn get_ast_typed_type_from_type_name(&self, name: &str) -> Option<ASTTypedType> {
            None
        }

        fn get_ast_typed_type_from_ast_type(&self, ast_type: &ASTType) -> Option<ASTTypedType> {
            None
        }

        fn get_typed_type_def_from_type_name(&self, type_to_find: &str) -> Option<ASTTypedTypeDef> {
            None
        }

        fn name(&self) -> String {
            "DummyTypeDefProvider".to_owned()
        }
    }

    impl DummyTypeDefProvider {
        pub fn new() -> Self {
            Self {}
        }
    }
}
