use linked_hash_map::LinkedHashMap;
use crate::codegen::backend::Backend;
use crate::codegen::LambdaSpace;
use crate::parser::ast::{ASTParameterDef, ASTTypeRef};

pub struct FunctionCallParameters<'a> {
    parameters: Vec<ASTParameterDef>,
    to_remove_from_stack: usize,
    before: String,
    parameters_values: LinkedHashMap<String, String>,
    backend: &'a dyn Backend,
    inline: bool,
    i: usize,
    i_for_context: usize,
    context_parameters_offset: usize
}

impl<'a> FunctionCallParameters<'a> {
    pub fn new(backend: &'a dyn Backend, parameters: Vec<ASTParameterDef>, inline: bool, context_parameters_offset: usize) -> Self {
        Self { to_remove_from_stack: 0, before: String::new(), parameters_values: LinkedHashMap::new(), backend, inline, parameters, i: 0, i_for_context: context_parameters_offset, context_parameters_offset }
    }

    pub fn add_string_literal(&mut self, param_name: &str, label: String, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), label);
        } else {
            Self::add(&mut self.before, &format!("    push    {}", label), comment);
            self.to_remove_from_stack += 1;
        }
    }

    pub fn add_number(&mut self, param_name: &str, n: &i32, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), format!("{}", n));
        } else {
            Self::add(&mut self.before, &format!("    push    {}", n), comment);
            self.to_remove_from_stack += 1;
        }
    }

    pub fn add_function_call(&mut self, comment: Option<&str>) {
        Self::add(&mut self.before, &"    push    eax".to_string(), comment);
        self.to_remove_from_stack += 1;
    }

    pub fn add_function_ref(&mut self, name: String, comment: Option<&str>) {
        Self::add(&mut self.before, &format!("    push    {}", name), comment);
        self.to_remove_from_stack += 1;
    }

    pub fn add_var(&mut self, original_param_name: String, par: &ASTParameterDef, lambda_space: Option<&LambdaSpace>, index: usize, comment: Option<&str>, indent: usize) -> usize{
        println!("{}adding var {}, i {}, index {}, from context {}", " ".repeat(indent * 4), original_param_name, self.i, index, par.from_context);

        let offset = if par.from_context {
            index
        } else {
            index
        };

        if par.from_context {
            if lambda_space.is_some() && lambda_space.unwrap().get(&par.name).is_some() {
                self.add_lambda_param_from_lambda_space(original_param_name, &par.name, &par.type_ref, lambda_space.unwrap(), comment, indent);
            } else {
                panic!("Inserting var {}, i {}, from context {} lambda_space: {:?}", par.name, self.i, par.from_context, lambda_space);
                //self.add_var_(&par.name, &par.type_ref, comment);
            }
            self.i_for_context += 1;
        } else {
            self.add_var_(original_param_name, &par.type_ref, offset, comment);
            self.i += 1;
        }
        offset
    }

    fn add_var_(&mut self, original_param_name: String, type_ref: &ASTTypeRef, index: usize, comment: Option<&str>) {
        let word_len = self.backend.word_len() as usize;

        if self.inline {
            self.parameters_values.insert(original_param_name, format!("[{}+{}+{}]", self.backend.stack_base_pointer(), word_len, (index + 1) * word_len));
        } else {
            let type_size = self.backend.type_size(type_ref).unwrap_or_else(|| panic!("Unsupported type size: {:?}", type_ref));

            Self::add(&mut self.before, &format!("    push     {} [{}+{}+{}]", type_size, self.backend.stack_base_pointer(), word_len, (index + 1) * word_len), comment);
            self.to_remove_from_stack += 1;
        }
    }

    pub fn add_lambda_param_from_lambda_space(&mut self, original_param_name: String, param_name: &str, type_ref: &ASTTypeRef, lambda_space: &LambdaSpace, comment: Option<&str>, indent: usize) {
        let parameter_value_address = lambda_space.get(param_name).unwrap();
        println!("{}add_lambda_param_from_lambda_space, param {}, address {}", " ".repeat(indent * 4), param_name, parameter_value_address);

        if self.inline {
            println!("{}inline", " ".repeat((indent + 1) * 4));
            self.parameters_values.insert(original_param_name, format!("[{}]", parameter_value_address));
        } else {
            println!("{}NOT inline", " ".repeat((indent + 1) * 4));
            let type_size = self.backend.type_size(type_ref).unwrap_or_else(|| panic!("Unsupported type size: {:?}", type_ref));

            Self::add(&mut self.before, &format!("    push     {} [{}]", type_size, parameter_value_address), comment);
            self.to_remove_from_stack += 1;
        }
    }

    pub fn resolve_asm_parameters(&self, body: &str, to_remove_from_stack: usize, ident: usize) -> String {
        let mut result = body.to_string();
        let mut i = 0;
        let word_len = self.backend.word_len() as i32;
        for par in self.parameters.iter() {
            if let Some(par_value) = self.parameters_values.get(&par.name) {
                println!("{}found parameter {}, value: {}", " ".repeat(ident * 4), par.name, par_value);
                result = result.replace(&format!("${}", par.name), par_value);
                continue;
            }

            println!("{}cannot find parameter {}, parameters_values {:?}", " ".repeat(ident * 4), par.name, self.parameters_values);

            let relative_address =
                if self.inline {
                    (i as i32 - self.to_remove_from_stack as i32 - to_remove_from_stack as i32) * word_len
                } else {
                    (i + 2) * self.backend.word_len() as i32
                };
            let register = self.backend.stack_base_pointer();
            let address = if relative_address < 0 {
                format!("[{}-{}]", register, -relative_address)
            } else {
                format!("[{}+{}]", register, relative_address)
            };
            result = result.replace(&format!("${}", par.name), &address);
            i += 1;
        }
        result
    }

    pub fn to_remove_from_stack(&self) -> usize {
        self.to_remove_from_stack
    }

    pub fn before(&self) -> &String {
        &self.before
    }

    pub fn push(&mut self, s: &str) {
        self.before.push_str(s);
    }

    fn add(dest: &mut String, code: &str, comment: Option<&str>) {
        if code.is_empty() {
            let string = " ".repeat(50);
            dest.push_str(&string);
        } else {
            let s = format!("{:width$}", code, width = 50);
            assert_eq!(s.len(), 50);
            dest.push_str(&s);
        }

        dest.push_str(&format!("; {}\n", comment.unwrap_or("")));
    }
}