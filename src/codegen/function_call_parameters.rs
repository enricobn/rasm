use std::collections::HashMap;
use crate::codegen::backend::Backend;
use crate::parser::ast::{ASTFunctionDef, ASTTypeRef};

pub struct FunctionCallParameters<'a> {
    call_function_def: ASTFunctionDef,
    to_remove_from_stack: usize,
    before: String,
    parameters_values: HashMap<String, String>,
    backend: &'a dyn Backend
}

impl <'a> FunctionCallParameters<'a> {

    pub fn new(backend: &'a dyn Backend, call_function_def: ASTFunctionDef) -> Self {
        Self {call_function_def, to_remove_from_stack: 0, before: String::new(), parameters_values: HashMap::new(), backend}
    }

    pub fn add_string_literal(&mut self, param_name: &String, label: String) {
        if self.call_function_def.inline {
            self.parameters_values.insert(param_name.clone(), format!("{}", label));
        } else {
            Self::add(&mut self.before, &format!("    push    {}", label));
            self.to_remove_from_stack += 1;
        }
    }

    pub fn add_number(&mut self, param_name: &String, n: &i32) {
        if self.call_function_def.inline {
            self.parameters_values.insert(param_name.clone(), format!("{}", n));
        } else {
            Self::add(&mut self.before, &format!("    push    {}", n));
            self.to_remove_from_stack += 1;
        }
    }

    pub fn add_function_call(&mut self) {
        // TODO I must get the register that is returned, getting that from the function def of the called function
        Self::add(&mut self.before, &format!("    push    eax"));
        self.to_remove_from_stack += 1;
    }

    pub fn add_var(&mut self, param_name: &String, type_ref: &ASTTypeRef, index: usize) {
        let word_len = self.backend.word_len() as usize;

        if self.call_function_def.inline {
            self.parameters_values.insert(param_name.clone(), format!("[{}+{}+{}]", self.backend.stack_base_pointer(), word_len, (index + 1) * word_len));
        } else {
            let type_size = self.backend.type_size(type_ref).expect(
                &format!("Unsupported type size: {:?}", type_ref));

            Self::add(&mut self.before, &format!("    push     {} [{}+{}+{}]", type_size, self.backend.stack_base_pointer(), word_len, (index + 1) * word_len));
            self.to_remove_from_stack += 1;
        }
    }

    pub fn resolve_asm_parameters(&self, function_def: &ASTFunctionDef, body: &str, inside_function: bool, to_remove_from_stack: usize) -> String {
        let mut result = body.to_string();
        let mut i = 0;
        let word_len = self.backend.word_len() as i32;
        for par in function_def.parameters.iter() {
            if let Some(par_value) = self.parameters_values.get(&par.name) {
                result = result.replace(&format!("${}", par.name), par_value);
                continue;
            }
            let relative_address =
                if function_def.inline {
                    if inside_function {
                        (i as i32 - self.to_remove_from_stack as i32 - to_remove_from_stack as i32) * word_len
                    } else {
                        i * self.backend.word_len() as i32
                    }
                } else {
                    (i + 2) * self.backend.word_len() as i32
                };
            let register = if inside_function {
                self.backend.stack_base_pointer()
            } else {
                self.backend.stack_pointer()
            };
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

    pub fn push(&mut self, s: &String) {
        self.before.push_str(s);
    }

    fn add(dest: &mut String, code: &str) {
        if code.is_empty() {
            let string = " ".repeat(50);
            dest.push_str(&string);
        } else {
            let s = format!("{:width$}", code, width = 50);
            assert_eq!(s.len(), 50);
            dest.push_str(&s);
        }

        dest.push_str("; generated\n");
    }
}