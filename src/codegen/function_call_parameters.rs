use std::collections::HashMap;
use crate::parser::ast::ASTFunctionDef;

pub struct FunctionCallParameters {
    call_function_def: ASTFunctionDef,
    to_remove_from_stack: usize,
    before: String,
    parameters_values: HashMap<String, String>
}

impl FunctionCallParameters {

    pub fn new(call_function_def: ASTFunctionDef) -> Self {
        Self {call_function_def, to_remove_from_stack: 0, before: String::new(), parameters_values: HashMap::new()}
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

    pub fn add_var(&mut self, index: usize) {
        /*if self.call_function_def.inline {
            self.parameters_values.insert(param_name.clone(), format!("{}", label));
        } else {

         */
            Self::add(&mut self.before, &format!("    push     dword [ebp+4+{}]", (index + 1) * 4));
            self.to_remove_from_stack += 1;
        //}
    }

    pub fn resolve_asm_parameters(&self, function_def: &ASTFunctionDef, body: &str, inside_function: bool, to_remove_from_stack: usize) -> String {
        let mut result = body.to_string();
        let mut i = 0;
        for par in function_def.parameters.iter() {
            if let Some(par_value) = self.parameters_values.get(&par.name) {
                result = result.replace(&format!("${}", par.name), par_value);
                continue;
            }
            let relative_address =
                if function_def.inline {
                    if inside_function {
                        (i as i32 - self.to_remove_from_stack as i32 - to_remove_from_stack as i32) * 4
                    } else {
                        i * 4
                    }
                } else {
                    (i + 2) * 4
                };
            let register = if inside_function {
                "ebp"
            } else {
                "esp"
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