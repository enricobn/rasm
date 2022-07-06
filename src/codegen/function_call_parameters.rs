use linked_hash_map::LinkedHashMap;
use log::debug;
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
    immediate: bool
}

impl<'a> FunctionCallParameters<'a> {

    ///
    ///
    /// # Arguments
    ///
    /// * `backend`:
    /// * `parameters`:
    /// * `inline`:
    /// * `immediate`: if true the result is not pushed on the stack, but moved to eax
    ///
    /// returns: FunctionCallParameters
    ///
    /// # Examples
    ///
    /// ```
    ///
    /// ```
    pub fn new(backend: &'a dyn Backend, parameters: Vec<ASTParameterDef>, inline: bool, immediate: bool) -> Self {
        Self { to_remove_from_stack: 0, before: String::new(), parameters_values: LinkedHashMap::new(), backend, inline, parameters, immediate }
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

    pub fn add_val(&mut self, original_param_name: String, par: &ASTParameterDef, index: usize, comment: Option<&str>, indent: usize) {
        debug!("{}adding val {}, index {}, from context {}", " ".repeat(indent * 4), original_param_name, index, par.from_context);
        self.add_val_(original_param_name, &par.type_ref, index, comment);
    }

    fn add_val_(&mut self, original_param_name: String, type_ref: &ASTTypeRef, index: usize, comment: Option<&str>) {
        let word_len = self.backend.word_len() as usize;

        if self.inline {
            self.parameters_values.insert(original_param_name, format!("[{}+{}+{}]", self.backend.stack_base_pointer(), word_len, (index + 1) * word_len));
        } else {
            let type_size = self.backend.type_size(type_ref).unwrap_or_else(|| panic!("Unsupported type size: {:?}", type_ref));

            if self.immediate {
                Self::add(&mut self.before, &format!("    mov {} eax, [{}+{}+{}]", type_size, self.backend.stack_base_pointer(), word_len, (index + 1) * word_len), comment);
            } else {
                Self::add(&mut self.before, &format!("    push     {} [{}+{}+{}]", type_size, self.backend.stack_base_pointer(), word_len, (index + 1) * word_len), comment);
            }
            self.to_remove_from_stack += 1;
        }
    }

    pub fn add_lambda_param_from_lambda_space(&mut self, param_name: &str, lambda_space: &LambdaSpace, comment: Option<&str>, indent: usize) {
        let lambda_space_index = lambda_space.get_index(param_name).unwrap();
        debug!("{}add_lambda_param_from_lambda_space, param {}, address {}", " ".repeat(indent * 4), param_name, lambda_space_index);

        let word_len = self.backend.word_len() as usize;
        let sbp = self.backend.stack_base_pointer();

        if self.inline {
            panic!("it should not happen since lambda cannot be inlined param_name: {}, comment: {:?}", param_name, comment);
            //self.parameters_values.insert(original_param_name, format!("[{}+{}+{}+{}]", sbp, word_len, (index + 1) * word_len, lambda_space_index * word_len));
        } else {
            Self::add(&mut self.before, "", comment);
            Self::add(&mut self.before, &format!("    mov     eax, [{}+{}]", sbp, word_len * 2), Some("The address to the lambda space"));
            Self::add(&mut self.before, &format!("    push   {} [eax + {}]", self.backend.pointer_size(), lambda_space_index * word_len), None);
            if self.immediate {
                Self::add(&mut self.before, "    pop eax", Some(&format!("immediate reference to val '{}'", param_name)));
            }
            self.to_remove_from_stack += 1;
        }
    }

    pub fn resolve_asm_parameters(&self, body: &str, to_remove_from_stack: usize, ident: usize) -> String {
        let mut result = body.to_string();
        let mut i = 0;
        let word_len = self.backend.word_len() as i32;
        for par in self.parameters.iter() {
            if let Some(par_value) = self.parameters_values.get(&par.name) {
                debug!("{}found parameter {}, value: {}", " ".repeat(ident * 4), par.name, par_value);
                result = result.replace(&format!("${}", par.name), par_value);
                continue;
            }

            debug!("{}cannot find parameter {}, parameters_values {:?}", " ".repeat(ident * 4), par.name, self.parameters_values);

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