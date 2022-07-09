use linked_hash_map::LinkedHashMap;
use log::debug;
use crate::codegen::backend::Backend;
use crate::codegen::{CodeGen, LambdaSpace, VarContext, VarKind};
use crate::parser::ast::{ASTFunctionDef, ASTParameterDef, ASTTypeRef};

pub struct FunctionCallParameters<'a> {
    parameters: Vec<ASTParameterDef>,
    to_remove_from_stack: usize,
    before: String,
    after: String,
    parameters_values: LinkedHashMap<String, String>,
    backend: &'a dyn Backend,
    inline: bool,
    immediate: bool,
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
        Self { to_remove_from_stack: 0, before: String::new(), after: String::new(), parameters_values: LinkedHashMap::new(), backend, inline, parameters, immediate }
    }

    pub fn add_string_literal(&mut self, param_name: &str, label: String, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), label);
        } else {
            CodeGen::add(&mut self.before, &format!("    mov {} [{} + {}], {}", self.backend.pointer_size(), self.backend.stack_pointer(),
                                                 self.to_remove_from_stack * self.backend.word_len() as usize, label), comment);
            /*
                        CodeGen::add(&mut self.before, &format!("    mov {} [{} - {}], {}", self.backend.pointer_size(), self.backend.stack_base_pointer(),
                                                             (self.parameters.len() - self.to_remove_from_stack) * self.backend.word_len() as usize, label), comment);

             */

            self.to_remove_from_stack += 1;
        }
    }

    pub fn add_number(&mut self, param_name: &str, n: &i32, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), format!("{}", n));
        } else {
            CodeGen::add(&mut self.before, &format!("    mov {} [{} + {}], {}", self.backend.word_size(), self.backend.stack_pointer(),
                                                 self.to_remove_from_stack * self.backend.word_len() as usize, n), comment);
            /*
                        CodeGen::add(&mut self.before, &format!("    mov {} [{} - {}], {}", self.backend.word_size(), self.backend.stack_base_pointer(),
                                                             (self.parameters.len() - self.to_remove_from_stack) * self.backend.word_len() as usize, n), comment);

             */

            self.to_remove_from_stack += 1;
        }
    }

    pub fn add_function_call(&mut self, comment: Option<&str>) {
        CodeGen::add(&mut self.before, &format!("    mov {} [{} + {}], eax", self.backend.word_size(), self.backend.stack_pointer(), self.to_remove_from_stack * self.backend.word_len() as usize), comment);
        //CodeGen::add(&mut self.before, &format!("    mov {} [{} - {}], eax", self.backend.word_size(), self.backend.stack_base_pointer(), (self.parameters.len() - self.to_remove_from_stack) * self.backend.word_len() as usize), comment);
        self.to_remove_from_stack += 1;
    }
    
    pub fn add_lambda(&mut self, def : &mut ASTFunctionDef, parent_lambda_space: Option<&LambdaSpace>, context: &VarContext, comment: Option<&str>) -> LambdaSpace {
        let mut lambda_space = LambdaSpace::new(context.clone());

        let num_of_params = context.iter().filter(|(_, kind)| {
            matches!(kind, VarKind::ParameterRef(_, _))
        }).count();

        let bp = self.backend.stack_base_pointer();
        let sp = self.backend.stack_pointer();
        let wl = self.backend.word_len() as usize;
        let pointer_size = self.backend.pointer_size();

        CodeGen::add(&mut self.before, &format!("    push {} ebx", pointer_size), None);
        CodeGen::add(&mut self.before, &format!("    push {} ecx", pointer_size), None);
        CodeGen::add(&mut self.before, &format!("    push {}", (num_of_params + 1) * wl), None);
        CodeGen::add(&mut self.before, &format!("\tpush   {} _lambda_space_heap", self.backend.pointer_size()), None);
        CodeGen::add(&mut self.before, "    call malloc", None);
        CodeGen::add(&mut self.before, &format!("    add {}, {}", sp, wl * 2), None);

        CodeGen::add(&mut self.before, "    mov ecx, eax", None);

        CodeGen::add(&mut self.after, &format!("    push {}", (num_of_params + 1) * wl), None);
        CodeGen::add(&mut self.after, &format!("\tpush   {} _lambda_space_heap", self.backend.pointer_size()), None);
        CodeGen::add(&mut self.after, "    call mdealloc", None);
        CodeGen::add(&mut self.after, &format!("    add {}, {}", sp, wl * 2), None);

        let mut i = 1;

        // TODO optimize: do not create parameters that are overridden by parent memcopy
        context.iter().for_each(|(name, kind)| {
            CodeGen::add(&mut self.before, &format!("    add  ecx, {}", wl), None);

            if let VarKind::ParameterRef(index, par) = kind {
                let type_size = self.backend.type_size(&par.type_ref).unwrap();

                let address = format!("{}+{}+{}", bp, wl, (index + 1) * wl);

                CodeGen::add(&mut self.before, &format!("    mov  {} ebx, [{}]", type_size, address),
                             Some(&format!("context parameter {}", name)));
                CodeGen::add(&mut self.before, "    mov  dword [ecx], ebx", None);

                lambda_space.add_context_parameter(name.clone(), i);
                def.parameters.push(ASTParameterDef { name: name.into(), type_ref: par.type_ref.clone(), from_context: true });
                i += 1;
            }
        });

        // I copy the lambda space of the parent
        if let Some(parent_lambda) = parent_lambda_space {
            let parent_lambda_size = parent_lambda.parameters_indexes.len() + 1;
            CodeGen::add(&mut self.before, "    push eax", None);
            CodeGen::add(&mut self.before, &format!("    push {} {}", self.backend.pointer_size(), parent_lambda_size), None);
            CodeGen::add(&mut self.before, "    push eax", None);
            CodeGen::add(&mut self.before, &format!("    push {} [{}+8]", self.backend.pointer_size(), self.backend.stack_base_pointer()), None);
            CodeGen::add(&mut self.before, "    call memcopy", None);
            CodeGen::add(&mut self.before, &format!("    add {},{}", self.backend.stack_pointer(), 3 * wl), None);
            CodeGen::add(&mut self.before, "    pop eax", None);
        }

        CodeGen::add(&mut self.before, "    pop ecx", None);
        CodeGen::add(&mut self.before, "    pop ebx", None);

        CodeGen::add(&mut self.before, &format!("    mov {} [eax], {}", pointer_size, def.name), None);
        CodeGen::add(&mut self.before, &format!("    mov {} [{} + {}], eax", self.backend.word_size(), self.backend.stack_pointer(), self.to_remove_from_stack * self.backend.word_len() as usize), comment);

        self.to_remove_from_stack += 1;

        lambda_space
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
                CodeGen::add(&mut self.before, &format!("    mov {} eax, [{}+{}+{}]", type_size, self.backend.stack_base_pointer(), word_len, (index + 1) * word_len), comment);
            } else {
                CodeGen::add(&mut self.before, "push ebx", None);
                CodeGen::add(&mut self.before, &format!("    mov {} ebx, [{}+{}+{}]", type_size, self.backend.stack_base_pointer(), word_len, (index + 1) * word_len), comment);

                // +1 because of push ebx
                CodeGen::add(&mut self.before, &format!("    mov [{} + {}], ebx", self.backend.stack_pointer(),
                                                     (self.to_remove_from_stack + 1) * self.backend.word_len() as usize), comment);
                /*
                CodeGen::add(&mut self.before, &format!("    mov [{} - {}], ebx", self.backend.stack_base_pointer(),
                                                     (self.parameters.len() - self.to_remove_from_stack) * self.backend.word_len() as usize), comment);

                 */

                CodeGen::add(&mut self.before, "pop ebx", None);
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
            CodeGen::add(&mut self.before, "", comment);
            CodeGen::add(&mut self.before, &format!("    mov     eax, [{}+{}]", sbp, word_len * 2), Some("The address to the lambda space"));
            CodeGen::add(&mut self.before, &format!("    mov   {} eax,[eax + {}]", self.backend.pointer_size(), lambda_space_index * word_len), None);
            CodeGen::add(&mut self.before, &format!("    mov [{} + {}], eax", self.backend.stack_pointer(),
                                                 self.to_remove_from_stack * self.backend.word_len() as usize), comment);
            /*
            CodeGen::add(&mut self.before, &format!("    mov [{} - {}], eax", self.backend.stack_base_pointer(),
                                                 (self.parameters.len() - self.to_remove_from_stack) * self.backend.word_len() as usize), comment);

             */
            //if self.immediate {
            //    CodeGen::add(&mut self.before, "    pop eax", Some(&format!("immediate reference to val '{}'", param_name)));
            //}
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

            let address = format!("[{}+{}]", self.backend.stack_base_pointer(), relative_address);

            result = result.replace(&format!("${}", par.name), &address);
            i += 1;
        }
        result
    }

    pub fn to_remove_from_stack(&self) -> usize {
        self.to_remove_from_stack
    }

    pub fn before(&self) -> String {
        let mut result = String::new();
        if !self.immediate && self.to_remove_from_stack > 0 {
            result.push_str(&format!("sub {}, {}\n", self.backend.stack_pointer(), self.backend.word_len() as usize * self.to_remove_from_stack));
        }
        result.push_str(&self.before);
        result
    }

    pub fn after(&self) -> String {
        self.after.clone()
    }

    pub fn push(&mut self, s: &str) {
        self.before.push_str(s);
    }

}