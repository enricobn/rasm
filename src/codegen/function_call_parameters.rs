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
    has_inline_lambda_param: bool,
    already_added_to_stack: usize,
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
    pub fn new(backend: &'a dyn Backend, parameters: Vec<ASTParameterDef>, inline: bool, immediate: bool, already_added_to_stack: usize) -> Self {
        Self {
            to_remove_from_stack: 0,
            before: String::new(),
            after: String::new(),
            parameters_values: LinkedHashMap::new(),
            backend,
            inline,
            parameters,
            immediate,
            has_inline_lambda_param: false,
            already_added_to_stack,
        }
    }

    pub fn add_string_literal(&mut self, param_name: &str, label: String, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), label);
        } else {
            CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], {}", self.backend.pointer_size(), self.backend.stack_pointer(),
                                                    self.to_remove_from_stack * self.backend.word_len() as usize, label), comment,
                         true);

            /*CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], {}", self.backend.pointer_size(), self.backend.stack_pointer(),
                                                    0, label), comment,
            true);

             */
        }
        self.added_to_stack();
    }

    pub fn add_number(&mut self, param_name: &str, n: &i32, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), format!("{}", n));
        } else {
            CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], {}", self.backend.word_size(), self.backend.stack_pointer(),
                                                    self.to_remove_from_stack * self.backend.word_len() as usize, n), comment,
                         true);


            /*
            CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], {}", self.backend.word_size(), self.backend.stack_pointer(),
                                                    0, n), comment,
            true);

             */
        }
        self.added_to_stack();
    }

    pub fn add_function_call(&mut self, comment: Option<&str>) {
        CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], eax", self.backend.word_size(), self.backend.stack_pointer(), self.to_remove_from_stack * self.backend.word_len() as usize), comment, true);
        self.added_to_stack();
        //CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], eax", self.backend.word_size(), self.backend.stack_pointer(), 0), comment, true);
    }

    pub fn add_lambda(&mut self, def: &mut ASTFunctionDef, parent_lambda_space: Option<&LambdaSpace>, context: &VarContext, comment: Option<&str>) -> LambdaSpace {
        let mut lambda_space = LambdaSpace::new(context.clone());

        let num_of_params = context.iter().filter(|(_, kind)| {
            matches!(kind, VarKind::ParameterRef(_, _))
        }).count();

        let stack_base_pointer = self.backend.stack_base_pointer();
        let stack_pointer = self.backend.stack_pointer();
        let word_len = self.backend.word_len() as usize;
        let word_size = self.backend.word_size();
        let pointer_size = self.backend.pointer_size();

        // TODO it should be great if we could inline the function lambdaSpaceMalloc
        CodeGen::add(&mut self.before, "mov     eax,[_lambda_space_heap]", None, true);
        CodeGen::add(&mut self.before, &format!("add     eax,{}", (num_of_params + 1) * word_len), None, true);
        CodeGen::add(&mut self.before, &format!("mov     {} [_lambda_space_heap],eax", word_size), None, true);
        CodeGen::add(&mut self.before, &format!("sub     eax,{}", (num_of_params + 1) * word_len), None, true);

        // now in eax there is the address to the memory allocated for the lambda space

        CodeGen::add(&mut self.before, &format!("push {} ebx", pointer_size), None, true);

        // now we push in after var the code to deallocate the lambda space
        // TODO it should be great if we could inline the function lambdaSpaceMdealloc (but we will loose a little optimization of not pushing the register, because in
        //      this code we have already saved ebx and we can use it)
        CodeGen::add(&mut self.after, "mov     ebx,[_lambda_space_heap]", None, true);
        CodeGen::add(&mut self.after, &format!("sub     ebx,{}", (num_of_params + 1) * word_len), None, true);
        CodeGen::add(&mut self.after, "mov     dword [_lambda_space_heap],ebx", None, true);

        let mut i = 1;

        // TODO optimize: do not create parameters that are overridden by parent memcopy
        context.iter().for_each(|(name, kind)| {
            if let VarKind::ParameterRef(index, par) = kind {
                let type_size = self.backend.type_size(&par.type_ref).unwrap();

                let address = format!("{}+{}+{}", stack_base_pointer, word_len, (index + 1) * word_len);

                CodeGen::add(&mut self.before, &format!("mov  {} ebx, [{}]", type_size, address),
                             Some(&format!("context parameter {}", name)),
                             true);
                CodeGen::add(&mut self.before, &format!("mov  dword [eax + {}], ebx", i * word_len), None, true);

                lambda_space.add_context_parameter(name.clone(), i);
                def.parameters.push(ASTParameterDef { name: name.into(), type_ref: par.type_ref.clone(), from_context: true });
                i += 1;
            }
        });

        // I copy the lambda space of the parent
        if let Some(parent_lambda) = parent_lambda_space {
            let parent_lambda_size = parent_lambda.parameters_indexes.len() + 1;
            CodeGen::add(&mut self.before, &format!("push {} {}", pointer_size, parent_lambda_size), None, true);
            CodeGen::add(&mut self.before, "push eax", None, true);
            CodeGen::add(&mut self.before, &format!("push {} [{}+8]", pointer_size, stack_base_pointer), None, true);
            CodeGen::add(&mut self.before, "call memcopy", None, true);
            CodeGen::add(&mut self.before, &format!("add {},{}", stack_pointer, 3 * word_len), None, true);
        }

        CodeGen::add(&mut self.before, "pop ebx", None, true);

        CodeGen::add(&mut self.before, &format!("mov {} [eax], {}", pointer_size, def.name), None, true);
        CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], eax", word_size, stack_pointer, self.to_remove_from_stack * word_len as usize), comment, true);
        self.added_to_stack();
        //CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], eax", word_size, stack_pointer, 0), comment, true);


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
                CodeGen::add(&mut self.before, &format!("mov {} eax, [{}+{}+{}]", type_size, self.backend.stack_base_pointer(), word_len, (index + 1) * word_len), comment, true);
            } else {
                CodeGen::add(&mut self.before, "push ebx", None, true);
                CodeGen::add(&mut self.before, &format!("mov {} ebx, [{}+{}+{}]", type_size, self.backend.stack_base_pointer(), word_len, (index + 1) * word_len), comment, true);

                // +1 because of push ebx
                CodeGen::add(&mut self.before, &format!("mov [{} + {}], ebx", self.backend.stack_pointer(),
                                                        (self.to_remove_from_stack + 1) * self.backend.word_len() as usize), comment,
                             true);


                /*CodeGen::add(&mut self.before, &format!("mov [{} + {}], ebx", self.backend.stack_pointer(),
                                                        0), comment,
                true);

                 */
                CodeGen::add(&mut self.before, "pop ebx", None, true);
            }
        }
        self.added_to_stack();
    }

    pub fn add_lambda_param_from_lambda_space(&mut self, original_param_name: &str, param_name: &str, lambda_space: &LambdaSpace, comment: Option<&str>, indent: usize) {
        let lambda_space_index = lambda_space.get_index(param_name).unwrap();
        debug!("{}add_lambda_param_from_lambda_space, original_param_name {}, param {}, lambda_space_index {}", " ".repeat(indent * 4), original_param_name, param_name, lambda_space_index);

        let word_len = self.backend.word_len() as usize;
        let sbp = self.backend.stack_base_pointer();

        if self.inline {
            self.has_inline_lambda_param = true;
            self.parameters_values.insert(original_param_name.into(), format!("[eax + {}]", lambda_space_index * word_len));
        } else {
            CodeGen::add(&mut self.before, "", comment, true);
            CodeGen::add(&mut self.before, &format!("mov     eax, [{}+{}]", sbp, word_len * 2), Some("The address to the lambda space"), true);
            CodeGen::add(&mut self.before, &format!("mov   {} eax,[eax + {}]", self.backend.pointer_size(), lambda_space_index * word_len), None, true);

            if !self.immediate {
                CodeGen::add(&mut self.before, &format!("mov [{} + {}], eax", self.backend.stack_pointer(),
                                                        self.to_remove_from_stack * self.backend.word_len() as usize), comment,
                             true);


                /*
                CodeGen::add(&mut self.before, &format!("mov [{} + {}], eax", self.backend.stack_pointer(),
                                                        0), comment,
                true);

                 */
            }
        }
        self.added_to_stack();
    }

    fn added_to_stack(&mut self) {
        /*
        if !self.immediate {
            CodeGen::add(&mut self.before, &format!("sub {}, {}", self.backend.stack_pointer(), self.backend.word_len() as usize),
                         Some("Prepare stack for parameter"), true);
        }

         */
        self.to_remove_from_stack += 1;
    }

    pub fn resolve_asm_parameters(&self, body: &str, to_remove_from_stack: usize, ident: usize) -> String {
        let mut result = body.to_string();
        let mut i = 0;
        let word_len = self.backend.word_len() as i32;
        for par in self.parameters.iter() {
            if let Some(par_value) = self.parameters_values.get(&par.name) {
                debug!("{}found parameter {}, value: {}", " ".repeat(ident * 4), par.name, par_value);
                result = result.replace(&format!("${}", par.name), par_value);
                i += 1;
                continue;
            }

            debug!("{}cannot find parameter {}, parameters_values {:?}", " ".repeat(ident * 4), par.name, self.parameters_values);

            let pointer = if self.inline {
                self.backend.stack_base_pointer()
            } else {
                self.backend.stack_base_pointer()
            };
            let relative_address =
                if self.inline {
                    debug!("{}  i {}, self.to_remove_from_stack {}, to_remove_from_stack {}", " ".repeat(ident * 4), i, self.to_remove_from_stack, to_remove_from_stack);
                    result.push_str(&format!(";i {}, self.to_remove_from_stack {}, to_remove_from_stack {}, already_added_to_stack {}\n", i, self.to_remove_from_stack, to_remove_from_stack, self.already_added_to_stack));
                    (i as i32 - self.to_remove_from_stack() as i32 - to_remove_from_stack as i32) * word_len
                } else {
                    (i + 2) * self.backend.word_len() as i32
                };

            let address = format!("[{}+{}]", pointer, relative_address);

            result = result.replace(&format!("${}", par.name), &address);
            i += 1;
        }
        result
    }

    pub fn to_remove_from_stack(&self) -> usize {
        self.parameters.len()
    }

    pub fn before(&self) -> String {
        let mut result = String::new();

        if !self.parameters.is_empty() {
            CodeGen::add(&mut result, &format!("sub {}, {}", self.backend.stack_pointer(), self.backend.word_len() as usize * self.parameters.len()),
                         Some("Prepare stack for parameters"), true);
        }

        if self.has_inline_lambda_param {
            CodeGen::add(&mut result, &format!("mov     eax, [{}+{}]", self.backend.stack_base_pointer(), self.backend.word_len() * 2),
                         Some("The address to the lambda space for inline lambda param"), true);
            if !self.immediate {
                CodeGen::add(&mut result, "push    eax", None, true);
            }
        }

        result.push_str(&self.before);

        if self.has_inline_lambda_param && !self.immediate {
            CodeGen::add(&mut result, "pop    eax", None, true);
        }
        result
    }

    pub fn after(&self) -> String {
        self.after.clone()
    }

    pub fn push(&mut self, s: &str) {
        self.before.push_str(s);
    }
}