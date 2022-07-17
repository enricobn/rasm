use linked_hash_map::LinkedHashMap;
use log::debug;
use crate::codegen::backend::Backend;
use crate::codegen::{CodeGen, LambdaSpace, VarContext, VarKind};
use crate::codegen::stack::Stack;
use crate::parser::ast::{ASTFunctionDef, ASTParameterDef, ASTTypeRef};

pub struct FunctionCallParameters<'a> {
    parameters: Vec<ASTParameterDef>,
    parameters_added: usize,
    before: String,
    parameters_values: LinkedHashMap<String, String>,
    backend: &'a dyn Backend,
    inline: bool,
    immediate: bool,
    has_inline_lambda_param: bool,
    lambda_slots_to_deallocate: usize,
    stack: &'a Stack,
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
    pub fn new(backend: &'a dyn Backend, parameters: Vec<ASTParameterDef>, inline: bool, immediate: bool, stack: &'a Stack) -> Self {
        Self {
            parameters_added: 0,
            before: String::new(),
            parameters_values: LinkedHashMap::new(),
            backend,
            inline,
            parameters,
            immediate,
            has_inline_lambda_param: false,
            lambda_slots_to_deallocate: 0,
            stack,
        }
    }

    pub fn add_string_literal(&mut self, param_name: &str, label: String, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), label);
        } else {
            CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], {}", self.backend.pointer_size(), self.backend.stack_pointer(),
                                                    self.parameters_added * self.backend.word_len() as usize, label), comment,
                         true);
        }
        self.parameter_added_to_stack(&format!("string literal {}", param_name));
    }

    pub fn add_number(&mut self, param_name: &str, n: &i32, comment: Option<&str>) {
        if self.inline {
            self.parameters_values.insert(param_name.into(), format!("{}", n));
        } else {
            CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], {}", self.backend.word_size(), self.backend.stack_pointer(),
                                                    self.parameters_added * self.backend.word_len() as usize, n), comment,
                         true);
        }
        self.parameter_added_to_stack(&format!("number {}", param_name));
    }

    pub fn add_function_call(&mut self, comment: Option<&str>) {
        CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], eax", self.backend.word_size(), self.backend.stack_pointer(), self.parameters_added * self.backend.word_len() as usize), comment, true);
        self.parameter_added_to_stack("function call result");
    }

    pub fn add_lambda(&mut self, def: &mut ASTFunctionDef, parent_lambda_space: Option<&LambdaSpace>, context: &VarContext, comment: Option<&str>) -> LambdaSpace {
        let mut lambda_space = LambdaSpace::new(context.clone());

        let num_of_values_in_context = context.iter().filter(|(_, kind)| {
            matches!(kind, VarKind::ParameterRef(_, _))
        }).count();

        let stack_base_pointer = self.backend.stack_base_pointer();
        let stack_pointer = self.backend.stack_pointer();
        let word_len = self.backend.word_len() as usize;
        let word_size = self.backend.word_size();
        let pointer_size = self.backend.pointer_size();

        self.lambda_slots_to_deallocate += num_of_values_in_context + 1;

        let mut i = 1;

        Self::allocate_lambda_space(self.backend, &mut self.before, "ecx", num_of_values_in_context + 1);

        // TODO optimize: do not create parameters that are overridden by parent memcopy
        context.iter().for_each(|(name, kind)| {
            if let VarKind::ParameterRef(index, par) = kind {
                self.indirect_mov(
                    &format!("{}+{}", stack_base_pointer, (index + 2) * word_len),
                    &format!("ecx + {}", i * word_len), "ebx", Some(&format!("context parameter {}", name)));

                lambda_space.add_context_parameter(name.clone(), i);
                def.parameters.push(ASTParameterDef { name: name.into(), type_ref: par.type_ref.clone(), from_context: true });
                i += 1;
            }
        });

        // I copy the lambda space of the parent
        if let Some(parent_lambda) = parent_lambda_space {
            self.mem_copy(&format!("[{}+8]", stack_base_pointer), "ecx", parent_lambda.parameters_indexes.len() + 1, None);
        }

        CodeGen::add(&mut self.before, &format!("mov {} [ecx], {}", pointer_size, def.name), None, true);
        CodeGen::add(&mut self.before, &format!("mov {} [{} + {}], ecx", word_size, stack_pointer, self.parameters_added * word_len as usize), comment, true);
        //CodeGen::add(&mut self.before, &format!("add ecx, {}", (num_of_values_in_context + 1) * word_len), comment, true);

        self.parameter_added_to_stack(&format!("lambda {}", def.name));

        lambda_space
    }

    pub fn add_val(&mut self, original_param_name: String, par: &ASTParameterDef, index: usize, comment: Option<&str>, indent: usize) {
        debug!("{}adding val {}, index {}, from context {}", " ".repeat(indent * 4), original_param_name, index, par.from_context);
        self.add_val_(original_param_name, &par.type_ref, index, comment);
    }

    fn add_val_(&mut self, original_param_name: String, type_ref: &ASTTypeRef, index: usize, comment: Option<&str>) {
        let word_len = self.backend.word_len() as usize;

        if self.inline {
            self.parameters_values.insert(original_param_name.clone(), format!("[{}+{}]", self.backend.stack_base_pointer(), (index + 2) * word_len));
        } else {
            let type_size = self.backend.type_size(type_ref).unwrap_or_else(|| panic!("Unsupported type size: {:?}", type_ref));

            if self.immediate {
                CodeGen::add(&mut self.before, &format!("mov {} eax, [{}+{}]", type_size, self.backend.stack_base_pointer(), (index + 2) * word_len), comment, true);
            } else {
                self.indirect_mov(&format!("{}+{}", self.backend.stack_base_pointer(), (index + 2) * word_len),
                                  &format!("{} + {}", self.backend.stack_pointer(),
                                           self.parameters_added * self.backend.word_len() as usize), "ebx", comment);
            }
        }
        self.parameter_added_to_stack(&format!("val {}", original_param_name));
    }

    pub fn add_lambda_param_from_lambda_space(&mut self, original_param_name: &str, param_name: &str, lambda_space: &LambdaSpace, comment: Option<&str>, indent: usize) {
        let lambda_space_index = lambda_space.get_index(param_name).unwrap();
        debug!("{}add_lambda_param_from_lambda_space, original_param_name {}, param {}, lambda_space_index {}", " ".repeat(indent * 4), original_param_name, param_name, lambda_space_index);

        let word_len = self.backend.word_len() as usize;
        let sbp = self.backend.stack_base_pointer();

        if self.inline {
            self.has_inline_lambda_param = true;
            self.parameters_values.insert(original_param_name.into(), format!("[edx + {}]", lambda_space_index * word_len));
        } else {
            CodeGen::add(&mut self.before, "", comment, true);
            CodeGen::add(&mut self.before, &format!("mov     ebx, [{}+{}]", sbp, word_len * 2), Some("The address to the lambda space"), true);

            if self.immediate {
                CodeGen::add(&mut self.before, &format!("mov   {} eax,[ebx + {}]", self.backend.pointer_size(), lambda_space_index * word_len), None, true);
            } else {
                self.indirect_mov(&format!("ebx + {}", lambda_space_index * word_len),
                                  &format!("{} + {}", self.backend.stack_pointer(),
                                           self.parameters_added * self.backend.word_len() as usize), "ebx", comment);
            }
        }
        self.parameter_added_to_stack(&format!("lambda from lambda space: {}", param_name));
    }

    pub fn resolve_asm_parameters(&self, body: &str, to_remove_from_stack: usize, ident: usize) -> String {
        let mut result = body.to_string();
        let mut i = 0;
        let word_len = self.backend.word_len() as i32;
        for par in self.parameters.iter() {
            if let Some(par_value) = self.parameters_values.get(&par.name) {
                debug!("{}found parameter {}, value: {}", " ".repeat(ident * 4), par.name, par_value);
                result.push_str(&format!(";found parameter {}, value: {}\n", par.name, par_value));
                result = result.replace(&format!("${}", par.name), par_value);
                i += 1;
                continue;
            }

            debug!("{}cannot find parameter {} in parameters_values {:?} we take if from the stack", " ".repeat(ident * 4), par.name, self.parameters_values);
            result.push_str(&format!(";cannot find parameter {} in parameters_values {:?} we take it from stack\n", par.name, self.parameters_values));

            let relative_address =
                if self.inline {
                    debug!("{}  i {}, self.to_remove_from_stack {}, to_remove_from_stack {}", " ".repeat(ident * 4), i, self.parameters_added, to_remove_from_stack);
                    result.push_str(&format!(";i {}, self.to_remove_from_stack {}, to_remove_from_stack {}\n", i, self.parameters_added, to_remove_from_stack));
                    (i as i32 - self.to_remove_from_stack() as i32 - to_remove_from_stack as i32) * word_len
                } else {
                    (i + 2) * self.backend.word_len() as i32
                };

            let address = format!("[{}+{}]", self.backend.stack_base_pointer(), relative_address);

            result = result.replace(&format!("${}", par.name), &address);
            i += 1;
        }
        result
    }

    fn parameter_added_to_stack(&mut self, param_name: &str) {
        self.parameters_added += 1;
        self.stack.reserve(param_name, 1);
    }

    pub fn to_remove_from_stack(&self) -> usize {
        self.parameters.len()
        //self.parameters_added
    }

    pub fn before(&self) -> String {
        let mut result = String::new();

        let word_len = self.backend.word_len() as usize;

        if !self.parameters.is_empty() {
            CodeGen::add(&mut result, &format!("sub {}, {}", self.backend.stack_pointer(), word_len * self.parameters.len()),
                         Some("Prepare stack for parameters"), true);
        }

        if self.has_inline_lambda_param {
            CodeGen::add(&mut result, &format!("mov     edx, [{}+{}]", self.backend.stack_base_pointer(), word_len * 2),
                         Some("The address to the lambda space for inline lambda param"), true);
        }

        result.push_str(&self.before);

        result
    }


    pub fn after(&self) -> String {
        let mut s = String::new();
        if self.lambda_slots_to_deallocate > 0 {
            Self::deallocate_lambda_space(self.backend, &mut s, "ebx", self.lambda_slots_to_deallocate);
        }
        s
    }

    fn allocate_lambda_space(backend: &dyn Backend, out: &mut String, register_to_store_result: &str, slots: usize) {
        CodeGen::add(out, "; lambda space allocation", None, true);
        CodeGen::add(out, &format!("mov     {register_to_store_result},[_lambda_space_heap]"), None, true);
        CodeGen::add(out, &format!("add     {register_to_store_result},{}", slots * backend.word_len() as usize), None, true);
        CodeGen::add(out, &format!("mov     {} [_lambda_space_heap],{register_to_store_result}", backend.word_size()), None, true);
        CodeGen::add(out, &format!("sub     {register_to_store_result},{}", slots * backend.word_len() as usize), None, true);
    }

    fn deallocate_lambda_space(backend: &dyn Backend, out: &mut String, tmp_register: &str, slots: usize) {
        CodeGen::add(out, "; lambda space deallocation", None, true);
        CodeGen::add(out, &format!("mov     {tmp_register},[_lambda_space_heap]"), None, true);
        CodeGen::add(out, &format!("sub     {tmp_register},{}", slots * backend.word_len() as usize), None, true);
        CodeGen::add(out, &format!("mov     dword [_lambda_space_heap],{tmp_register}"), None, true);
    }

    pub fn push(&mut self, s: &str) {
        self.before.push_str(s);
    }

    fn mem_copy(&mut self, source: &str, dest: &str, slots: usize, comment: Option<&str>) {
        CodeGen::add(&mut self.before, &format!("push {} {}", self.backend.pointer_size(), slots), comment, true);
        CodeGen::add(&mut self.before, &format!("push {} {}", self.backend.pointer_size(), dest), comment, true);
        CodeGen::add(&mut self.before, &format!("push {} {}", self.backend.pointer_size(), source), comment, true);
        CodeGen::add(&mut self.before, "call memcopy", comment, true);
        self.restore_stack(3, comment);
    }

    fn restore_stack(&mut self, slots: usize, comment: Option<&str>) {
        CodeGen::add(&mut self.before, &format!("add {},{}", self.backend.stack_pointer(), slots * self.backend.word_len() as usize), comment, true);
    }

    fn indirect_mov(&mut self, source: &str, dest: &str, temporary_register: &str, comment: Option<&str>) {
        CodeGen::add(&mut self.before, &format!("mov  {} {}, [{}]", self.backend.word_size(), temporary_register, source),
                     comment, true);
        CodeGen::add(&mut self.before, &format!("mov  {} [{}], {}", self.backend.word_size(), dest, temporary_register), comment, true);
    }
}