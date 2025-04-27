use std::cell::RefCell;

use crate::{
    codegen::{
        enh_ast::{EnhASTType, EnhBuiltinTypeKind},
        get_reference_type_name,
        statics::Statics,
        text_macro::{get_type, MacroParam, TextMacro, TextMacroEval},
        typedef_provider::TypeDefProvider,
        CodeGen,
    },
    enh_type_check::typed_ast::{
        ASTTypedFunctionDef, ASTTypedType, BuiltinTypedTypeKind, DefaultFunctionCall,
    },
};

use super::code_gen_asm::CodeGenAsm;

thread_local! {
    static COUNT : RefCell<usize> = RefCell::new(0);
}

pub struct AsmPrintRefMacro {
    code_gen: CodeGenAsm,
}

impl TextMacroEval for AsmPrintRefMacro {
    fn eval_macro(
        &self,
        _statics: &mut Statics,
        text_macro: &TextMacro,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
    ) -> String {
        let result = match text_macro.parameters.get(0) {
            None => panic!("cannot find parameter for printRef macro"),
            Some(par) => match par {
                MacroParam::Plain(name, ast_type, ast_typed_type) => self.print_ref(
                    name,
                    ast_type,
                    ast_typed_type,
                    function_def,
                    type_def_provider,
                    0,
                    &self.code_gen,
                ),
                MacroParam::StringLiteral(_) => {
                    panic!("String is nt a valid parameter for printRef macro ")
                }
                MacroParam::Ref(name, ast_type, ast_typed_type) => self.print_ref(
                    name,
                    ast_type,
                    ast_typed_type,
                    function_def,
                    type_def_provider,
                    0,
                    &self.code_gen,
                ),
            },
        };

        result
    }

    fn is_pre_macro(&self) -> bool {
        true
    }

    fn default_function_calls(&self) -> Vec<DefaultFunctionCall> {
        vec![
            DefaultFunctionCall::new(
                "print",
                vec![EnhASTType::Builtin(EnhBuiltinTypeKind::I32)],
                0,
            ),
            DefaultFunctionCall::new(
                "println",
                vec![EnhASTType::Builtin(EnhBuiltinTypeKind::I32)],
                0,
            ),
            DefaultFunctionCall::new(
                "print",
                vec![EnhASTType::Builtin(EnhBuiltinTypeKind::String)],
                0,
            ),
            DefaultFunctionCall::new(
                "println",
                vec![EnhASTType::Builtin(EnhBuiltinTypeKind::String)],
                0,
            ),
            DefaultFunctionCall::new("println", Vec::new(), 0),
        ]
    }
}

impl AsmPrintRefMacro {
    pub fn new(code_gen: CodeGenAsm) -> Self {
        Self { code_gen }
    }

    fn print_ref(
        &self,
        src: &str,
        ast_type_o: &Option<EnhASTType>,
        ast_typed_type_o: &Option<ASTTypedType>,
        function_def: Option<&ASTTypedFunctionDef>,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        code_gen: &CodeGenAsm,
    ) -> String {
        if indent > 20 {
            return String::new();
        }

        let ast_typed_type = if let Some(ast_typed_type) = ast_typed_type_o {
            ast_typed_type.clone()
        } else if let Some(ast_type) = ast_type_o {
            if let Some(ast_typed_type) =
                type_def_provider.get_ast_typed_type_from_ast_type(ast_type)
            {
                ast_typed_type
            } else {
                match ast_type_o {
                    None => {
                        panic!("printRef macro: cannot find the type of the parameter {src}, please specify it")
                    }
                    Some(ast_type) => match ast_type {
                        EnhASTType::Builtin(EnhBuiltinTypeKind::String) => {
                            ASTTypedType::Builtin(BuiltinTypedTypeKind::String)
                        }
                        EnhASTType::Generic(_, generic_type_name, var_types) => {
                            match function_def {
                                None => panic!(),
                                Some(f) => {
                                    match f.resolved_generic_types.get(generic_type_name, var_types)
                                    {
                                        None => {
                                            panic!("printRef macro: Cannot find generic type {generic_type_name}")
                                        }
                                        Some(ast_typed_type) => ast_typed_type.clone(),
                                    }
                                }
                            }
                        }
                        EnhASTType::Custom {
                            namespace,
                            name: custom_type_name,
                            param_types: _,
                            index: _,
                        } => get_type(namespace, custom_type_name, type_def_provider, function_def),
                        _ => panic!("printRef macro: unsupported type {ast_type}"),
                    },
                }
            }
        } else {
            panic!();
        };

        let (name, code, new_line) = match ast_typed_type {
            ASTTypedType::Builtin(BuiltinTypedTypeKind::String) => {
                ("str".to_owned(), String::new(), true)
            }
            ASTTypedType::Enum { namespace: _, name } => (
                name.clone(),
                self.print_ref_enum(&name, src, type_def_provider, indent + 1, code_gen),
                false,
            ),
            ASTTypedType::Struct { namespace: _, name } => (
                name.clone(),
                self.print_ref_struct(&name, src, type_def_provider, indent + 1, code_gen),
                true,
            ),
            ASTTypedType::Type {
                namespace: _,
                name,
                native_type: _,
                is_ref: _,
            } => (
                name.clone(),
                self.print_ref_type(&name, src, type_def_provider, indent + 1, code_gen),
                true,
            ),
            _ => panic!("unsupported type {ast_typed_type}"),
        };

        let mut result = String::new();

        let ident_string = " ".repeat(indent * 2);
        self.print_str(&mut result, &format!("{ident_string}{name} "), code_gen);

        self.print_i32(&mut result, src, code_gen);
        code_gen.add(&mut result, "push    ebx", None, true);
        code_gen.add(
            &mut result,
            &format!("push    {} {src}", code_gen.word_size()),
            None,
            true,
        );
        code_gen.add(&mut result, "pop    ebx", None, true);
        self.print_str(&mut result, " refcount ", code_gen);
        if new_line {
            self.println_i32(&mut result, "[ebx + 12]", code_gen);
        } else {
            self.print_i32(&mut result, "[ebx + 12]", code_gen);
        }
        code_gen.add(&mut result, "pop    ebx", None, true);

        result.push_str(&code);

        result
    }

    fn println_i32(&self, out: &mut String, value: &str, code_gen: &CodeGenAsm) {
        code_gen.add(
            out,
            //&format!("$call(println_i32_Unit,{value}:i32)"),
            &format!("$call(println,{value}:i32)"),
            None,
            true,
        );
    }

    fn print_i32(&self, out: &mut String, value: &str, code_gen: &CodeGenAsm) {
        code_gen.add(
            out,
            //&format!("$call(print_i32_Unit,{value}:i32)"),
            &format!("$call(print,{value}:i32)"),
            None,
            true,
        );
    }

    fn print_str(&self, out: &mut String, value: &str, code_gen: &CodeGenAsm) {
        code_gen.add(
            out,
            //&format!("$call(print_str_Unit,\"{value}\")"),
            &format!("$call(print,\"{value}\")"),
            None,
            true,
        );
    }

    fn println_str(&self, out: &mut String, value: &str, code_gen: &CodeGenAsm) {
        code_gen.add(
            out,
            //&format!("$call(println_str_Unit,\"{value}\")"),
            &format!("$call(println,\"{value}\")"),
            None,
            true,
        );
    }

    fn print_ref_struct(
        &self,
        name: &str,
        src: &str,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        code_gen: &CodeGenAsm,
    ) -> String {
        let mut result = String::new();
        code_gen.add(&mut result, "push    ebx", None, true);
        code_gen.add(&mut result, &format!("mov dword ebx, {src}"), None, true);
        code_gen.add(&mut result, "mov dword ebx, [ebx]", None, true);
        if let Some(s) = type_def_provider.get_struct_def_by_name(name) {
            for (i, p) in s.properties.iter().enumerate() {
                let ast_type_o = if matches!(
                    p.ast_type,
                    ASTTypedType::Builtin(BuiltinTypedTypeKind::String)
                ) {
                    Some(EnhASTType::Builtin(EnhBuiltinTypeKind::String))
                } else {
                    type_def_provider.get_type_from_custom_typed_type(&p.ast_type)
                };
                if ast_type_o.is_some() {
                    let par_result = self.print_ref(
                        &format!("[ebx + {}]", i * code_gen.word_len()),
                        &ast_type_o,
                        &None,
                        None,
                        type_def_provider,
                        indent + 1,
                        code_gen,
                    );
                    result.push_str(&par_result);
                }
            }
        } else {
            panic!("Cannot find struct {name}");
        }
        code_gen.add(&mut result, "pop    ebx", None, true);
        result
    }

    fn print_ref_enum(
        &self,
        name: &str,
        src: &str,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        code_gen: &CodeGenAsm,
    ) -> String {
        let count = COUNT.with(|count| {
            *count.borrow_mut() += 1;
            *count.borrow()
        });

        let end_label_name = &format!("._{name}_end_{}", count);
        let ws = code_gen.word_size();
        let wl = code_gen.word_len();

        let mut result = String::new();
        code_gen.add(&mut result, "push    ebx", None, true);
        code_gen.add(&mut result, &format!("mov dword ebx, {src}"), None, true);
        code_gen.add(&mut result, "mov dword ebx, [ebx]", None, true);
        self.print_str(&mut result, " value ", code_gen);
        if let Some(s) = type_def_provider.get_enum_def_by_name(name) {
            for (i, variant) in s.variants.iter().enumerate() {
                let count = COUNT.with(|count| {
                    *count.borrow_mut() += 1;
                    *count.borrow()
                });
                let label_name = &format!("._{name}_{}_{}", variant.name, count);
                code_gen.add(&mut result, &format!("cmp {ws} [ebx], {}", i), None, true);
                code_gen.add(&mut result, &format!("jne {label_name}"), None, true);
                self.println_str(&mut result, &variant.name, code_gen);

                for (j, par) in variant.parameters.iter().enumerate() {
                    if get_reference_type_name(&par.ast_type, type_def_provider).is_some() {
                        let ast_type = if matches!(
                            &par.ast_type,
                            ASTTypedType::Builtin(BuiltinTypedTypeKind::String)
                        ) {
                            Some(EnhASTType::Builtin(EnhBuiltinTypeKind::String))
                        } else {
                            type_def_provider.get_type_from_custom_typed_type(&par.ast_type)
                        };

                        let par_result = self.print_ref(
                            &format!("[ebx + {}]", (variant.parameters.len() - j) * wl),
                            &ast_type,
                            &None,
                            None,
                            type_def_provider,
                            indent + 1,
                            code_gen,
                        );
                        result.push_str(&par_result);
                    }
                }
                code_gen.add(&mut result, &format!("jmp {end_label_name}"), None, false);
                code_gen.add(&mut result, &format!("{label_name}:"), None, false);
            }
        } else {
            panic!("Cannot find enum {name}");
        }
        self.print_str(&mut result, "unknown ", code_gen);
        self.println_i32(&mut result, "[ebx]", code_gen);
        code_gen.add(&mut result, "$call(exitMain, 1)", None, false);
        code_gen.add(&mut result, &format!("{end_label_name}:"), None, false);
        code_gen.add(&mut result, "pop    ebx", None, true);
        result
    }

    fn print_ref_type(
        &self,
        name: &str,
        src: &str,
        type_def_provider: &dyn TypeDefProvider,
        indent: usize,
        code_gen: &CodeGenAsm,
    ) -> String {
        if let Some(s) = type_def_provider.get_type_def_by_name(name) {
            let count = COUNT.with(|count| {
                *count.borrow_mut() += 1;
                *count.borrow()
            });

            let mut result = String::new();

            code_gen.add(
                &mut result,
                &format!("push  {} eax", code_gen.word_size()),
                None,
                true,
            );
            code_gen.add(
                &mut result,
                &format!("push  {} ebx", code_gen.word_size()),
                None,
                true,
            );
            code_gen.add(
                &mut result,
                &format!("push  {} ecx", code_gen.word_size()),
                None,
                true,
            );
            // TODO type classes
            for (i, ((generic_name, var_types), ast_typed_type)) in
                s.generic_types.iter().enumerate()
            {
                code_gen.add(
                    &mut result,
                    &format!("$call({}References, {src}:i32,{i})", s.original_name),
                    None,
                    true,
                );

                code_gen.add(
                    &mut result,
                    &format!("mov   {} eax,[eax]", code_gen.word_size()),
                    None,
                    true,
                );

                // count
                code_gen.add(
                    &mut result,
                    &format!("mov   {} ebx,[eax]", code_gen.word_size()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("add {} eax,{}", code_gen.word_size(), code_gen.word_len()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!(".loop_{name}_{generic_name}_{count}:"),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("test {} ebx,ebx", code_gen.word_size()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("jz .end_{name}_{generic_name}_{count}"),
                    None,
                    true,
                );
                let inner_result = self.print_ref(
                    "[eax]",
                    &None,
                    &Some(ast_typed_type.clone()),
                    None,
                    type_def_provider,
                    indent + 1,
                    code_gen,
                );
                result.push_str(&inner_result);
                code_gen.add(
                    &mut result,
                    &format!("dec {} ebx", code_gen.word_size()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("add {} eax,{}", code_gen.word_size(), code_gen.word_len()),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!("jmp .loop_{name}_{generic_name}_{count}"),
                    None,
                    true,
                );
                code_gen.add(
                    &mut result,
                    &format!(".end_{name}_{generic_name}_{count}:"),
                    None,
                    true,
                );
            }
            code_gen.add(&mut result, "pop  ecx", None, true);
            code_gen.add(&mut result, "pop  ebx", None, true);
            code_gen.add(&mut result, "pop  eax", None, true);

            result
        } else {
            panic!("print_ref_type, cannot find type {name}");
        }
    }
}
