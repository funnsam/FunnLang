use std::path::Path;
use std::collections::HashMap;

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::FunctionValue;
use inkwell::values::IntValue;
use inkwell::values::PointerValue;
use inkwell::types::IntType;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::targets::*;

use crate::ast::nodes::Expr;
use crate::ast::nodes::FuncDefArg;
use crate::ast::nodes::Type;
use crate::ast::nodes::{Program, Node};

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    cur_fn: Option<FunctionValue<'ctx>>,
    cur_vars: HashMap<String, PointerValue<'ctx>>
}

impl<'ctx> CodeGen<'ctx> {
    pub fn compile(ast: &Program, path: &Path, filetype: &FileType) {
        let context = Context::create();
        let module  = context.create_module("FunnLang");
        let mut codegen = CodeGen {
            context : &context,
            module,
            builder : context.create_builder(),
            cur_fn  : None,
            cur_vars: HashMap::new(),
        };
        
        codegen.compile_ast(ast);

        codegen.write(path, filetype);
    }

    pub fn write(&self, path: &Path, filetype: &FileType) {
        Target::initialize_x86(&InitializationConfig::default());
        let triple  = TargetMachine::get_default_triple();
        let target  = Target::from_triple(&triple).unwrap();
        let cpu     = TargetMachine::get_host_cpu_name();
        let features= TargetMachine::get_host_cpu_features();
        let reloc   = RelocMode::Default;
        let model   = CodeModel::Default;
        let opt     = OptimizationLevel::Default;
        let target_machine = target
            .create_target_machine(
                &triple, 
                cpu.to_str().unwrap(), 
                features.to_str().unwrap(), 
                opt, 
                reloc, 
                model
            )
            .unwrap();
        
        target_machine.write_to_file(&self.module, *filetype, path).unwrap();
    }

    fn compile_ast(&mut self, ast: &Program) {
        for statement in &ast.body {
            match statement {
                Node::FuncDefine { func_name, func_args, func_type, func_body, linkage } => {
                    let ty = self.as_llvm_type(func_type).fn_type(&self.args_to_metadata(func_args), false);
                    let func = self.module.add_function(func_name, ty, linkage.as_inkwell_linkage());

                    self.cur_fn = Some(func);

                    let entry = self.context.append_basic_block(func, "entry");
                    self.builder.position_at_end(entry);

                    self.compile_ast(func_body);

                    self.cur_fn = None;
                },
                Node::Return(val) => {
                    match val {
                        Some(val) => {
                            let e = self.compile_expr(val);
                            self.builder.build_return(Some(&e));
                        },
                        None => {
                            self.builder.build_return(None);
                        }
                    };
                },
                _ => todo!()
            }
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> IntValue<'ctx> {
        match expr {
            Expr::Number(v) => self.context.i64_type().const_int(*v as u64, false),
            _ => todo!()
        }
    }

    fn as_llvm_type(&mut self, typ: &Type) -> IntType<'ctx> {
        match typ {
            Type::Name(v) => {
                match v.as_str() {
                    "int" | "i32" => self.context.i32_type(),
                    _ => panic!()
                }
            }
            _ => todo!()
        }
    }

    fn args_to_metadata(&mut self, args: &Vec<FuncDefArg>) -> Vec<BasicMetadataTypeEnum<'ctx>> {
        let mut ret = Vec::new();
        for typ in args {
            match typ.clone().typ {
                Type::Name(n) => {
                    if n.contains("int") {
                        ret.push(BasicMetadataTypeEnum::IntType(self.as_llvm_type(&(*typ).clone().typ)));
                    }
                }
                _ => todo!()
            }
        }
        ret
    }
}