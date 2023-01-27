use std::path::Path;
use std::collections::HashMap;

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::*;
use inkwell::types::*;
use inkwell::targets::*;

use crate::CompilerTarget;
use crate::ast::nodes::BiOp;
use crate::ast::nodes::Expr;
use crate::ast::nodes::FuncDefArg;
use crate::ast::nodes::Type;
use crate::ast::nodes::{Program, Node};

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    cur_fn: Option<FunctionValue<'ctx>>,
    vars: Vec<HashMap<String, (PointerValue<'ctx>, AnyTypeEnum<'ctx>)>>
}

fn initialize(target: &CompilerTarget) {
    match target {
        CompilerTarget::RV64 => Target::initialize_riscv(&InitializationConfig::default()),
        CompilerTarget::AA64 => Target::initialize_aarch64(&InitializationConfig::default()),
        CompilerTarget::X64  => Target::initialize_x86(&InitializationConfig::default()),
        CompilerTarget::WASM => Target::initialize_webassembly(&InitializationConfig::default())
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn compile(ast: &Program, path: &Path, filetype: &FileType, emit_ir: bool, target: &CompilerTarget) {
        let context = Context::create();
        let module  = context.create_module("FunnLang");
        let mut codegen = CodeGen {
            context : &context,
            module,
            builder : context.create_builder(),
            cur_fn  : None,
            vars: Vec::new(),
        };
        
        codegen.compile_ast(ast);

        initialize(target);
        codegen.write(path, filetype);

        if emit_ir {
            codegen.module.print_to_stderr();
        }
    }

    pub fn write(&self, path: &Path, filetype: &FileType) {
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

    fn as_fn_type(ret: AnyTypeEnum<'ctx>, args: Vec<BasicMetadataTypeEnum<'ctx>>) -> FunctionType<'ctx> {
        match BasicTypeEnum::try_from(ret) {
            Ok(v) => v.fn_type(&args, false),
            Err(_) => {
                match ret {
                    AnyTypeEnum::VoidType(v) => v.fn_type(&args, false),
                    AnyTypeEnum::ArrayType(_) | AnyTypeEnum::FloatType(_) | AnyTypeEnum::IntType(_) | AnyTypeEnum::PointerType(_) |
                    AnyTypeEnum::StructType(_) | AnyTypeEnum::VectorType(_) => unreachable!(),
                    _ => todo!()
                }
            }
        }
    }

    fn compile_ast(&mut self, ast: &Program) {
        self.vars.push(self.vars.last().unwrap_or(&HashMap::new()).clone());
        for statement in &ast.body {
            match statement {
                Node::FuncDefine { func_name, func_args, func_type, func_body, linkage } => {
                    let ty = Self::as_fn_type(self.as_llvm_type(func_type), self.args_to_metadata(func_args));
                    let func = self.module.add_function(func_name, ty, linkage.as_inkwell_linkage());

                    self.cur_fn = Some(func);
                    let alloca_entry = self.context.append_basic_block(func, "__var_allocs");
                    let entry = self.context.append_basic_block(func, "__entry");

                    self.builder.position_at_end(entry);
                    self.compile_ast(func_body);

                    self.builder.position_at_end(alloca_entry);
                    self.builder.build_unconditional_branch(entry);

                    self.cur_fn = None;
                },
                Node::Return(val) => {
                    match val {
                        Some(val) => {
                            let e = self.compile_expr(val, &self.cur_fn.unwrap().get_type().get_return_type().unwrap());
                            self.builder.build_return(Some(&e));
                        },
                        None => {
                            self.builder.build_return(None);
                        }
                    };
                },
                Node::FuncCall { func_name, func_args } => {
                    let func = self.module.get_function(func_name).unwrap();
                    let mut args = Vec::with_capacity(func_args.len());
                    for (i, expr) in func_args.iter().enumerate() {
                        let expr = self.compile_expr(expr, &func.get_type().get_param_types()[i]);
                        args.push(BasicMetadataValueEnum::IntValue(expr));
                    }
                    self.builder.build_call(func, args.as_slice(), func_name);
                },
                Node::VarDefine { var_type, var_name, val_expr } => {
                    let typ = self.as_llvm_type(var_type);
                    let alloca_entry = self.cur_fn.unwrap().get_first_basic_block();
                    self.builder.position_at_end(alloca_entry.unwrap());
                    let alloca = self.builder.build_alloca(
                        BasicTypeEnum::try_from(typ).unwrap(),
                        &format!("var_{var_name}")
                    );
                    self.vars.last_mut().unwrap().insert(var_name.to_owned(), (alloca, typ));
                    self.builder.position_at_end(self.cur_fn.unwrap().get_last_basic_block().unwrap());
                    let val = self.compile_expr(val_expr, &typ.try_into().unwrap());
                    self.builder.build_store(alloca, val);
                },
                Node::VarAssign { var_name, val_expr } => {
                    let val = self.compile_expr(val_expr, &self.vars.last().unwrap()[var_name].1.try_into().unwrap());
                    self.builder.build_store(self.vars.last().unwrap()[var_name].0, val);
                },
                _ => todo!()
            }
        }
        self.vars.pop();
    }

    fn compile_expr(&mut self, expr: &Expr, prefer: &BasicTypeEnum<'ctx>) -> IntValue<'ctx> {
        match expr {
            Expr::Number(v) => prefer.into_int_type().const_int(*v as u64, false),
            Expr::Ident(v) => {
                let var = self.vars.last().unwrap().get(v).unwrap();
                self.builder.build_load(BasicTypeEnum::try_from(var.1).unwrap(), var.0, &format!("load_{v}")).into_int_value()
            },
            Expr::BiOp { left, oper, right } => {
                let lhs = self.compile_expr(left, prefer);
                let rhs = self.compile_expr(right, prefer);
                match oper {
                    BiOp::Add => self.builder.build_int_add(lhs, rhs, "add"),
                    BiOp::Sub => self.builder.build_int_sub(lhs, rhs, "sub"),
                    BiOp::Mul => self.builder.build_int_mul(lhs, rhs, "mul"),
                    BiOp::Div => self.builder.build_int_unsigned_div(lhs, rhs, "div"),
                    BiOp::Mod => self.builder.build_int_unsigned_rem(lhs, rhs, "mod"),
                    BiOp::And => self.builder.build_and(lhs, rhs, "and"),
                    BiOp::Or  => self.builder.build_or(lhs, rhs, "or"),
                    BiOp::XOr => self.builder.build_xor(lhs, rhs, "xor"),
                    BiOp::LSh => self.builder.build_left_shift(lhs, rhs, "lsh"),
                    BiOp::RSh => self.builder.build_right_shift(lhs, rhs, false, "rsh")
                }
            },
            Expr::Cast { typ, val } => {
                let val = self.compile_expr(val, prefer);
                let typ = self.as_llvm_type(typ);
                self.builder.build_cast(InstructionOpcode::BitCast, val, BasicTypeEnum::try_from(typ).unwrap(), "cast").into_int_value()
            },
            _ => todo!()
        }
    }

    fn as_llvm_type(&self, typ: &Type) -> AnyTypeEnum<'ctx> {
        match typ {
            Type::Name(v) => {
                match v.as_str() {
                    "void"
                        => AnyTypeEnum::VoidType(self.context.void_type()),
                    "i8"
                        => AnyTypeEnum::IntType(self.context.i8_type()),
                    "i16"
                        => AnyTypeEnum::IntType(self.context.i16_type()),
                    "int" | "i32"
                        => AnyTypeEnum::IntType(self.context.i32_type()),
                    "i64"
                        => AnyTypeEnum::IntType(self.context.i64_type()),
                    _ => panic!()
                }
            }
            _ => todo!()
        }
    }

    fn args_to_metadata(&mut self, args: &Vec<FuncDefArg>) -> Vec<BasicMetadataTypeEnum<'ctx>> {
        let mut ret = Vec::new();
        for typ in args {
            ret.push(BasicMetadataTypeEnum::try_from(self.as_llvm_type(&(*typ).clone().typ)).unwrap());
        }
        ret
    }
}
