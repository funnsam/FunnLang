use std::path::Path;
use std::collections::HashMap;

use inkwell::*;
use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::*;
use inkwell::types::*;
use inkwell::targets::*;

use crate::CompilerTarget;
use crate::ast::nodes::*;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    cur_fn: Option<FunctionValue<'ctx>>,
    vars: Vec<HashMap<String, (PointerValue<'ctx>, AnyTypeEnum<'ctx>)>>,
    loops: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>
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
            vars    : Vec::new(),
            loops   : Vec::new()
        };
        
        codegen.compile_ast(ast);

        // if emit_ir {
        //     codegen.module.print_to_stderr();
        // }

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
                    let args = self.args_to_metadata(func_args);
                    let ty = Self::as_fn_type(self.as_llvm_type(func_type), args);
                    let func = self.module.add_function(func_name, ty, linkage.as_inkwell_linkage());

                    if *linkage != InternLinkage::Extern {
                        self.cur_fn = Some(func);
                        let alloca_entry = self.context.append_basic_block(func, "__var_allocs");
                        let entry = self.context.append_basic_block(func, "__entry");

                        self.builder.position_at_end(alloca_entry);
                        let parms = func.get_params();
                        let types = ty.get_param_types();
                        for (i, el) in (*func_args).iter().enumerate() {
                            let a = self.builder.build_alloca(types[i], &format!("fn_arg_{}", &el.name));
                            self.builder.build_store(a, parms[i]);
                            self.vars.last_mut().unwrap().insert(el.name.to_owned(), (a, types[i].as_any_type_enum()));
                        }

                        self.builder.position_at_end(entry);
                        self.compile_ast(func_body);

                        self.builder.position_at_end(alloca_entry);
                        self.builder.build_unconditional_branch(entry);
                        self.cur_fn = None;
                    }
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
                Node::While { cond, body } => {
                    let cond_blk = self.context.append_basic_block(self.cur_fn.unwrap(), "__while_cond_blk");
                    let body_blk = self.context.append_basic_block(self.cur_fn.unwrap(), "__while_body_blk");
                    let end_blk  = self.context.append_basic_block(self.cur_fn.unwrap(), "__while_end_blk");
                    self.builder.build_unconditional_branch(cond_blk);
                    self.loops.push((cond_blk, end_blk));
                    
                    self.builder.position_at_end(cond_blk);
                    let cond = self.compile_expr(cond, &self.context.i32_type().try_into().unwrap());
                    self.builder.build_conditional_branch(cond, body_blk, end_blk);
                    
                    self.builder.position_at_end(body_blk);
                    self.compile_ast(body);
                    self.builder.build_unconditional_branch(cond_blk);

                    self.loops.pop();

                    let trueend = self.context.append_basic_block(self.cur_fn.unwrap(), "__while_true_end");
                    self.builder.position_at_end(end_blk);
                    self.builder.build_unconditional_branch(trueend);
                    self.builder.position_at_end(trueend);
                },
                Node::For { loopv, ty, from, to, body, downward } => {
                    let ty = BasicTypeEnum::try_from(self.as_llvm_type(ty)).unwrap().to_owned();
                    let from = self.compile_expr(from, &ty);
                    let to = self.compile_expr(to, &ty);

                    let aloc_blk = self.context.append_basic_block(self.cur_fn.unwrap(), "__for_alloc_blk");
                    let cond_blk = self.context.append_basic_block(self.cur_fn.unwrap(), "__for_cond_blk");
                    let body_blk = self.context.append_basic_block(self.cur_fn.unwrap(), "__for_body_blk");
                    let end_blk  = self.context.append_basic_block(self.cur_fn.unwrap(), "__for_end_blk");
                    self.loops.push((cond_blk, end_blk));
                    
                    self.builder.build_unconditional_branch(aloc_blk);
                    self.builder.position_at_end(aloc_blk);
                    let loopcounter = self.builder.build_alloca(ty, &format!("loopc_var_{loopv}"));
                    self.builder.build_store(loopcounter, from);
                    self.vars.last_mut().unwrap().insert(loopv.to_owned(), (loopcounter, ty.as_any_type_enum()));

                    self.builder.build_unconditional_branch(body_blk);
                    self.builder.position_at_end(cond_blk);

                    let lc = self.builder.build_load(ty, loopcounter, "lc_tmp");
                    let ok = if !downward {
                        let tmp = self.builder.build_int_add(lc.into_int_value(), ty.into_int_type().const_int(1, true), "lc_inc");
                        self.builder.build_store(loopcounter, tmp);

                        self.builder.build_int_compare(IntPredicate::ULT, lc.try_into().unwrap(), to, "lc_cmp")
                    } else {
                        let tmp = self.builder.build_int_sub(lc.into_int_value(), ty.into_int_type().const_int(1, true), "lc_dec");
                        self.builder.build_store(loopcounter, tmp);

                        self.builder.build_int_compare(IntPredicate::UGT, lc.try_into().unwrap(), to, "lc_cmp")
                    };

                    self.builder.build_conditional_branch(ok, body_blk, end_blk);

                    self.builder.position_at_end(body_blk);
                    self.compile_ast(body);
                    self.builder.build_unconditional_branch(cond_blk);

                    self.loops.pop();

                    let trueend = self.context.append_basic_block(self.cur_fn.unwrap(), "__for_true_end");
                    self.builder.position_at_end(end_blk);
                    self.builder.build_unconditional_branch(trueend);
                    self.builder.position_at_end(trueend);
                }
                Node::AsmBlock(asm, parm) => {
                    let parm = parm.to_owned().unwrap_or(("".to_owned(), vec![]));

                    let mut typs  = Vec::with_capacity(parm.1.len());
                    let mut exprs = Vec::with_capacity(parm.1.len());

                    for i in &parm.1 {
                        let typ = self.as_llvm_type(&i.0);
                        typs.push(BasicMetadataTypeEnum::try_from(typ).unwrap());
                        exprs.push(BasicMetadataValueEnum::try_from(self.compile_expr(&i.1, &typ.try_into().unwrap())).unwrap())
                    }

                    let asm_fn = self.context.void_type().fn_type(typs.as_slice(), false);
                    let asm = self.context.create_inline_asm(
                        asm_fn,
                        asm.to_owned(),
                        parm.0,
                        true,
                        false,
                        None,
                        false
                    );
                    self.builder.build_indirect_call(asm_fn, asm, exprs.as_slice(), "asmblk");
                },
                Node::Break => {
                    self.builder.build_unconditional_branch(self.loops.last().unwrap().1);
                    break;
                },
                Node::Continue => {
                    self.builder.build_unconditional_branch(self.loops.last().unwrap().0);
                    break;
                },
                Node::Branch { cond, body } => {
                    let mut all = Vec::new();
                    for (i, el) in cond.iter().enumerate() {
                        let expr = self.compile_expr(el, &self.context.i32_type().try_into().unwrap());

                        let body_blk = self.context.append_basic_block(self.cur_fn.unwrap(), "__if_body");
                        let end_blk  = self.context.append_basic_block(self.cur_fn.unwrap(), "__if_end");

                        self.builder.build_conditional_branch(expr, body_blk, end_blk);
                        self.builder.position_at_end(body_blk);
                        
                        self.compile_ast(&body[i]);

                        self.builder.position_at_end(end_blk);

                        all.push(body_blk);
                    }

                    let after_all = self.context.append_basic_block(self.cur_fn.unwrap(), "__after_if");

                    if cond.len() != body.len() {
                        self.compile_ast(body.last().unwrap());
                    }

                    self.builder.build_unconditional_branch(after_all);

                    for i in &all {
                        self.builder.position_at_end(*i);
                        self.builder.build_unconditional_branch(after_all);
                    }

                    self.builder.position_at_end(after_all);
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
            Expr::CompOp { left, oper, right } => {
                let lhs = self.compile_expr(left, prefer);
                let rhs = self.compile_expr(right, prefer);
                let rhs = self.builder.build_cast(
                    InstructionOpcode::BitCast, rhs, lhs.get_type(), "cmp_rhs_autocast"
                ).into_int_value();

                self.builder.build_int_compare(match oper {
                    CompOp::EQ  => IntPredicate::EQ,
                    CompOp::NEQ => IntPredicate::NE,
                    CompOp::LT  => IntPredicate::ULT,
                    CompOp::LTE => IntPredicate::ULE,
                    CompOp::GT  => IntPredicate::UGT,
                    CompOp::GTE => IntPredicate::UGE,
                }, lhs, rhs, "comparason")
            },
            Expr::Cast { typ, val } => {
                let val = self.compile_expr(val, prefer);
                let typ = self.as_llvm_type(typ);
                self.builder.build_cast(InstructionOpcode::BitCast, val, BasicTypeEnum::try_from(typ).unwrap(), "cast").into_int_value()
            },
            Expr::Function { name, args: _args } => {
                let func = self.module.get_function(name.as_str());
                let mut args = Vec::with_capacity(_args.len());
                for i in _args.iter() {
                    args.push(BasicMetadataValueEnum::try_from(self.compile_expr(i, prefer)).unwrap())
                }
                self.builder.build_call(func.unwrap(), args.as_slice(), &format!("call_{name}")).try_as_basic_value().unwrap_left().into_int_value()
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
