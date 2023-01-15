use std::{collections::HashMap};

use codegem::ir::{ModuleBuilder, Module, Value, FunctionId, VariableId, Type as IRType, ToIntegerOperation, Operation, Terminator};

use crate::ast::nodes::{Program, Node, FuncDefArg, Expr, BoolOp, CompOp};

pub fn compiler(prog: Program) -> Module {
    let mut builder = ModuleBuilder::default()
        .with_name("FunnLang");
    let mut functions: HashMap<String, FunctionId> = HashMap::new();
    let mut variables: HashMap<String, VariableId> = HashMap::new();

    compile(prog, &mut builder, &mut functions, &mut variables);
    builder.build()
}

fn compile(prog: Program, builder: &mut ModuleBuilder, functions: &mut HashMap<String, FunctionId>, variables: &HashMap<String, VariableId>) {
    let mut vars = variables.clone();
    for statement in prog.body {
        match statement {
            Node::FuncDefine { func_name, func_args, func_type, func_body } => {
                let func = builder.new_function(&func_name, 
                    &(funnlang_arg_to_ir_arg(func_args)
                        .iter()
                        .map(|(a, n)| (a.as_str(), n.clone())).collect::<Vec<(&str, IRType)>>()), 
                    &func_type.to_ir_type());
                builder.switch_to_function(func);
                functions.insert(func_name, func);
                let b = builder.push_block().unwrap();
                builder.switch_to_block(b);
                compile(func_body, builder, functions, &vars);
                builder.set_terminator(Terminator::ReturnVoid);
            }
            Node::VarDefine { var_type, var_name, val_expr } => {
                let var = builder.push_variable(&var_name, &var_type.clone().to_ir_type()).unwrap();
                vars.insert(var_name, var);
                let expr = compile_expr(val_expr, builder, functions, &vars, &var_type.clone().to_ir_type());
                builder.push_instruction(&var_type.to_ir_type(), Operation::SetVar(var, expr));
            }
            Node::While { cond, body } => {
                let cond_block = builder.push_block().unwrap();
                builder.set_terminator(Terminator::Jump(cond_block));
                builder.switch_to_block(cond_block);
                let body_block = builder.push_block().unwrap();
                let end_block = builder.push_block().unwrap();
                let cond = compile_expr(cond, builder, functions, &vars, &IRType::Integer(true, 32));
                builder.set_terminator(Terminator::Branch(cond, body_block, end_block));
                builder.switch_to_block(body_block);
                compile(body, builder, functions, &vars);
                builder.set_terminator(Terminator::Jump(cond_block));
                builder.switch_to_block(end_block);
            }
            Node::VarAssign { var_name, val_expr } => {
                let val = compile_expr(val_expr, builder, functions, &vars, &IRType::Integer(true, 32));
                builder.push_instruction(&IRType::Void, Operation::SetVar(vars[&var_name], val));
            }
            _ => todo!(),
        }
    }
}

fn compile_expr(expr: Expr, builder: &mut ModuleBuilder, functions: &mut HashMap<String, FunctionId>, variables: &HashMap<String, VariableId>, typ: &IRType) -> Value {
    match expr {
        Expr::Number(n) => builder.push_instruction(typ, n.to_integer_operation()).unwrap(),
        Expr::BoolOp { left, oper, right } => {
            match oper {
                BoolOp::Add => {
                    let left = compile_expr(*left, builder, functions, variables, typ);
                    let right = compile_expr(*right, builder, functions, variables, typ);
                    builder.push_instruction(typ, Operation::Add(left, right)).unwrap()
                }
                BoolOp::Sub => {
                    let left = compile_expr(*left, builder, functions, variables, typ);
                    let right = compile_expr(*right, builder, functions, variables, typ);
                    builder.push_instruction(typ, Operation::Sub(left, right)).unwrap()
                }
                BoolOp::Mul => {
                    let left = compile_expr(*left, builder, functions, variables, typ);
                    let right = compile_expr(*right, builder, functions, variables, typ);
                    builder.push_instruction(typ, Operation::Mul(left, right)).unwrap()
                }
                BoolOp::Div => {
                    let left = compile_expr(*left, builder, functions, variables, typ);
                    let right = compile_expr(*right, builder, functions, variables, typ);
                    builder.push_instruction(typ, Operation::Div(left, right)).unwrap()
                }
                BoolOp::Mod => {
                    let left = compile_expr(*left, builder, functions, variables, typ);
                    let right = compile_expr(*right, builder, functions, variables, typ);
                    builder.push_instruction(typ, Operation::Mod(left, right)).unwrap()
                }
                BoolOp::And => {
                    let left = compile_expr(*left, builder, functions, variables, typ);
                    let right = compile_expr(*right, builder, functions, variables, typ);
                    builder.push_instruction(typ, Operation::BitAnd(left, right)).unwrap()
                }
                BoolOp::Or => {
                    let left = compile_expr(*left, builder, functions, variables, typ);
                    let right = compile_expr(*right, builder, functions, variables, typ);
                    builder.push_instruction(typ, Operation::BitOr(left, right)).unwrap()
                }
                BoolOp::XOr => {
                    let left = compile_expr(*left, builder, functions, variables, typ);
                    let right = compile_expr(*right, builder, functions, variables, typ);
                    builder.push_instruction(typ, Operation::BitXor(left, right)).unwrap()
                }
            }
        }
        Expr::Ident(i) => {
            builder.push_instruction(typ, Operation::GetVar(variables[&i])).unwrap()
        }
        Expr::CompOp { left, oper, right } => {
            let lhs = compile_expr(*left, builder, functions, variables, typ);
            let rhs = compile_expr(*right, builder, functions, variables, typ);
            match oper {
                CompOp::EQ => {
                    builder.push_instruction(&typ, Operation::Eq(lhs, rhs)).unwrap()
                }
                CompOp::NEQ => {
                    builder.push_instruction(&typ, Operation::Ne(lhs, rhs)).unwrap()
                }
                CompOp::GT => {
                    builder.push_instruction(&typ, Operation::Gt(lhs, rhs)).unwrap()
                }
                CompOp::GTE => {
                    builder.push_instruction(&typ, Operation::Ge(lhs, rhs)).unwrap()
                }
                CompOp::LT => {
                    builder.push_instruction(&typ, Operation::Lt(lhs, rhs)).unwrap()
                }
                CompOp::LTE => {
                    builder.push_instruction(&typ, Operation::Le(lhs, rhs)).unwrap()
                }
            }
        }
        _ => todo!("Unimplimented expression type.")
    }
}

fn funnlang_arg_to_ir_arg(args: Vec<FuncDefArg>) -> Vec<(String, IRType)> {
    let mut ret = Vec::new();
    for arg in args {
        ret.push((arg.name, arg.typ.to_ir_type()));
    }
    ret
}