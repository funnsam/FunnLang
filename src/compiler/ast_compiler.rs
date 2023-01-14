use std::{collections::HashMap, hash::Hash};

use codegem::ir::{ModuleBuilder, Module, Value, FunctionId, VariableId};

use crate::ast::nodes::Program;

pub fn compiler(prog: Program) -> Module {
    let mut builder = ModuleBuilder::default()
        .with_name("FunnLang");
    let mut functions: HashMap<String, FunctionId> = HashMap::new();
    let mut variables: HashMap<String, VariableId> = HashMap::new();

    compile(prog, &mut builder, &mut functions, &mut variables);

    builder.build()
}

fn compile(prog: Program, builder: &mut ModuleBuilder, functions: &mut HashMap<String, FunctionId>, variables: &mut HashMap<String, VariableId>) {
    for statement in prog.body {
        match statement {
            _ => todo!(),
        }
    }
}