use std::{collections::HashMap, fmt::Display, io::{Write, self}};

use {
    codegem::ir::{Operation, Terminator, Value, Linkage},
    codegem::regalloc::RegisterAllocator,
};

use codegem::arch::{Instr, InstructionSelector, Location, VCode, VCodeGenerator, VReg, Function};

pub const AA64_REGISTER_ZERO: usize = 0;
pub const AA64_REGISTER_X0  : usize = 1;
pub const AA64_REGISTER_X1  : usize = 2;
pub const AA64_REGISTER_X2  : usize = 3;
pub const AA64_REGISTER_X3  : usize = 4;
pub const AA64_REGISTER_X4  : usize = 5;
pub const AA64_REGISTER_X5  : usize = 6;
pub const AA64_REGISTER_X6  : usize = 7;
pub const AA64_REGISTER_X7  : usize = 8;
pub const AA64_REGISTER_X8  : usize = 9;
pub const AA64_REGISTER_X9  : usize = 10;
pub const AA64_REGISTER_X10 : usize = 11;
pub const AA64_REGISTER_X11 : usize = 12;
pub const AA64_REGISTER_X12 : usize = 13;
pub const AA64_REGISTER_X13 : usize = 14;
pub const AA64_REGISTER_X14 : usize = 15;
pub const AA64_REGISTER_X15 : usize = 16;
pub const AA64_REGISTER_IP0 : usize = 17;
pub const AA64_REGISTER_IP1 : usize = 18;
pub const AA64_REGISTER_X18 : usize = 19;
pub const AA64_REGISTER_X19 : usize = 20;
pub const AA64_REGISTER_X20 : usize = 21;
pub const AA64_REGISTER_X21 : usize = 22;
pub const AA64_REGISTER_X22 : usize = 23;
pub const AA64_REGISTER_X23 : usize = 24;
pub const AA64_REGISTER_X24 : usize = 25;
pub const AA64_REGISTER_X25 : usize = 26;
pub const AA64_REGISTER_X26 : usize = 27;
pub const AA64_REGISTER_X27 : usize = 28;
pub const AA64_REGISTER_X28 : usize = 29;
pub const AA64_REGISTER_FP  : usize = 30;
pub const AA64_REGISTER_LR  : usize = 31;
pub const AA64_REGISTER_SP  : usize = 32;

pub enum AA64AluOp {
    Add,
    Sub,
    Mul,
    Div,
    Lsl,
    Lsr,
    And,
    Orr,
    Eor,
}

impl Display for AA64AluOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AA64AluOp::Add  => write!(f, "add"),
            AA64AluOp::Sub  => write!(f, "sub"),
            AA64AluOp::Mul  => write!(f, "mul"),
            AA64AluOp::Div  => write!(f, "udiv"),
            AA64AluOp::Lsl  => write!(f, "lsl"),
            AA64AluOp::Lsr  => write!(f, "lsr"),
            AA64AluOp::And  => write!(f, "and"),
            AA64AluOp::Orr  => write!(f, "orr"),
            AA64AluOp::Eor  => write!(f, "eor"),
        }
    }
}

pub enum AA64CompOp {
    EQ, NE,
    GT, GE,
    LT, LE
}

impl Display for AA64CompOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AA64CompOp::EQ  => write!(f, "eq"),
            AA64CompOp::NE  => write!(f, "ne"),
            AA64CompOp::GT  => write!(f, "gt"),
            AA64CompOp::GE  => write!(f, "ge"),
            AA64CompOp::LT  => write!(f, "lt"),
            AA64CompOp::LE  => write!(f, "le")
        }
    }
}

pub enum AA64Instruction {
    PhiPlaceholder {
        rd: VReg,
        options: Vec<(Location, Value)>,
    },
    
    Integer {
        rd: VReg,
        value: u64,
    },

    AluOp {
        op: AA64AluOp,
        rd: VReg,
        rx: VReg,
        ry: VReg,
    },

    AluOpImm {
        op: AA64AluOp,
        rd: VReg,
        rx: VReg,
        imm: i16,
    },

    Bl {
        rd: VReg,
        location: Location,
        clobbers: Vec<VReg>,
    },

    Bne {
        rx: VReg,
        ry: VReg,
        location: Location,
    },

    Ret,

    Load {
        rd: VReg,
        imm: i16,
        rx: VReg,
    },

    Store {
        rx: VReg,
        imm: i16,
        ry: VReg,
    },

    Compare {
        rx: VReg,
        ry: VReg
    },

    CondSet {
        rd: VReg,
        cnd: AA64CompOp
    },
}

impl Display for AA64Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AA64Instruction::PhiPlaceholder { rd, .. } => write!(f, "phi {} ...", rd),

            AA64Instruction::Integer { rd, value } => write!(f, "ldr {}, ={}", rd, value),

            AA64Instruction::AluOp { op, rd, rx, ry } => write!(f, "{} {}, {}, {}", op, rd, rx, ry),
            AA64Instruction::AluOpImm { op, rd, rx, imm } => write!(f, "{} {}, {}, {}", op, rd, rx, imm),

            AA64Instruction::Bl { rd, location, .. } => write!(f, "bl {}, {}", rd, location),

            AA64Instruction::Bne { rx, ry, location } => {
                write!(f, "bne {}, {}, {}", rx, ry, location)
            }

            AA64Instruction::Ret => write!(f, "ret"),

            AA64Instruction::Load { rd, imm, rx } => write!(f, "load {}, {}({})", rd, imm, rx),

            AA64Instruction::Store { rx, imm, ry } => write!(f, "store {}, {}({})", rx, imm, ry),

            AA64Instruction::Compare { rx, ry } => write!(f, "cmp {}, {}", rx, ry),

            AA64Instruction::CondSet { rd, cnd } => write!(f, "cset {}, {}", rd, cnd)
        }
    }
}

impl Instr for AA64Instruction {
    fn get_regs() -> Vec<VReg> {
        vec![
            VReg::RealRegister(AA64_REGISTER_X0),
            VReg::RealRegister(AA64_REGISTER_X1),
            VReg::RealRegister(AA64_REGISTER_X2),
            VReg::RealRegister(AA64_REGISTER_X3),
            VReg::RealRegister(AA64_REGISTER_X4),
            VReg::RealRegister(AA64_REGISTER_X5),
            VReg::RealRegister(AA64_REGISTER_X6),
            VReg::RealRegister(AA64_REGISTER_X7),
            VReg::RealRegister(AA64_REGISTER_X8),
            VReg::RealRegister(AA64_REGISTER_X9),
            VReg::RealRegister(AA64_REGISTER_X10),
            VReg::RealRegister(AA64_REGISTER_X11),
            VReg::RealRegister(AA64_REGISTER_X12),
            VReg::RealRegister(AA64_REGISTER_X13),
            VReg::RealRegister(AA64_REGISTER_X14),
            VReg::RealRegister(AA64_REGISTER_X15),
            VReg::RealRegister(AA64_REGISTER_IP0),
            VReg::RealRegister(AA64_REGISTER_IP1),
            VReg::RealRegister(AA64_REGISTER_X18),
            VReg::RealRegister(AA64_REGISTER_X19),
            VReg::RealRegister(AA64_REGISTER_X20),
            VReg::RealRegister(AA64_REGISTER_X21),
            VReg::RealRegister(AA64_REGISTER_X22),
            VReg::RealRegister(AA64_REGISTER_X23),
            VReg::RealRegister(AA64_REGISTER_X24),
            VReg::RealRegister(AA64_REGISTER_X25),
            VReg::RealRegister(AA64_REGISTER_X26),
            VReg::RealRegister(AA64_REGISTER_X27),
            VReg::RealRegister(AA64_REGISTER_X28),
            VReg::RealRegister(AA64_REGISTER_FP),
            VReg::RealRegister(AA64_REGISTER_LR),
        ]
    }

    fn get_arg_regs() -> Vec<VReg> {
        vec![
            VReg::RealRegister(AA64_REGISTER_X0),
            VReg::RealRegister(AA64_REGISTER_X1),
            VReg::RealRegister(AA64_REGISTER_X2),
            VReg::RealRegister(AA64_REGISTER_X3),
            VReg::RealRegister(AA64_REGISTER_X4),
            VReg::RealRegister(AA64_REGISTER_X5),
            VReg::RealRegister(AA64_REGISTER_X6),
            VReg::RealRegister(AA64_REGISTER_X7),
        ]
    }

    fn collect_registers<A>(&self, alloc: &mut A)
    where
        A: RegisterAllocator,
    {
        match self {
            AA64Instruction::PhiPlaceholder { .. } => (),

            AA64Instruction::Integer { rd, .. } => {
                alloc.add_def(*rd);
            }

            AA64Instruction::AluOp { rd, rx, ry, .. } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            AA64Instruction::AluOpImm { rd, rx, .. } => {
                alloc.add_def(*rd);
                alloc.add_use(*rx);
            }
            
            AA64Instruction::Bl { clobbers, .. } => {
                for (clobber, arg) in clobbers.iter().zip(AA64Instruction::get_arg_regs().into_iter()) {
                    alloc.add_use(*clobber);
                    alloc.force_same(*clobber, arg);
                }
            }

            AA64Instruction::Bne { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            }

            AA64Instruction::Ret => (),
            AA64Instruction::Load { .. } => (),
            AA64Instruction::Store { .. } => (),

            AA64Instruction::Compare { rx, ry, .. } => {
                alloc.add_use(*rx);
                alloc.add_use(*ry);
            },

            AA64Instruction::CondSet { rd, .. } => {
                alloc.add_use(*rd);
            }
        }
    }

    fn apply_reg_allocs(&mut self, alloc: &HashMap<VReg, VReg>) {
        match self {
            AA64Instruction::PhiPlaceholder { .. } => (),

            AA64Instruction::Integer { rd, .. } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
            }

            AA64Instruction::AluOp { rd, rx, ry, .. } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
                if let Some(new) = alloc.get(rx) {
                    *rx = *new;
                }
                if let Some(new) = alloc.get(ry) {
                    *ry = *new;
                }
            }

            AA64Instruction::AluOpImm { rd, rx, .. } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
                if let Some(new) = alloc.get(rx) {
                    *rx = *new;
                }
            }

            AA64Instruction::Bl { rd, .. } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
            }

            AA64Instruction::Bne { rx, ry, .. } => {
                if let Some(new) = alloc.get(rx) {
                    *rx = *new;
                }
                if let Some(new) = alloc.get(ry) {
                    *ry = *new;
                }
            }

            AA64Instruction::Ret => (),

            AA64Instruction::Load { .. } => (),

            AA64Instruction::Store { .. } => (),

            AA64Instruction::Compare { rx, ry, .. } => {
                if let Some(new) = alloc.get(rx) {
                    *rx = *new;
                }
                if let Some(new) = alloc.get(ry) {
                    *ry = *new;
                }
            },

            AA64Instruction::CondSet { rd, .. } => {
                if let Some(new) = alloc.get(rd) {
                    *rd = *new;
                }
            },
        }
    }

    fn mandatory_transforms(vcode: &mut VCode<Self>) {
        for func in vcode.functions.iter_mut() {
            for labelled in func.labels.iter_mut() {
                let mut swaps = Vec::new();
                #[derive(Copy, Clone)]
                enum SwapType {
                    Rd,
                    Rx,
                    Ry,
                }
                use SwapType::*;

                for (i, instruction) in labelled.instructions.iter_mut().enumerate() {
                    match instruction {
                        AA64Instruction::PhiPlaceholder { .. } => (),

                        AA64Instruction::Integer { rd, .. } => {
                            if let VReg::Spilled(spill) = *rd {
                                swaps.push((i, spill, Rd));
                                *rd = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                        }

                        AA64Instruction::AluOp { rd, rx, ry, .. } => {
                            if let VReg::Spilled(spill) = *rx {
                                swaps.push((i, spill, Rx));
                                *rx = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                            if let VReg::Spilled(spill) = *ry {
                                swaps.push((i, spill, Ry));
                                *ry = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                            if let VReg::Spilled(spill) = *rd {
                                swaps.push((i, spill, Rd));
                                *rd = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                        }

                        AA64Instruction::AluOpImm { rd, rx, .. } => {
                            if let VReg::Spilled(spill) = *rx {
                                swaps.push((i, spill, Rx));
                                *rx = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                            if let VReg::Spilled(spill) = *rd {
                                swaps.push((i, spill, Rd));
                                *rd = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                        }

                        AA64Instruction::Bl { .. } => (),

                        AA64Instruction::Bne { rx, ry, .. } => {
                            if let VReg::Spilled(spill) = *rx {
                                swaps.push((i, spill, Rx));
                                *rx = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                            if let VReg::Spilled(spill) = *ry {
                                swaps.push((i, spill, Ry));
                                *rx = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                        }

                        AA64Instruction::Ret => (),

                        AA64Instruction::Load { .. } => (),

                        AA64Instruction::Store { .. } => (),

                        AA64Instruction::Compare { rx, ry, .. } => {
                            if let VReg::Spilled(spill) = *rx {
                                swaps.push((i, spill, Rx));
                                *rx = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                            if let VReg::Spilled(spill) = *ry {
                                swaps.push((i, spill, Ry));
                                *rx = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                        },

                        AA64Instruction::CondSet { rd, .. } => {
                            if let VReg::Spilled(spill) = *rd {
                                swaps.push((i, spill, Rd));
                                *rd = VReg::RealRegister(AA64_REGISTER_IP0);
                            }
                        }
                    }
                }

                for (index, spill, swap_type) in swaps.into_iter().rev() {
                    match swap_type {
                        Rd => {
                            labelled.instructions.insert(index + 1, AA64Instruction::Store {
                                rx: VReg::RealRegister(AA64_REGISTER_IP0),
                                imm: spill as i16 * -8,
                                ry: VReg::RealRegister(AA64_REGISTER_FP),
                            });
                        }

                        Rx => {
                            labelled.instructions.insert(index, AA64Instruction::Load {
                                rd: VReg::RealRegister(AA64_REGISTER_IP0),
                                imm: spill as i16 * -8,
                                rx: VReg::RealRegister(AA64_REGISTER_FP),
                            });
                        }

                        Ry => {
                            labelled.instructions.insert(index, AA64Instruction::Load {
                                rd: VReg::RealRegister(AA64_REGISTER_IP0),
                                imm: spill as i16 * -8,
                                rx: VReg::RealRegister(AA64_REGISTER_FP),
                            });
                        }
                    }
                }
            }
        }
    }

    fn emit_assembly(file: &mut impl Write, vcode: &VCode<Self>) -> io::Result<()> {
        for func in vcode.functions.iter() {
            match func.linkage {
                Linkage::External => {
                    writeln!(file, ".extern {}", func.name)?;
                    continue;
                }

                Linkage::Private => (),

                Linkage::Public => {
                    writeln!(file, ".global {}", func.name)?;
                }
            }

            writeln!(file, "{}:", func.name)?;
            for instruction in func.pre_labels.iter() {
                write_instruction(file, vcode, func, instruction)?;
            }
            for (i, labelled) in func.labels.iter().enumerate() {
                writeln!(file, ".{}.L{}:", func.name, i)?;
                for instruction in labelled.instructions.iter() {
                    write_instruction(file, vcode, func, instruction)?;
                }
            }

            writeln!(file)?;
        }
        Ok(())
    }
}

fn write_instruction(file: &mut impl Write, vcode: &VCode<AA64Instruction>, func: &Function<AA64Instruction>, instruction: &AA64Instruction) -> io::Result<()> {
    match instruction {
        AA64Instruction::PhiPlaceholder { .. } => (),

        AA64Instruction::Integer { rd, value } => {
            writeln!(file, "    ldr {}, ={}", register(*rd), value)?;
        }

        AA64Instruction::AluOp { op, rd, rx, ry } => {
            writeln!(file, "    {} {}, {}, {}", op, register(*rd), register(*rx), register(*ry))?;
        }

        AA64Instruction::AluOpImm { op: AA64AluOp::Sub, rd, rx, imm } => {
            writeln!(file, "    addi {}, {}, {}", register(*rd), register(*rx), -imm)?;
        }

        AA64Instruction::AluOpImm { op, rd, rx, imm } => {
            writeln!(file, "    {} {}, {}, {}", op, register(*rd), register(*rx), imm)?;
        }

        AA64Instruction::Bl { rd: _, location, .. } => {
            match *location {
                Location::InternalLabel(_) => {
                    writeln!(file, "    bl .{}{}", func.name, location)?;
                }
                Location::Function(f) => {
                    writeln!(file, "    bl {}", vcode.functions[f].name)?;
                }
            }
        }

        AA64Instruction::Bne { rx, ry, location } => {
            match *location {
                Location::InternalLabel(_) => {
                    writeln!(file, "    cmp {}, {}", register(*rx), register(*ry))?;
                    writeln!(file, "    bne .{}{}", func.name, location)?;
                }
                Location::Function(f) => {
                    writeln!(file, "    cmp {}, {}", register(*rx), register(*ry))?;
                    writeln!(file, "    bne {}", vcode.functions[f].name)?;
                }
            }
        }

        AA64Instruction::Ret => {
            for instruction in func.pre_return.iter() {
                write_instruction(file, vcode, func, instruction)?;
            }

            writeln!(file, "    ret")?;
        }

        AA64Instruction::Load { rd, imm, rx } => {
            writeln!(file, "    ldr {}, [{}, #{}]", register(*rd), register(*rx), imm)?;
        }

        AA64Instruction::Store { rx, imm, ry } => {
            writeln!(file, "    str {}, [{}, #{}]", register(*rx), register(*ry), imm)?;
        }

        AA64Instruction::Compare { rx, ry } => {
            writeln!(file, "    cmp {}, {}", register(*rx), register(*ry))?;
        }

        AA64Instruction::CondSet { rd, cnd } => {
            writeln!(file, "    cset {}, {}", register(*rd), cnd)?;
    }
    }

    Ok(())
}

fn register(reg: VReg) -> String {
    match reg {
        VReg::RealRegister(reg) => {
            String::from(match reg {
                v if v == AA64_REGISTER_ZERO => "xzr",
                v if v == AA64_REGISTER_X0  => "x0",
                v if v == AA64_REGISTER_X1  => "x1",
                v if v == AA64_REGISTER_X2  => "x2",
                v if v == AA64_REGISTER_X3  => "x3",
                v if v == AA64_REGISTER_X4  => "x4",
                v if v == AA64_REGISTER_X5  => "x5",
                v if v == AA64_REGISTER_X6  => "x6",
                v if v == AA64_REGISTER_X7  => "x7",
                v if v == AA64_REGISTER_X8  => "x8",
                v if v == AA64_REGISTER_X9  => "x9",
                v if v == AA64_REGISTER_X10 => "x10",
                v if v == AA64_REGISTER_X11 => "x11",
                v if v == AA64_REGISTER_X12 => "x12",
                v if v == AA64_REGISTER_X13 => "x13",
                v if v == AA64_REGISTER_X14 => "x14",
                v if v == AA64_REGISTER_X15 => "x15",
                v if v == AA64_REGISTER_IP0 => "x16",
                v if v == AA64_REGISTER_IP1 => "x17",
                v if v == AA64_REGISTER_X18 => "x18",
                v if v == AA64_REGISTER_X19 => "x19",
                v if v == AA64_REGISTER_X20 => "x20",
                v if v == AA64_REGISTER_X21 => "x21",
                v if v == AA64_REGISTER_X22 => "x22",
                v if v == AA64_REGISTER_X23 => "x23",
                v if v == AA64_REGISTER_X24 => "x24",
                v if v == AA64_REGISTER_X25 => "x25",
                v if v == AA64_REGISTER_X26 => "x26",
                v if v == AA64_REGISTER_X27 => "x27",
                v if v == AA64_REGISTER_X28 => "x28",
                v if v == AA64_REGISTER_FP  => "x29",
                v if v == AA64_REGISTER_LR  => "x30",
                v if v == AA64_REGISTER_SP  => "sp",
                _ => unreachable!(),
            })
        }

        VReg::Virtual(_) => unreachable!(),

        VReg::Spilled(s) => format!("#-{}(fp)", 8 * s),
    }
}

#[derive(Default)]
pub struct AA64Selector;

impl InstructionSelector for AA64Selector {
    type Instruction = AA64Instruction;

    fn select_pre_function_instructions(&mut self, gen: &mut VCodeGenerator<Self::Instruction, Self>) {
        gen.push_prelabel_instruction(AA64Instruction::AluOpImm {
            op: AA64AluOp::Add,
            rd: VReg::RealRegister(AA64_REGISTER_SP),
            rx: VReg::RealRegister(AA64_REGISTER_SP),
            imm: -16,
        });
        gen.push_prelabel_instruction(AA64Instruction::Store {
            rx: VReg::RealRegister(AA64_REGISTER_FP),
            imm: 8,
            ry: VReg::RealRegister(AA64_REGISTER_SP),
        });
        gen.push_prelabel_instruction(AA64Instruction::Store {
            rx: VReg::RealRegister(AA64_REGISTER_LR),
            imm: 0,
            ry: VReg::RealRegister(AA64_REGISTER_SP),
        });
        gen.push_prelabel_instruction(AA64Instruction::AluOpImm {
            op: AA64AluOp::Add,
            rd: VReg::RealRegister(AA64_REGISTER_FP),
            rx: VReg::RealRegister(AA64_REGISTER_SP),
            imm: 0,
        });

        // TODO: autodetect these
        let callee_saved_regs = [
            AA64_REGISTER_X19,
            AA64_REGISTER_X20,
            AA64_REGISTER_X21,
            AA64_REGISTER_X22,
            AA64_REGISTER_X23,
            AA64_REGISTER_X24,
            AA64_REGISTER_X25,
            AA64_REGISTER_X26,
            AA64_REGISTER_X27,
            AA64_REGISTER_X28,
            AA64_REGISTER_FP,
            AA64_REGISTER_ZERO
        ];
        gen.push_prelabel_instruction(AA64Instruction::AluOpImm {
            op: AA64AluOp::Add,
            rd: VReg::RealRegister(AA64_REGISTER_SP),
            rx: VReg::RealRegister(AA64_REGISTER_SP),
            imm: -(callee_saved_regs.len() as i16 * 8),
        });
        for (i, &reg) in callee_saved_regs.iter().enumerate() {
            gen.push_prelabel_instruction(AA64Instruction::Store {
                rx: VReg::RealRegister(reg),
                imm: (i as i16) * 8,
                ry: VReg::RealRegister(AA64_REGISTER_SP),
            });
        }
        for (i, &reg) in callee_saved_regs.iter().enumerate() {
            gen.push_prereturn_instruction(AA64Instruction::Load {
                rd: VReg::RealRegister(reg),
                imm: (i as i16) * 8,
                rx: VReg::RealRegister(AA64_REGISTER_SP),
            });
        }
        gen.push_prereturn_instruction(AA64Instruction::AluOpImm {
            op: AA64AluOp::Add,
            rd: VReg::RealRegister(AA64_REGISTER_SP),
            rx: VReg::RealRegister(AA64_REGISTER_SP),
            imm: callee_saved_regs.len() as i16 * 8,
        });

        gen.push_prereturn_instruction(AA64Instruction::AluOpImm {
            op: AA64AluOp::Add,
            rd: VReg::RealRegister(AA64_REGISTER_SP),
            rx: VReg::RealRegister(AA64_REGISTER_FP),
            imm: 0,
        });
        gen.push_prereturn_instruction(AA64Instruction::Load {
            rd: VReg::RealRegister(AA64_REGISTER_LR),
            imm: 0,
            rx: VReg::RealRegister(AA64_REGISTER_FP),
        });
        gen.push_prereturn_instruction(AA64Instruction::Load {
            rd: VReg::RealRegister(AA64_REGISTER_FP),
            imm: 8,
            rx: VReg::RealRegister(AA64_REGISTER_FP),
        });
        gen.push_prereturn_instruction(AA64Instruction::AluOpImm {
            op: AA64AluOp::Add,
            rd: VReg::RealRegister(AA64_REGISTER_SP),
            rx: VReg::RealRegister(AA64_REGISTER_SP),
            imm: 16,
        });
    }

    fn select_instr(
        &mut self,
        gen: &mut VCodeGenerator<Self::Instruction, Self>,
        result: Option<Value>,
        op: Operation,
    ) {
        let rd = match result {
            Some(val) => {
                gen.get_vreg(val)
            }

            None => VReg::RealRegister(AA64_REGISTER_ZERO),
        };

        match op {
            Operation::Identity(value) => {
                let rx = gen.get_vreg(value);
                gen.push_instruction(AA64Instruction::AluOp { op: AA64AluOp::Add, rd, rx, ry: VReg::RealRegister(AA64_REGISTER_ZERO) });
            }

            Operation::Integer(_signed, mut value) => {
                // TODO: better way to do this
                while value.len() < 8 {
                    value.push(0);
                }
                let value = u64::from_le_bytes(value[..8].try_into().unwrap());
                gen.push_instruction(AA64Instruction::Integer { rd, value });
            }

            Operation::Add(a, b)
            | Operation::Sub(a, b)
            | Operation::Mul(a, b)
            | Operation::Div(a, b)
            | Operation::Mod(a, b)
            | Operation::Bsl(a, b)
            | Operation::Bsr(a, b)
            | Operation::BitAnd(a, b)
            | Operation::BitOr(a, b)
            | Operation::BitXor(a, b) => {
                let rx = gen.get_vreg(a);
                let ry = gen.get_vreg(b);
                gen.push_instruction(AA64Instruction::AluOp {
                    op: match op {
                        Operation::Add(_, _)    => AA64AluOp::Add,
                        Operation::Sub(_, _)    => AA64AluOp::Sub,
                        Operation::Mul(_, _)    => AA64AluOp::Mul,
                        Operation::Div(_, _)    => AA64AluOp::Div,
                        Operation::Mod(_, _)    => todo!(),
                        Operation::Bsl(_, _)    => AA64AluOp::Lsl,
                        Operation::Bsr(_, _)    => AA64AluOp::Lsr,
                        Operation::BitAnd(_, _) => AA64AluOp::And,
                        Operation::BitOr(_, _)  => AA64AluOp::Orr,
                        Operation::BitXor(_, _) => AA64AluOp::Eor,
                        _ => unreachable!(),
                    }, rd, rx, ry });
            }

            Operation::Eq(a, b) => {
                let rx = gen.get_vreg(b);
                let ry = gen.get_vreg(a);
                let rd = gen.new_unassociated_vreg();
                gen.push_instruction(AA64Instruction::Compare { rx, ry });
                gen.push_instruction(AA64Instruction::CondSet { rd, cnd: AA64CompOp::EQ });
            }

            Operation::Ne(a, b) => {
                let rx = gen.get_vreg(b);
                let ry = gen.get_vreg(a);
                let rd = gen.new_unassociated_vreg();
                gen.push_instruction(AA64Instruction::Compare { rx, ry });
                gen.push_instruction(AA64Instruction::CondSet { rd, cnd: AA64CompOp::NE });
            }

            Operation::Lt(a, b) => {
                let rx = gen.get_vreg(b);
                let ry = gen.get_vreg(a);
                let rd = gen.new_unassociated_vreg();
                gen.push_instruction(AA64Instruction::Compare { rx, ry });
                gen.push_instruction(AA64Instruction::CondSet { rd, cnd: AA64CompOp::LT });
            }

            Operation::Le(a, b) => {
                let rx = gen.get_vreg(b);
                let ry = gen.get_vreg(a);
                let rd = gen.new_unassociated_vreg();
                gen.push_instruction(AA64Instruction::Compare { rx, ry });
                gen.push_instruction(AA64Instruction::CondSet { rd, cnd: AA64CompOp::LE });
            }

            Operation::Gt(a, b) => {
                let rx = gen.get_vreg(b);
                let ry = gen.get_vreg(a);
                let rd = gen.new_unassociated_vreg();
                gen.push_instruction(AA64Instruction::Compare { rx, ry });
                gen.push_instruction(AA64Instruction::CondSet { rd, cnd: AA64CompOp::GT });
            }

            Operation::Ge(a, b) => {
                let rx = gen.get_vreg(b);
                let ry = gen.get_vreg(a);
                let rd = gen.new_unassociated_vreg();
                gen.push_instruction(AA64Instruction::Compare { rx, ry });
                gen.push_instruction(AA64Instruction::CondSet { rd, cnd: AA64CompOp::GE });
            }

            Operation::Phi(mapping) => {
                gen.push_instruction(AA64Instruction::PhiPlaceholder {
                    rd,
                    options: mapping
                        .into_iter()
                        .filter_map(|(b, v)| {
                            if let Some(&l) = gen.label_map().get(&b) {
                                Some((Location::InternalLabel(l), v))
                            } else {
                                None
                            }
                        })
                        .collect(),
                });
            }

            Operation::GetVar(_) => unreachable!(),
            Operation::SetVar(_, _) => unreachable!(),

            Operation::Call(f, args) => {
                if let Some(&f) = gen.func_map().get(&f) {
                    // TODO: better way to do this
                    let mut save_regs = AA64Instruction::get_arg_regs();
                    save_regs.push(VReg::RealRegister(AA64_REGISTER_X9));
                    save_regs.push(VReg::RealRegister(AA64_REGISTER_X10));
                    save_regs.push(VReg::RealRegister(AA64_REGISTER_X11));
                    save_regs.push(VReg::RealRegister(AA64_REGISTER_X12));
                    save_regs.push(VReg::RealRegister(AA64_REGISTER_X13));
                    save_regs.push(VReg::RealRegister(AA64_REGISTER_X14));
                    save_regs.push(VReg::RealRegister(AA64_REGISTER_X15));
                    save_regs.push(VReg::RealRegister(AA64_REGISTER_ZERO));
                    gen.push_instruction(AA64Instruction::AluOpImm {
                        op: AA64AluOp::Add,
                        rd: VReg::RealRegister(AA64_REGISTER_SP),
                        rx: VReg::RealRegister(AA64_REGISTER_SP),
                        imm: -(save_regs.len() as i16 * 8),
                    });
                    for (i, &rx) in save_regs.iter().enumerate() {
                        gen.push_instruction(AA64Instruction::Store {
                            rx,
                            imm: i as i16 * 8,
                            ry: VReg::RealRegister(AA64_REGISTER_SP),
                        });
                    }

                    let clobbers: Vec<_> = args.into_iter().map(|v| {
                        let clobber = gen.new_unassociated_vreg();

                        let rx = gen.get_vreg(v);
                        gen.push_instruction(AA64Instruction::AluOp {
                            op: AA64AluOp::Add,
                            rd: clobber,
                            rx,
                            ry: VReg::RealRegister(AA64_REGISTER_ZERO),
                        });

                        clobber
                    }).collect();
                    gen.push_instruction(AA64Instruction::Bl {
                        rd: VReg::RealRegister(AA64_REGISTER_LR),
                        location: Location::Function(f),
                        clobbers,
                    });

                    gen.push_instruction(AA64Instruction::AluOp {
                        op: AA64AluOp::Add,
                        rd,
                        rx: VReg::RealRegister(AA64_REGISTER_X0),
                        ry: VReg::RealRegister(AA64_REGISTER_ZERO),
                    });

                    // TODO: better way of doing this
                    let rd_ = rd;
                    for (i, &rd) in save_regs.iter().enumerate() {
                        if rd == rd_ {
                            continue;
                        }
                        gen.push_instruction(AA64Instruction::Load {
                            rd,
                            imm: i as i16 * 8,
                            rx: VReg::RealRegister(AA64_REGISTER_SP),
                        });
                    }
                    gen.push_instruction(AA64Instruction::AluOpImm {
                        op: AA64AluOp::Add,
                        rd: VReg::RealRegister(AA64_REGISTER_SP),
                        rx: VReg::RealRegister(AA64_REGISTER_SP),
                        imm: (save_regs.len() as i16 * 8),
                    });
                }
            }

            Operation::CallIndirect(_, _) => todo!(),
            Operation::Load(_) => todo!(),
            Operation::Store(_, _) => todo!(),
            Operation::Bitcast(_, _) => todo!(),
            Operation::BitExtend(_, _) => todo!(),
            Operation::BitReduce(_, _) => todo!(),
        }
    }

    fn select_term(&mut self, gen: &mut VCodeGenerator<Self::Instruction, Self>, op: Terminator) {
        match op {
            Terminator::NoTerminator => (),

            Terminator::ReturnVoid => {
                gen.push_instruction(AA64Instruction::Ret);
            }

            Terminator::Return(v) => {
                let rx = gen.get_vreg(v);
                gen.push_instruction(AA64Instruction::AluOpImm {
                    op: AA64AluOp::Add,
                    rd: VReg::RealRegister(AA64_REGISTER_X0),
                    rx,
                    imm: 0,
                });
                gen.push_instruction(AA64Instruction::Ret);
            }

            Terminator::Jump(label) => {
                if let Some(&label) = gen.label_map().get(&label) {
                    gen.push_instruction(AA64Instruction::Bl {
                        rd: VReg::RealRegister(AA64_REGISTER_ZERO),
                        location: Location::InternalLabel(label),
                        clobbers: Vec::new(),
                    });
                }
            }

            Terminator::Branch(v, l1, l2) => {
                let rx = gen.get_vreg(v);
                if let Some(&l1) = gen.label_map().get(&l1) {
                    gen.push_instruction(AA64Instruction::Bne {
                        rx,
                        ry: VReg::RealRegister(AA64_REGISTER_ZERO),
                        location: Location::InternalLabel(l1),
                    });
                }
                if let Some(&l2) = gen.label_map().get(&l2) {
                    gen.push_instruction(AA64Instruction::Bl {
                        rd: VReg::RealRegister(AA64_REGISTER_ZERO),
                        location: Location::InternalLabel(l2),
                        clobbers: Vec::new(),
                    });
                }
            }
        }
    }

    fn post_function_generation(&mut self, func: &mut Function<Self::Instruction>, gen: &mut VCodeGenerator<Self::Instruction, Self>) {
        let mut v = Vec::new();
        for (i, labelled) in func.labels.iter().enumerate() {
            for (j, instr) in labelled.instructions.iter().enumerate() {
                if let AA64Instruction::PhiPlaceholder { .. } = instr {
                    v.push((i, j));
                }
            }
        }

        for (label_index, instr_index) in v.into_iter().rev() {
            let phi = func.labels[label_index].instructions.remove(instr_index);
            if let AA64Instruction::PhiPlaceholder { rd, options } = phi {
                for (label, v) in options {
                    if let Location::InternalLabel(label) = label {
                        let rx = gen.get_vreg(v);
                        let labelled = &mut func.labels[label];
                        labelled.instructions.insert(
                            labelled.instructions.len() - 1,
                            AA64Instruction::AluOp {
                                op: AA64AluOp::Add,
                                rd,
                                rx,
                                ry: VReg::RealRegister(AA64_REGISTER_ZERO),
                            },
                        );
                    }
                }
            }
        }
    }

    fn post_generation(&mut self, _vcode: &mut VCode<Self::Instruction>) { }
}
