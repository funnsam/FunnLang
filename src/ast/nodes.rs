use codegem::ir::Type as IRType;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Node>,
    pub escaped: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    VarDefine {
        var_type: Type,
        var_name: String,
        val_expr: Expr,
    },
    VarAssign {
        var_name: String,
        val_expr: Expr,
    },
    FuncDefine {
        func_name: String,
        func_args: Vec<FuncDefArg>,
        func_type: Type,
        func_body: Program
    },
    FuncCall {
        func_name: String,
        func_args: Vec<Expr>
    },
    While {
        cond: Expr,
        body: Program,
    },
    For {
        loopv: String,
        from: Expr, to: Expr,
        body: Program
    },
    Branch {
        cond: Vec<Expr>,
        body: Vec<Program>
    },
    CodeBlock(Program),
    AsmBlock(String),
    Return(Expr)
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDefArg {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Name(String),
    Pointer(Box<Type>),
    Array(Box<Type>, usize)
}

impl Type {
    pub fn to_ir_type(self) -> IRType {
        match self {
            Type::Name(n) => {
                match n.as_str() {
                    "i32" => IRType::Integer(true, 32),
                    "i16" => IRType::Integer(true, 16),
                    "i8" => IRType::Integer(true, 8),
                    "u32" => IRType::Integer(false, 32),
                    "u16" => IRType::Integer(false, 16),
                    "u8" => IRType::Integer(false, 8),
                    _ => todo!("Unimplimented type.")
                }
            }
            _ => todo!("Unimplimented type.")
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(i64),
    Ident(String),
    Function{
        name    : String,
        args    : Vec<Expr>
    },
    BoolOp {
        left    : Box<Expr>,
        oper    : BoolOp,
        right   : Box<Expr>
    },
    UnaryOp {
        oper    : UnaryOp,
        val     : Box<Expr>
    },
    CompOp {
        left    : Box<Expr>,
        oper    : CompOp,
        right   : Box<Expr>
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg, Abs, Not,
    Ptr, Drf
}

#[derive(Debug, Clone, PartialEq)]
pub enum BoolOp {
    Add, Sub, Mul, Div, Mod,
    And, Or , XOr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompOp {
    EQ, NEQ,
    LT, LTE,
    GT, GTE
}