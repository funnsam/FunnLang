use codegem::ir::{Type as IRType, Linkage};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Node>,
    pub escaped: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InternLinkage {
    Public, Private,
    Extern
}

impl InternLinkage {
    pub fn to_ir_linkage(&self) -> Linkage {
        match self {
            Self::Public    => Linkage::Public,
            Self::Private   => Linkage::Private,
            Self::Extern    => Linkage::External
        }
    }
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
        func_body: Program,
        linkage  : InternLinkage
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
    Break,
    Continue,
    Branch {
        cond: Vec<Expr>,
        body: Vec<Program>
    },
    CodeBlock(Program),
    AsmBlock(String),
    Return(Option<Expr>)
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
                    "i8"    => IRType::Integer(true, 8),
                    "i16"   => IRType::Integer(true, 16),
                    "i32"   => IRType::Integer(true, 32),
                    "i64"   => IRType::Integer(true, 64),

                    "u8"    => IRType::Integer(false, 8),
                    "u16"   => IRType::Integer(false, 16),
                    "u32"   => IRType::Integer(false, 32),
                    "u64"   => IRType::Integer(false, 64),

                    "int"   => IRType::Integer(true, 64),
                    "uint"  => IRType::Integer(false, 64),

                    "void"  => IRType::Void,
                    _ => todo!("Unimplimented type.")
                }
            },
            Type::Pointer(_t) => {
                // IRType::Pointer(t)
                IRType::Void
            },
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
    },
    Cast {
        typ : Type,
        val : Box<Expr>
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

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::Node::*;
        match self {
            VarDefine { var_type: _, var_name: _, val_expr: _ }
                => write!(f, "variable definition"),
            VarAssign { var_name: _, val_expr: _ }
                => write!(f, "set variable"),
            FuncDefine { func_name: _, func_args: _, func_type: _, func_body: _, linkage: _ }
                => write!(f, "function definition"),
            FuncCall { func_name: _, func_args: _ }
                => write!(f, "function call"),
            While { cond: _, body: _ }
                => write!(f, "while loop"),
            For { loopv: _, from: _, to: _, body: _ }
                => write!(f, "for loop"),
            Break
                => write!(f, "break"),
            Continue
                => write!(f, "continue"),
            Branch { cond: _, body: _ }
                => write!(f, "conditional branch"),
            CodeBlock(_)
                => write!(f, "code block"),
            AsmBlock(_)
                => write!(f, "assembly code block"),
            Return(_)
                => write!(f, "return")
        }
    }
}