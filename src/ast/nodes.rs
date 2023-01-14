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
    Break,
    Continue,
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
    Cast(Type)
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
                => write!(f, "Define variable"),
            VarAssign { var_name: _, val_expr: _ }
                => write!(f, "Assign variable"),
            FuncDefine { func_name: _, func_args: _, func_type: _, func_body: _ }
                => write!(f, "Define function"),
            FuncCall { func_name: _, func_args: _ }
                => write!(f, "Function call"),
            While { cond: _, body: _ }
                => write!(f, "While loop"),
            For { loopv: _, from: _, to: _, body: _ }
                => write!(f, "For loop"),
            Break
                => write!(f, "Break"),
            Continue
                => write!(f, "Continue"),
            Branch { cond: _, body: _ }
                => write!(f, "Conditional branch"),
            CodeBlock(_)
                => write!(f, "Block"),
            AsmBlock(_)
                => write!(f, "Assembly code block"),
            Return(_)
                => write!(f, "Return")
        }
    }
}