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

use inkwell::module::Linkage;

impl InternLinkage {
    pub fn as_inkwell_linkage(&self) -> Option<Linkage> {
        match self {
            Self::Public    => None,
            Self::Private   => Some(Linkage::Private),
            Self::Extern    => Some(Linkage::External)
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
        ty: Type,
        from: Expr, to: Expr,
        body: Program,
        downward: bool
    },
    Break,
    Continue,
    Branch {
        cond: Vec<Expr>,
        body: Vec<Program>
    },
    CodeBlock(Program),
    AsmBlock(String, Option<(String, Vec<(Type, Expr)>)>),
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
}


#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(i64),
    Ident(String),
    Function{
        name    : String,
        args    : Vec<Expr>
    },
    BiOp {
        left    : Box<Expr>,
        oper    : BiOp,
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
pub enum BiOp {
    Add, Sub, Mul, Div, Mod,
    And, Or , XOr, LSh, RSh
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
            VarDefine { .. }
                => write!(f, "variable definition"),
            VarAssign { .. }
                => write!(f, "set variable"),
            FuncDefine { .. }
                => write!(f, "function definition"),
            FuncCall { func_name: _, func_args: _ }
                => write!(f, "function call"),
            While { .. }
                => write!(f, "while loop"),
            For { .. }
                => write!(f, "for loop"),
            Break
                => write!(f, "break"),
            Continue
                => write!(f, "continue"),
            Branch { .. }
                => write!(f, "conditional branch"),
            CodeBlock(_)
                => write!(f, "code block"),
            AsmBlock(..)
                => write!(f, "assembly code block"),
            Return(..)
                => write!(f, "return")
        }
    }
}
