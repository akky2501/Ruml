use id::*;
use typing::*;

#[derive(Debug, Clone)]
pub enum Ast {
    Unit,
    Int(i64),
    //Float(f64),
    //String(String),
    Bool(bool),

    Var(Id, Type),
    //Anotation () type anotation expression
    Seq (Vec<Ast>, Type),
    Logical { lhs: Box<Ast>, op: Id, rhs: Box<Ast>, type_: Type },
    BinaryOp { lhs: Box<Ast>, op: Id, rhs: Box<Ast>, type_: Type },
    UnaryOp { op: Id, expr: Box<Ast>, type_: Type },
    Apply { func: Box<Ast>, args: Vec<Ast>, type_: Type },
    Ctor { ctor: Id, args: Option<Box<Ast>>, type_: Type },
    Tuple(Vec<Ast>, Type),

    If { cond: Box<Ast>, then: Box<Ast>, else_: Box<Ast>, type_: Type },
    Match { expr: Box<Ast>, arms: Vec<PatternArm>, type_: Type },

    Let { name: (Id, Type), expr: Box<Ast>, cont: Box<Ast>, type_: Type},
    LetRec { funcs: Vec<FuncInfo>, cont: Box<Ast>, type_: Type},
    LetTuple { names: Vec<(Id, Type)>, expr: Box<Ast>, cont: Box<Ast>, type_: Type},

    // TypeAlias {},
    DataDecl { name: Id, parameter: Vec<Id>, ctors: Vec<DataDeclInfo> },
}

#[derive(Debug, Clone)]
pub struct PatternArm {
    pub pattern: PatternAst,
    pub guard: Option<Ast>,
    pub expr: Ast,
    //pub type_: Type,
}

#[derive(Debug, Clone)]
pub enum PatternAst {
    Wildcard(Id),
    Discard,
    Unit,
    Int(i64),
    Bool(bool),
    Tuple(Vec<PatternAst>),
    DataDestruct{data: Id, args: Vec<PatternAst>},
    //As{ pattern: Box<PatternAst>, bind: Id },
}
/*
wildcard
discard
constant
destruct
as
or
range
*/

#[derive(Debug, Clone)]
pub struct FuncInfo {
    pub name: Id,
    pub formals: Vec<(Id, Type)>,
    pub expr: Ast,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct DataDeclInfo {
    pub name: Id,
    pub formal: Option<TypeAst>,
}

#[derive(Debug, Clone)]
pub enum TypeAst {
    Func(Vec<TypeAst>, Box<TypeAst>),
    Tuple(Vec<TypeAst>),
    Unit,
    Int,
    //Float,
    Bool,
    Data{ data_type: Id, args: Vec<TypeAst> },
    Var(Id),
}

