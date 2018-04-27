
use id::*;
use typing::*;

#[derive(Debug)]
pub enum TypedAst {
    Unit,
    Int(i64),
    Bool(bool),

    Var(Id, Type),
    Seq (Vec<TypedAst>, Type),
    Logical { lhs: Box<TypedAst>, op: Id, rhs: Box<TypedAst>, ty: Type },
    BinaryOp { lhs: Box<TypedAst>, op: Id, rhs: Box<TypedAst>, ty: Type },
    UnaryOp { op: Id, expr: Box<TypedAst>, ty: Type },
    Apply { func: Box<TypedAst>, args: Vec<TypedAst>, ty: Type },
    Ctor { ctor: Id, args: Option<Box<TypedAst>>, ty: Type },
    Tuple(Vec<TypedAst>, Type),

    If { cond: Box<TypedAst>, then: Box<TypedAst>, else_: Box<TypedAst>, ty: Type },
    Match { expr: Box<TypedAst>, arms: Vec<TypedPatternArm>, ty: Type },

    Let { name: Id, bind_ty: Type, expr: Box<TypedAst>, cont: Box<TypedAst>,ty: Type},
    LetRec { funcs: Vec<TypedFuncInfo>, cont: Box<TypedAst>, ty: Type},
    LetTuple { names: Vec<Id>, expr: Box<TypedAst>, cont: Box<TypedAst>, ty: Type},
}

#[derive(Debug)]
pub struct TypedPatternArm {
    pub pattern: TypedPatternAst,
    pub guard: Option<TypedAst>,
    pub expr: TypedAst,
    pub type_: Type,
}

#[derive(Debug)]
pub enum TypedPatternAst {
    Wildcard(Id, Type),
    Discard,
    Unit,
    Int(i64),
    Bool(bool),
    Tuple(Vec<TypedPatternAst>, Type),
    DataDestruct{data: Id, args: Vec<TypedPatternAst>, type_: Type},
}

#[derive(Debug)]
pub struct TypedFuncInfo {
    pub name: Id,
    pub formals: Vec<Id>,
    pub expr: TypedAst,
    pub type_: Type
}
