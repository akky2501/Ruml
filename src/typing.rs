use std::collections::HashSet;
use std::collections::HashMap;
use std::sync::Mutex;

use id::*;
use ast::*;
use constructor_table::*;

pub type TypeId = usize;

lazy_static! {
    static ref TYPE_VAR_NEXT_COUNT: Mutex<TypeId> = Mutex::new(0);
}

pub fn gen_var_count() -> TypeId {
    let r = *TYPE_VAR_NEXT_COUNT.lock().unwrap();
    *TYPE_VAR_NEXT_COUNT.lock().unwrap() += 1;
    r
}

#[derive(Debug, Clone)]
pub enum Type {
    None,
    Unit,
    Int,
    Bool,
    Func{ args: Vec<Type>, ret: Box<Type> },
    Tuple(Vec<Type>),
    Data{ name: Id, args: Vec<Type> },
    Var(TypeId),
}

impl Type {
    fn free_variables(&self) -> HashSet<TypeId> {
        fn fv_impl(ty: &Type, fv: &mut HashSet<TypeId>){
            use typing::Type::*;
            match ty {
                &Unit | &Bool | &Int => (),
                &Func{ ref args, ref ret } => { 
                    for x in args.iter() {
                        fv_impl(x, fv);
                    }
                    fv_impl(ret, fv);
                },
                &Tuple(ref t) => {
                    for x in t.iter() {
                        fv_impl(x, fv);
                    }
                },
                &Data{ name: _, ref args } => {
                    for x in args.iter() {
                        fv_impl(x, fv);
                    }
                },
                &Var(x) => {
                    fv.insert(x);
                },
                &None => unreachable!(),
            };
        }

        let mut fv = HashSet::new();
        fv_impl(self,&mut fv);
        fv
    }

    fn rewrite_variables(&mut self, from: TypeId, to: TypeId) {
        use typing::Type::*;
        match *self {
            Unit | Bool | Int => (),
            Func{ ref mut args, ref mut ret } => {
                    for i in args.iter_mut() {
                        i.rewrite_variables(from,to);
                    }
                    ret.rewrite_variables(from,to);
            },
            Tuple(ref mut x) => {
                for i in x.iter_mut(){
                    i.rewrite_variables(from,to);
                }
            },
            Data{ name: _, ref mut args } => {
                for i in args.iter_mut() {
                    i.rewrite_variables(from, to);
                }
            },
            Var(ref mut x) => {
                if *x == from {
                    *x = to;
                }
            },
            None => unreachable!(),
        }
    }

    fn subst(&mut self, var: TypeId, to: &Type){
        use typing::Type::*;
        match *self {
            Unit | Bool | Int => (),
            Func{ ref mut args, ref mut ret} => {
                    for i in args.iter_mut() {
                        i.subst(var,to);
                    }
                    ret.subst(var,to);
            },
            Tuple(ref mut x) => {
                for i in x.iter_mut(){
                    i.subst(var,to);
                }
            },
            Data{ name: _, ref mut args} => {
                for i in args.iter_mut() {
                    i.subst(var, to);
                }
            },
            Var(x) => {
                if x == var {
                    *self = to.clone();
                }
            },
            None => unreachable!(), // TODO: あやしい
        }

    }

    pub fn apply(&mut self, subst: &TypeSubst) {
        for &(ref var, ref to) in subst.equations.iter() {
            self.subst(*var, to);
        }
    }

    pub fn generalize(&self, env: &TypeEnv) -> TypeScheme {
        let tfv = self.free_variables();
        let efv = env.free_variables();
        let bind: HashSet<_> = tfv.difference(&efv).cloned().collect();
        TypeScheme{ bind: bind, body: self.clone() }
    }

}


#[derive(Debug,Clone)]
pub struct TypeScheme {
    pub bind: HashSet<TypeId>,
    pub body: Type,
}

impl TypeScheme {
    fn instantiate(&self) -> Type {
        let mut t = self.body.clone();
        for from in self.bind.iter() {
            let to = gen_var_count();
            t.rewrite_variables(*from, to);
        }
        t
    }

    fn free_variables(&self) -> HashSet<TypeId> {
        let fv = self.body.free_variables();
        fv.difference(&self.bind).cloned().collect()
    }
    
    fn apply(&mut self, subst: &TypeSubst) {
        for &(ref var, ref to) in subst.equations.iter() {
            if ! self.bind.contains(var) {
                self.body.subst(*var, to);
            }
        }
    }

}


#[derive(Debug, Clone)]
pub struct TypeEnv {
    seq: Vec<(Id,TypeScheme)>,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv{ seq: Vec::new() }
    }

    fn free_variables(&self) -> HashSet<TypeId> {
        let mut s = HashSet::new();
        for &(_,ref i) in &(self.seq) {
            s = s.union(&(i.free_variables())).cloned().collect();
        }
        s
    }

    fn push(&mut self, id: Id, ty: TypeScheme){
        self.seq.push((id, ty));
    }

    fn push_t(&mut self, id: Id, ty: Type) {
        self.seq.push((id, TypeScheme{bind: HashSet::new(), body: ty}));
    }

    fn pop(&mut self) {
        self.seq.pop();
    }

    fn contains(&self, id: &Id) -> Option<&TypeScheme> {
        if let Some(x) = self.seq.iter().rposition(|&(ref x, _)| *x == *id) {
            Some(&self.seq[x].1)
        }
        else { None }
    }

    fn apply(&mut self, subst: &TypeSubst) {
        for x in self.seq.iter_mut() {
            x.1.apply(subst);
        }
    }

}

#[derive(Debug)]
pub struct TypeSubst {
    equations: Vec<(TypeId,Type)>
}

impl TypeSubst {
    pub fn new() -> Self {
        TypeSubst{ equations: vec![] }
    }

    fn push(&mut self, var: TypeId, ty: Type) {
        self.equations.push((var,ty));
    }

    fn append(&mut self, mut s: TypeSubst) {
        self.equations.append(&mut s.equations);
    }

}


fn unify(mut c: Vec<(Type, Type)>) -> Result<TypeSubst, ()> {
    if let Some(last) = c.pop() {
        match last {
            (Type::Unit, Type::Unit) |
            (Type::Int,  Type::Int)  |
            (Type::Bool, Type::Bool) => {
                unify(c)
            },
            (Type::Var(x), Type::Var(y)) if x == y => {
                unify(c)
            },
            (Type::Var(x), t@_) |
            (t@_, Type::Var(x)) => {
                if ! t.free_variables().contains(&x) { // occur check
                    let mut subst = TypeSubst::new();
                    subst.push(x,t);
                    for i in c.iter_mut() {
                        i.0.apply(&subst); i.1.apply(&subst);
                    }
                    let s = unify(c)?;
                    subst.append(s); // 型代入の合成の順序は大事!
                    Ok(subst)
                }
                else { Err(()) }
            },
            (Type::Func{args: args1, ret: ret1}, Type::Func{args: args2, ret: ret2}) => {
                c.push((*ret1, *ret2));
                c.push((Type::Tuple(args1), Type::Tuple(args2)));
                unify(c)
            },
            (Type::Data{name: name1, args: args1}, Type::Data{name: name2, args: args2}) => {
                if name1 == name2 {
                    c.push((Type::Tuple(args1), Type::Tuple(args2)));
                    unify(c)
                } else { panic!() }
            },
            (Type::Tuple(elems1), Type::Tuple(elems2)) => {
                if elems1.len() == elems2.len() {
                    let mut t: Vec<_> = elems1.into_iter()
                                          .zip(elems2.into_iter()).collect();
                    c.append(&mut t);
                    unify(c)
                }
                else { Err(()) }
            },
            (_,_) => Err(()),
        }
    }
    else {
        Ok(TypeSubst::new())
    }
}

// Algorithm J
pub fn infer_types(exprs: &mut Vec<Ast>, table: &CtorTable) {
    let mut env = TypeEnv::new();
    let mut gsubst = TypeSubst::new();

    for expr in exprs.iter_mut() {
        infer(expr, &mut env, &mut gsubst, table).unwrap();
        match expr {
            &mut Ast::Let { name: (ref mut id, ref mut ty), expr: _, cont: _, type_: _} => {
                let t = ty.clone().generalize(&env);
                env.push(id.clone(), t);
            },
            &mut Ast::LetRec { ref mut funcs, cont: _, type_: _} => {
                for f in funcs.iter() {
                    let t = f.type_.clone().generalize(&env);
                    env.push(f.name.clone(), t);
                }
            },
            &mut Ast::LetTuple { ref mut names, expr: _, cont: _, type_: _} => {
                for i in names.iter() {
                    let t = i.1.clone().generalize(&env);
                    env.push(i.0.clone(), t);
                }
            },
            _ => unreachable!(),
        }
    }
    println!("{:?}", gsubst);
    for expr in exprs.iter_mut() {
        subst_expr(expr, &gsubst);
    }
}

fn infer(expr: &mut Ast, env: &mut TypeEnv, gsubst: &mut TypeSubst, table: &CtorTable) -> Result<Type, ()> {
    macro_rules! unify{
        ($e1:expr, $e2:expr) => {{
            let mut v = vec![($e1, $e2)];
            for x in v.iter_mut() {
                x.0.apply(gsubst);
                x.1.apply(gsubst);
            }
            let s = unify(v)?;
            gsubst.append(s);
        }};
    }

    macro_rules! infer{
        ($expr:expr) => {{
            infer($expr, env, gsubst, table)?
        }};

        ($id:ident : $ex:expr , $expr:expr) => {{
            let e = $ex;
            let i = $id.clone();
            env.push(i, e);
            let ty = infer!($expr);
            env.pop();
            ty
        }};
    }
    
    match *expr {
        Ast::Unit    => Ok(Type::Unit),
        Ast::Bool(_) => Ok(Type::Bool),
        Ast::Int(_)  => Ok(Type::Int),
        Ast::UnaryOp{ ref mut op, ref mut expr, ref mut type_} => {
            match op.as_str() {
                "-" => {
                    unify!(Type::Int, infer!(expr));
                    *type_ = Type::Int;
                    Ok(Type::Int)
                },
                _ => unimplemented!(),
            }
        },
        Ast::BinaryOp{ ref mut lhs, ref mut op, ref mut rhs, ref mut type_} => {
            match op.as_str() {
                "+" | "-" | "*" | "/" => {
                    let r1 = infer!(lhs);
                    let r2 = infer!(rhs);
                    unify!(r1.clone(), r2);
                    unify!(r1, Type::Int);
                    *type_ = Type::Int;
                    Ok(type_.clone())
                },
                "=" => {
                    unify!(infer!(lhs), infer!(rhs));
                    *type_ = Type::Bool;
                    Ok(type_.clone())
                },
                _ => unreachable!(),
            }
        },
        Ast::Seq (ref mut expr, ref mut ty) => {
            let n = expr.len();
            let mut c = 0;
            for e in expr.iter_mut() {
                let t = infer!(e);
                if c+1 != n {
                    unify!(Type::Unit, t);
                } else {
                    *ty = t;
                }
                c += 1;
            }
            Ok(ty.clone())
        },
        Ast::Logical { ref mut lhs, ref mut op, ref mut rhs, ref mut type_ } => {
            match op.as_str() {
                "||" | "&&" => {
                    let r1 = infer!(lhs);
                    let r2 = infer!(rhs);
                    unify!(r1.clone(), r2);
                    unify!(r1, Type::Bool);
                    *type_ = Type::Bool;
                    Ok(type_.clone())
                },
                _ => unreachable!(),
            }
        },
        Ast::If { ref mut cond, ref mut then, ref mut else_, ref mut type_ } => {
            let ty = infer!(cond);
            unify!(Type::Bool, ty);
            let ty = infer!(then);
            unify!(ty.clone(), infer!(else_));
            *type_ = ty.clone();
            Ok(ty)
        },
        Ast::Let { name: (ref name, ref mut ty), ref mut expr, ref mut cont, ref mut type_} => {
            let a = infer!(expr);
            unify!(ty.clone(), a);
            env.apply(gsubst);
            ty.apply(gsubst);
            *type_ = infer!(name : ty.generalize(env), cont);
            Ok(type_.clone())
        },
        Ast::Var(ref id, ref mut ty) => {
            if let Some(tys) = env.contains(id) {
                let r = tys.instantiate();
                *ty = r.clone();
                Ok(r)
            }
            else { Err(()) }
        },
        Ast::LetRec { ref mut funcs, ref mut cont, ref mut type_} => {
            for func in funcs.iter_mut() {
                let t = func.type_.clone().generalize(&env);
                env.push(func.name.clone(), t);
            }

            for func in funcs.iter_mut() {

                for i in func.formals.iter() {
                    env.push_t(i.0.clone(), i.1.clone());
                }

                let r = infer!(&mut func.expr); // TODO: body内にnameが変数として出てきたときに残る型変数への代入
                
                for _ in 0..func.formals.len() {
                    env.pop();
                }

                let a = Type::Func{
                    args: func.formals.iter().map(|&(_, ref t)| t.clone())
                            .collect::<Vec<_>>(),
                    ret: Box::new(r)};
                unify!(func.type_.clone(), a);
                env.apply(gsubst);
                
                func.type_.apply(gsubst);
                for i in func.formals.iter_mut() {
                    i.1.apply(gsubst);
                }
            }

            for _ in 0..funcs.len() {
                env.pop();
            }

            for func in funcs.iter_mut() {
                let e = func.type_.generalize(env);
                env.push(func.name.clone(), e);
            }

            let ret = infer!(cont);


            for _ in 0..funcs.len() {
                env.pop();
            }

            *type_ = ret.clone();

            Ok(ret)
        },
        Ast::Apply{ ref mut func, ref mut args, ref mut type_} => {
            let tf = infer!(func);
            let mut tya = Vec::new();
            for i in args.iter_mut() {
                tya.push(infer!(i));
            }
            let mut newvar = Type::Var(gen_var_count());
            unify!(Type::Func{args: tya, ret: Box::new(newvar.clone())}, tf);
            newvar.apply(gsubst);
            *type_ = newvar.clone();
            Ok(newvar)
        },
        Ast::Tuple(ref mut t, ref mut ty) => {
            let mut tup: Vec<Type> = Vec::new();
            for i in t.iter_mut() {
                tup.push(infer!(i));
            }
            *ty = Type::Tuple(tup);
            Ok(ty.clone())
        },
        Ast::LetTuple { names: ref mut decl, ref mut expr, ref mut cont, ref mut type_ } => {
            for i in decl.iter() {
                env.push_t(i.0.clone(), i.1.clone());
            }

            let r = infer!(expr);
            
            for _ in 0..decl.len() {
                env.pop();
            }

            let a = Type::Tuple(decl.iter().map(|&(_, ref t)| t.clone()).collect());
            
            unify!(r, a);
            env.apply(gsubst);

            for i in decl.iter_mut() {
                let gen = {
                    i.1.apply(gsubst);  
                    i.1.generalize(env)
                };
                env.push(i.0.clone(), gen);
            }

            let r = infer!(cont);
            
            for _ in 0..decl.len() {
                env.pop();
            }
            *type_ = r.clone();
            Ok(r)
        },
        Ast::Ctor { ref ctor, ref mut args, ref mut type_ } => {
            let info = table.find(ctor).unwrap();

            let mut param_pair = HashMap::new();
            
            for i in info.type_parameter.iter() {
                param_pair.insert(i, Type::Var(gen_var_count()));
            }

            let mut ret_type = info.ret_type.clone();
            for i in info.type_parameter.iter() {
                ret_type.subst(*i, param_pair.get(i).unwrap());
            }

            let r = if let &mut Some(ref mut args) = args {
                let mut arg_type = info.arg_type.clone();
                for a in arg_type.iter_mut() {
                    for i in info.type_parameter.iter() {
                        a.subst(*i, param_pair.get(i).unwrap());
                    }
                }

                let t = infer!(args);
                unify!(t, Type::Tuple(arg_type));

                ret_type

            } else {
                ret_type
            };
            *type_ = r.clone();
            Ok(r)
        },
        Ast::Match { ref mut expr, ref mut arms, ref mut type_ } => {
            let ety = infer!(expr);

            let mut tys = vec![];

            for arm in arms.iter_mut() {
                let (aty, binds) = infer_pattern(&mut arm.pattern, env, gsubst, table)?;
                unify!(ety.clone(), aty);
                let n = binds.len();
                for i in binds.into_iter() {
                    let t = TypeScheme{ bind: HashSet::new(), body: i.1};
                    env.push(i.0,t);
                }
                tys.push(infer!(&mut arm.expr));
                for _ in 0..n {
                    env.pop();
                }
            }

            /*for i in tys.iter_mut() {
                i.apply(gsubst);
            }*/

            for i in 1..tys.len() {
                unify!(tys[0].clone(), tys[i].clone());
            }

            *type_ = tys[0].clone();
            Ok(type_.clone())
        },
        Ast::DataDecl { name: _, parameter: _, ctors: _ } => unreachable!(),
    }
}

fn infer_pattern(pattern: &PatternAst, env: &mut TypeEnv, gsubst: &mut TypeSubst, table: &CtorTable) -> Result<(Type, Vec<(Id, Type)>), ()> {
    macro_rules! unify{
        ($e1:expr, $e2:expr) => {{
            let mut v = vec![($e1, $e2)];
            for x in v.iter_mut() {
                x.0.apply(gsubst);
                x.1.apply(gsubst);
            }
            let s = unify(v)?;
            gsubst.append(s);
        }};
    }
    
    match *pattern {
        PatternAst::Wildcard(ref id) => {
            let t = Type::Var(gen_var_count());
            Ok((t.clone(), vec![(id.clone(), t)]))
        },
        PatternAst::Discard => Ok((Type::Var(gen_var_count()), vec![])),
        PatternAst::Unit => Ok((Type::Unit, vec![])),
        PatternAst::Int(_) => Ok((Type::Int, vec![])),
        PatternAst::Bool(_) => Ok((Type::Bool, vec![])),
        PatternAst::Tuple(ref t) => {
            let mut ty = vec![];
            let mut bind = vec![];
            for i in t.iter() {
                let (t, mut b) = infer_pattern(i, env, gsubst, table)?;
                ty.push(t);
                bind.append(&mut b);
            }

            Ok((Type::Tuple(ty), bind))
        },
        PatternAst::DataDestruct{ref data, ref args} => {
            let info = table.find(data).unwrap();

            let mut param_pair = HashMap::new();
            
            for i in info.type_parameter.iter() {
                param_pair.insert(i, Type::Var(gen_var_count()));
            }

            let mut ret_type = info.ret_type.clone();
            for i in info.type_parameter.iter() {
                ret_type.subst(*i, param_pair.get(i).unwrap());
            }

            let mut arg_type = info.arg_type.clone();
            for a in arg_type.iter_mut() {
                for i in info.type_parameter.iter() {
                    a.subst(*i, param_pair.get(i).unwrap());
                }
            }

            let (t, bind) = infer_pattern(&PatternAst::Tuple(args.clone()), env, gsubst, table)?;
            unify!(t, Type::Tuple(arg_type));

            Ok((ret_type, bind))

        },
        _ => unimplemented!(),
    }
}

fn subst_expr(expr: &mut Ast, subst: &TypeSubst) {
    use ast::Ast::*;
    match expr {
        &mut Var(_, ref mut ty) => { ty.apply(subst); },
        &mut Logical { ref mut lhs, op: _, ref mut rhs , ref mut type_} |
        &mut BinaryOp { ref mut lhs, op: _, ref mut rhs , ref mut type_} => {
            subst_expr(lhs, subst);
            subst_expr(rhs, subst);
            type_.apply(subst);
        },
        &mut UnaryOp { op: _, ref mut expr , ref mut type_} => {
            subst_expr(expr, subst);
            type_.apply(subst);
        },
        &mut Apply { ref mut func, ref mut args , ref mut type_} => {
            subst_expr(func, subst);
            for i in args.iter_mut() {
                subst_expr(i, subst);
            }
            type_.apply(subst);
        },
        &mut Ctor { ref mut ctor, ref mut args , ref mut type_} => {
            // ctor のマングリングどうしよう
            for i in args.iter_mut() {
                subst_expr(i, subst);
            }
            type_.apply(subst);
        },
        &mut Seq (ref mut t, ref mut ty) |
        &mut Tuple(ref mut t, ref mut ty) => {
            for i in t.iter_mut() {
                subst_expr(i, subst);
            }
            ty.apply(subst);
        },
        &mut If { ref mut cond, ref mut then, ref mut else_ , ref mut type_} => {
            subst_expr(cond, subst);
            subst_expr(then, subst);
            subst_expr(else_, subst);
            type_.apply(subst);
        },
        &mut Match { ref mut expr, ref mut arms , ref mut type_} => {
            subst_expr(expr, subst);
            for i in arms.iter_mut() {
                subst_expr(&mut i.expr, subst);
            }
            type_.apply(subst);
        },
        &mut Let { name: (_, ref mut ty), ref mut expr, ref mut cont, ref mut type_} => {
            subst_expr(expr, subst);
            subst_expr(cont, subst);
            ty.apply(subst);
            type_.apply(subst);
        },
        &mut LetRec { ref mut funcs, ref mut cont, ref mut type_} => {
            for i in funcs.iter_mut() {
                subst_expr(&mut i.expr, subst);
                for &mut (_, ref mut ty) in i.formals.iter_mut() {
                    ty.apply(subst);
                }
                i.type_.apply(subst);
            }

            subst_expr(cont, subst);
            type_.apply(subst);
        },
        &mut LetTuple { ref mut names, ref mut expr, ref mut cont, ref mut type_} =>  {
            subst_expr(expr, subst);
            for &mut (_, ref mut ty) in names.iter_mut() {
                ty.apply(subst);
            }
            subst_expr(cont, subst);
        },
        _ => (),
    }
}