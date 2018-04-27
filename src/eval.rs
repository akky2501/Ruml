use std::rc::Rc;
use std::rc::Weak;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::fmt::Debug;

use id::*;
use ast::*;
use constructor_table::*;

#[derive(Debug, Clone)]
enum Value {
    Err,
    Unit,
    Int(i64),
    Bool(bool),
    Func{ entry: Ast, formals: Vec<Id> , env: Env},
    Tuple(Vec<Value>),
    Data{ ctor: Id, args: Vec<Value>},

    Print
}

type EnvNodeRef<K, V> = Rc<RefCell<EnvNode<K, V>>>;
type EnvNodeRefWeak<K, V> = Weak<RefCell<EnvNode<K, V>>>;

#[derive(Debug)]
struct EnvNode<K: Eq + Hash + Debug, V: Debug> {
    parent: EnvNodeRefWeak<K, V>,
    children: Vec<EnvNodeRef<K, V>>,
    table: HashMap<K, V>,
}

impl<K: Eq + Hash + Debug, V: Clone + Debug> EnvNode<K, V> {
    fn get(&self, key: &K) -> Option<V> {
        if let Some(v) = self.table.get(key) {
            Some(v.clone())
        } else if let Some(p) = Weak::upgrade(&self.parent) {
            p.borrow().get(key)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment<K: Eq + Hash + Debug, V: Debug>(EnvNodeRef<K, V>);

impl<K: Eq + Hash + Debug, V: Clone + Debug> Environment<K, V> {
    pub fn new() -> Self {
        let node = EnvNode{ parent: Weak::new(), children: Vec::new(), table: HashMap::new()};
        Environment(Rc::new(RefCell::new(node)))
    }

    pub fn push_child(&self, child: &Environment<K, V>) {
        child.0.borrow_mut().parent = Rc::downgrade(&self.0);
        self.0.borrow_mut().children.push(child.0.clone());
    }

    pub fn get(&self, key: &K) -> Option<V> {
        self.0.borrow().get(key)
    }

    pub fn set(&self, key: K, value: V) -> Option<V> {
        self.0.borrow_mut().table.insert(key, value)
    }

    pub fn remove(&self, key: &K) -> Option<V> {
        self.0.borrow_mut().table.remove(key)
    }
}

type Env = Environment<Id, Value>;

pub fn eval(asts: Vec<Ast>, table: CtorTable) {
    let mut genv = Env::new();
    for ast in asts.iter() {
        match ast {
           &Ast::Let { name: (ref name, _), ref expr, cont: _, type_: _} => {
                let v = eval_inner(expr, &mut genv, &table);
                genv.set(name.clone(), v);
            },
            &Ast::LetRec { ref funcs, cont: _, type_: _} => {
                for f in funcs.iter() {
                    if f.name.as_str() == "print" {
                        continue;
                    }
                    let new_env = Env::new();
                    genv.push_child(&new_env);
                    let v = Value::Func{ entry: f.expr.clone(), formals: f.formals.iter().map(|x| x.0.clone()).collect(), env: new_env };
                    genv.set(f.name.clone(), v);
                }
            },
            &Ast::LetTuple { ref names, ref expr, cont: _, type_: _} => {
                let v = eval_inner(expr, &mut genv, &table);
                if let Value::Tuple(t) = v {
                    if t.len() == names.len() {
                        for i in 0..t.len() {
                            genv.set(names[i].0.clone(), t[i].clone());
                        }
                    } else { panic!() }
                } else { panic!() }
            },
            _ => unreachable!(),
        }
    }

    //println!("env:\n{:?}", genv);
    //println!("{:?}", genv.get(&Id::new("a_@2".to_string())));
    //println!("{:?}", genv.get(&Id::new("a_@0".to_string())));
}

fn eval_inner(ast: &Ast, env: &mut Env, table: &CtorTable) -> Value {
    use ast::Ast::*;
    match ast {
        &Unit => Value::Unit,
        &Int(i64) => Value::Int(i64),
        &Bool(bool) => Value::Bool(bool),
        &Var(ref id, _) => {
            if id.as_str() == "print" {
                Value::Print
            } else {
                env.get(id).unwrap()
            }
        },
        &Logical { ref lhs, ref op, ref rhs, type_: _ } => {
            if let Value::Bool(l) = eval_inner(lhs, env, table) {
                match op.as_str() {
                    "&&" => {
                        if l {
                            if let Value::Bool(r) = eval_inner(rhs, env, table){
                                if r {
                                    return Value::Bool(true)
                                } else {
                                    return Value::Bool(false)
                                }
                            }
                        } else {
                            return Value::Bool(false);
                        }
                    },
                    "||" => {
                        if ! l {
                            if let Value::Bool(r) = eval_inner(rhs, env, table){
                                if ! r {
                                    return Value::Bool(false)
                                } else {
                                    return Value::Bool(true)
                                }
                            }
                        } else {
                            return Value::Bool(true);
                        }
                    },
                    _ => unreachable!(),
                }
            }
            Value::Err
        },
        &BinaryOp { ref lhs, ref op, ref rhs, type_: _ } => {
            if let Value::Int(l) = eval_inner(lhs, env, table) {
                if let Value::Int(r) = eval_inner(rhs, env, table){
                    match op.as_str() {
                        "+" => return Value::Int(l+r),
                        "-" => return Value::Int(l-r),
                        "*" => return Value::Int(l*r),
                        "/" => return Value::Int(l/r),
                        "=" => return Value::Bool(l==r),
                        _ => {
                            println!("{:?}", op);
                            unreachable!()
                        },
                    }
                }
            }
            Value::Err
        },
        &UnaryOp { ref op, ref expr , type_: _} => {
            if let Value::Int(e) = eval_inner(expr, env, table){
                match op.as_str() {
                    "-" => return Value::Int(-e),
                    _ => unreachable!(),
                }
            }
            return Value::Err;
        },
        &Apply { ref func, ref args , type_: _} => {
            //println!("env: {:?}", env);
            let f = eval_inner(func, env, table);
            //println!("{:?}", f);
            let args: Vec<_> = args.iter().map(|x| eval_inner(x, env, table)).collect();

            if let Value::Func{entry, formals, env} = f {
                let mut new_env = Env::new();
                env.push_child(&new_env);
                for (i, j) in formals.into_iter().zip(args.into_iter()) {
                    new_env.set(i, j);
                }
                eval_inner(&entry, &mut new_env, table)
            } else {
                match f {
                    Value::Print => {
                        println!("{:?}", args);
                        Value::Unit
                    },
                    _ => return Value::Err,
                }
            }
        },
        &Ctor { ref ctor, ref args , type_: _} => {
            if let &Some(ref args) = args {
                let v = eval_inner(args, env, table);
                if let Value::Tuple(t) = v {
                    Value::Data{ctor: ctor.clone(), args: t}
                } else { Value::Err }
            } else {
                Value::Data{ ctor: ctor.clone(), args: vec![] }
            }
        },
        &Seq (ref t, _) => {
            let n = t.len();
            let mut c = 0;
            for e in t.iter() {
                let r = eval_inner(e, env, table);
                if c+1 == n { // return tail value
                    return r;
                }
                c += 1;
            }
            Value::Err
        }
        &Tuple(ref t, _) => {
            let r = t.iter().map(|x| eval_inner(x, env, table)).collect();
            Value::Tuple(r)
        },
        &If { ref cond, ref then, ref else_ , type_: _} => {
            if let Value::Bool(cond) = eval_inner(cond, env, table) {
                if cond {
                    eval_inner(then, env, table)
                } else {
                    eval_inner(else_, env, table)
                }
            } else { Value::Err }
        },
        &Match { ref expr, ref arms , type_: _} => unimplemented!(),
        &Let { name: (ref name, _), ref expr, ref cont, type_: _} => {
            let v = eval_inner(expr, env, table);
            let mut new_env = Env::new();
            env.push_child(&new_env);
            new_env.set(name.clone(), v);
            let v = eval_inner(cont, &mut new_env, table);
            v
        },
        &LetRec { ref funcs, ref cont, type_: _} => {
            //let mut new_env = Env::new();
            //env.push_child(&new_env);
            for f in funcs.iter() {
                let new_env = Env::new();
                env.push_child(&new_env);
                let v = Value::Func{ entry: f.expr.clone(), formals: f.formals.iter().map(|x| x.0.clone()).collect(), env: new_env };
                //new_env.set(f.name.clone(), v);
                env.set(f.name.clone(), v);
            }
            //let v = eval_inner(cont, &mut new_env, table);
            let v = eval_inner(cont, env, table);
            v
        },
        &LetTuple { ref names, ref expr, ref cont, type_: _} => {
            let v = eval_inner(expr, env, table);
            let mut new_env = Env::new();
            if let Value::Tuple(t) = v {
                if t.len() == names.len() {
                    for i in 0..t.len() {
                        new_env.set(names[i].0.clone(), t[i].clone());
                    }
                } else { return Value::Err }
            } else { return Value::Err }

            let v = eval_inner(cont, &mut new_env, table);
            
            v
        },
        _ => unreachable!(),
    }
}





