use id::*;
use ast::*;
use typing::*;
use constructor_table::*;


pub fn mangle_and_separate(ast: Vec<Ast>) -> (Vec<Ast>, CtorTable) {
    let (mut code, data) = separate(ast);

    // TODO: type decl check

    let mut table = CtorTable::new();
    for ast in data.into_iter() {
        collect_ctors(&mut table, ast);
    }

    let mut env =vec![];
    for mut ast in code.iter_mut() {
        let old = match ast {
            &mut Ast::Let { name: (ref id, _), expr: _, cont: _, type_: _} => {
                vec![id.clone()]
            },
            &mut Ast::LetRec { ref mut funcs, cont: _, type_: _} => {
                funcs.iter().map(|x| x.name.clone()).collect()
            },
            &mut Ast::LetTuple { ref mut names, expr: _, cont: _, type_: _} => {
                names.iter().map(|x| x.0.clone()).collect()                
            },
            _ => unreachable!(),
        };
        mangle(&mut ast, &mut env);
        let new = match ast {
            &mut Ast::Let { name: (ref id, _), expr: _, cont: _, type_: _} => {
                vec![id.clone()]
            },
            &mut Ast::LetRec { ref mut funcs, cont: _, type_: _} => {
                funcs.iter().map(|x| x.name.clone()).collect()
            },
            &mut Ast::LetTuple { ref mut names, expr: _, cont: _, type_: _} => {
                names.iter().map(|x| x.0.clone()).collect()                
            },
            _ => unreachable!(),
        };
        for x in old.into_iter().zip(new.into_iter()) {
            env.push(x);
        }
    }

    (code, table)
}

fn separate(ast: Vec<Ast>) -> (Vec<Ast>, Vec<Ast>) {
    use ast::Ast::*;    
    ast.into_iter().fold((vec![], vec![]), |mut acc, ast|{
        match ast {
            DataDecl{name: _, parameter: _, ctors: _} => acc.1.push(ast),
            _ => acc.0.push(ast),
        };
        acc
    })
}


fn mangle(code: &mut Ast, env: &mut Vec<(Id, Id)>) { // env: vec<(old, new)>
    use ast::Ast::*;

    macro_rules! new_name {
        ($name: expr) => ({
            if $name.as_str() == "print" {
                (Id::new("print".to_string()), Id::new("print".to_string()))
            } else {
                let old = $name.clone();
                *($name) = Id::mangle($name);
                let new = $name.clone();
                (old, new)
            }
        })
    }

    macro_rules! replace {
        ($old: ident) => {
            if let Some(&(_, ref new)) = env.iter().rev().find(|&x| x.0 == *$old) {
                *$old = new.clone();
            } else { panic!(); }
        }
    }

    match code {
        &mut Var(ref mut id, _) => {
            replace!(id)
        },
        &mut Logical { ref mut lhs, op: _, ref mut rhs , type_: _} |
        &mut BinaryOp { ref mut lhs, op: _, ref mut rhs , type_: _} => {
            mangle(lhs, env);
            mangle(rhs, env);
        },
        &mut UnaryOp { op: _, ref mut expr , type_: _} => {
            mangle(expr, env);
        },
        &mut Apply { ref mut func, ref mut args , type_: _} => {
            mangle(func, env);
            for i in args.iter_mut() {
                mangle(i, env);
            }
        },
        &mut Ctor { ref mut ctor, ref mut args , type_: _} => {
            // ctor のマングリングどうしよう
            for i in args.iter_mut() {
                mangle(i, env);
            }
        },
        &mut Seq (ref mut t, _) |
        &mut Tuple(ref mut t, _) => {
            for i in t.iter_mut() {
                mangle(i, env);
            }
        },
        &mut If { ref mut cond, ref mut then, ref mut else_ , type_: _} => {
            mangle(cond, env);
            mangle(then, env);
            mangle(else_, env);
        },
        &mut Match { ref mut expr, ref mut arms , type_: _} => {
            mangle(expr, env);
            for i in arms.iter_mut() {
                mangle_pattern_arm(i, env);
            }
        },
        &mut Let { name: (ref mut name, _), ref mut expr, ref mut cont, type_: _} => {
            mangle(expr, env);
            env.push(new_name!(name));
            mangle(cont, env);
            env.pop();
        },
        &mut LetRec { ref mut funcs, ref mut cont, type_: _} => {
            let fcount = funcs.len();
            for i in funcs.iter_mut() {
                let name = &mut i.name;
                env.push(new_name!(name));
            }

            for func in funcs.iter_mut() {
                let arg_count = func.formals.len();
                for &mut (ref mut i, _) in func.formals.iter_mut() {
                    env.push(new_name!(i));
                }

                mangle(&mut func.expr, env);

                for _ in 0..arg_count {
                    env.pop();
                }
            }

            mangle(cont, env);

            for _ in 0 .. fcount {
                env.pop();
            }
        },
        &mut LetTuple { ref mut names, ref mut expr, ref mut cont, type_: _} =>  {
            mangle(expr, env);
            let count = names.len();
            for &mut (ref mut name, _) in names.iter_mut() {
                let old = name.clone();
                *name = Id::mangle(name);
                let new = name.clone();
                env.push((old, new));
            }
            mangle(cont, env);
            for _ in 0 .. count {
                env.pop();
            }
        },

        _ => (),
    }
}

fn mangle_pattern_arm(arm: &mut PatternArm, env: &mut Vec<(Id, Id)>) {
    let count = mangle_pattern(&mut arm.pattern, env);

    if let Some(ref mut guard) = arm.guard {
        mangle(guard, env);
    }

    mangle(&mut arm.expr, env);

    for _ in 0..count {
        env.pop();
    }
}

fn mangle_pattern(arm: &mut PatternAst, env: &mut Vec<(Id, Id)>) -> usize {
    use ast::PatternAst::*;
    match arm {
        &mut Wildcard(ref mut id) => {
            let old = id.clone();
            *id = Id::mangle(id);
            let new = id.clone();
            env.push((old, new));
            1
        },
        &mut Tuple(ref mut t) => {
            let mut count = 0;
            for i in t.iter_mut() {
                count += mangle_pattern(i, env);
            }
            count
        },
        &mut DataDestruct{ ref mut data, ref mut args } => {
            // TODO: dataのマングリング
            let mut count = 0;
            for i in args.iter_mut() {
                count += mangle_pattern(i, env);
            }
            count
        },
        _ => 0,
    }
}


fn collect_ctors(table: &mut CtorTable, ast: Ast) {
    use ast::Ast::*;
    if let DataDecl{name, parameter, ctors} = ast {
        let mut env = vec![];
        let mut type_parameter = vec![];
        for id in parameter.into_iter() {
            let t = gen_var_count();
            env.push((id, t));
            type_parameter.push(t);
        }

        let ret = Type::Data{name, args: type_parameter.iter().map(|x| Type::Var(*x)).collect()};

        for ctor in ctors.into_iter() {
            let info = if let Some(formal) = ctor.formal {
                let t = flat_tuple_type(convert_type(formal, &env));
                CtorInfo{ type_parameter: type_parameter.clone(),
                          ret_type: ret.clone(),
                          arg_type: t}
            } else {
                CtorInfo{ type_parameter: type_parameter.clone(),
                          ret_type: ret.clone(),
                          arg_type: vec![]}
            };
            table.push(ctor.name, info);
        }

    } else { panic!() }
}


fn convert_type(ast: TypeAst, env: &Vec<(Id, TypeId)>) -> Type {
    use ast::TypeAst::*;
    match ast {
        Func(args, ret) => {
            let args = args.into_iter().map(|a| convert_type(a, env)).collect();
            let ret = box convert_type(*ret, env);
            Type::Func{ args, ret}
        },
        Tuple(t) => {
            let t = t.into_iter().map(|a| convert_type(a, env)).collect();
            Type::Tuple(t)            
        },
        Unit => Type::Unit,
        Int => Type::Int,
        Bool => Type::Bool,
        Data{ data_type, args} => {
            let args = args.into_iter().map(|a| convert_type(a, env)).collect();
            Type::Data{ name: data_type, args}
        },
        Var(id) => {
            if let Some(&(_, tid)) = env.iter().rev().find(|x| x.0 == id) {
                Type::Var(tid)
            } else { panic!() }
        },
    }
}

fn flat_tuple_type(ty: Type) -> Vec<Type> {
    match ty {
        Type::Tuple(t) => t,
        t@_ => vec![t],
    }
}