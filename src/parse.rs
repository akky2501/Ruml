use std::collections::HashSet;
use std::iter::FromIterator;
use std::str::FromStr;
use pom::*;
use pom::parser::{
    is_a, sym, one_of, seq, call, list, end
};

use id::*;
use ast::*;
use typing::Type;
use typing::*;

pub fn parse(src: String) -> Vec<Ast> {
    let mut input = TextInput::new(src.as_str());
    let r = program().parse(&mut input).unwrap();
    //println!("{:?}", input);
    r
}

type P<T> = Parser<char, T>;

lazy_static! {
    static ref RESERVED: HashSet<Id> = {
        let mut s = HashSet::new();
        s.insert(Id::new("let".to_string()));
        s.insert(Id::new("rec".to_string()));
        s.insert(Id::new("in".to_string()));
        s.insert(Id::new("true".to_string()));
        s.insert(Id::new("false".to_string()));
        s.insert(Id::new("type".to_string()));
        s.insert(Id::new("if".to_string()));
        s.insert(Id::new("then".to_string()));
        s.insert(Id::new("else".to_string()));
        s.insert(Id::new("match".to_string()));
        s.insert(Id::new("with".to_string()));
        s.insert(Id::new("as".to_string()));
        s.insert(Id::new("and".to_string()));
        s.insert(Id::new("int".to_string()));
        s.insert(Id::new("bool".to_string()));
        s
    };
}

fn program() -> P<Vec<Ast>> {
    /*let l = (call(let_decl) + ( ms1() * seq("in") * ms1() * call(expr)).opt() )
            .map(|((name, expr), cont)|{
                let cont = match cont {
                    Some(c) => c,
                    None => Ast::Unit,
                };
                Ast::Let{ name, expr: box expr, cont: box cont}
            });
    
    let lr = (call(let_rec_decl) + ( ms1() * seq("in") * ms1() * call(expr)).opt() )
            .map(|(funcs, cont)|{
                let cont = match cont {
                    Some(c) => c,
                    None => Ast::Unit,
                };
                Ast::LetRec{ funcs, cont: box cont }
            });
    
    let lt = (call(let_tuple_decl) + ( ms1() * seq("in") * ms1() * call(expr)).opt() )
            .map(|((names, expr), cont)|{
                let cont = match cont {
                    Some(c) => c,
                    None => Ast::Unit,
                };
                Ast::LetTuple{ names, expr: box expr, cont: box cont}
            });*/

    let l = call(let_decl).map(|(name, expr)|{
                Ast::Let{ name:(name, Type::Var(gen_var_count())), expr: box expr, cont: box Ast::Unit, type_: Type::None}
            });
    
    let lr = call(let_rec_decl).map(|funcs|{
                Ast::LetRec{ funcs, cont: box Ast::Unit , type_: Type::None}
            });
    
    let lt = call(let_tuple_decl).map(|(names, expr)|{
                Ast::LetTuple{ names: names.into_iter().map(|x| (x, Type::Var(gen_var_count()))).collect(),
                                expr: box expr, cont: box Ast::Unit, type_: Type::None}
            });

    let elems = call(data_decl)
              | lt
              | lr
              | l;


    ms() * list(elems, ms())/*.map(|x| {
        println!("{:?}", x);
        x
    })*/ - ms() - end()
}

fn data_decl() -> P<Ast> {
    let arms = (call(data_ident) + ( ms1() * seq("of") * ms1() * call(type_repesent)).opt())
               .map(|(name, formal)| DataDeclInfo{ name, formal });
    let body = seq("type") * (sp()*type_parameter()).repeat(0..) + ms1() * call(type_ident)
               - ms() - sym('=') - ms() + list(arms, ms()*sym('|')*ms());
    body.map(|((parameter, name), ctors)| Ast::DataDecl { name, parameter, ctors})
}

fn type_repesent() -> P<TypeAst> {
    list(call(type_term), sp0()*seq("->")*sp0())
    .map(|mut t| {
        let l = t.len();
        if l == 0 {
            panic!();
        } else if l == 1 {
            t.pop().unwrap()
        } else {
            let r = t.pop().unwrap();
            TypeAst::Func(t, box r)
        }
    })
}

fn type_term() -> P<TypeAst> {
    list(call(type_factor), sp0()*seq("*")*sp0())
    .map(|mut t| {
        let l = t.len();
        if l == 0 {
            panic!();
        } else if l == 1 {
            t.pop().unwrap()
        } else {
            TypeAst::Tuple(t)
        }
    })
}

fn type_factor() -> P<TypeAst> {
    list(call(type_primary), sp())
    .map(|mut t| {
        let l = t.len();
        if l == 0 {
            panic!()
        } else if l == 1 {
            t.pop().unwrap()
        } else {
            if let TypeAst::Var(id) = t.pop().unwrap() {
                TypeAst::Data{data_type: id, args: t}
            } else {panic!()}
        }
    })
}

fn type_primary() -> P<TypeAst> {
    let prim = sym('(') * ms() * call(type_repesent) - ms() - sym(')');
    let var = call(type_ident).map(|id| TypeAst::Var(id));    
    let parameter = call(type_parameter).map(|id| TypeAst::Var(id));    
    let int_lit = seq("int").map(|_| TypeAst::Int);
    let bool_lit = seq("bool").map(|_| TypeAst::Bool);
    let unit_lit = seq("()").map(|_| TypeAst::Unit);

    unit_lit | prim | var | parameter | int_lit | bool_lit
}


fn expr() -> P<Ast> {
    call(seq_expr)
}

fn seq_expr() -> P<Ast> {
    list(call(logical_or_expr), ms()*sym(';')*ms())
    .convert(|mut x| {
        //println!("se {:?}", x);
        let len = x.len();
        if len >= 2 {
            Ok(Ast::Seq(x, Type::None))
        } else if len == 1 {
            //println!("x.len {}", x.len());
            Ok(x.pop().unwrap())
        } else {
            Err("seq failed")
        }
    })
}

fn logical_or_expr() -> P<Ast> {
    let tail = ms() * seq("||").map(|_| Id::new("||".to_string())) - ms() + call(logical_and_expr);
    let e = call(logical_and_expr) + tail.repeat(0..);
    e.map(|(head, tail)|{
        //println!("or {:?} {:?}", head , tail);
        tail.into_iter().fold(head, |lhs, (op, rhs)|{
            Ast::Logical{lhs: box lhs, op, rhs: box rhs, type_: Type::None}
        })
    })
}

fn logical_and_expr() -> P<Ast> {
    let tail = ms() * seq("&&").map(|_| Id::new("&&".to_string())) - ms() + call(binary_expr);
    let e = call(binary_expr) + tail.repeat(0..);
    e.map(|(head, tail)|{
        //println!("and {:?} {:?}", head, tail);
        tail.into_iter().fold(head, |lhs, (op, rhs)|{
            Ast::Logical{lhs: box lhs, op, rhs: box rhs, type_: Type::None}
        })
    })
}

fn binary_expr() -> P<Ast> {
    let ops = seq("=").map(|_| Id::new("=".to_string()))
            | seq("+").map(|_| Id::new("+".to_string()))
            | seq("-").map(|_| Id::new("-".to_string()))
            | seq("*").map(|_| Id::new("*".to_string()))
            | seq("/").map(|_| Id::new("/".to_string()));
    let tail = ms() * ops - ms() + call(term);
    let e = call(unary_expr) + tail.repeat(0..);
    e.map(|(head, tail)|{
        // ここで演算子順位法を使えば良い
        //println!("bin {:?} {:?}", head, tail);
        tail.into_iter().fold(head, |lhs, (op, rhs)|{
            Ast::BinaryOp{lhs: box lhs, op, rhs: box rhs, type_: Type::None}
        })
    })
}

fn unary_expr() -> P<Ast> {
    let ops = seq("-").map(|_| Id::new("-".to_string()))
            | seq("!").map(|_| Id::new("!".to_string()));
    let e = (ops - ms()).repeat(0..) + call(term);
    e.map(|(ops, exp)| {
        //println!("un {:?} {:?}", ops, exp);
        ops.into_iter().rev().fold(exp, |expr, op| {
            Ast::UnaryOp{op, expr: box expr, type_: Type::None}
        })
    })
}

fn term() -> P<Ast> {
    call(data_ctor)
    | call(apply_expr)
    | call(if_expr)
    | call(let_expr)
    | call(match_expr)
}

fn data_ctor() -> P<Ast> {
    (call(data_ident) + (sp().repeat(1..) * call(tuple_or_primary_expr)).repeat(0..) )
    .map(|(ctor, args)|{
        Ast::Ctor{
            ctor,
            args: if args.len() == 0 {
                None
            } else {
                Some(box Ast::Tuple(args, Type::None))
            }
            , type_: Type::None
        }
    })
}

fn apply_expr() -> P<Ast> {
    (call(tuple_or_primary_expr) + (sp().repeat(1..) * call(tuple_or_primary_expr)).repeat(0..) )
    .map(|(func, args)| {
        if args.len() == 0 {
            func
        } else {
            Ast::Apply{ func: box func, args, type_: Type::None}
        }
    })
}

fn if_expr() -> P<Ast> {
    (seq("if") * ms1() * call(expr) - ms()
    + seq("then") * ms1() * call(expr) - ms()
    + seq("else") * ms1() * call(expr))
    .map(|((cond, then), else_)| Ast::If{cond: box cond, then: box then, else_: box else_, type_: Type::None} )
}

fn match_expr() -> P<Ast> {
    (seq("match") * ms() * call(expr) - ms() - seq("with") - ms() - (sym('|') * ms()).opt() + list(call(pattern_arm), ms()*sym('|')*ms()))
    .map(|(expr, arms)| Ast::Match{ expr: box expr, arms, type_: Type::None}) 
}

fn pattern_arm() -> P<PatternArm> {
    (call(pattern) + (ms() * seq("when") * ms() * call(expr)).opt() - ms() - seq("->") - ms() + call(expr))
    //.map(|((pattern, guard), expr)| PatternArm{pattern, guard, expr})
    .map(|((pattern, _), expr)| PatternArm{pattern, guard: None, expr, /*type_: Type::None*/})
}

fn pattern() -> P<PatternAst> {
    call(wildcard_pat)
    | call(discard_pat)
    | call(unit_pat)
    | call(int_pat)
    | call(bool_pat)
    | call(data_pat)
    | call(tuple_pat)
}

fn pattern_inner() -> P<PatternAst> {
    call(pattern)
    | call(primary_pat)
}

fn wildcard_pat() -> P<PatternAst> {
    var_ident().map(|c| PatternAst::Wildcard(c))
}

fn discard_pat() -> P<PatternAst> {
    sym('_').map(|_| PatternAst::Discard)
}

fn unit_pat() -> P<PatternAst> {
    seq("()").map(|_| PatternAst::Unit)
}

fn int_pat() -> P<PatternAst> {
    int_lit().map(|i| PatternAst::Int(i))
}

fn bool_pat() -> P<PatternAst> {
    bool_lit().map(|b| PatternAst::Bool(b))
}

fn data_pat() -> P<PatternAst> {
    let e = data_ident() - ms1() + list(call(pattern_inner), ms1());
    e.map(|(data, args)| PatternAst::DataDestruct {data, args} )
}

fn tuple_pat() -> P<PatternAst> {
    let e = sym('(') * ms() * call(pattern_inner) + (ms() * sym(',') * ms() * call(pattern_inner)).repeat(1..) - ms() - sym(')');
    e.map(|(head, mut tail)| {
        let mut head = vec![head];
        head.append(&mut tail);
        PatternAst::Tuple(head)
    })
}

fn primary_pat() -> P<PatternAst> {
    sym('(') * call(pattern_inner) - sym(')')
}

fn tuple_or_primary_expr() -> P<Ast> {
    let e = (sym('(') * ms() * list(call(expr), ms()*sym(',')*ms()) - ms() - sym(')'))
            .map(|mut exp| {
                if exp.len() >=2 { // tuple
                    Ast::Tuple(exp, Type::None)
                } else { // primary
                    exp.pop().unwrap()
                }
            });
    call(factor) | e
}

fn factor() -> P<Ast> {
    call(literal) | call(var_ident).map(|id| Ast::Var(id,Type::Var(gen_var_count())))
    | call(data_ident).map(|ctor| Ast::Ctor{ ctor, args: None, type_: Type::None})
}

fn let_decl() -> P<((Id, Ast))> {
    seq("let") * ms1() * call(var_ident) - ms() - sym('=') - ms() + call(expr)
}

fn let_rec_decl() -> P<Vec<FuncInfo>> { // let rec で引数0個の場合は、Unitを一つ取る関数とみなす。
    let head = seq("let") * ms1() * seq("rec");
    let l = (call(var_ident) - sp().repeat(1..) + list(call(var_ident), sp().repeat(0..)) - ms() - sym('=') - ms() + call(expr))
            .map(|((name, formals), expr)| FuncInfo{ name, formals: formals.into_iter().map(|x| (x, Type::Var(gen_var_count()))).collect(), expr , type_: Type::Var(gen_var_count())});
    (head * ms1() * list(l, ms1()*seq("and")*ms1()))
}

fn let_tuple_decl() -> P<(Vec<Id>, Ast)> {
    seq("let") * ms1() * sym('(') * ms() * list(call(var_ident), sp0()*sym(',')*sp0()) - ms() - sym(')') - ms() - sym('=') - ms() + call(expr)
}

fn let_expr() -> P<Ast> {
    let l = (call(let_decl) - ms1() - seq("in") - ms1() + call(expr))
            .map(|((name, expr), cont)| Ast::Let{ name: (name, Type::Var(gen_var_count())), expr: box expr, cont: box cont, type_: Type::None});
    
    let lr = (call(let_rec_decl) - ms1() - seq("in") - ms1() + call(expr))
            .map(|(funcs, cont)| Ast::LetRec{ funcs, cont: box cont , type_: Type::None});
    
    let lt = (call(let_tuple_decl) - ms1() - seq("in") - ms1() + call(expr))
            .map(|((names, expr), cont)| Ast::LetTuple{ names: names.into_iter().map(|x| (x, Type::Var(gen_var_count()))).collect()
                                                    , expr: box expr, cont: box cont, type_: Type::None});

    lt | lr | l
}

fn literal() -> P<Ast> {
    call(unit_lit).map(|_| Ast::Unit) | call(int_lit).map(|i| Ast::Int(i)) | call(bool_lit).map(|b| Ast::Bool(b))
}

fn int_lit() -> P<i64> {
    let n = one_of("123456789") - one_of("0123456789").repeat(0..) | sym('0');
    n.collect().map(|x| {
        let s = String::from_iter(x);
        i64::from_str(&s).unwrap()
    })
}

fn bool_lit() -> P<bool> {
    seq("true").map(|_| true) | seq("false").map(|_| false)
}

fn unit_lit() -> P<()> {
    seq("()").map(|_| () )
}

fn var_ident() -> P<Id> {
    let front = is_a(|c: char| {
        //println!("{} {}", c, c.is_lowercase());
        c.is_lowercase()
    });
    let tail = (sym('_') | is_a(|c: char| c.is_alphanumeric())).repeat(0..);
    (front+tail).collect().convert(|x|{
        let id = Id::new(String::from_iter(x));
        //println!("var {:?}", id);
         if ! RESERVED.contains(&id) {
            //println!("var ok");
            Ok(id)
         } else { /*println!("var ng");*/ Err("this word is reserved") }
    })
}

fn type_ident() -> P<Id> {
    var_ident()
}

fn data_ident() -> P<Id> {
    let front = is_a(|c: char| {
        //println!("{} {}", c, c.is_uppercase());
        c.is_uppercase()
    });
    let tail = (sym('_') | is_a(|c: char| c.is_alphanumeric())).repeat(0..);
    (front+tail).collect().map(|x| Id::new(String::from_iter(x)))
}

fn type_parameter() -> P<Id> {
    let front = sym('\'');
    let tail = is_a(|c: char| c.is_lowercase()).repeat(1..);
    (front+tail).collect().map(|x| Id::new(String::from_iter(x)))
}

fn ms() -> P<()> { // multi space
    one_of(" \t\r\n").repeat(0..).discard()
}

fn ms1() -> P<()> { // multi space
    one_of(" \t\r\n").repeat(1..).discard()
}

fn sp0() -> P<()> { // multi space
    one_of(" ").repeat(0..).discard()
}

fn sp() -> P<()> { // multi space
    one_of(" ").discard()
}
