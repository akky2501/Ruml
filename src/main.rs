#![feature(box_syntax, box_patterns)]

#[macro_use]
extern crate lazy_static;
extern crate pom;

mod id;
mod parse;
mod ast;
mod mangling;
mod typing;
mod constructor_table;
mod eval;


use std::io::prelude::*;
use std::fs::OpenOptions;
use std::io::BufReader;

use parse::*;
use mangling::*;
use typing::*;
use eval::*;

fn main() {
    let mut input_text = String::new();
    {
        let file = OpenOptions::new()
                                .read(true)
                                .open("test_src/test2.rml")
                                .expect("target file is not found.");
        let mut buf_reader = BufReader::new(file);
        buf_reader.read_to_string(&mut input_text).unwrap();
    }
    println!("{}", input_text);    
    let ast = parse(input_text);
    //println!("{:?}", ast);
    let (mut code, table) = mangle_and_separate(ast);
    println!("code:\n{:?}\ntable:\n{:?}", code, table);

    infer_types(&mut code, &table);
    println!("code:\n{:?}", code);

    eval(code, table);
}
