use std::collections::HashMap;
use id::*;
use typing::*;

#[derive(Debug)]
pub struct CtorTable {
    ctors: HashMap<Id,CtorInfo>,
}

#[derive(Debug)]
pub struct CtorInfo {
    pub type_parameter: Vec<TypeId>,
    pub ret_type: Type,
    pub arg_type: Vec<Type>,
}

impl CtorTable {
    pub fn new() -> Self {
        CtorTable { ctors: HashMap::new() }
    }

    pub fn push(&mut self, id: Id, ctor: CtorInfo) {
        self.ctors.insert(id, ctor);
    }

    pub fn find(&self, id: &Id) -> Option<&CtorInfo> {
        self.ctors.get(id)
    }
}