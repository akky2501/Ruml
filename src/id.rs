use std::sync::Mutex;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Id(String);

lazy_static! {
    static ref NEXT_COUNT: Mutex<usize> = Mutex::new(0);
}

impl Id {

    pub fn new(id: String) -> Self {
        Id(id)
    }
    
    pub fn gen(prefix: &str) -> Self {
        let r = Id::new(format!("{}{}", prefix, *NEXT_COUNT.lock().unwrap()));
        *NEXT_COUNT.lock().unwrap() += 1;
        return r;
    }

    pub fn mangle(id: &Id) -> Self {
        Id::gen(format!("{}_@", id.0.as_str()).as_str())
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
