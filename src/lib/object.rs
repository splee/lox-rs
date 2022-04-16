use std::fmt;

#[derive(Debug, Clone)]
pub enum Object {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Boolean(v) => write!(f, "{}", v),
            Object::Nil => write!(f, "nil"),
            Object::Number(v) => {
                if v % 1.0 > 0.0 {
                    write!(f, "{}", v)
                } else {
                    write!(f, "{:.0}", v)
                }
            },
            Object::String(v) => write!(f, "{}", v),
        }
    }
}