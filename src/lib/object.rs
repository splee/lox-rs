use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(v) => *v,
            Object::Nil => false,
            Object::Number(v) => *v != 0.0f64,
            Object::String(v) => !v.is_empty(),
        }
    }
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
            }
            Object::String(v) => write!(f, "{}", v),
        }
    }
}
