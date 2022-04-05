
#[derive(Debug, Clone)]
pub enum Object {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
}