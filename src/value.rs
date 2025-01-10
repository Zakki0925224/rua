use crate::vm::ExeState;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    String(String),
    Function(fn(&mut ExeState) -> i32),
}
