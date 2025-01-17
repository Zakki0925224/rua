use crate::{bytecode::ByteCode, parse::ParseProto, value::Value};
use std::{cmp::Ordering, collections::HashMap, io::Read};

fn lib_print(state: &mut ExeState) -> i32 {
    println!("{:?}", state.stack[state.func_index + 1]);
    0
}

pub struct ExeState {
    globals: HashMap<String, Value>,
    stack: Vec<Value>,
    func_index: usize,
}

impl ExeState {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert("print".into(), Value::Function(lib_print));

        Self {
            globals,
            stack: Vec::new(),
            func_index: 0,
        }
    }

    pub fn execute<R: Read>(&mut self, proto: &ParseProto<R>) -> anyhow::Result<()> {
        for code in proto.byte_codes.iter() {
            match *code {
                ByteCode::GetGlobal(dst, name) => {
                    let name: &str = (&proto.constants[name as usize]).into();
                    let v = self.globals.get(name).unwrap_or(&Value::Nil).clone();
                    self.set_stack(dst.into(), v)?;
                }
                ByteCode::SetGlobal(name, src) => {
                    let name = &proto.constants[name as usize];
                    let value = self.stack[src as usize].clone();
                    self.globals.insert(name.into(), value);
                }
                ByteCode::SetGlobalConst(name, src) => {
                    let name = &proto.constants[name as usize];
                    let value = proto.constants[src as usize].clone();
                    self.globals.insert(name.into(), value);
                }
                ByteCode::SetGlobalGlobal(name, src) => {
                    let name = &proto.constants[name as usize];
                    let src: &str = (&proto.constants[src as usize]).into();
                    let value = self.globals.get(src).unwrap_or(&Value::Nil).clone();
                    self.globals.insert(name.into(), value);
                }
                ByteCode::LoadConst(dst, c) => {
                    let v = proto.constants[c as usize].clone();
                    self.set_stack(dst, v)?;
                }
                ByteCode::LoadNil(dst) => {
                    self.set_stack(dst, Value::Nil)?;
                }
                ByteCode::LoadBool(dst, b) => {
                    self.set_stack(dst, Value::Boolean(b))?;
                }
                ByteCode::LoadInt(dst, i) => {
                    self.set_stack(dst, Value::Integer(i as i64))?;
                }
                ByteCode::Move(dst, src) => {
                    let v = self.stack[src as usize].clone();
                    self.set_stack(dst, v)?;
                }
                ByteCode::Call(func, _) => {
                    self.func_index = func as usize;
                    let func = &self.stack[self.func_index];
                    if let Value::Function(f) = func {
                        f(self);
                    } else {
                        return Err(anyhow::anyhow!("invalid function: {:?}", func));
                    }
                }
            }
        }

        Ok(())
    }

    fn set_stack(&mut self, dst: u8, v: Value) -> anyhow::Result<()> {
        let dst = dst as usize;
        match dst.cmp(&self.stack.len()) {
            Ordering::Equal => self.stack.push(v),
            Ordering::Less => self.stack[dst] = v,
            Ordering::Greater => return Err(anyhow::anyhow!("invalid stack index: {}", dst)),
        }

        Ok(())
    }
}
