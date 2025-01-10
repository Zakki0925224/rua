use crate::{bytecode::ByteCode, parse::ParseProto, value::Value};
use std::{cmp::Ordering, collections::HashMap};

fn lib_print(state: &mut ExeState) -> i32 {
    println!("{:?}", state.stack[1]);
    0
}

pub struct ExeState {
    globals: HashMap<String, Value>,
    stack: Vec<Value>,
}

impl ExeState {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert(String::from("print"), Value::Function(lib_print));

        Self {
            globals,
            stack: Vec::new(),
        }
    }

    pub fn execute(&mut self, proto: &ParseProto) -> anyhow::Result<()> {
        for code in proto.byte_codes.iter() {
            match *code {
                ByteCode::GetGlobal(dst, name) => {
                    let name = &proto.constants[name as usize];
                    if let Value::String(key) = name {
                        let v = self.globals.get(key).unwrap_or(&Value::Nil).clone();
                        self.set_stack(dst, v)?;
                    } else {
                        return Err(anyhow::anyhow!("invalid global key: {:?}", name));
                    }
                }
                ByteCode::LoadConst(dst, c) => {
                    let v = proto.constants[c as usize].clone();
                    self.set_stack(dst, v)?;
                }
                ByteCode::Call(func, _) => {
                    let func = &self.stack[func as usize];
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
