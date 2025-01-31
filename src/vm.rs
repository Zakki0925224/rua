use anyhow::Ok;

use crate::{
    bytecode::ByteCode,
    parse::ParseProto,
    value::{Table, Value},
};
use std::{cell::RefCell, cmp::Ordering, collections::HashMap, io::Read, rc::Rc};

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
                ByteCode::LoadConst(dst, c) => {
                    let v = proto.constants[c as usize].clone();
                    self.set_stack(dst, v)?;
                }
                ByteCode::LoadNil(dst, n) => {
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
                ByteCode::NewTable(dst, narray, nmap) => {
                    let table = Table::new(narray as usize, nmap as usize);
                    self.set_stack(dst, Value::Table(Rc::new(RefCell::new(table))))?;
                }
                ByteCode::SetInt(t, i, v) => {
                    let value = self.stack[v as usize].clone();
                    self.set_table_int(t, i as i64, value)?;
                }
                ByteCode::SetIntConst(t, i, v) => {
                    let value = proto.constants[v as usize].clone();
                    self.set_table_int(t, i as i64, value);
                }
                ByteCode::SetField(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = self.stack[v as usize].clone();
                    self.set_table(t, key, value)?;
                }
                ByteCode::SetFieldConst(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = proto.constants[v as usize].clone();
                    self.set_table(t, key, value)?;
                }
                ByteCode::SetTable(t, k, v) => {
                    let key = self.stack[k as usize].clone();
                    let value = self.stack[v as usize].clone();
                    self.set_table(t, key, value)?;
                }
                ByteCode::SetTableConst(t, k, v) => {
                    let key = self.stack[k as usize].clone();
                    let value = proto.constants[v as usize].clone();
                    self.set_table(t, key, value)?;
                }
                ByteCode::SetList(table, n) => {
                    let ivalue = table as usize + 1;
                    if let Value::Table(table) = self.stack[table as usize].clone() {
                        let values = self.stack.drain(ivalue..ivalue + n as usize);
                        table.borrow_mut().array.extend(values);
                    } else {
                        return Err(anyhow::anyhow!("no table"));
                    }
                }
                ByteCode::GetInt(dst, t, k) => {
                    let value = self.get_table_int(t, k as i64)?;
                    self.set_stack(dst, value)?;
                }
                ByteCode::GetField(dst, t, k) => {
                    let key = &proto.constants[k as usize];
                    let value = self.get_table(t, key)?;
                    self.set_stack(dst, value)?;
                }
                ByteCode::GetTable(dst, t, k) => {
                    let key = &self.stack[k as usize];
                    let value = self.get_table(t, key)?;
                    self.set_stack(dst, value)?;
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

    fn fill_stack(&mut self, begin: usize, num: usize) {
        let end = begin + num;
        let len = self.stack.len();

        if begin < len {
            self.stack[begin..len].fill(Value::Nil);
        }

        if end > len {
            self.stack.resize(end, Value::Nil);
        }
    }

    fn set_table_int(&mut self, t: u8, i: i64, value: Value) -> anyhow::Result<()> {
        if let Value::Table(table) = &self.stack[t as usize] {
            let mut table = table.borrow_mut();
            if i > 0 && (i < 4 || i < table.array.capacity() as i64 * 2) {
                set_vec(&mut table.array, i as usize - 1, value);
            } else {
                table.map.insert(Value::Integer(i), value);
            }
        } else {
            return Err(anyhow::anyhow!("invalid table"));
        }

        Ok(())
    }

    fn do_set_table(&mut self, t: u8, key: Value, value: Value) -> anyhow::Result<()> {
        if let Value::Table(table) = &self.stack[t as usize] {
            table.borrow_mut().map.insert(key, value);
        } else {
            return Err(anyhow::anyhow!("invalid table"));
        }

        Ok(())
    }

    fn set_table(&mut self, t: u8, key: Value, value: Value) -> anyhow::Result<()> {
        match key {
            Value::Integer(i) => self.set_table_int(t, i, value),
            _ => self.do_set_table(t, key, value),
        }
    }

    fn get_table_int(&self, t: u8, i: i64) -> anyhow::Result<Value> {
        if let Value::Table(table) = &self.stack[t as usize] {
            let table = table.borrow();
            let value = table
                .array
                .get(i as usize - 1)
                .unwrap_or_else(|| table.map.get(&Value::Integer(i)).unwrap_or(&Value::Nil))
                .clone();
            return Ok(value);
        } else {
            return Err(anyhow::anyhow!("set invalid table"));
        }
    }

    fn do_get_table(&self, t: u8, key: &Value) -> anyhow::Result<Value> {
        if let Value::Table(table) = &self.stack[t as usize] {
            let table = table.borrow();
            let value = table.map.get(key).unwrap_or(&Value::Nil).clone();
            return Ok(value);
        } else {
            return Err(anyhow::anyhow!("set invalid table"));
        }
    }

    fn get_table(&self, t: u8, key: &Value) -> anyhow::Result<Value> {
        match key {
            Value::Integer(i) => self.get_table_int(t, *i),
            _ => self.do_get_table(t, key),
        }
    }
}

fn set_vec(vec: &mut Vec<Value>, i: usize, value: Value) {
    match i.cmp(&vec.len()) {
        Ordering::Less => vec[i] = value,
        Ordering::Equal => vec.push(value),
        Ordering::Greater => {
            vec.resize(i, Value::Nil);
            vec.push(value);
        }
    }
}
