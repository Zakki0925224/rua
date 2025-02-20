use clap::Parser;
use std::fs::File;

mod bytecode;
mod lex;
mod parse;
mod utils;
mod value;
mod vm;

#[derive(Parser, Debug)]
struct Args {
    path: String,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let file = File::open(args.path)?;
    let proto = parse::ParseProto::load(file)?;
    vm::ExeState::new().execute(&proto)?;

    Ok(())
}
