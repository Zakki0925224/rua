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
    path: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    match args.path {
        Some(path) => run_file(&path),
        None => run_repl(),
    }
}

fn run_file(path: &str) -> anyhow::Result<()> {
    let file = File::open(path)?;
    let proto = parse::ParseProto::load(file)?;
    vm::ExeState::new().execute(&proto)?;

    Ok(())
}

fn run_repl() -> anyhow::Result<()> {
    println!("Lua interpreter (Exit with Ctrl+C)");

    let mut exe_state = vm::ExeState::new();
    let mut editor = rustyline::DefaultEditor::new()?;
    loop {
        let line = editor.readline("\n>> ")?;

        if line.trim().is_empty() {
            continue;
        }

        editor.add_history_entry(line.as_str())?;

        // parse
        let proto = match parse::ParseProto::load(line.as_bytes()) {
            Ok(proto) => proto,
            Err(e) => {
                eprintln!("Parse error: {}", e);
                continue;
            }
        };
        // exectute
        if let Err(e) = exe_state.execute(&proto) {
            eprintln!("Execution error: {}", e);
        }
    }
}
