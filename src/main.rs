mod lib;
use anyhow::{bail, Result};
use std::path::PathBuf;
use std::io::{BufRead, Write};
use clap::Parser as ClapParser;
use lib::lox::AstPrinter;
use lib::parser::parse;
use lib::scanning::scan;
use lib::interpreter::Interpreter;

#[derive(ClapParser, Debug)]
struct Args {
    file: Option<PathBuf>,
    #[clap(long)]
    print_ast: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    if let Some(pbuf) = args.file {
        if let Err(why) = run_file(pbuf, args.print_ast) {
            bail!(why)
        }
        return Ok(())
    }

    // If we got here, we're in prompt mode.
    println!("Running lox interpreter...");
    loop {
        match run_prompt(args.print_ast) {
            Ok(_) => continue,
            Err(why) => eprintln!("ERROR: {}", why),
        }
    }
}

fn run_file(path: PathBuf, print_ast: bool) -> Result<()> {
    println!("Running lox source at {}", path.display());
    if !path.exists() || !path.is_file() {
        bail!("Path does not exist or is not a valid file.");
    }
    let content = match std::fs::read(path) {
        Ok(c) => c,
        Err(_) => bail!("Failed to read file."),
    };
    match std::str::from_utf8(&content) {
        Ok(c) => {
            let _ = run(c, print_ast)?;
            Ok(())
        },
        Err(_) => bail!("Failed to decode unicode."),
    }
}

fn print_prompt() -> Result<()> {
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    // TODO: Why do std::io::Error not count for anyhow::Error!?
    if let Err(why) = handle.write(b"\n>> ") {
        bail!("Failed to write prompt to console.\n\nCaused by:\n{:#?}", why);
    }
    if let Err(why) = handle.flush() {
        bail!("Failed to flush prompt to console.\n\nCaused by:\n{:#?}", why);
    }
    Ok(())
}

fn run_prompt(print_ast: bool) -> Result<()> {
    let stdin = std::io::stdin();
    print_prompt()?;
    for line in stdin.lock().lines() {
        match line {
            Ok(l) => {
                run(&l, print_ast)?;
            }
            Err(_) => bail!("Failed to read line"),
        }
        print_prompt()?;
    }
    Ok(())
}

fn run(script: &str, print_ast: bool) -> Result<()> {
    let tokens = scan(script)?;

    let statements = match parse(&tokens) {
        Ok(v) => v,
        Err(why) => bail!(why),
    };

    if print_ast {
        let mut printer = AstPrinter {};
        for stmt in &statements {
            let expr_string = match printer.print(stmt) {
                Ok(v) => v,
                Err(why) => bail!(why)
            };
            println!("{}", expr_string);
        }
    }

    let mut interpreter = Interpreter {};
    if let Err(why) = interpreter.interpret(&statements) {
        bail!(why)
    }
    Ok(())
}