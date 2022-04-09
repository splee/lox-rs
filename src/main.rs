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
}

fn main() {
    let args = Args::parse();
    if let Err(why) = match args.file {
        Some(pbuf) => run_file(pbuf),
        None => run_prompt(),
    } {
        eprintln!("ERROR: {}", why);
    }
}

fn run_file(path: PathBuf) -> Result<()> {
    println!("Running lox source at {}", path.display());
    if !path.exists() || !path.is_file() {
        bail!("Path does not exist or is not a valid file.");
    }
    let content = match std::fs::read(path) {
        Ok(c) => c,
        Err(_) => bail!("Failed to read file."),
    };
    match std::str::from_utf8(&content) {
        Ok(c) => run(c),
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

fn run_prompt() -> Result<()> {
    println!("Running lox interpreter...");
    let stdin = std::io::stdin();
    print_prompt()?;
    for line in stdin.lock().lines() {
        println!("");
        match line {
            Ok(l) => run(&l)?,
            Err(_) => bail!("Failed to read line"),
        }
        print_prompt()?;
    }
    Ok(())
}

fn run(script: &str) -> Result<()> {
    println!("Script:");
    println!("{}", script);
    let tokens = scan(script)?;

    let expression = match parse(&tokens) {
        Ok(v) => v,
        Err(why) => bail!(why),
    };

    let mut printer = AstPrinter {};
    let expr_string = match printer.print(&expression) {
        Ok(v) => v,
        Err(why) => bail!(why)
    };
    println!("{}", expr_string);

    let mut interpreter = Interpreter {};
    let result = match interpreter.evaluate(&expression) {
        Ok(v) => v,
        Err(why) => bail!(why),
    };
    println!("Object: {:?}", result);
    Ok(())
}