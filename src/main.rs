mod lib;
use anyhow::{bail, Result};
use std::path::PathBuf;
//use std::io::{BufRead, Write};
use clap::Parser as ClapParser;
use lib::ast::{Expr, ExprVisitor, Op};
use lib::lox::AstPrinter;
use lib::scanning::{scan, TokenType};
use lib::parser::parse;

#[derive(ClapParser, Debug)]
struct Args {
    file: Option<PathBuf>
}

fn main() {
    let args = Args::parse();
    if let Err(why) = match args.file {
        Some(pbuf) => run_file(pbuf),
        None => run_printer(),
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

/*
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
*/

fn run(script: &str) -> Result<()> {
    println!("Script:");
    println!("{}", script);
    let tokens = scan(script)?;

    let expression = match parse(&tokens) {
        Ok(v) => v,
        Err(why) => bail!(why),
    };
    
    let mut printer = AstPrinter;
    println!("{}", printer.visit_expr(&expression));
    Ok(())
}

fn run_printer() -> Result<()> {
    let left = Box::new(Expr::Unary(
        Box::new(Op { token_type: TokenType::Minus }),
        Box::new(Expr::NumericLiteral(123.0)),
    ));
    let op = Box::new(Op { token_type: TokenType::Star });
    let right = Box::new(Expr::Grouping(Box::new(Expr::NumericLiteral(45.67))));
    let expression = Box::new(Expr::Binary(left, op, right));

    
    let mut printer = AstPrinter;
    println!("AST:");
    println!("{}", printer.visit_expr(&expression));
    Ok(())
}