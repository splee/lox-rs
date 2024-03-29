mod lib;
use anyhow::{bail, Result};
use clap::Parser as ClapParser;
use lib::interpreter::Interpreter;
use lib::lox::AstPrinter;
use lib::parser::parse;
use lib::scanner::scan;
use std::io;
use std::io::{BufRead, Write};
use std::path::PathBuf;
use tracing::{debug, error, info, instrument, warn};
use tracing_subscriber::filter::EnvFilter;

#[derive(ClapParser, Debug)]
struct Args {

    /// The path to a Lox script to run.  If no path is passed, the interpreter will be launched.
    file: Option<PathBuf>,

    /// Print the AST as the program is executed.
    #[clap(long, action)]
    print_ast: bool,

    /// Enable detailed logging.
    #[clap(long, action)]
    enable_logging: bool,
}

fn setup_tracing() -> Result<()> {
    let filter: EnvFilter = "trace".parse().expect("filter should parse");
    if let Err(why) = tracing_subscriber::fmt()
        .pretty()
        .with_writer(io::stderr)
        .with_env_filter(filter)
        .try_init()
    {
        bail!(why)
    } else {
        Ok(())
    }
}

#[instrument]
fn main() -> Result<()> {
    let args = Args::parse();

    if args.enable_logging {
        setup_tracing()?;
    }

    if let Some(pbuf) = args.file {
        info!(path = pbuf.to_str(), "Executing lox file");
        if let Err(why) = run_file(pbuf, args.print_ast) {
            error!("Failed to execute file - {}", why);
            bail!(why)
        }
        return Ok(());
    }

    // If we got here, we're in prompt mode.
    debug!("No path found in args, starting in interactive mode.");
    println!("Running lox interpreter...");
    match run_prompt(args.print_ast) {
        Ok(_) => Ok(()),
        Err(why) => {
            error!("Error running interactive interpreter: {}", why);
            bail!(why)
        }
    }
}

fn run_file(path: PathBuf, print_ast: bool) -> Result<()> {
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
        }
        Err(_) => bail!("Failed to decode unicode."),
    }
}

fn print_prompt() -> Result<()> {
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    // TODO: Why do std::io::Error not count for anyhow::Error!?
    if let Err(why) = handle.write(b"\n>> ") {
        bail!(
            "Failed to write prompt to console.\n\nCaused by:\n{:#?}",
            why
        );
    }
    if let Err(why) = handle.flush() {
        bail!(
            "Failed to flush prompt to console.\n\nCaused by:\n{:#?}",
            why
        );
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

#[instrument(skip(script))]
fn run(script: &str, print_ast: bool) -> Result<()> {
    let tokens = match scan(script) {
        Ok(v) => v,
        Err(why) => bail!("Failed to scan source: {}", why),
    };
    debug!(tokens = tokens.len(), "Tokens scanned successfully.");

    let statements = match parse(&tokens) {
        Ok(v) => v,
        Err(why) => {
            error!("Failed to parse statements: {}", why);
            bail!(why);
        }
    };

    if print_ast {
        let mut printer = AstPrinter {};
        for stmt in &statements {
            let expr_string = match printer.print(stmt) {
                Ok(v) => v,
                Err(why) => bail!(why),
            };
            println!("{}", expr_string);
        }
    }

    let mut interpreter = Interpreter::new(std::io::stdout());
    if let Err(why) = interpreter.interpret(&statements) {
        bail!(why)
    }
    Ok(())
}
