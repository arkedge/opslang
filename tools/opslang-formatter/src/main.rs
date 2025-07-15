use anyhow::{Context, Result};
use clap::Parser;
use opslang_formatter::{FormatterConfig, format_file, format_source};
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "opslang-formatter")]
#[command(about = "Format opslang source files")]
#[command(version)]
struct Cli {
    /// Configuration file path
    #[arg(short, long)]
    config: PathBuf,

    /// Input files to format (if none provided, reads from stdin)
    #[arg(value_name = "FILE")]
    files: Vec<PathBuf>,

    /// Format files in place
    #[arg(short, long)]
    in_place: bool,

    /// Print formatted output to stdout instead of modifying files
    #[arg(long)]
    stdout: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Load configuration
    let config = FormatterConfig::from_file(&cli.config)
        .with_context(|| format!("Failed to load config from {}", cli.config.display()))?;

    if cli.files.is_empty() {
        // Read from stdin
        let mut input = String::new();
        io::stdin()
            .read_to_string(&mut input)
            .context("Failed to read from stdin")?;

        let formatted =
            format_source(&input, &config).context("Failed to format input from stdin")?;

        print!("{formatted}");
    } else {
        // Process files
        for file_path in cli.files {
            if cli.stdout {
                // Read file and print formatted output to stdout
                let source = fs::read_to_string(&file_path)
                    .with_context(|| format!("Failed to read file: {}", file_path.display()))?;

                let formatted = format_source(&source, &config)
                    .with_context(|| format!("Failed to format file: {}", file_path.display()))?;

                println!("=== {} ===", file_path.display());
                print!("{formatted}");
            } else if cli.in_place {
                // Format file in place
                format_file(&file_path, &config).with_context(|| {
                    format!("Failed to format file in place: {}", file_path.display())
                })?;

                eprintln!("Formatted: {}", file_path.display());
            } else {
                // Read file and print formatted output to stdout (default behavior)
                let source = fs::read_to_string(&file_path)
                    .with_context(|| format!("Failed to read file: {}", file_path.display()))?;

                let formatted = format_source(&source, &config)
                    .with_context(|| format!("Failed to format file: {}", file_path.display()))?;

                print!("{formatted}");
            }
        }
    }

    Ok(())
}
