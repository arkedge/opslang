use clap::Parser;
use opslang_ast::version::VersionMarker;
use opslang_migration::{Migrate, V0ToV1};
use std::collections::HashSet;
use std::env;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use walkdir::WalkDir;

#[derive(Parser)]
#[command(
    name = "opslang-migration",
    about = "Migrates opslang source files from v0 to v1"
)]
struct Cli {
    /// Input file or directory paths
    #[arg(help = "Input file or directory paths to process")]
    inputs: Vec<String>,

    /// Output file path (defaults to stdout when processing single file)
    #[arg(short, long, value_name = "FILE")]
    output: Option<String>,

    /// Disable reading from stdin when no inputs provided
    #[arg(long, help = "Disable reading from stdin")]
    no_stdin: bool,

    /// Stop processing if input is a directory instead of processing recursively
    #[arg(long, help = "Don't process directories recursively")]
    no_recursive: bool,

    /// Stop on first error when processing multiple inputs
    #[arg(long, help = "Stop on first error")]
    fail_fast: bool,

    /// Ignore git working directory status check
    #[arg(long, help = "Ignore git working directory dirty status")]
    allow_dirty: bool,

    /// Create separate output files instead of in-place modification
    #[arg(
        long,
        help = "Create separate output files with version suffix instead of modifying in-place"
    )]
    separate_files: bool,

    /// Omit metadata comments (shebang is always included)
    #[arg(
        long,
        help = "Omit metadata comments like timestamp and command line (shebang is always included)"
    )]
    no_metadata: bool,
}

#[derive(Debug)]
enum MigrationError {
    IoError(std::io::Error),
    GitDirty,
    DirectoryInput,
    MigrationFailed(String),
    NoInputs,
}

impl std::fmt::Display for MigrationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MigrationError::IoError(e) => write!(f, "IO error: {e}"),
            MigrationError::GitDirty => write!(
                f,
                "Git working directory is dirty. Use --allow-dirty to bypass this check."
            ),
            MigrationError::DirectoryInput => write!(
                f,
                "Input is a directory. Use recursive processing by disabling --no-recursive or provide files directly."
            ),
            MigrationError::MigrationFailed(msg) => write!(f, "{msg}"),
            MigrationError::NoInputs => write!(
                f,
                "No inputs provided and stdin disabled. Provide input files or remove --no-stdin."
            ),
        }
    }
}

impl std::error::Error for MigrationError {}

impl From<std::io::Error> for MigrationError {
    fn from(error: std::io::Error) -> Self {
        MigrationError::IoError(error)
    }
}

impl From<walkdir::Error> for MigrationError {
    fn from(error: walkdir::Error) -> Self {
        MigrationError::IoError(error.into())
    }
}

fn check_git_status_in_directory(dir: &Path) -> Result<bool, MigrationError> {
    let output = Command::new("git")
        .args(["status", "--porcelain"])
        .current_dir(dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .output()
        .map_err(|_| {
            MigrationError::IoError(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "git command not found",
            ))
        })?;

    Ok(!output.stdout.is_empty())
}

fn validate_git_status_for_files(
    files: &[PathBuf],
    allow_dirty: bool,
) -> Result<(), MigrationError> {
    if allow_dirty {
        return Ok(());
    }

    let mut checked_dirs = HashSet::new();

    for file_path in files {
        if let Some(parent_dir) = file_path.parent() {
            let canonical_dir = parent_dir.canonicalize().map_err(MigrationError::IoError)?;

            if checked_dirs.insert(canonical_dir.clone())
                && check_git_status_in_directory(&canonical_dir)?
            {
                return Err(MigrationError::GitDirty);
            }
        }
    }

    Ok(())
}

fn is_opslang_file(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext == "ops" || ext == "opslang")
        .unwrap_or(false)
}

fn collect_input_files(inputs: &[String], recursive: bool) -> Result<Vec<PathBuf>, MigrationError> {
    let mut files = Vec::new();

    for input in inputs {
        let path = Path::new(input);

        if path.is_file() {
            files.push(path.to_path_buf());
        } else if path.is_dir() {
            if !recursive {
                return Err(MigrationError::DirectoryInput);
            }

            for entry in WalkDir::new(path) {
                let entry = entry?;
                let entry_path = entry.path();

                if entry_path.is_file() && is_opslang_file(entry_path) {
                    files.push(entry_path.to_path_buf());
                }
            }
        } else {
            return Err(MigrationError::IoError(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Input path not found: {input}"),
            )));
        }
    }

    Ok(files)
}

fn migrate_content(content: &str) -> Result<String, MigrationError> {
    V0ToV1.migrate(content).map_err(|e| {
        let error_msg = format_migration_error(&e.to_string());
        MigrationError::MigrationFailed(error_msg)
    })
}

fn add_metadata_to_content(content: &str, include_comments: bool) -> String {
    let metadata = generate_metadata(include_comments);
    format!("{metadata}{content}")
}

fn generate_output_filename(input_path: &Path) -> PathBuf {
    let version = opslang_ast::V1::version();
    let mut file_stem = input_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output")
        .to_string();

    file_stem.push_str(&format!(".{version}"));

    if let Some(ext) = input_path.extension().and_then(|s| s.to_str()) {
        file_stem.push('.');
        file_stem.push_str(ext);
    }

    input_path.with_file_name(file_stem)
}

fn generate_metadata(include_comments: bool) -> String {
    let mut metadata = String::new();

    // Shebang is always included
    metadata.push_str("#! lang=v1\n");

    if include_comments {
        // Add timestamp and command line info
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        let datetime = chrono::DateTime::from_timestamp(now as i64, 0)
            .unwrap()
            .format("%Y-%m-%d %H:%M:%S UTC");

        let args: Vec<String> = env::args().collect();
        let command_line = args.join(" ");

        metadata.push_str(&format!("# Generated on {datetime} by: {command_line}\n"));
    }

    metadata
}

fn format_migration_error(error: &str) -> String {
    if error.contains("Parse error:") {
        // Extract the location information if available
        if let Some(pos_start) = error.find("error at ")
            && let Some(pos_end) = error[pos_start..].find(": ")
        {
            let location = &error[pos_start + 9..pos_start + pos_end];
            return format!(
                "Parse error at {location}: The syntax is not valid opslang v0. Please check your syntax."
            );
        }
        "Parse error: The syntax is not valid opslang v0. Please check your syntax.".to_string()
    } else if error.contains("Conversion error:") {
        format!(
            "Conversion error: {}",
            error.strip_prefix("Conversion error: ").unwrap_or(error)
        )
    } else if error.contains("Print error:") {
        format!(
            "Output generation error: {}",
            error.strip_prefix("Print error: ").unwrap_or(error)
        )
    } else {
        error.to_string()
    }
}

fn process_single_input(
    input_content: String,
    output_path: Option<&str>,
    include_metadata_comments: bool,
    input_file_path: Option<&Path>,
    allow_dirty: bool,
) -> Result<(), MigrationError> {
    // Check git status for the file's directory if it's a file (not stdin)
    if let Some(file_path) = input_file_path {
        validate_git_status_for_files(&[file_path.to_path_buf()], allow_dirty)?;
    }
    let migrated_content = migrate_content(&input_content)?;
    let output_content = add_metadata_to_content(&migrated_content, include_metadata_comments);

    if let Some(output_file) = output_path {
        fs::write(output_file, output_content)?;
        eprintln!("Migration completed successfully. Output written to: {output_file}");
    } else {
        print!("{output_content}");
    }

    Ok(())
}

fn process_multiple_inputs(
    files: Vec<PathBuf>,
    fail_fast: bool,
    separate_files: bool,
    include_metadata_comments: bool,
    allow_dirty: bool,
) -> Result<(), MigrationError> {
    // Check git status for all directories before processing
    validate_git_status_for_files(&files, allow_dirty)?;

    let mut errors = Vec::new();

    for file_path in files {
        match fs::read_to_string(&file_path) {
            Ok(content) => match migrate_content(&content) {
                Ok(migrated_content) => {
                    let output_content =
                        add_metadata_to_content(&migrated_content, include_metadata_comments);

                    let output_path = if separate_files {
                        generate_output_filename(&file_path)
                    } else {
                        file_path.clone()
                    };

                    if let Err(e) = fs::write(&output_path, output_content) {
                        let error_msg = format!("Failed to write {}: {}", output_path.display(), e);
                        if fail_fast {
                            return Err(MigrationError::IoError(e));
                        }
                        errors.push(error_msg);
                    } else if separate_files {
                        eprintln!(
                            "Migrated: {} -> {}",
                            file_path.display(),
                            output_path.display()
                        );
                    } else {
                        eprintln!("Migrated: {}", file_path.display());
                    }
                }
                Err(e) => {
                    let error_msg = format!("{}: {}", file_path.display(), e);
                    if fail_fast {
                        return Err(e);
                    }
                    errors.push(error_msg);
                }
            },
            Err(e) => {
                let error_msg = format!("Failed to read {}: {}", file_path.display(), e);
                if fail_fast {
                    return Err(MigrationError::IoError(e));
                }
                errors.push(error_msg);
            }
        }
    }

    if !errors.is_empty() {
        eprintln!("Errors encountered during migration:");
        for error in &errors {
            eprintln!("  - {error}");
        }
        return Err(MigrationError::MigrationFailed(format!(
            "{} errors occurred",
            errors.len()
        )));
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    // Note: Git status is now checked per-directory in processing functions

    // Handle input sources
    match cli.inputs.len() {
        0 => {
            // No inputs provided
            if cli.no_stdin {
                eprintln!("Error: {}", MigrationError::NoInputs);
                std::process::exit(1);
            }

            // Read from stdin
            let mut buffer = String::new();
            if let Err(e) = io::stdin().read_to_string(&mut buffer) {
                eprintln!("Error: Failed to read from stdin: {e}");
                std::process::exit(1);
            }
            if let Err(e) = process_single_input(
                buffer,
                cli.output.as_deref(),
                !cli.no_metadata,
                None,
                cli.allow_dirty,
            ) {
                eprintln!("Error: {e}");
                std::process::exit(1);
            }
        }
        1 => {
            // Single input
            let input = &cli.inputs[0];

            if input == "-" {
                // Explicit stdin
                let mut buffer = String::new();
                if let Err(e) = io::stdin().read_to_string(&mut buffer) {
                    eprintln!("Error: Failed to read from stdin: {e}");
                    std::process::exit(1);
                }
                if let Err(e) = process_single_input(
                    buffer,
                    cli.output.as_deref(),
                    !cli.no_metadata,
                    None,
                    cli.allow_dirty,
                ) {
                    eprintln!("Error: {e}");
                    std::process::exit(1);
                }
            } else {
                let path = Path::new(input);
                if path.is_file() {
                    // Single file
                    let content = match fs::read_to_string(path) {
                        Ok(content) => content,
                        Err(e) => {
                            eprintln!("Error: Failed to read file '{input}': {e}");
                            std::process::exit(1);
                        }
                    };
                    let output_path = if cli.output.is_none() {
                        if cli.separate_files {
                            Some(generate_output_filename(path).to_string_lossy().to_string())
                        } else {
                            // In-place modification for single file
                            Some(input.to_string())
                        }
                    } else {
                        cli.output.clone()
                    };

                    if let Err(e) = process_single_input(
                        content,
                        output_path.as_deref(),
                        !cli.no_metadata,
                        Some(path),
                        cli.allow_dirty,
                    ) {
                        eprintln!("Error: {e}");
                        std::process::exit(1);
                    }
                } else if path.is_dir() {
                    // Single directory
                    let files = match collect_input_files(&cli.inputs, !cli.no_recursive) {
                        Ok(files) => files,
                        Err(e) => {
                            eprintln!("Error: {e}");
                            std::process::exit(1);
                        }
                    };
                    if let Err(e) = process_multiple_inputs(
                        files,
                        cli.fail_fast,
                        cli.separate_files,
                        !cli.no_metadata,
                        cli.allow_dirty,
                    ) {
                        eprintln!("Error: {e}");
                        std::process::exit(1);
                    }
                } else {
                    eprintln!("Error: Input path not found: {input}");
                    std::process::exit(1);
                }
            }
        }
        _ => {
            // Multiple inputs
            if cli.output.is_some() {
                eprintln!("Warning: Output file option ignored when processing multiple inputs");
            }

            let files = match collect_input_files(&cli.inputs, !cli.no_recursive) {
                Ok(files) => files,
                Err(e) => {
                    eprintln!("Error: {e}");
                    std::process::exit(1);
                }
            };
            if let Err(e) = process_multiple_inputs(
                files,
                cli.fail_fast,
                cli.separate_files,
                !cli.no_metadata,
                cli.allow_dirty,
            ) {
                eprintln!("Error: {e}");
                std::process::exit(1);
            }
        }
    }

    Ok(())
}
