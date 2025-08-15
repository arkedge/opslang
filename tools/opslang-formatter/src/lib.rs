use anyhow::{Context as AnyhowContext, Result};
use opslang_ast::v1::context::Context;
use opslang_parser::{ParseOps, ParserInput};
use opslang_printer::{CommentAligned, Naive, PrettyPrint, PrintOptions};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::fs;
use std::path::Path;

/// Runtime strategy selection for formatting
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum FormatterStrategy {
    Naive,
    #[default]
    CommentAligned,
}

/// Configuration for the formatter
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct FormatterConfig {
    pub strategy: FormatterStrategy,
    #[serde(flatten)]
    pub print_options: PrintOptionsConfig,
}

/// Serializable version of print options
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PrintOptionsConfig {
    /// Base options
    #[serde(flatten)]
    pub base: BasePrintOptionsConfig,
    /// Comment alignment configuration
    #[serde(flatten)]
    pub comment_alignment: CommentAlignmentConfig,
}

/// Serializable version of base print options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BasePrintOptionsConfig {
    /// String used for indentation (usually spaces or tabs)
    #[serde(default = "default_indent_str")]
    pub indent_str: String,

    /// Current indentation level
    #[serde(default)]
    pub indent_level: usize,

    /// Whether to reserve a space for break tokens
    #[serde(default = "default_reserve_for_break")]
    pub reserve_for_break: bool,

    /// Maximum line width
    #[serde(default = "default_max_width")]
    pub max_width: usize,

    /// Newline style
    #[serde(default)]
    pub newline_style: NewlineStyleConfig,
}

/// Serializable version of comment alignment
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CommentAlignmentConfig {
    /// Grouping strategy for comment alignment
    #[serde(default)]
    pub grouping: CommentGroupingConfig,
    /// Position calculation method
    #[serde(default)]
    pub position: CommentPositionConfig,
}

/// Serializable version of comment grouping
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CommentGroupingConfig {
    #[default]
    Consecutive,
    PerBlock,
}

/// Serializable version of comment position
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CommentPositionConfig {
    #[default]
    ToLongest,
    ToFixed {
        column: usize,
        fallback_to_longest: bool,
    },
    ToTabMultiple {
        tab_size: usize,
        fallback_to_longest: bool,
    },
}

/// Serializable version of newline style
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum NewlineStyleConfig {
    #[default]
    Unix,
    Windows,
}

// Default functions for serde
fn default_indent_str() -> String {
    "    ".to_string() // 4 spaces
}

fn default_reserve_for_break() -> bool {
    true
}

fn default_max_width() -> usize {
    80
}

impl Default for BasePrintOptionsConfig {
    fn default() -> Self {
        Self {
            indent_str: default_indent_str(),
            indent_level: 0,
            reserve_for_break: default_reserve_for_break(),
            max_width: default_max_width(),
            newline_style: NewlineStyleConfig::default(),
        }
    }
}

impl FormatterConfig {
    /// Load configuration from TOML file
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let content = fs::read_to_string(path.as_ref())
            .with_context(|| format!("Failed to read config file: {}", path.as_ref().display()))?;

        let config: Self =
            toml::from_str(&content).with_context(|| "Failed to parse TOML configuration")?;

        Ok(config)
    }
}

/// Convert config types to printer types
impl From<&FormatterConfig> for PrintOptions<Naive> {
    fn from(config: &FormatterConfig) -> Self {
        let base = opslang_printer::BasePrintOptions {
            indent_str: Cow::Owned(config.print_options.base.indent_str.clone()),
            indent_level: config.print_options.base.indent_level,
            reserve_for_break: config.print_options.base.reserve_for_break,
            max_width: config.print_options.base.max_width,
            newline_style: match config.print_options.base.newline_style {
                NewlineStyleConfig::Unix => opslang_printer::NewlineStyle::Unix,
                NewlineStyleConfig::Windows => opslang_printer::NewlineStyle::Windows,
            },
        };

        PrintOptions::new(base, Default::default())
    }
}

impl From<&FormatterConfig> for PrintOptions<CommentAligned> {
    fn from(config: &FormatterConfig) -> Self {
        let base = opslang_printer::BasePrintOptions {
            indent_str: Cow::Owned(config.print_options.base.indent_str.clone()),
            indent_level: config.print_options.base.indent_level,
            reserve_for_break: config.print_options.base.reserve_for_break,
            max_width: config.print_options.base.max_width,
            newline_style: match config.print_options.base.newline_style {
                NewlineStyleConfig::Unix => opslang_printer::NewlineStyle::Unix,
                NewlineStyleConfig::Windows => opslang_printer::NewlineStyle::Windows,
            },
        };

        let comment_alignment = opslang_printer::CommentAlignment {
            grouping: match config.print_options.comment_alignment.grouping {
                CommentGroupingConfig::Consecutive => opslang_printer::CommentGrouping::Consecutive,
                CommentGroupingConfig::PerBlock => opslang_printer::CommentGrouping::PerBlock,
            },
            position: match config.print_options.comment_alignment.position {
                CommentPositionConfig::ToLongest => opslang_printer::CommentPosition::ToLongest,
                CommentPositionConfig::ToFixed {
                    column,
                    fallback_to_longest,
                } => opslang_printer::CommentPosition::ToFixed {
                    column,
                    fallback_to_longest,
                },
                CommentPositionConfig::ToTabMultiple {
                    tab_size,
                    fallback_to_longest,
                } => opslang_printer::CommentPosition::ToTabMultiple {
                    tab_size,
                    fallback_to_longest,
                },
            },
        };

        PrintOptions::new(base, comment_alignment)
    }
}

/// Format opslang source code according to the specified configuration
pub fn format_source(source: &str, config: &FormatterConfig) -> Result<String> {
    // Create v1 context for allocating reference types
    let ctx = Context::new();

    // Parse the source code using ParseOps
    let input = ParserInput {
        content: source,
        file_name: "input.ops".into(),
    };

    let program = opslang_ast::v1::Program::parse(input, &ctx)
        .map_err(|e| anyhow::anyhow!("Parse error: {e:?}"))?;

    // Format using the appropriate strategy
    let mut output = String::new();

    match config.strategy {
        FormatterStrategy::Naive => {
            let options: PrintOptions<Naive> = config.into();
            PrettyPrint::<Naive>::pretty_print(&program, &mut output, &options)
                .with_context(|| "Failed to format with naive strategy")?;
        }
        FormatterStrategy::CommentAligned => {
            let options: PrintOptions<CommentAligned> = config.into();
            PrettyPrint::<CommentAligned>::pretty_print(&program, &mut output, &options)
                .with_context(|| "Failed to format with comment-aligned strategy")?;
        }
    }

    Ok(output)
}

/// Format a file in place
pub fn format_file<P: AsRef<Path>>(file_path: P, config: &FormatterConfig) -> Result<()> {
    let source = fs::read_to_string(&file_path)
        .with_context(|| format!("Failed to read file: {}", file_path.as_ref().display()))?;

    let formatted = format_source(&source, config)?;

    fs::write(&file_path, formatted)
        .with_context(|| format!("Failed to write file: {}", file_path.as_ref().display()))?;

    Ok(())
}
