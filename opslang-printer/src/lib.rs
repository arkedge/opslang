use std::fmt::{self, Write};
use std::marker::PhantomData;
use std::ops::Deref;

pub mod v1;

/// Sealed trait for formatting strategies
mod sealed {
    pub trait Sealed {}
}

/// Trait representing different formatting strategies.
///
/// This trait is sealed and can only be implemented by types defined in this crate.
/// Different strategies control how elements are formatted, particularly how comments
/// are positioned and aligned.
pub trait Strategy: sealed::Sealed + Default {}

/// Naive formatting strategy that adds comments with simple spacing.
///
/// This strategy follows a simple approach: comments are added after expressions
/// with a single space separator, without any column alignment.
#[derive(Debug, Clone, Copy, Default)]
pub struct Naive;

impl sealed::Sealed for Naive {}
impl Strategy for Naive {}

/// Comment-aligned formatting strategy that aligns comments in consecutive lines.
///
/// This strategy analyzes consecutive non-empty lines and aligns their comments
/// to the same column position. The alignment position is determined by the
/// alignment configuration in PrintOptions.
#[derive(Debug, Clone, Copy, Default)]
pub struct CommentAligned;

impl sealed::Sealed for CommentAligned {}
impl Strategy for CommentAligned {}

/// Comment grouping strategy for alignment
#[derive(Debug, Clone, Copy, Default)]
pub enum CommentGrouping {
    /// Group consecutive lines (separated by empty lines)
    #[default]
    Consecutive,
    /// Group comments per block scope
    PerBlock,
}

/// Comment position calculation method
#[derive(Debug, Clone, Copy, Default)]
pub enum CommentPosition {
    /// Align comments to the longest line in the group
    #[default]
    ToLongest,
    /// Align comments to a fixed column position
    /// If `fallback_to_longest` is true, lines longer than the fixed position
    /// fall back to longest alignment within the group
    ToFixed {
        column: usize,
        fallback_to_longest: bool,
    },
    /// Align comments to multiples of the given tab size
    /// If `fallback_to_longest` is true, lines that don't fit the tab boundary
    /// may fall back to longest alignment within the group
    ToTabMultiple {
        tab_size: usize,
        fallback_to_longest: bool,
    },
}

/// Configuration for comment alignment
#[derive(Debug, Clone, Copy, Default)]
pub struct CommentAlignment {
    /// Grouping strategy for comment alignment
    pub grouping: CommentGrouping,
    /// Position calculation method
    pub position: CommentPosition,
}

/// Style for newlines in output
#[derive(Debug, Clone, Copy)]
pub enum NewlineStyle {
    /// Unix style (\n)
    Unix,
    /// Windows style (\r\n)
    Windows,
}

/// Basic pretty-print options without comment alignment
#[derive(Debug, Clone, Copy)]
pub struct BasePrintOptions {
    /// String used for indentation (usually spaces or tabs)
    pub indent_str: &'static str,
    /// Current indentation level
    pub indent_level: usize,
    /// Maximum line width
    pub max_width: usize,
    /// Newline style
    pub newline_style: NewlineStyle,
    /// Whether to add trailing commas
    pub trailing_comma: bool,
}

/// Complete pretty-print options with comment alignment
#[derive(Debug, Clone, Copy, Default)]
pub struct PrintOptions<S: Strategy> {
    /// Base options
    pub base: BasePrintOptions,
    /// Comment alignment configuration
    pub comment_alignment: CommentAlignment,
    _strategy: PhantomData<S>,
}

impl Default for BasePrintOptions {
    fn default() -> Self {
        Self {
            indent_str: "    ", // 4 spaces
            indent_level: 0,
            max_width: 80,
            newline_style: NewlineStyle::Unix,
            trailing_comma: true,
        }
    }
}

impl BasePrintOptions {
    /// Create a new BasePrintOptions with increased indentation level
    pub fn with_increased_indent(&self) -> Self {
        let mut new_opts = *self;
        new_opts.indent_level += 1;
        new_opts
    }

    /// Get the current indentation string
    pub fn current_indent(&self) -> String {
        self.indent_str.repeat(self.indent_level)
    }

    /// Get the newline character(s)
    pub fn newline(&self) -> &'static str {
        match self.newline_style {
            NewlineStyle::Unix => "\n",
            NewlineStyle::Windows => "\r\n",
        }
    }
}

impl<S: Strategy> PrintOptions<S> {
    /// Create a new PrintOptions
    pub fn new(base: BasePrintOptions, comment_alignment: CommentAlignment) -> Self {
        Self {
            base,
            comment_alignment,
            _strategy: PhantomData,
        }
    }

    /// Create a new PrintOptions with increased indentation level
    pub fn with_increased_indent(&self) -> Self {
        Self {
            base: self.base.with_increased_indent(),
            comment_alignment: self.comment_alignment,
            _strategy: PhantomData,
        }
    }

    /// Create PrintOptions from BasePrintOptions with default comment alignment
    pub fn from_base(base: BasePrintOptions) -> Self {
        Self {
            base,
            comment_alignment: CommentAlignment::default(),
            _strategy: PhantomData,
        }
    }

    /// Create PrintOptions from BasePrintOptions with specific comment alignment
    pub fn from_base_with_alignment(
        base: BasePrintOptions,
        comment_alignment: CommentAlignment,
    ) -> Self {
        Self {
            base,
            comment_alignment,
            _strategy: PhantomData,
        }
    }
}

/// Deref implementation to allow PrintOptions to be used as BasePrintOptions
impl<S: Strategy> Deref for PrintOptions<S> {
    type Target = BasePrintOptions;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

/// Main trait for pretty-printing AST elements with different strategies
///
/// This trait is version-agnostic and can be implemented for AST elements
/// from different versions. The strategy type parameter determines how
/// formatting is performed, particularly for comment alignment.
pub trait PrettyPrint<S: Strategy> {
    /// Convert to a pretty-printed string
    fn to_pretty_string(&self, options: &PrintOptions<S>) -> String {
        let mut result = String::new();
        self.pretty_print(&mut result, options).unwrap();
        result
    }

    /// Write pretty-printed output to the specified writer
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result;
}

/// Pretty-printer instance with a specific strategy
pub struct Printer<S: Strategy> {
    options: PrintOptions<S>,
}

impl<S: Strategy> Printer<S> {
    /// Create a new printer with the specified options
    pub fn new(options: PrintOptions<S>) -> Self {
        Self { options }
    }

    /// Create a printer with default options
    pub fn with_default_options() -> Self {
        Self::new(PrintOptions::default())
    }

    /// Pretty-print the specified object to a string
    pub fn print<T: PrettyPrint<S>>(&self, item: &T) -> String {
        item.to_pretty_string(&self.options)
    }

    /// Pretty-print the specified object to the specified writer
    pub fn print_to<T: PrettyPrint<S>>(&self, item: &T, writer: &mut impl Write) -> fmt::Result {
        item.pretty_print(writer, &self.options)
    }

    /// Create a new printer with modified options
    pub fn with_options(&self, options: PrintOptions<S>) -> Self {
        Self { options }
    }
}

impl<S: Strategy> Default for Printer<S> {
    fn default() -> Self {
        Self::with_default_options()
    }
}

/// Zero-sized type for writing indentation
///
/// This type provides a method to write indentation to a writer. It's designed
/// as a ZST (Zero-Sized Type) to provide a clean API for indentation operations
/// while maintaining zero runtime cost.
///
/// # Usage
///
/// ```rust
/// # use opslang_printer::{Indent, PrintOptions, Naive};
/// # use std::fmt::Write;
/// # let mut output = String::new();
/// # let options = PrintOptions::<Naive>::default();
/// Indent.write(&mut output, &options).unwrap();
/// ```
#[derive(Debug, Clone, Copy, Default)]
pub struct Indent;

impl Indent {
    /// Write the current indentation to the writer
    pub fn write(self, writer: &mut impl Write, options: &BasePrintOptions) -> fmt::Result {
        writer.write_str(&options.current_indent())
    }
}

/// Zero-sized type for writing newlines
///
/// This type provides a method to write newlines to a writer according to the
/// configured newline style. It's designed as a ZST to provide a clean API
/// for newline operations while maintaining zero runtime cost.
///
/// # Usage
///
/// ```rust
/// # use opslang_printer::{Newline, PrintOptions, Naive};
/// # use std::fmt::Write;
/// # let mut output = String::new();
/// # let options = PrintOptions::<Naive>::default();
/// Newline.write(&mut output, &options).unwrap();
/// ```
#[derive(Debug, Clone, Copy, Default)]
pub struct Newline;

impl Newline {
    /// Write a newline to the writer
    pub fn write(self, writer: &mut impl Write, options: &BasePrintOptions) -> fmt::Result {
        writer.write_str(options.newline())
    }
}

/// Zero-sized type for writing indented newlines
///
/// This type provides a method to write a newline followed by indentation.
/// It's designed as a ZST to provide a clean API for combined newline and
/// indentation operations while maintaining zero runtime cost.
///
/// # Usage
///
/// ```rust
/// # use opslang_printer::{IndentedNewline, PrintOptions, Naive};
/// # use std::fmt::Write;
/// # let mut output = String::new();
/// # let options = PrintOptions::<Naive>::default();
/// IndentedNewline.write(&mut output, &options).unwrap();
/// ```
#[derive(Debug, Clone, Copy, Default)]
pub struct IndentedNewline;

impl IndentedNewline {
    /// Write a newline followed by indentation to the writer
    pub fn write(self, writer: &mut impl Write, options: &BasePrintOptions) -> fmt::Result {
        Newline.write(writer, options)?;
        Indent.write(writer, options)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base_print_options_default() {
        let opts = BasePrintOptions::default();
        assert_eq!(opts.indent_str, "    ");
        assert_eq!(opts.indent_level, 0);
        assert_eq!(opts.max_width, 80);
        assert!(opts.trailing_comma);
    }

    #[test]
    fn test_print_options_default() {
        let opts = PrintOptions::<Naive>::default();
        assert_eq!(opts.base.indent_str, "    ");
        assert_eq!(opts.base.indent_level, 0);
        assert_eq!(opts.base.max_width, 80);
        assert!(opts.base.trailing_comma);
        assert!(matches!(
            opts.comment_alignment.position,
            CommentPosition::ToLongest
        ));
        assert!(matches!(
            opts.comment_alignment.grouping,
            CommentGrouping::Consecutive
        ));
    }

    #[test]
    fn test_print_options_deref() {
        let opts = PrintOptions::<Naive>::default();
        // Can access BasePrintOptions fields directly through Deref
        assert_eq!(opts.indent_str, "    ");
        assert_eq!(opts.indent_level, 0);
        assert_eq!(opts.max_width, 80);
        assert!(opts.trailing_comma);
    }

    #[test]
    fn test_base_print_options_indent() {
        let opts = BasePrintOptions::default();
        let nested = opts.with_increased_indent();
        assert_eq!(nested.indent_level, 1);
        assert_eq!(nested.current_indent(), "    ");
    }

    #[test]
    fn test_print_options_indent() {
        let opts = PrintOptions::<Naive>::default();
        let nested = opts.with_increased_indent();
        assert_eq!(nested.indent_level, 1);
        assert_eq!(nested.current_indent(), "    ");
    }

    #[test]
    fn test_zst_helpers() {
        let opts = BasePrintOptions::default();
        let mut output = String::new();

        Indent.write(&mut output, &opts).unwrap();
        assert_eq!(output, "");

        Newline.write(&mut output, &opts).unwrap();
        assert_eq!(output, "\n");

        output.clear();
        let nested_opts = opts.with_increased_indent();
        IndentedNewline.write(&mut output, &nested_opts).unwrap();
        assert_eq!(output, "\n    ");
    }

    #[test]
    fn test_zst_helpers_with_print_options() {
        let opts = PrintOptions::<Naive>::default();
        let mut output = String::new();

        // PrintOptions can be used where BasePrintOptions is expected due to Deref
        Indent.write(&mut output, &opts).unwrap();
        assert_eq!(output, "");

        Newline.write(&mut output, &opts).unwrap();
        assert_eq!(output, "\n");

        output.clear();
        let nested_opts = opts.with_increased_indent();
        IndentedNewline.write(&mut output, &nested_opts).unwrap();
        assert_eq!(output, "\n    ");
    }

    #[test]
    fn test_printer_creation() {
        let printer: Printer<Naive> = Printer::default();
        assert_eq!(printer.options.indent_level, 0);
    }

    #[test]
    fn test_printer_with_options() {
        let options = PrintOptions {
            base: BasePrintOptions {
                indent_str: "\t",
                indent_level: 2,
                max_width: 120,
                newline_style: NewlineStyle::Windows,
                trailing_comma: false,
            },
            comment_alignment: CommentAlignment {
                grouping: CommentGrouping::Consecutive,
                position: CommentPosition::ToFixed {
                    column: 40,
                    fallback_to_longest: false,
                },
            },
            ..Default::default()
        };
        let printer: Printer<Naive> = Printer::new(options);
        assert_eq!(printer.options.indent_str, "\t");
        assert_eq!(printer.options.indent_level, 2);
        assert_eq!(printer.options.max_width, 120);
        assert!(!printer.options.trailing_comma);
    }

    #[test]
    fn test_construction_methods() {
        let base = BasePrintOptions {
            indent_str: "\t",
            indent_level: 1,
            max_width: 120,
            newline_style: NewlineStyle::Windows,
            trailing_comma: false,
        };

        let opts_default = PrintOptions::<Naive>::from_base(base);
        assert_eq!(opts_default.indent_str, "\t");
        assert!(matches!(
            opts_default.comment_alignment.position,
            CommentPosition::ToLongest
        ));

        let opts_aligned = PrintOptions::<Naive>::from_base_with_alignment(
            base,
            CommentAlignment {
                grouping: CommentGrouping::Consecutive,
                position: CommentPosition::ToFixed {
                    column: 40,
                    fallback_to_longest: false,
                },
            },
        );
        assert_eq!(opts_aligned.indent_str, "\t");
        assert!(matches!(
            opts_aligned.comment_alignment.position,
            CommentPosition::ToFixed {
                column: 40,
                fallback_to_longest: false
            }
        ));
    }
}
