use opslang_ast::Row;
use peg::str::LineCol;
use thiserror::Error;

pub mod parser {
    pub use crate::ops_parser::row as parse_row;
}

peg::parser! {
    grammar ops_parser() for str {
        use opslang_ast::*;
        rule file_path() -> FilePath
            = full_name:ident()
            { FilePath { full_name: full_name.to_owned() } }

        rule variable_path() -> VariablePath
            = raw:ident()
            { VariablePath { raw: raw.to_owned() } }

        rule ident() -> &'input str
            = $(
                [c if c.is_ascii_alphabetic()]
                [c if c.is_ascii_alphanumeric() || c == '_' || c == '/' || c == '.' || c == '-']*
            )

        rule destination() -> Destination
            = component:$([c if c.is_ascii_alphanumeric()]+) destination_sep()
            exec_method:$([c if c.is_ascii_alphanumeric() || c == '_']+)
            { Destination { component: component.to_owned(), exec_method: exec_method.to_owned() } }

        rule destination_sep() = "_"

        pub(crate) rule command() -> Command
            = destinations:(d:destination() _ "." {d})* _ name:cmd_name() _ args:(e:expr() _ {e})*
            { Command { destinations, name: name.to_owned(), args } }

        rule cmd_name() -> &'input str
            = $(
                [c if c.is_ascii_alphabetic()]
                [c if c.is_ascii_alphanumeric() || c == '_']*
            )

        pub(crate) rule expr() -> Expr
            = precedence!{
                x:@ _ "||" _ y:(@) { Expr::BinOp(BinOpKind::Or, Box::new(x), Box::new(y)) }
                x:@ _ "if" __ y:(@) { Expr::BinOp(BinOpKind::If, Box::new(x), Box::new(y)) }
                --
                x:@ _ "and" __ y:(@) { Expr::BinOp(BinOpKind::And, Box::new(x), Box::new(y)) }
                --
                x:(@) _ e:eq_binop_kind() _ y:@ {Expr::BinOp(BinOpKind::Compare(e), Box::new(x), Box::new(y)) }
                --
                x:(@) _ "in" __ y:@ { Expr::BinOp(BinOpKind::In, Box::new(x), Box::new(y)) }
                x:(@) _ c:compare_binop_kind() _ y:@ { Expr::BinOp(BinOpKind::Compare(c), Box::new(x), Box::new(y)) }
                --
                x:(@) _ "+" _ y:@ { Expr::BinOp(BinOpKind::Add, Box::new(x), Box::new(y)) }
                x:(@) _ "-" _ y:@ { Expr::BinOp(BinOpKind::Sub, Box::new(x), Box::new(y)) }
                        "-" _ v:@ { Expr::UnOp(UnOpKind::Neg, Box::new(v)) }
                --
                x:(@) _ "*" _ y:@ { Expr::BinOp(BinOpKind::Mul, Box::new(x), Box::new(y)) }
                x:(@) _ "/" _ y:@ { Expr::BinOp(BinOpKind::Div, Box::new(x), Box::new(y)) }
                x:(@) _ "%" _ y:@ { Expr::BinOp(BinOpKind::Mod, Box::new(x), Box::new(y)) }
                --
                x:@ _ "(" _ v:(expr() ** (_ "," _)) _ ")" { Expr::FunCall(Box::new(x), v) }
                --
                "(" _ v:expr() _ ")" { v }
                n:literal() { Expr::Literal(n) }
                v:variable_path() { Expr::Variable(v) }
            }

        pub(crate) rule numeric() -> Numeric
            = "0x" i:$(['a'..='f' | 'A'..='F' | '0'..='9' | '_']+)
            { Numeric::Integer(i.to_owned(), IntegerPrefix::Hexadecimal) }
            / "0o" i:$(['0'..='7' | '_']+)
            { Numeric::Integer(i.to_owned(), IntegerPrefix::Octal) }
            / "0b" i:$(['0' | '1' | '_']+)
            { Numeric::Integer(i.to_owned(), IntegerPrefix::Binary) }
            / i:$(['0'..='9']['0'..='9' | '_']*) !['.' | 'e' | 'E']
            { Numeric::Integer(i.to_owned(), IntegerPrefix::Decimal) }
            / f:$(['0'..='9']['0'..='9' | '_']*
                "."? (['0'..='9']['0'..='9' | '_']*)?
                (['e' | 'E']['+' | '-']['0'..='9' | '_']*)?
            ) { Numeric::Float(f.to_owned()) }

        rule numeric_suffix() -> NumericSuffix
            = "s" ![c if c.is_alphanumeric()] { NumericSuffix::Second }

        rule literal() -> Literal
            = "[" _ v:(expr() ** (_ "," _)) _ "]" { Literal::Array(v) }
            / "\"" s:$([c if c != '"']*) "\"" { Literal::String(s.to_owned()) }
            / n:numeric() s:numeric_suffix()? { Literal::Numeric(n, s) }

        rule compare_binop_kind() -> CompareBinOpKind
            = ">=" { CompareBinOpKind::GreaterEq }
            / "<=" { CompareBinOpKind::LessEq }
            / ">" { CompareBinOpKind::Greater }
            / "<" { CompareBinOpKind::Less }

        rule eq_binop_kind() -> CompareBinOpKind
            = "!=" { CompareBinOpKind::NotEqual }
            / "==" { CompareBinOpKind::Equal }

        pub(crate) rule call() -> Call
            = "call" __ path:file_path()
            { Call { path } }

        pub(crate) rule wait_sec() -> WaitSec
            = "wait_sec" __ sec:expr()
            { WaitSec { sec } }

        pub(crate) rule wait_until() -> WaitUntil
            = "wait_until" __ condition:expr()
            { WaitUntil { condition } }

        pub(crate) rule wait_inc() -> WaitInc
            = "wait_inc" __ condition:expr()
            { WaitInc { condition } }

        pub(crate) rule check_value() -> CheckValue
            = "check_value" __ condition:expr()
            { CheckValue { condition } }

        pub(crate) rule let_bind() -> Let
            = "let" __ raw:ident() _ "=" _ rhs:expr()
            { Let { variable: Ident { raw: raw.to_owned()}, rhs } }

        pub(crate) rule get() -> Get
            = "get" __ variable:variable_path()
            { Get { variable } }

        rule reserved_control() -> ReservedControl
            = call:call() { ReservedControl::Call(call) }
            / wait_sec:wait_sec() { ReservedControl::WaitSec(wait_sec) }
            / wait_until:wait_until() { ReservedControl::WaitUntil(wait_until) }
            / check_value:check_value() { ReservedControl::CheckValue(check_value) }
            / let_bind:let_bind() { ReservedControl::Let(let_bind) }
            / get:get() { ReservedControl::Get(get) }
            / command:command() { ReservedControl::Command(command) }

        rule comment() -> Comment
            = "#" _ s:$([_]*) { Comment(s.to_owned()) }

        pub rule row_() -> Row
            = breaks:"."? _ r:(
                  content:reserved_control() _ comment_trailing:comment()?
                    { Row { breaks, content: Some(content), comment_trailing } }
                / comment_trailing:comment()?
                    { Row { breaks, content: None, comment_trailing } }
            ) { r }

        pub rule row() -> SRow
            = spanned(<row_()>)

        rule _() = ws()*
        rule __() = ![c if c.is_alphanumeric()] _

        rule ws() = quiet!{[c if c.is_whitespace()]}

        // cf. https://github.com/kevinmehall/rust-peg/issues/283#issuecomment-1014858352
        rule spanned<T>(inner : rule<T>) -> Spanned<T>
            = b:position!() value:inner() e:position!() { Spanned { span: b..e, value } }
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("parse failed: {0}")]
    ParseError(#[from] peg::error::ParseError<LineCol>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_numeric() {
        let r = ops_parser::numeric("5e-3");
        dbg!(r.unwrap());
        let r = ops_parser::numeric("0");
        dbg!(r.unwrap());
        let r = ops_parser::numeric("10");
        dbg!(r.unwrap());
        let r = ops_parser::numeric("0.1");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_expr() {
        let r = ops_parser::expr("a + b * c");
        dbg!(r.unwrap());
        let r = ops_parser::expr("a * b + c");
        dbg!(r.unwrap());
        let r = ops_parser::expr("1 + (a.a * (0xB_C))");
        dbg!(r.unwrap());
        let r = ops_parser::expr("a ((c), d) + b");
        dbg!(r.unwrap());
        let r = ops_parser::expr("ABC.DEF == 0xabcdef || 5s");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_space() {
        fn differ<T: std::cmp::PartialEq + std::fmt::Debug, E: std::fmt::Debug>(
            f: impl Fn(&'static str) -> Result<T, E>,
            l: &'static str,
            r: &'static str,
        ) {
            let l = f(l).unwrap();
            let r = f(r).unwrap();
            assert_ne!(l, r);
        }
        fn not_differ<T: std::cmp::PartialEq + std::fmt::Debug, E: std::fmt::Debug>(
            f: impl Fn(&'static str) -> Result<T, E>,
            l: &'static str,
            r: &'static str,
        ) {
            let l = f(l).unwrap();
            let r = f(r).unwrap();
            assert_eq!(l, r);
        }
        differ(
            ops_parser::expr,
            "EXAMPLE.VARIABLE.NAME/s",
            "EXAMPLE.VARIBLAE.NAME / s",
        );
        not_differ(
            ops_parser::check_value,
            "check_value x in [ 100, 200 ]",
            "check_value x in[ 100,200]",
        );
        not_differ(ops_parser::let_bind, "let variable=0", "let variable = 0");
    }
    #[test]
    fn test_fun_call() {
        let r = ops_parser::let_bind(
            "let result_of_test_fun = Test.fun2(Test.fun1([P.VEC.X, P.VEC.Y, P.VEC.Z]))",
        );
        dbg!(r.unwrap());
    }
    #[test]
    fn test_call() {
        let r = ops_parser::call("call OTHER_FILE.ops");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_wait_sec() {
        let r = ops_parser::wait_sec("wait_sec 12");
        dbg!(r.unwrap());
        let r = ops_parser::wait_sec("wait_sec 0.1");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_wait_until() {
        let r = ops_parser::wait_until("wait_until HEX.VALUE == 0x0123cdef || 5s");
        dbg!(r.unwrap());
        let r = ops_parser::wait_until("wait_until SOME.CONDITION.TO.BE.TESTED == FALSE");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_check_value() {
        let r = ops_parser::check_value("check_value TEST.VAR1 < 0.05");
        dbg!(r.unwrap());
        let r = ops_parser::check_value("check_value TEST.VAR2.X  in [ 0.01, 0.04 ]");
        dbg!(r.unwrap());
        let r = ops_parser::check_value(r#"check_value TEST.VAR_3 == "OFF""#);
        dbg!(r.unwrap());
        let r = ops_parser::check_value(
            r#"check_value TEST.VAR_4.X in [ 0, 0.07 ] if TEST.Var5 == "OFF""#,
        );
        dbg!(r.unwrap());
    }
    #[test]
    fn test_let() {
        let r = ops_parser::let_bind("let relative_x = relative_x + 9");
        dbg!(r.unwrap());
        let r = ops_parser::let_bind("let HYPHEN-IS_VALID = 1");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_get() {
        let r = ops_parser::get("get abc_xyz");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_command() {
        let r = ops_parser::command("Cmd_DO_IT");
        dbg!(r.unwrap());
        let r = ops_parser::command("ABC_DEF.Cmd_DO_IT");
        dbg!(r.unwrap());
        let r = ops_parser::command("ABC_DEF.Cmd_WITH_ARGS some_value 0xaa 0xbb 2");
        dbg!(r.unwrap());
        let r = ops_parser::command("ABC_DEF.G_H.Cmd_WITH_ARGS relative_x 0xcc 0xdd 0xee 0xff");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_rows() {
        let s = r#".# ****** #
.let CURRENT_EXAMPLE_TLM_VALUE = FOO.BAR.EXAMPLE_TLM.VALUE
.ABC_DEF.Cmd_DO_IT
 wait_until FOO.BAR.EXAMPLE_TLM.VALUE > CURRENT_EXAMPLE_TLM_VALUE || 5s"#;
        for l in s.lines() {
            let r = ops_parser::row(l);
            dbg!(r.unwrap());
        }
    }
}
