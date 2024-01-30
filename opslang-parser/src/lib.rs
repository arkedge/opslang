use opslang_ast::Row;
use peg::str::LineCol;
use thiserror::Error;

pub mod parser {
    pub use crate::ops_parser::{row as parse_row, statements as parse_statements};
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

        rule tlm_ref() -> VariablePath
            = "$" raw:ident()
            { VariablePath { raw: raw.to_owned() } }

        rule ident() -> &'input str
            = $(
                [c if c.is_ascii_alphabetic()]
                [c if c.is_ascii_alphanumeric() || c == '_' || c == '/' || c == '.' || c == '-']*
            )

        rule alphanum_underscore() -> &'input str
            = $([c if c.is_ascii_alphanumeric() || c == '_']*)

        rule receiver() -> ReceiverComponent
            = exec_method:alphanum_underscore() "." name:alphanum_underscore()
            { ReceiverComponent { name: name.to_owned(), exec_method: exec_method.to_owned() } }
        rule executor() -> ExecutorComponent
            = name:alphanum_underscore()
            { ExecutorComponent { name: name.to_owned() } }

        rule time_indicator() -> Expr
            = e:expr() ":" { e } // TODO: これで大丈夫か検討

        rule destination_spec() -> DestinationSpec
           = receiver_component:("@" r:receiver() { r })? _
             time_indicator:time_indicator()? _
             executor_component:("@@" e:executor() { e })?
           { DestinationSpec { receiver_component, time_indicator, executor_component } }

        pub(crate) rule command() -> Command
            = destination:destination_spec() _ name:cmd_name() _ args:(e:expr() _ {e})*
            { Command { destination, name: name.to_owned(), args } }

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
                x:@ _ "&&" __ y:(@) { Expr::BinOp(BinOpKind::And, Box::new(x), Box::new(y)) }
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
                v:tlm_ref() { Expr::TlmRef(v) }
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
            / "time!" _ "(" _ content:$([c if c != ')']*) _ ")" {?
                let datetime = chrono::DateTime::parse_from_rfc3339(content).map_err(|_| "valid datetime")?;
                Ok(Literal::DateTime(datetime.into()))
            }
            / "tlmid!" _ "(" _ content:$([c if c != ')']*) _ ")" {?
                Ok(Literal::TlmId(content.to_owned()))
            }

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

        pub(crate) rule wait() -> Wait
            = "wait" __ condition:expr()
            { Wait { condition } }

        pub(crate) rule wait_inc() -> WaitInc
            = "wait_inc" __ condition:expr()
            { WaitInc { condition } }

        pub(crate) rule assert() -> Assert
            = "assert" __ condition:expr()
            { Assert { condition } }

        pub(crate) rule assert_eq() -> AssertEq
            = "assert_eq" __ left:expr() _ "," _ right:expr()
            { AssertEq { left, right, tolerance: None } }

        pub(crate) rule assert_approx_eq() -> AssertEq
            = "assert_approx_eq" __ left:expr() _ "," _ right:expr() _ "," _ tolerance:expr()
            { AssertEq { left, right, tolerance: Some(tolerance) } }

        pub(crate) rule let_bind() -> Let
            = "let" __ raw:ident() _ "=" _ rhs:expr()
            { Let { variable: Ident { raw: raw.to_owned()}, rhs } }

        pub(crate) rule print() -> Print
            = "print" __ arg:expr()
            { Print { arg } }

        pub(crate) rule set() -> Set
            = "set" __ name:variable_path() _ "=" _ expr:expr()
            { Set { name, expr } }

        rule reserved_control() -> SingleStatement
            = call:call() { SingleStatement::Call(call) }
            / wait:wait() { SingleStatement::Wait(wait) }
            / assert:assert() { SingleStatement::Assert(assert) }
            / assert_eq:assert_eq() { SingleStatement::AssertEq(assert_eq) }
            / assert_eq:assert_approx_eq() { SingleStatement::AssertEq(assert_eq) }
            / let_bind:let_bind() { SingleStatement::Let(let_bind) }
            / print:print() { SingleStatement::Print(print) }
            / set:set() { SingleStatement::Set(set) }
            / command:command() { SingleStatement::Command(command) }

        rule comment() -> Comment
            = "#" _ s:$([c if c != '\n']*) { Comment(s.to_owned()) }

        pub rule row_() -> Row
            = _ breaks:"."? _ r:(
                  content:reserved_control() _ comment_trailing:comment()?
                    { Row { breaks, content: Some(content), comment_trailing } }
                / comment_trailing:comment()?
                    { Row { breaks, content: None, comment_trailing } }
            ) { r }

        pub rule row() -> SRow
            = spanned(<row_()>)

        rule block_delay() -> Expr
            = "delay" _ "=" _ e:expr() { e }


        rule row_line() -> SRow
            = row:row() _ newline() { row }

        pub rule block_() -> Block
            = _ default_receiver_component:("@" r:receiver(){r})? _ delay:block_delay()?
            _ "{" _ comment_first:comment()? newline()
              rows:(row_line()*)
            _ "}" _ comment_last:comment()?
            { Block { default_receiver_component, delay, rows, comment_first, comment_last } }

        pub rule block() -> SBlock
            = spanned(<block_()>)

        pub rule statement() -> Statement
            = block:block() { Statement::Block(block) }
            / row:row() { Statement::Single(row) }

        pub rule statements() -> Vec<Statement>
            = s:statement() ** (_ newline()) { s }

        rule ws() = quiet!{[c if c.is_whitespace() && c != '\n']}
        rule _() = ws()*
        rule __() = ![c if c.is_alphanumeric()] _
        rule newline() = "\n"


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
        let r = ops_parser::expr(r#"time!(2021-01-01T00:00:00.00Z)"#);
        dbg!(r.unwrap());
        let r = ops_parser::expr(r#"time!(2021-01-01T00:00:00.00+09:00)"#);
        dbg!(r.unwrap());
        let r = ops_parser::expr(r#"tlmid!(AB.CD.EF)"#);
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
            ops_parser::assert,
            "assert x in [ 100, 200 ]",
            "assert x in[ 100,200]",
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
    fn test_wait() {
        let r = ops_parser::wait("wait 12s");
        dbg!(r.unwrap());
        let r = ops_parser::wait("wait 0.1s");
        dbg!(r.unwrap());
        let r = ops_parser::wait("wait 1 == 1");
        dbg!(r.unwrap());
        let r = ops_parser::wait("wait HEX.VALUE == 0x0123cdef || 5s");
        dbg!(r.unwrap());
        let r = ops_parser::wait("wait SOME.CONDITION.TO.BE.TESTED == FALSE");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_assert() {
        let r = ops_parser::assert("assert TEST.VAR1 < 0.05");
        dbg!(r.unwrap());
        let r = ops_parser::assert("assert TEST.VAR2.X  in [ 0.01, 0.04 ]");
        dbg!(r.unwrap());
        let r = ops_parser::assert(r#"assert TEST.VAR_3 == "OFF""#);
        dbg!(r.unwrap());
        let r = ops_parser::assert(r#"assert TEST.VAR_4.X in [ 0, 0.07 ] if TEST.Var5 == "OFF""#);
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
    fn test_print() {
        let r = ops_parser::print("print abc_xyz");
        dbg!(r.unwrap());
        let r = ops_parser::print("print $A.B");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_set() {
        let r = ops_parser::set("set A.B = 1");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_command() {
        let r = ops_parser::command("DO_IT");
        dbg!(r.unwrap());
        let r = ops_parser::command("@ABC.DEF DO_IT");
        dbg!(r.unwrap());
        let r = ops_parser::command("@ABC.DEF CMD_WITH_ARGS some_value 0xaa 0xbb 2");
        dbg!(r.unwrap());
        let r = ops_parser::command("@ABC.DEF @@G CMD_WITH_ARGS relative_x 0xcc 0xdd 0xee 0xff");
        dbg!(r.unwrap());
    }
    #[test]
    fn test_block() {
        let r = ops_parser::block(
            r#"{ #comment
  let x = 2
  . call OTHER_FILE.ops
  . #hello
}"#,
        );
        dbg!(r.unwrap());

        let r = ops_parser::block(
            r#"delay=1s {
  let x = 2
  @AB.CD @@GH DO_IT
} #comment"#,
        );
        dbg!(r.unwrap());
        let r = ops_parser::block(
            r#"  @AB.CD delay=1s {
    0: DO_IT
    0 + 1: @@EF DO_IT 1 2 3
    2: DO_IT x y z
    @XY.ZW FOO_BAR tlmid!(AB.CD)
    wait 1 == 1
}"#,
        );
        dbg!(r.unwrap());
    }

    #[test]
    fn test_file() {
        let s = include_str!("../tests/test.ops");
        let r = ops_parser::statements(s);
        dbg!(r.unwrap());
    }
    #[test]
    fn test_rows() {
        let s = r#".# ****** #
    .set X.Y=2
    .let CURRENT_EXAMPLE_TLM_VALUE = FOO.BAR.EXAMPLE_TLM.VALUE
    .@@DEF DO_IT
     wait $FOO.BAR.EXAMPLE_TLM.VALUE > CURRENT_EXAMPLE_TLM_VALUE || 5s
     let foobar = $FOO.BAR
     wait foobar.EXAMPLE_TLM.VALUE > CURRENT_EXAMPLE_TLM_VALUE || 5s
    "#;
        for l in s.lines() {
            let r = ops_parser::row(l);
            dbg!(r.unwrap());
        }
    }
}
