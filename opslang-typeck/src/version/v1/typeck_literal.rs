use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn typeck_literal(
        &mut self,
        typing_env: &Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        literal: &'cx ast::Literal<'cx>,
    ) -> Result<ir::Expr<'cx>> {
        match literal {
            ast::Literal::String(s) => {
                let ir_string = ir::String {
                    value: self.ir_cx.alloc_str(&s.unescape()?),
                    syn: s,
                };
                let ir_literal = ast::Literal::String(ir_string);
                let string_type = Ty::mk_string(self.typing_cx);
                let ir_expr =
                    ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), string_type);
                Ok(ir_expr)
            }
            ast::Literal::Numeric(numeric) => {
                let numeric_kind = match &numeric.kind {
                    ast::literal::NumericKind::Integer(prefix) => {
                        // rawフィールドから実際の値を計算
                        let value = match numeric.raw.parse::<i64>() {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(anyhow!(
                                    "failed to parse integer literal: {}",
                                    numeric.raw
                                ));
                            }
                        };
                        NumericKind::Int(*prefix, value)
                    }
                    ast::literal::NumericKind::Float => {
                        // フロートリテラルの値を計算
                        let value = match numeric.raw.parse::<f64>() {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(anyhow!(
                                    "failed to parse float literal: {}",
                                    numeric.raw
                                ));
                            }
                        };
                        NumericKind::Float(value)
                    }
                };
                let ir_numeric = ir::Numeric {
                    kind: numeric_kind,
                    syn: numeric,
                };
                let ir_literal = ast::Literal::Numeric(ir_numeric);
                let numeric_type = match numeric.kind {
                    ast::literal::NumericKind::Integer(_) => Ty::mk_int(self.typing_cx),
                    ast::literal::NumericKind::Float => Ty::mk_float(self.typing_cx),
                };
                let ir_expr =
                    ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), numeric_type);
                Ok(ir_expr)
            }
            ast::Literal::Array(array) => {
                if array.exprs.is_empty() {
                    // Empty array - use a type variable for the element type
                    let element_type = Ty::mk_variable(self.typing_cx, TypeVariable::fresh());
                    let array_type = Ty::mk_array(self.typing_cx, element_type);

                    let ir_array = ast::literal::Array {
                        left_bracket: array.left_bracket.into_token(),
                        exprs: &[],
                        right_bracket: array.right_bracket.into_token(),
                    };
                    let ir_literal = ast::Literal::Array(ir_array);
                    let ir_expr =
                        ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), array_type);
                    Ok(ir_expr)
                } else {
                    // Non-empty array - type check all elements
                    let mut ir_exprs = Vec::new();

                    // Type check first element to establish the element type
                    let first_ir = self.typeck_expr(typing_env, subst, &array.exprs[0])?;

                    let ty = first_ir.ty;
                    ir_exprs.push(first_ir);
                    let mut element_type = ty;

                    // Type check remaining elements and unify with element type
                    for expr in &array.exprs[1..] {
                        let expr_ir = self.typeck_expr(typing_env, subst, expr)?;

                        self.unify(subst, element_type, expr_ir.ty)?;

                        let ty = expr_ir.ty;
                        ir_exprs.push(expr_ir);
                        element_type = ty;
                    }

                    let array_type = Ty::mk_array(self.typing_cx, element_type);
                    let ir_array = ast::Literal::array(
                        array.left_bracket.into_token(),
                        self.ir_cx.alloc_expr_slice(ir_exprs),
                        array.right_bracket.into_token(),
                    );
                    let ir_expr =
                        ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_array), array_type);
                    Ok(ir_expr)
                }
            }
            ast::Literal::Bytes(bytes) => {
                let byte_data = self.ir_cx.alloc_bytes(bytes.as_bytes());
                let ir_bytes = ir::Bytes {
                    value: byte_data,
                    syn: bytes,
                };
                let ir_literal = ast::Literal::Bytes(ir_bytes);
                let bytes_type = Ty::mk_string(self.typing_cx);
                let ir_expr =
                    ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), bytes_type);
                Ok(ir_expr)
            }
            ast::Literal::HexBytes(hex_bytes) => {
                let byte_data = self.ir_cx.alloc_bytes(
                    &hex_bytes
                        .as_bytes()
                        .map_err(|c| anyhow!("illegal hex charactor: {c}"))?,
                );
                let ir_hex_bytes = ir::HexBytes {
                    value: byte_data,
                    syn: hex_bytes,
                };
                let ir_literal = ast::Literal::HexBytes(ir_hex_bytes);
                let hex_bytes_type = Ty::mk_string(self.typing_cx);
                let ir_expr = ir::Expr::new(
                    ast::ExprMut::literal(self.ir_cx, ir_literal),
                    hex_bytes_type,
                );
                Ok(ir_expr)
            }
            ast::Literal::DateTime(dt) => {
                // Parse the raw datetime string to chrono::DateTime<Utc>
                let parsed_datetime = match dt.raw.parse::<chrono::DateTime<Utc>>() {
                    Ok(datetime) => datetime,
                    Err(_) => {
                        return Err(anyhow!("failed to parse datetime literal: {}", dt.raw));
                    }
                };

                let ir_datetime = ir::DateTime {
                    value: parsed_datetime,
                    syn: dt,
                };
                let ir_literal = ast::Literal::DateTime(ir_datetime);
                let time_type = Ty::mk_time(self.typing_cx);
                let ir_expr =
                    ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), time_type);
                Ok(ir_expr)
            }
        }
    }
}
