use super::*;
use chrono::Duration;
use opslang_ir::version::v1::NumericKind;
use opslang_ty::version::v1::{FloatVid, IntVid};

impl<'cx> TypeChecker<'cx> {
    pub(super) fn typeck_literal<'env>(
        &mut self,
        typing_env: &Environment<'cx, 'env>,
        subst: &mut Substitution<'cx>,
        literal: &'cx ast::Literal<'cx>,
    ) -> Result<(ir::Literal<'cx>, Ty<'cx>)> {
        match literal {
            ast::Literal::String(s) => {
                let ir_string = ir::String {
                    value: self.ir_cx.alloc_str(&s.unescape()?),
                    ast: s,
                };
                let ir_literal = ir::Literal::String(ir_string);
                let string_type = Ty::mk_string(self.tcx);
                Ok((ir_literal, string_type))
            }
            ast::Literal::Numeric(numeric) => {
                let mut kind = NumericKind::Repr(numeric.raw);
                // Check if there's a suffix and resolve it
                let ty = if let Some(suffix) = &numeric.suffix {
                    self.resolve_numeric_suffix(suffix, &mut kind, numeric.raw)?
                } else {
                    match &numeric.kind {
                        ast::literal::NumericKind::Integer(_prefix) => {
                            Ty::mk_int_var(self.tcx, IntVid::fresh())
                        }
                        ast::literal::NumericKind::Float => {
                            Ty::mk_float_var(self.tcx, FloatVid::fresh())
                        }
                    }
                };

                let ir_numeric = ir::Numeric { kind, ast: numeric };
                let ir_literal = ir::Literal::Numeric(ir_numeric);
                Ok((ir_literal, ty))
            }
            ast::Literal::Array(array) => {
                if array.exprs.is_empty() {
                    // Empty array - use a type variable for the element type
                    let element_type = Ty::mk_variable(self.tcx, TyVid::fresh());
                    let array_type = Ty::mk_array(self.tcx, element_type);

                    let ir_array = ir::Array {
                        left_bracket: array.left_bracket.into_token(),
                        exprs: Vec::new(),
                        right_bracket: array.right_bracket.into_token(),
                    };
                    let ir_literal = ir::Literal::Array(ir_array);
                    Ok((ir_literal, array_type))
                } else {
                    // Non-empty array - type check all elements
                    let mut ir_exprs = Vec::new();

                    // Type check first element to establish the element type
                    let first_ir = self.typeck_expr(typing_env, subst, &array.exprs[0])?;

                    let ty = first_ir.ty;
                    ir_exprs.push(first_ir);
                    let element_type = ty;

                    // Type check remaining elements and unify with element type
                    for expr in &array.exprs[1..] {
                        let expr_ir = self.typeck_expr(typing_env, subst, expr)?;

                        self.unify(subst, element_type, expr_ir.ty)?;

                        ir_exprs.push(expr_ir);
                    }

                    let array_type = Ty::mk_array(self.tcx, element_type);
                    let ir_array = ast::Literal::array(
                        array.left_bracket.into_token(),
                        ir_exprs,
                        array.right_bracket.into_token(),
                    );
                    Ok((ir_array, array_type))
                }
            }
            ast::Literal::Bytes(bytes) => {
                let byte_data = self.ir_cx.alloc_bytes(bytes.as_bytes());
                let ir_bytes = ir::Bytes {
                    value: byte_data,
                    ast: bytes,
                };
                let ir_literal = ast::Literal::Bytes(ir_bytes);
                let bytes_type = Ty::mk_bytes(self.tcx);
                Ok((ir_literal, bytes_type))
            }
            ast::Literal::HexBytes(hex_bytes) => {
                let byte_data = self.ir_cx.alloc_bytes(
                    &hex_bytes
                        .as_bytes()
                        .map_err(|c| anyhow!("illegal hex charactor: {c}"))?,
                );
                let ir_hex_bytes = ir::HexBytes {
                    value: byte_data,
                    ast: hex_bytes,
                };
                let ir_literal = ast::Literal::HexBytes(ir_hex_bytes);
                let hex_bytes_type = Ty::mk_bytes(self.tcx);
                Ok((ir_literal, hex_bytes_type))
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
                    ast: dt,
                };
                let ir_literal = ast::Literal::DateTime(ir_datetime);
                let time_type = Ty::mk_time(self.tcx);
                Ok((ir_literal, time_type))
            }
        }
    }

    /// Resolves a numeric suffix to the corresponding type and numeric kind.
    ///
    /// This function takes a numeric suffix (like "i32", "u64", "f32") and returns
    /// the corresponding type and numeric representation for IR generation.
    fn resolve_numeric_suffix(
        &mut self,
        suffix: &ast::literal::NumericSuffix<'cx>,
        kind: &mut NumericKind<'cx>,
        repr: &'cx str,
    ) -> Result<Ty<'cx>> {
        let suffix = suffix.0.raw;
        Ok(match suffix {
            "i8" => Ty::mk_i8(self.tcx),
            "i16" => Ty::mk_i16(self.tcx),
            "i32" => Ty::mk_i32(self.tcx),
            "i64" => Ty::mk_i64(self.tcx),

            "u8" => Ty::mk_u8(self.tcx),
            "u16" => Ty::mk_u16(self.tcx),
            "u32" => Ty::mk_u32(self.tcx),
            "u64" => Ty::mk_u64(self.tcx),

            "f32" => Ty::mk_f32(self.tcx),
            "f64" => Ty::mk_f64(self.tcx),

            "f" => Ty::mk_float_var(self.tcx, FloatVid::fresh()),

            "s" => {
                *kind = NumericKind::Duration(Duration::seconds(repr.parse()?));
                Ty::mk_duration(self.tcx)
            }
            "ms" => {
                *kind = NumericKind::Duration(Duration::milliseconds(repr.parse()?));
                Ty::mk_duration(self.tcx)
            }
            "us" => {
                *kind = NumericKind::Duration(Duration::microseconds(repr.parse()?));
                Ty::mk_duration(self.tcx)
            }
            "ns" => {
                *kind = NumericKind::Duration(Duration::nanoseconds(repr.parse()?));
                Ty::mk_duration(self.tcx)
            }

            suffix => Err(anyhow!("unknown suffix: {suffix}"))?,
        })
    }
}

pub(super) fn parse_literal<'cx>(unparsed: &'cx str, ty: Ty<'cx>) -> Result<ir::NumericKind<'cx>> {
    use opslang_ty::version::{FloatTy, IntTy, UintTy};
    match ty.kind() {
        TyKind::Int(int_ty) => {
            let int = match int_ty {
                IntTy::I8 => unparsed.parse::<i8>()? as i64,
                IntTy::I16 => unparsed.parse::<i16>()? as i64,
                IntTy::I32 => unparsed.parse::<i32>()? as i64,
                IntTy::I64 => unparsed.parse::<i64>()?,
            };
            Ok(ir::NumericKind::Int(int))
        }
        TyKind::Uint(uint_ty) => {
            let uint = match uint_ty {
                UintTy::U8 => unparsed.parse::<u8>()? as u64,
                UintTy::U16 => unparsed.parse::<u16>()? as u64,
                UintTy::U32 => unparsed.parse::<u32>()? as u64,
                UintTy::U64 => unparsed.parse::<u64>()?,
            };
            Ok(ir::NumericKind::Uint(uint))
        }
        TyKind::Float(float_ty) => {
            let float = match float_ty {
                FloatTy::F32 => unparsed.parse::<f32>()?.to_bits() as u64,
                FloatTy::F64 => unparsed.parse::<f64>()?.to_bits(),
            };
            Ok(ir::NumericKind::Float(float))
        }
        ty => Err(anyhow!("unexpected literal type `{ty}` to be parsed",)),
    }
}
