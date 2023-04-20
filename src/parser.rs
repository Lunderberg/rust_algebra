use crate::{expr::Expr, DecimalLiteral, Error, OperatorPrecedence, Token, Tokenizer};
use std::iter::Peekable;
use typed_dag::{Arena, BuilderRef, HasDefaultContainer};

type Container = <crate::expr::Expr as HasDefaultContainer>::Container;

pub struct Parser<'arena, I: Iterator<Item = Result<Token, Error>>> {
    tokens: Peekable<I>,
    arena: &'arena mut Arena<Container>,
}

impl<'arena, 'str> Parser<'arena, Tokenizer<std::str::Chars<'str>>> {
    pub fn parse(
        arena: &'arena mut Arena<Container>,
        text: &'str str,
    ) -> Result<BuilderRef<Expr>, Error> {
        let tokens = Tokenizer::new(text.chars());
        let mut parser = Self::new(tokens, arena);
        parser.expect_expr()
    }
}

impl<'arena, I: Iterator<Item = Result<Token, Error>>> Parser<'arena, I> {
    fn new(iter: I, arena: &'arena mut Arena<Container>) -> Self {
        Self {
            tokens: iter.peekable(),
            arena,
        }
    }

    fn expect_expr(&mut self) -> Result<BuilderRef<Expr>, Error> {
        let out = self.expect_expr_precedence(OperatorPrecedence::Expr)?;
        self.expect_end()?;
        Ok(out)
    }

    fn expect_token(&mut self, expected: Token) -> Result<(), Error> {
        let token: Token = self.tokens.next().ok_or(Error::UnexpectedEndOfExpr)??;
        (token == expected)
            .then_some(())
            .ok_or(Error::UnexpectedToken(expected, token))
    }

    fn expect_end(&mut self) -> Result<(), Error> {
        match self.tokens.next() {
            None => Ok(()),
            Some(Err(err)) => Err(err),
            Some(Ok(token)) => Err(Error::ExpectedEndOfFile(token)),
        }
    }

    fn expect_expr_precedence(
        &mut self,
        parent_precedence: OperatorPrecedence,
    ) -> Result<BuilderRef<Expr>, Error> {
        let mut expr: BuilderRef<Expr> = self
            .tokens
            .next()
            .ok_or(Error::UnexpectedEndOfExpr)?
            .and_then(|token: Token| -> Result<BuilderRef<Expr>, Error> {
                match token {
                    Token::IntLiteral(val) => Ok(self.arena.push(Expr::Int(val))),
                    Token::BoolLiteral(val) => Ok(self.arena.push(Expr::Bool(val))),
                    Token::DecimalLiteral(val) => {
                        let expr = self.convert_decimal(val);
                        Ok(self.arena.push(expr))
                    }

                    Token::Plus => self.expect_expr_precedence(OperatorPrecedence::UnaryNeg),

                    Token::Minus => self
                        .tokens
                        .next_if(|peek| {
                            matches!(
                                peek,
                                Ok(Token::IntLiteral(_)) | Ok(Token::DecimalLiteral(_)),
                            )
                        })
                        .map(|next| match next.unwrap() {
                            Token::IntLiteral(val) => Expr::Int(-val),
                            Token::DecimalLiteral(val) => self.convert_decimal(val.negative()),
                            _ => panic!(),
                        })
                        .map(|expr| Ok(self.arena.push(expr)))
                        .unwrap_or_else(|| {
                            self.expect_expr_precedence(OperatorPrecedence::UnaryNeg)
                                .map(|expr| self.arena.push(Expr::UnaryNeg(expr)))
                        }),

                    Token::LeftParen => {
                        let expr = self.expect_expr_precedence(OperatorPrecedence::Expr)?;
                        self.expect_token(Token::RightParen)?;
                        Ok(expr)
                    }
                    Token::Id(id) => Err(Error::NotImplemented(format!(
                        "Variables not yet implemented, but found '{id}'"
                    ))),
                    Token::Multiply | Token::Divide | Token::RightParen => {
                        Err(Error::ExpectedStartOfExpr(token))
                    }
                }
            })?;

        loop {
            let peek = self.tokens.next_if(|res| {
                res.as_ref()
                    .map(|token| {
                        token
                            .operator_precedence()
                            .map(|prec| prec > parent_precedence)
                            .unwrap_or(false)
                    })
                    .unwrap_or(false)
            });

            if let Some(Ok(op)) = peek {
                let op_precedence = op.operator_precedence().unwrap();
                let rhs = self.expect_expr_precedence(op_precedence)?;
                expr = self.arena.push(match op {
                    Token::Minus => Expr::Sub(expr, rhs),
                    Token::Plus => Expr::Add(expr, rhs),
                    Token::Multiply => Expr::Mul(expr, rhs),
                    Token::Divide => Expr::Div(expr, rhs),
                    _ => panic!(),
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn convert_decimal(&mut self, decimal: DecimalLiteral) -> Expr<BuilderRef> {
        let (num, denom) = decimal.as_fraction();
        if denom == 1 {
            Expr::Int(num)
        } else {
            let num = self.arena.push(Expr::Int(num));
            let denom = self.arena.push(Expr::Int(denom));
            Expr::Div(num, denom)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr::Expr::*;
    use typed_dag::Visitable;
    use typed_dag_derive::tree;

    fn parse_compare(string: &'static str, expected: Arena<Container, Expr>) -> Result<(), Error> {
        let parsed = Arena::try_build(|arena| Parser::parse(arena, string))?;
        println!("Parsed: {}", parsed.visit_root().borrow());
        println!("Expected: {}", expected.visit_root().borrow());
        assert_eq!(parsed.visit_root().borrow(), expected.visit_root().borrow());
        Ok(())
    }

    #[test]
    fn test_parse_one_digit_int() -> Result<(), Error> {
        parse_compare("5", tree! {Int(5)})
    }

    #[test]
    fn test_parse_two_digit_int() -> Result<(), Error> {
        parse_compare("42", tree! {Int(42)})
    }

    #[test]
    fn test_parse_negative_int() -> Result<(), Error> {
        parse_compare("-12", tree! {Int(-12)})
    }

    #[test]
    fn test_parse_addition() -> Result<(), Error> {
        parse_compare("5+10", tree! {Add(Int(5), Int(10))})
    }

    #[test]
    fn test_parse_addition_with_spaces() -> Result<(), Error> {
        parse_compare("5 + 10", tree! {Add(Int(5), Int(10))})
    }

    #[test]
    fn test_parse_multiple_addition() -> Result<(), Error> {
        parse_compare("5 + 10 + 15", tree! {Add(Add(Int(5),Int(10)), Int(15))})
    }

    #[test]
    fn test_parse_subtraction() -> Result<(), Error> {
        parse_compare("5 - 10", tree! {Sub(Int(5), Int(10))})
    }

    #[test]
    fn test_parse_mixed_addition_subtraction() -> Result<(), Error> {
        parse_compare("5 + 15 - 10", tree! {Sub(Add(Int(5), Int(15)), Int(10))})
    }

    #[test]
    fn test_parse_parentheses() -> Result<(), Error> {
        parse_compare("5 + (15 - 10)", tree! {Add(Int(5), Sub(Int(15), Int(10)))})
    }

    #[test]
    fn test_parse_multiply() -> Result<(), Error> {
        parse_compare("5 * 10", tree! {Mul(Int(5), Int(10))})
    }

    #[test]
    fn test_parse_multiply_left_precedence() -> Result<(), Error> {
        parse_compare("5*10 + 15", tree! {Add(Mul(Int(5),Int(10)), Int(15))})
    }

    #[test]
    fn test_parse_multiply_right_precedence() -> Result<(), Error> {
        parse_compare("5 + 10*15", tree! {Add(Int(5), Mul(Int(10),Int(15)))})
    }

    #[test]
    fn test_parse_divide() -> Result<(), Error> {
        parse_compare("5 / 10", tree! {Div(Int(5), Int(10))})
    }

    #[test]
    fn test_parse_mixed_multiply_divide() -> Result<(), Error> {
        parse_compare(
            "2 * 5 / 10 * 42",
            tree! {Mul(
            Div(Mul(Int(2), Int(5)),
                Int(10)),
               Int(42))},
        )
    }

    #[test]
    fn test_literal_true() -> Result<(), Error> {
        parse_compare("true", tree! {Bool(true)})
    }

    #[test]
    fn test_literal_false() -> Result<(), Error> {
        parse_compare("false", tree! {Bool(false)})
    }

    #[test]
    fn test_positive_float() -> Result<(), Error> {
        parse_compare("1.5", tree! {Div(Int(3), Int(2))})
    }

    #[test]
    fn test_negative_float() -> Result<(), Error> {
        parse_compare("-1.5", tree! {Div(Int(-3), Int(2))})
    }

    #[test]
    fn test_positive_float_without_ipart() -> Result<(), Error> {
        parse_compare(".5", tree! {Div(Int(1), Int(2))})
    }

    #[test]
    fn test_negative_float_without_ipart() -> Result<(), Error> {
        parse_compare("-.5", tree! {Div(Int(-1), Int(2))})
    }

    #[test]
    fn test_positive_float_without_fpart() -> Result<(), Error> {
        parse_compare("1.", tree! {Int(1)})
    }

    #[test]
    fn test_negative_float_without_fpart() -> Result<(), Error> {
        parse_compare("-1.", tree! {Int(-1)})
    }

    #[test]
    fn test_float_with_leading_zero_in_fpart() -> Result<(), Error> {
        parse_compare("1.0625", tree! {Div(Int(17), Int(16))})
    }

    #[test]
    fn test_standalone_dot_is_error() -> Result<(), Error> {
        assert!(Arena::try_build(|arena| Parser::parse(arena, ".")).is_err());
        Ok(())
    }
}
