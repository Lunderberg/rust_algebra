use crate::{expr::Expr, Error, OperatorPrecedence, Token, Tokenizer};
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
        self.expect_expr_precedence(OperatorPrecedence::Expr)
    }

    fn expect_token(&mut self, expected: Token) -> Result<(), Error> {
        let token: Token = self.tokens.next().ok_or(Error::UnexpectedEndOfExpr)??;
        (token == expected)
            .then_some(())
            .ok_or(Error::UnexpectedToken(token))
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

                    Token::Minus | Token::Plus => self
                        .tokens
                        .next_if(|peek| matches!(peek, Ok(Token::IntLiteral(_))))
                        .map(|peek| match peek {
                            Ok(Token::IntLiteral(val)) => val,
                            _ => panic!(),
                        })
                        .map(|val| match token {
                            Token::Minus => Ok(self.arena.push(Expr::Int(-val))),
                            Token::Plus => Ok(self.arena.push(Expr::Int(val))),
                            _ => panic!(),
                        })
                        .unwrap_or_else(|| {
                            self.expect_expr_precedence(OperatorPrecedence::UnaryNeg)
                                .map(|expr| match token {
                                    Token::Minus => self.arena.push(Expr::UnaryNeg(expr)),
                                    Token::Plus => expr,
                                    _ => panic!(),
                                })
                        }),
                    Token::LeftParen => {
                        let expr = self.expect_expr()?;
                        self.expect_token(Token::RightParen)?;
                        Ok(expr)
                    }
                    Token::Id(id) => Err(Error::NotImplemented(format!(
                        "Variables not yet implemented, but found '{id}'"
                    ))),
                    Token::Multiply | Token::Divide | Token::RightParen => {
                        Err(Error::UnexpectedToken(token))
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
}

#[cfg(test)]
mod test {
    use super::*;
    use typed_dag::Visitable;

    fn parse_compare<Func>(string: &'static str, expected: Func) -> Result<(), Error>
    where
        Func: FnOnce(&mut Arena<Container>) -> BuilderRef<Expr>,
    {
        let parsed = Arena::build(|arena| Parser::parse(arena, string).unwrap());
        let expected = Arena::build(expected);
        // println!("Parsed: {}", parsed.visit_root().borrow());
        // println!("Expected: {}", expected.visit_root().borrow());
        assert_eq!(parsed.visit_root().borrow(), expected.visit_root().borrow());
        Ok(())
    }

    #[test]
    fn test_parse_one_digit_int() -> Result<(), Error> {
        parse_compare("5", |arena| arena.push(Expr::Int(5)))
    }

    #[test]
    fn test_parse_two_digit_int() -> Result<(), Error> {
        parse_compare("42", |arena| arena.push(Expr::Int(42)))
    }

    #[test]
    fn test_parse_negative_int() -> Result<(), Error> {
        parse_compare("-12", |arena| arena.push(Expr::Int(-12)))
    }

    #[test]
    fn test_parse_addition() -> Result<(), Error> {
        parse_compare("5+10", |arena| {
            let lhs = arena.push(Expr::Int(5));
            let rhs = arena.push(Expr::Int(10));
            arena.push(Expr::Add(lhs, rhs))
        })
    }

    #[test]
    fn test_parse_addition_with_spaces() -> Result<(), Error> {
        parse_compare("5 + 10", |arena| {
            let lhs = arena.push(Expr::Int(5));
            let rhs = arena.push(Expr::Int(10));
            arena.push(Expr::Add(lhs, rhs))
        })
    }

    #[test]
    fn test_parse_multiple_addition() -> Result<(), Error> {
        parse_compare("5 + 10 + 15", |arena| {
            let a = arena.push(Expr::Int(5));
            let b = arena.push(Expr::Int(10));
            let c = arena.push(Expr::Int(15));
            let d = arena.push(Expr::Add(a, b));
            arena.push(Expr::Add(d, c))
        })
    }

    #[test]
    fn test_parse_subtraction() -> Result<(), Error> {
        parse_compare("5 - 10", |arena| {
            let lhs = arena.push(Expr::Int(5));
            let rhs = arena.push(Expr::Int(10));
            arena.push(Expr::Sub(lhs, rhs))
        })
    }

    #[test]
    fn test_parse_mixed_addition_subtraction() -> Result<(), Error> {
        parse_compare("5 + 15 - 10", |arena| {
            let a = arena.push(Expr::Int(5));
            let b = arena.push(Expr::Int(15));
            let c = arena.push(Expr::Add(a, b));
            let d = arena.push(Expr::Int(10));
            arena.push(Expr::Sub(c, d))
        })
    }

    #[test]
    fn test_parse_parentheses() -> Result<(), Error> {
        parse_compare("5 + (15 - 10)", |arena| {
            let a = arena.push(Expr::Int(5));
            let b = arena.push(Expr::Int(15));
            let c = arena.push(Expr::Int(10));
            let d = arena.push(Expr::Sub(b, c));
            arena.push(Expr::Add(a, d))
        })
    }

    #[test]
    fn test_parse_multiply() -> Result<(), Error> {
        parse_compare("5 * 10", |arena| {
            let lhs = arena.push(Expr::Int(5));
            let rhs = arena.push(Expr::Int(10));
            arena.push(Expr::Mul(lhs, rhs))
        })
    }

    #[test]
    fn test_parse_multiply_left_precedence() -> Result<(), Error> {
        parse_compare("5*10 + 15", |arena| {
            let a = arena.push(Expr::Int(5));
            let b = arena.push(Expr::Int(10));
            let c = arena.push(Expr::Mul(a, b));
            let d = arena.push(Expr::Int(15));
            arena.push(Expr::Add(c, d))
        })
    }

    #[test]
    fn test_parse_multiply_right_precedence() -> Result<(), Error> {
        parse_compare("5 + 10*15", |arena| {
            let a = arena.push(Expr::Int(5));
            let b = arena.push(Expr::Int(10));
            let c = arena.push(Expr::Int(15));
            let d = arena.push(Expr::Mul(b, c));
            arena.push(Expr::Add(a, d))
        })
    }

    #[test]
    fn test_parse_divide() -> Result<(), Error> {
        parse_compare("5 / 10", |arena| {
            let lhs = arena.push(Expr::Int(5));
            let rhs = arena.push(Expr::Int(10));
            arena.push(Expr::Div(lhs, rhs))
        })
    }

    #[test]
    fn test_parse_mixed_multiply_divide() -> Result<(), Error> {
        parse_compare("2 * 5 / 10 * 42", |arena| {
            let a = arena.push(Expr::Int(2));
            let b = arena.push(Expr::Int(5));
            let c = arena.push(Expr::Mul(a, b));
            let d = arena.push(Expr::Int(10));
            let e = arena.push(Expr::Div(c, d));
            let f = arena.push(Expr::Int(42));
            arena.push(Expr::Mul(e, f))
        })
    }

    #[test]
    fn test_literal_true() -> Result<(), Error> {
        parse_compare("true", |arena| arena.push(Expr::Bool(true)))
    }

    #[test]
    fn test_literal_false() -> Result<(), Error> {
        parse_compare("false", |arena| arena.push(Expr::Bool(false)))
    }
}
