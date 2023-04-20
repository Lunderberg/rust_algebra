use crate::{expr::Expr, DecimalLiteral, Error, OperatorPrecedence, Token, Tokenizer};
use std::iter::Peekable;
use typed_dag::{Arena, BuilderRef, HasDefaultContainer};

type Container = <crate::expr::Expr as HasDefaultContainer>::Container;

pub fn parse_expr<I: IntoIterator<Item = char>>(
    chars: I,
    arena: &mut Arena<Container>,
) -> Result<BuilderRef<Expr>, Error> {
    let tokens = Tokenizer::new(chars.into_iter());
    let mut parser = Parser::new(tokens, arena);
    parser.expect_expr()
}

struct Parser<'arena, I: Iterator<Item = Result<Token, Error>>> {
    tokens: Peekable<I>,
    arena: &'arena mut Arena<Container>,
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
