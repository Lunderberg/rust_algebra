use crate::{expr::*, DecimalLiteral, Error, OperatorPrecedence, Token, Tokenizer};
use typed_dag::{Arena, BuilderRef, HasDefaultContainer, RecursiveObj, StorageRef, Visitable};

use std::convert::From;
use std::iter::Peekable;

type Container = <crate::expr::Expr as HasDefaultContainer>::Container;

pub fn parse_expr<I: IntoIterator<Item = char>>(
    chars: I,
    arena: &mut Arena<Container>,
) -> Result<BuilderRef<Expr>, Error> {
    let tokens = Tokenizer::new(chars.into_iter());
    let mut parser = Parser::new(tokens, arena);
    let top_expr = parser.expect_expr()?;
    let top_ref = match top_expr {
        ParseExpr::Int(expr) => arena.push(Expr::Int(expr)),
        ParseExpr::Bool(expr) => arena.push(Expr::Bool(expr)),
        ParseExpr::Rational(expr) => arena.push(Expr::Rational(expr)),
    };
    Ok(top_ref)
}

struct Parser<'arena, I: Iterator<Item = Result<Token, Error>>> {
    tokens: Peekable<I>,
    arena: &'arena mut Arena<Container>,
}

enum ParseExpr {
    Int(BuilderRef<Int>),
    Bool(BuilderRef<Bool>),
    Rational(BuilderRef<Rational>),
}
impl From<BuilderRef<Int>> for ParseExpr {
    fn from(value: BuilderRef<Int>) -> Self {
        ParseExpr::Int(value)
    }
}
impl From<BuilderRef<Bool>> for ParseExpr {
    fn from(value: BuilderRef<Bool>) -> Self {
        ParseExpr::Bool(value)
    }
}
impl From<BuilderRef<Rational>> for ParseExpr {
    fn from(value: BuilderRef<Rational>) -> Self {
        ParseExpr::Rational(value)
    }
}

impl<'arena, I: Iterator<Item = Result<Token, Error>>> Parser<'arena, I> {
    fn new(iter: I, arena: &'arena mut Arena<Container>) -> Self {
        Self {
            tokens: iter.peekable(),
            arena,
        }
    }

    fn expect_expr(&mut self) -> Result<ParseExpr, Error> {
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

    fn push<Obj: RecursiveObj<'static, Ref = BuilderRef>>(&mut self, obj: Obj) -> ParseExpr
    where
        <Obj::Family as typed_dag::RecursiveFamily<'static>>::Sibling<StorageRef>: Into<Container>,
        BuilderRef<Obj::Family>: Into<ParseExpr>,
    {
        self.arena.push(obj).into()
    }

    fn expect_expr_precedence(
        &mut self,
        parent_precedence: OperatorPrecedence,
    ) -> Result<ParseExpr, Error> {
        let mut expr: ParseExpr = self
            .tokens
            .next()
            .ok_or(Error::UnexpectedEndOfExpr)?
            .and_then(|token: Token| -> Result<ParseExpr, Error> {
                match token {
                    Token::IntLiteral(val) => Ok(self.arena.push(Int::Literal(val)).into()),
                    Token::BoolLiteral(val) => Ok(self.arena.push(Bool::Literal(val)).into()),
                    Token::DecimalLiteral(val) => Ok(self.convert_decimal(val)),

                    Token::Plus => self.expect_expr_precedence(OperatorPrecedence::UnaryNeg),

                    Token::Minus => self
                        .tokens
                        .next_if(|peek| {
                            matches!(
                                peek,
                                Ok(Token::IntLiteral(_)) | Ok(Token::DecimalLiteral(_)),
                            )
                        })
                        .map(|next| -> ParseExpr {
                            match next.unwrap() {
                                Token::IntLiteral(val) => {
                                    self.arena.push(Int::Literal(-val)).into()
                                }
                                Token::DecimalLiteral(val) => self.convert_decimal(val.negative()),
                                _ => panic!(),
                            }
                        })
                        .map(|expr| Ok(expr))
                        .unwrap_or_else(|| {
                            self.expect_expr_precedence(OperatorPrecedence::UnaryNeg)
                                .and_then(|expr| -> Result<ParseExpr, Error> {
                                    match expr {
                                        ParseExpr::Int(expr) => {
                                            Ok(self.arena.push(Int::Negative(expr)).into())
                                        }
                                        ParseExpr::Bool(expr) => {
                                            Err(Error::InvalidOperation(format!(
                                                "Cannot negate boolean expression '{}'",
                                                self.arena.visit_by_ref(expr).borrow()
                                            )))
                                        }
                                        ParseExpr::Rational(expr) => {
                                            Ok(self.arena.push(Rational::Negative(expr)).into())
                                        }
                                    }
                                })
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

                let (lhs, rhs) = match (expr, rhs) {
                    (ParseExpr::Int(lhs), ParseExpr::Rational(rhs)) => {
                        let one = self.arena.push(Int::Literal(1));
                        let lhs = self.arena.push(Rational::Ratio(lhs, one));
                        (lhs.into(), rhs.into())
                    }
                    (ParseExpr::Rational(lhs), ParseExpr::Int(rhs)) => {
                        let one = self.arena.push(Int::Literal(1));
                        let rhs = self.arena.push(Rational::Ratio(rhs, one));
                        (lhs.into(), rhs.into())
                    }
                    (lhs, rhs) => (lhs, rhs),
                };

                expr = match (lhs, &op, rhs) {
                    (ParseExpr::Int(lhs), Token::Plus, ParseExpr::Int(rhs)) => {
                        Ok(self.push(Int::Add(lhs, rhs)))
                    }
                    (ParseExpr::Int(lhs), Token::Minus, ParseExpr::Int(rhs)) => {
                        Ok(self.push(Int::Sub(lhs, rhs)))
                    }
                    (ParseExpr::Int(lhs), Token::Multiply, ParseExpr::Int(rhs)) => {
                        Ok(self.push(Int::Mul(lhs, rhs)))
                    }
                    (ParseExpr::Int(lhs), Token::Divide, ParseExpr::Int(rhs)) => {
                        Ok(self.push(Rational::Ratio(lhs, rhs)))
                    }
                    (ParseExpr::Rational(lhs), Token::Plus, ParseExpr::Rational(rhs)) => {
                        Ok(self.push(Rational::Add(lhs, rhs)))
                    }
                    (ParseExpr::Rational(lhs), Token::Minus, ParseExpr::Rational(rhs)) => {
                        Ok(self.push(Rational::Sub(lhs, rhs)))
                    }
                    (ParseExpr::Rational(lhs), Token::Multiply, ParseExpr::Rational(rhs)) => {
                        Ok(self.push(Rational::Mul(lhs, rhs)))
                    }
                    (ParseExpr::Rational(lhs), Token::Divide, ParseExpr::Rational(rhs)) => {
                        Ok(self.push(Rational::Div(lhs, rhs)))
                    }
                    (
                        ParseExpr::Bool(b),
                        Token::Minus | Token::Plus | Token::Multiply | Token::Divide,
                        _,
                    ) => Err(Error::InvalidOperation(format!(
                        "Cannot use operator {} with boolean LHS {}",
                        op,
                        self.arena.visit_by_ref(b).borrow()
                    ))),
                    (
                        _,
                        Token::Minus | Token::Plus | Token::Multiply | Token::Divide,
                        ParseExpr::Bool(b),
                    ) => Err(Error::InvalidOperation(format!(
                        "Cannot use operator {} with boolean LHS {}",
                        op,
                        self.arena.visit_by_ref(b).borrow()
                    ))),

                    _ => panic!(),
                }?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn convert_decimal(&mut self, decimal: DecimalLiteral) -> ParseExpr {
        let (num, denom) = decimal.as_fraction();
        if denom == 1 {
            self.arena.push(Int::Literal(num)).into()
        } else {
            let num = self.arena.push(Int::Literal(num));
            let denom = self.arena.push(Int::Literal(denom));
            self.arena.push(Rational::Ratio(num, denom)).into()
        }
    }
}
