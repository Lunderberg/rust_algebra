use std::iter::Peekable;
use std::str::FromStr;

use crate::{Element, Error, Expr, OperatorPrecedence};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    IntLiteral(i64),
    Minus,
    Plus,
    Multiply,
    Divide,
    LeftParen,
    RightParen,
}

struct Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    iter: Peekable<I>,
}

struct Parser<I>
where
    I: Iterator<Item = Result<Token, Error>>,
{
    tokens: Peekable<I>,
    items: Vec<Element>,
}

impl Token {
    fn operator_precedence(&self) -> Option<OperatorPrecedence> {
        use Token::*;
        match self {
            IntLiteral(_) => None,
            Minus => Some(OperatorPrecedence::AddSub),
            Plus => Some(OperatorPrecedence::AddSub),
            Multiply => Some(OperatorPrecedence::MulDiv),
            Divide => Some(OperatorPrecedence::MulDiv),
            LeftParen => None,
            RightParen => None,
        }
    }
}

impl<I> Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    fn new(iter: I) -> Self {
        Self {
            iter: iter.peekable(),
        }
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = Result<Token, Error>>,
{
    fn new(iter: I) -> Self {
        Self {
            tokens: iter.peekable(),
            items: Vec::new(),
        }
    }

    fn try_int(&mut self) -> Option<i64> {
        self.tokens
            .next_if(|res| match res {
                Ok(Token::IntLiteral(_)) => true,
                _ => false,
            })
            .map(|res| match res {
                Ok(Token::IntLiteral(val)) => Some(val),
                _ => None,
            })
            .flatten()
    }

    fn expect_expr(&mut self) -> Result<(), Error> {
        self.expect_expr_precedence(OperatorPrecedence::Expr)
    }

    fn expect_token(&mut self, expected: Token) -> Result<(), Error> {
        self.tokens
            .next()
            .ok_or(Error::UnexpectedEndOfExpr)
            .and_then(|res| res)
            .and_then(|token| {
                if token == expected {
                    Ok(())
                } else {
                    Err(Error::UnexpectedToken(token))
                }
            })
    }

    fn expect_expr_precedence(
        &mut self,
        parent_precedence: OperatorPrecedence,
    ) -> Result<(), Error> {
        self.tokens
            .next()
            .ok_or(Error::UnexpectedEndOfExpr)?
            .and_then(|token: Token| -> Result<Option<Element>, Error> {
                match token {
                    Token::IntLiteral(val) => Ok(Some(Element::Int(val))),
                    Token::Minus => self.try_int().map_or_else(
                        || {
                            Err(Error::NotImplemented(
                                "Unary Minus not implemented for expressions".to_string(),
                            ))
                        },
                        |val| Ok(Some(Element::Int(-val))),
                    ),
                    Token::Plus => self.try_int().map_or_else(
                        || {
                            Err(Error::NotImplemented(
                                "Unary Plus not implemented for expressions".to_string(),
                            ))
                        },
                        |val| Ok(Some(Element::Int(val))),
                    ),
                    Token::LeftParen => {
                        self.expect_expr()?;
                        self.expect_token(Token::RightParen)?;
                        Ok(None)
                    }
                    Token::RightParen | Token::Multiply | Token::Divide => {
                        Err(Error::UnexpectedToken(token))
                    }
                }
            })
            .and_then(|opt_el| {
                if let Some(element) = opt_el {
                    self.items.push(element);
                }
                Ok(())
            })?;

        while let Some(op) = self
            .tokens
            .next_if(|res| match res {
                Ok(token) => token
                    .operator_precedence()
                    .map_or(false, |prec| prec > parent_precedence),
                _ => false,
            })
            .map(|res| res.unwrap())
        {
            let prec = op.operator_precedence().unwrap();
            let lhs_index = self.items.len();
            self.expect_expr_precedence(prec)?;
            let rhs_index = self.items.len();
            let out_index = self.items.len() + 1;

            let lhs_offset = out_index - lhs_index;
            let rhs_offset = out_index - rhs_index;

            let item = match op {
                Token::Plus => Element::Add(lhs_offset, rhs_offset),
                Token::Minus => Element::Sub(lhs_offset, rhs_offset),
                Token::Multiply => Element::Mul(lhs_offset, rhs_offset),
                Token::Divide => Element::Div(lhs_offset, rhs_offset),
                _ => panic!(),
            };

            self.items.push(item);
        }

        Ok(())
    }

    fn expect_end(&mut self) -> Result<(), Error> {
        self.tokens.next().map_or_else(
            || Ok(()),
            |res| Err(res.map_or_else(|err| err, |token| Error::UnexpectedToken(token))),
        )
    }
}

impl FromStr for Expr {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Error> {
        let tokens = Tokenizer::new(s.chars());
        let mut parser = Parser::new(tokens);

        parser.expect_expr()?;
        parser.expect_end()?;

        Ok(Self {
            items: parser.items,
        })
    }
}

impl<I> Iterator for Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Result<Token, Error>> {
        self.iter
            .by_ref()
            .skip_while(|c| c.is_whitespace())
            .next()
            .map(|c| match c {
                '0'..='9' => {
                    let mut val: i64 = c.to_digit(10).unwrap() as i64;
                    loop {
                        match self.iter.peek() {
                            Some('0'..='9') => {
                                let c = self.iter.next().unwrap();
                                let digit = c.to_digit(10).unwrap() as i64;
                                val = val * 10 + digit;
                            }
                            _ => {
                                break;
                            }
                        }
                    }
                    Ok(Token::IntLiteral(val))
                }

                '-' => Ok(Token::Minus),
                '+' => Ok(Token::Plus),
                '*' => Ok(Token::Multiply),
                '/' => Ok(Token::Divide),

                '(' => Ok(Token::LeftParen),
                ')' => Ok(Token::RightParen),

                _ => Err(Error::UnexpectedCharacter(c)),
            })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_one_digit_int() -> Result<(), Error> {
        let parsed: Expr = "5".parse()?;
        let expected = Expr {
            items: vec![Element::Int(5)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_two_digit_int() -> Result<(), Error> {
        let parsed: Expr = "42".parse()?;
        let expected = Expr {
            items: vec![Element::Int(42)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_negative_int() -> Result<(), Error> {
        let parsed: Expr = "-12".parse()?;
        let expected = Expr {
            items: vec![Element::Int(-12)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_addition() -> Result<(), Error> {
        let parsed: Expr = "5+10".parse()?;
        let expected = Expr {
            items: vec![Element::Int(5), Element::Int(10), Element::Add(2, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_addition_with_spaces() -> Result<(), Error> {
        let parsed: Expr = "5 + 10".parse()?;
        let expected = Expr {
            items: vec![Element::Int(5), Element::Int(10), Element::Add(2, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_multiple_addition() -> Result<(), Error> {
        let parsed: Expr = "5 + 10 + 15".parse()?;
        let expected = Expr {
            items: vec![
                Element::Int(5),
                Element::Int(10),
                Element::Add(2, 1),
                Element::Int(15),
                Element::Add(2, 1),
            ],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_subtraction() -> Result<(), Error> {
        let parsed: Expr = "5 - 10".parse()?;
        let expected = Expr {
            items: vec![Element::Int(5), Element::Int(10), Element::Sub(2, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_mixed_addition_subtraction() -> Result<(), Error> {
        let parsed: Expr = "5 + 15 - 10".parse()?;
        let expected = Expr {
            items: vec![
                Element::Int(5),
                Element::Int(15),
                Element::Add(2, 1),
                Element::Int(10),
                Element::Sub(2, 1),
            ],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_parentheses() -> Result<(), Error> {
        let parsed: Expr = "5 + (15 - 10)".parse()?;
        let expected = Expr {
            items: vec![
                Element::Int(5),
                Element::Int(15),
                Element::Int(10),
                Element::Sub(2, 1),
                Element::Add(4, 1),
            ],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_multiply() -> Result<(), Error> {
        let parsed: Expr = "5 * 10".parse()?;
        let expected = Expr {
            items: vec![Element::Int(5), Element::Int(10), Element::Mul(2, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_multiply_left_precedence() -> Result<(), Error> {
        let parsed: Expr = "5 * 10 + 15".parse()?;
        let expected = Expr {
            items: vec![
                Element::Int(5),
                Element::Int(10),
                Element::Mul(2, 1),
                Element::Int(15),
                Element::Add(2, 1),
            ],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_multiply_right_precedence() -> Result<(), Error> {
        let parsed: Expr = "5 + 10 * 15".parse()?;
        let expected = Expr {
            items: vec![
                Element::Int(5),
                Element::Int(10),
                Element::Int(15),
                Element::Mul(2, 1),
                Element::Add(4, 1),
            ],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_divide() -> Result<(), Error> {
        let parsed: Expr = "5 / 10".parse()?;
        let expected = Expr {
            items: vec![Element::Int(5), Element::Int(10), Element::Div(2, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_mixed_multiply_divide() -> Result<(), Error> {
        let parsed: Expr = "2 * 5 / 10 * 42".parse()?;
        let expected = Expr {
            items: vec![
                Element::Int(2),
                Element::Int(5),
                Element::Mul(2, 1),
                Element::Int(10),
                Element::Div(2, 1),
                Element::Int(42),
                Element::Mul(2, 1),
            ],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }
}
