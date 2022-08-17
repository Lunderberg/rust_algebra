use std::iter::Peekable;
use std::str::FromStr;

use super::{Element, Error, Expr, Result};

#[derive(Debug, Clone, Copy)]
pub enum Token {
    IntLiteral(i64),
    Minus,
    // Plus,
}

struct Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    iter: Peekable<I>,
}

struct Parser<I>
where
    I: Iterator<Item = Result<Token>>,
{
    tokens: Peekable<I>,
    items: Vec<Element>,
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
    I: Iterator<Item = Result<Token>>,
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

    fn expect_expr(&mut self) -> Result<()> {
        self.tokens
            .next()
            .ok_or(Error::UnexpectedEndOfExpr)?
            .and_then(|token: Token| {
                let item = match token {
                    Token::IntLiteral(val) => Element::Int(val),
                    Token::Minus => self.try_int().map_or_else(
                        || panic!("Unary NOT not implemented"),
                        |val| Element::Int(-val),
                    ),
                };
                self.items.push(item);
                Ok(())
            })
    }

    fn expect_end(&mut self) -> Result<()> {
        self.tokens.peek().as_ref().map_or_else(
            || Ok(()),
            |res| Err(res.map_or_else(|err| err, |token| Error::UnexpectedToken(token))),
        )
    }
}

impl FromStr for Expr {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
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
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Result<Token>> {
        self.iter.next().map(|c| match c {
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

            // '+' => Ok(Token::Plus),
            _ => Err(Error::UnexpectedCharacter(c)),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_one_digit_int() -> Result<()> {
        let parsed: Expr = "5".parse()?;
        let expected = Expr {
            items: vec![Element::Int(5)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_two_digit_int() -> Result<()> {
        let parsed: Expr = "42".parse()?;
        let expected = Expr {
            items: vec![Element::Int(42)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_negative_int() -> Result<()> {
        let parsed: Expr = "-12".parse()?;
        let expected = Expr {
            items: vec![Element::Int(-12)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_addition() -> Result<()> {
        let parsed: Expr = "5 + 10".parse()?;
        let expected = Expr {
            items: vec![Element::Int(5), Element::Int(10), Element::Add(2, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_multiple_addition() -> Result<()> {
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
    fn test_parse_subtraction() -> Result<()> {
        let parsed: Expr = "5 - 10".parse()?;
        let expected = Expr {
            items: vec![Element::Int(5), Element::Int(10), Element::Sub(2, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_mixed_addition_subtraction() -> Result<()> {
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
    fn test_parse_parentheses() -> Result<()> {
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
}
