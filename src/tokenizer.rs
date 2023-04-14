use std::iter::Peekable;

use crate::{Error, OperatorPrecedence};

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

pub(crate) struct Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    iter: Peekable<I>,
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

impl Token {
    pub(crate) fn operator_precedence(&self) -> Option<OperatorPrecedence> {
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
    pub(crate) fn new(iter: I) -> Self {
        Self {
            iter: iter.peekable(),
        }
    }
}
