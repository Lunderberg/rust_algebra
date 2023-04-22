use std::fmt::{Display, Formatter};
use std::iter::Peekable;

use crate::{Error, OperatorPrecedence};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    IntLiteral(i64),
    BoolLiteral(bool),
    DecimalLiteral(DecimalLiteral),
    Id(String),
    Minus,
    Plus,
    Multiply,
    Divide,
    Or,
    And,
    LeftParen,
    RightParen,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecimalLiteral {
    int: i64,
    num: i64,
    denom: i64,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::IntLiteral(val) => write!(f, "{val}"),
            Token::BoolLiteral(val) => write!(f, "{val}"),
            Token::DecimalLiteral(_) => todo!(),
            Token::Id(name) => write!(f, "{name}"),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Multiply => write!(f, "*"),
            Token::Divide => write!(f, "/"),
            Token::Or => write!(f, "||"),
            Token::And => write!(f, "&&"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
        }
    }
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
        self.skip_whitespace();

        None.or_else(|| self.next_number())
            .or_else(|| self.next_symbol())
            .or_else(|| self.next_id())
            .or_else(|| self.iter.next().map(|c| Err(Error::UnexpectedCharacter(c))))
    }
}

impl Token {
    pub(crate) fn operator_precedence(&self) -> Option<OperatorPrecedence> {
        use Token::*;
        match self {
            IntLiteral(_) => None,
            BoolLiteral(_) => None,
            DecimalLiteral { .. } => None,
            Id(_) => None,
            Minus => Some(OperatorPrecedence::AddSub),
            Plus => Some(OperatorPrecedence::AddSub),
            Multiply => Some(OperatorPrecedence::MulDiv),
            Divide => Some(OperatorPrecedence::MulDiv),
            And => Some(OperatorPrecedence::Boolean),
            Or => Some(OperatorPrecedence::Boolean),
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

    fn skip_whitespace(&mut self) {
        while let Some(_) = self.iter.next_if(|c| c.is_whitespace()) {}
    }

    fn next_number(&mut self) -> Option<Result<Token, Error>> {
        let whole = self.next_int();
        let fraction = self
            .iter
            .next_if_eq(&'.')
            .map(|_| self.next_int())
            .flatten();

        match (whole, fraction) {
            (Some((int, _)), None) => Some(Ok(Token::IntLiteral(int))),
            (whole, Some((num, digits))) => {
                let int = whole.map(|(int, _)| int).unwrap_or(0);
                let denom = 10_i64.pow(digits as u32);
                let decimal = DecimalLiteral { int, num, denom };
                Some(Ok(Token::DecimalLiteral(decimal)))
            }

            _ => None,
        }
    }

    fn next_int(&mut self) -> Option<(i64, usize)> {
        self.iter.next_if(char::is_ascii_digit).map(|c| {
            let mut val = c.to_digit(10).unwrap() as i64;
            let mut digits = 1;
            while let Some(c) = self.iter.next_if(char::is_ascii_digit) {
                let digit = c.to_digit(10).unwrap() as i64;
                val = 10 * val + digit;
                digits += 1;
            }
            (val, digits)
        })
    }

    fn next_symbol(&mut self) -> Option<Result<Token, Error>> {
        self.iter
            .next_if(|c| matches!(c, '-' | '+' | '*' | '/' | '(' | ')' | '|' | '&'))
            .map(|c| match c {
                '-' => Ok(Token::Minus),
                '+' => Ok(Token::Plus),
                '*' => Ok(Token::Multiply),
                '/' => Ok(Token::Divide),
                '(' => Ok(Token::LeftParen),
                ')' => Ok(Token::RightParen),
                '|' => match self.iter.next() {
                    Some('|') => Ok(Token::Or),
                    Some(c) => Err(Error::UnexpectedCharacter(c)),
                    None => Err(Error::UnexpectedEndOfExpr),
                },
                '&' => match self.iter.next() {
                    Some('&') => Ok(Token::And),
                    Some(c) => Err(Error::UnexpectedCharacter(c)),
                    None => Err(Error::UnexpectedEndOfExpr),
                },
                _ => panic!(),
            })
    }

    fn next_id(&mut self) -> Option<Result<Token, Error>> {
        let valid_first_char = |c: &char| c.is_ascii_alphabetic() || *c == '_';
        let valid_remaining = |c: &char| c.is_ascii_alphanumeric() || *c == '_';

        self.iter.next_if(valid_first_char).map(|first| {
            let id: String = std::iter::once(first)
                .chain(std::iter::from_fn(|| self.iter.next_if(valid_remaining)))
                .collect();
            let token = match id.as_str() {
                "true" => Token::BoolLiteral(true),
                "false" => Token::BoolLiteral(false),
                _ => Token::Id(id),
            };
            Ok(token)
        })
    }
}

impl DecimalLiteral {
    pub fn negative(&self) -> Self {
        Self {
            int: -self.int,
            num: -self.num,
            denom: self.denom,
        }
    }
    pub fn as_fraction(&self) -> (i64, i64) {
        let num = self.int * self.denom + self.num;
        let gcd = num::integer::gcd(num, self.denom);
        (num / gcd, self.denom / gcd)
    }
}
