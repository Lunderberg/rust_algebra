use std::fmt::Display;

#[derive(PartialEq, Eq, Debug)]
pub struct Expr {
    items: Vec<Element>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Element {
    Int(i64),
    Add(usize, usize),
    Sub(usize, usize),
}

pub struct SubExpr<'a> {
    items: &'a [Element],
}

impl<'a> SubExpr<'a> {
    fn child<'b>(&self, rel_index: usize) -> SubExpr<'b>
    where
        'a: 'b,
    {
        let index = self.items.len() - rel_index;
        SubExpr {
            items: &self.items[..index],
        }
    }
}

impl<'a> From<&'a Expr> for SubExpr<'a> {
    fn from(expr: &'a Expr) -> Self {
        Self { items: &expr.items }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let view: SubExpr = self.into();
        write!(f, "{}", view)
    }
}

impl<'a> Display for SubExpr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Element::*;
        match self.items.last().unwrap() {
            Int(val) => write!(f, "{}", val),
            Add(lhs, rhs) => write!(f, "{} + {}", self.child(*lhs), self.child(*rhs)),
            Sub(lhs, rhs) => write!(f, "{} - {}", self.child(*lhs), self.child(*rhs)),
        }
    }
}

macro_rules! parse {
    (@push $items:ident, $subexpr: expr) => {{
        $items.push($subexpr);
        $items.len() - 1
    }};

    (@internal $items:ident, $num:literal) => {{
        parse!(@push $items, Element::Int($num))
    }};

    (@internal $items:ident, $rhs:tt, +, $($lhs:tt),+) => {{
        let lhs_index = parse!(@internal $items, $($lhs),+);
        let rhs_index = parse!(@internal $items, $rhs);
        let sum_index = $items.len();

        parse!(@push $items, Element::Add(sum_index - lhs_index, sum_index - rhs_index))
    }};
    (@internal $items:ident, $rhs:tt, -, $($lhs:tt),+) => {{
        let lhs_index = parse!(@internal $items, $($lhs),+);
        let rhs_index = parse!(@internal $items, $rhs);
        let diff_index = $items.len();

        parse!(@push $items, Element::Sub(diff_index - lhs_index, diff_index - rhs_index))
    }};
    (@internal $items:ident, ( $($expr:tt)+ )) => {{
        parse!(@reverse $items, internal, [$($expr),+], [])
    }};
    (@internal $items:ident, $($expression:tt) , +) => {
        parse!(@reverse $items, error, [$($expression),+], [])
    };
    (@error $items:ident, $($expression:tt) , +) => {
        compile_error!(concat!("Unexpected expression: '",
                               $(stringify!($expression) ),*,
                               "'"))
    };
    (@reverse $items:ident, $next:ident, [], [$($rev:tt),*]) => {
        parse!(@$next $items, $($rev),*)
    };
    (@reverse $items:ident, $next:ident, [$first:tt $(, $rest:tt)*], [$($rev:tt),*]) => {
        parse!(@reverse $items, $next, [$($rest),*], [$first $(,$rev)*])
    };
    ($($expression:tt)+) => {{
        let mut items = Vec::new();
        parse!(@reverse items, internal, [$($expression),+], []);
        Expr { items }
    }};
}

#[cfg(test)]
mod test {
    use super::super::Error;
    use super::*;

    #[test]
    fn test_parse_int() -> Result<(), Error> {
        let parsed = parse![5];
        let expected = Expr {
            items: vec![Element::Int(5)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_format_int() {
        let parsed = parse![42];
        assert_eq!(format!("{}", parsed), "42");
    }

    #[test]
    fn test_parse_addition() -> Result<(), Error> {
        let parsed = parse![5 + 10];
        let expected = Expr {
            items: vec![Element::Int(5), Element::Int(10), Element::Add(2, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_format_addition() {
        let parsed = parse![5 + 10];
        assert_eq!(format!("{}", parsed), "5 + 10");
    }

    #[test]
    fn test_parse_multiple_addition() -> Result<(), Error> {
        let parsed = parse![5 + 10 + 15];
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
    fn test_format_multiple_addition() {
        let parsed = parse![5 + 10 + 15];
        assert_eq!(format!("{}", parsed), "5 + 10 + 15");
    }

    #[test]
    fn test_parse_subtraction() -> Result<(), Error> {
        let parsed = parse![5 - 10];
        let expected = Expr {
            items: vec![Element::Int(5), Element::Int(10), Element::Sub(2, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_format_subtraction() {
        let parsed = parse![5 - 10];
        assert_eq!(format!("{}", parsed), "5 - 10");
    }

    #[test]
    fn test_parse_mixed_addition_subtraction() -> Result<(), Error> {
        let parsed = parse![5 + 15 - 10];
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
    fn test_format_mixed_addition_subtraction() {
        let parsed = parse![5 + 15 - 10];
        assert_eq!(format!("{}", parsed), "5 + 15 - 10");
    }

    #[test]
    fn test_parse_parentheses() -> Result<(), Error> {
        let parsed = parse![5 + (15 - 10)];
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

    // #[test]
    // fn test_format_parentheses() {
    //     let parsed = parse![5 + 10];
    //     assert_eq!(format!("{}", parsed), "5 + (15 - 10)");
    // }
}
