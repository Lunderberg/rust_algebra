#[derive(PartialEq, Eq, Debug)]
pub struct Expr {
    items: Vec<Subexpr>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Subexpr {
    Int(i64),
    Add(usize, usize),
    Sub(usize, usize),
}

macro_rules! parse {
    (@push $items:ident, $subexpr: expr) => {{
        $items.push($subexpr);
        $items.len() - 1
    }};

    (@internal $items:ident, $num:literal) => {{
        parse!(@push $items, Subexpr::Int($num))
    }};

    (@internal $items:ident, $rhs:tt, +, $($lhs:tt),+) => {{
        let lhs_index = parse!(@internal $items, $($lhs),+);
        let rhs_index = parse!(@internal $items, $rhs);

        parse!(@push $items, Subexpr::Add(lhs_index, rhs_index))
    }};
    (@internal $items:ident, $rhs:tt, -, $($lhs:tt),+) => {{
        let lhs_index = parse!(@internal $items, $($lhs),+);
        let rhs_index = parse!(@internal $items, $rhs);

        parse!(@push $items, Subexpr::Sub(lhs_index, rhs_index))
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
            items: vec![Subexpr::Int(5)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_addition() -> Result<(), Error> {
        let parsed = parse![5 + 10];
        let expected = Expr {
            items: vec![Subexpr::Int(5), Subexpr::Int(10), Subexpr::Add(0, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_multiple_addition() -> Result<(), Error> {
        let parsed = parse![5 + 10 + 15];
        let expected = Expr {
            items: vec![
                Subexpr::Int(5),
                Subexpr::Int(10),
                Subexpr::Add(0, 1),
                Subexpr::Int(15),
                Subexpr::Add(2, 3),
            ],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_subtraction() -> Result<(), Error> {
        let parsed = parse![5 - 10];
        let expected = Expr {
            items: vec![Subexpr::Int(5), Subexpr::Int(10), Subexpr::Sub(0, 1)],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_mixed_addition_subtraction() -> Result<(), Error> {
        let parsed = parse![5 + 15 - 10];
        let expected = Expr {
            items: vec![
                Subexpr::Int(5),
                Subexpr::Int(15),
                Subexpr::Add(0, 1),
                Subexpr::Int(10),
                Subexpr::Sub(2, 3),
            ],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }

    #[test]
    fn test_parse_parentheses() -> Result<(), Error> {
        let parsed = parse![5 + (15 - 10)];
        let expected = Expr {
            items: vec![
                Subexpr::Int(5),
                Subexpr::Int(15),
                Subexpr::Int(10),
                Subexpr::Sub(1, 2),
                Subexpr::Add(0, 3),
            ],
        };
        assert_eq!(parsed, expected);
        Ok(())
    }
}
