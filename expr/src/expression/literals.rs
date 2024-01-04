pub const ADD: &str = "+";
pub const SUB: &str = "-";
pub const MUL: &str = "*";
pub const DIV: &str = "/";
pub const POW: &str = "^";

#[allow(dead_code)]
pub const OPERATORS: [&str; 5] = [ADD, SUB, MUL, DIV, POW];

pub const SIN: &str = "Sin";
pub const COS: &str = "Cos";
pub const TAN: &str = "Tan";
pub const SQRT: &str = "Sqrt";
pub const ROOT: &str = "Root";
pub const EXP: &str = "Exp";
pub const LN: &str = "Ln";
pub const LOG: &str = "Log";
pub const DIFF: &str = "D";
pub const INT: &str = "Int";
pub const ABS: &str = "Abs";

#[allow(dead_code)]
pub const FUNCTIONS: [&str; 11] = [SIN, COS, TAN, SQRT, ROOT, EXP, LN, LOG, DIFF, INT, ABS];

pub const PI_SHORT: &str = "π";
pub const PI: &str = "Pi";
pub const EULER: &str = "E";

#[allow(dead_code)]
pub const CONSTANTS: [&str; 2] = [PI, EULER];

pub const COMMA: &str = ",";
pub const EQUALS: &str = "=";
pub const LEFT_PARENTHESES: &str = "([{";
pub const RIGHT_PARENTHESES: &str = ")]}";

pub const fn matching_parentheses(c: char) -> Option<char> {
    match c {
        '(' => Some(')'),
        '[' => Some(']'),
        '{' => Some('}'),
        ')' => Some('('),
        ']' => Some('['),
        '}' => Some('{'),
        _ => None,
    }
}

pub mod predicates {
    use crate::{Action, Expression, Number};
    use num_integer::Integer;

    macro_rules! make_predicates {
        {$(pub fn $predicate_name:ident ($ex:ident: &Expression) -> bool $body:block)*} => {
            $(pub fn $predicate_name($ex: &Expression) -> bool $body)*

            pub const PREDICATES: [&str; [$(stringify!($predicate_name)),*].len()] = [$(stringify!($predicate_name)),*];
        };
    }

    make_predicates! {
        pub fn is_empty(e: &Expression) -> bool {
            e.children.is_empty()
        }

        pub fn is_number(e: &Expression) -> bool {
            matches!(e.action, Action::Num { .. })
        }

        pub fn is_integer(e: &Expression) -> bool {
            matches!(
                e.action,
                Action::Num {
                    value: Number::Int(_)
                }
            )
        }

        pub fn is_rational(e: &Expression) -> bool {
            matches!(
                e.action,
                Action::Num {
                    value: Number::Rational(_, _)
                }
            )
        }

        pub fn is_zero(e: &Expression) -> bool {
            matches!(
                e.action,
                Action::Num {
                    value: Number::Int(0)
                }
            )
        }

        pub fn is_one(e: &Expression) -> bool {
            matches!(
                e.action,
                Action::Num {
                    value: Number::Int(1)
                }
            )
        }

        pub fn is_even(e: &Expression) -> bool {
            if let Action::Num { value } = e.action {
                if let Number::Int(i) = value {
                    i.is_even()
                } else {
                    false
                }
            } else {
                false
            }
        }

        pub fn is_odd(e: &Expression) -> bool {
            if let Action::Num { value } = e.action {
                if let Number::Int(i) = value {
                    i.is_odd()
                } else {
                    false
                }
            } else {
                false
            }
        }

        pub fn is_positive(e: &Expression) -> bool {
            matches!(e.action, Action::Num { value } if value > Number::Int(0))
        }

        pub fn is_nonnegative(e: &Expression) -> bool {
            matches!(e.action, Action::Num { value } if value >= Number::Int(0))
        }

        pub fn is_negative(e: &Expression) -> bool {
            matches!(e.action, Action::Num { value } if value < Number::Int(0))
        }

        pub fn is_nonpositive(e: &Expression) -> bool {
            matches!(e.action, Action::Num { value } if value <= Number::Int(0))
        }

        pub fn is_constant(e: &Expression) -> bool {
            matches!(e.action, Action::Const(_))
        }

        pub fn is_value(e: &Expression) -> bool {
            is_number(e) || is_constant(e)
        }

        pub fn is_variable(e: &Expression) -> bool {
            matches!(e.action, Action::Var { .. })
        }

        pub fn can_combine(e: &Expression) -> bool {
            if let Action::Add | Action::Mul = e.action {
                e.children.len() > 1
            } else {
                false
            }
        }

        pub fn is_rational_reducible(e: &Expression) -> bool {
            if let Action::Num{value: Number::Rational(num, den)} = e.action {
                num.gcd(&den) != 1
            } else {
                false
            }
        }
    }
}

pub mod maps {
    use super::predicates::{is_integer, is_number, is_one, is_zero};
    use crate::{Action, Expression, Number};
    use num_integer::Integer;
    use std::collections::HashMap;

    macro_rules! make_maps {
        {$(pub fn $map_name:ident ($ex:ident: &Expression, $map:tt: &HashMap<String, Expression> $(, $param:ident: $param_type:ty)*) -> Expression $body:block)*} => {
            $(pub fn $map_name($ex: &Expression, $map: &HashMap<String, Expression> $(, $param: $param_type)*) -> Expression $body)*

            pub const MAPS: [&str; [$(stringify!($map_name)),*].len()] = [$(stringify!($map_name)),*];
        };
    }

    make_maps! {
        pub fn reduce(e: &Expression, _: &HashMap<String, Expression>, action: Action) -> Expression {
            if let Action::Add | Action::Mul = action {
                assert_eq!(e.action, action);
                let mut res = if let Action::Num { value } = action.identity().action {
                    value
                } else {
                    unreachable!()
                };
                for c in &e.children {
                    assert!(is_number(c));
                    if let Action::Num { value } = c.action {
                        res = match action {
                            Action::Add => res + value,
                            Action::Mul => res * value,
                            _ => unreachable!(),
                        };
                    } else {
                        unreachable!()
                    }
                }
                Expression::create_value(res)
            } else if let Action::Pow = action {
                assert_eq!(e.action, action);
                assert!(e.children.len() == 2);
                assert!(is_integer(&e.children[1]));
                assert!(!is_zero(&e.children[0]));
                assert!(!is_one(&e.children[0]));

                let (negative, pow) = if let Action::Num {
                    value: Number::Int(i),
                } = &e.children[1].action
                {
                    (*i < 0, i.unsigned_abs() as usize)
                } else {
                    unreachable!()
                };

                if let Action::Num { value } = e.children[0].action {
                    let mut res = value;
                    for _ in 1..pow {
                        res = res * value;
                    }
                    if negative {
                        res = res.inverse();
                    }
                    return Expression::create_value(res);
                }

                assert!(!negative);
                Expression::new(
                    std::iter::repeat(e.children[0].clone())
                        .take(pow)
                        .collect(),
                    Action::Mul,
                )
            } else {
                panic!("Cannot reduce {:?}", action);
            }
        }

        pub fn rational_reduce(e: &Expression, _: &HashMap<String, Expression>) -> Expression {
            if let Action::Num{value: Number::Rational(num, den)} = e.action {
                let gcd = num.gcd(&den);
                if gcd == den {
                    Expression::create_value(Number::Int(num / den))
                } else if gcd != 1 {
                    Expression::create_value(Number::Rational(num / gcd, den / gcd))
                } else {
                    panic!("{:?} is not reducible", e);
                }
            } else {
                panic!("Expected rational number, got {:?}", e);
            }
        }

        pub fn create_rational(num: &Expression, patterns: &HashMap<String, Expression>, den: Expression) -> Expression {
            let den = den.substitute_pattern(patterns);
            match (&num.action, &den.action) {
                (Action::Num { value: Number::Int(num) }, Action::Num { value: Number::Int(den) }) => {
                    Expression::create_value(Number::Rational(*num, *den))
                },
                _ => panic!("Cannot create rational from {:?} and {:?}", num, den)
            }
        }

        pub fn distribute(
            e: &Expression,
            patterns: &HashMap<String, Expression>,
            expr: Expression,
            action: Action
        ) -> Expression {
            let expr = expr.substitute_pattern(patterns);
            if let Action::Mul = action {
                assert_eq!(e.action, Action::Add);
                let mut res = vec![];
                if expr.children.len() == 1 {
                    for c in &e.children {
                        res.push(c.clone() * expr.children[0].clone());
                    }
                } else {
                    for c in &e.children {
                        res.push(c.clone() * expr.clone());
                    }
                }
                Expression::new(res, Action::Add)
            } else if let Action::Pow = action {
                assert_eq!(e.action, Action::Mul);
                Expression::new(
                    e.children
                        .iter()
                        .map(|c| c.clone() ^ expr.clone())
                        .collect(),
                    Action::Mul,
                )
            } else {
                panic!("Cannot distribute {:?}", action);
            }
        }
    }
}
