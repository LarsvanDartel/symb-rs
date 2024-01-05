use crate::{Action, Expression, Number};

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

fn part(e: &Expression, action: &Action) -> (Option<Expression>, Option<Expression>) {
    if let Action::Add = action {
        if let Action::Mul = e.action {
            let mut p1 = vec![];
            let mut p2 = vec![];
            for c in &e.children {
                if predicates::is_number(c) {
                    p2.push(c.clone());
                } else {
                    p1.push(c.clone());
                }
            }
            let p1 = if p1.is_empty() {
                None
            } else if p1.len() == 1 {
                Some(p1[0].clone())
            } else {
                Some(Expression::new(p1, Action::Mul))
            };
            let p2 = if p2.is_empty() {
                None
            } else if p2.len() == 1 {
                Some(p2[0].clone())
            } else {
                Some(Expression::new(p2, Action::Mul))
            };
            (p1, p2)
        } else {
            if predicates::is_number(e) {
                (None, Some(e.clone()))
            } else {
                (Some(e.clone()), None)
            }
        }
    } else if let Action::Mul = action {
        if let Action::Pow = e.action {
            (Some(e.children[0].clone()), Some(e.children[1].clone()))
        } else {
            (Some(e.clone()), None)
        }
    } else {
        panic!("Invalid action {:?}", action);
    }
}

fn count_variables(e: &Expression) -> [usize; 26] {
    if let Action::Var { name } = &e.action {
        let mut res = [0; 26];
        res[name.chars().next().unwrap() as usize - 'a' as usize] = 1;
        res
    } else if let Action::Pow = e.action {
        if let Action::Num {
            value: Number::Int(i),
        } = e.children[1].action
        {
            if i < 0 {
                return [0; 26];
            }
            let mut res = count_variables(&e.children[0]);
            for c in &mut res {
                *c *= i as usize;
            }
            res
        } else {
            return [0; 26];
        }
    } else if let Action::Mul = e.action {
        let mut res = [0; 26];
        for c in &e.children {
            let p = count_variables(c);
            for (i, c) in p.iter().enumerate() {
                res[i] += c;
            }
        }
        res
    } else {
        [0; 26]
    }
}

pub mod predicates {
    use super::{count_variables, part};
    use crate::{Action, Expression, Function, Number};
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

        pub fn can_reduce(e: &Expression) -> bool {
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

        pub fn can_combine(e: &Expression) -> bool {
            if let Action::Add | Action::Mul = e.action {
                for (i, c1) in e.children.iter().enumerate() {
                    let p1 = part(c1, &e.action).0;
                    if p1.is_none() {
                        continue;
                    }
                    for c2 in &e.children[i + 1..] {
                        if p1 == part(c2, &e.action).0 {
                            return true;
                         }
                    }
                }
                false
            } else {
                false
            }
        }

        pub fn is_derivative_independent(e: &Expression) -> bool {
            if let Action::Fun(Function::D) = e.action {
                assert_eq!(e.children.len(), 2);
                let var = if let Action::Var { name } = &e.children[1].action {
                    name
                } else {
                    panic!("Expected variable, got {:?}", e.children[1]);
                };
                !e.children[0].has_variable(var)
            } else {
                false
            }
        }

        pub fn is_sorted(e: &Expression) -> bool {
            if let Action::Add = e.action {
                for (i, c) in e.children.iter().enumerate().skip(1) {
                    let a = count_variables(c);
                    let b = count_variables(&e.children[i - 1]);
                    if a.iter().sum::<usize>() > b.iter().sum::<usize>() {
                        return false;
                    } else if a.iter().sum::<usize>() == b.iter().sum::<usize>() && a > b {
                        return false;
                    }
                }
                true
            } else if let Action::Mul = e.action {
                for (i, c) in e.children.iter().enumerate().skip(1) {
                    if is_value(c) {
                        return false;
                    } else if count_variables(c) > count_variables(&e.children[i - 1]) && !is_value(&e.children[i - 1]) {
                        return false;
                    }
                }
                true
            } else {
                true
            }
        }
    }
}

pub mod maps {
    use super::predicates::{is_integer, is_number, is_one, is_value, is_zero};
    use crate::{
        literals::{count_variables, part},
        Action, Expression, Function, Number,
    };
    use num_integer::Integer;
    use std::collections::HashMap;

    macro_rules! make_maps {
        {$(pub fn $map_name:ident ($ex:ident: &Expression) -> Expression $body:block)*} => {
            $(pub fn $map_name($ex: &Expression) -> Expression $body)*

            pub const MAPS: [&str; [$(stringify!($map_name)),*].len()] = [$(stringify!($map_name)),*];
        };
    }

    make_maps! {
        pub fn reduce(e: &Expression) -> Expression {
            if let Action::Add | Action::Mul = e.action {
                let mut res = if let Action::Num { value } = e.action.identity().action {
                    value
                } else {
                    unreachable!()
                };
                for c in &e.children {
                    assert!(is_number(c));
                    if let Action::Num { value } = c.action {
                        res = match e.action {
                            Action::Add => res + value,
                            Action::Mul => res * value,
                            _ => unreachable!(),
                        };
                    } else {
                        unreachable!()
                    }
                }
                Expression::create_value(res)
            } else if let Action::Pow = e.action {
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
            } else if let Action::Fun(Function::D) = e.action {
                match e.children[0].action {
                    Action::Add => {
                        let mut res = vec![];
                        for c in &e.children[0].children {
                            res.push(Expression::create_function(
                                Function::D,
                                vec![c.clone(), e.children[1].clone()],
                            ));
                        }
                        Expression::new(res, Action::Add)
                    },
                    Action::Mul => {
                        let f = e.children[0].clone();
                        assert!(f.children.len() >= 2);
                        let arg = e.children[1].clone();
                        let arg_name = if let Action::Var { name } = &arg.action {
                            name
                        } else {
                            panic!("Expected variable, got {:?}", arg);
                        };
                        let independent = f.children
                            .iter()
                            .enumerate()
                            .filter(|(_, c)| !c.has_variable(arg_name))
                            .collect::<Vec<_>>();

                        if independent.len() == f.children.len() {
                            return Expression::create_value(Number::Int(0));
                        }

                        if !independent.is_empty() {
                            let mut p1 = independent
                                .iter()
                                .map(|(i, _)| f.children[*i].clone())
                                .collect::<Vec<_>>();
                            let p2 = f.children
                                .iter()
                                .enumerate()
                                .filter(|(i, _)| !independent.iter().any(|(j, _)| i == j))
                                .map(|(_, c)| c.clone())
                                .collect::<Vec<_>>();
                            let d = if p2.len() == 1 {
                                Expression::create_function(Function::D, vec![p2[0].clone(), arg.clone()])
                            } else {
                                Expression::create_function(Function::D, vec![Expression::new(p2, Action::Mul), arg.clone()])
                            };
                            p1.push(d);
                            return Expression::new(p1, Action::Mul);
                        }

                        let p1 = f.children[0].clone();
                        let p2 = if f.children.len() == 2 {
                            f.children[1].clone()
                        } else {
                            Expression::new(f.children[1..].to_vec(), Action::Mul)
                        };
                        let d1 = Expression::create_function(Function::D, vec![p1.clone(), arg.clone()]);
                        let d2 = Expression::create_function(Function::D, vec![p2.clone(), arg.clone()]);
                        Expression::new(vec![p1 * d2, p2 * d1], Action::Add)
                    },
                    _ => panic!("Cannot derive {:?}", e.children[0].action),
                }

            } else {
                panic!("Cannot reduce {}", e);
            }
        }

        pub fn rational_reduce(e: &Expression) -> Expression {
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

        pub fn create_rational(e: &Expression) -> Expression {
            assert_eq!(e.action, Action::Div);
            assert_eq!(e.children.len(), 2);
            let num = if let Action::Num{value: Number::Int(i)} = e.children[0].action {
                i
            } else {
                panic!("Expected integer, got {:?}", e.children[0]);
            };
            let den = if let Action::Num{value: Number::Int(i)} = e.children[1].action {
                i
            } else {
                panic!("Expected integer, got {:?}", e.children[1]);
            };
            Expression::create_value(Number::Rational(num, den))
        }

        pub fn distribute(e: &Expression) -> Expression {
            if let Action::Mul = e.action {
                assert!(e.children.len() >= 2);
                assert_eq!(e.children[0].action, Action::Add);
                let a = e.children[0].clone();
                let b = if e.children.len() == 2 {
                    e.children[1].clone()
                } else {
                    Expression::new(e.children[1..].to_vec(), Action::Mul)
                };
                let mut res = vec![];
                for c in &a.children {
                    res.push(b.clone() * c.clone());
                }
                Expression::new(res, Action::Add)
            } else if let Action::Pow = e.action {
                assert_eq!(e.children.len(), 2);
                assert_eq!(e.children[0].action, Action::Mul);
                let a = e.children[0].clone();
                let b = e.children[1].clone();
                Expression::new(
                    a.children
                        .iter()
                        .map(|c| Expression::new_binary(
                            c.clone(),
                            b.clone(),
                            Action::Pow)
                        )
                        .collect(),
                    Action::Mul,
                )
            } else {
                panic!("Cannot distribute {}", e);
            }
        }

        pub fn combine(e: &Expression) -> Expression {
            if let Action::Add | Action::Mul = e.action {
                let mut cnt = HashMap::new();
                let mut res = vec![];
                for (i, c) in e.children.iter().enumerate() {
                    let p = part(c, &e.action);
                    if p.0.is_none() {
                        res.push(c.clone());
                        continue;
                    }
                    cnt.entry(p.0.unwrap()).or_insert_with(Vec::new).push((i, p.1));
                }
                for (p, indices) in cnt {
                    if indices.len() == 1 {
                        res.push(e.children[indices[0].0].clone());
                        continue;
                    }
                    let mut c = vec![];
                    for (_, p2) in &indices {
                        if p2.is_none() {
                            c.push(Action::Mul.identity());
                        } else {
                            c.push(p2.clone().unwrap());
                        }
                    }

                    if let Action::Add = e.action {
                        res.push(Expression::new_binary(Expression::new(c, Action::Add), p, Action::Mul));
                    } else {
                        res.push(Expression::new_binary(p, Expression::new(c, Action::Add), Action::Pow));
                    }
                }

                Expression::new(res, e.action.clone())
            } else {
                panic!("Cannot combine {}", e);
            }
        }

        pub fn sort(e: &Expression) -> Expression {
            let mut res = e.children.clone();
            if e.action == Action::Mul {
                res.sort_by(|a, b| if is_value(a) {
                    std::cmp::Ordering::Less
                } else if is_value(b) {
                    std::cmp::Ordering::Greater
                } else {
                    let a = count_variables(a);
                    let b = count_variables(b);
                    b.cmp(&a)
                });
            } else if e.action == Action::Add {
                res.sort_by(|a, b| {
                    let a = count_variables(a);
                    let b = count_variables(b);
                    if a.iter().sum::<usize>() == b.iter().sum::<usize>() {
                        b.cmp(&a)
                    } else {
                        b.iter().sum::<usize>().cmp(&a.iter().sum::<usize>())
                    }
                });
            } else {
                panic!("Cannot sort {}", e);
            }
            Expression::new(res, e.action.clone())
        }
    }
}
