use std::{collections::HashMap, str::FromStr};

use super::predicate::{
    is_integer, is_nonnegative, is_number, is_one, is_root_reducible, is_value, is_zero,
};
use super::{Action, Expression, Function, Number};
use num_integer::{Integer, Roots};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Map {
    Reduce,
    RationalReduce,
    RootReduce,
    LogReduce,
    CreateRational,
    Distribute,
    Combine,
    Sort,
    Abs,
    Max,
}

impl Map {
    pub fn map(&self, expr: Expression) -> Expression {
        match self {
            Self::Reduce => reduce(&expr),
            Self::RationalReduce => rational_reduce(&expr),
            Self::RootReduce => root_reduce(&expr),
            Self::LogReduce => log_reduce(&expr),
            Self::CreateRational => create_rational(&expr),
            Self::Distribute => distribute(&expr),
            Self::Combine => combine(&expr),
            Self::Sort => sort(&expr),
            Self::Abs => abs(&expr),
            Self::Max => max(&expr),
        }
    }
}

impl FromStr for Map {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "reduce" => Ok(Self::Reduce),
            "rational_reduce" => Ok(Self::RationalReduce),
            "root_reduce" => Ok(Self::RootReduce),
            "log_reduce" => Ok(Self::LogReduce),
            "create_rational" => Ok(Self::CreateRational),
            "distribute" => Ok(Self::Distribute),
            "combine" => Ok(Self::Combine),
            "sort" => Ok(Self::Sort),
            "abs" => Ok(Self::Abs),
            "max" => Ok(Self::Max),
            _ => Err(()),
        }
    }
}

fn reduce(e: &Expression) -> Expression {
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
            std::iter::repeat(e.children[0].clone()).take(pow).collect(),
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
            }
            Action::Mul => {
                let f = e.children[0].clone();
                assert!(f.children.len() >= 2);
                let arg = e.children[1].clone();
                let arg_name = if let Action::Var { name } = &arg.action {
                    name
                } else {
                    panic!("Expected variable, got {:?}", arg);
                };
                let independent = f
                    .children
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
                    let p2 = f
                        .children
                        .iter()
                        .enumerate()
                        .filter(|(i, _)| !independent.iter().any(|(j, _)| i == j))
                        .map(|(_, c)| c.clone())
                        .collect::<Vec<_>>();
                    let d = if p2.len() == 1 {
                        Expression::create_function(Function::D, vec![p2[0].clone(), arg.clone()])
                    } else {
                        Expression::create_function(
                            Function::D,
                            vec![Expression::new(p2, Action::Mul), arg.clone()],
                        )
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
            }
            _ => panic!("Cannot derive {:?}", e.children[0].action),
        }
    } else {
        panic!("Cannot reduce {}", e);
    }
}

pub(crate) fn rational_reduce(e: &Expression) -> Expression {
    if let Action::Num {
        value: Number::Rational(mut num, mut den),
    } = e.action
    {
        if den < 0 {
            num = -num;
            den = -den;
        }
        let gcd = num.gcd(&den);
        if gcd == den {
            Expression::create_value(Number::Int(num / den))
        } else if gcd != 1 {
            Expression::create_value(Number::Rational(num / gcd, den / gcd))
        } else {
            Expression::create_value(Number::Rational(num, den))
        }
    } else {
        panic!("Expected rational number, got {:?}", e);
    }
}

pub(crate) fn root_reduce(e: &Expression) -> Expression {
    assert!(is_root_reducible(e));
    let (base, num) = if let Action::Fun(Function::Sqrt) = e.action {
        (2, &e.children[0])
    } else if let Action::Fun(Function::Root) = e.action {
        if let Action::Num {
            value: Number::Int(i),
        } = e.children[0].action
        {
            (i, &e.children[1])
        } else {
            panic!("Expected integer, got {:?}", e.children[0]);
        }
    } else if let Action::Pow = e.action {
        let (base, pow) = if let Action::Num {
            value: Number::Rational(num, den),
        } = e.children[1].action
        {
            (den, num)
        } else {
            panic!("Expected rational number, got {:?}", e.children[1]);
        };

        let num = if let Action::Num {
            value: Number::Int(i),
        } = e.children[0].action
        {
            if pow == 1 {
                Expression::create_value(Number::Int(i))
            } else {
                Expression::new_binary(
                    Expression::create_value(i),
                    Expression::create_value(pow),
                    Action::Pow,
                )
            }
        } else {
            panic!("Expected integer, got {:?}", e.children[0]);
        };

        return if base == 2 {
            Expression::create_function(Function::Sqrt, vec![num])
        } else {
            Expression::create_function(Function::Root, vec![Expression::create_value(base), num])
        };
    } else {
        panic!("Expected root, got {:?}", e);
    };

    let num = if let Action::Num { value } = num.action {
        value
    } else {
        panic!("Expected number, got {:?}", num);
    };

    match num {
        Number::Int(i) => Expression::create_value(i.nth_root(base as u32)),
        Number::Rational(num, den) => {
            Expression::new_binary(create_root(base, num), create_root(base, den), Action::Div)
        }
        Number::Real(f) => Expression::create_value(f.powf(1.0 / base as f64)),
    }
}

fn create_root(base: i64, num: i64) -> Expression {
    if base == 2 {
        Expression::create_function(Function::Sqrt, vec![Expression::create_value(num)])
    } else {
        Expression::create_function(
            Function::Root,
            vec![
                Expression::create_value(base),
                Expression::create_value(num),
            ],
        )
    }
}

pub(crate) fn log_reduce(e: &Expression) -> Expression {
    assert_eq!(e.action, Action::Fun(Function::Log));
    assert_eq!(e.children.len(), 2);
    let base = if let Action::Num {
        value: Number::Int(i),
    } = e.children[0].action
    {
        i
    } else {
        panic!("Expected integer, got {:?}", e.children[0]);
    };

    if base == 1 {
        panic!("Cannot take log of 1");
    }

    let num = if let Action::Num { value } = e.children[1].action {
        value
    } else {
        panic!("Expected number, got {:?}", e.children[1]);
    };

    let (num, inv) = match num {
        Number::Int(i) => (i, false),
        Number::Rational(num, den) => {
            assert!(num == 1);
            (den, true)
        }
        Number::Real(f) => {
            return Expression::create_value(f.log(base as f64).round());
        }
    };

    let mut i = 1;
    let mut log = 0;
    while i < num {
        i *= base;
        log += (!inv as i64) * 2 - 1;
    }
    if i == num {
        Expression::create_value(log)
    } else {
        panic!("Cannot take log of {}", num);
    }
}

pub(crate) fn create_rational(e: &Expression) -> Expression {
    assert_eq!(e.action, Action::Div);
    assert_eq!(e.children.len(), 2);
    let num = if let Action::Num {
        value: Number::Int(i),
    } = e.children[0].action
    {
        i
    } else {
        panic!("Expected integer, got {:?}", e.children[0]);
    };
    let den = if let Action::Num {
        value: Number::Int(i),
    } = e.children[1].action
    {
        i
    } else {
        panic!("Expected integer, got {:?}", e.children[1]);
    };
    Expression::create_value(Number::Rational(num, den))
}

pub(crate) fn distribute(e: &Expression) -> Expression {
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
                .map(|c| Expression::new_binary(c.clone(), b.clone(), Action::Pow))
                .collect(),
            Action::Mul,
        )
    } else {
        panic!("Cannot distribute {}", e);
    }
}

pub(crate) fn combine(e: &Expression) -> Expression {
    if let Action::Add | Action::Mul = e.action {
        let mut cnt = HashMap::new();
        let mut res = vec![];
        for (i, c) in e.children.iter().enumerate() {
            let p = c.split(&e.action);
            if p.0.is_none() {
                res.push(c.clone());
                continue;
            }
            cnt.entry(p.0.unwrap())
                .or_insert_with(Vec::new)
                .push((i, p.1));
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
                res.push(Expression::new_binary(
                    Expression::new(c, Action::Add),
                    p,
                    Action::Mul,
                ));
            } else {
                res.push(Expression::new_binary(
                    p,
                    Expression::new(c, Action::Add),
                    Action::Pow,
                ));
            }
        }

        Expression::new(res, e.action.clone())
    } else {
        panic!("Cannot combine {}", e);
    }
}

pub(crate) fn sort(e: &Expression) -> Expression {
    let mut res = e.children.clone();
    if e.action == Action::Mul {
        res.sort_by(|a, b| {
            if is_value(a) {
                std::cmp::Ordering::Less
            } else if is_value(b) {
                std::cmp::Ordering::Greater
            } else {
                let a = a.count_variables();
                let b = b.count_variables();
                b.cmp(&a)
            }
        });
    } else if e.action == Action::Add {
        res.sort_by(|a, b| {
            let a = a.count_variables();
            let b = b.count_variables();
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

pub(crate) fn abs(e: &Expression) -> Expression {
    if let Action::Num { value } = e.action {
        match value {
            Number::Int(n) => Expression::create_value(n.abs()),
            Number::Rational(num, den) => Expression::create_value((num.abs(), den.abs())),
            Number::Real(f) => Expression::create_value(f.abs()),
        }
    } else if let Action::Const(c) = &e.action {
        if is_nonnegative(&Expression::create_value(Number::from(*c))) {
            e.clone()
        } else {
            -e.clone()
        }
    } else {
        panic!("Expected value, got {}", e);
    }
}

pub(crate) fn max(e: &Expression) -> Expression {
    assert_eq!(e.action, Action::Fun(Function::Max));
    assert!(e.children.len() >= Function::Max.arity().0);

    let maximum_index = e
        .children
        .iter()
        .map(|c| {
            if let Action::Num { value } = &c.action {
                *value
            } else if let Action::Const(c) = &c.action {
                Number::from(*c)
            } else {
                panic!("Expected number, got {}", c);
            }
        })
        .enumerate()
        .reduce(|(i, a), (j, b)| if b > a { (j, b) } else { (i, a) })
        .unwrap()
        .0;
    return e.children[maximum_index].clone();
}
