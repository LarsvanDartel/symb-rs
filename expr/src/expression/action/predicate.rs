use std::str::FromStr;

use crate::{Action, Expression, Function, Number};
use num_integer::{Integer, Roots};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Predicate {
    predicates: Vec<(PredicateType, bool)>,
}

impl Predicate {
    pub fn new(predicates: Vec<(PredicateType, bool)>) -> Self {
        Self { predicates }
    }

    pub(crate) fn matches(&self, expr: &Expression) -> bool {
        self.predicates.iter().all(|(p, b)| p.matches(expr) == *b)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PredicateType {
    IsNumber,
    IsInteger,
    IsRational,
    IsZero,
    IsOne,
    IsEven,
    IsOdd,
    IsPositive,
    IsNonnegative,
    IsNegative,
    IsNonpositive,
    IsConstant,
    IsValue,
    IsVariable,
    IsRationalReducible,
    IsRootReducible,
    IsLogReducible,
    CanCombine,
    IsIndependent,
    IsSorted,
}

impl PredicateType {
    pub(crate) fn arity(&self) -> usize {
        match self {
            Self::IsIndependent => 2,
            _ => 1,
        }
    }

    pub(crate) fn eval(&self, args: &Vec<Expression>) -> bool {
        if self.arity() != args.len() {
            panic!(
                "Expected {} arguments, got {}",
                self.arity(),
                args.len()
            );
        }
        if self.arity() == 1 {
            return self.matches(&args[0]);
        }
        match self {
            Self::IsIndependent => is_independent(&args[0], &args[1]),
            _ => unimplemented!()
        }
    }

    pub(crate) fn matches(&self, expr: &Expression) -> bool {
        match self {
            Self::IsNumber => is_number(expr),
            Self::IsInteger => is_integer(expr),
            Self::IsRational => is_rational(expr),
            Self::IsZero => is_zero(expr),
            Self::IsOne => is_one(expr),
            Self::IsEven => is_even(expr),
            Self::IsOdd => is_odd(expr),
            Self::IsPositive => is_positive(expr),
            Self::IsNonnegative => is_nonnegative(expr),
            Self::IsNegative => is_negative(expr),
            Self::IsNonpositive => is_nonpositive(expr),
            Self::IsConstant => is_constant(expr),
            Self::IsValue => is_value(expr),
            Self::IsVariable => is_variable(expr),
            Self::IsRationalReducible => is_rational_reducible(expr),
            Self::IsRootReducible => is_root_reducible(expr),
            Self::IsLogReducible => is_log_reducible(expr),
            Self::CanCombine => can_combine(expr),
            Self::IsIndependent => todo!(),
            Self::IsSorted => is_sorted(expr),
        }
    }
}

impl FromStr for PredicateType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "is_number" => Ok(Self::IsNumber),
            "is_integer" => Ok(Self::IsInteger),
            "is_rational" => Ok(Self::IsRational),
            "is_zero" => Ok(Self::IsZero),
            "is_one" => Ok(Self::IsOne),
            "is_even" => Ok(Self::IsEven),
            "is_odd" => Ok(Self::IsOdd),
            "is_positive" => Ok(Self::IsPositive),
            "is_nonnegative" => Ok(Self::IsNonnegative),
            "is_negative" => Ok(Self::IsNegative),
            "is_nonpositive" => Ok(Self::IsNonpositive),
            "is_constant" => Ok(Self::IsConstant),
            "is_value" => Ok(Self::IsValue),
            "is_variable" => Ok(Self::IsVariable),
            "is_rational_reducible" => Ok(Self::IsRationalReducible),
            "is_root_reducible" => Ok(Self::IsRootReducible),
            "is_log_reducible" => Ok(Self::IsLogReducible),
            "can_combine" => Ok(Self::CanCombine),
            "is_independent" => Ok(Self::IsIndependent),
            "is_sorted" => Ok(Self::IsSorted),
            _ => Err(()),
        }
    }
}

pub(crate) fn is_number(expr: &Expression) -> bool {
    matches!(expr.action, Action::Num { .. })
}

pub(crate) fn is_integer(expr: &Expression) -> bool {
    matches!(
        expr.action,
        Action::Num {
            value: Number::Int(_)
        }
    )
}

pub(crate) fn is_rational(expr: &Expression) -> bool {
    matches!(
        expr.action,
        Action::Num {
            value: Number::Rational(_, _)
        }
    )
}

pub(crate) fn is_zero(expr: &Expression) -> bool {
    expr.action == Action::Num {
        value: Number::Int(0)
    }
}

pub(crate) fn is_one(expr: &Expression) -> bool {
    expr.action == Action::Num {
        value: Number::Int(1)
    }
}

pub(crate) fn is_even(expr: &Expression) -> bool {
    matches!(expr.action, Action::Num { value: Number::Int(n) } if n.is_even())
}

pub(crate) fn is_odd(expr: &Expression) -> bool {
    matches!(expr.action, Action::Num { value: Number::Int(n) } if n.is_odd())
}

pub(crate) fn is_positive(expr: &Expression) -> bool {
    matches!(expr.action, Action::Num { value } if value > Number::Int(0))
}

pub(crate) fn is_nonnegative(expr: &Expression) -> bool {
    matches!(expr.action, Action::Num { value } if value >= Number::Int(0))
}

pub(crate) fn is_negative(expr: &Expression) -> bool {
    matches!(expr.action, Action::Num { value } if value < Number::Int(0))
}

pub(crate) fn is_nonpositive(expr: &Expression) -> bool {
    matches!(expr.action, Action::Num { value } if value <= Number::Int(0))
}

pub(crate) fn is_constant(expr: &Expression) -> bool {
    matches!(expr.action, Action::Const(_))
}

pub(crate) fn is_value(expr: &Expression) -> bool {
    matches!(expr.action, Action::Num { .. } | Action::Const(_))
}

pub(crate) fn is_variable(expr: &Expression) -> bool {
    matches!(expr.action, Action::Var { .. })
}

pub(crate) fn is_rational_reducible(expr: &Expression) -> bool {
    if let Action::Num { value: Number::Rational(num, den) } = expr.action {
        den < 0 || den == 1 || num.gcd(&den) != 1
    } else {
        false
    }
}

pub(crate) fn is_root_reducible(expr: &Expression) -> bool {
    let (base, num) = if Action::Fun(Function::Sqrt) == expr.action {
        (2, &expr.children[0])
    } else if Action::Fun(Function::Root) == expr.action {
        if let Action::Num {
            value: Number::Int(i),
        } = expr.children[0].action
        {
            (i, &expr.children[1])
        } else {
            return false;
        }
    } else if Action::Pow == expr.action {
        return matches!(
            expr.children[0].action,
            Action::Num {
                value: Number::Int(_)
            }
        ) && matches!(
            expr.children[1].action,
            Action::Num {
                value: Number::Rational(_, _)
            }
        ) && !is_rational_reducible(&expr.children[1]);
    } else {
        return false;
    };

    let num = if let Action::Num { value } = num.action {
        value
    } else {
        return false;
    };

    match num {
        Number::Int(i) => is_perfect_root(i, base),
        Number::Rational(num, den) => is_perfect_root(num, base) && is_perfect_root(den, base),
        _ => true,
    }
}

/// Computes the `n`-th root of `num` if it is an integer.
fn is_perfect_root(num: i64, n: i64) -> bool {
    if num < 0 && n % 2 == 0 || n <= 0 {
        return false;
    }
    let root = num.nth_root(n as u32);
    root.pow(n as u32) == num
}

pub(crate) fn is_log_reducible(expr: &Expression) -> bool {
    if Action::Fun(Function::Log) != expr.action {
        return false;
    }

    if !is_positive(&expr.children[0])
        || !is_positive(&expr.children[1])
        || is_one(&expr.children[0]) {
        return false;
    }

    let base = if let Action::Num { value: Number::Int(i) } = &expr.children[0].action {
        *i
    } else {
        return false;
    };
    let num = if let Action::Num { value } = &expr.children[1].action {
        value.clone()
    } else {
        return false;
    };

    let num = match num {
        Number::Int(i) => i,
        Number::Rational(num, den) => {
            if num != 1 {
                return false;
            }
            den
        }
        _ => return true,
    };

    let mut i = 1;
    while i < num {
        i *= base;
    }
    i == num
}

pub(crate) fn can_combine(expr: &Expression) -> bool {
    if expr.action == Action::Add || expr.action == Action::Mul {
        for (i, c1) in expr.children.iter().enumerate() {
            let p1 = c1.split(&expr.action).0;
            if p1.is_none() {
                continue;
            }
            for c2 in &expr.children[i + 1..] {
                if p1 == c2.split(&expr.action).0 {
                    return true;
                }
            }
        }
        false
    } else {
        false
    }
}

pub(crate) fn is_independent(expr: &Expression, var: &Expression) -> bool {
    let var = if let Action::Var { name } = &var.action {
        name
    } else {
        return false;
    };
    !expr.has_variable(var)
}

pub(crate) fn is_sorted(expr: &Expression) -> bool {
    if Action::Add == expr.action {
        for (i, c) in expr.children.iter().enumerate().skip(1) {
            let a = c.count_variables();
            let b = expr.children[i - 1].count_variables();
            if a.iter().sum::<usize>() > b.iter().sum::<usize>()
                || (a.iter().sum::<usize>() == b.iter().sum::<usize>() && a > b)
            {
                return false;
            }
        }
        true
    } else if Action::Mul == expr.action {
        for (i, c) in expr.children.iter().enumerate().skip(1) {
            if is_value(&expr.children[i - 1]) {
                continue;
            }
            if is_value(c) {
                return false;
            }
            let a = c.count_variables();
            let b = expr.children[i - 1].count_variables();
            if a > b {
                return false;
            }
        }
        true
    } else {
        true
    }
}
