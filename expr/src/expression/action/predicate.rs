use std::str::FromStr;

use crate::{Action, Expression, Function, Number};
use num_integer::Integer;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Predicate {
    predicates: Vec<(PredicateType, bool)>,
}

impl Predicate {
    pub fn new(predicates: Vec<(PredicateType, bool)>) -> Self {
        Self { predicates }
    }

    pub fn matches(&self, expr: &Expression) -> bool {
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
    CanCombine,
    IsDerivativeIndependent,
    IsSorted,
}

impl PredicateType {
    fn matches(&self, expr: &Expression) -> bool {
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
            Self::CanCombine => can_combine(expr),
            Self::IsDerivativeIndependent => is_derivative_independent(expr),
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
            "can_combine" => Ok(Self::CanCombine),
            "is_derivative_independent" => Ok(Self::IsDerivativeIndependent),
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
    matches!(
        expr.action,
        Action::Num {
            value: Number::Int(0)
        }
    )
}

pub(crate) fn is_one(expr: &Expression) -> bool {
    matches!(
        expr.action,
        Action::Num {
            value: Number::Int(1)
        }
    )
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
    matches!(expr.action, Action::Num { value: Number::Rational(num, den) } if num.gcd(&den) != 1)
}

pub(crate) fn can_combine(expr: &Expression) -> bool {
    if let Action::Add | Action::Mul = expr.action {
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

pub(crate) fn is_derivative_independent(expr: &Expression) -> bool {
    if let Action::Fun(Function::D) = expr.action {
        assert_eq!(expr.children.len(), 2);
        let var = if let Action::Var { name } = &expr.children[1].action {
            name
        } else {
            panic!("Expected variable, got {:?}", expr.children[1]);
        };
        !expr.children[0].has_variable(var)
    } else {
        false
    }
}

pub(crate) fn is_sorted(expr: &Expression) -> bool {
    if let Action::Add = expr.action {
        for (i, c) in expr.children.iter().enumerate().skip(1) {
            let a = c.count_variables();
            let b = expr.children[i - 1].count_variables();
            if a.iter().sum::<usize>() > b.iter().sum::<usize>() {
                return false;
            } else if a.iter().sum::<usize>() == b.iter().sum::<usize>() && a > b {
                return false;
            }
        }
        true
    } else if let Action::Mul = expr.action {
        for (i, c) in expr.children.iter().enumerate().skip(1) {
            let a = c.count_variables();
            let b = expr.children[i - 1].count_variables();
            if a.iter().sum::<usize>() == 0 {
                return false;
            } else if a > b && b.iter().sum::<usize>() != 0 {
                return false;
            }
        }
        true
    } else {
        true
    }
}
