use std::hash::{Hash, Hasher};
use std::str::FromStr;

use crate::literals::{to_superscript, to_subscript};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    /// Integer
    Int(i64),

    /// Rational
    Rational(i64, i64),

    /// Float
    Real(f64),
}

impl Number {
    pub fn inverse(self) -> Self {
        match self {
            Number::Int(int) => Number::Rational(1, int),
            Number::Rational(num, den) => Number::Rational(den, num),
            Number::Real(real) => Number::Real(1.0 / real),
        }
    }
}

impl From<i64> for Number {
    fn from(int: i64) -> Self {
        Number::Int(int)
    }
}

impl From<f64> for Number {
    fn from(real: f64) -> Self {
        Number::Real(real)
    }
}

impl From<(i64, i64)> for Number {
    fn from(rational: (i64, i64)) -> Self {
        Number::Rational(rational.0, rational.1)
    }
}

impl FromStr for Number {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains('.') {
            let mut parts = s.split('.');
            let int = parts.next().unwrap().parse::<i64>()?;
            let dec = parts.next().unwrap();
            let den = 10i64.pow(dec.len() as u32);
            let dec = dec.parse::<i64>()?;
            Ok(Number::Rational(int * den + dec, den))
        } else {
            Ok(Number::Int(s.parse::<i64>()?))
        }
    }
}

impl From<&str> for Number {
    fn from(s: &str) -> Self {
        Number::from_str(s).unwrap()
    }
}

impl From<String> for Number {
    fn from(string: String) -> Self {
        Number::from(string.as_str())
    }
}

impl std::ops::Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(a), Number::Int(b)) => Number::Int(a + b),
            (Number::Int(a), Number::Rational(b, c)) => Number::Rational(a * c + b, c),
            (Number::Int(a), Number::Real(b)) => Number::Real(a as f64 + b),
            (Number::Rational(a, b), Number::Int(c)) => Number::Rational(a + b * c, b),
            (Number::Rational(a, b), Number::Rational(c, d)) => {
                Number::Rational(a * d + b * c, b * d)
            }
            (Number::Rational(a, b), Number::Real(c)) => Number::Real(a as f64 / b as f64 + c),
            (Number::Real(a), Number::Int(b)) => Number::Real(a + b as f64),
            (Number::Real(a), Number::Rational(b, c)) => Number::Real(a + b as f64 / c as f64),
            (Number::Real(a), Number::Real(b)) => Number::Real(a + b),
        }
    }
}

impl std::ops::Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(a), Number::Int(b)) => Number::Int(a * b),
            (Number::Int(a), Number::Rational(b, c)) => Number::Rational(a * b, c),
            (Number::Int(a), Number::Real(b)) => Number::Real(a as f64 * b),
            (Number::Rational(a, b), Number::Int(c)) => Number::Rational(a * c, b),
            (Number::Rational(a, b), Number::Rational(c, d)) => Number::Rational(a * c, b * d),
            (Number::Rational(a, b), Number::Real(c)) => Number::Real(a as f64 / b as f64 * c),
            (Number::Real(a), Number::Int(b)) => Number::Real(a * b as f64),
            (Number::Real(a), Number::Rational(b, c)) => Number::Real(a * b as f64 / c as f64),
            (Number::Real(a), Number::Real(b)) => Number::Real(a * b),
        }
    }
}

impl std::ops::Mul<i64> for Number {
    type Output = Self;

    fn mul(self, rhs: i64) -> Self::Output {
        match self {
            Number::Int(a) => Number::Int(a * rhs),
            Number::Rational(a, b) => Number::Rational(a * rhs, b),
            Number::Real(a) => Number::Real(a * rhs as f64),
        }
    }
}

impl std::ops::Div<i64> for Number {
    type Output = Self;

    fn div(self, rhs: i64) -> Self::Output {
        match self {
            Number::Int(a) => Number::Int(a / rhs),
            Number::Rational(a, b) => Number::Rational(a, b * rhs),
            Number::Real(a) => Number::Real(a / rhs as f64),
        }
    }
}

impl std::cmp::PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => a.partial_cmp(b),
            (Number::Int(a), Number::Rational(b, c)) if *c > 0 => (a * c).partial_cmp(b),
            (Number::Int(a), Number::Rational(b, c)) if *c < 0 => b.partial_cmp(&(a * c)),
            (Number::Int(a), Number::Real(b)) => (*a as f64).partial_cmp(b),
            (Number::Rational(a, b), Number::Int(c)) if *b > 0 => a.partial_cmp(&(c * b)),
            (Number::Rational(a, b), Number::Int(c)) if *b < 0 => (c * b).partial_cmp(a),
            (Number::Rational(a, b), Number::Rational(c, d)) if b * d > 0 => {
                (a * d).partial_cmp(&(c * b))
            }
            (Number::Rational(a, b), Number::Rational(c, d)) if b * d < 0 => {
                (c * b).partial_cmp(&(a * d))
            }
            (Number::Rational(a, b), Number::Real(c)) => (*a as f64 / *b as f64).partial_cmp(c),
            (Number::Real(a), Number::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Number::Real(a), Number::Rational(b, c)) => a.partial_cmp(&(*b as f64 / *c as f64)),
            (Number::Real(a), Number::Real(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Number::Int(int) => f.write_str(&int.to_string()),
            Number::Rational(num, den) => {
                let num = to_superscript(num.to_string());
                let den = to_subscript(den.to_string());
                f.write_str(&format!("{}⁄{}", num, den))
            }
            Number::Real(real) => f.write_str(&real.to_string()),
        }
    }
}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Number::Int(int) => int.hash(state),
            Number::Rational(num, den) => {
                num.hash(state);
                den.hash(state);
            }
            Number::Real(real) => real.to_bits().hash(state),
        }
    }
}
