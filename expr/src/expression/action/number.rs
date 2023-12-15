use std::str::FromStr;

use crate::Constant;

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    /// Integer
    Int(i32),

    /// Rational
    Rational(i32, i32),

    /// Float
    Real(f64),
}

impl From<i32> for Number {
    fn from(int: i32) -> Self {
        Number::Int(int)
    }
}

impl From<f64> for Number {
    fn from(real: f64) -> Self {
        Number::Real(real)
    }
}

impl From<(i32, i32)> for Number {
    fn from(rational: (i32, i32)) -> Self {
        Number::Rational(rational.0, rational.1)
    }
}

impl FromStr for Number {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains('/') {
            let mut parts = s.split('/');
            let num = parts.next().unwrap().parse::<i32>()?;
            let den = parts.next().unwrap().parse::<i32>()?;
            Ok(Number::Rational(num, den))
        } else if s.contains('.') {
            let mut parts = s.split('.');
            let int = parts.next().unwrap().parse::<i32>()?;
            let dec = parts.next().unwrap();
            let den = 10i32.pow(dec.len() as u32);
            let dec = dec.parse::<i32>()?;
            Ok(Number::Rational(int * den + dec, den))
        } else {
            Ok(Number::Int(s.parse::<i32>()?))
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

impl From<Constant> for Number {
    fn from(constant: Constant) -> Self {
        match constant {
            Constant::Pi => Number::Real(std::f64::consts::PI),
            Constant::E => Number::Real(std::f64::consts::E),
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
        match self.clone() {
            Number::Int(int) => f.write_str(&int.to_string()),
            Number::Rational(num, den) => f.write_str(&format!("({}/{})", num, den)),
            Number::Real(real) => f.write_str(&real.to_string()),
        }
    }
}
