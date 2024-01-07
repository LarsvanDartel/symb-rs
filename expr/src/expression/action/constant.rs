use std::str::FromStr;

use super::{literals, Number};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constant {
    /// Pi
    Pi,

    /// Euler's number
    E,
}

impl FromStr for Constant {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            literals::PI_SHORT | literals::PI => Ok(Constant::Pi),
            literals::EULER => Ok(Constant::E),
            _ => Err(format!("Unknown constant: {}", s)),
        }
    }
}

impl From<&str> for Constant {
    fn from(s: &str) -> Self {
        Constant::from_str(s).unwrap()
    }
}

impl From<String> for Constant {
    fn from(string: String) -> Self {
        Constant::from(string.as_str())
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

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Pi => f.write_str(literals::PI_SHORT),
            Constant::E => f.write_str(literals::EULER),
        }
    }
}
