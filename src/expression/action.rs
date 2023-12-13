use std::num::ParseIntError;
use std::str::FromStr;

use super::literals;

#[derive(Debug, Clone)]
pub enum Action {
    /// Addition
    Add,

    /// Subtraction
    Sub,

    /// Multiplication
    Mul,

    /// Division
    Div,

    /// Power
    Pow,

    /// Equals
    Equals,

    /// Variable
    Var(String),

    /// Number
    Num(Number),

    /// Function
    Fun(Function),

    /// Constant
    Const(Constant),

    /// Error
    Err(String),
}

impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Action::Add => f.write_str(literals::ADD),
            Action::Sub => f.write_str(literals::SUB),
            Action::Mul => f.write_str(literals::MUL),
            Action::Div => f.write_str(literals::DIV),
            Action::Pow => f.write_str(literals::POW),
            Action::Equals => f.write_str(literals::EQUALS),
            Action::Var(name) => f.write_str(&name),
            Action::Num(number) => f.write_str(&number.to_string()),
            Action::Fun(function) => f.write_str(&function.to_string()),
            Action::Const(c) => f.write_str(&c.to_string()),
            Action::Err(message) => f.write_fmt(format_args!("Error: {}", message)),
        }
    }
}

impl PartialEq for Action {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Action::Add, Action::Add) => true,
            (Action::Sub, Action::Sub) => true,
            (Action::Add, Action::Sub) => true,
            (Action::Sub, Action::Add) => true,

            (Action::Mul, Action::Mul) => true,
            (Action::Div, Action::Div) => true,
            (Action::Mul, Action::Div) => true,
            (Action::Div, Action::Mul) => true,

            (Action::Pow, Action::Pow) => true,
            (Action::Var(..), Action::Var(..)) => true,
            (Action::Num(..), Action::Num(..)) => true,
            (Action::Fun(..), Action::Fun(..)) => true,
            (Action::Const(..), Action::Const(..)) => true,
            (Action::Err(..), Action::Err(..)) => true,
            (Action::Equals, Action::Equals) => true,

            _ => false,
        }
    }
}

const PRIORITY: [Action; 5] = [
    Action::Add,
    Action::Sub,
    Action::Mul,
    Action::Div,
    Action::Pow,
];

impl PartialOrd for Action {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let self_index = PRIORITY.iter().position(|x| x == self);
        let other_index = PRIORITY.iter().position(|x| x == other);

        if self_index.is_none() || other_index.is_none() {
            return None;
        }

        self_index.partial_cmp(&other_index)
    }
}

#[derive(Debug, Clone)]
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
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('.');
        let int = parts.next().unwrap().parse::<i32>()?;
        if let Some(dec) = parts.next() {
            let dec = dec.parse::<i32>()?;
            let mut den = 1;
            for _ in 0..dec.to_string().len() {
                den *= 10;
            }
            Ok(Number::Rational(int * den + dec, den))
        } else {
            Ok(Number::Int(int))
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

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Number::Int(int) => f.write_str(&int.to_string()),
            Number::Rational(num, den) => f.write_str(&format!("{}/{}", num, den)),
            Number::Real(real) => f.write_str(&real.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
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

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Constant::Pi => f.write_str(literals::PI_SHORT),
            Constant::E => f.write_str(literals::EULER),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Function {
    /// Sine
    Sin,

    /// Cosine
    Cos,

    /// Tangent
    Tan,

    /// Square root
    Sqrt,

    /// Root
    Root,

    /// Exponential
    Exp,

    /// Natural logarithm
    Ln,

    /// Logarithm
    Log,

    /// Derivative
    Diff,

    /// Integral
    Int,
}

impl FromStr for Function {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            literals::SIN => Ok(Function::Sin),
            literals::COS => Ok(Function::Cos),
            literals::TAN => Ok(Function::Tan),
            literals::SQRT => Ok(Function::Sqrt),
            literals::ROOT => Ok(Function::Root),
            literals::EXP => Ok(Function::Exp),
            literals::LN => Ok(Function::Ln),
            literals::LOG => Ok(Function::Log),
            literals::DIFF => Ok(Function::Diff),
            literals::INT => Ok(Function::Int),
            _ => Err(format!("Unknown function: {}", s)),
        }
    }
}

impl From<&str> for Function {
    fn from(s: &str) -> Self {
        Function::from_str(s).unwrap()
    }
}

impl From<String> for Function {
    fn from(string: String) -> Self {
        Function::from(string.as_str())
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Function::Sin => f.write_str(literals::SIN),
            Function::Cos => f.write_str(literals::COS),
            Function::Tan => f.write_str(literals::TAN),
            Function::Sqrt => f.write_str(literals::SQRT),
            Function::Root => f.write_str(literals::ROOT),
            Function::Exp => f.write_str(literals::EXP),
            Function::Ln => f.write_str(literals::LN),
            Function::Log => f.write_str(literals::LOG),
            Function::Diff => f.write_str(literals::DIFF),
            Function::Int => f.write_str(literals::INT),
        }
    }
}
