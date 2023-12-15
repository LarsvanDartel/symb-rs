use std::num::ParseIntError;
use std::str::FromStr;

use crate::Expression;

use super::literals;

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
    Var { name: String },

    /// Number
    Num { value: Number },

    /// Function
    Fun(Function),

    /// Constant
    Const(Constant),

    /// Error
    Err(String),

    /// Placeholder for a single expression
    Slot {
        name: String,
        predicate: &'static dyn Fn(&Expression) -> bool,
    },

    /// Placeholder for a list of expressions
    Segment {
        name: String,
        predicate: &'static dyn Fn(&Expression) -> bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Priority {
    AddSub = 1,
    MulDiv = 2,
    Pow = 3,
    None = 4,
}

impl Action {
    pub fn priority(&self) -> Priority {
        match self {
            Action::Add | Action::Sub => Priority::AddSub,
            Action::Mul | Action::Div => Priority::MulDiv,
            Action::Pow => Priority::Pow,
            _ => Priority::None,
        }
    }

    pub fn identity(&self) -> Expression {
        match self {
            Self::Add => Expression::create_value(0),
            Self::Mul => Expression::create_value(1),
            _ => panic!("No identity for {:?}", self),
        }
    }
}

impl PartialEq for Action {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var { name: a }, Self::Var { name: b }) => a == b,
            (Self::Num { value: a }, Self::Num { value: b }) => a == b,
            (Self::Fun(a), Self::Fun(b)) => a == b,
            (Self::Const(a), Self::Const(b)) => a == b,
            (Self::Err(_), Self::Err(_)) => false,
            (Self::Slot { name: a, .. }, Self::Slot { name: b, .. }) => a == b,
            (Self::Segment { name: a, .. }, Self::Segment { name: b, .. }) => a == b,
            (Self::Add, Self::Add) => true,
            (Self::Sub, Self::Sub) => true,
            (Self::Mul, Self::Mul) => true,
            (Self::Div, Self::Div) => true,
            (Self::Pow, Self::Pow) => true,
            (Self::Equals, Self::Equals) => true,
            _ => false,
        }
    }
}

impl PartialOrd for Action {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some((self.priority() as u8).cmp(&(other.priority() as u8)))
    }
}

impl Clone for Action {
    fn clone(&self) -> Self {
        match self {
            Self::Add => Self::Add,
            Self::Sub => Self::Sub,
            Self::Mul => Self::Mul,
            Self::Div => Self::Div,
            Self::Pow => Self::Pow,
            Self::Equals => Self::Equals,
            Self::Var { name } => Self::Var { name: name.clone() },
            Self::Num { value } => Self::Num {
                value: value.clone(),
            },
            Self::Fun(arg0) => Self::Fun(arg0.clone()),
            Self::Const(arg0) => Self::Const(arg0.clone()),
            Self::Err(arg0) => Self::Err(arg0.clone()),
            Self::Slot { name, predicate } => Self::Slot {
                name: name.clone(),
                predicate: *predicate,
            },
            Self::Segment { name, predicate } => Self::Segment {
                name: name.clone(),
                predicate: *predicate,
            },
        }
    }
}

impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Action::Add => f.write_str(literals::ADD),
            Action::Sub => f.write_str(literals::SUB),
            Action::Mul => f.write_str(literals::MUL),
            Action::Div => f.write_str(literals::DIV),
            Action::Pow => f.write_str(literals::POW),
            Action::Equals => f.write_str(literals::EQUALS),
            Action::Var { name } => f.write_str(name),
            Action::Num { value } => f.write_str(&value.to_string()),
            Action::Fun(function) => f.write_str(&function.to_string()),
            Action::Const(c) => f.write_str(&c.to_string()),
            Action::Err(message) => f.write_fmt(format_args!("Error: {}", message)),
            Action::Slot { name, .. } => f.write_str(name),
            Action::Segment { name, .. } => f.write_str(name),
        }
    }
}

impl std::fmt::Debug for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "Add"),
            Self::Sub => write!(f, "Sub"),
            Self::Mul => write!(f, "Mul"),
            Self::Div => write!(f, "Div"),
            Self::Pow => write!(f, "Pow"),
            Self::Equals => write!(f, "Equals"),
            Self::Var { name } => f.debug_struct("Var").field("name", name).finish(),
            Self::Num { value } => f.debug_struct("Num").field("value", value).finish(),
            Self::Fun(arg0) => f.debug_tuple("Fun").field(arg0).finish(),
            Self::Const(arg0) => f.debug_tuple("Const").field(arg0).finish(),
            Self::Err(arg0) => f.debug_tuple("Err").field(arg0).finish(),
            Self::Slot { name, .. } => f.debug_struct("Slot").field("name", name).finish(),
            Self::Segment { name, .. } => f.debug_struct("Segment").field("name", name).finish(),
        }
    }
}

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
    type Err = ParseIntError;

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

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Number::Int(int) => f.write_str(&int.to_string()),
            Number::Rational(num, den) => f.write_str(&format!("{}/{}", num, den)),
            Number::Real(real) => f.write_str(&real.to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

    /// Absolute value
    Abs,
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
            literals::ABS => Ok(Function::Abs),
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
            Function::Abs => f.write_str(literals::ABS),
        }
    }
}
