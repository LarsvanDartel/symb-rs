use std::str::FromStr;

use crate::literals;

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
