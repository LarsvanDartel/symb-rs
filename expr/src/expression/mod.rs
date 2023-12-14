mod action;
pub mod literals;
mod parser;

pub use action::{Action, Constant, Function, Number};
use parser::Parser;
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq)]
pub struct Expression {
    children: Vec<Expression>,
    action: Action,
}

impl Expression {
    pub fn new(children: Vec<Expression>, action: Action) -> Self {
        Self { children, action }
    }

    pub fn new_binary(lhs: Expression, rhs: Expression, action: Action) -> Self {
        Self::new(vec![lhs, rhs], action)
    }

    pub fn new_empty(action: Action) -> Self {
        Self::new(vec![], action)
    }

    pub fn create_value<T: Into<Number>>(number: T) -> Self {
        Self::new_empty(Action::Num(number.into()))
    }

    pub fn create_variable<T: ToString>(name: T) -> Self {
        Self::new_empty(Action::Var(name.to_string()))
    }

    pub fn create_function<T: Into<Function>>(fun: T, args: Vec<Expression>) -> Self {
        Self::new(args, Action::Fun(fun.into()))
    }

    pub fn create_constant<T: Into<Constant>>(c: T) -> Self {
        Self::new_empty(Action::Const(c.into()))
    }

    pub fn create_error<T: ToString>(message: T) -> Self {
        Self::new_empty(Action::Err(message.to_string()))
    }
}

impl From<i32> for Expression {
    fn from(i: i32) -> Self {
        Self::create_value(i)
    }
}

impl FromStr for Expression {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Parser::new(s).parse()
    }
}

impl From<&str> for Expression {
    fn from(s: &str) -> Self {
        Self::from_str(s).unwrap()
    }
}

impl std::ops::Add for Expression {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, rhs, Action::Add)
    }
}

impl std::ops::Sub for Expression {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, rhs, Action::Sub)
    }
}

impl std::ops::Mul for Expression {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, rhs, Action::Mul)
    }
}

impl std::ops::Div for Expression {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, rhs, Action::Div)
    }
}

impl std::ops::Neg for Expression {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::new_binary(
            Expression::create_value(action::Number::Int(1)),
            self,
            Action::Mul,
        )
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.children.len() == 0 {
            f.write_str(&self.action.to_string())?;
        } else if let Action::Fun(_) = self.action {
            f.write_str(&self.action.to_string())?;
            f.write_str("(")?;
            f.write_str(&self.children[0].to_string())?;
            for i in 1..self.children.len() {
                f.write_str(", ")?;
                f.write_str(&self.children[i].to_string())?;
            }
            f.write_str(")")?;
        } else {
            for i in 0..self.children.len() {
                if i > 0 {
                    f.write_str(&self.action.to_string())?;
                }
                if self.children[i].action <= self.action {
                    f.write_str("(")?;
                    f.write_str(&self.children[i].to_string())?;
                    f.write_str(")")?;
                } else {
                    f.write_str(&self.children[i].to_string())?;
                }
            }
        }
        Ok(())
    }
}