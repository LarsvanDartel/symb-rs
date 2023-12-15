mod action;
pub mod literals;
pub mod predicates;
mod parser;

pub use action::{Action, Constant, Function, Number};
use parser::Parser;
use std::{collections::HashMap, str::FromStr};

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
        Self::new_empty(Action::Num {
            value: number.into(),
        })
    }

    pub fn create_variable<T: ToString>(name: T) -> Self {
        Self::new_empty(Action::Var {
            name: name.to_string(),
        })
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

impl Expression {
    pub fn is_number(&self) -> bool {
        if let Action::Num { .. } = &self.action {
            true
        } else {
            false
        }
    }

    pub fn is_integer(&self) -> bool {
        if let Action::Num { value: Number::Int(_) } = &self.action {
            true
        } else {
            false
        }
    }

    pub fn is_positive(&self) -> bool {
        if let Action::Num { value } = &self.action {
            value > &Number::Int(0)
        } else {
            false
        }
    }

    pub fn is_nonnegative(&self) -> bool {
        if let Action::Num { value } = &self.action {
            value >= &Number::Int(0)
        } else {
            false
        }
    }

    pub fn is_negative(&self) -> bool {
        if let Action::Num { value } = &self.action {
            value < &Number::Int(0)
        } else {
            false
        }
    }

    pub fn is_nonpositive(&self) -> bool {
        if let Action::Num { value } = &self.action {
            value <= &Number::Int(0)
        } else {
            false
        }
    }

    pub fn is_constant(&self) -> bool {
        if let Action::Const(_) = &self.action {
            true
        } else {
            false
        }
    }

    pub fn is_value(&self) -> bool {
        self.is_number() || self.is_constant()
    }

    pub fn is_variable(&self) -> bool {
        if let Action::Var { .. } = &self.action {
            true
        } else {
            false
        }
    }
}

impl Expression {
    fn match_children_unordered(
        &self,
        other: &Self,
        patterns: &mut HashMap<String, Expression>,
    ) -> bool {
        if other.children.iter().any(|c| {
            if let Action::Segment { .. } | Action::Slot { .. } = c.action {
                true
            } else {
                false
            }
        }) {
            panic!("Ambiguous pattern match");
        }

        if self.children.len() == 0 {
            return self.action.identity().matches(other, patterns);
        }

        if other.children.len() == 0 {
            return self.matches(&other.action.identity(), patterns);
        }

        let mut self_matched = vec![false; self.children.len()];
        let mut other_matched = vec![false; other.children.len()];

        let mut self_patterns = vec![];

        for i in 0..self.children.len() {
            if let Action::Segment { .. } | Action::Slot { .. } = self.children[i].action {
                self_patterns.push(i);
                self_matched[i] = true;
                continue;
            }

            for j in 0..other.children.len() {
                if other_matched[j] {
                    continue;
                }
                if self.children[i].matches(&other.children[j], patterns) {
                    self_matched[i] = true;
                    other_matched[j] = true;
                    break;
                }
            }

            if !self_matched[i] {
                return false;
            }
        }

        if !self_matched.iter().all(|&b| b) {
            return false;
        }

        // for all patterns, determine all possible matches (i.e. all children of other that are not matched and for which predicate holds)
        let mut possible_matches = vec![vec![]; self_patterns.len()];

        for i in 0..other.children.len() {
            if other_matched[i] {
                continue;
            }
            for j in 0..self_patterns.len() {
                let p = &self.children[self_patterns[j]];
                if let Action::Slot { predicate, .. } = p.action {
                    if predicate(&other.children[i]) {
                        if other_matched[i] {
                            return false;
                        }
                        possible_matches[j].push(i);
                        other_matched[i] = true;
                    }
                }
            }
            if other_matched[i] {
                continue;
            }
            for j in 0..self_patterns.len() {
                let p = &self.children[self_patterns[j]];
                if let Action::Segment { predicate, .. } = p.action {
                    if predicate(&other.children[i]) {
                        if other_matched[i] {
                            return false;
                        }
                        possible_matches[j].push(i);
                        other_matched[i] = true;
                    }
                }
            }
        }

        if !other_matched.iter().all(|&b| b) {
            return false;
        }

        for i in 0..self_patterns.len() {
            let p = &self.children[self_patterns[i]];
            if let Action::Slot { .. } = p.action {
                if possible_matches[i].len() > 1 {
                    return false;
                }
                if possible_matches[i].len() == 0 {
                    return p.matches(&Expression::new_empty(other.action.clone()), patterns);
                }
                let j = possible_matches[i][0];
                if !p.matches(&other.children[j], patterns) {
                    return false;
                }
            } else if let Action::Segment { .. } = p.action {
                let matched = if possible_matches[i].len() == 1 {
                    other.children[possible_matches[i][0]].clone()
                } else {
                    Expression::new(
                        possible_matches[i]
                            .iter()
                            .map(|&j| other.children[j].clone())
                            .collect(),
                        other.action.clone(),
                    )
                };

                if !p.matches(&matched, patterns) {
                    return false;
                }
            }
        }

        true
    }
    fn match_children_ordered(
        &self,
        other: &Self,
        patterns: &mut HashMap<String, Expression>,
    ) -> bool {
        if other.children.iter().any(|c| {
            if let Action::Segment { .. } | Action::Slot { .. } = c.action {
                true
            } else {
                false
            }
        }) {
            panic!("Ambiguous pattern match");
        }

        if self.children.len() != other.children.len() {
            return false;
        }

        for i in 0..self.children.len() {
            if !self.children[i].matches(&other.children[i], patterns) {
                return false;
            }
        }

        true
    }

    pub fn matches(&self, other: &Self, patterns: &mut HashMap<String, Expression>) -> bool {
        match (&self.action, &other.action) {
            (
                Action::Segment { .. } | Action::Slot { .. },
                Action::Segment { .. } | Action::Slot { .. },
            ) => panic!("Ambiguous pattern match"),
            (_, Action::Slot { .. }) | (_, Action::Segment { .. }) => other.matches(self, patterns),
            (Action::Slot { name, .. }, _) | (Action::Segment { name, .. }, _) => {
                if let Some(expr) = patterns.get(name) {
                    return expr.clone().matches(other, patterns);
                }
                patterns.insert(name.clone(), other.clone());
                true
            }
            (Action::Add, Action::Add) => self.match_children_unordered(other, patterns),
            (Action::Mul, Action::Mul) => self.match_children_unordered(other, patterns),
            (a, b) if a == b => self.match_children_ordered(other, patterns),
            (Action::Add, _) => {
                self.matches(&Expression::new(vec![other.clone()], Action::Add), patterns)
            }
            (Action::Mul, _) => {
                self.matches(&Expression::new(vec![other.clone()], Action::Mul), patterns)
            }
            (_, Action::Add | Action::Mul) => other.matches(self, patterns),
            (_, Action::Pow) => other.matches(self, patterns),
            (Action::Pow, _) => self.matches(
                &Expression::new_binary(
                    other.clone(),
                    Expression::create_value(action::Number::Int(1)),
                    Action::Pow,
                ),
                patterns,
            ),
            _ => false,
        }
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

impl std::ops::BitXor for Expression {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, rhs, Action::Pow)
    }
}

impl std::ops::Neg for Expression {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::new_binary(
            Expression::create_value(action::Number::Int(-1)),
            self,
            Action::Mul,
        )
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Action::Fun(_) = self.action {
            f.write_str(&self.action.to_string())?;
            f.write_str("(")?;
            for i in 0..self.children.len() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                f.write_str(&self.children[i].to_string())?;
            }
            f.write_str(")")?;
        } else if self.children.len() == 0 {
            f.write_str(&self.action.to_string())?;
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
