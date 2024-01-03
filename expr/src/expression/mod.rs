mod action;
pub mod literals;
mod parser;

pub use action::{Action, Constant, Function, Number};
use parser::Parser;
use std::{collections::HashMap, str::FromStr};

use crate::Rule;

#[derive(Clone)]
pub struct Expression {
    children: Vec<Expression>,
    action: Action,
}

/// Creating new expressions
impl Expression {
    pub fn new(children: Vec<Expression>, action: Action) -> Self {
        if let Some(arity) = action.arity() {
            if children.len() != arity {
                panic!(
                    "Wrong number of arguments for {:?}: expected {}, got {}",
                    action,
                    arity,
                    children.len()
                );
            }
        }
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

/// Properties of expressions
impl Expression {
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    pub const fn is_number(&self) -> bool {
        matches!(&self.action, Action::Num { .. })
    }

    pub const fn is_integer(&self) -> bool {
        matches!(
            self.action,
            Action::Num {
                value: Number::Int(_)
            }
        )
    }

    pub const fn is_zero(&self) -> bool {
        matches!(
            self.action,
            Action::Num {
                value: Number::Int(0)
            }
        )
    }

    pub const fn is_one(&self) -> bool {
        matches!(
            self.action,
            Action::Num {
                value: Number::Int(1)
            }
        )
    }

    pub fn is_positive(&self) -> bool {
        matches!(&self.action, Action::Num { value } if value > &Number::Int(0))
    }

    pub fn is_nonnegative(&self) -> bool {
        matches!(&self.action, Action::Num { value } if value >= &Number::Int(0))
    }

    pub fn is_negative(&self) -> bool {
        matches!(&self.action, Action::Num { value } if value < &Number::Int(0))
    }

    pub fn is_nonpositive(&self) -> bool {
        matches!(&self.action, Action::Num { value } if value <= &Number::Int(0))
    }

    pub const fn is_constant(&self) -> bool {
        matches!(&self.action, Action::Const(_))
    }

    pub const fn is_value(&self) -> bool {
        self.is_number() || self.is_constant()
    }

    pub const fn is_variable(&self) -> bool {
        matches!(&self.action, Action::Var { .. })
    }

    pub fn can_combine(&self) -> bool {
        if let Action::Add | Action::Mul = self.action {
            self.children.len() > 1
        } else {
            false
        }
    }

    pub const fn is_pattern(&self) -> bool {
        matches!(&self.action, Action::Slot { .. } | Action::Segment { .. })
    }
}

/// Matching expressions
impl Expression {
    fn match_children_unordered(
        &self,
        other: &Self,
        patterns: &mut HashMap<String, Expression>,
    ) -> bool {
        if other.children.iter().any(|c| c.is_pattern()) {
            panic!("Ambiguous pattern match");
        }

        if self.children.is_empty() {
            return self.action.identity().matches(other, patterns);
        }

        if other.children.is_empty() {
            return self.matches(&other.action.identity(), patterns);
        }

        let mut self_matched = vec![false; self.children.len()];
        let mut other_matched = vec![false; other.children.len()];

        let mut self_patterns = vec![];

        for (i, c1) in self.children.iter().enumerate() {
            if c1.is_pattern() {
                self_patterns.push(i);
                self_matched[i] = true;
                continue;
            }

            for (j, c2) in other.children.iter().enumerate() {
                if other_matched[j] {
                    continue;
                }
                if c1.matches(c2, patterns) {
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

        // for all patterns, determine all possible matches (i.e. all children of other that are not matched yet and are matched by the pattern)
        let mut possible_matches = vec![vec![]; self_patterns.len()];

        for (i, c) in other.children.iter().enumerate() {
            if other_matched[i] {
                continue;
            }
            for (j, p) in self_patterns.iter().enumerate() {
                let p = &self.children[*p];
                if let Action::Slot { matcher, .. } = p.action {
                    if matcher(c) {
                        if other_matched[i] {
                            patterns.clear();
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
            for (j, p) in self_patterns.iter().enumerate() {
                let p = &self.children[*p];
                if let Action::Segment { matcher, .. } = p.action {
                    if matcher(c) {
                        if other_matched[i] {
                            patterns.clear();
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
                if possible_matches[i].is_empty() {
                    return p.matches(&Expression::new_empty(other.action.clone()), patterns);
                }
                let j = possible_matches[i][0];
                if !p.matches(&other.children[j], patterns) {
                    return false;
                }
            } else if let Action::Segment { .. } = p.action {
                let matched = Expression::new(
                    possible_matches[i]
                        .iter()
                        .map(|&j| other.children[j].clone())
                        .collect(),
                    other.action.clone(),
                );

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
        if other.children.iter().any(|c| c.is_pattern()) {
            panic!("Ambiguous pattern match");
        }

        if self.children.len() != other.children.len() {
            return false;
        }

        for i in 0..self.children.len() {
            if !self.children[i].matches(&other.children[i], patterns) {
                patterns.clear();
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
            (_, Action::Slot { .. }) | (_, Action::Segment { .. }) => false,
            (
                Action::Slot {
                    name,
                    matcher,
                    predicate,
                },
                _,
            ) => {
                if let Some(expr) = patterns.get(name) {
                    return expr.clone().matches(other, patterns);
                }
                if !matcher(other) || !predicate(other) {
                    patterns.clear();
                    return false;
                }
                patterns.insert(name.clone(), other.clone());
                true
            }
            (
                Action::Segment {
                    name, predicate, ..
                },
                _,
            ) => {
                if let Some(expr) = patterns.get(name) {
                    return expr.clone().matches(other, patterns);
                }
                if !predicate(other) {
                    patterns.clear();
                    return false;
                }
                patterns.insert(name.clone(), other.clone());
                true
            }
            (Action::Add, Action::Add) => self.match_children_unordered(other, patterns),
            (Action::Mul, Action::Mul) => self.match_children_unordered(other, patterns),
            (a, b) if a == b => self.match_children_ordered(other, patterns),
            (Action::Add, _) if self.children.len() > 1 => {
                self.matches(&Expression::new(vec![other.clone()], Action::Add), patterns)
            }
            (Action::Mul, _) if self.children.len() > 1 => {
                self.matches(&Expression::new(vec![other.clone()], Action::Mul), patterns)
            }
            (Action::Pow, _) => {
                if self.children.iter().any(Expression::is_number) {
                    return false;
                }
                self.matches(
                    &Expression::new_binary(
                        other.clone(),
                        Expression::create_value(action::Number::Int(1)),
                        Action::Pow,
                    ),
                    patterns,
                )
            }
            _ => false,
        }
    }

    pub(crate) fn substitute_pattern(&self, patterns: &HashMap<String, Expression>) -> Self {
        match &self.action {
            Action::Segment { name, .. } | Action::Slot { name, .. } => {
                if let Some(expr) = patterns.get(name) {
                    expr.clone()
                } else {
                    Expression::create_error(format!("Pattern {} not found", name))
                }
            }
            Action::Map { map, .. } => {
                map(&self.children[0].substitute_pattern(patterns), patterns)
            }
            _ => {
                let mut children = vec![];
                for c in &self.children {
                    if let Action::Err(_) = c.action {
                        return c.clone();
                    }
                    if let Action::Segment { .. } = c.action {
                        children.extend(c.substitute_pattern(patterns).children);
                    } else {
                        children.push(c.substitute_pattern(patterns));
                    }
                }
                Expression::new(children, self.action.clone())
            }
        }
    }
}

/// Applying rules to expressions
impl Expression {
    pub fn apply_ruleset(&self, rules: &[&dyn Rule], print: bool) -> Expression {
        let mut expr = self.clone();
        loop {
            let mut applied = false;
            for rule in rules {
                if let Some(new_expr) = expr.apply_rule(*rule) {
                    if print {
                        println!("{}: {} = {}", rule.name(), expr, new_expr);
                    }
                    expr = new_expr;
                    applied = true;
                    break;
                }
            }
            if !applied {
                break;
            }
        }
        expr
    }

    pub(crate) fn apply_rule(&self, rule: &dyn Rule) -> Option<Expression> {
        if let Some(e) = rule.apply(self) {
            if e == *self {
                panic!("Rule {} is not simplifying expression", rule.name());
            }
            Some(e)
        } else {
            for (i, c) in self.children.iter().enumerate() {
                if let Some(e) = c.apply_rule(rule) {
                    let mut children = self.children.clone();
                    children[i] = e;
                    return Some(Expression::new(children, self.action.clone()));
                }
            }
            None
        }
    }

    #[allow(dead_code)]
    pub(crate) fn apply_rule_unchecked(&self, rule: &dyn Rule) -> Option<Expression> {
        if let Some(e) = rule.apply(self) {
            Some(e)
        } else {
            for (i, c) in self.children.iter().enumerate() {
                if let Some(e) = c.apply_rule_unchecked(rule) {
                    let mut children = self.children.clone();
                    children[i] = e;
                    return Some(Expression::new(children, self.action.clone()));
                }
            }
            None
        }
    }
}

/// Applying functions to expressions
impl Expression {
    pub fn reduce(&self, _: &HashMap<String, Expression>, action: Action) -> Expression {
        if let Action::Add | Action::Mul = action {
            assert_eq!(self.action, action);
            let mut res = if let Action::Num { value } = action.identity().action {
                value
            } else {
                unreachable!()
            };
            for c in &self.children {
                assert!(c.is_number());
                if let Action::Num { value } = &c.action {
                    res = match action {
                        Action::Add => res + *value,
                        Action::Mul => res * *value,
                        _ => unreachable!(),
                    };
                } else {
                    unreachable!()
                }
            }
            Expression::create_value(res)
        } else if let Action::Pow = action {
            assert_eq!(self.action, action);
            assert!(self.children.len() == 2);
            assert!(self.children[1].is_integer());
            assert!(!self.children[0].is_zero());
            assert!(!self.children[0].is_one());

            let (negative, pow) = if let Action::Num {
                value: Number::Int(i),
            } = &self.children[1].action
            {
                (*i < 0, i.unsigned_abs() as usize)
            } else {
                unreachable!()
            };

            if let Action::Num { value } = &self.children[0].action {
                let mut res = *value;
                for _ in 1..pow {
                    res = res * *value;
                }
                if negative {
                    res = res.inverse();
                }
                return Expression::create_value(res);
            }

            assert!(!negative);
            Expression::new(
                std::iter::repeat(self.children[0].clone())
                    .take(pow)
                    .collect(),
                Action::Mul,
            )
        } else {
            panic!("Cannot reduce {:?}", action);
        }
    }

    pub fn distribute(
        &self,
        patterns: &HashMap<String, Expression>,
        expr: Expression,
        action: Action,
    ) -> Expression {
        let expr = expr.substitute_pattern(patterns);
        if let Action::Mul = action {
            assert_eq!(self.action, Action::Add);
            let mut res = vec![];
            if expr.children.len() == 1 {
                for c in &self.children {
                    res.push(c.clone() * expr.children[0].clone());
                }
            } else {
                for c in &self.children {
                    res.push(c.clone() * expr.clone());
                }
            }
            Expression::new(res, Action::Add)
        } else if let Action::Pow = action {
            assert_eq!(self.action, Action::Mul);
            Expression::new(
                self.children
                    .iter()
                    .map(|c| c.clone() ^ expr.clone())
                    .collect(),
                Action::Mul,
            )
        } else {
            panic!("Cannot distribute {:?}", action);
        }
    }
}

impl From<i64> for Expression {
    fn from(i: i64) -> Self {
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
        Self::from_str(s).unwrap_or_else(Self::create_error)
    }
}

impl From<String> for Expression {
    fn from(s: String) -> Self {
        Self::from(s.as_str())
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

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.matches(other, &mut HashMap::new())
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
        } else if self.children.is_empty() {
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

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.children.is_empty() {
            write!(f, "{:?}", self.action)
        } else {
            write!(f, "{:?}(", self.action)?;
            for i in 0..self.children.len() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{:?}", self.children[i])?;
            }
            write!(f, ")")
        }
    }
}
