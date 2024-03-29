mod action;
pub mod literals;
mod parser;

use crate::Rule;
pub use action::{Action, Constant, Function, Map, Number, Predicate, PredicateType};
use literals::{to_subscript, to_superscript};
use parser::Parser;
use std::{
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    str::FromStr,
};

#[derive(Clone)]
pub struct Expression {
    children: Vec<Expression>,
    action: Action,
}

/// Creating new expressions
impl Expression {
    pub fn new(children: Vec<Expression>, action: Action) -> Self {
        if let Some(arity) = action.arity() {
            if children.len() < arity.0 || children.len() > arity.1 {
                panic!(
                    "Wrong number of arguments for {:?}: expected ({}, {}), got {}",
                    action,
                    arity.0,
                    arity.1,
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

/// Utility functions
impl Expression {
    pub(crate) fn split(&self, action: &Action) -> (Option<Expression>, Option<Expression>) {
        if let Action::Add = action {
            if let Action::Mul = self.action {
                let mut p1 = vec![];
                let mut p2 = vec![];
                for c in &self.children {
                    if let Action::Num { .. } = c.action {
                        p2.push(c.clone());
                    } else {
                        p1.push(c.clone());
                    }
                }
                let p1 = if p1.is_empty() {
                    None
                } else if p1.len() == 1 {
                    Some(p1[0].clone())
                } else {
                    Some(Expression::new(p1, Action::Mul))
                };
                let p2 = if p2.is_empty() {
                    None
                } else if p2.len() == 1 {
                    Some(p2[0].clone())
                } else {
                    Some(Expression::new(p2, Action::Mul))
                };
                (p1, p2)
            } else if let Action::Num { .. } = self.action {
                (None, Some(self.clone()))
            } else {
                (Some(self.clone()), None)
            }
        } else if let Action::Mul = action {
            if let Action::Pow = self.action {
                (
                    Some(self.children[0].clone()),
                    Some(self.children[1].clone()),
                )
            } else {
                if let Action::Num { .. } = self.action {
                    (None, Some(self.clone()))
                } else {
                    (Some(self.clone()), None)
                }
            }
        } else {
            panic!("Invalid action {:?}", action);
        }
    }

    pub(crate) fn count_variables(&self) -> [usize; 26] {
        if let Action::Var { name } = &self.action {
            let mut res = [0; 26];
            res[name.chars().next().unwrap() as usize - 'a' as usize] = 1;
            res
        } else if let Action::Pow = self.action {
            if let Action::Num {
                value: Number::Int(i),
            } = self.children[1].action
            {
                if i < 0 {
                    return [0; 26];
                }
                let mut res = self.children[0].count_variables();
                for c in &mut res {
                    *c *= i as usize;
                }
                res
            } else {
                return [0; 26];
            }
        } else if let Action::Mul = self.action {
            let mut res = [0; 26];
            for c in &self.children {
                let p = c.count_variables();
                for (i, c) in p.iter().enumerate() {
                    res[i] += c;
                }
            }
            res
        } else {
            [0; 26]
        }
    }

    pub(crate) fn has_variable(&self, var: &String) -> bool {
        if let Action::Var { name } = &self.action {
            return name == var;
        }
        self.children.iter().any(|c| c.has_variable(var))
    }

    pub fn is_error(&self) -> bool {
        matches!(self.action, Action::Err(_))
    }

    pub fn is_equality(&self) -> bool {
        matches!(self.action, Action::Equals)
    }

    pub fn get_lhs(&self) -> Option<&Expression> {
        if let Action::Equals = self.action {
            Some(&self.children[0])
        } else {
            None
        }
    }

    pub fn get_rhs(&self) -> Option<&Expression> {
        if let Action::Equals = self.action {
            Some(&self.children[1])
        } else {
            None
        }
    }
}

/// Matching expressions
impl Expression {
    fn is_pattern(&self) -> bool {
        matches!(self.action, Action::Slot { .. } | Action::Segment { .. })
    }

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
                if let Action::Slot { predicate, .. } = &p.action {
                    if !predicate.matches(c) {
                        continue;
                    }
                    if other_matched[i] {
                        return false;
                    }
                    possible_matches[j].push(i);
                    other_matched[i] = true;
                }
            }
            if other_matched[i] {
                continue;
            }
            for (j, p) in self_patterns.iter().enumerate() {
                let p = &self.children[*p];
                if let Action::Segment { predicate, .. } = &p.action {
                    if !predicate.matches(c) {
                        continue;
                    }
                    if other_matched[i] {
                        return false;
                    }
                    possible_matches[j].push(i);
                    other_matched[i] = true;
                }
            }
        }

        if !other_matched.iter().all(|&b| b) {
            return false;
        }

        for (i, p) in self_patterns.iter().enumerate() {
            let p = &self.children[*p];
            let matched = if let Action::Slot { .. } = p.action {
                if possible_matches[i].len() > 1 {
                    return false;
                }
                possible_matches[i]
                    .first()
                    .map(|&j| other.children[j].clone())
                    .unwrap_or(other.action.identity())
            } else if let Action::Segment { .. } = p.action {
                Expression::new(
                    possible_matches[i]
                        .iter()
                        .map(|&j| other.children[j].clone())
                        .collect(),
                    other.action.clone(),
                )
            } else {
                unreachable!()
            };
            if !p.matches(&matched, patterns) {
                return false;
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
                return false;
            }
        }

        true
    }

    pub(crate) fn matches(&self, other: &Self, patterns: &mut HashMap<String, Expression>) -> bool {
        match (&self.action, &other.action) {
            (
                Action::Segment { .. } | Action::Slot { .. },
                Action::Segment { .. } | Action::Slot { .. },
            ) => panic!("Ambiguous pattern match"),
            (_, Action::Slot { .. }) | (_, Action::Segment { .. }) => false,
            (Action::Slot { name, predicate }, _) => {
                if let Some(expr) = patterns.get(name) {
                    expr.clone().matches(other, patterns)
                } else if !predicate.matches(other) {
                    false
                } else {
                    patterns.insert(name.clone(), other.clone());
                    true
                }
            }
            (Action::Segment { name, min_size, .. }, _) => {
                if let Some(expr) = patterns.get(name) {
                    expr.clone().matches(other, patterns)
                } else if other.children.len() < *min_size {
                    false
                } else {
                    patterns.insert(name.clone(), other.clone());
                    true
                }
            }
            (Action::Fun(Function::Max), Action::Fun(Function::Max)) => {
                self.match_children_unordered(other, patterns)
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
                if self
                    .children
                    .iter()
                    .any(|c| matches!(c.action, Action::Num { .. }))
                {
                    false
                } else {
                    self.matches(
                        &Expression::new_binary(
                            other.clone(),
                            Expression::create_value(action::Number::Int(1)),
                            Action::Pow,
                        ),
                        patterns,
                    )
                }
            }
            _ => false,
        }
    }

    pub(crate) fn matches_final(&self, patterns: &HashMap<String, Expression>) -> bool {
        let expr = self.substitute_pattern(patterns);
        expr.eval_predicates()
    }

    fn eval_predicates(&self) -> bool {
        match &self.action {
            Action::And => self.children.iter().all(|c| c.eval_predicates()),
            Action::Predicate((predicate, positive)) => predicate.eval(&self.children) == *positive,
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
            Action::Map { map, .. } => map.map(self.children[0].substitute_pattern(patterns)),
            _ => {
                let mut children = vec![];
                for c in &self.children {
                    let mut c = c.clone();
                    if c.action == Action::Fun(Function::D) {
                        if let Action::Slot { name: n0, .. } = &c.children[0].action {
                            if let Action::Slot { name: n1, .. } = &c.children[1].action {
                                if patterns.get(n0) == patterns.get(n1) {
                                    c = Expression::create_value(1);
                                }
                            }
                        }
                    } else if c.action == Action::Div {
                        if let Action::Slot { name, .. } = &c.children[1].action {
                            if matches!(
                                patterns.get(name).map(|e| &e.action),
                                Some(Action::Num {
                                    value: Number::Int(1)
                                })
                            ) {
                                c = c.children[0].clone();
                            }
                        }
                    }
                    if matches!(self.action, Action::Add | Action::Mul) {
                        let expr = if let Action::Slot { name, .. } | Action::Segment { name, .. } =
                            &c.action
                        {
                            if let Some(expr) = patterns.get(name) {
                                expr
                            } else {
                                &c
                            }
                        } else {
                            &c
                        };
                        if expr == &self.action.identity() {
                            continue;
                        }
                    }
                    if let Action::Err(_) = c.action {
                        return c.clone();
                    }
                    if let Action::Segment { .. } = c.action {
                        let c = c.substitute_pattern(patterns);
                        if c.action == self.action {
                            children.extend(c.children);
                        } else {
                            children.push(c);
                        }
                    } else {
                        children.push(c.substitute_pattern(patterns));
                    }
                }
                if let Action::Add | Action::Mul = self.action {
                    if children.is_empty() {
                        return self.action.identity();
                    }
                    if children.len() == 1 {
                        return children.pop().unwrap();
                    }
                }
                Expression::new(children, self.action.clone())
            }
        }
    }
}

/// Applying rules to expressions
impl Expression {
    pub fn apply_rule(&self, rule: &Rule) -> Option<Expression> {
        if let Some(e) = rule.apply(self) {
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
        if let (Action::Add, Action::Add) | (Action::Mul, Action::Mul) =
            (&self.action, &other.action)
        {
            let mut matched = vec![false; other.children.len()];
            for c1 in &self.children {
                let mut found = false;
                for (i, c2) in other.children.iter().enumerate() {
                    if matched[i] {
                        continue;
                    }
                    if c1 == c2 {
                        matched[i] = true;
                        found = true;
                        break;
                    }
                }
                if !found {
                    return false;
                }
            }
            return matched.iter().all(|&b| b);
        }

        if self.action != other.action {
            return false;
        }

        if self.children.len() != other.children.len() {
            return false;
        }

        for i in 0..self.children.len() {
            if self.children[i] != other.children[i] {
                return false;
            }
        }
        true
    }
}

impl Eq for Expression {}

impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.action.hash(state);
        if let Action::Add | Action::Mul = self.action {
            let mut children = self.children.clone();
            // xor the hashes of the children to make the hash order-independent
            let mut hash = 0;
            for c in &mut children {
                let mut h = DefaultHasher::new();
                c.hash(&mut h);
                hash ^= h.finish();
            }
            hash.hash(state);
        } else {
            for c in &self.children {
                c.hash(state);
            }
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Action::Fun(fun) = &self.action {
            let mut ignore = false;
            if let Function::Root = fun {
                if let Action::Num {
                    value: Number::Int(i),
                } = &self.children[0].action
                {
                    f.write_str(&to_superscript(i.to_string()))?;
                    f.write_str(&self.action.to_string())?;
                    ignore = true;
                }
            } else if let Function::Log = fun {
                if let Action::Num {
                    value: Number::Int(i),
                } = &self.children[0].action
                {
                    f.write_str(&self.action.to_string())?;
                    f.write_str(&to_subscript(i.to_string()))?;
                    ignore = true;
                }
            }
            if !ignore {
                f.write_str(&self.action.to_string())?;
            }
            f.write_str("(")?;
            for i in 0..self.children.len() {
                if ignore && i == 0 {
                    continue;
                }
                if i > 0 && !ignore {
                    f.write_str(",")?;
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
