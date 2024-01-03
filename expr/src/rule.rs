use std::collections::HashMap;

use crate::Expression;

pub trait Rule {
    fn apply(&self, expr: &Expression) -> Option<Expression>;
    fn name(&self) -> &str {
        "Unnamed rule"
    }
}

impl std::fmt::Debug for dyn Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

pub struct MatchRule {
    name: String,
    pattern: Expression,
    replacement: Expression,
}

impl MatchRule {
    pub fn new<T: ToString>(name: T, pattern: Expression, replacement: Expression) -> Self {
        Self {
            name: name.to_string(),
            pattern,
            replacement,
        }
    }
}

impl Rule for MatchRule {
    fn apply(&self, expr: &Expression) -> Option<Expression> {
        let mut patterns = HashMap::new();
        if self.pattern.matches(expr, &mut patterns) {
            Some(self.replacement.substitute_pattern(&patterns))
        } else {
            if !patterns.is_empty() {
                println!("{:?}", self);
                println!("{:?}", expr);
                for (key, value) in patterns {
                    println!("{} => {}", key, value);
                }
                panic!("Pattern matching failed, but patterns were found")
            }
            None
        }
    }

    fn name(&self) -> &str {
        &self.name
    }
}

impl std::fmt::Debug for MatchRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {} => {}", self.name, self.pattern, self.replacement)
    }
}