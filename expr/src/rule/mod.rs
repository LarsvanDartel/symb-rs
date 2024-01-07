use std::collections::HashMap;

use crate::Expression;

pub trait Rule {
    fn apply(&self, expr: &Expression) -> Option<Expression>;
    fn counts(&self) -> bool {
        true
    }
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
    predicate: Option<Expression>,
    show: bool,
}

impl MatchRule {
    pub fn new<T: ToString>(
        name: T,
        pattern: Expression,
        replacement: Expression,
        predicate: Option<Expression>,
        show: bool,
    ) -> Self {
        Self {
            name: name.to_string(),
            pattern,
            replacement,
            predicate,
            show,
        }
    }
}

impl Rule for MatchRule {
    fn apply(&self, expr: &Expression) -> Option<Expression> {
        let mut patterns = HashMap::new();
        if expr.is_error() {
            return None;
        }
        if self.pattern.matches(expr, &mut patterns) {
            if let Some(predicate) = &self.predicate {
                if !predicate.matches_final(&patterns) {
                    return None;
                }
            }
            Some(self.replacement.substitute_pattern(&patterns))
        } else {
            //if !patterns.is_empty() {
            //    println!("{:?}", self);
            //    println!("{:?}", expr);
            //    for (key, value) in patterns {
            //        println!("{} => {}", key, value);
            //    }
            //    panic!("Pattern matching failed, but patterns were found")
            //}
            None
        }
    }

    fn counts(&self) -> bool {
        self.show
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
