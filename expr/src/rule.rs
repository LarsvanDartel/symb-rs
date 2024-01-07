use std::collections::HashMap;

use crate::Expression;

#[derive(Clone)]
pub struct Rule {
    name: String,
    pattern: Expression,
    replacement: Expression,
    predicate: Option<Expression>,
    show: bool,
}

impl Rule {
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

    pub fn apply(&self, expr: &Expression) -> Option<Expression> {
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

    pub fn counts(&self) -> bool {
        self.show
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl std::fmt::Debug for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {} => {}", self.name, self.pattern, self.replacement)
    }
}
