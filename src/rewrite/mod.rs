extern crate expr;

mod simple;

pub use self::simple::SimpleRewriter;

use expr::{Expression, Rule};

pub struct RewriteRecord {
    pub message: String,
    pub old: Expression,
    pub new: Expression,
}

pub trait Rewriter {
    /// Rewrite expression `a` to expression `b`.
    /// Returns a vector of rewrite records that can be used to undo the rewrite,
    /// with `result[0].old == a` and `result[result.len() - 1].new == b`.
    /// If the rewrite is not possible, returns `None`.
    fn rewrite(a: &Expression, b: &Expression, rules: &[Rule]) -> Option<Vec<RewriteRecord>>;
}