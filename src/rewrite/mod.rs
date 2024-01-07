extern crate expr;

use expr::Expression;

pub trait Rewriter {
    /// Rewrite expression `a` to expression `b`.
    /// Returns a vector of expressions that are equivalent to `a` and `b`,
    /// with `result[0] = a` and `result[result.len() - 1] = b`.
    fn rewrite(a: &Expression, b: &Expression) -> Vec<Expression>;
}