mod simple;
mod bfsrewriter;

pub use simple::SimpleRewriter;
pub use bfsrewriter::{SingleBfsRewriter, DoubleBfsRewriter};

use expr::{Expression, Rule};

#[derive(Clone)]
pub struct RewriteRecord {
    pub message: String,
    pub old: Expression,
    pub new: Expression,
}

impl RewriteRecord {
    pub fn reverse(&self) -> RewriteRecord {
        RewriteRecord {
            message: self.message.clone(),
            old: self.new.clone(),
            new: self.old.clone(),
        }
    }

    pub fn print_records(mut records: Vec<RewriteRecord>) {
        for record in records.iter_mut() {
            record.message = format!("({})", record.message);
        }
        let max = records.iter().map(|record| record.message.len()).max().unwrap();
        for record in records {
            println!(
                "{:<width$} => {} = {}",
                record.message,
                record.old.to_string(),
                record.new.to_string(),
                width = max
            );
        }
    }
}

pub trait Rewriter {
    /// Rewrite expression `a` to expression `b`.
    /// Returns a vector of rewrite records that can be used to undo the rewrite,
    /// with `result[0].old == a` and `result[result.len() - 1].new == b`.
    /// If the rewrite is not possible, returns `None`.
    fn rewrite(a: &Expression, b: &Expression, rules: &[Rule]) -> Option<Vec<RewriteRecord>>;
}