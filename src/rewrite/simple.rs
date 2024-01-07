extern crate expr;

use crate::rules::apply_rules;

use super::{Rewriter, RewriteRecord};
use expr::{Expression, Rule};
pub struct SimpleRewriter;

impl Rewriter for SimpleRewriter {
    fn rewrite<'a>(a: &Expression, b: &Expression, rules: &[Rule]) -> Option<Vec<RewriteRecord>> {
        let mut a = a.clone();
        let mut b = b.clone();

        let a_records = apply_rules(rules, &mut a, true, false);
        let b_records = apply_rules(rules, &mut b, true, false);

        if a != b {
            return None;
        }

        let mut records = Vec::new();
        records.extend(a_records);
        records.extend(b_records.into_iter().map(|record| RewriteRecord {
            message: record.message,
            old: record.new,
            new: record.old,
        }).rev());

        Some(records)
    }
}