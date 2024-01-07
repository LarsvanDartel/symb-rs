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
        
        // remove records from the tail where a and b match
        let mut i = 0;
        while i < a_records.len() && i < b_records.len() {
            if a_records[a_records.len() - i - 1].old != b_records[b_records.len() - i - 1].old {
                break;
            }
            i += 1;
        }
        let na = a_records.len() - i;
        let nb = b_records.len() - i;

        let mut records = Vec::new();

        records.extend(a_records.into_iter().take(na));
        records.extend(b_records.into_iter().take(nb).map(|record| RewriteRecord {
            old: record.old,
            new: record.new,
            message: record.message,
        }).rev());

        Some(records)
    }
}