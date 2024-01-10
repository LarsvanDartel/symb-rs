use std::collections::{HashMap, VecDeque, HashSet};

use expr::{Expression, Rule};

use crate::rules::apply_rule;

use super::{RewriteRecord, Rewriter, SimpleRewriter};

pub struct SingleBfsRewriter;

impl Rewriter for SingleBfsRewriter {
    fn rewrite(a: &Expression, b: &Expression, rules: &[Rule]) -> Option<Vec<RewriteRecord>> {
        let bound = 10;
        let mut seen = HashSet::new();
        let mut prev = HashMap::new();
        let mut queue = VecDeque::new();

        queue.push_back((a.clone(), 0));

        while let Some((mut expr, depth)) = queue.pop_front() {
            if depth > bound {
                continue;
            }
            if seen.contains(&expr) {
                continue;
            }
            seen.insert(expr.clone());
            if &expr == b {
                break;
            }
            for rule in rules {
                if let Some(record) = apply_rule(rule, &mut expr, true) {
                    let new_expr = record.new.clone();
                    if !prev.contains_key(&new_expr) {
                        prev.insert(
                            new_expr.clone(),
                            (expr.clone(), rule.name().to_string()),
                        );
                    }
                    queue.push_back((new_expr, depth + 1));
                }
            }
        }

        let mut records = Vec::new();
        let mut expr = b;
        if !prev.contains_key(expr) {
            return None;
        }
        while expr != a {
            let (prev_expr, rule_name) = prev.get(expr).unwrap();
            records.push(RewriteRecord {
                message: rule_name.clone(),
                old: prev_expr.clone(),
                new: expr.clone(),
            });
            expr = prev_expr;
        }
        records.reverse();
        Some(records)
    }
}

pub struct DoubleBfsRewriter;

impl Rewriter for DoubleBfsRewriter {
    fn rewrite(a: &Expression, b: &Expression, rules: &[Rule]) -> Option<Vec<RewriteRecord>> {
        let mut bound = SimpleRewriter::rewrite(a, b, rules)?.len();

        let mut a_seen = HashMap::new();
        let mut b_seen = HashMap::new();

        let mut path = None;

        let mut prev = HashMap::new();

        let mut queue = VecDeque::new();

        queue.push_back((a.clone(), 0, true));
        queue.push_back((b.clone(), 0, false));

        while let Some((mut expr, n_rules, is_a)) = queue.pop_front() {
            if n_rules > bound {
                continue;
            }
            let (seen, other_seen) = if is_a {
                (&mut a_seen, &b_seen)
            } else {
                (&mut b_seen, &a_seen)
            };
            if seen.contains_key(&expr) {
                continue;
            }
            seen.insert(expr.clone(), n_rules);
            if let Some(other_rules) = other_seen.get(&expr) {
                if n_rules + other_rules <= bound {
                    bound = n_rules + other_rules;
                    path = Some(expr.clone());
                }
            }
            for rule in rules {
                if let Some(record) = apply_rule(rule, &mut expr, true) {
                    let new_expr = record.new.clone();
                    if !prev.contains_key(&(record.new, is_a)) {
                        prev.insert(
                            (new_expr.clone(), is_a),
                            (expr.clone(), rule.name().to_string()),
                        );
                    }
                    queue.push_back((new_expr, n_rules + 1, is_a));
                }
            }
        }

        let intermidiate = path.expect("no path found");

        let mut records = Vec::new();
        let mut expr = &intermidiate;
        while expr != a {
            let (prev_expr, rule_name) = prev.get(&(expr.clone(), true)).unwrap();
            records.push(RewriteRecord {
                message: rule_name.clone(),
                old: prev_expr.clone(),
                new: expr.clone(),
            });
            expr = prev_expr;
        }
        records.reverse();
        let mut expr = &intermidiate;
        while expr != b {
            let (prev_expr, rule_name) = prev.get(&(expr.clone(), false)).unwrap();
            records.push(RewriteRecord {
                message: rule_name.clone(),
                old: expr.clone(),
                new: prev_expr.clone(),
            });
            expr = prev_expr;
        }
        Some(records)
    }
}
