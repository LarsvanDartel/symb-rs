use expr::Expression;
use std::io::Write;

use symb::{
    rewrite::{DoubleBfsRewriter, Rewriter, SimpleRewriter, SingleBfsRewriter, RewriteRecord},
    rules::{apply_rules, CLEANUP_RULES, FULL_EXPAND_RULES},
};

fn main() {
    loop {
        print!("calc > ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }
        let simple = input.contains("simple");
        if simple {
            input = input.replace("simple", "");
        }
        let mut expr = Expression::from(input);

        apply_rules(&CLEANUP_RULES, &mut expr, false);
        println!("Received: {}", expr);
        //println!("Received: {:?}", expr);

        let records = if expr.is_equality() {
            let mut lhs = expr.get_lhs().unwrap().clone();
            apply_rules(&CLEANUP_RULES, &mut lhs, false);
            let mut rhs = expr.get_rhs().unwrap().clone();
            apply_rules(&CLEANUP_RULES, &mut rhs, false);
            let records = if let Some(records) = if simple {
                SimpleRewriter::rewrite(&lhs, &rhs, &FULL_EXPAND_RULES)
            } else {
                if let Some(records) =
                    SingleBfsRewriter::rewrite(&lhs, &rhs, &FULL_EXPAND_RULES)
                {
                    Some(records)
                } else if let Some(records) =
                    SingleBfsRewriter::rewrite(&rhs, &lhs, &FULL_EXPAND_RULES)
                {
                    Some(records)
                } else if let Some(records) =
                DoubleBfsRewriter::rewrite(&lhs, &rhs, &FULL_EXPAND_RULES)
                {
                    Some(records)
                } else {
                    None
                }
            } {
                records
            } else {
                println!("Could not rewrite");
                continue;
            };
            if records.is_empty() {
                continue;
            }
            records
        } else {
            apply_rules(&FULL_EXPAND_RULES, &mut expr, true)
        };

        RewriteRecord::print_records(records);
    }
}
