extern crate expr;

mod rewrite;
mod rules;

use expr::Expression;
use std::io::Write;

use crate::{
    rewrite::{Rewriter, SimpleRewriter},
    rules::apply_rules,
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
        let mut expr = Expression::from(input);

        if expr.is_equality() {
            let mut lhs = expr.get_lhs().unwrap().clone();
            apply_rules(&rules::CLEANUP_RULES, &mut lhs, false, false);
            let mut rhs = expr.get_rhs().unwrap().clone();
            apply_rules(&rules::CLEANUP_RULES, &mut rhs, false, false);
            println!("Received: {} = {}", lhs, rhs);
            let records = if let Some(records) = SimpleRewriter::rewrite(&lhs, &rhs, &rules::FULL_EXPAND_RULES) {
                records
            } else {
                println!("Could not rewrite");
                continue;
            };
            if records.is_empty() {
                continue;
            }
            println!("Rewrite records:");
            let lmax = records
                .iter()
                .map(|record| record.old.to_string().len())
                .max()
                .unwrap();
            let rmax = records
                .iter()
                .map(|record| record.new.to_string().len())
                .max()
                .unwrap();
            for record in records {
                println!(
                    "=> {:>lmax$} = {:>rmax$}   ({})",
                    record.old.to_string(),
                    record.new.to_string(),
                    record.message,
                    lmax = lmax,
                    rmax = rmax
                );
            }
        } else {
            apply_rules(&rules::CLEANUP_RULES, &mut expr, false, false);
            println!("Received: {}", expr);
            //println!("Received: {:?}", expr);

            apply_rules(&rules::FULL_EXPAND_RULES, &mut expr, true, true);
            println!("Result: {}", expr);
            //println!("Result: {:?}", expr);
        }
    }
}
