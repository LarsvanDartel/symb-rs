extern crate expr;

mod rules;
mod rewrite;

use expr::Expression;
use rules::Rules;
use std::io::Write;

fn main() {
    let cleanup_ruleset = Rules::Cleanup.create_ruleset();
    let expand_ruleset = Rules::FullExpand.create_ruleset();
    let finalize_ruleset = Rules::Finalize.create_ruleset();

    loop {
        print!("calc > ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }
        let expr = Expression::from(input);
        let expr = cleanup_ruleset.apply(expr, false);
        println!("Received: {}", expr);
        //println!("Received: {:?}", expr);

        let expr = expand_ruleset.apply_finalize(expr, &finalize_ruleset, true);
        println!("Result: {}", expr);
        //println!("Result: {:?}", expr);
    }
}
