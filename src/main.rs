extern crate expr;
extern crate expr_macro;

use std::{io::Write, str::FromStr};
use expr::Expression;

fn main() {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }
        let expr = Expression::from_str(&input).unwrap();
        println!("{}", expr);
        println!("{:?}", expr);
    }
}
