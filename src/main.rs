extern crate expr;
extern crate expr_macro;

//use std::{io::Write, str::FromStr};
//use expr::Expression;

use std::collections::HashMap;

use expr_macro::expr;

fn main() {
    //loop {
    //    print!("> ");
    //    std::io::stdout().flush().unwrap();
    //    let mut input = String::new();
    //    std::io::stdin().read_line(&mut input).unwrap();
    //    if input.trim() == "exit" {
    //        break;
    //    }
    //    let expr = Expression::from_str(&input).unwrap();
    //    println!("{}", expr);
    //    println!("{:?}", expr);
    //}

    let a = expr!(+(~~x:(positive), ~~y:(nonpositive)));
    let b = expr!(+(1, -2, "0.5", "-1/2", "1/-2", "-1/-2", 0, 0.0, 1.0, -1.0));

    let mut map = HashMap::new();
    let matches = a.matches(&b, &mut map);
    println!("{:?}", matches);
    for (k, v) in map {
        println!("{} -> {}", k, v);
    }
}
