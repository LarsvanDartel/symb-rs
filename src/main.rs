extern crate expr;
extern crate expr_macro;

use expr_macro::{symb, expr};

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
    //    println!("{:?
    //    }", expr);
    //}

    symb!(x);
    let a = expr!(+(Sin(Pi), x));
    println!("{}", a);
    println!("{:?}", a);
}