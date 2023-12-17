extern crate expr;
extern crate expr_macro;

use expr::rule::Rule;
use expr::Expression;
use std::io::Write;

use expr_macro::rule;

fn main() {
    let cleanup_rules: &[Box<dyn Rule>] = &[
        rule!("associativity addition", +(+(~~a), ~~b) => +(~~a, ~~b)),
        rule!("associativity multiplication", *(*(~~a), ~~b) => *(~~a, ~~b)),
    ];

    let expand_rules: &[Box<dyn Rule>] = &[
        rule!("associativity addition", +(+(~~a), ~~b) => +(~~a, ~~b)),
        rule!("collapse addition", +(~a) => ~a),
        rule!("numeric addition", +(~~a:is_number:can_combine, ~~b:!is_number) => +(reduce(~~a, +), ~~b)),
        rule!("identity addition", +(~~a:!is_zero:!is_empty, ~~b:is_zero:!is_empty) => ~~a),
        rule!("identity minus", -(~a, ~b) => +(~a, *(-1, ~b))),
        rule!("associativity multiplication", *(*(~~a), ~~b) => *(~~a, ~~b)),
        rule!("collapse multiplication", *(~a) => ~a),
        rule!("numeric multiplication", *(~~a:is_number:can_combine, ~~b:!is_number) => *(reduce(~~a, *), ~~b)),
        rule!("identity multiplication", *(~~a:!is_one:!is_empty, ~~b:is_one:!is_empty) => ~~a),
        rule!("absorber multiplication", *(~~a::!is_empty, 0) => 0),
        rule!("distributivity multiplication", *(~~a::!is_empty, +(~~b)) => distribute(~~b, ~~a, *)),
        rule!("identity divide", /(~a, 1) => ~a),
        rule!("identity divide", /(~a, ~b) => *(~a, ^(~b, -1))),
        rule!("numeric power", ^(~a:(is_number,!is_zero,!is_one), ~b:(is_integer,is_nonnegative,!is_one)) => reduce(^(~a, ~b), ^)),
        rule!("power expansion", ^(+(~~a), ~b:(is_integer,is_nonnegative,!is_one)) => reduce(^(+(~~a), ~b), ^)),
        rule!("identity power", ^(~a, 1) => ~a),
        rule!("absorber power", ^(~a:!is_zero, 0) => 1),
        rule!("absorber power", ^(1, ~a) => 1),
        rule!("absorber power", ^(0, ~a::is_positive) => 0),
        rule!("distributivity power", ^(*(~~a), ~b:(is_integer,!is_one,!is_zero)) => distribute(~~a, ~b, ^)),
    ];

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }
        let expr = Expression::from(input);
        println!("Received: {}", expr);
        let expr = expr.apply_ruleset(cleanup_rules, false);
        println!("Parsed: {}", expr);

        let expr = expr.apply_ruleset(expand_rules, true);
        println!("Result: {}", expr);
        println!("Result: {:?}", expr);
    }
}
