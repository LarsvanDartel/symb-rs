extern crate expr;
extern crate expr_macro;

use expr::{Expression, Rule};
use std::io::Write;

use expr_macro::rule;

struct RuleSet(Vec<Box<dyn Rule>>);
impl expr::RuleSet for RuleSet {
    fn rules(&self) -> impl Iterator<Item = &dyn Rule> {
        self.0.iter().map(|r| r.as_ref())
    }
}

enum Rules {
    Cleanup,
    Form,
    FullExpand,
    Addition,
    Multiplication,
    Division,
    Powers,
    Derivative,
    Trigonometry,
}

impl Rules {
    fn create_rulesets(sets: &[Self]) -> RuleSet {
        RuleSet(
            sets.iter()
                .map(|s| s.create_ruleset())
                .flat_map(|s| s.0)
                .collect(),
        )
    }
    fn create_ruleset(&self) -> RuleSet {
        match self {
            Self::Cleanup => RuleSet(vec![
                rule!("associativity addition", +(+(~~a), ~~b) => +(~~a, ~~b), false),
                rule!("associativity multiplication", *(*(~~a), ~~b) => *(~~a, ~~b), false),
                rule!("create rational", /(~a:is_integer, ~b:(is_integer,!is_zero)) => create_rational(~a, ~b), false),
            ]),
            Self::Form => RuleSet(vec![
                rule!("identity minus", -(~a, ~b) => +(~a, *(-1, ~b))),
                rule!("identity divide", /(~a, ~b:(!is_zero,!is_one)) => *(~a, ^(~b, -1))),
            ]),
            Self::Addition => RuleSet(vec![
                rule!("associativity addition", +(+(~~a), ~~b) => +(~~a, ~~b), false),
                rule!("collapse addition", +(~a) => ~a, false),
                rule!("numeric addition", +(~~a:is_number:can_reduce, ~~b:!is_number) => +(reduce(~~a, +), ~~b)),
                rule!("identity addition", +(~~a:!is_zero:!is_empty, ~~b:is_zero:!is_empty) => ~~a),
                rule!("combine terms", +(~~a::can_combine) => combine(~~a, +)),
            ]),
            Self::Multiplication => RuleSet(vec![
                rule!("associativity multiplication", *(*(~~a), ~~b) => *(~~a, ~~b), false),
                rule!("collapse multiplication", *(~a) => ~a, false),
                rule!("numeric multiplication", *(~~a:is_number:can_reduce, ~~b:!is_number) => *(reduce(~~a, *), ~~b)),
                rule!("identity multiplication", *(~~a:!is_one:!is_empty, ~~b:is_one:!is_empty) => ~~a),
                rule!("absorber multiplication", *(~~a::!is_empty, 0) => 0),
                rule!("distributivity multiplication", *(~~a::!is_empty, +(~~b)) => distribute(~~b, ~~a, *)),
                rule!("combine multiplication", *(~~a::can_combine) => combine(~~a, *)),
            ]),
            Self::Division => RuleSet(vec![
                rule!("rational simplification", ~a:is_rational_reducible => rational_reduce(~a)),
                rule!("division by zero", /(~a, 0) => Error("undefined")),
                rule!("identity divide", /(~a, 1) => ~a),
                rule!("create rational", /(~a:is_integer, ~b:(is_integer,!is_zero)) => create_rational(~a, ~b)),
            ]),
            Self::Powers => RuleSet(vec![
                rule!("numeric power", ^(~a:(is_number,!is_zero,!is_one), ~b:(is_integer,!is_zero,!is_one)) => reduce(^(~a, ~b), ^)),
                rule!("power expansion", ^(+(~~a), ~b:(is_integer,is_positive,!is_one)) => reduce(^(+(~~a), ~b), ^)),
                rule!("identity power", ^(~a, 1) => ~a),
                rule!("absorber power", ^(~a:!is_zero, 0) => 1),
                rule!("absorber power", ^(1, ~a) => 1),
                rule!("absorber power", ^(0, ~a::is_positive) => 0),
                rule!("division by zero", ^(0, ~a::is_negative) => Error("undefined")),
                rule!("undeterminate form 0^0", ^(0, 0) => Error("undefined")),
                rule!("associativity power", ^(^(~a, ~b:!is_one), ~c:!is_one) => ^(~a, *(~b, ~c))),
                rule!("distributivity power", ^(*(~~a), ~b:(is_integer,!is_one,!is_zero)) => distribute(~~a, ~b, ^)),
                rule!("combine powers", ^(^(~a, ~b:(!is_one)), ~c) => ^(~a, *(~b, ~c))),
            ]),
            Self::Derivative => RuleSet(vec![
                rule!("indepence", ~a::is_derivative_independent => 0),
                rule!("linearity", D(*(~~a:is_value:!is_empty, ~~b:!is_value:!is_empty), ~x) => *(~~a, D(~~b, ~x))),
                rule!("identity", D(~x, ~x) => 1),
                rule!("natural power", D(^(~x, ~n:(is_integer,!is_zero,!is_one)), ~x) => *(~n, ^(~x, +(~n, -1)))),
                rule!("natural power", D(^(~y:!is_variable, ~n:(is_integer,!is_zero,!is_one)), ~x) => *(~n, ^(~y, +(~n, -1)), D(~y, ~x))),
                rule!("additivity", D(+(~~a), ~x) => reduce(D(+(~~a), ~x), D)),
                rule!("multiplicativity", D(*(~~a::can_reduce), ~x) => reduce(D(*(~~a), ~x), D)),

            ]),
            Self::Trigonometry => RuleSet(vec![
                // Trigonometry
                rule!("unit circle sin", Sin(0) => 0),
                rule!("unit circle sin", Sin(*(1/6, Pi)) => 1/2),
                rule!("unit circle sin", Sin(*(1/4, Pi)) => *(1/2, Sqrt(2))),
                rule!("unit circle sin", Sin(*(1/3, Pi)) => *(1/2, Sqrt(3))),
                rule!("unit circle sin", Sin(*(1/2, Pi)) => 1),
                rule!("unit circle sin", Sin(*(2/3, Pi)) => *(1/2, Sqrt(3))),
                rule!("unit circle sin", Sin(*(3/4, Pi)) => *(1/2, Sqrt(2))),
                rule!("unit circle sin", Sin(*(5/6, Pi)) => 1/2),
                rule!("unit circle sin", Sin(Pi) => 0),
                rule!("unit circle sin", Sin(*(7/6, Pi)) => -1/2),
                rule!("unit circle sin", Sin(*(5/4, Pi)) => *(-1/2, Sqrt(2))),
                rule!("unit circle sin", Sin(*(4/3, Pi)) => *(-1/2, Sqrt(3))),
                rule!("unit circle sin", Sin(*(3/2, Pi)) => -1),
                rule!("unit circle sin", Sin(*(5/3, Pi)) => *(-1/2, Sqrt(3))),
                rule!("unit circle sin", Sin(*(7/4, Pi)) => *(-1/2, Sqrt(2))),
                rule!("unit circle sin", Sin(*(11/6, Pi)) => -1/2),
                rule!("periodicity sin", Sin(+(*(~a:(is_even,!is_zero), Pi), ~~b)) => Sin(+(~~b))),
                rule!("periodicity sin", Sin(+(*(~a:(is_odd,!is_one), Pi), ~~b)) => Sin(+(Pi, ~~b))),
                rule!("unit circle cos", Cos(0) => 1),
                rule!("unit circle cos", Cos(*(1/6, Pi)) => *(1/2, Sqrt(3))),
                rule!("unit circle cos", Cos(*(1/4, Pi)) => *(1/2, Sqrt(2))),
                rule!("unit circle cos", Cos(*(1/3, Pi)) => 1/2),
                rule!("unit circle cos", Cos(*(1/2, Pi)) => 0),
                rule!("unit circle cos", Cos(*(2/3, Pi)) => -1/2),
                rule!("unit circle cos", Cos(*(3/4, Pi)) => *(-1/2, Sqrt(2))),
                rule!("unit circle cos", Cos(*(5/6, Pi)) => *(-1/2, Sqrt(3))),
                rule!("unit circle cos", Cos(Pi) => -1),
                rule!("unit circle cos", Cos(*(7/6, Pi)) => *(-1/2, Sqrt(3))),
                rule!("unit circle cos", Cos(*(5/4, Pi)) => *(-1/2, Sqrt(2))),
                rule!("unit circle cos", Cos(*(4/3, Pi)) => -1/2),
                rule!("unit circle cos", Cos(*(3/2, Pi)) => 0),
                rule!("unit circle cos", Cos(*(5/3, Pi)) => 1/2),
                rule!("unit circle cos", Cos(*(7/4, Pi)) => *(1/2, Sqrt(2))),
                rule!("unit circle cos", Cos(*(11/6, Pi)) => *(1/2, Sqrt(3))),
                rule!("periodicity cos", Cos(+(*(~a:(is_even,!is_zero), Pi), ~~b)) => Cos(+(~~b))),
                rule!("periodicity cos", Cos(+(*(~a:(is_odd,!is_one), Pi), ~~b)) => Cos(+(Pi, ~~b))),
                rule!("unit circle tan", Tan(0) => 0),
                rule!("unit circle tan", Tan(*(1/6, Pi)) => *(1/3, Sqrt(3))),
                rule!("unit circle tan", Tan(*(1/4, Pi)) => 1),
                rule!("unit circle tan", Tan(*(1/3, Pi)) => Sqrt(3)),
                rule!("unit circle tan", Tan(*(1/2, Pi)) => Error("undefined")),
                rule!("unit circle tan", Tan(*(2/3, Pi)) => *(-1, Sqrt(3))),
                rule!("unit circle tan", Tan(*(3/4, Pi)) => -1),
                rule!("unit circle tan", Tan(*(5/6, Pi)) => *(-1/3, Sqrt(3))),
                rule!("periodicity tan", Tan(Pi) => Tan(0)),
                rule!("periodicity tan", Tan(+(*(~a:(is_integer,!is_zero), Pi), ~~b)) => Tan(+(~~b))),
                rule!("sin^2 + cos^2 = 1", +(^(Sin(~a), 2), ^(Cos(~a), 2)) => 1),
            ]),
            Self::FullExpand => Self::create_rulesets(&[
                Self::Form,
                Self::Addition,
                Self::Multiplication,
                Self::Division,
                Self::Powers,
                Self::Derivative,
                Self::Trigonometry,
            ]),
        }
    }
}

fn main() {
    let cleanup_ruleset = Rules::Cleanup.create_ruleset();
    let expand_ruleset = Rules::FullExpand.create_ruleset();
    loop {
        print!("calc > ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }
        let expr = Expression::from(input);
        println!("Received: {}", expr);
        println!("Received: {:?}", expr);

        let expr = expr.apply_ruleset(&cleanup_ruleset, true);
        println!("Parsed: {}", expr);
        println!("Parsed: {:?}", expr);

        let expr = expr.apply_ruleset(&expand_ruleset, true);
        println!("Result: {}", expr);
        println!("Result: {:?}", expr);
    }
}
