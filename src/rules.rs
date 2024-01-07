extern crate expr;
extern crate expr_macro;

use expr::Rule;
use expr_macro::rule;

pub struct RuleSet(Vec<Box<dyn Rule>>);
impl expr::RuleSet for RuleSet {
    fn rules(&self) -> impl Iterator<Item = &dyn Rule> {
        self.0.iter().map(|r| r.as_ref())
    }
}

pub enum Rules {
    Cleanup,
    Form,
    FullExpand,
    Addition,
    Multiplication,
    Division,
    Powers,
    Derivative,
    Integration,
    Logarithm,
    Sqrt,
    Trigonometry,
    Misc,
    Finalize,
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
    pub fn create_ruleset(&self) -> RuleSet {
        match self {
            Self::Cleanup => RuleSet(vec![
                rule!("associativity addition", +(+(~~a), ~~b) => +(~~a, ~~b), false),
                rule!("associativity multiplication", *(*(~~a), ~~b) => *(~~a, ~~b), false),
                rule!("create rational", /(~a:is_integer, ~b:(is_integer,!is_zero)) => create_rational(/(~a, ~b)), false),
            ]),
            Self::Form => RuleSet(vec![
                rule!("identity minus", -(~a, ~b) => +(~a, *(-1, ~b))),
                rule!("identity divide", /(~a, ~b) => *(~a, ^(~b, -1))),
                rule!("identity exp", ^(E, ~a:!is_number) => Exp(~a)),
            ]),
            Self::Addition => RuleSet(vec![
                rule!("associativity addition", +(+(~~a), ~~b) => +(~~a, ~~b), false),
                rule!("collapse addition", +(~a) => ~a, false),
                rule!("numeric addition", +(~~a:is_number:2, ~~b:!is_number) => +(reduce(~~a), ~~b)),
                rule!("identity addition", +(~~a:!is_zero:1, ~~b:is_zero:1) => ~~a),
            ]),
            Self::Multiplication => RuleSet(vec![
                rule!("associativity multiplication", *(*(~~a), ~~b) => *(~~a, ~~b), false),
                rule!("collapse multiplication", *(~a) => ~a, false),
                rule!("numeric multiplication", *(~~a:is_number:2, ~~b:!is_number) => *(reduce(~~a), ~~b)),
                rule!("identity multiplication", *(~~a:!is_one:1, ~~b:is_one:1) => ~~a),
                rule!("absorber multiplication", *(~~a::1, 0) => 0),
                rule!("distributivity multiplication", *(~~a::1, +(~~b)) => distribute(*(~~b, ~~a))),
                rule!("combine terms", ~a:can_combine => combine(~a)),
            ]),
            Self::Division => RuleSet(vec![
                rule!("rational simplification", ~a:is_rational_reducible => rational_reduce(~a)),
                rule!("division by zero", /(~a, 0) => Error("undefined")),
                rule!("identity divide", /(~a, 1) => ~a),
                rule!("create rational", /(~a:is_integer, ~b:(is_integer,!is_zero)) => create_rational(/(~a, ~b))),
            ]),
            Self::Powers => RuleSet(vec![
                rule!("numeric power", ^(~a:(is_number,!is_zero,!is_one), ~b:(is_integer,!is_zero,!is_one)) => reduce(^(~a, ~b))),
                rule!("power expansion", ^(+(~~a), ~b:(is_integer,is_positive,!is_one)) => reduce(^(~~a, ~b))),
                rule!("identity power", ^(~a, 1) => ~a),
                rule!("absorber power", ^(~a:!is_zero, 0) => 1),
                rule!("absorber power", ^(1, ~a) => 1),
                rule!("absorber power", ^(0, ~a:is_positive) => 0),
                rule!("division by zero", ^(0, ~a:is_negative) => Error("undefined")),
                rule!("undeterminate form 0^0", ^(0, 0) => Error("undefined")),
                rule!("associativity power", ^(^(~a, ~b:!is_one), ~c:!is_one) => ^(~a, *(~b, ~c))),
                rule!("distributivity power", ^(*(~~a), ~b:(is_integer,!is_one,!is_zero)) => distribute(^(~~a, ~b))),
                rule!("combine powers", ^(^(~a, ~b:(!is_one)), ~c) => ^(~a, *(~b, ~c))),
            ]),
            Self::Derivative => RuleSet(vec![
                rule!("independence", ~a:is_derivative_independent => 0),
                rule!("linearity", D(*(~~a:is_value:1, ~~b:!is_value:1), ~x) => *(~~a, D(~~b, ~x))),
                rule!("identity", D(~x, ~x) => 1),
                rule!("additivity", D(+(~~a), ~x) => reduce(D(~~a, ~x))),
                rule!("multiplicativity", D(*(~~a::2), ~x) => reduce(D(~~a, ~x))),
                rule!("natural power", D(^(~x, ~n:(is_integer,!is_zero,!is_one)), ~x) => *(~n, ^(~x, +(~n, -1)))),
                rule!("natural power", D(^(~y:!is_variable, ~n:(is_integer,!is_zero,!is_one)), ~x) => *(~n, D(~y, ~x), ^(~y, +(~n, -1)))),
                rule!("derivative of e^x", D(Exp(~y), ~x) => *(D(~y, ~x), Exp(~y))),
                rule!("derivative of a^x", D(^(~a:is_value, ~y), ~x) => *(D(~y, ~x), ^(~a, ~y), Ln(~a))),
                rule!("rewrite x^x", D(^(~a:!is_value, ~y:!is_value), ~x) => D(Exp(*(~y, Ln(~a))), ~x)),
                rule!("derivative of ln", D(Ln(~y), ~x) => *(D(~y, ~x), ^(~y, -1))),
                rule!("rewrite log", D(Log(~base, ~y), ~x) => D(*(Ln(~x), ^(Ln(~base), -1)), ~x)),
                rule!("derivative of sqrt", D(Sqrt(~y), ~x) => *(D(~y, ~x), ^(*(2, Sqrt(~y)), -1))),
                rule!("rewrite root", D(Root(~n, ~x), ~x) => D(^(~x, ^(~n, -1)), ~x)),
                rule!("derivative of sin", D(Sin(~y), ~x) => *(D(~y, ~x), Cos(~y))),
                rule!("derivative of cos", D(Cos(~y), ~x) => *(-1, D(~y, ~x), Sin(~y))),
                rule!("derivative of tan", D(Tan(~y), ~x) => *(D(~y, ~x), ^(Cos(~y), -2))),
            ]),
            Self::Integration => RuleSet(vec![
                rule!("independence", ~a:is_integral_independent => independent_integrate(~a)),
            ]),
            Self::Logarithm => RuleSet(vec![
                rule!("domain log", Ln(~a:is_nonpositive) => Error("domain log")),
                rule!("domain log", Log(~base, ~a:is_nonpositive) => Error("domain log")),
                rule!("domain log", Log(~base:is_nonpositive, ~a) => Error("domain log")),
                rule!("reduce log", ~a:is_log_reducible => log_reduce(~a)),
                rule!("log(1) = 0", Ln(1) => 0),
                rule!("log(1) = 0", Log(~base, 1) => 0),
                rule!("ln(e^x)=x", Ln(^(E, ~x)) => ~x),
                rule!("ln(e^x)=x", Ln(Exp(~x)) => ~x),
                rule!("ln(e^x)=x", Ln(*(E, ~~x)) => +(1, Ln(~~x))),
                rule!("log(a) + log(b) = log(ab)", +(Ln(~a), Ln(~b), ~~c) => +(Ln(*(~a, ~b)), ~~c)),
                rule!("log(a) + log(b) = log(ab)", +(Log(~base, ~a), Log(~base, ~b), ~~c) => +(Log(~base, *(~a, ~b)), ~~c)),
                rule!("n * log(a) = log(a^n)", *(~n:is_integer, Ln(~a), ~~b) => *(Ln(^(~a, ~n)), ~~b)),
                rule!("n * log(a) = log(a^n)", *(~n:is_integer, Log(~base, ~a), ~~b) => *(Log(~base, ^(~a, ~n)), ~~b)),
            ]),
            Self::Sqrt => RuleSet(vec![
                rule!("domain sqrt", Sqrt(~a:is_negative) => Error("domain sqrt")),
                rule!("domain root", Root(~n:is_even, ~a:is_negative) => Error("domain root")),
                rule!("reduce root", ~a:is_root_reducible => root_reduce(~a)),
                rule!("sqrt(a) * sqrt(b) = sqrt(ab)", *(Sqrt(~a), Sqrt(~b), ~~c) => *(Sqrt(*(~a, ~b)), ~~c)),
                rule!("root(a) * root(b) = root(ab)", *(Root(~n, ~a), Root(~n, ~b), ~~c) => *(Root(~n, *(~a, ~b)), ~~c)),
                rule!("sqrt(a) ^ 2 = a", ^(Sqrt(~a), 2) => ~a),
                rule!("sqrt(a ^ 2) = a", Sqrt(^(~a, 2)) => ~a),
                rule!("root(a) ^ n = a", ^(Root(~n, ~a), ~n) => ~a),
                rule!("root(a ^ n) = a", Root(^(~a, ~n), ~n) => ~a),
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
                rule!("periodicity sin", Sin(+(*(~a:(is_even,!is_zero), Pi), ~~b)) => Sin(~~b)),
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
                rule!("periodicity cos", Cos(+(*(~a:(is_even,!is_zero), Pi), ~~b)) => Cos(~~b)),
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
                rule!("periodicity tan", Tan(+(*(~a:(is_integer,!is_zero), Pi), ~~b)) => Tan(~~b)),
                rule!("sin^2 + cos^2 = 1", +(^(Sin(~a), 2), ^(Cos(~a), 2)) => 1),
            ]),
            Self::Misc => RuleSet(vec![
                rule!("abs", Abs(~n:is_value) => abs(~n)),
                rule!("max", Max(~a:is_value, ~b:is_value) => max(Max(~a, ~b))),
            ]),
            Self::FullExpand => Self::create_rulesets(&[
                Self::Form,
                Self::Addition,
                Self::Multiplication,
                Self::Division,
                Self::Powers,
                Self::Derivative,
                Self::Integration,
                Self::Logarithm,
                Self::Sqrt,
                Self::Trigonometry,
                Self::Misc,
            ]),
            Self::Finalize => RuleSet(vec![rule!("reorder terms", ~a:!is_sorted => sort(~a))]),
        }
    }
}
