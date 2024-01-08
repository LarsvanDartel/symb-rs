use expr::{Expression, Rule};
use expr_macro::rule;
use once_cell::sync::Lazy;

use crate::rewrite::RewriteRecord;

pub fn apply_rule(
    rule: &Rule,
    expr: &mut Expression,
    finalize: bool,
    print: bool,
) -> Option<RewriteRecord> {
    let mut new_expr = expr.apply_rule(rule)?;
    if finalize {
        apply_rules(&FINALIZE_RULES, &mut new_expr, false, false);
    }
    if print && rule.counts() {
        println!("{}: {} = {}", rule.name(), expr, new_expr);
    }
    Some(RewriteRecord {
        message: rule.name().to_string(),
        old: expr.clone(),
        new: new_expr.clone(),
    })
}

pub fn apply_rules(
    rules: &[Rule],
    expr: &mut Expression,
    finalize: bool,
    print: bool,
) -> Vec<RewriteRecord> {
    let mut records = Vec::new();
    let mut cnt = 0;
    loop {
        let mut applied = false;
        for rule in rules {
            if let Some(record) = apply_rule(rule, expr, finalize, print) {
                *expr = record.new.clone();
                applied = true;
                if rule.counts() {
                    cnt += 1;
                    records.push(record);
                }
                break;
            }
        }
        if !applied {
            break;
        }
    }
    if print && cnt != 0 {
        println!("{} rule{} applied", cnt, if cnt == 1 { "" } else { "s" });
    }
    records
}

pub static CLEANUP_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("associativity addition", +(+(~~a::1), ~~b::1) => +(~~a, ~~b), false),
        rule!("associativity multiplication", *(*(~~a::1), ~~b::1) => *(~~a, ~~b), false),
        rule!("create rational", /(~a:is_integer, ~b:(is_integer && !is_zero)) => create_rational(/(~a, ~b)), false),
    ]
});

static FORM_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("identity minus", -(~a, ~b) => +(~a, *(-1, ~b))),
        rule!("identity divide", /(~a, ~b) => *(~a, ^(~b, -1))),
        rule!("identity exp", ^(E, ~a:!is_number) => Exp(~a)),
    ]
});

static ADDITION_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("associativity addition", +(+(~~a::1), ~~b::1) => +(~~a, ~~b), false),
        rule!("collapse addition", +(~a) => ~a, false),
        rule!("numeric addition", +(~~a:is_number:2, ~~b:!is_number) => +(reduce(~~a), ~~b)),
        rule!("identity addition", +(~~a:!is_zero:1, ~~b:is_zero:1) => ~~a),
    ]
});

static MULTIPLICATION_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("associativity multiplication", *(*(~~a::1), ~~b::1) => *(~~a, ~~b), false),
        rule!("collapse multiplication", *(~a) => ~a, false),
        rule!("numeric multiplication", *(~~a:is_number:2, ~~b:!is_number) => *(reduce(~~a), ~~b)),
        rule!("identity multiplication", *(~~a:!is_one:1, ~~b:is_one:1) => ~~a),
        rule!("absorber multiplication", *(~~a::1, 0) => 0),
        rule!("distributivity multiplication", *(~~a::1, +(~~b)) => distribute(*(~~b, ~~a))),
        rule!("combine terms", ~a:can_combine => combine(~a)),
    ]
});

static DIVIDE_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("rational simplification", ~a:is_rational_reducible => rational_reduce(~a)),
        rule!("division by zero", /(~a, 0) => Error("undefined")),
        rule!("identity divide", /(~a, 1) => ~a),
    ]
});

static POWER_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("numeric power", ^(~a:(is_number && !is_zero && !is_one), ~b:(is_integer && !is_zero && !is_one)) => reduce(^(~a, ~b))),
        rule!("power expansion", ^(+(~~a), ~b:(is_integer && is_positive && !is_one)) => reduce(^(~~a, ~b))),
        rule!("identity power", ^(~a, 1) => ~a),
        rule!("absorber power", ^(~a:!is_zero, 0) => 1),
        rule!("absorber power", ^(1, ~a) => 1),
        rule!("absorber power", ^(0, ~a:is_positive) => 0),
        rule!("division by zero", ^(0, ~a:is_negative) => Error("undefined")),
        rule!("undeterminate form 0^0", ^(0, 0) => Error("undefined")),
        rule!("associativity power", ^(^(~a, ~b:!is_one), ~c:!is_one) => ^(~a, *(~b, ~c))),
        rule!("distributivity power", ^(*(~~a), ~b:(is_integer && !is_one && !is_zero)) => distribute(^(~~a, ~b))),
        rule!("combine powers", ^(^(~a, ~b:(!is_one)), ~c) => ^(~a, *(~b, ~c))),
    ]
});

static DERIVATIVE_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("independence", D(~y, ~x:is_variable) => 0 if is_independent(~y, ~x)),
        rule!("linearity", D(*(~~a:is_value:1, ~~b:!is_value:1), ~x) => *(~~a, D(~~b, ~x))),
        rule!("identity", D(~x, ~x) => 1),
        rule!("additivity", D(+(~~a), ~x) => reduce(D(~~a, ~x))),
        rule!("multiplicativity", D(*(~~a::2), ~x) => reduce(D(~~a, ~x))),
        rule!("natural power", D(^(~y, ~n:(is_integer && !is_zero && !is_one)), ~x) => *(~n, D(~y, ~x), ^(~y, +(~n, -1)))),
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
    ]
});

static INTEGRATION_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("independence", Int(~y, ~x:is_variable) => *(~y, ~x) if is_independent(~y, ~x)),
        rule!("homogeneity", Int(*(~~a:is_value:1, ~~b:!is_value:1), ~x) => *(~~a, Int(~~b, ~x))),
        // rule!("additivity", Int(+(~~a), ~x) => reduce(Int(~~a, ~x))),
        rule!("primitive of cos", Int(Cos(+(*(~a:is_value, ~x:is_variable), ~~b:is_value)), ~x) => *(/(1, ~a), Sin(+(*(~a, ~x), ~~b)))),
        rule!("primitive of sin", Int(Sin(+(*(~a:is_value, ~x:is_variable), ~~b:is_value)), ~x) => *(/(-1, ~a), Cos(+(*(~a, ~x), ~~b)))),
        rule!("primitive of exp", Int(Exp(+(*(~a:is_value, ~x:is_variable), ~~b:is_value)), ~x) => *(/(1, ~a), Exp(+(*(~a, ~x), ~~b)))),
        rule!("primitive of 1/(ax+b)", Int(/(1, +(*(~a:is_value, ~x:is_variable), ~~b:is_value)), ~x) => *(/(1, ~a), Ln(Abs(+(*(~a, ~x), ~~b))))),
    ]
});

static LOG_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("domain log", Ln(~a:is_nonpositive) => Error("domain log")),
        rule!("domain log", Log(~base, ~a:is_nonpositive) => Error("domain log")),
        rule!("domain log", Log(~base:is_nonpositive, ~a) => Error("domain log")),
        rule!("reduce log", ~a:is_log_reducible => log_reduce(~a)),
        rule!("log(1) = 0", Ln(1) => 0),
        rule!("log(1) = 0", Log(~base, 1) => 0),
        rule!("ln(e^x)=x", Ln(^(E, ~x)) => ~x),
        rule!("ln(e^x)=x", Ln(Exp(~x)) => ~x),
        rule!("ln(e^x)=x", Ln(*(E, ~~x)) => +(1, Ln(~~x))),
        rule!("logb(b^x)=x", Log(~base, ^(~base, ~x)) => ~x),
        rule!("log(a) + log(b) = log(ab)", +(Ln(~a), Ln(~b), ~~c) => +(Ln(*(~a, ~b)), ~~c)),
        rule!("log(a) + log(b) = log(ab)", +(Log(~base, ~a), Log(~base, ~b), ~~c) => +(Log(~base, *(~a, ~b)), ~~c)),
        rule!("n * log(a) = log(a^n)", *(~n:(is_integer && !is_one), Ln(~a), ~~b) => *(Ln(^(~a, ~n)), ~~b)),
        rule!("n * log(a) = log(a^n)", *(~n:(is_integer && !is_one), Log(~base, ~a), ~~b) => *(Log(~base, ^(~a, ~n)), ~~b)),
    ]
});

static SQRT_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("domain sqrt", Sqrt(~a:is_negative) => Error("domain sqrt")),
        rule!("domain root", Root(~n:is_even, ~a:is_negative) => Error("domain root")),
        rule!("reduce root", ~a:is_root_reducible => root_reduce(~a)),
        rule!("sqrt(a) * sqrt(b) = sqrt(ab)", *(Sqrt(~a), Sqrt(~b), ~~c) => *(Sqrt(*(~a, ~b)), ~~c)),
        rule!("root(a) * root(b) = root(ab)", *(Root(~n, ~a), Root(~n, ~b), ~~c) => *(Root(~n, *(~a, ~b)), ~~c)),
        rule!("sqrt(a) ^ 2 = a", ^(Sqrt(~a), 2) => ~a),
        rule!("sqrt(a ^ 2) = a", Sqrt(^(~a, 2)) => ~a),
        rule!("root(a) ^ n = a", ^(Root(~n, ~a), ~n) => ~a),
        rule!("root(a ^ n) = a", Root(^(~a, ~n), ~n) => ~a),
    ]
});

static TRIG_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
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
        rule!("periodicity sin", Sin(+(*(~a:(is_even && !is_zero), Pi), ~~b)) => Sin(~~b)),
        rule!("periodicity sin", Sin(+(*(~a:(is_odd && !is_one), Pi), ~~b)) => Sin(+(Pi, ~~b))),
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
        rule!("periodicity cos", Cos(+(*(~a:(is_even && !is_zero), Pi), ~~b)) => Cos(~~b)),
        rule!("periodicity cos", Cos(+(*(~a:(is_odd && !is_one), Pi), ~~b)) => Cos(+(Pi, ~~b))),
        rule!("unit circle tan", Tan(0) => 0),
        rule!("unit circle tan", Tan(*(1/6, Pi)) => *(1/3, Sqrt(3))),
        rule!("unit circle tan", Tan(*(1/4, Pi)) => 1),
        rule!("unit circle tan", Tan(*(1/3, Pi)) => Sqrt(3)),
        rule!("unit circle tan", Tan(*(1/2, Pi)) => Error("undefined")),
        rule!("unit circle tan", Tan(*(2/3, Pi)) => *(-1, Sqrt(3))),
        rule!("unit circle tan", Tan(*(3/4, Pi)) => -1),
        rule!("unit circle tan", Tan(*(5/6, Pi)) => *(-1/3, Sqrt(3))),
        rule!("periodicity tan", Tan(Pi) => Tan(0)),
        rule!("periodicity tan", Tan(+(*(~a:(is_integer && !is_zero), Pi), ~~b)) => Tan(~~b)),
        rule!("sin^2 + cos^2 = 1", +(^(Sin(~a), 2), ^(Cos(~a), 2), ~~c) => +(1, ~~c)),
    ]
});

static MISC_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("abs", Abs(~n:is_value) => abs(~n)),
        rule!("max", Max(~a:is_value, ~b:is_value) => max(Max(~a, ~b))),
    ]
});

pub static FULL_EXPAND_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    [
        CLEANUP_RULES.as_slice(),
        FORM_RULES.as_slice(),
        ADDITION_RULES.as_slice(),
        MULTIPLICATION_RULES.as_slice(),
        DIVIDE_RULES.as_slice(),
        POWER_RULES.as_slice(),
        DERIVATIVE_RULES.as_slice(),
        INTEGRATION_RULES.as_slice(),
        LOG_RULES.as_slice(),
        SQRT_RULES.as_slice(),
        TRIG_RULES.as_slice(),
        MISC_RULES.as_slice(),
    ]
    .concat()
});

static FINALIZE_RULES: Lazy<Vec<Rule>> = Lazy::new(|| {
    vec![
        rule!("associativity addition", +(+(~~a::1), ~~b::1) => +(~~a, ~~b), false),
        rule!("associativity multiplication", *(*(~~a::1), ~~b::1) => *(~~a, ~~b), false),
        rule!("collapse addition", +(~a) => ~a, false),
        rule!("collapse multiplication", *(~a) => ~a, false),
        rule!("reorder terms", ~a:!is_sorted => sort(~a), false),
        rule!("create rational", /(~a:is_integer, ~b:(is_integer && !is_zero)) => create_rational(/(~a, ~b)), false),
    ]
});
