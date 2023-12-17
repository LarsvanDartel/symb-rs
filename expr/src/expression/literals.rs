pub const ADD: &str = "+";
pub const SUB: &str = "-";
pub const MUL: &str = "*";
pub const DIV: &str = "/";
pub const POW: &str = "^";

#[allow(dead_code)]
pub const OPERATORS: [&str; 5] = [ADD, SUB, MUL, DIV, POW];

pub const SIN: &str = "Sin";
pub const COS: &str = "Cos";
pub const TAN: &str = "Tan";
pub const SQRT: &str = "Sqrt";
pub const ROOT: &str = "Root";
pub const EXP: &str = "Exp";
pub const LN: &str = "Ln";
pub const LOG: &str = "Log";
pub const DIFF: &str = "D";
pub const INT: &str = "Int";
pub const ABS: &str = "Abs";

#[allow(dead_code)]
pub const FUNCTIONS: [&str; 11] = [SIN, COS, TAN, SQRT, ROOT, EXP, LN, LOG, DIFF, INT, ABS];

pub const PI_SHORT: &str = "π";
pub const PI: &str = "Pi";
pub const EULER: &str = "E";

#[allow(dead_code)]
pub const CONSTANTS: [&str; 2] = [PI, EULER];

pub const COMMA: &str = ",";
pub const EQUALS: &str = "=";
pub const LEFT_PARENTHESES: &str = "([{";
pub const RIGHT_PARENTHESES: &str = ")]}";

pub const fn matching_parentheses(c: char) -> Option<char> {
    match c {
        '(' => Some(')'),
        '[' => Some(']'),
        '{' => Some('}'),
        ')' => Some('('),
        ']' => Some('['),
        '}' => Some('{'),
        _ => None,
    }
}

pub const PREDICATES: [&str; 13] = [
    "is_empty",
    "is_number",
    "is_integer",
    "is_zero",
    "is_one",
    "is_positive",
    "is_nonnegative",
    "is_negative",
    "is_nonpositive",
    "is_constant",
    "is_variable",
    "is_value",
    "can_combine",
];

pub const MAPS: [&str; 2] = ["reduce", "distribute"];
