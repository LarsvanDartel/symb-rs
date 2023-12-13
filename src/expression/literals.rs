pub(crate) const ADD: &str = "+";
pub(crate) const SUB: &str = "-";
pub(crate) const MUL: &str = "*";
pub(crate) const DIV: &str = "/";
pub(crate) const POW: &str = "^";

pub(crate) const OPERATORS: [&str; 5] = [ADD, SUB, MUL, DIV, POW];

pub(crate) const SIN: &str = "Sin";
pub(crate) const COS: &str = "Cos";
pub(crate) const TAN: &str = "Tan";
pub(crate) const SQRT: &str = "Sqrt";
pub(crate) const ROOT: &str = "Root";
pub(crate) const EXP: &str = "Exp";
pub(crate) const LN: &str = "Ln";
pub(crate) const LOG: &str = "Log";
pub(crate) const DIFF: &str = "D";
pub(crate) const INT: &str = "Int";

pub(crate) const FUNCTIONS: [&str; 10] = [SIN, COS, TAN, SQRT, ROOT, EXP, LN, LOG, DIFF, INT];

pub(crate) const PI_SHORT: &str = "π";
pub(crate) const PI: &str = "Pi";
pub(crate) const EULER: &str = "E";

pub(crate) const CONSTANTS: [&str; 2] = [PI, EULER];

pub(crate) const COMMA: &str = ",";
pub(crate) const EQUALS: &str = "=";
pub(crate) const LEFT_PARENTHESES: &str = "([{";
pub(crate) const RIGHT_PARENTHESES: &str = ")]}";

pub(crate) const fn matching_parentheses(c: char) -> Option<char> {
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