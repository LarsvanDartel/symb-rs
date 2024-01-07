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
pub const SQRT_SHORT: &str = "√";
pub const ROOT: &str = "Root";
pub const EXP: &str = "Exp";
pub const LN: &str = "Ln";
pub const LOG: &str = "Log";
pub const DIFF: &str = "D";
pub const INT: &str = "Int";
pub const ABS: &str = "Abs";
pub const MAX: &str = "Max";

#[allow(dead_code)]
pub const FUNCTIONS: [&str; 12] = [SIN, COS, TAN, SQRT, ROOT, EXP, LN, LOG, DIFF, INT, ABS, MAX];

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

pub fn to_superscript(s: String) -> String {
    s.chars()
        .map(|c| match c {
            '0' => '⁰',
            '1' => '¹',
            '2' => '²',
            '3' => '³',
            '4' => '⁴',
            '5' => '⁵',
            '6' => '⁶',
            '7' => '⁷',
            '8' => '⁸',
            '9' => '⁹',
            '-' => '⁻',
            _ => c,
        })
        .collect()
}

pub fn to_subscript(s: String) -> String {
    s.chars()
        .map(|c| match c {
            '0' => '₀',
            '1' => '₁',
            '2' => '₂',
            '3' => '₃',
            '4' => '₄',
            '5' => '₅',
            '6' => '₆',
            '7' => '₇',
            '8' => '₈',
            '9' => '₉',
            '-' => '₋',
            _ => c,
        })
        .collect()
}