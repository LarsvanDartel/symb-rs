mod constant;
mod function;
mod map;
mod number;
mod predicate;

use crate::{literals, Expression};
pub use constant::Constant;
pub use function::Function;
pub use map::Map;
pub use number::Number;
pub use predicate::{Predicate, PredicateType};

#[derive(Clone, PartialEq, Hash)]
pub enum Action {
    /// Addition
    Add,

    /// Subtraction
    Sub,

    /// Multiplication
    Mul,

    /// Division
    Div,

    /// Power
    Pow,

    /// Equals
    Equals,

    /// Variable
    Var { name: String },

    /// Number
    Num { value: Number },

    /// Function
    Fun(Function),

    /// Constant
    Const(Constant),

    /// Error
    Err(String),

    /// Placeholder for a single expression
    Slot { name: String, predicate: Predicate },

    /// Placeholder for a list of expressions
    Segment {
        name: String,
        predicate: Predicate,
        min_size: usize,
    },

    /// Map from expression to expression
    Map { name: String, map: Map },

    /// Predicate
    Predicate((PredicateType, bool)),

    /// And
    And,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Priority {
    Equals = 0,
    AddSub = 1,
    MulDiv = 2,
    Pow = 3,
    None = 4,
}

impl Action {
    pub fn priority(&self) -> Priority {
        match self {
            Action::Add | Action::Sub => Priority::AddSub,
            Action::Mul | Action::Div => Priority::MulDiv,
            Action::Pow => Priority::Pow,
            Action::Equals => Priority::Equals,
            _ => Priority::None,
        }
    }

    pub fn arity(&self) -> Option<(usize, usize)> {
        match self {
            Action::Add | Action::Mul => None,
            Action::Sub | Action::Div | Action::Pow => Some((2, 2)),
            Action::Equals => Some((2, 2)),
            Action::Fun(function) => Some(function.arity()),
            Action::Map { .. } => Some((1, 1)),
            Action::Predicate((predicate, _)) => Some(predicate.arity()),
            _ => Some((0, 0)),
        }
    }

    pub fn identity(&self) -> Expression {
        match self {
            Self::Add => Expression::create_value(0),
            Self::Mul => Expression::create_value(1),
            _ => panic!("No identity for {:?}", self),
        }
    }
}

impl PartialOrd for Action {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some((self.priority() as u8).cmp(&(other.priority() as u8)))
    }
}

impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Action::Add => f.write_str(literals::ADD),
            Action::Sub => f.write_str(literals::SUB),
            Action::Mul => f.write_str(literals::MUL),
            Action::Div => f.write_str(literals::DIV),
            Action::Pow => f.write_str(literals::POW),
            Action::Equals => f.write_str(literals::EQUALS),
            Action::Var { name } => f.write_str(name),
            Action::Num { value } => f.write_str(&value.to_string()),
            Action::Fun(function) => f.write_str(&function.to_string()),
            Action::Const(c) => f.write_str(&c.to_string()),
            Action::Err(message) => f.write_fmt(format_args!("Error: {}", message)),
            Action::Slot { name, .. } => f.write_fmt(format_args!("~{}", name)),
            Action::Segment { name, .. } => f.write_fmt(format_args!("~~{}", name)),
            Action::Map { name, .. } => f.write_str(name),
            Action::Predicate((predicate, positive)) => {
                if !*positive {
                    f.write_str("!")?;
                }
                f.write_fmt(format_args!("{:?}", predicate))
            }
            Action::And => f.write_str("And"),
        }
    }
}

impl std::fmt::Debug for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "Add"),
            Self::Sub => write!(f, "Sub"),
            Self::Mul => write!(f, "Mul"),
            Self::Div => write!(f, "Div"),
            Self::Pow => write!(f, "Pow"),
            Self::Equals => write!(f, "Equals"),
            Self::Var { name } => f.debug_tuple("Var").field(name).finish(),
            Self::Num { value } => write!(f, "{:?}", value),
            Self::Fun(function) => write!(f, "{:?}", function),
            Self::Const(constant) => write!(f, "{:?}", constant),
            Self::Err(message) => f.debug_tuple("Err").field(message).finish(),
            Self::Slot { name, .. } => f
                .debug_struct("Slot")
                .field("name", name)
                .finish_non_exhaustive(),
            Self::Segment { name, .. } => f
                .debug_struct("Segment")
                .field("name", name)
                .finish_non_exhaustive(),
            Self::Map { name, .. } => write!(f, "{:?}", name),
            Self::Predicate((predicate, positive)) => {
                if !*positive {
                    write!(f, "!")?;
                }
                write!(f, "{:?}", predicate)
            }
            Self::And => write!(f, "And"),
        }
    }
}
