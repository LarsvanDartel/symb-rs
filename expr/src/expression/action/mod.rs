mod constant;
mod function;
mod number;

use crate::{literals, Expression};
pub use constant::Constant;
pub use function::Function;
pub use number::Number;

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
    Slot {
        name: String,
        matcher: &'static dyn Fn(&Expression) -> bool,
        predicate: &'static dyn Fn(&Expression) -> bool,
    },

    /// Placeholder for a list of expressions
    Segment {
        name: String,
        matcher: &'static dyn Fn(&Expression) -> bool,
        predicate: &'static dyn Fn(&Expression) -> bool,
    },

    /// Map from expression to expression
    Map {
        name: String,
        map: &'static dyn Fn(&Expression) -> Expression,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Priority {
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
            _ => Priority::None,
        }
    }

    pub fn arity(&self) -> Option<usize> {
        match self {
            Action::Add | Action::Mul => None,
            Action::Sub | Action::Div | Action::Pow => Some(2),
            Action::Equals => Some(2),
            Action::Fun(function) => Some(function.arity()),
            Action::Map { .. } => Some(1),
            _ => Some(0),
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

impl PartialEq for Action {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var { name: a }, Self::Var { name: b }) => a == b,
            (Self::Num { value: a }, Self::Num { value: b }) => a == b,
            (Self::Fun(a), Self::Fun(b)) => a == b,
            (Self::Const(a), Self::Const(b)) => a == b,
            (Self::Err(_), Self::Err(_)) => false,
            (Self::Slot { name: a, .. }, Self::Slot { name: b, .. }) => a == b,
            (Self::Segment { name: a, .. }, Self::Segment { name: b, .. }) => a == b,
            (Self::Add, Self::Add) => true,
            (Self::Sub, Self::Sub) => true,
            (Self::Mul, Self::Mul) => true,
            (Self::Div, Self::Div) => true,
            (Self::Pow, Self::Pow) => true,
            (Self::Equals, Self::Equals) => true,
            _ => false,
        }
    }
}

impl PartialOrd for Action {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some((self.priority() as u8).cmp(&(other.priority() as u8)))
    }
}

impl Clone for Action {
    fn clone(&self) -> Self {
        match self {
            Self::Add => Self::Add,
            Self::Sub => Self::Sub,
            Self::Mul => Self::Mul,
            Self::Div => Self::Div,
            Self::Pow => Self::Pow,
            Self::Equals => Self::Equals,
            Self::Var { name } => Self::Var { name: name.clone() },
            Self::Num { value } => Self::Num {
                value: value.clone(),
            },
            Self::Fun(arg0) => Self::Fun(arg0.clone()),
            Self::Const(arg0) => Self::Const(arg0.clone()),
            Self::Err(arg0) => Self::Err(arg0.clone()),
            Self::Slot {
                name,
                matcher,
                predicate,
            } => Self::Slot {
                name: name.clone(),
                matcher: *matcher,
                predicate: *predicate,
            },
            Self::Segment {
                name,
                matcher,
                predicate,
            } => Self::Segment {
                name: name.clone(),
                matcher: *matcher,
                predicate: *predicate,
            },
            Self::Map { name, map } => Self::Map {
                name: name.clone(),
                map: *map,
            },
        }
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
        }
    }
}
