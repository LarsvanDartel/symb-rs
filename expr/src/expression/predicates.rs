use crate::Expression;

#[allow(non_upper_case_globals)]
pub const number: &'static dyn Fn(&Expression) -> bool = &|e| e.is_number();

#[allow(non_upper_case_globals)]
pub const integer: &'static dyn Fn(&Expression) -> bool = &|e| e.is_integer();

#[allow(non_upper_case_globals)]
pub const positive: &'static dyn Fn(&Expression) -> bool = &|e| e.is_positive();

#[allow(non_upper_case_globals)]
pub const nonnegative: &'static dyn Fn(&Expression) -> bool = &|e| e.is_nonnegative();

#[allow(non_upper_case_globals)]
pub const negative: &'static dyn Fn(&Expression) -> bool = &|e| e.is_negative();

#[allow(non_upper_case_globals)]
pub const nonpositive: &'static dyn Fn(&Expression) -> bool = &|e| e.is_nonpositive();

#[allow(non_upper_case_globals)]
pub const constant: &'static dyn Fn(&Expression) -> bool = &|e| e.is_constant();

#[allow(non_upper_case_globals)]
pub const variable: &'static dyn Fn(&Expression) -> bool = &|e| e.is_variable();

#[allow(non_upper_case_globals)]
pub const value: &'static dyn Fn(&Expression) -> bool = &|e| e.is_value();
