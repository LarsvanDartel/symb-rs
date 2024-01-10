use pyo3::prelude::*;

pub mod rewrite;
pub mod rules;

use rewrite::{DoubleBfsRewriter, Rewriter, SimpleRewriter, SingleBfsRewriter};
use rules::{apply_rules, CLEANUP_RULES, FULL_EXPAND_RULES};
use expr::Expression;

#[pyfunction]
fn run(mut input: String) -> PyResult<String> {
    let mut result = String::new();
    let simple = input.contains("simple");
    if simple {
        input = input.replace("simple", "");
    }
    let mut expr = Expression::from(input);

    apply_rules(&CLEANUP_RULES, &mut expr, false);

    let mut records = if expr.is_equality() {
        let mut lhs = expr.get_lhs().unwrap().clone();
        apply_rules(&CLEANUP_RULES, &mut lhs, false);
        let mut rhs = expr.get_rhs().unwrap().clone();
        apply_rules(&CLEANUP_RULES, &mut rhs, false);
        let records = if let Some(records) = if simple {
            SimpleRewriter::rewrite(&lhs, &rhs, &FULL_EXPAND_RULES)
        } else {
            if let Some(records) =
                SingleBfsRewriter::rewrite(&lhs, &rhs, &FULL_EXPAND_RULES)
            {
                Some(records)
            } else if let Some(records) =
                SingleBfsRewriter::rewrite(&rhs, &lhs, &FULL_EXPAND_RULES)
            {
                Some(records)
            } else if let Some(records) =
            DoubleBfsRewriter::rewrite(&lhs, &rhs, &FULL_EXPAND_RULES)
            {
                Some(records)
            } else {
                None
            }
        } {
            records
        } else {
            return Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
                "Could not rewrite",
            ));
        };
        if records.is_empty() {
            return Ok(String::new());
        }
        records
    } else {
        apply_rules(&FULL_EXPAND_RULES, &mut expr, true)
    };

    for record in records.iter_mut() {
        record.message = format!("({})", record.message);
    }
    let max = records.iter().map(|record| record.message.len()).max().unwrap();
    for record in records {
        result.push_str(format!(
            "{:<width$} => {} = {}\n",
            record.message,
            record.old.to_string(),
            record.new.to_string(),
            width = max
        ).as_str());
    }

    Ok(result)
}

#[pymodule]
fn symb_rs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    Ok(())
}