use pyo3::prelude::*;
use symb_rs_core::*;

#[pyfunction]
fn run(mut input: String) -> PyResult<()> {
    let simple = input.contains("simple");
    if simple {
        input = input.replace("simple", "");
    }
    let mut expr = Expression::from(input);

    apply_rules(&CLEANUP_RULES, &mut expr, false);
    println!("Received: {}", expr);
    //println!("Received: {:?}", expr);

    let records = if expr.is_equality() {
        let mut lhs = expr.get_lhs().unwrap().clone();
        apply_rules(&CLEANUP_RULES, &mut lhs, false);
        let mut rhs = expr.get_rhs().unwrap().clone();
        apply_rules(&CLEANUP_RULES, &mut rhs, false);
        let records = if let Some(records) = if simple {
            SimpleRewriter::rewrite(&lhs, &rhs, &FULL_EXPAND_RULES)
        } else {
            if let Some(records) = SingleBfsRewriter::rewrite(&lhs, &rhs, &FULL_EXPAND_RULES) {
                Some(records)
            } else if let Some(records) = SingleBfsRewriter::rewrite(&rhs, &lhs, &FULL_EXPAND_RULES)
            {
                Some(records)
            } else if let Some(records) = DoubleBfsRewriter::rewrite(&lhs, &rhs, &FULL_EXPAND_RULES)
            {
                Some(records)
            } else {
                None
            }
        } {
            records
        } else {
            println!("Could not rewrite");
            return Ok(());
        };
        if records.is_empty() {
            println!("Expressions already equal");
            return Ok(());
        }
        records
    } else {
        let records = apply_rules(&FULL_EXPAND_RULES, &mut expr, true);
        if records.is_empty() {
            println!("Already fully expanded");
            return Ok(());
        }
        records
    };

    println!("Rewrite steps:");
    RewriteRecord::print_records(records.clone());
    Ok(())
}

#[pymodule]
fn symb_rs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    Ok(())
}
