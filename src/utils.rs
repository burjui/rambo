macro_rules! error {
    ($format_string: expr $(, $argument: expr)*) => { Err(From::from(format!($format_string $(, $argument)*))) };
}
