macro_rules! error {
    ($format_string: expr $(, $argument: expr)*) => { Err(From::from(format!($format_string $(, $argument)*))) };
}

macro_rules! warning {
    ($format_string: expr $(, $argument: expr)*) => {{
        print!("warning: "); println!($format_string $(, $argument)*)
    }};
}

use std::fmt::Debug;
use itertools::Itertools;
crate trait ByLine: Iterator {
    fn join_as_strings(&mut self, separator: &str) -> String
        where Self: Sized, Self::Item: Debug
    {
        self.map(|x| format!("{:?}", x)).join(separator)
    }
}

impl<I> ByLine for I where I: Iterator {}
