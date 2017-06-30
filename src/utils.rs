macro_rules! error {
    ($format_string: expr $(, $argument: expr)*) => { Err(From::from(format!($format_string $(, $argument)*))) };
}

use std::fmt::Debug;
use itertools::Itertools;
pub trait ByLine: Iterator {
    fn to_string(&mut self, separator: &str) -> String
        where Self: Sized, Self::Item: Debug
    {
        self.map(|x| format!("{:?}", x)).join(separator)
    }
}

impl<I> ByLine for I where I: Iterator {}