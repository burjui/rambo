use itertools::Itertools;
use std::fmt::Debug;

crate const DEBUG_PRINT_OPTION_NAME: &str = "debug";
crate const DEBUG_PRINT_OPTION: &str = "--debug";

macro_rules! error {
    ($format_string: expr $(, $argument: expr)*) => { Err(From::from(format!($format_string $(, $argument)*))) };
}

macro_rules! warning {
    ($format_string: expr $(, $argument: expr)*) => {{
        print!("warning: "); println!($format_string $(, $argument)*)
    }};
}

macro_rules! warning_at {
    ($source: expr, $format_string: expr $(, $argument: expr)*) => {{
        print!("warning: {}: ", $source);
        println!($format_string $(, $argument)*)
    }};
}

macro_rules! debug_print {
    ($format_string: expr $(, $argument: expr)*) => {{
        for arg in std::env::args() {
            if arg == crate::utils::DEBUG_PRINT_OPTION {
                print!($format_string $(, $argument)*);
                break;
            }
        }
    }};
}

macro_rules! debug_println {
    ($format_string: expr $(, $argument: expr)*) => {{
        debug_print!($format_string $(, $argument)*);
        debug_print!("\n");
    }};
}

crate trait ByLine: Iterator {
    fn join_as_strings(&mut self, separator: &str) -> String
        where Self: Sized, Self::Item: Debug
    {
        self.map(|x| format!("{:?}", x)).join(separator)
    }
}

impl<I> ByLine for I where I: Iterator {}
