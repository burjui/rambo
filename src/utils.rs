#[cfg(test)]
use std::error::Error;

use once_cell::sync::Lazy;
use once_cell::sync_lazy;
use regex::Regex;
use termcolor::ColorChoice;
use termcolor::StandardStream;

pub(crate) static WHITESPACE_REGEX: Lazy<Regex> = sync_lazy!(Regex::new(r"[ \t\n]+").unwrap());

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

#[cfg(test)]
macro_rules! location {
    () => (format!("{}({})", file!(), line!()))
}

#[cfg(test)]
macro_rules! typecheck {
    ($text: expr) => ({
        use crate::utils::typecheck;
        typecheck(location!(), $text)
    })
}

#[cfg(test)]
pub(crate) fn typecheck(name: String, text: &str) -> Result<crate::semantics::ExprRef, Box<dyn std::error::Error>> {
    let file = crate::source::SourceFile::create(name, text);
    let lexer = crate::lexer::Lexer::new(file);
    let mut parser = crate::parser::Parser::new(lexer);
    let ast = parser.parse()?;

    use crate::semantics::SemanticsChecker;
    let checker = SemanticsChecker::new();
    checker.check_module(&ast)
}

pub(crate) fn stdout() -> StandardStream {
    let is_tty = unsafe { libc::isatty(libc::STDOUT_FILENO as i32) } != 0;
    let color_choice = if is_tty { ColorChoice::Always } else { ColorChoice::Never };
    StandardStream::stdout(color_choice)
}

#[cfg(test)]
pub(crate) type TestResult = Result<(), Box<dyn Error>>;

macro_rules! matches {
    ($expr: expr, $($($pattern: pat)|+ $(if $guard: expr)?),+) => {
        match $expr {
            $($($pattern)|+ $(if $guard)? => true,)+
            _ => false,
        }
    }
}

pub(crate) trait RetainIndex {
    fn retain_index(&mut self, predicate: impl Fn(usize) -> bool, remap: impl FnMut(usize, usize));
}

impl<T> RetainIndex for Vec<T> {
    fn retain_index(&mut self, predicate: impl Fn(usize) -> bool, mut remap: impl FnMut(usize, usize)) {
        let mut index = 0;
        let mut new_index = 0;
        self.retain(|_| {
            let retain = predicate(index);
            if retain {
                remap(index, new_index);
                new_index += 1;
            }
            index += 1;
            retain
        });
    }
}
