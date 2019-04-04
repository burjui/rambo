#[cfg(test)]
use std::error::Error;
use std::mem::swap;
use std::ops::Range;

use termcolor::ColorChoice;
use termcolor::StandardStream;

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

pub(crate) fn intersect<'a, T: Ord + Copy>(mut r1: &'a Range<T>, mut r2: &'a Range<T>) -> Option<Range<T>> {
    if r1.start > r2.start {
        swap(&mut r1, &mut r2);
    }
    if r1.end <= r2.start {
        None
    } else {
        Some(r1.start.max(r2.start) .. r1.end.min(r2.end))
    }
}

#[cfg(test)]
macro_rules! test_intersection {
    ($range1: expr, $range2: expr, $expected: expr) => {{
        use std::ops::Range;
        let range1: Range<u32> = $range1;
        let range2: Range<u32> = $range2;
        let expected: Option<Range<u32>> = $expected;
        let intersection = intersect(&range1, &range2);
        assert_eq!(&intersection, &expected);
        assert_eq!(&intersection, &intersect(&range2, &range1));
    }};
}

pub(crate) trait RetainIndices<T> {
    fn retain_indices(&mut self, predicate: impl FnMut(&T, usize) -> bool, remap: impl FnMut(&T, usize, usize));
}

impl<T> RetainIndices<T> for Vec<T> {
    fn retain_indices(&mut self, mut predicate: impl FnMut(&T, usize) -> bool, mut remap: impl FnMut(&T, usize, usize)) {
        let mut index = 0;
        let mut new_index = 0;
        self.retain(|value| {
            let retain = predicate(value, index);
            if retain {
                remap(value, index, new_index);
                new_index += 1;
            }
            index += 1;
            retain
        });
    }
}
