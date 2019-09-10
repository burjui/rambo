use std::error::Error;
use std::io;
use std::io::Write;
use std::mem::swap;
use std::ops::Range;

use termcolor::Color;
use termcolor::ColorChoice;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

use copy_in_place::copy_in_place;

pub(crate) type GenericResult<T> = std::result::Result<T, Box<dyn Error>>;

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
    () => {
        format!("{}({})", file!(), line!())
    };
}

#[cfg(test)]
macro_rules! typecheck {
    ($text: expr) => {{
        use crate::utils::typecheck;
        typecheck(location!(), $text)
    }};
}

#[cfg(test)]
pub(crate) fn typecheck(name: String, text: &str) -> GenericResult<crate::semantics::ExprRef> {
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
    let color_choice = if is_tty {
        ColorChoice::Always
    } else {
        ColorChoice::Never
    };
    StandardStream::stdout(color_choice)
}

pub(crate) fn stderr() -> StandardStream {
    let is_tty = unsafe { libc::isatty(libc::STDERR_FILENO as i32) } != 0;
    let color_choice = if is_tty {
        ColorChoice::Always
    } else {
        ColorChoice::Never
    };
    StandardStream::stderr(color_choice)
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

pub(crate) fn intersection<'a, T: Ord + Copy>(
    mut r1: &'a Range<T>,
    mut r2: &'a Range<T>,
) -> Option<Range<T>> {
    if r1.start > r2.start {
        swap(&mut r1, &mut r2);
    }
    if r1.end <= r2.start {
        None
    } else {
        Some(r1.start.max(r2.start)..r1.end.min(r2.end))
    }
}

#[cfg(test)]
macro_rules! test_intersection {
    ($range1: expr, $range2: expr, $expected: expr) => {{
        use std::ops::Range;
        let range1: Range<u32> = $range1;
        let range2: Range<u32> = $range2;
        let expected: Option<Range<u32>> = $expected;
        assert_eq!(&intersection(&range1, &range2), &expected);
        assert_eq!(
            &intersection(&range1, &range2),
            &intersection(&range2, &range1)
        );
    }};
}

#[test]
fn range_intersection() {
    test_intersection!(0..10, 10..20, None);
    test_intersection!(5..15, 10..20, Some(10..15));
    test_intersection!(10..20, 12..15, Some(12..15));
}

pub(crate) trait RetainIndices<T> {
    fn retain_indices(
        &mut self,
        predicate: impl FnMut(&T, usize) -> bool,
        remap: impl FnMut(&T, usize, usize),
    );
}

impl<T> RetainIndices<T> for Vec<T> {
    fn retain_indices(
        &mut self,
        mut predicate: impl FnMut(&T, usize) -> bool,
        mut remap: impl FnMut(&T, usize, usize),
    ) {
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

pub(crate) trait VecUtils<T> {
    fn insert_slice(&mut self, index: usize, s: &[T]);
}

impl<T: Default + Copy> VecUtils<T> for Vec<T> {
    fn insert_slice(&mut self, index: usize, src: &[T]) {
        let old_len = self.len();
        let slice_len = src.len();
        self.resize_with(old_len + slice_len, T::default);
        let new_len = self.len();
        let slice_end = index + slice_len;
        let range = index..new_len - slice_len;
        copy_in_place(self, range.clone(), new_len - range.end + range.start);
        (&mut self[index..slice_end]).copy_from_slice(src)
    }
}

pub(crate) fn dump_memory(ram: &[u8], base_address: u32) -> io::Result<()> {
    let stderr = &mut stderr();
    for (index, &byte) in ram.iter().enumerate() {
        if index % 4 == 0 {
            if index > 0 {
                writeln!(stderr)?;
            }
            write!(stderr, "[{:08x}] ", base_address + index as u32)?;
        } else {
            write!(stderr, " ")?;
        }
        stderr.set_color(
            ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true),
        )?;
        write!(stderr, "{:02x}", byte)?;
        stderr.reset()?;
    }
    writeln!(stderr, "\n")
}
