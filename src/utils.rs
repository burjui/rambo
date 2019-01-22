#[cfg(test)]
use std::error::Error;

use once_cell::sync::Lazy;
use once_cell::sync_lazy;
use regex::Regex;
use termcolor::ColorChoice;
use termcolor::StandardStream;

pub(crate) static WHITESPACE_REGEX: Lazy<Regex> = sync_lazy!(Regex::new(r"[ \t]+").unwrap());

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

#[cfg(feature = "dump_ssa_in_tests")]
pub(crate) mod ssa {
    use std::error::Error;

    use crate::ssa::SSAStatement;

    pub(crate) fn dump(code: &str, ssa: &[SSAStatement]) -> Result<(), Box<dyn Error>> {
        use std::fs::File;
        use std::io::Write;
        use std::sync::Mutex;
        use itertools::Itertools;
        use once_cell::sync::Lazy;
        use once_cell::sync_lazy;

        static SSA_TXT: Lazy<Mutex<File>> = sync_lazy!(Mutex::new(File::create("ssa.txt").unwrap()));
        let mut file = SSA_TXT.lock().unwrap();
        writeln!(file, "-------------\nCODE:\n{}\n\nSSA:\n{:#?}", code, ssa.iter().format("\n"))?;
        Ok(())
    }
}

#[cfg(test)]
pub(crate) type TestResult = Result<(), Box<dyn Error>>;
