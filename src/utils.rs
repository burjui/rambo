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
crate fn typecheck(name: String, text: &str) -> Result<crate::semantics::ExprRef, Box<dyn std::error::Error>> {
    let file = crate::source::SourceFile::create(name, text);
    let lexer = crate::lexer::Lexer::new(file);
    let mut parser = crate::parser::Parser::new(lexer);
    let ast = parser.parse()?;
    crate::semantics::check_module(&ast)
}

crate fn stdout() -> StandardStream {
    let is_tty = unsafe { libc::isatty(libc::STDOUT_FILENO as i32) } != 0;
    let color_choice = if is_tty { ColorChoice::Always } else { ColorChoice::Never };
    StandardStream::stdout(color_choice)
}
