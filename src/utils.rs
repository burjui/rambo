use std::error::Error;

use termcolor::{ColorChoice, StandardStream};

pub(crate) type GenericResult<T> = std::result::Result<T, Box<dyn Error>>;

pub(crate) macro error {
    ($format_string: expr $(, $argument: expr)*) => { Err(From::from(format!($format_string $(, $argument)*))) }
}

pub(crate) macro warning {
    ($format_string: expr $(, $argument: expr)*) => {{
        print!("warning: "); println!($format_string $(, $argument)*)
    }}
}

pub(crate) macro warning_at {
    ($source: expr, $format_string: expr $(, $argument: expr)*) => {{
        print!("warning: {}: ", $source);
        println!($format_string $(, $argument)*)
    }}
}

pub(crate) macro function() {{
    struct X;
    let name = core::any::type_name::<X>();
    &name[..name.len() - 3]
}}

#[cfg(test)]
pub(crate) macro function_name() {
    function!().rsplit("::").next().unwrap()
}

pub(crate) macro impl_deref_for_newtype($name: ty, $target: ty) {
    impl std::ops::Deref for $name {
        type Target = $target;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}

#[cfg(test)]
pub(crate) fn typecheck(name: String, text: &str) -> GenericResult<crate::semantics::ExprRef> {
    use crate::semantics::{EnableWarnings, SemanticsChecker};

    let file = crate::source::SourceFile::create(name, text);
    let lexer = crate::lexer::Lexer::new(file);
    let mut parser = crate::parser::Parser::new(lexer);
    let ast = parser.parse()?;

    let checker = SemanticsChecker::new(EnableWarnings(false));
    checker.check_module(&ast)
}

pub(crate) fn stdout() -> StandardStream {
    #[cfg(not(test))]
    let color_choice = {
        let is_tty = unsafe { libc::isatty(libc::STDOUT_FILENO) } != 0;
        if is_tty {
            ColorChoice::Always
        } else {
            ColorChoice::Never
        }
    };
    #[cfg(test)]
    let color_choice = ColorChoice::Never;
    StandardStream::stdout(color_choice)
}

pub(crate) fn stderr() -> StandardStream {
    let is_tty = unsafe { libc::isatty(libc::STDERR_FILENO) } != 0;
    let color_choice = if is_tty {
        ColorChoice::Always
    } else {
        ColorChoice::Never
    };
    StandardStream::stderr(color_choice)
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
        self.copy_within(range.clone(), new_len - range.end + range.start);
        self[index..slice_end].copy_from_slice(src)
    }
}
