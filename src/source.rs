use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

#[derive(Copy, Clone)]
pub struct Location<'a> {
    pub source_name: &'a str,
    pub offset: usize,
    pub line_index: usize,
    pub column_index: usize
}

impl<'a> Location<'a> {
    pub fn new(source_name: &str) -> Location {
        Location { source_name: source_name, offset: 0, line_index: 0, column_index: 0 }
    }
}

impl<'a> Display for Location<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        formatter.write_str(&format!("{}({}:{})", self.source_name, self.line_index + 1, self.column_index + 1))
    }
}
