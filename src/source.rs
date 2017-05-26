use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use std::path::Path;

#[derive(Copy, Clone)]
pub struct Source<'a> {
    pub file: &'a SourceFile<'a>,
    pub segment: Segment
}

pub struct SourceFile<'a> {
    pub path: &'a Path,
    pub text: &'a str,
    pub line_offsets: Vec<usize>
}

#[derive(Copy, Clone)]
pub struct Location {
    pub offset: usize,
    pub line_index: usize,
    pub column_index: usize
}

#[derive(Copy, Clone)]
pub struct Segment {
    pub start: Location,
    pub end: Location
}

impl<'a> Source<'a> {
    pub fn text(&self) -> &'a str {
        &self.file.text[self.segment.start.offset .. self.segment.end.offset]
    }
}

impl<'a> Display for Source<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(&format!("{:?}({})", self.file.path, self.segment.start))
    }
}

impl<'a> Debug for Source<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(self.text())
    }
}

impl Location {
    pub fn new() -> Location {
        Location { offset: 0, line_index: 0, column_index: 0 }
    }
}

impl Display for Location {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(&format!("{}:{}", self.line_index + 1, self.column_index + 1))
    }
}
