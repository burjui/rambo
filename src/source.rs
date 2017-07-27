use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use std::path::Path;
use std::error::Error;
use std::iter::once;

#[derive(Copy, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize
}

impl Debug for Position {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(&format!("{}:{}", self.line + 1, self.column + 1))
    }
}

#[derive(Copy, Clone)]
pub struct Range {
    pub start: usize,
    pub end: usize
}

#[derive(Copy, Clone)]
pub struct Source<'a> {
    pub file: &'a SourceFile<'a>,
    pub range: Range
}

impl<'a> Source<'a> {
    pub fn text(&self) -> &'a str {
        &self.file.text[self.range.start .. self.range.end]
    }

    pub fn extend(&self, end: usize) -> Source<'a> {
        assert!(end >= self.range.end);
        assert!(end <= self.file.text.len());
        Source {
            file: self.file,
            range: Range {
                start: self.range.start,
                end
            }
        }
    }
}

impl<'a> Display for Source<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "{:?}({:?})", self.file.path.to_string_lossy(), self.file.position(self.range.start).unwrap())
    }
}

impl<'a> Debug for Source<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(self.text())
    }
}

pub struct SourceFile<'a> {
    pub path: &'a Path,
    pub text: &'a str,
    pub lines: Vec<Range>,
}

impl<'a> SourceFile<'a> {
    pub fn read(path: &Path) -> Result<String, Box<Error>> {
        use std::fs::File;
        use std::io::Read;
        let mut file = File::open(path)?;
        let mut text = String::new();
        file.read_to_string(&mut text)?;
        Ok(text)
    }

    pub fn new(text: &'a str, path: &'a Path) -> Result<SourceFile<'a>, Box<Error>> {
        let text = Self::skip_byte_order_mark(text, path)?;
        let lines = Self::collect_lines(text);
        Ok(SourceFile {
            text,
            path,
            lines
        })
    }

    pub fn position(&self, offset: usize) -> Result<Position, Box<Error>> {
        for (index, line) in self.lines.iter().enumerate() {
            if offset <= line.end {
                return Ok(Position {
                    line: index,
                    column: offset - line.start
                })
            }
        }
        error!("{:?}: offset {} is out of range", self.path.to_string_lossy(), offset)
    }

    pub fn empty_source(&self) -> Source {
        Source {
            file: self,
            range: Range {
                start: 0,
                end: 0
            }
        }
    }

    fn skip_byte_order_mark(text: &'a str, path: &Path) -> Result<&'a str, Box<Error>> {
        const UTF8_BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];
        const BOMS: &[(&[u8], &str)] = &[
            (&[0x00, 0x00, 0xFE, 0xFF], "UTF-32BE"),
            (&[0xFF, 0xFE, 0x00, 0x00], "UTF-32LE"),
            (&[0xFF, 0xFE], "UTF-16LE"),
            (&[0xFE, 0xFF], "UTF-16BE"),
            (&UTF8_BOM, "UTF8")
        ];
        let text_bytes = text.as_bytes();
        let bom_length = BOMS.iter()
            .find(|&&(bom, _)| text_bytes.starts_with(bom))
            .map_or(Ok(0), |&(bom, bom_name)|
                if bom == UTF8_BOM {
                    Ok(bom.len())
                } else {
                    Err(format!("{:?}: this file has a {} byte order mark at the beginning, \
                                but only UTF-8 encoding is supported", path, bom_name))
                }
            )?;
        Ok(&text[bom_length..])
    }

    fn collect_lines(text: &str) -> Vec<Range> {
        let mut lines = vec![];
        let mut line_start = 0;
        let eof = (text.len(), '\n');
        for (offset, character) in text.char_indices().chain(once(eof)) {
            if character == '\n' {
                let range = Range {
                    start: line_start,
                    end: offset
                };
                lines.push(range);
                line_start = offset + 1;
            }
        }
        lines
    }
}
