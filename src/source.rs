use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use std::error::Error;
use std::iter::once;
use std::rc::Rc;

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

#[derive(Clone)]
pub struct Source {
    pub file: Rc<SourceFile>,
    pub range: Range
}

impl Source {
    pub fn text(&self) -> &str {
        &self.file.text[self.range.start .. self.range.end]
    }

    pub fn extend(&self, end: usize) -> Source {
        assert!(end >= self.range.end);
        assert!(end <= self.file.text.len());
        Source {
            file: self.file.clone(),
            range: Range {
                start: self.range.start,
                end
            }
        }
    }
}

impl Display for Source {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "{:?}({:?})", self.file.path, self.file.position(self.range.start).unwrap())
    }
}

impl Debug for Source {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(self.text())
    }
}

pub struct SourceFile {
    pub path: String,
    pub text: String,
    pub bom_length: usize,
    pub lines: Vec<Range>,
}

impl<'a> SourceFile {
    pub fn read(path: &str) -> Result<String, Box<Error>> {
        use std::fs::File;
        use std::io::Read;
        let mut file = File::open(path)?;
        let mut text = String::new();
        file.read_to_string(&mut text)?;
        Ok(text)
    }

    pub fn new(text: String, path: String) -> Result<SourceFile, Box<Error>> {
        let bom_length = Self::skip_byte_order_mark(&text, &path)?;
        let lines = Self::collect_lines(&text, bom_length);
        Ok(SourceFile {
            path,
            text,
            bom_length,
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
        error!("{:?}: offset {} is out of range", self.path, offset)
    }

    fn skip_byte_order_mark(text: &'a str, path: &str) -> Result<usize, Box<Error>> {
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
        Ok(bom_length)
    }

    fn collect_lines(text: &str, start_offset: usize) -> Vec<Range> {
        let mut lines = vec![];
        let mut line_start = start_offset;
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
