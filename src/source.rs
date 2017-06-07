use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use std::path::{Path, PathBuf};
use std::error::Error;

#[derive(Copy, Clone)]
pub struct Source<'a> {
    pub file: &'a SourceFile,
    pub segment: Segment
}

pub struct SourceFile {
    path: PathBuf,
    text: String,
    bom_length: usize,
    pub size: usize,
    pub line_offsets: Vec<usize>
}

impl SourceFile {
    pub fn new(path: &Path) -> Result<SourceFile, Box<Error>> {
        let text = Self::read_file(path)?;
        let size = text.len();
        let bom_length = Self::skip_byte_order_mark(&text, path)?;
        let line_offsets = Self::find_line_offsets(&text[bom_length..]);
        Ok(SourceFile {
            path: path.to_owned(),
            text,
            bom_length,
            size,
            line_offsets
        })
    }

    pub fn path(&self) -> &Path {
        self.path.as_path()
    }

    pub fn text(&self) -> &str {
        &self.text[self.bom_length..]
    }

    fn read_file(path: &Path) -> Result<String, Box<Error>> {
        use std::fs::File;
        use std::io::Read;
        let mut file = File::open(path)?;
        let mut text = String::new();
        file.read_to_string(&mut text)?;
        Ok(text)
    }

    fn skip_byte_order_mark(text: &str, path: &Path) -> Result<usize, String> {
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

    fn find_line_offsets(text: &str) -> Vec<usize> {
        let mut line_offsets = vec![];
        line_offsets.push(0);
        for (offset, character) in text.char_indices() {
            if character == '\n' {
                line_offsets.push(offset + 1);
            }
        }
        line_offsets
    }
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
        &self.file.text()[self.segment.start.offset .. self.segment.end.offset]
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
