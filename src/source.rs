use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use std::error::Error;
use std::rc::Rc;

#[derive(Copy, Clone)]
crate struct Position {
    crate line: usize,
    crate column: usize
}

impl Debug for Position {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FmtResult {
        formatter.write_str(&format!("{}:{}", self.line + 1, self.column + 1))
    }
}

#[derive(Copy, Clone)]
crate struct Range {
    crate start: usize,
    crate end: usize
}

#[derive(Clone)]
crate struct Source {
    crate file: Rc<SourceFile>,
    crate range: Range
}

impl Source {
    crate fn text(&self) -> &str {
        &self.file.text[self.range.start .. self.range.end]
    }

    // TODO replace usize argument with another Source
    crate fn extend(&self, end: usize) -> Source {
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
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FmtResult {
        write!(formatter, "{}({:?})", self.file.path, self.file.position(self.range.start).unwrap())
    }
}

impl Debug for Source {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FmtResult {
        formatter.write_str(self.text())
    }
}

crate struct SourceFile {
    crate path: String,
    crate text: String,
    crate lines: Vec<Range>,
}

#[derive(Debug)]
enum BOM { UTF8, UTF16LE, UTF16BE, UTF32LE, UTF32BE }

impl BOM {
    fn bytes(&self) -> &[u8] {
        match self {
            BOM::UTF8 => &[0xEF, 0xBB, 0xBF],
            BOM::UTF16LE => &[0xFF, 0xFE],
            BOM::UTF16BE => &[0xFE, 0xFF],
            BOM::UTF32LE => &[0xFF, 0xFE, 0x00, 0x00],
            BOM::UTF32BE => &[0x00, 0x00, 0xFE, 0xFF],
        }
    }

    fn name(&self) -> &str {
        match self {
            BOM::UTF8 => "UTF-8",
            BOM::UTF16LE => "UTF-16LE",
            BOM::UTF16BE => "UTF-16BE",
            BOM::UTF32LE => "UTF-32LE",
            BOM::UTF32BE => "UTF-32BE",
        }
    }
}

impl<'a> SourceFile {
    crate fn read(path: &str) -> Result<Vec<u8>, Box<dyn Error>> {
        use std::fs::File;
        use std::io::Read;
        use std::io::BufReader;

        let file = File::open(path)?;
        let mut buf_reader = BufReader::new(file);
        let mut data = Vec::new();
        buf_reader.read_to_end(&mut data)?;
        Ok(data)
    }

    crate fn new(path: &str, data: &[u8]) -> Result<SourceFile, Box<dyn Error>> {
        let text = Self::decode(&data, path)?;
        let lines = Self::collect_lines(&text);
        Ok(SourceFile {
            path: path.to_string(),
            text,
            lines
        })
    }

    crate fn position(&self, offset: usize) -> Result<Position, Box<dyn Error>> {
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

    fn decode(data: &'a [u8], path: &str) -> Result<String, Box<dyn Error>> {
        let offset = match Self::detect_bom(&data) {
            Some(bom) => if let BOM::UTF8 = bom {
                Ok(bom.bytes().len())
            } else {
                Err(format!("{:?}: encoding {} is not supported, convert the file to {}",
                            path, bom.name(), BOM::UTF8.name()))
            },
            None => Ok(0)
        }?;

        use std::str::from_utf8;
        Ok(from_utf8(&data[offset..]).map(str::to_string)?)
    }

    fn detect_bom(data: &[u8]) -> Option<&'static BOM> {
        const BOMS: [BOM; 5] = [ BOM::UTF8, BOM::UTF32LE, BOM::UTF32BE, BOM::UTF16LE, BOM::UTF16BE ];
        BOMS.into_iter().find(|bom| data.starts_with(bom.bytes()))
    }

    fn collect_lines(text: &str) -> Vec<Range> {
        use std::iter::once;

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
