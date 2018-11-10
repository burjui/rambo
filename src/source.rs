use std::error::Error;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::Deref;

use crate::unique_rc::UniqueRc;

#[derive(Copy, Clone)]
crate struct Position {
    line: usize,
    column: usize
}

impl Debug for Position {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&format!("{}:{}", self.line + 1, self.column + 1))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
crate struct Range {
    start: usize,
    end: usize
}

impl Range {
    crate fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    crate fn start(&self) -> usize { self.start }
    crate fn end(&self) -> usize { self.end }
}

#[derive(Clone, PartialEq, Eq, Hash)]
crate struct Source {
    file: SourceFileRef,
    range: Range
}

impl Source {
    crate fn new(file: SourceFileRef, range: Range) -> Self {
        Self { file, range }
    }

    crate fn file(&self) -> &SourceFileRef { &self.file }
    crate fn range(&self) -> &Range { &self.range }
}

// TODO use Source::new()

impl Source {
    crate fn text(&self) -> &str {
        &self.file.text[self.range.start .. self.range.end]
    }

    crate fn extend(&self, until: &Source) -> Source {
        assert_eq!(self.file, until.file);
        let end = until.range.end;
        assert!(end >= self.range.end);
        assert!(end <= self.file.text.len());
        Source::new(self.file.clone(), Range::new(self.range.start, end))
    }
}

impl Display for Source {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}({:?})", self.file.path, self.file.position(self))
    }
}

impl Debug for Source {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.text())
    }
}

crate struct SourceFile {
    path: String,
    text: String,
    lines: Vec<Range>,
    size: usize,
}

impl SourceFile {
    crate fn load(path: &str) -> Result<SourceFileRef, Box<dyn Error>> {
        use std::fs::File;
        use std::io::Read;
        use std::io::BufReader;

        let file = File::open(path)?;
        let mut buf_reader = BufReader::new(file);
        let mut data = Vec::new();
        buf_reader.read_to_end(&mut data)?;
        let size = data.len();
        let text = Self::decode(&data, path)?;
        let lines = Self::collect_lines(&text);
        Ok(SourceFileRef::from(SourceFile {
            path: path.to_owned(),
            text,
            lines,
            size
        }))
    }

    #[cfg(test)]
    crate fn empty(name: String) -> SourceFileRef {
        SourceFileRef::from(SourceFile {
            path: name,
            text: "".to_owned(),
            lines: vec![ Range::new(0, 0) ],
            size: 0
        })
    }

    crate fn path(&self) -> &str { &self.path }
    crate fn text(&self) -> &str { &self.text }
    crate fn lines(&self) -> &[Range] { &self.lines }
    crate fn size(&self) -> usize { self.size }

    fn decode(data: &[u8], path: &str) -> Result<String, Box<dyn Error>> {
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
        Ok(from_utf8(&data[offset..]).map(str::to_owned)?)
    }

    fn detect_bom(data: &[u8]) -> Option<&'static BOM> {
        const BOMS: [BOM; 5] = [ BOM::UTF8, BOM::UTF32LE, BOM::UTF32BE, BOM::UTF16LE, BOM::UTF16BE ];
        BOMS.iter().find(|bom| data.starts_with(bom.bytes()))
    }

    fn collect_lines(text: &str) -> Vec<Range> {
        use std::iter::once;

        let mut lines = vec![];
        let mut line_start = 0;
        let eof = (text.len(), '\n');
        for (offset, character) in text.char_indices().chain(once(eof)) {
            if character == '\n' {
                lines.push(Range::new(line_start, offset));
                line_start = offset + 1;
            }
        }
        lines
    }
}

impl Debug for SourceFile {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.path, formatter)
    }
}

crate type SourceFileRef = UniqueRc<SourceFile>;

impl SourceFileRef {
    crate fn position(&self, source: &Source) -> Position {
        assert_eq!(&source.file, self);
        let offset = source.range.start;
        for (index, line) in self.lines.iter().enumerate() {
            if offset <= line.end {
                return Position {
                    line: index,
                    column: offset - line.start
                }
            }
        }
        unreachable!("{:?}: offset {} is out of range", self.path, offset)
    }
}

impl Debug for SourceFileRef {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.deref(), formatter)
    }
}

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
