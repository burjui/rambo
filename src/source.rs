use std::{
    error::Error,
    fmt::{
        Debug,
        Display,
        Formatter,
    },
    ops::Range,
};

use crate::unique_rc::StaticRef;

#[derive(Copy, Clone)]
pub(crate) struct Position {
    line: usize,
    column: usize,
}

impl Debug for Position {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&format!("{}:{}", self.line + 1, self.column + 1))
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct Source {
    file: SourceFileRef,
    range: Range<usize>,
}

impl Source {
    pub(crate) const fn new(file: SourceFileRef, range: Range<usize>) -> Self {
        Self { file, range }
    }

    pub(crate) const fn file(&self) -> &SourceFileRef {
        &self.file
    }

    pub(crate) const fn range(&self) -> &Range<usize> {
        &self.range
    }
}

impl Source {
    pub(crate) fn text(&self) -> &str {
        &self.file.text[self.range.start..self.range.end]
    }

    pub(crate) fn extend(&self, until: &Self) -> Self {
        assert_eq!(self.file, until.file);
        let end = until.range.end;
        assert!(end >= self.range.end);
        assert!(end <= self.file.text.len());
        Self::new(self.file.clone(), self.range.start..end)
    }
}

impl Display for Source {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "{}({:?})",
            self.file.name,
            self.file.position(self)
        )
    }
}

impl Debug for Source {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.text())
    }
}

pub(crate) struct SourceFile {
    name: String,
    text: String,
    lines: Vec<Range<usize>>,
    size: usize,
}

impl SourceFile {
    pub(crate) fn load(path: &str) -> Result<SourceFileRef, Box<dyn Error>> {
        use std::{
            fs::File,
            io::{
                BufReader,
                Read,
            },
        };

        let file = File::open(path)?;
        let mut buf_reader = BufReader::new(file);
        let mut data = Vec::new();
        buf_reader.read_to_end(&mut data)?;
        let size = data.len();
        let text = Self::decode(&data, path)?;
        Ok(Self::from_text(path.to_owned(), text, size))
    }

    #[cfg(test)]
    pub(crate) fn create(name: String, text: &str) -> SourceFileRef {
        let size = text.len();
        Self::from_text(name, text.to_owned(), size)
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn text(&self) -> &str {
        &self.text
    }

    pub(crate) fn lines(&self) -> &[Range<usize>] {
        &self.lines
    }

    pub(crate) const fn size(&self) -> usize {
        self.size
    }

    fn from_text(name: String, text: String, size: usize) -> SourceFileRef {
        let lines = Self::collect_lines(&text);
        SourceFileRef::from(Self {
            name,
            text,
            lines,
            size,
        })
    }

    fn decode(data: &[u8], path: &str) -> Result<String, Box<dyn Error>> {
        use std::str::from_utf8;

        let offset = match Self::detect_bom(data) {
            Some(bom) => {
                if let Bom::UTF8 = bom {
                    Ok(bom.bytes().len())
                } else {
                    Err(format!(
                        "{:?}: encoding {} is not supported, convert the file to {}",
                        path,
                        bom.name(),
                        Bom::UTF8.name()
                    ))
                }
            }
            None => Ok(0),
        }?;
        Ok(from_utf8(&data[offset..]).map(str::to_owned)?)
    }

    fn detect_bom(data: &[u8]) -> Option<&'static Bom> {
        const BOMS: [Bom; 5] = [
            Bom::UTF8,
            Bom::UTF32LE,
            Bom::UTF32BE,
            Bom::UTF16LE,
            Bom::UTF16BE,
        ];
        BOMS.iter().find(|bom| data.starts_with(bom.bytes()))
    }

    fn collect_lines(text: &str) -> Vec<Range<usize>> {
        use std::iter::once;

        let mut lines = vec![];
        let mut line_start = 0;
        let eof = (text.len(), '\n');
        for (offset, character) in text.char_indices().chain(once(eof)) {
            if character == '\n' {
                lines.push(line_start..offset);
                line_start = offset + 1;
            }
        }
        lines
    }
}

impl Debug for SourceFile {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.name, formatter)
    }
}

pub(crate) type SourceFileRef = StaticRef<SourceFile>;

impl SourceFileRef {
    pub(crate) fn position(&self, source: &Source) -> Position {
        assert_eq!(&source.file, self);
        let offset = source.range.start;
        for (index, line) in self.lines.iter().enumerate() {
            if offset <= line.end {
                return Position {
                    line: index,
                    column: offset - line.start,
                };
            }
        }
        unreachable!("{:?}: offset {} is out of range", self.name, offset)
    }
}

enum Bom {
    UTF8,
    UTF16LE,
    UTF16BE,
    UTF32LE,
    UTF32BE,
}

impl Bom {
    fn bytes(&self) -> &[u8] {
        match self {
            Self::UTF8 => &[0xEF, 0xBB, 0xBF],
            Self::UTF16LE => &[0xFF, 0xFE],
            Self::UTF16BE => &[0xFE, 0xFF],
            Self::UTF32LE => &[0xFF, 0xFE, 0x00, 0x00],
            Self::UTF32BE => &[0x00, 0x00, 0xFE, 0xFF],
        }
    }

    fn name(&self) -> &str {
        match self {
            Self::UTF8 => "UTF-8",
            Self::UTF16LE => "UTF-16LE",
            Self::UTF16BE => "UTF-16BE",
            Self::UTF32LE => "UTF-32LE",
            Self::UTF32BE => "UTF-32BE",
        }
    }
}
