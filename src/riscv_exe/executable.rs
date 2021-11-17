use bincode::{Decode, Encode};

#[derive(Encode, Decode)]
pub struct Executable {
    pub code: Vec<u8>,
    pub data: Vec<u8>,
    pub comments: Vec<(usize, String)>,
    pub relocations: Vec<Relocation>,
    pub entry: usize,
}

impl Executable {
    pub fn new() -> Self {
        Executable {
            code: Vec::new(),
            data: Vec::new(),
            comments: Vec::new(),
            relocations: Vec::new(),
            entry: 0,
        }
    }

    pub fn add_comment(&mut self, offset: usize, comment: &str) {
        match self
            .comments
            .binary_search_by_key(&offset, |(offset, _)| *offset)
        {
            Ok(index) => {
                let (_, existing_comment) = &mut self.comments[index];
                existing_comment.push('\n');
                existing_comment.push_str(comment);
            }

            Err(index) => self.comments.insert(index, (offset, comment.to_string())),
        }
    }

    pub fn comment_at(&self, offset: usize) -> Option<&str> {
        match self
            .comments
            .binary_search_by_key(&offset, |(offset, _)| *offset)
        {
            Ok(index) => {
                let (_, comment) = &self.comments[index];
                Some(comment.as_str())
            }

            Err(_) => None,
        }
    }
}

impl Default for Executable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Encode, Decode)]
pub enum RelocationKind {
    Function,
    DataLoad,
    DataStore,
}

#[derive(Encode, Decode)]
pub struct Relocation {
    pub offset: usize,
    pub target: usize,
    pub kind: RelocationKind,
}

impl Relocation {
    pub fn new(offset: usize, target: usize, kind: RelocationKind) -> Self {
        Self {
            offset,
            target,
            kind,
        }
    }
}
