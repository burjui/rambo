use std::mem::replace;

use crate::ir::Ident;

pub(crate) struct IdentGenerator {
    next_id: usize,
}

impl IdentGenerator {
    pub(crate) fn new() -> Self {
        Self {
            next_id: 0
        }
    }

    pub(crate) fn new_id(&mut self) -> Ident {
        let next_id = self.next_id + 1;
        Ident(replace(&mut self.next_id, next_id))
    }

    pub(crate) fn id_count(self) -> usize {
        self.next_id
    }
}
