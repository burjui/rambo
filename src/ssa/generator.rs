use std::mem::replace;

use crate::ssa::SSAId;
use crate::ssa::SSAIdName;

crate struct SSAIdGenerator {
    next_id: usize,
}

impl SSAIdGenerator {
    crate fn new() -> Self {
        Self { next_id: 0 }
    }

    crate fn new_id(&mut self, name: SSAIdName) -> SSAId {
        SSAId {
            name,
            unique_id: self.unique_id()
        }
    }

    crate fn next_version(&mut self, id: &SSAId) -> SSAId {
        match &id.name {
            SSAIdName::Binding { binding, version } => SSAId {
                name: SSAIdName::Binding {
                    binding: binding.clone(),
                    version: version + 1
                },
                unique_id: self.unique_id()
            },
            _ => panic!("trying to get next version of an SSA id that is not tied to any binding: {:?}", id)
        }
    }

    fn unique_id(&mut self) -> usize {
        let new_id = self.next_id + 1;
        replace(&mut self.next_id, new_id)
    }
}

#[test]
fn test() {
    let mut generator = SSAIdGenerator::new();
    let id1 = generator.new_id(SSAIdName::Label(0));
    let id2 = generator.new_id(SSAIdName::Label(0));
    assert_ne!(id1, id2);
    assert_eq!(id1.name, id2.name);

    let id3 = generator.new_id(SSAIdName::Tmp(0));
    assert_ne!(id3.name, id2.name);
}
