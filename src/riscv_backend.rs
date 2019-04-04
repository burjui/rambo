use std::error::Error;
use std::fmt;
use std::mem::size_of_val;

use petgraph::stable_graph::NodeIndex;

use crate::ir::ControlFlowGraph;
use crate::utils::intersection;

pub(crate) fn generate(cfg: &ControlFlowGraph) -> RICSVImage {
    Backend::new(cfg).generate()
}

pub(crate) struct RICSVImage {
    pub(crate) code: Vec<u32>,
    pub(crate) code_base_address: u32,
    pub(crate) data: Vec<u8>,
    pub(crate) data_base_address: u32,
}

impl RICSVImage {
    fn empty() -> Self {
        Self {
            code: Vec::new(),
            code_base_address: 0,
            data: Vec::new(),
            data_base_address: 0,
        }
    }

    pub(crate) fn verify(&self) -> Result<(), ImageOverlapError> {
        let code_range = self.code_base_address .. (self.code_base_address + size_of_val(&self.code[..]) as u32);
        let data_range = self.data_base_address .. (self.data_base_address + size_of_val(&self.data[..]) as u32);
        if intersection(&code_range, &data_range).is_none() {
            Ok(())
        } else {
            Err(ImageOverlapError)
        }
    }
}

#[derive(Debug)]
pub(crate) struct ImageOverlapError;

impl Error for ImageOverlapError {}

impl fmt::Display for ImageOverlapError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[test]
fn range_intersection() {
    test_intersection!(0..10, 10..20, None);
    test_intersection!(5..15, 10..20, Some(10..15));
    test_intersection!(10..20, 12..15, Some(12..15));
}

struct Backend<'a> {
    cfg: &'a ControlFlowGraph,
    image: RICSVImage,
}

impl<'a> Backend<'a> {
    fn new(cfg: &'a ControlFlowGraph) -> Self {
        Self {
            cfg,
            image: RICSVImage::empty(),
        }
    }

    fn generate(mut self) -> RICSVImage {
        let mut next_block = Some(self.cfg.entry_block);
        while let Some(block) = next_block {
            next_block = self.generate_block(block);
        }
        self.image
    }

    fn generate_block(&mut self, _block: NodeIndex) -> Option<NodeIndex> {
        unimplemented!()
    }
}
