//! Global value numbering implementation

use fixedbitset::FixedBitSet;
use multimap::MultiMap;
use petgraph::algo::DfsSpace;
use petgraph::algo::has_path_connecting;
use petgraph::graph::NodeIndex;
use petgraph::prelude::Direction::Incoming;
use petgraph::visit::EdgeRef;

use crate::ir::BasicBlockGraph;
use crate::ir::Ident;
use crate::ir::idgen::IdentGenerator;
use crate::ir::Value;

#[derive(Deref)]
pub(crate) struct Reused(bool);

pub(crate) struct GVN {
    values: MultiMap<Value, (NodeIndex, Ident)>,
}

impl GVN {
    pub(crate) fn new() -> Self {
        Self {
            values: MultiMap::new(),
        }
    }

    pub(crate) fn gvn(
        &mut self, graph: &BasicBlockGraph, block: NodeIndex, value: &Value,
        idgen: &mut IdentGenerator, )
        -> (Ident, Reused)
    {
        if let Value::Arg(_) = value {
            (idgen.new_id(), Reused(false))
        } else {
            let predecessors = graph
                .edges_directed(block, Incoming)
                .map(|edge| edge.source());
            let empty_vec = Vec::new();
            let instances = self.values.get_vec(value).unwrap_or(&empty_vec);
            let mut instance = None;
            for (source, ident) in instances.iter().rev() {
                if *source == block || has_path_to_all(graph, *source, predecessors.clone()) {
                    instance = Some(*ident);
                    break;
                }
            }
            instance
                .map(|ident| (ident, Reused(true)))
                .unwrap_or_else(|| {
                    let ident = idgen.new_id();
                    self.values.insert(value.clone(), (block, ident));
                    (ident, Reused(false))
                })
        }
    }
}

fn has_path_to_all(graph: &BasicBlockGraph, src: NodeIndex, dst: impl Iterator<Item = NodeIndex> + Clone) -> bool {
    let mut dst_copy = dst.clone();
    let mut dfs = DfsSpace::new(&**graph);
    dst.clone().next().is_some() && dst_copy.all(|dst| has_path(graph, src, dst, Some(&mut dfs)))
}

fn has_path(graph: &BasicBlockGraph, src: NodeIndex, dst: NodeIndex, dfs_space: Option<&mut DfsSpace<NodeIndex, FixedBitSet>>) -> bool {
    has_path_connecting(&**graph, src, dst, dfs_space)
}
