use std::{
    fmt::{
        Debug,
        Display,
    },
    ops::{
        Index,
        IndexMut,
    },
};

use crate::slab::{
    Slab,
    SlabIndex,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default, Hash, PartialOrd, Ord)]
pub(crate) struct NodeIndex(SlabIndex);

impl Display for NodeIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub(crate) struct EdgeIndex(SlabIndex);

#[derive(PartialEq)]
pub(crate) enum Direction {
    Incoming,
    Outgoing,
}

#[derive(Clone, Copy)]
pub(crate) struct Edge {
    pub(crate) source: NodeIndex,
    pub(crate) target: NodeIndex,
}

#[derive(Clone)]
pub(crate) struct StableGraph<NodeData> {
    nodes: Slab<NodeData>,
    edges: Slab<Edge>,
}

impl<NodeData> StableGraph<NodeData> {
    pub(crate) fn new() -> Self {
        Self {
            nodes: Slab::new(),
            edges: Slab::new(),
        }
    }

    pub(crate) fn add_node(&mut self, node: NodeData) -> NodeIndex {
        NodeIndex(self.nodes.insert(node))
    }

    pub(crate) fn add_edge(&mut self, source: NodeIndex, target: NodeIndex) -> EdgeIndex {
        EdgeIndex(self.edges.insert(Edge { source, target }))
    }

    pub(crate) fn remove_node(&mut self, node: NodeIndex) {
        self.nodes.remove(node.0);
        let involved_edges = self
            .edges
            .iter()
            .filter_map(|(edge_index, edge)| {
                if edge.source == node || edge.target == node {
                    Some(edge_index)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        for edge in involved_edges {
            self.edges.remove(edge);
        }
    }

    pub(crate) fn edges_directed(
        &self,
        node: NodeIndex,
        direction: Direction,
    ) -> impl Iterator<Item = Edge> + '_ {
        self.edges.iter().filter_map(move |(_, edge)| {
            if node == edge.source && direction == Direction::Outgoing
                || node == edge.target && direction == Direction::Incoming
            {
                Some(*edge)
            } else {
                None
            }
        })
    }

    pub(crate) fn edge_indices(&self) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.edges.iter().map(|(index, _)| EdgeIndex(index))
    }

    pub(crate) fn node_indices(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.nodes.iter().map(|(index, _)| NodeIndex(index))
    }

    pub(crate) fn has_edge(&self, from: NodeIndex, to: NodeIndex) -> bool {
        self.edges
            .iter()
            .any(|(_, edge)| edge.source == from && edge.target == to)
    }
}

impl<NodeData> Index<NodeIndex> for StableGraph<NodeData> {
    type Output = NodeData;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.nodes[index.0]
    }
}

impl<NodeData> IndexMut<NodeIndex> for StableGraph<NodeData> {
    fn index_mut(&mut self, index: NodeIndex) -> &mut Self::Output {
        &mut self.nodes[index.0]
    }
}

impl<NodeData> Index<EdgeIndex> for StableGraph<NodeData> {
    type Output = Edge;

    fn index(&self, index: EdgeIndex) -> &Self::Output {
        &self.edges[index.0]
    }
}

impl<NodeData> IndexMut<EdgeIndex> for StableGraph<NodeData> {
    fn index_mut(&mut self, index: EdgeIndex) -> &mut Self::Output {
        &mut self.edges[index.0]
    }
}
