use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::iter::FromIterator;
use std::iter::once;

use itertools::Itertools;
use multimap::MultiMap;
use petgraph::graph::DiGraph;

use crate::codegen::SSAId;
use crate::codegen::SSAOp;
use crate::codegen::SSAStatement;

#[cfg(test)] mod tests;

crate type ControlFlowGraph<'a> = DiGraph<CFGNode<'a>, ()>;

crate enum CFGNode<'a> {
    Entry,
    Exit,
    BasicBlock(&'a [SSAStatement])
}

impl<'a> fmt::Debug for CFGNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CFGNode::Entry => f.write_str("entry"),
            CFGNode::Exit => f.write_str("exit"),
            CFGNode::BasicBlock(ssa) => write!(f, "{:?}", ssa.iter().format("\n"))
        }
    }
}

crate fn build_control_flow_graph(ssa: &[SSAStatement]) -> ControlFlowGraph<'_> {
    let mut graph = ControlFlowGraph::new();
    let entry = graph.add_node(CFGNode::Entry);
    let exit = graph.add_node(CFGNode::Exit);

    let target_indices = HashMap::<SSAId, usize>::from_iter(
        ssa.iter().enumerate().map(|(index, statement)| (statement.target.id.clone(), index)));
    let mut leaders = vec![0usize];
    let mut edges = Vec::<(&SSAId, &SSAId)>::new();
    let mut call_next = MultiMap::<&SSAId, &SSAStatement>::new();
    let mut functions = HashSet::<&SSAId>::new();
    let mut current_function = None::<&SSAId>;
    let mut from_entry = None;
    let mut to_exit = None;
    for (i, statement) in ssa.iter().enumerate() {
        if i == 0 {
            from_entry = Some(i);
        }
        match &statement.op {
            SSAOp::Br(target) => {
                leaders.push(i + 1);
                let target_index = target_indices[target];
                leaders.push(target_index);
                edges.push((&statement.target.id, target));
                edges.push((&ssa[target_index - 1].target.id, target));
                if let SSAOp::Cbz(_, _) = &statement.op {
                    if let Some(next) = ssa.get(i + 1) {
                        edges.push((&statement.target.id, &next.target.id));
                    }
                }
            },
            SSAOp::Cbz(_, target) => {
                let target_index = target_indices[target];
                leaders.push(target_index);
                leaders.push(i + 1);
                edges.push((&statement.target.id, target));
                if let SSAOp::Cbz(_, _) = &statement.op {
                    if let Some(next) = ssa.get(i + 1) {
                        edges.push((&statement.target.id, &next.target.id));
                    }
                }
            },
            SSAOp::End(_) | SSAOp::Call(_, _) | SSAOp::Return(_) => {
                leaders.push(i + 1);
                match &statement.op {
                    SSAOp::End(_) => to_exit = Some(i),
                    SSAOp::Call(target, _) => {
                        functions.insert(target);
                        edges.push((&statement.target.id, target));
                        if let Some(next) = ssa.get(i + 1) {
                            call_next.insert(target, next);
                        }
                    },
                    SSAOp::Return(_) => {
                        if let Some(function) = current_function {
                            if let Some(next) = call_next.get_vec(function) {
                                for next_statement in next {
                                    edges.push((&statement.target.id, &next_statement.target.id))
                                }
                            }
                            current_function = None;
                        }
                    },
                    _ => ()
                }
            },
            SSAOp::Label => {
                let label = &statement.target.id;
                if current_function.is_none() && functions.contains(label) {
                    current_function = Some(label);
                }
                leaders.push(i);
            },
            _ => ()
        }
    }
    leaders.sort_unstable();
    leaders.dedup();
    let blocks_by_statement_indices = leaders.into_iter().tuple_windows()
        .flat_map(|(leader, next)| {
            let node = graph.add_node(CFGNode::BasicBlock(&ssa[leader .. next]));
            once(node).cycle().take(next - leader)
        })
        .collect::<Vec<_>>();
    from_entry.map(|i| graph.add_edge(entry, blocks_by_statement_indices[i], ()));
    to_exit.map(|i| graph.add_edge(blocks_by_statement_indices[i], exit, ()));
    for (from, to) in edges {
        graph.add_edge(blocks_by_statement_indices[target_indices[from]], blocks_by_statement_indices[target_indices[to]], ());
    }
    graph
}
