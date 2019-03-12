use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::iter::once;
use std::iter::repeat;
use std::mem::replace;

use itertools::Itertools;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;

use crate::frontend::get_statement_ident_operands;
use crate::frontend::get_statement_idents_mut;
use crate::frontend::IdentDefinition;
use crate::frontend::StatementLocation;
use crate::ir::BasicBlock;
use crate::ir::BasicBlockGraph;
use crate::ir::IdentGenerator;
use crate::ir::Statement;
use crate::ir::value_storage::ValueIndex;
use crate::ir::value_storage::ValueStorage;
use crate::ir::VarId;

pub(crate) fn remove_dead_code(
    entry_block: NodeIndex,
    mut program_result: VarId,
    values: &mut ValueStorage,
    graph: &mut BasicBlockGraph,
    mut definitions: HashMap<VarId, IdentDefinition>)
{
    let mut ident_usage = compute_ident_usage(program_result, values, graph, &mut definitions);
    remove_unused_definitions(&mut ident_usage, values, graph, &mut definitions);
    remove_empty_blocks(graph);
    merge_consecutive_basic_blocks(entry_block, values, graph, &mut ident_usage, &mut definitions);
    remove_unused_definitions(&mut ident_usage, values, graph, &mut definitions);
    rename_idents(values, graph, &mut definitions, &mut program_result, &mut ident_usage);
    remove_unused_values(&definitions, values);
}

fn compute_ident_usage(
    program_result: VarId,
    values: &mut ValueStorage,
    graph: &mut BasicBlockGraph,
    definitions: &mut HashMap<VarId, IdentDefinition>)
    -> HashMap<VarId, usize>
{
    let mut ident_usage = HashMap::from_iter(
        definitions
            .keys()
            .cloned()
            .zip(repeat(0)));
    let used_idents = graph
        .node_indices()
        .flat_map(|node| graph[node].iter())
        .flat_map(|statement| get_statement_ident_operands(&values, statement))
        .chain(once(program_result));
    for ident in used_idents {
        *ident_usage.get_mut(&ident).unwrap() += 1;
    }
    ident_usage
}

fn remove_unused_definitions(
    ident_usage: &mut HashMap<VarId, usize>,
    values: &mut ValueStorage,
    graph: &mut BasicBlockGraph,
    definitions: &mut HashMap<VarId, IdentDefinition>)
{
    let unused_idents = ident_usage
        .iter()
        .filter_map(|(ident, usages)| match usages {
            0 => Some(*ident),
            _ => None
        })
        .collect_vec();
    for ident in unused_idents {
        unuse(ident, graph, values, ident_usage, definitions);
    }

    let (unused_idents, dead_code): (Vec<_>, Vec<_>) = ident_usage
        .iter()
        .filter_map(|(ident, usage)| match usage {
            0 => Some((*ident, definitions[ident].location)),
            _ => None,
        })
        .unzip();
    for ident in unused_idents {
        definitions.remove(&ident);
        ident_usage.remove(&ident);
    }
    remove_statements(dead_code, graph);
}

fn unuse(
    ident: VarId,
    graph: &BasicBlockGraph,
    values: &ValueStorage,
    ident_usage: &mut HashMap<VarId, usize>,
    definitions: &mut HashMap<VarId, IdentDefinition>)
{
    let usage_count = ident_usage.get_mut(&ident).unwrap();
    if *usage_count > 0 {
        *usage_count -= 1;
    }

    if *usage_count == 0 {
        let location = &definitions[&ident].location;
        let statement = &graph[location.block][location.index];
        for ident in get_statement_ident_operands(&values, statement) {
            unuse(ident, graph, values, ident_usage, definitions);
        }
    }
}

fn rename_idents(
    values: &mut ValueStorage,
    graph: &mut BasicBlockGraph,
    definitions: &mut HashMap<VarId, IdentDefinition>,
    program_result: &mut VarId,
    ident_usage: &mut HashMap<VarId, usize>)
{
    let mut idgen = IdentGenerator::new();
    let ident_rename_map: HashMap<VarId, VarId> = HashMap::from_iter({
        definitions
            .iter()
            .sorted_by_key(|(_, definition)| definition.location)
            .map(|(ident, _)| (*ident, idgen.next_id()))
    });
    let blocks = graph
        .node_indices()
        .collect_vec();
    for block in blocks {
        for statement in graph[block].iter_mut() {
            for ident in get_statement_idents_mut(values, statement) {
                let renamed = ident_rename_map[&ident];
                *ident = renamed;
            };
        }
    }

    let old_ident_usage = replace(ident_usage, HashMap::new());
    *ident_usage = HashMap::from_iter(
        old_ident_usage
            .into_iter()
            .map(|(from, usage)| (ident_rename_map[&from], usage)));

    let old_definitions = replace(definitions, HashMap::new());
    *definitions = HashMap::from_iter(
        old_definitions
            .into_iter()
            .map(|(from, definition)| (ident_rename_map[&from], definition)));
    *program_result = ident_rename_map[program_result];
}

fn remove_empty_blocks(graph: &mut BasicBlockGraph) {
    let blocks = graph
        .node_indices()
        .collect_vec();
    for block in blocks {
        let is_empty = graph[block]
            .iter()
            .find(|statement| !matches!(statement, Statement::Comment(_)))
            .is_none();
        if is_empty {
            let sources = graph
                .edges_directed(block, Direction::Incoming)
                .map(|edge| edge.source())
                .collect_vec();
            let targets = graph
                .edges_directed(block, Direction::Outgoing)
                .map(|edge| edge.target())
                .collect_vec();
            if sources.len() == 1 && targets.len() == 1 {
                let source = sources[0];
                let target = targets[0];
                if graph.find_edge(source, target).is_none() {
                    graph.add_edge(source, target, ());
                }
                graph.remove_node(block);
            }
        }
    }
}

fn merge_consecutive_basic_blocks(
    block: NodeIndex,
    values: &ValueStorage,
    graph: &mut BasicBlockGraph,
    ident_usage: &mut HashMap<VarId, usize>,
    definitions: &mut HashMap<VarId, IdentDefinition>)
{
    let successors = graph
        .edges_directed(block, Direction::Outgoing)
        .map(|edge| edge.target())
        .collect_vec();
    for successor in &successors {
        merge_consecutive_basic_blocks(*successor, values, graph, ident_usage, definitions);
    }
    if successors.len() == 1 {
        let successor = successors[0];
        if graph.edges_directed(successor, Direction::Incoming).count() == 1 {
            if let Some(Statement::CondJump(condition, _, _)) = graph[block].find_last() {
                unuse(*condition, graph, values, ident_usage, definitions);
                graph[block].pop();
            }

            let successor_basic_block = replace(&mut graph[successor], BasicBlock::new());
            let basic_block = &mut graph[block];
            for statement in successor_basic_block.into_iter() {
                if let Statement::Definition { ident, .. } = statement {
                    let definition = definitions.get_mut(&ident).unwrap();
                    definition.location.block = block;
                    definition.location.index = basic_block.push(statement);
                }
            }

            let targets = graph
                .edges_directed(successor, Direction::Outgoing)
                .map(|edge| edge.target())
                .collect_vec();
            for target in targets {
                graph.add_edge(block, target, ());
            }
            graph.remove_node(successor);
        }
    }
}

fn remove_unused_values(definitions: &HashMap<VarId, IdentDefinition>, values: &mut ValueStorage) {
    let values_used: HashSet<ValueIndex> = definitions
        .values()
        .map(|definition| definition.value_index)
        .collect();
    values.retain(|value_index| values_used.contains(&value_index));
}

fn remove_statements(locations: Vec<StatementLocation>, graph: &mut BasicBlockGraph) {
    for location in locations {
        graph[location.block].remove(location.index);
    }
}
