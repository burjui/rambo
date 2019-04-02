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

use crate::frontend::get_statement_operands;
use crate::ir::{BasicBlock, FunctionMap, StatementLocation};
use crate::ir::BasicBlockGraph;
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::value_storage::ValueId;
use crate::ir::value_storage::ValueStorage;

pub(crate) fn remove_dead_code(
    entry_block: NodeIndex,
    program_result: &mut ValueId,
    values: &mut ValueStorage,
    graph: &mut BasicBlockGraph,
    definitions: &mut HashMap<ValueId, StatementLocation>,
    functions: &mut FunctionMap)
{
    let mut value_usage = compute_value_usage(*program_result, values, graph, definitions);
    remove_unused_definitions(&mut value_usage, values, graph, definitions);
    remove_empty_blocks(graph);
    merge_consecutive_basic_blocks(entry_block, values, graph, &mut value_usage, definitions);
    remove_unused_definitions(&mut value_usage, values, graph, definitions);
//    rename_idents(values, graph, definitions, program_result, &mut ident_usage);
    remove_unused_values(&definitions, values, functions);
}

fn compute_value_usage(
    program_result: ValueId,
    values: &mut ValueStorage,
    graph: &mut BasicBlockGraph,
    definitions: &mut HashMap<ValueId, StatementLocation>)
    -> HashMap<ValueId, usize>
{
    let mut value_usage = HashMap::from_iter(
        definitions
            .keys()
            .cloned()
            .zip(repeat(0)));
    let used_values = graph
        .node_indices()
        .flat_map(|node| graph[node].iter())
        .flat_map(|statement| get_statement_operands(&values, statement))
        .chain(once(program_result));
    for value_id in used_values {
        *value_usage.get_mut(&value_id).unwrap() += 1;
    }
    value_usage
}

fn remove_unused_definitions(
    value_usage: &mut HashMap<ValueId, usize>,
    values: &mut ValueStorage,
    graph: &mut BasicBlockGraph,
    definitions: &mut HashMap<ValueId, StatementLocation>)
{
    let unused_values = value_usage
        .iter()
        .filter_map(|(value_id, usages)| match usages {
            0 => Some(*value_id),
            _ => None
        })
        .collect_vec();
    for value_id in unused_values {
        unuse(value_id, graph, values, value_usage, definitions);
    }

    let (unused_values, dead_code): (Vec<_>, Vec<_>) = value_usage
        .iter()
        .filter_map(|(value_id, usage)| match usage {
            0 => Some((*value_id, definitions[value_id])),
            _ => None,
        })
        .unzip();
    for value_id in unused_values {
        definitions.remove(&value_id);
        value_usage.remove(&value_id);
    }
    remove_statements(dead_code, graph);
}

fn unuse(
    value_id: ValueId,
    graph: &BasicBlockGraph,
    values: &ValueStorage,
    value_usage: &mut HashMap<ValueId, usize>,
    definitions: &mut HashMap<ValueId, StatementLocation>)
{
    let usage_count = value_usage.get_mut(&value_id).unwrap();
    if *usage_count > 0 {
        *usage_count -= 1;
    }

    if *usage_count == 0 {
        let location = &definitions[&value_id];
        let statement = &graph[location.block][location.index];
        for operand in get_statement_operands(&values, statement) {
            unuse(operand, graph, values, value_usage, definitions);
        }
    }
}

//fn rename_idents(
//    values: &mut ValueStorage,
//    graph: &mut BasicBlockGraph,
//    definitions: &mut HashMap<VarId, StatementLocation>,
//    program_result: &mut VarId,
//    ident_usage: &mut HashMap<VarId, usize>)
//{
//    let mut idgen = IdentGenerator::new();
//    let ident_rename_map: HashMap<VarId, VarId> = HashMap::from_iter({
//        definitions
//            .iter()
//            .sorted_by_key(|(var_id, _)| var_id)
//            .map(|(ident, _)| (*ident, idgen.next_id()))
//    });
//    let blocks = graph
//        .node_indices()
//        .collect_vec();
//    for block in blocks {
//        for statement in graph[block].iter_mut() {
//            for ident in get_statement_idents_mut(values, statement) {
//                let renamed = ident_rename_map[&ident];
//                *ident = renamed;
//            };
//        }
//    }
//
//    let old_ident_usage = replace(ident_usage, HashMap::new());
//    *ident_usage = HashMap::from_iter(
//        old_ident_usage
//            .into_iter()
//            .map(|(from, usage)| (ident_rename_map[&from], usage)));
//
//    let old_definitions = replace(definitions, HashMap::new());
//    *definitions = HashMap::from_iter(
//        old_definitions
//            .into_iter()
//            .map(|(from, definition)| (ident_rename_map[&from], definition)));
//    *program_result = ident_rename_map[program_result];
//}

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
    value_usage: &mut HashMap<ValueId, usize>,
    definitions: &mut HashMap<ValueId, StatementLocation>)
{
    let successors = graph
        .edges_directed(block, Direction::Outgoing)
        .map(|edge| edge.target())
        .collect_vec();
    for successor in &successors {
        merge_consecutive_basic_blocks(*successor, values, graph, value_usage, definitions);
    }
    if successors.len() == 1 {
        let successor = successors[0];
        if graph.edges_directed(successor, Direction::Incoming).count() == 1 {
            if let Some(Statement::CondJump(condition, _, _)) = graph[block].find_last() {
                unuse(*condition, graph, values, value_usage, definitions);
                graph[block].pop();
            }

            let successor_basic_block = replace(&mut graph[successor], BasicBlock::new());
            let basic_block = &mut graph[block];
            for statement in successor_basic_block.into_iter() {
                if let Statement::Definition(value_id) = statement {
                    let definition = definitions.get_mut(&value_id).unwrap();
                    definition.block = block;
                    definition.index = basic_block.push(statement);
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

fn remove_unused_values(
    definitions: &HashMap<ValueId, StatementLocation>,
    values: &mut ValueStorage,
    functions: &mut FunctionMap)
{
    let values_used = definitions
        .keys()
        .collect::<HashSet<_>>();
    for value_index in values.indices() {
        if !values_used.contains(value_index) {
            if let Value::Function(fn_id) = &values[*value_index] {
                functions.remove(fn_id);
            }
        }
    }
    values.retain(|value_index| values_used.contains(&value_index));
}

fn remove_statements(locations: Vec<StatementLocation>, graph: &mut BasicBlockGraph) {
    for location in locations {
        graph[location.block].remove(location.index);
    }
}
