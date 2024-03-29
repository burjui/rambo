use std::{
    iter::{once, repeat},
    mem::replace,
};

use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    ir::{
        get_statement_operands_mut, get_statement_value_operands, get_value_operands_mut,
        value_storage::{ValueId, ValueStorage},
        BasicBlock, ControlFlowGraph, FunctionMap, Statement, StatementLocation, Value,
    },
    stable_graph::{Direction, NodeIndex},
};

pub(crate) fn remove_dead_code(
    entry_block: NodeIndex,
    program_result: &mut ValueId,
    values: &mut ValueStorage,
    cfg: &mut ControlFlowGraph,
    definitions: &mut FxHashMap<ValueId, StatementLocation>,
    functions: &mut FunctionMap,
) {
    let mut value_usage = compute_value_usage(*program_result, values, cfg, definitions);
    remove_unused_definitions(&mut value_usage, values, cfg, definitions);
    remove_empty_blocks(cfg);
    merge_consecutive_basic_blocks(entry_block, values, cfg, &mut value_usage, definitions);
    remove_unused_definitions(&mut value_usage, values, cfg, definitions);
    remove_unused_values(definitions, values, functions, cfg, program_result);
}

fn compute_value_usage(
    program_result: ValueId,
    values: &ValueStorage,
    cfg: &ControlFlowGraph,
    definitions: &FxHashMap<ValueId, StatementLocation>,
) -> FxHashMap<ValueId, usize> {
    let mut value_usage = definitions
        .keys()
        .copied()
        .zip(repeat(0))
        .collect::<FxHashMap<_, _>>();
    let used_values = cfg
        .node_indices()
        .flat_map(|node| cfg[node].iter())
        .flat_map(|statement| get_statement_value_operands(values, statement))
        .chain(once(program_result));
    for value_id in used_values {
        *value_usage.get_mut(&value_id).unwrap() += 1;
    }
    value_usage
}

fn remove_unused_definitions(
    value_usage: &mut FxHashMap<ValueId, usize>,
    values: &ValueStorage,
    cfg: &mut ControlFlowGraph,
    definitions: &mut FxHashMap<ValueId, StatementLocation>,
) {
    let unused_values = value_usage
        .iter()
        .filter_map(|(value_id, usages)| match usages {
            0 => Some(*value_id),
            _ => None,
        })
        .collect_vec();
    for value_id in unused_values {
        disuse(value_id, cfg, values, value_usage, definitions);
    }

    let (unused_values, dead_code): (Vec<_>, Vec<_>) = value_usage
        .iter()
        .filter_map(|(value_id, usage)| match usage {
            0 => Some((*value_id, definitions[value_id])),
            _ => None,
        })
        .unzip();
    for value_id in unused_values {
        value_usage.remove(&value_id);
        match &values[value_id] {
            Value::Arg(_) => None,
            _ => definitions.remove(&value_id),
        };
    }
    remove_statements(dead_code, cfg);
}

fn disuse(
    value_id: ValueId,
    cfg: &ControlFlowGraph,
    values: &ValueStorage,
    value_usage: &mut FxHashMap<ValueId, usize>,
    definitions: &mut FxHashMap<ValueId, StatementLocation>,
) {
    let usage_count = value_usage.get_mut(&value_id).unwrap();
    if *usage_count > 0 {
        *usage_count -= 1;
    }

    if *usage_count == 0 {
        let location = &definitions[&value_id];
        let statement = &cfg[location.block][*location];
        for operand in get_statement_value_operands(values, statement) {
            disuse(operand, cfg, values, value_usage, definitions);
        }
    }
}

fn remove_empty_blocks(cfg: &mut ControlFlowGraph) {
    let blocks = cfg.node_indices().collect_vec();
    for block in blocks {
        let is_empty = !cfg[block]
            .iter()
            .any(|statement| !matches!(statement, Statement::Comment(_)));
        if is_empty {
            let sources = cfg
                .edges_directed(block, Direction::Incoming)
                .map(|edge| edge.source)
                .collect_vec();
            let targets = cfg
                .edges_directed(block, Direction::Outgoing)
                .map(|edge| edge.target)
                .collect_vec();
            if sources.len() == 1 && targets.len() == 1 {
                let source = sources[0];
                let target = targets[0];
                if !cfg.has_edge(source, target) {
                    cfg.add_edge(source, target);
                }
                cfg.remove_node(block);
            }
        }
    }
}

fn merge_consecutive_basic_blocks(
    block: NodeIndex,
    values: &ValueStorage,
    cfg: &mut ControlFlowGraph,
    value_usage: &mut FxHashMap<ValueId, usize>,
    definitions: &mut FxHashMap<ValueId, StatementLocation>,
) {
    let successors = cfg
        .edges_directed(block, Direction::Outgoing)
        .map(|edge| edge.target)
        .collect_vec();
    for successor in &successors {
        merge_consecutive_basic_blocks(*successor, values, cfg, value_usage, definitions);
    }
    if successors.len() == 1 {
        let successor = successors[0];
        if cfg.edges_directed(successor, Direction::Incoming).count() == 1 {
            if let Some(Statement::CondJump(condition, _, _)) = cfg[block].find_last() {
                disuse(*condition, cfg, values, value_usage, definitions);
                cfg[block].pop();
            }

            let successor_basic_block = replace(&mut cfg[successor], BasicBlock::new());
            let basic_block = &mut cfg[block];
            for statement in successor_basic_block.into_iter() {
                if let Statement::Definition(value_id) = statement {
                    let definition = definitions.get_mut(&value_id).unwrap();
                    definition.block = block;
                    definition.index = basic_block.push(statement);
                } else {
                    basic_block.push(statement);
                }
            }

            let targets = cfg
                .edges_directed(successor, Direction::Outgoing)
                .map(|edge| edge.target)
                .collect_vec();
            for target in targets {
                cfg.add_edge(block, target);
            }
            cfg.remove_node(successor);
        }
    }
}

fn remove_unused_values(
    definitions: &mut FxHashMap<ValueId, StatementLocation>,
    values: &mut ValueStorage,
    functions: &mut FunctionMap,
    cfg: &mut ControlFlowGraph,
    program_result: &mut ValueId,
) {
    for (_, value_id) in values.iter() {
        if !definitions.contains_key(&value_id) {
            if let Value::Function(fn_id, _) = &values[value_id] {
                functions.remove(fn_id);
            }
        }
    }

    let mut remap = FxHashMap::default();
    let mut new_definitions = FxHashMap::default();
    values.retain(
        |value_id| definitions.contains_key(&value_id),
        |from, to| {
            remap.insert(from, to);
            new_definitions.insert(to, definitions[&from]);
        },
    );
    for (value, _) in values.iter_mut() {
        for operand in get_value_operands_mut(value) {
            *operand = remap[operand];
        }
    }
    let blocks = cfg.node_indices().collect_vec();
    for block in blocks {
        let basic_block = &mut cfg[block];
        for statement in basic_block.iter_mut() {
            for operand in get_statement_operands_mut(statement) {
                *operand = remap[operand];
            }
        }
    }
    *definitions = new_definitions;
    *program_result = remap[program_result];
}

fn remove_statements(locations: Vec<StatementLocation>, cfg: &mut ControlFlowGraph) {
    for location in locations {
        cfg[location.block].remove(location);
    }
}
