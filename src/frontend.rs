/*
The minimal SSA algorithm is based on the following paper:

Braun M., Buchwald S., Hack S., Leißa R., Mallon C., Zwinkau A.
(2013) Simple and Efficient Construction of Static Single Assignment Form.
In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg
*/

use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::empty;
use std::iter::FromIterator;
use std::iter::once;
use std::mem::replace;
#[cfg(test)] use std::rc::Rc;

use itertools::Itertools;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;

use crate::ir::BasicBlock;
use crate::ir::BasicBlockGraph;
use crate::ir::ControlFlowGraph;
use crate::ir::FnId;
use crate::ir::Ident;
use crate::ir::IdentGenerator;
use crate::ir::Phi;
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::value_storage::ValueIndex;
use crate::ir::value_storage::ValueStorage;
use crate::ir::Variable;
use crate::ir::VarId;
use crate::semantics::BindingKind;
use crate::semantics::BindingRef;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use crate::unique_rc::UniqueRc;
#[cfg(test)] use crate::utils::TestResult;

pub(crate) struct FrontEnd {
    name: String,
    enable_warnings: bool,
    include_comments: bool,
    graph: BasicBlockGraph,
    idgen: IdentGenerator<VarId>,
    function_idgen: IdentGenerator<FnId>,
    variables: HashMap<Variable, HashMap<NodeIndex, VarId>>,
    variables_read: HashSet<Variable>,
    values: ValueStorage,
    definitions: HashMap<VarId, IdentDefinition>,
    sealed_blocks: HashSet<NodeIndex>,
    incomplete_phis: HashMap<NodeIndex, HashMap<Variable, VarId>>,
    phi_users: HashMap<VarId, HashSet<VarId>>,
    entry_block: NodeIndex,
    undefined: VarId,
    undefined_users: Vec<StatementLocation>,
}

impl FrontEnd {
    pub(crate) fn new(name: &str) -> Self {
        let mut instance = Self {
            name: name.to_owned(),
            enable_warnings: false,
            include_comments: false,
            graph: BasicBlockGraph::new(),
            idgen: IdentGenerator::new(),
            function_idgen: IdentGenerator::new(),
            variables: HashMap::new(),
            variables_read: HashSet::new(),
            values: ValueStorage::new(),
            definitions: HashMap::new(),
            sealed_blocks: HashSet::new(),
            incomplete_phis: HashMap::new(),
            phi_users: HashMap::new(),
            entry_block: NodeIndex::new(0),
            undefined: VarId::default(),
            undefined_users: Vec::new(),
        };
        instance.entry_block = instance.new_block();
        instance.seal_block(instance.entry_block);
        instance.undefined = instance.define(instance.entry_block, Value::Undefined);
        instance
    }

    pub(crate) fn enable_warnings(mut self, value: bool) -> Self {
        self.enable_warnings = value;
        self
    }

    pub(crate) fn include_comments(mut self, value: bool) -> Self {
        self.include_comments = value;
        self
    }

    pub(crate) fn build(mut self, code: &ExprRef) -> ControlFlowGraph {
        let (exit_block, program_result) = self.process_expr(code, self.entry_block);
        let program_result = self.define(exit_block, Value::Return(program_result));
        self.assert_no_undefined_variables();

        if self.enable_warnings {
            let variables = self.variables
                .into_iter()
                .map(|(variable, _)| variable)
                .collect::<HashSet<_>>();
            let redundant_bindings = variables
                .difference(&self.variables_read)
                .filter_map(|variable| match variable {
                    Variable::Binding(binding) => Some(binding.clone()),
                    Variable::Value(_) => None,
                });
            warn_about_redundant_bindings(redundant_bindings);
        }

        remove_dead_code(program_result, &mut self.values, &mut self.graph, &mut self.definitions);

        ControlFlowGraph {
            name: self.name,
            graph: self.graph,
            entry_block: self.entry_block,
            exit_block,
            undefined: self.undefined,
            values: self.values,
        }
    }

    fn assert_no_undefined_variables(&self) {
        assert!(self.undefined_users.is_empty(), "found undefined usages:\n{:?}\n",
                self.undefined_users.iter()
                    .map(|user| &self.graph[user.block][user.index])
                    .format("\n"));
    }

    fn process_expr(&mut self, expr: &ExprRef, block: NodeIndex) -> (NodeIndex, VarId) {
        match &**expr {
            TypedExpr::Block(block_data) => {
                let mut current_block = block;
                let mut current_ident = None;
                for statement in &block_data.statements {
                    let (block, ident) = self.process_statement(statement, current_block);
                    current_block = block;
                    current_ident = Some(ident);
                }
                let ident = current_ident.unwrap_or_else(|| self.define(block, Value::Unit));
                (current_block, ident)
            }

            TypedExpr::Unit(source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::Unit))
            }

            TypedExpr::Int(value, source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::Int(value.clone())))
            }

            TypedExpr::String(value, source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::String(value.clone())))
            }

            TypedExpr::AddInt(left, right, source) => self.process_binary(block, left, right, source, Value::AddInt),
            TypedExpr::SubInt(left, right, source) => self.process_binary(block, left, right, source, Value::SubInt),
            TypedExpr::MulInt(left, right, source) => self.process_binary(block, left, right, source, Value::MulInt),
            TypedExpr::DivInt(left, right, source) => self.process_binary(block, left, right, source, Value::DivInt),
            TypedExpr::AddStr(left, right, source) => self.process_binary(block, left, right, source, Value::AddString),

            TypedExpr::Reference(binding, source) => {
                self.comment(block, source.text());
                (block, self.read_variable(&Variable::Binding(binding.clone()), block))
            }

            TypedExpr::Conditional { condition, positive, negative, source, .. } => {
                self.comment(block, source.text());
                let (block, condition) = self.process_expr(condition, block);
                let positive_block = self.new_block();
                let negative_block = self.new_block();

                let statement_location = StatementLocation::new(block, self.graph[block].len());
                self.record_phi_and_undefined_usages(statement_location, condition);
                self.graph[block].push(
                    Statement::CondJump(condition, positive_block, negative_block));

                self.graph.add_edge(block, positive_block, ());
                self.graph.add_edge(block, negative_block, ());
                self.seal_block(positive_block);
                self.seal_block(negative_block);
                let (positive_exit_block, positive) = self.process_expr(positive, positive_block);
                let (negative_exit_block, negative) = self.process_expr(negative, negative_block);

                let exit = self.new_block();
                self.graph.add_edge(positive_exit_block, exit, ());
                self.graph.add_edge(negative_exit_block, exit, ());
                self.seal_block(exit);
                let result_phi = self.define_phi(exit, &[positive, negative]);
                (exit, self.try_remove_trivial_phi(result_phi))
            }

            TypedExpr::Assign(binding, value, source) => {
                self.comment(block, source.text());
                let variable = Variable::Binding(binding.clone());
                let (block, ident) = self.process_expr(value, block);
                self.write_variable(&variable, block, ident);
                (block, ident)
            }

            TypedExpr::Lambda(lambda, source) => {
                let mut function = FrontEnd::new(source.text())
                    .enable_warnings(self.enable_warnings)
                    .include_comments(self.include_comments);
                let function_id = self.function_idgen.next_id();
                for parameter in &lambda.parameters {
                    function.comment(function.entry_block, &parameter.name);
                    let index = if let BindingKind::Arg(index) = parameter.kind {
                        index
                    } else {
                        unreachable!()
                    };
                    let declaration = function.define(function.entry_block, Value::Arg(index));
                    function.write_variable(&Variable::Binding(parameter.clone()), function.entry_block, declaration);
                }
                let function_cfg = UniqueRc::from(function.build(&lambda.body));
                (block, self.define(block, Value::Function(function_id, function_cfg)))
            }

            TypedExpr::Application { function, arguments, source, .. } => {
                self.comment(block, source.text());
                let (block, function_ident) = self.process_expr(function, block);
                let mut current_block = block;
                let mut argument_idents = vec![];
                for argument in arguments {
                    let (block, ident) = self.process_expr(argument, current_block);
                    argument_idents.push(ident);
                    current_block = block;
                }
                (block, self.define(current_block, Value::Call(function_ident, argument_idents)))
            }

            _ => unimplemented!("process_expr: {:?}", expr)
        }
    }

    fn process_binary(&mut self, block: NodeIndex, left: &ExprRef, right: &ExprRef, source: &Source,
                      value_constructor: impl Fn(VarId, VarId) -> Value) -> (NodeIndex, VarId)
    {
        let (block, left_ident) = self.process_expr(left, block);
        let (block, right_ident) = self.process_expr(right, block);
        self.comment(block, source.text());
        (block, self.define(block, value_constructor(left_ident, right_ident)))
    }

    fn process_statement(&mut self, statement: &TypedStatement, block: NodeIndex) -> (NodeIndex, VarId) {
        match statement {
            TypedStatement::Binding(binding) => {
                self.comment(block, binding.source.text());
                let (block, ident) = self.process_expr(&binding.data, block);
                self.write_variable(&Variable::Binding(binding.clone()), block, ident);
                (block, ident)
            },
            TypedStatement::Expr(expr) => {
                self.process_expr(expr, block)
            }
        }
    }

    fn write_variable(&mut self, variable: &Variable, block: NodeIndex, ident: VarId) {
        self.variables
            .entry(variable.clone())
            .or_insert_with(HashMap::new)
            .insert(block, ident);
    }

    fn read_variable(&mut self, variable: &Variable, block: NodeIndex) -> VarId {
        self.variables_read.insert(variable.clone());
        self.variables
            .get(variable)
            .and_then(|map| map.get(&block))
            .map(Clone::clone)
            .unwrap_or_else(|| self.read_variable_recursive(variable, block))
    }

    fn read_variable_recursive(&mut self, variable: &Variable, block: NodeIndex) -> VarId {
        let ident = if !self.sealed_blocks.contains(&block) {
            // Incomplete CFG
            let phi = self.define_phi(block, &[]);
            self.incomplete_phis
                .entry(block)
                .or_insert_with(HashMap::new)
                .insert(variable.clone(), phi);
            phi
        } else {
            let mut predecessors = self.graph
                .edges_directed(block, Direction::Incoming)
                .map(|edge| edge.source());
            let first_predecessor = predecessors.next();
            let second_predecessor = predecessors.next();
            if let (Some(predecessor), None) = (first_predecessor, second_predecessor) {
                // Optimize the common case of one predecessor: no phi needed
                self.read_variable(variable, predecessor)
            } else {
                // Break potential cycles with operandless phi
                let phi = self.define_phi(block, &[]);
                self.write_variable(variable, block, phi);
                self.add_phi_operands(variable, phi)
            }
        };
        self.write_variable(variable, block, ident);
        ident
    }

    fn add_phi_operands(&mut self, variable: &Variable, phi: VarId) -> VarId {
        let predecessors = self.graph
            .edges_directed(self.definitions[&phi].location.block, Direction::Incoming)
            .map(|edge| edge.source())
            .collect_vec();
        for predecessor in predecessors {
            let operand = self.read_variable(variable, predecessor);
            self.phi_operands_mut(phi).push(operand);
        }
        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: VarId) -> VarId {
        let mut same = None;
        for &operand in self.phi_operands(phi) {
            if operand == self.undefined {
                same = Some(self.undefined);
                break;
            }
            if Some(operand) == same || operand == phi {
                // Unique value or self−reference
                continue;
            }
            if same.is_some() {
                // The phi merges at least two values: not trivial
                return phi;
            }
            same = Some(operand);
        }

        let same = match same {
            Some(ident) => ident,
            None => self.undefined
        };

        self.phi_users.get_mut(&phi).unwrap().remove(&phi);
        let mut possible_trivial_phis = HashSet::new();
        for user in &self.phi_users[&phi] {
            let value_index = self.definitions[user].value_index;
            let value = &mut self.values[value_index];
            replace_ident(value, phi, same);
            if let Value::Phi(_) = value {
                possible_trivial_phis.insert(*user);
            }
        }

        for phi in possible_trivial_phis {
            self.try_remove_trivial_phi(phi);
        }

        same
    }

    fn seal_block(&mut self, block: NodeIndex) {
        let variables = self.incomplete_phis
            .get(&block)
            .map(|incomplete_phis| incomplete_phis
                .keys()
                .cloned()
                .collect_vec());
        if let Some(variables) = variables {
            for variable in &variables {
                self.add_phi_operands(variable, self.incomplete_phis[&block][variable]);
            }
        }
        self.sealed_blocks.insert(block);
    }

    fn define(&mut self, block: NodeIndex, value: Value) -> VarId {
        let (value_index, reused) = self.values.insert(value);
        let variable = Variable::Value(value_index);
        let ident = if *reused {
            self.read_variable(&variable, block)
        } else {
            self.undefined
        };
        if ident != self.undefined {
            ident
        } else {
            let ident = self.idgen.next_id();
            self.graph[block].push(Statement::Definition {
                ident,
                value_index,
            });
            self.write_variable(&Variable::Value(value_index), block, ident);
            let location = StatementLocation::new(block, self.graph[block].len() - 1);
            self.definitions.insert(ident, IdentDefinition { value_index, location });
            self.record_phi_and_undefined_usages(location, ident);
            if let Value::Phi(_) = &self.values[value_index] {
                self.phi_users.insert(ident, HashSet::new());
            }
            ident
        }
    }

    fn define_phi(&mut self, block: NodeIndex, operands: &[VarId]) -> VarId {
        self.define(block, Value::Phi(Phi(operands.to_vec())))
    }

    fn record_phi_and_undefined_usages(&mut self, location: StatementLocation, ident: VarId) {
        let value_index = self.definitions[&ident].value_index;
        let value = &self.values[value_index];
        let (is_using_undefined, phis_used) = get_value_ident_operands(value)
            .fold((false, vec![]), |(mut is_using_undefined, mut phis_used), ident| {
                if ident == self.undefined {
                    is_using_undefined = true;
                } else if self.is_a_phi(ident) {
                    phis_used.push(ident);
                }
                (is_using_undefined, phis_used)
            });
        if is_using_undefined {
            self.undefined_users.push(location);
        }
        for phi in phis_used {
            self.phi_users.get_mut(&phi).unwrap().insert(ident);
        }
    }

    fn is_a_phi(&self, ident: VarId) -> bool {
        ident != self.undefined && self.phi_users.contains_key(&ident)
    }

    fn phi_operands(&self, phi: VarId) -> &[VarId] {
        let value_index = self.definitions[&phi].value_index;
        match &self.values[value_index] {
            Value::Phi(Phi(operands)) => operands,
            _ => unreachable!()
        }
    }

    fn phi_operands_mut(&mut self, phi: VarId) -> &mut Vec<VarId> {
        let value_index = self.definitions[&phi].value_index;
        match &mut self.values[value_index] {
            Value::Phi(Phi(operands)) => operands,
            _ => unreachable!()
        }
    }

    fn comment(&mut self, block: NodeIndex, comment: &str) {
        if self.include_comments {
            self.graph[block].push(Statement::Comment(comment.to_owned()));
        }
    }

    fn new_block(&mut self) -> NodeIndex {
        self.graph.add_node(BasicBlock::new())
    }
}

fn warn_about_redundant_bindings(redundant_bindings: impl Iterator<Item = BindingRef>) {
    let redundant_bindings = redundant_bindings
        .sorted_by(|a, b| a.source.range().start().cmp(&b.source.range().start()));
    for binding in redundant_bindings {
        warning_at!(binding.source, "unused definition: {}", binding.source.text());
    }
}

// TODO refactor remove_dead_code()
fn remove_dead_code(
    program_result: VarId,
    values: &mut ValueStorage,
    graph: &mut BasicBlockGraph,
    definitions: &mut HashMap<VarId, IdentDefinition>)
{
    let mut ident_usage: HashMap<VarId, usize> = HashMap::from_iter(
        definitions
            .keys()
            .map(|ident| {
                let usages = match &values[definitions[&ident].value_index] {
                    Value::Return(_) => 1,
                    _ => 0,
                };
                (*ident, usages)
            }));
    {
        let used_idents = graph
            .raw_nodes()
            .iter()
            .map(|node| &node.weight)
            .flat_map(|block| block.iter())
            .flat_map(|statement| get_statement_ident_operands(&values, statement))
            .chain(once(program_result));
        for ident in used_idents {
            *ident_usage.get_mut(&ident).unwrap() += 1;
        }
    }
    let unused_idents = definitions
        .keys()
        .filter_map(|ident| match ident_usage[ident] {
            0 => Some(*ident),
            _ => None,
        })
        .collect_vec();
    for ident in unused_idents {
        unuse(ident, &mut ident_usage, definitions, graph, values);
    }

    let dead_code = ident_usage
        .iter()
        .filter_map(|(ident, usages)| match usages {
            0 => Some(definitions[ident].location),
            _ => None,
        })
        .collect_vec();
    remove_statements(graph, dead_code);

    let unused_idents = ident_usage
        .into_iter()
        .filter_map(|(ident, usages)| match usages {
            0 => Some(ident),
            _ => None,
        });
    for unused_ident in unused_idents {
        definitions.remove(&unused_ident);
    }

    let ident_rename_map: HashMap<VarId, VarId> = HashMap::from_iter({
        definitions
            .keys()
            .into_iter()
            .sorted()
            .enumerate()
            .map(|(i, ident)| (*ident, VarId::new(i)))
    });

    graph
        .node_weights_mut()
        .flat_map(|block| block.iter_mut())
        .for_each(|statement| {
            for ident in get_statement_idents_mut(values, statement) {
                *ident = ident_rename_map[&ident];
            }
        });

    let mut blocks = graph
        .node_indices()
        .collect_vec();
    blocks.sort_by(|a, b| b.cmp(&a));

    for block in blocks {
        let is_empty = graph[block]
            .iter()
            .filter(|statement| match statement {
                Statement::Comment(_) => false,
                _ => true,
            })
            .next()
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
            for source in &sources {
                let source_basic_block: &mut BasicBlock = &mut graph[*source];
                if let Some(Statement::CondJump(_, _, _)) = source_basic_block.last() {
                    source_basic_block.pop();
                }
                for target in &targets {
                    if !graph.contains_edge(*source, *target) {
                        graph.add_edge(*source, *target, ());
                    }
                }
            }
            graph.remove_node(block);
        }
    }

    for block in graph.node_indices().sorted_by(|a, b| b.cmp(&a)) {
        let targets = graph
            .edges_directed(block, Direction::Outgoing)
            .map(|edge| edge.target())
            .collect_vec();
        if targets.len() == 1 {
            if let Some(Statement::CondJump(_, _, _)) = graph[block].last() {
                graph[block].pop();
                let mut target_basic_block = replace(&mut graph[targets[0]], BasicBlock::new());
                (&mut graph[block]).append(&mut target_basic_block);
                graph.remove_node(targets[0]);
            }
        }
    }

    // TODO remove unused values and reindex the rest
}

fn unuse(
    ident: VarId,
    ident_usage: &mut HashMap<VarId, usize>,
    definitions: &mut HashMap<VarId, IdentDefinition>,
    graph: &BasicBlockGraph,
    values: &ValueStorage)
{
    let usage_count = ident_usage.get_mut(&ident).unwrap();
    if *usage_count > 0 {
        *usage_count -= 1;
    }

    if *usage_count == 0 {
        let location = &definitions[&ident].location;
        let statement = &graph[location.block][location.index];
        for ident in get_statement_ident_operands(&values, statement) {
            unuse(ident, ident_usage, definitions, graph, values);
        }
    }
}

fn get_statement_ident_operands<'a>(values: &'a ValueStorage, statement: &'a Statement) -> Box<dyn Iterator<Item = VarId> + 'a> {
    match statement {
        Statement::Comment(_) => Box::new(empty()),
        Statement::Definition { value_index, .. } => get_value_ident_operands(&values[*value_index]),
        Statement::CondJump(condition, _, _) => Box::new(once(*condition)),
    }
}

fn get_statement_idents_mut<'a>(values: &'a mut ValueStorage, statement: &'a mut Statement) -> Box<dyn Iterator<Item = &'a mut VarId> + 'a> {
    match statement {
        Statement::Comment(_) => Box::new(empty()),
        Statement::Definition { ident, value_index } => Box::new(once(ident).chain(get_value_ident_operands_mut(&mut values[*value_index]))),
        Statement::CondJump(condition, _, _) => Box::new(once(condition)),
    }
}

fn remove_statements(graph: &mut BasicBlockGraph, mut locations: Vec<StatementLocation>) {
    let locations = {
        locations.sort_unstable_by(|a, b| a.block.cmp(&b.block).then_with(|| a.index.cmp(&b.index)));
        locations.dedup();
        locations
    };
    let mut current_state: Option<RemoveStatementsState<'_>> = None;
    for location in locations {
        let need_new_state = match &current_state {
            Some(state) => location.block != state.block,
            None => true,
        };
        if need_new_state {
            if let Some(previous_state) = current_state.take() {
                previous_state.finish();
            }
            current_state = Some(RemoveStatementsState::new(location.block, graph));
        }
        if let Some(state) = current_state.as_mut() {
            state.remove_at(location.index);
        }
    }
    if let Some(state) = current_state {
        state.finish();
    }
}

fn replace_ident(value: &mut Value, ident: VarId, replacement: VarId) {
    let operands: Box<dyn Iterator<Item = &mut VarId>> = match value {
        Value::Undefined |
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::Arg(_) => Box::new(empty()),

        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) |
        Value::AddString(left, right) => Box::new(once(left).chain(once(right))),
        Value::Phi(operands) => Box::new(operands.iter_mut()),
        Value::Call(function, arguments) => Box::new(once(function).chain(arguments.iter_mut())),
        Value::Return(result) => Box::new(once(result)),
    };
    for operand in operands {
        if *operand == ident {
            *operand = replacement;
        }
    }
}

fn get_value_ident_operands<'a>(value: &'a Value) -> Box<dyn Iterator<Item = VarId> + 'a> {
    match value {
        Value::Undefined |
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::Arg(_) => Box::new(empty()),

        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) |
        Value::AddString(left, right) => Box::new(once(*left).chain(once(*right))),

        Value::Phi(operands) => Box::new(operands.iter().cloned()),
        Value::Call(function, arguments) => Box::new(once(*function).chain(arguments.iter().cloned())),
        Value::Return(result) => Box::new(once(*result)),
    }
}

fn get_value_ident_operands_mut<'a>(value: &'a mut Value) -> Box<dyn Iterator<Item = &mut VarId> + 'a> {
    match value {
        Value::Undefined |
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::Arg(_) => Box::new(empty()),

        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) |
        Value::AddString(left, right) => Box::new(once(left).chain(once(right))),

        Value::Phi(operands) => Box::new(operands.iter_mut()),
        Value::Call(function, arguments) => Box::new(once(function).chain(arguments.iter_mut())),
        Value::Return(result) => Box::new(once(result)),
    }
}

#[derive(Debug)]
struct RemoveStatementsState<'a> {
    block: NodeIndex,
    basic_block: &'a mut BasicBlock,
    hole_start: usize,
    hole_end: usize,
    total_length: usize,
    has_removed_any: bool,
}

impl<'a> RemoveStatementsState<'a> {
    fn new(block: NodeIndex, graph: &'a mut BasicBlockGraph) -> Self {
        Self {
            block,
            basic_block: &mut graph[block],
            hole_start: 0,
            hole_end: 0,
            total_length: 0,
            has_removed_any: false,
        }
    }

    fn remove_at(&mut self, index: usize) {
        if index > self.hole_end {
            self.total_length += index - self.hole_end;
            while self.hole_end < index {
                self.basic_block.swap(self.hole_start, self.hole_end);
                self.hole_start += 1;
                self.hole_end += 1;
            }
        }
        self.hole_end += 1;
        self.has_removed_any = true;
    }

    fn finish(mut self) {
        if self.has_removed_any {
            self.remove_at(self.basic_block.len());
            unsafe { self.basic_block.set_len(self.total_length); }
            self.basic_block.shrink_to_fit();
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct StatementLocation {
    block: NodeIndex,
    index: usize
}

impl StatementLocation {
    fn new(block: NodeIndex, index: usize) -> Self {
        Self { block, index }
    }
}

#[derive(Debug)]
struct IdentDefinition {
    value_index: ValueIndex,
    location: StatementLocation,
}

#[test]
fn gen_ir() -> TestResult {
    let code = typecheck!("
    let x = 47
    let y = 29
    let z = y
    1
    let r = if 0 {
        let a = z
        let b = a
        z = 22
        x + b + y
        1
    } else {
        z = 33
        1
    }
    z
    r + 1
    z + 1

    z = 6
    z + 7
    z = 8
    z + 9

    let f = λ (g: λ (u:str, v:str) -> str, u:str, v:str) -> g u v
    let s1 = f (λ (u:str, v:str) -> \"(\" + u + \"; \" + v + \")\") \"hello\" \"world\"
    let s2 = f (λ (u:str, v:str) -> \"<\" + u + \"; \" + v + \">\") (\"bye, \" + \"world\") \"seeya\"
    let result = s1 + s2
    result
    ")?;

    let cfg = FrontEnd::new(&location!())
        .enable_warnings(false)
        .include_comments(true)
        .build(&code);
    let mut output = std::fs::File::create("ir_cfg.dot")?;
    crate::graphviz::Graphviz::new(&cfg, cfg.name.clone()).fmt(&mut output)?;

    let value = crate::ir::eval::EvalContext::new(&cfg).eval();
    assert_eq!(value, Value::String(Rc::new("(hello; world)<bye, world; seeya>".to_owned())));

    Ok(())
}
