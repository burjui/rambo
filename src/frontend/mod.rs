/*
The minimal SSA algorithm is based on the following paper:

Braun M., Buchwald S., Hack S., Leißa R., Mallon C., Zwinkau A.
(2013) Simple and Efficient Construction of Static Single Assignment Form.
In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg
*/

use std::collections::HashMap;
use std::collections::HashSet;
#[cfg(test)] use std::ffi::OsStr;
#[cfg(test)] use std::fs::File;
use std::iter::empty;
use std::iter::once;
#[cfg(test)] use std::path::Path;
use std::rc::Rc;

use itertools::free::chain;
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::identities::One;
use num_traits::identities::Zero;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;

use crate::frontend::dead_code::remove_dead_code;
#[cfg(test)] use crate::graphviz::graphviz_dot_write;
use crate::ir::{BasicBlock, FunctionMap, StatementLocation};
use crate::ir::BasicBlockGraph;
use crate::ir::ControlFlowGraph;
use crate::ir::FnId;
use crate::ir::IdentGenerator;
use crate::ir::Phi;
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::value_storage::{UNDEFINED_VALUE, ValueId};
use crate::ir::value_storage::ValueStorage;
use crate::ir::Variable;
use crate::semantics::BindingRef;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
#[cfg(test)] use crate::utils::TestResult;

mod dead_code;

pub(crate) struct FrontEnd {
    name: String,
    enable_warnings: bool,
    include_comments: bool,
    enable_cfp: bool,
    enable_dce: bool,
    graph: BasicBlockGraph,
    function_idgen: IdentGenerator<FnId>,
    variables: HashMap<Variable, HashMap<NodeIndex, ValueId>>,
    variables_read: HashSet<Variable>,
    values: ValueStorage,
    functions: FunctionMap,
    definitions: HashMap<ValueId, StatementLocation>,
    sealed_blocks: HashSet<NodeIndex>,
    incomplete_phis: HashMap<NodeIndex, HashMap<Variable, ValueId>>,
    phi_users: HashMap<ValueId, HashSet<ValueId>>,
    entry_block: NodeIndex,
    undefined_users: Vec<StatementLocation>,
    markers: Vec<Option<Marker>>,
}

impl FrontEnd {
    pub(crate) fn new(name: &str) -> Self {
        let mut instance = Self {
            name: name.to_owned(),
            enable_warnings: false,
            include_comments: false,
            enable_cfp: false,
            enable_dce: false,
            graph: BasicBlockGraph::new(),
            function_idgen: IdentGenerator::new(),
            variables: HashMap::new(),
            variables_read: HashSet::new(),
            values: ValueStorage::new(),
            functions: HashMap::new(),
            definitions: HashMap::new(),
            sealed_blocks: HashSet::new(),
            incomplete_phis: HashMap::new(),
            phi_users: HashMap::new(),
            entry_block: NodeIndex::new(0),
            undefined_users: Vec::new(),
            markers: Vec::new(),
        };
        instance.entry_block = instance.new_block();
        instance.seal_block(instance.entry_block);
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

    pub(crate) fn enable_cfp(mut self, value: bool) -> Self {
        self.enable_cfp = value;
        self
    }

    pub(crate) fn enable_dce(mut self, value: bool) -> Self {
        self.enable_dce = value;
        self
    }

    pub(crate) fn build(mut self, code: &ExprRef) -> ControlFlowGraph {
        let (exit_block, mut program_result) = self.process_expr(code, self.entry_block);
        self.push_return(exit_block, program_result);
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

        if self.enable_dce {
            remove_dead_code(self.entry_block, &mut program_result, &mut self.values,
                             &mut self.graph, &mut self.definitions, &mut self.functions);
        }

        ControlFlowGraph {
            name: self.name,
            graph: self.graph,
            entry_block: self.entry_block,
            definitions: self.definitions,
            values: self.values,
            functions: self.functions,
            result: program_result,
        }
    }

    fn assert_no_undefined_variables(&self) {
        assert!(self.undefined_users.is_empty(), "found undefined usages:\n{:?}\n",
                self.undefined_users.iter()
                    .map(|user| &self.graph[user.block][user.index])
                    .format("\n"));
    }

    // TODO make block the first argument everywhere
    fn process_expr(&mut self, expr: &ExprRef, block: NodeIndex) -> (NodeIndex, ValueId) {
        match &**expr {
            TypedExpr::Block(statements, _) => {
                let mut current_block = block;
                let mut current_value_id = None;
                for statement in statements {
                    let (block, value_id) = self.process_statement(statement, current_block);
                    current_block = block;
                    current_value_id = Some(value_id);
                }
                let value_id = current_value_id.unwrap_or_else(|| self.define(block, Value::Unit));
                (current_block, value_id)
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
                let condition_value = &self.values[condition];

                if self.enable_cfp && condition_value.is_constant() {
                    return match condition_value {
                        Value::Int(n) => {
                            if n.is_zero() {
                                self.process_expr(negative, block)
                            } else {
                                self.process_expr(positive, block)
                            }
                        }

                        _ => unreachable!(),
                    };
                }

                let positive_block = self.new_block();
                let negative_block = self.new_block();

                let statement_index = self.graph[block].push(
                    Statement::CondJump(condition, positive_block, negative_block));
                let statement_location = StatementLocation::new(block, statement_index);
                self.record_phi_and_undefined_usages(statement_location, condition);

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
                let (block, value_id) = self.process_expr(value, block);
                self.write_variable(&variable, block, value_id);
                (block, value_id)
            }

            TypedExpr::Lambda(lambda, source) => {
                let mut function = FrontEnd::new(source.text())
                    .enable_warnings(self.enable_warnings)
                    .include_comments(self.include_comments)
                    .enable_cfp(self.enable_cfp)
                    .enable_dce(self.enable_dce);
                for (index, parameter) in lambda.parameters.iter().enumerate() {
                    function.comment(function.entry_block, &parameter.name);
                    let declaration = function.define(function.entry_block, Value::Arg(index));
                    function.write_variable(&Variable::Binding(parameter.clone()), function.entry_block, declaration);
                }
                let fn_id = self.function_idgen.next_id();
                self.functions.insert(fn_id, function.build(&lambda.body));
                (block, self.define(block, Value::Function(fn_id)))
            }

            TypedExpr::Application { function, arguments, source, .. } => {
                self.comment(block, source.text());
                let (block, function_value_id) = self.process_expr(function, block);
                let mut current_block = block;
                let mut argument_value_ids = vec![];
                for argument in arguments {
                    let (block, value_id) = self.process_expr(argument, current_block);
                    argument_value_ids.push(value_id);
                    current_block = block;
                }
                (block, self.define(block, Value::Call(function_value_id, argument_value_ids)))
            }

            _ => unimplemented!("process_expr: {:?}", expr)
        }
    }

    fn process_binary(&mut self, block: NodeIndex, left: &ExprRef, right: &ExprRef, source: &Source,
                      value_constructor: impl Fn(ValueId, ValueId) -> Value) -> (NodeIndex, ValueId)
    {
        let (block, left_value_id) = self.process_expr(left, block);
        let (block, right_value_id) = self.process_expr(right, block);
        self.comment(block, source.text());
        (block, self.define(block, value_constructor(left_value_id, right_value_id)))
    }

    fn process_statement(&mut self, statement: &TypedStatement, block: NodeIndex) -> (NodeIndex, ValueId) {
        match statement {
            TypedStatement::Expr(expr) => self.process_expr(expr, block),

            TypedStatement::Binding(binding) => {
                self.comment(block, binding.source.text());
                let (block, value_id) = self.process_expr(&binding.data, block);
                self.write_variable(&Variable::Binding(binding.clone()), block, value_id);
                (block, value_id)
            },
        }
    }

    fn write_variable(&mut self, variable: &Variable, block: NodeIndex, value_id: ValueId) {
        self.variables
            .entry(variable.clone())
            .or_default()
            .insert(block, value_id);
    }

    fn read_variable(&mut self, variable: &Variable, block: NodeIndex) -> ValueId {
        self.reset_markers();
        self.read_variable_core(variable, block)
    }

    fn read_variable_core(&mut self, variable: &Variable, block: NodeIndex) -> ValueId {
        self.variables_read.insert(variable.clone());
        self.variables
            .get(variable)
            .and_then(|map| map.get(&block))
            .map(Clone::clone)
            .unwrap_or_else(|| self.read_variable_recursive(variable, block))
    }

    fn read_variable_recursive(&mut self, variable: &Variable, block: NodeIndex) -> ValueId {
        let value_id = if !self.sealed_blocks.contains(&block) {
            // Incomplete CFG
            let phi = self.define_phi(block, &[]);
            self.incomplete_phis
                .entry(block)
                .or_default()
                .insert(variable.clone(), phi);
            phi
        } else {
            match self.get_marker(block) {
                None => self.mark_block(block, Marker::Initial),

                Some(Marker::Initial) => {
                    let phi = self.define_phi(block, &[]);
                    self.mark_block(block, Marker::Phi(phi));
                }

                _ => (),
            }

            let mut predecessors = self.graph
                .edges_directed(block, Direction::Incoming)
                .map(|edge| edge.source());
            let first_predecessor = predecessors.next();
            let second_predecessor = predecessors.next();
            if let (Some(predecessor), None) = (first_predecessor, second_predecessor) {
                // Optimize the common case of one predecessor: no phi needed
                self.read_variable_core(variable, predecessor)
            } else {
                // Break potential cycles with operandless phi
                let predecessors = chain(first_predecessor, second_predecessor)
                    .chain(predecessors)
                    .collect_vec();
                let definitions = predecessors
                    .into_iter()
                    .map(|predecessor| self.read_variable_core(variable, predecessor))
                    .dedup()
                    .collect_vec();
                self.remove_marker(block);
                match definitions.len() {
                    1 => definitions[0],
                    0 => UNDEFINED_VALUE,
                    _ => {
                        if definitions.contains(&UNDEFINED_VALUE) {
                            UNDEFINED_VALUE
                        } else {
                            let phi = match self.get_marker(block) {
                                Some(Marker::Phi(phi)) => *phi,
                                _ => self.define_phi(block, &[]),
                            };
                            let phi_operands = self.phi_operands_mut(phi);
                            *phi_operands = definitions;
                            phi_operands.sort();
                            phi
                        }
                    }
                }
            }
        };
        self.write_variable(variable, block, value_id);
        value_id
    }

    fn add_phi_operands(&mut self, variable: &Variable, phi: ValueId) -> ValueId {
        let predecessors = self.graph
            .edges_directed(self.definitions[&phi].block, Direction::Incoming)
            .map(|edge| edge.source())
            .collect_vec();
        for predecessor in predecessors {
            let operand = self.read_variable_core(variable, predecessor);
            self.phi_operands_mut(phi).push(operand);
        }
        self.phi_operands_mut(phi).sort();
        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: ValueId) -> ValueId {
        let mut same = None;
        for &operand in self.phi_operands(phi) {
            if operand == UNDEFINED_VALUE {
                same = Some(operand);
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
            Some(value_id) => value_id,
            None => UNDEFINED_VALUE,
        };

        self.phi_users.get_mut(&phi).unwrap().remove(&phi);
        let mut possible_trivial_phis = HashSet::new();
        for user in &self.phi_users[&phi] {
            let value = &mut self.values[*user];
            replace_value_id(value, phi, same);
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

    fn define(&mut self, block: NodeIndex, value: Value) -> ValueId {
        let value = normalize(value);
        let (value_index, reused) = self.values.insert(value.clone());
        let variable = Variable::Value(value_index);
        if *reused {
            self.read_variable(&variable, block)
        } else {
            if self.enable_cfp {
                fold_constants(block, &self.definitions, &mut self.values, &self.functions, value_index);
            }
            let basic_block = &mut self.graph[block];
            let statement_index = basic_block.push(Statement::Definition(value_index));
            self.write_variable(&Variable::Value(value_index), block, value_index);
            let location = StatementLocation::new(block, statement_index);

            self.definitions.insert(value_index, location);
            self.record_phi_and_undefined_usages(location, value_index);
            if let Value::Phi(_) = &self.values[value_index] {
                self.phi_users.insert(value_index, HashSet::new());
            }
            value_index
        }
    }

    fn define_phi(&mut self, block: NodeIndex, operands: &[ValueId]) -> ValueId {
        let mut operands = operands.to_vec();
        operands.sort();
        self.define(block, Value::Phi(Phi(operands)))
    }

    fn push_return(&mut self, block: NodeIndex, value_id: ValueId) {
        self.graph[block].push(Statement::Return(value_id));
    }

    fn record_phi_and_undefined_usages(&mut self, location: StatementLocation, value_id: ValueId) {
        let (is_using_undefined, phis_used) = get_value_operands(&self.values[value_id])
            .fold((false, vec![]), |(mut is_using_undefined, mut phis_used), value_id| {
                if value_id == UNDEFINED_VALUE {
                    is_using_undefined = true;
                } else if self.is_a_phi(value_id) {
                    phis_used.push(value_id);
                }
                (is_using_undefined, phis_used)
            });
        if is_using_undefined {
            self.undefined_users.push(location);
        }
        for phi in phis_used {
            self.phi_users.get_mut(&phi).unwrap().insert(value_id);
        }
    }

    fn is_a_phi(&self, value_id: ValueId) -> bool {
        value_id != UNDEFINED_VALUE && self.phi_users.contains_key(&value_id)
    }

    fn phi_operands(&self, phi: ValueId) -> &[ValueId] {
        match &self.values[phi] {
            Value::Phi(Phi(operands)) => operands,
            _ => unreachable!()
        }
    }

    fn phi_operands_mut(&mut self, phi: ValueId) -> &mut Vec<ValueId> {
        match &mut self.values[phi] {
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

    fn reset_markers(&mut self) {
        let len = self.markers.len();
        self.markers.truncate(0);
        self.markers.resize(len, None);
    }

    fn get_marker(&mut self, block: NodeIndex) -> &Option<Marker> {
        let index = self.marker_index(block);
        &self.markers[index]
    }

    fn mark_block(&mut self, block: NodeIndex, marker: Marker) {
        let index = self.marker_index(block);
        self.markers[index] = Some(marker);
    }

    fn remove_marker(&mut self, block: NodeIndex) {
        let index = self.marker_index(block);
        self.markers[index] = None;
    }

    fn marker_index(&mut self, block: NodeIndex) -> usize {
        let index = block.index();
        if index >= self.markers.len() {
            self.markers.resize(index +  1, None);
        }
        index
    }
}

fn warn_about_redundant_bindings(redundant_bindings: impl Iterator<Item = BindingRef>) {
    let redundant_bindings = redundant_bindings
        .sorted_by_key(|binding| binding.source.range().start());
    for binding in redundant_bindings {
        warning_at!(binding.source, "unused definition: {}", binding.source.text());
    }
}

fn fold_constants(
    block: NodeIndex,
    definitions: &HashMap<ValueId, StatementLocation>,
    values: &mut ValueStorage,
    functions: &FunctionMap,
    value_index: ValueId)
{
    let folded = match &values[value_index] {
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function(_) => None,

        Value::AddInt(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(_, left_value), (_, right_value)| {
            match (left_value, right_value) {
                (Value::Int(left), Value::Int(right)) => Some(Value::Int(left + right)),
                (Value::Int(left), _) if left.is_zero() => Some(right_value.clone()),
                (_, Value::Int(right)) if right.is_zero() => Some(left_value.clone()),
                _ => None,
            }
        }),

        Value::SubInt(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(left, left_value), (right, right_value)| {
            if left == right {
                Some(Value::Int(BigInt::from(0)))
            } else {
                match (left_value, right_value) {
                    (Value::Int(left), Value::Int(right)) => Some(Value::Int(left - right)),
                    (_, Value::Int(right)) if right.is_zero() => Some(left_value.clone()),
                    _ => None,
                }
            }
        }),

        Value::MulInt(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(_, left_value), (_, right_value)| {
            match (left_value, right_value) {
                (Value::Int(left), Value::Int(right)) => Some(Value::Int(left * right)),
                (Value::Int(left), _) if left.is_zero() => Some(Value::Int(BigInt::from(0))),
                (_, Value::Int(right)) if right.is_zero() => Some(Value::Int(BigInt::from(0))),
                (Value::Int(left), _) if left.is_one() => Some(right_value.clone()),
                (_, Value::Int(right)) if right.is_one() => Some(left_value.clone()),
                _ => None,
            }
        }),

        Value::DivInt(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(_, left_value), (_, right_value)| {
            match (left_value, right_value) {
                (Value::Int(left), Value::Int(right)) => Some(Value::Int(left / right)),
                (Value::Int(left), _) if left.is_zero() => Some(Value::Int(BigInt::from(0))),
                (_, Value::Int(right)) if right.is_one() => Some(left_value.clone()),
                _ => None,
            }
        }),

        Value::AddString(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(_, left_value), (_, right_value)| {
            match (left_value, right_value) {
                (Value::String(left), Value::String(right)) => {
                    let mut result = String::with_capacity(left.len() + right.len());
                    result.push_str(&left);
                    result.push_str(&right);
                    Some(Value::String(Rc::new(result)))
                },
                _ => None,
            }
        }),

        Value::Call(function, arguments) =>
            fold_call(*function, arguments.to_vec(), values, functions),

        Value::Phi(_) |
        Value::Arg(_) => None,
    };
    if let Some(value) = folded {
        values[value_index] = value;
    }
}

fn fold_binary(
    block: NodeIndex,
    definitions: &HashMap<ValueId, StatementLocation>,
    values: &mut ValueStorage,
    functions: &FunctionMap,
    left: ValueId, right: ValueId,
    fold: impl FnOnce((ValueId, &Value), (ValueId, &Value)) -> Option<Value>) -> Option<Value>
{
    fold_constants(block, definitions, values, functions, left);
    fold_constants(block, definitions, values, functions, right);
    fold((left, &values[left]), (right, &values[right]))
}

fn fold_call(
    function: ValueId,
    arguments: Vec<ValueId>,
    values: &mut ValueStorage,
    functions: &FunctionMap) -> Option<Value>
{
    let arguments_are_constant = arguments
        .iter()
        .all(|argument| values[*argument].is_constant());
    if arguments_are_constant {
        let mut function_cfg = match &values[function] {
            Value::Function(fn_id) => functions[fn_id].clone(),
            _ => unreachable!(),
        };
        // FIXME probably should store argument ids somewhere, instead of collecting them like that
        let parameter_ids = function_cfg.definitions
            .keys()
            .filter_map(|value_index| match function_cfg.values[*value_index] {
                Value::Arg(index) => Some((*value_index, index)),
                _ => None,
            })
            .collect_vec();
        for (parameter_value_index, argument_index) in parameter_ids {
            function_cfg.values[parameter_value_index] =
                values[arguments[argument_index]].clone();
        }

        let result_block = function_cfg.definitions[&function_cfg.result].block;
        fold_constants(result_block, &function_cfg.definitions,
                       &mut function_cfg.values, functions, function_cfg.result);
        let result_value = &function_cfg.values[function_cfg.result];
        if result_value.is_constant() {
            return Some(result_value.clone())
        }
    }
    None
}

fn get_statement_operands<'a>(values: &'a ValueStorage, statement: &'a Statement)
    -> Box<dyn Iterator<Item = ValueId> + 'a>
{
    match statement {
        Statement::Comment(_) => Box::new(empty()),
        Statement::Definition(value_index) => get_value_operands(&values[*value_index]),
        Statement::CondJump(condition, _, _) => Box::new(once(*condition)),
        Statement::Return(result) => Box::new(once(*result)),
    }
}

fn replace_value_id(value: &mut Value, value_id: ValueId, replacement: ValueId) {
    let operands: Box<dyn Iterator<Item = &mut ValueId>> = match value {
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
    };
    for operand in operands {
        if *operand == value_id {
            *operand = replacement;
        }
    }
}

fn get_value_operands<'a>(value: &'a Value) -> Box<dyn Iterator<Item =ValueId> + 'a> {
    match value {
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

        Value::Phi(phi) => Box::new(phi.iter().cloned()),
        Value::Call(function, arguments) => Box::new(once(*function).chain(arguments.iter().cloned())),
    }
}

fn normalize(value: Value) -> Value {
    match &value {
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::SubInt(_, _) |
        Value::DivInt(_, _) |
        Value::AddString(_, _) |
        Value::Call(_, _) |
        Value::Arg(_) => value,

        Value::AddInt(left, right) => Value::AddInt(*left.min(&right), *left.max(&right)),
        Value::MulInt(left, right) => Value::MulInt(*left.min(&right), *left.max(&right)),

        Value::Phi(Phi(operands)) => {
            let sorted_operands = operands
                .iter()
                .cloned()
                .sorted()
                .collect_vec();
            Value::Phi(Phi(sorted_operands))
        },
    }
}

#[derive(Copy, Clone)]
enum Marker {
    Initial,
    Phi(ValueId),
}

macro_rules! test_frontend {
    ($name: ident, $code: expr, $check: expr) => {
        #[test]
        fn $name() -> TestResult {
            let code = typecheck!($code)?;
            let cfg = FrontEnd::new(&location!())
                .enable_warnings(false)
                .include_comments(false)
                .enable_cfp(true)
                .enable_dce(true)
                .build(&code);
            let test_src_path = Path::new(file!());
            let test_src_file_name = test_src_path
                .file_name()
                .and_then(OsStr::to_str)
                .expect(&format!("failed to extract the file name from path: {}", test_src_path.display()));
            let mut file = File::create(format!("{}_{}_cfg.dot", test_src_file_name, line!()))?;
            graphviz_dot_write(&mut file, &cfg)?;
            let check: fn(ControlFlowGraph) = $check;
            check(cfg);
            Ok(())
        }
    }
}

macro_rules! test_frontend_eval {
    ($name: ident, $code: expr, $expected_result: expr) => {
        test_frontend!{ $name, $code, |cfg| {
            let value = crate::ir::eval::EvalContext::new(&cfg, &cfg.functions).eval();
            assert_eq!(value, $expected_result);
        }}
    }
}

test_frontend_eval! {
    generic,
    "
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
    ",
    Value::String(Rc::new("(hello; world)<bye, world; seeya>".to_owned()))
}

test_frontend_eval! {
    block_removal,
    "
    let x = 0
    let y = 999
    if x {
        x = 1
    } else {
        x = 2
    }
    if y {
        x = 3
    } else {
        x = 4
    }
    x
    ",
    Value::Int(BigInt::from(3))
}

test_frontend_eval! {
    block_removal2,
    &"
    let x = \"abc\"
    let y = \"efg\"
    let z = λ (a:str, b:str) -> {
        let result = a + b
        result
    }
    if 0 {
        \"false\"
    }
    else {
        let a = (z x y)
        z a \"/test\"
    }
    let nn = 1
    ".repeat(7),
    Value::Unit
}

test_frontend_eval!{
    marker_eval,
    "
    let a = 1
    let b = 2
    (a + 1) * (b - 1)
    let f = λ (a: num, b: num) -> a + b
    let g = λ (f: λ (a: num, b: num) -> num, a: num, b: num) → f a b + 1
    g f 1 2
    (a + 1) * (b - 1)
    a = 10
    g f 1 2
    (a + 1) * (b - 1)
    a = 10
    ",
    Value::Int(BigInt::from(10))
}

test_frontend!{
    conditional_cfp,
    "
    let f = λ (a: num, b: num) -> a + b
    let x = 0
    if x {
        x = f 3 4
    } else {
        x = f 5 6
    }
    x
    ",
    |cfg| assert_eq!(cfg.graph.edge_count(), 0)
}
